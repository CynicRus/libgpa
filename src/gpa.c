#include "gpa.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#define GPA_ID {'G', 'P', 'A', ' '}
#define INITIAL_FILES_CAPACITY 16
#define PATH_SEPARATOR '/'
#define DEFAULT_XOR_KEY "gpa_default_key"
#define DEFAULT_XOR_KEY_SIZE (sizeof(DEFAULT_XOR_KEY) - 1)
#define MAX_PATH_LEN 4096
#define BUFFER_SIZE 8192 // Буфер для потоковой обработки

GPAArchive *g_gpa_archive = NULL;
bool g_archive_active = false;
GPAEncryptionContext *g_encryption_context = NULL;
GPACompressionContext *g_compression_context = NULL;

FILE *g_log_file = NULL;

static void gpa_log(const char *format, ...)
{
    if (!g_log_file)
        return;

    va_list args;
    va_start(args, format);
    vfprintf(g_log_file, format, args);
    va_end(args);
    fflush(g_log_file);
}

static void gpa_xor_crypt(uint8_t *data, uint64_t size, const uint8_t *key, uint64_t key_size)
{
    for (uint64_t i = 0; i < size; i++)
        data[i] ^= key[i % key_size];
}

static void gpa_refresh_offsets(void)
{
    if (!g_archive_active || !g_gpa_archive || g_gpa_archive->file_count == 0)
        return;

    uint64_t offset = 12; // Заголовок: 4 (ID) + 4 (version) + 4 (file_count)
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        offset += 1 + g_gpa_archive->files[i]->path_len + 8 + 8 + 8 + 1 + 1; // Метаданные
        g_gpa_archive->files[i]->offset = offset;
        offset += g_gpa_archive->files[i]->size;
    }
}

static char *gpa_get_parent_path(const char *path)
{
    if (!path)
    {
        gpa_log("gpa_get_parent_path: NULL path\n");
        return NULL;
    }

    char *normalized = gpa_normalize_path(path);
    if (!normalized)
    {
        gpa_log("gpa_get_parent_path: Failed to normalize path %s\n", path);
        return NULL;
    }

    char *last_sep = strrchr(normalized, PATH_SEPARATOR);
    if (!last_sep || last_sep == normalized)
    {
        free(normalized);
        gpa_log("gpa_get_parent_path: No parent directory for %s\n", path);
        return strdup("");
    }

    *last_sep = '\0';
    char *parent = strdup(normalized);
    free(normalized);
    if (!parent)
    {
        gpa_log("gpa_get_parent_path: Memory allocation failed\n");
        return NULL;
    }

    return parent;
}

static int32_t gpa_compress_zlib(const uint8_t *input, uint64_t input_size, uint8_t **output, uint64_t *output_size)
{
    z_stream strm = {0};
    int ret = deflateInit(&strm, Z_DEFAULT_COMPRESSION);
    if (ret != Z_OK)
    {
        gpa_log("gpa_compress_zlib: deflateInit failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    strm.next_in = (Bytef *)input;
    strm.avail_in = input_size;
    *output_size = deflateBound(&strm, input_size);
    *output = malloc(*output_size);
    if (!*output)
    {
        deflateEnd(&strm);
        gpa_log("gpa_compress_zlib: Memory allocation failed for %llu bytes\n", *output_size);
        return GPA_ERR_MEMORY;
    }

    strm.next_out = *output;
    strm.avail_out = *output_size;

    ret = deflate(&strm, Z_FINISH);
    if (ret != Z_STREAM_END)
    {
        free(*output);
        deflateEnd(&strm);
        gpa_log("gpa_compress_zlib: deflate failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    *output_size = strm.total_out;
    deflateEnd(&strm);
    return GPA_OK;
}

static int32_t gpa_decompress_zlib(const uint8_t *input, uint64_t input_size, uint64_t real_size, uint8_t **output)
{
    z_stream strm = {0};
    int ret = inflateInit(&strm);
    if (ret != Z_OK)
    {
        gpa_log("gpa_decompress_zlib: inflateInit failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    strm.next_in = (Bytef *)input;
    strm.avail_in = input_size;
    *output = malloc(real_size);
    if (!*output)
    {
        inflateEnd(&strm);
        gpa_log("gpa_decompress_zlib: Memory allocation failed for %llu bytes\n", real_size);
        return GPA_ERR_MEMORY;
    }

    strm.next_out = *output;
    strm.avail_out = real_size;

    ret = inflate(&strm, Z_FINISH);
    if (ret != Z_STREAM_END)
    {
        free(*output);
        inflateEnd(&strm);
        gpa_log("gpa_decompress_zlib: inflate failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    if (strm.total_out != real_size)
    {
        free(*output);
        inflateEnd(&strm);
        gpa_log("gpa_decompress_zlib: Decompressed size %llu does not match expected %llu\n", (uint64_t)strm.total_out, real_size);
        return GPA_ERR_COMPRESSION;
    }

    inflateEnd(&strm);
    return GPA_OK;
}

#ifdef GPA_WITH_LZMA
static int32_t gpa_compress_lzma(const uint8_t *input, uint64_t input_size, uint8_t **output, uint64_t *output_size)
{
    lzma_stream strm = LZMA_STREAM_INIT;
    lzma_ret ret = lzma_easy_encoder(&strm, LZMA_PRESET_DEFAULT, LZMA_CHECK_CRC64);
    if (ret != LZMA_OK)
    {
        gpa_log("gpa_compress_lzma: lzma_easy_encoder failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    strm.next_in = input;
    strm.avail_in = input_size;
    *output_size = input_size + input_size / 3 + 128;
    *output = malloc(*output_size);
    if (!*output)
    {
        lzma_end(&strm);
        gpa_log("gpa_compress_lzma: Memory allocation failed for %llu bytes\n", *output_size);
        return GPA_ERR_MEMORY;
    }

    strm.next_out = *output;
    strm.avail_out = *output_size;

    ret = lzma_code(&strm, LZMA_FINISH);
    if (ret != LZMA_STREAM_END)
    {
        free(*output);
        lzma_end(&strm);
        gpa_log("gpa_compress_lzma: lzma_code failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    *output_size = strm.total_out;
    lzma_end(&strm);
    return GPA_OK;
}

static int32_t gpa_decompress_lzma(const uint8_t *input, uint64_t input_size, uint64_t real_size, uint8_t **output)
{
    lzma_stream strm = LZMA_STREAM_INIT;
    lzma_ret ret = lzma_stream_decoder(&strm, UINT64_MAX, 0);
    if (ret != LZMA_OK)
    {
        gpa_log("gpa_decompress_lzma: lzma_stream_decoder failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    strm.next_in = input;
    strm.avail_in = input_size;
    *output = malloc(real_size);
    if (!*output)
    {
        lzma_end(&strm);
        gpa_log("gpa_decompress_lzma: Memory allocation failed for %llu bytes\n", real_size);
        return GPA_ERR_MEMORY;
    }

    strm.next_out = *output;
    strm.avail_out = real_size;

    ret = lzma_code(&strm, LZMA_FINISH);
    if (ret != LZMA_STREAM_END)
    {
        free(*output);
        lzma_end(&strm);
        gpa_log("gpa_decompress_lzma: lzma_code failed with code %d\n", ret);
        return GPA_ERR_COMPRESSION;
    }

    if (strm.total_out != real_size)
    {
        free(*output);
        lzma_end(&strm);
        gpa_log("gpa_decompress_lzma: Decompressed size %llu does not match expected %llu\n", (uint64_t)strm.total_out, real_size);
        return GPA_ERR_COMPRESSION;
    }

    lzma_end(&strm);
    return GPA_OK;
}
#endif

#ifdef GPA_WITH_ZSTD
static int32_t gpa_compress_zstd(const uint8_t *input, uint64_t input_size, uint8_t **output, uint64_t *output_size)
{
    *output_size = ZSTD_compressBound(input_size);
    *output = malloc(*output_size);
    if (!*output)
    {
        gpa_log("gpa_compress_zstd: Memory allocation failed for %llu bytes\n", *output_size);
        return GPA_ERR_MEMORY;
    }

    size_t compressed_size = ZSTD_compress(*output, *output_size, input, input_size, ZSTD_CLEVEL_DEFAULT);
    if (ZSTD_isError(compressed_size))
    {
        free(*output);
        gpa_log("gpa_compress_zstd: ZSTD_compress failed with error %s\n", ZSTD_getErrorName(compressed_size));
        return GPA_ERR_COMPRESSION;
    }

    *output_size = compressed_size;
    return GPA_OK;
}

static int32_t gpa_decompress_zstd(const uint8_t *input, uint64_t input_size, uint64_t real_size, uint8_t **output)
{
    *output = malloc(real_size);
    if (!*output)
    {
        gpa_log("gpa_decompress_zstd: Memory allocation failed for %llu bytes\n", real_size);
        return GPA_ERR_MEMORY;
    }

    size_t decompressed_size = ZSTD_decompress(*output, real_size, input, input_size);
    if (ZSTD_isError(decompressed_size))
    {
        free(*output);
        gpa_log("gpa_decompress_zstd: ZSTD_decompress failed with error %s\n", ZSTD_getErrorName(decompressed_size));
        return GPA_ERR_COMPRESSION;
    }

    if (decompressed_size != real_size)
    {
        free(*output);
        gpa_log("gpa_decompress_zstd: Decompressed size %llu does not match expected %llu\n", (uint64_t)decompressed_size, real_size);
        return GPA_ERR_COMPRESSION;
    }

    return GPA_OK;
}
#endif

#ifdef GPA_WITH_LZ4
static int32_t gpa_compress_lz4(const uint8_t *input, uint64_t input_size, uint8_t **output, uint64_t *output_size)
{
    // Определяем максимальный размер сжатых данных
    // LZ4_compressBound гарантирует достаточный размер для сжатых данных
    *output_size = LZ4_compressBound((int)input_size);
    *output = malloc(*output_size);
    if (!*output)
    {
        gpa_log("gpa_compress_lz4: Memory allocation failed for %llu bytes\n", *output_size);
        return GPA_ERR_MEMORY;
    }

    // Выполняем сжатие данных с использованием LZ4
    int compressed_size = LZ4_compress_default((const char *)input, (char *)*output,
                                               (int)input_size, (int)*output_size);
    if (compressed_size <= 0)
    {
        free(*output);
        gpa_log("gpa_compress_lz4: LZ4 compression failed\n");
        return GPA_ERR_COMPRESSION;
    }

    // Обновляем размер выходных данных
    *output_size = (uint64_t)compressed_size;
    return GPA_OK;
}

static int32_t gpa_decompress_lz4(const uint8_t *input, uint64_t input_size, uint64_t real_size, uint8_t **output)
{
    // Выделяем память под распакованные данные
    *output = malloc(real_size);
    if (!*output)
    {
        gpa_log("gpa_decompress_lz4: Memory allocation failed for %llu bytes\n", real_size);
        return GPA_ERR_MEMORY;
    }

    // Выполняем распаковку данных
    int decompressed_size = LZ4_decompress_safe((const char *)input, (char *)*output,
                                                (int)input_size, (int)real_size);
    if (decompressed_size < 0)
    {
        free(*output);
        gpa_log("gpa_decompress_lz4: LZ4 decompression failed\n");
        return GPA_ERR_COMPRESSION;
    }

    // Проверяем, соответствует ли размер распакованных данных ожидаемому
    if ((uint64_t)decompressed_size != real_size)
    {
        free(*output);
        gpa_log("gpa_decompress_lz4: Decompressed size %llu does not match expected %llu\n",
                (uint64_t)decompressed_size, real_size);
        return GPA_ERR_COMPRESSION;
    }

    return GPA_OK;
}

#endif

#ifdef GPA_WITH_BZ2
static int32_t gpa_compress_bzip2(const uint8_t *input, uint64_t input_size, uint8_t **output, uint64_t *output_size)
{
    // Для bzip2 нет стандартной функции для определения максимального размера выходных данных
    // Обычно используют эмпирическое правило: размер выходных данных не превышает 1.01*размер_входных_данных + 600 байт
    *output_size = (uint64_t)(1.01 * input_size) + 600;
    *output = malloc(*output_size);
    if (!*output)
    {
        gpa_log("gpa_compress_bzip2: Memory allocation failed for %llu bytes\n", *output_size);
        return GPA_ERR_MEMORY;
    }

    // Подготовка переменных для bzip2
    bz_stream strm;
    memset(&strm, 0, sizeof(strm));
    
    // Инициализация bzip2 для сжатия
    // 9 - максимальный уровень сжатия, 0 - без вербозного вывода, 30 - стандартная рабочая величина
    int bz_error = BZ2_bzCompressInit(&strm, 9, 0, 30);
    if (bz_error != BZ_OK)
    {
        free(*output);
        gpa_log("gpa_compress_bzip2: BZ2_bzCompressInit failed with error %d\n", bz_error);
        return GPA_ERR_COMPRESSION;
    }

    strm.next_in = (char*)input;
    strm.avail_in = (unsigned int)input_size;
    strm.next_out = (char*)*output;
    strm.avail_out = (unsigned int)*output_size;

    // Выполняем сжатие
    bz_error = BZ2_bzCompress(&strm, BZ_FINISH);
    
    // Освобождаем ресурсы bzip2
    BZ2_bzCompressEnd(&strm);
    
    if (bz_error != BZ_STREAM_END)
    {
        free(*output);
        gpa_log("gpa_compress_bzip2: BZ2_bzCompress failed with error %d\n", bz_error);
        return GPA_ERR_COMPRESSION;
    }

    // Обновляем размер выходных данных
    *output_size = (uint64_t)(strm.total_out_lo32);
    return GPA_OK;
}

static int32_t gpa_decompress_bzip2(const uint8_t *input, uint64_t input_size, uint64_t real_size, uint8_t **output)
{
    // Выделяем память под распакованные данные
    *output = malloc(real_size);
    if (!*output)
    {
        gpa_log("gpa_decompress_bzip2: Memory allocation failed for %llu bytes\n", real_size);
        return GPA_ERR_MEMORY;
    }

    // Подготовка переменных для bzip2
    bz_stream strm;
    memset(&strm, 0, sizeof(strm));

    // Инициализация bzip2 для распаковки
    // 0 - без вербозного вывода, 0 - не использовать память малого размера
    int bz_error = BZ2_bzDecompressInit(&strm, 0, 0);
    if (bz_error != BZ_OK)
    {
        free(*output);
        gpa_log("gpa_decompress_bzip2: BZ2_bzDecompressInit failed with error %d\n", bz_error);
        return GPA_ERR_COMPRESSION;
    }

    strm.next_in = (char*)input;
    strm.avail_in = (unsigned int)input_size;
    strm.next_out = (char*)*output;
    strm.avail_out = (unsigned int)real_size;

    // Выполняем распаковку
    bz_error = BZ2_bzDecompress(&strm);
    
    // Освобождаем ресурсы bzip2
    BZ2_bzDecompressEnd(&strm);

    if (bz_error != BZ_STREAM_END)
    {
        free(*output);
        gpa_log("gpa_decompress_bzip2: BZ2_bzDecompress failed with error %d\n", bz_error);
        return GPA_ERR_COMPRESSION;
    }

    // Проверяем, соответствует ли размер распакованных данных ожидаемому
    if (strm.total_out_lo32 != (unsigned int)real_size)
    {
        free(*output);
        gpa_log("gpa_decompress_bzip2: Decompressed size %u does not match expected %llu\n", 
                strm.total_out_lo32, real_size);
        return GPA_ERR_COMPRESSION;
    }

    return GPA_OK;
}
#endif

static int32_t gpa_compress(const uint8_t *input, uint64_t input_size, uint8_t **output, uint64_t *output_size, uint8_t compression_type)
{
    if (!input || input_size == 0 || !output || !output_size)
    {
        gpa_log("gpa_compress: Invalid input parameters\n");
        return GPA_ERR_INVALID_PATH;
    }

    if (compression_type == GPA_COMPRESS_NONE)
    {
        *output = malloc(input_size);
        if (!*output)
        {
            gpa_log("gpa_compress: Memory allocation failed for %llu bytes\n", input_size);
            return GPA_ERR_MEMORY;
        }
        memcpy(*output, input, input_size);
        *output_size = input_size;
        return GPA_OK;
    }
    else if (compression_type == GPA_COMPRESS_ZLIB)
    {
        return gpa_compress_zlib(input, input_size, output, output_size);
    }
#ifdef GPA_WITH_LZMA
    else if (compression_type == GPA_COMPRESS_LZMA)
    {
        return gpa_compress_lzma(input, input_size, output, output_size);
    }
#endif
#ifdef GPA_WITH_ZSTD
    else if (compression_type == GPA_COMPRESS_ZSTD)
    {
        return gpa_compress_zstd(input, input_size, output, output_size);
    }
#endif
#ifdef GPA_WITH_LZ4
    else if (compression_type == GPA_COMPRESS_LZ4)
    {
        return gpa_compress_lz4(input, input_size, output, output_size);
    }
#endif
#ifdef GPA_WITH_BZ2
    else if (compression_type == GPA_COMPRESS_BZ2)
    {
        return gpa_compress_bzip2(input, input_size, output, output_size);
    }
#endif
    else if (compression_type == GPA_COMPRESS_CUSTOM && g_compression_context && g_compression_context->compress_func)
    {
        gpa_log("gpa_compress_data: Using custom compression function\n");
        return g_compression_context->compress_func(input, input_size, output, output_size, g_compression_context->user_data);
    }
    else
    {
        gpa_log("gpa_compress: Invalid or unsupported compression type %d\n", compression_type);
        return GPA_ERR_COMPRESSION;
    }
}

static int32_t gpa_decompress(const uint8_t *input, uint64_t input_size, uint64_t real_size, uint8_t **output, uint8_t compression_type)
{
    if (!input || input_size == 0 || real_size == 0 || !output)
    {
        gpa_log("gpa_decompress: Invalid input parameters\n");
        return GPA_ERR_INVALID_PATH;
    }

    if (compression_type == GPA_COMPRESS_NONE)
    {
        *output = malloc(real_size);
        if (!*output)
        {
            gpa_log("gpa_decompress: Memory allocation failed for %llu bytes\n", real_size);
            return GPA_ERR_MEMORY;
        }
        memcpy(*output, input, real_size);
        return GPA_OK;
    }
    else if (compression_type == GPA_COMPRESS_ZLIB)
    {
        return gpa_decompress_zlib(input, input_size, real_size, output);
    }
#ifdef GPA_WITH_LZMA
    else if (compression_type == GPA_COMPRESS_LZMA)
    {
        return gpa_decompress_lzma(input, input_size, real_size, output);
    }
#endif
#ifdef GPA_WITH_ZSTD
    else if (compression_type == GPA_COMPRESS_ZSTD)
    {
        return gpa_decompress_zstd(input, input_size, real_size, output);
    }
#endif
#ifdef GPA_WITH_LZ4
    else if (compression_type == GPA_COMPRESS_LZ4)
    {
        return gpa_decompress_lz4(input, input_size, real_size, output);
    }
#endif
#ifdef GPA_WITH_BZ2
    else if (compression_type == GPA_COMPRESS_BZ2)
    {
        return gpa_decompress_bzip2(input, input_size, real_size, output);
    }
#endif
    else if (compression_type == GPA_COMPRESS_CUSTOM && g_compression_context && g_compression_context->decompress_func)
    {
        gpa_log("gpa_decompress_data: Using custom decompression function\n");
        return g_compression_context->decompress_func(input, input_size, real_size, output, g_compression_context->user_data);
    }
    else
    {
        gpa_log("gpa_decompress: Invalid or unsupported compression type %d\n", compression_type);
        return GPA_ERR_COMPRESSION;
    }
}

static char *gpa_normalize_path(const char *path)
{
    if (!path)
    {
        gpa_log("gpa_normalize_path: NULL path\n");
        return NULL;
    }
    char *normalized = strdup(path);
    if (!normalized)
    {
        gpa_log("gpa_normalize_path: Memory allocation failed\n");
        return NULL;
    }

    for (char *p = normalized; *p; p++)
    {
        if (*p == '\\' || *p == '/')
            *p = PATH_SEPARATOR;
    }
    return normalized;
}

static char *gpa_get_filename(const char *path)
{
    if (!path)
    {
        gpa_log("gpa_get_filename: NULL path\n");
        return NULL;
    }
    gpa_log("gpa_get_filename: %s \n", path);
    const char *last_sep = strrchr(path, PATH_SEPARATOR);
    char *result = strdup(last_sep ? last_sep + 1 : path);
    if (!result)
    {
        gpa_log("gpa_get_filename: Memory allocation failed\n");
    }
    return result;
}

static char *gpa_get_relative_path(const char *full_path, const char *base_path)
{
    if (!full_path || !base_path)
    {
        gpa_log("gpa_get_relative_path: NULL full_path or base_path\n");
        return NULL;
    }

    char *norm_full = gpa_normalize_path(full_path);
    char *norm_base = gpa_normalize_path(base_path);
    if (!norm_full || !norm_base)
    {
        free(norm_full);
        free(norm_base);
        return NULL;
    }

    size_t base_len = strlen(norm_base);
    const char *rel_path = norm_full;

    if (strncmp(norm_full, norm_base, base_len) == 0)
    {
        rel_path = norm_full + base_len;
        if (*rel_path == PATH_SEPARATOR)
            rel_path++;
    }

    char *result = strdup(rel_path);
    free(norm_full);
    free(norm_base);
    if (!result)
    {
        gpa_log("gpa_get_relative_path: Memory allocation failed\n");
        return NULL;
    }

    return result;
}

static int32_t gpa_encrypt_data(uint8_t *data, uint64_t size)
{
    if (!data || size < 4)
    {
        gpa_log("gpa_encrypt_data: Invalid parameters\n");
        return GPA_ERR_INVALID_PARAM;
    }

    // Вычисляем CRC32 для исходных данных (без последних 4 байтов)
    uint32_t crc = crc32(0L, Z_NULL, 0);
    crc = crc32(crc, data, size - 4);

    // Шифруем данные (без последних 4 байтов для CRC)
    if (g_encryption_context && g_encryption_context->encrypt_func)
    {
        gpa_log("gpa_encrypt_data: Using custom encryption function with key_size=%llu\n", g_encryption_context->key_size);
        g_encryption_context->encrypt_func(data, size - 4, g_encryption_context->key, g_encryption_context->key_size);
    }
    else if (g_encryption_context && g_encryption_context->key)
    {
        gpa_log("gpa_encrypt_data: Encrypting %llu bytes with user key (size=%llu), crc=%lu\n",
                size - 4, g_encryption_context->key_size, crc);
        gpa_xor_crypt(data, size - 4, g_encryption_context->key, g_encryption_context->key_size);
    }
    else
    {
        gpa_log("gpa_encrypt_data: Encrypting %llu bytes with default key (size=%llu), crc=%lu\n",
                size - 4, (uint64_t)DEFAULT_XOR_KEY_SIZE, crc);
        gpa_xor_crypt(data, size - 4, (const uint8_t *)DEFAULT_XOR_KEY, DEFAULT_XOR_KEY_SIZE);
    }

    // Сохраняем CRC32 в последние 4 байта
    memcpy(data + size - 4, &crc, 4);

    return GPA_OK;
}

static int32_t gpa_decrypt_data(uint8_t *data, uint64_t size)
{
    if (!data || size < 4)
    {
        gpa_log("gpa_decrypt_data: Invalid parameters or size too small\n");
        return GPA_ERR_INVALID_PARAM;
    }

    // Извлекаем CRC32 из последних 4 байтов
    uint32_t stored_crc;
    memcpy(&stored_crc, data + size - 4, 4);

    // Дешифруем только данные (без CRC)
    if (g_encryption_context && g_encryption_context->decrypt_func)
    {
        gpa_log("gpa_decrypt_data: Using custom decryption function with key_size=%llu\n", g_encryption_context->key_size);
        g_encryption_context->decrypt_func(data, size - 4, g_encryption_context->key, g_encryption_context->key_size);
    }
    else if (g_encryption_context && g_encryption_context->key)
    {
        gpa_log("gpa_decrypt_data: Decrypting %llu bytes with user key (size=%llu)\n",
                size - 4, g_encryption_context->key_size);
        gpa_xor_crypt(data, size - 4, g_encryption_context->key, g_encryption_context->key_size);
    }
    else
    {
        gpa_log("gpa_decrypt_data: Decrypting %llu bytes with default key (size=%llu)\n",
                size - 4, (uint64_t)DEFAULT_XOR_KEY_SIZE);
        gpa_xor_crypt(data, size - 4, (const uint8_t *)DEFAULT_XOR_KEY, DEFAULT_XOR_KEY_SIZE);
    }

    // Проверяем CRC32 расшифрованных данных
    uint32_t computed_crc = crc32(0L, Z_NULL, 0);
    computed_crc = crc32(computed_crc, data, size - 4);
    if (computed_crc != stored_crc)
    {
        gpa_log("gpa_decrypt_data: CRC mismatch (expected %lu, got %lu), wrong key or corrupted data\n",
                stored_crc, computed_crc);
        return GPA_ERR_DECRYPTION_FAILED;
    }

    return GPA_OK;
}

GPA_API void gpa_init_log(const char *log_path)
{
    if (g_log_file)
        fclose(g_log_file);
    g_log_file = fopen(log_path, "w");
}

GPA_API void gpa_close_log()
{
    if (g_log_file)
    {
        fclose(g_log_file);
        g_log_file = NULL;
    }
}

GPA_API int32_t gpa_get_version(uint8_t *buffer, size_t buffer_size)
{
    if (buffer == NULL || buffer_size == 0)
    {
        gpa_log("gpa_get_version: Invalid buffer or buffer_size\n");
        return -1;
    }

    int result = snprintf((char *)buffer, buffer_size, "%d.%d.%d",
                          GPA_VERSION_MAJOR,
                          GPA_VERSION_MINOR,
                          GPA_VERSION_PATCH);

    if (result < 0 || (size_t)result >= buffer_size)
    {
        gpa_log("gpa_get_version: Buffer too small (%zu bytes) for version string\n", buffer_size);
        return GPA_ERR_BUFFER_TO_SMALL;
    }

    return GPA_OK;
}

GPA_API int32_t gpa_create_archive(const char *filename)
{
    if (!filename)
    {
        gpa_log("gpa_create_archive: NULL filename\n");
        return GPA_ERR_INVALID_PATH;
    }
    if (g_archive_active)
    {
        gpa_log("gpa_create_archive: Archive already loaded\n");
        return GPA_ERR_ARCHIVE_ALREADY_LOADED;
    }

    g_gpa_archive = calloc(1, sizeof(GPAArchive));
    if (!g_gpa_archive)
    {
        gpa_log("gpa_create_archive: Memory allocation failed for GPAArchive\n");
        return GPA_ERR_MEMORY;
    }

    g_gpa_archive->filename = gpa_normalize_path(filename);
    if (!g_gpa_archive->filename)
    {
        free(g_gpa_archive);
        return GPA_ERR_MEMORY;
    }

    g_gpa_archive->id[0] = 'G';
    g_gpa_archive->id[1] = 'P';
    g_gpa_archive->id[2] = 'A';
    g_gpa_archive->id[3] = ' ';
    g_gpa_archive->version[0] = GPA_VERSION_MAJOR;
    g_gpa_archive->version[1] = GPA_VERSION_MINOR;
    g_gpa_archive->version[2] = GPA_VERSION_PATCH;
    g_gpa_archive->version[3] = 0;
    g_gpa_archive->files = calloc(INITIAL_FILES_CAPACITY, sizeof(GPAFile *));
    if (!g_gpa_archive->files)
    {
        free(g_gpa_archive->filename);
        free(g_gpa_archive);
        gpa_log("gpa_create_archive: Memory allocation failed for files array\n");
        return GPA_ERR_MEMORY;
    }
    g_gpa_archive->files_capacity = INITIAL_FILES_CAPACITY;
    g_archive_active = true;
    gpa_log("gpa_create_archive: Created archive %s\n", filename);
    return GPA_OK;
}

GPA_API int32_t gpa_load_archive(const char *filename)
{
    if (!filename)
    {
        gpa_log("gpa_load_archive: NULL filename\n");
        return GPA_ERR_INVALID_PATH;
    }
    if (g_archive_active)
    {
        gpa_log("gpa_load_archive: Archive already loaded\n");
        return GPA_ERR_ARCHIVE_ALREADY_LOADED;
    }

    FILE *file = fopen(filename, "rb");
    if (!file)
    {
        gpa_log("gpa_load_archive: Failed to open file %s\n", filename);
        return GPA_ERR_FILE_NOT_FOUND;
    }

    g_gpa_archive = calloc(1, sizeof(GPAArchive));
    if (!g_gpa_archive)
    {
        fclose(file);
        gpa_log("gpa_load_archive: Memory allocation failed for GPAArchive\n");
        return GPA_ERR_MEMORY;
    }

    g_gpa_archive->filename = gpa_normalize_path(filename);
    if (!g_gpa_archive->filename)
    {
        free(g_gpa_archive);
        fclose(file);
        return GPA_ERR_MEMORY;
    }

    // Чтение заголовка
    if (fread(g_gpa_archive->id, 1, 4, file) != 4 ||
        g_gpa_archive->id[0] != 'G' || g_gpa_archive->id[1] != 'P' ||
        g_gpa_archive->id[2] != 'A' || g_gpa_archive->id[3] != ' ')
    {
        free(g_gpa_archive->filename);
        free(g_gpa_archive);
        fclose(file);
        gpa_log("gpa_load_archive: Invalid archive format\n");
        return GPA_ERR_INVALID_FORMAT;
    }

    if (fread(g_gpa_archive->version, 1, 4, file) != 4)
    {
        free(g_gpa_archive->filename);
        free(g_gpa_archive);
        fclose(file);
        gpa_log("gpa_load_archive: Failed to read version\n");
        return GPA_ERR_IO;
    }

    if (fread(&g_gpa_archive->file_count, 4, 1, file) != 1)
    {
        free(g_gpa_archive->filename);
        free(g_gpa_archive);
        fclose(file);
        gpa_log("gpa_load_archive: Failed to read file count\n");
        return GPA_ERR_IO;
    }

    gpa_log("gpa_load_archive: File count = %u\n", g_gpa_archive->file_count);

    if (g_gpa_archive->file_count == 0)
    {
        g_gpa_archive->files = NULL;
        g_gpa_archive->files_capacity = 0;
        fclose(file);
        g_archive_active = true;
        gpa_log("gpa_load_archive: Loaded empty archive %s\n", filename);
        return GPA_OK;
    }

    g_gpa_archive->files = calloc(g_gpa_archive->file_count, sizeof(GPAFile *));
    if (!g_gpa_archive->files)
    {
        free(g_gpa_archive->filename);
        free(g_gpa_archive);
        fclose(file);
        gpa_log("gpa_load_archive: Memory allocation failed for files array\n");
        return GPA_ERR_MEMORY;
    }
    g_gpa_archive->files_capacity = g_gpa_archive->file_count;

    // Первый этап: чтение всех метаданных
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        GPAFile *file_entry = calloc(1, sizeof(GPAFile));
        if (!file_entry)
        {
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Memory allocation failed for file entry %u\n", i);
            return GPA_ERR_MEMORY;
        }

        if (fread(&file_entry->path_len, 1, 1, file) != 1)
        {
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Failed to read path length for file %u\n", i);
            return GPA_ERR_IO;
        }

        if (file_entry->path_len == 0 || file_entry->path_len > MAX_PATH_LEN)
        {
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Invalid path length %u for file %u\n", file_entry->path_len, i);
            return GPA_ERR_INVALID_FORMAT;
        }

        file_entry->path = malloc(file_entry->path_len + 1);
        if (!file_entry->path)
        {
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Memory allocation failed for path of file %u\n", i);
            return GPA_ERR_MEMORY;
        }

        if (fread(file_entry->path, 1, file_entry->path_len, file) != file_entry->path_len)
        {
            free(file_entry->path);
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Failed to read path for file %u\n", i);
            return GPA_ERR_IO;
        }
        file_entry->path[file_entry->path_len] = '\0';

        if (fread(&file_entry->offset, 8, 1, file) != 1 ||
            fread(&file_entry->size, 8, 1, file) != 1 ||
            fread(&file_entry->real_size, 8, 1, file) != 1 ||
            fread(&file_entry->compression_type, 1, 1, file) != 1 ||
            fread(&file_entry->encrypted, 1, 1, file) != 1)
        {
            free(file_entry->path);
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Failed to read metadata for file %u\n", i);
            return GPA_ERR_IO;
        }

        if (file_entry->size > 1ULL << 40 || file_entry->real_size > 1ULL << 40)
        {
            free(file_entry->path);
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Invalid size %llu or real_size %llu for file %u\n",
                    file_entry->size, file_entry->real_size, i);
            return GPA_ERR_INVALID_FORMAT;
        }

        bool valid_compression = (file_entry->compression_type == GPA_COMPRESS_NONE ||
                                  file_entry->compression_type == GPA_COMPRESS_ZLIB
#ifdef GPA_WITH_LZMA
                                  || file_entry->compression_type == GPA_COMPRESS_LZMA
#endif
#ifdef GPA_WITH_ZSTD
                                  || file_entry->compression_type == GPA_COMPRESS_ZSTD
#endif
#ifdef GPA_WITH_LZ4
                                  || file_entry->compression_type == GPA_COMPRESS_LZ4
#endif
#ifdef GPA_WITH_BZ2
                                  || file_entry->compression_type == GPA_COMPRESS_BZ2
#endif
                                  || file_entry->compression_type == GPA_COMPRESS_CUSTOM);

        if (!valid_compression || file_entry->encrypted > 1)
        {
            free(file_entry->path);
            free(file_entry);
            for (uint32_t j = 0; j < i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Invalid compression_type %u or encrypted %u for file %u\n",
                    file_entry->compression_type, file_entry->encrypted, i);
            return GPA_ERR_INVALID_FORMAT;
        }

        gpa_log("gpa_load_archive: File %u, path=%s, path_len=%u, compression_type=%d, encrypted=%d, size=%llu, real_size=%llu, offset=%llu\n",
                i, file_entry->path, file_entry->path_len, file_entry->compression_type,
                file_entry->encrypted, file_entry->size, file_entry->real_size, file_entry->offset);

        g_gpa_archive->files[i] = file_entry;
    }

    // Второй этап: чтение данных
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        GPAFile *file_entry = g_gpa_archive->files[i];
        if (fseek(file, file_entry->offset, SEEK_SET) != 0)
        {
            for (uint32_t j = 0; j <= i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]->data);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Failed to seek to offset %llu for file %u\n", file_entry->offset, i);
            return GPA_ERR_IO;
        }

        file_entry->data = malloc(file_entry->size);
        if (!file_entry->data)
        {
            for (uint32_t j = 0; j <= i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]->data);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Memory allocation failed for data of file %u (size=%llu)\n", i, file_entry->size);
            return GPA_ERR_MEMORY;
        }

        if (fread(file_entry->data, 1, file_entry->size, file) != file_entry->size)
        {
            for (uint32_t j = 0; j <= i; j++)
            {
                free(g_gpa_archive->files[j]->path);
                free(g_gpa_archive->files[j]->data);
                free(g_gpa_archive->files[j]);
            }
            free(g_gpa_archive->files);
            free(g_gpa_archive->filename);
            free(g_gpa_archive);
            fclose(file);
            gpa_log("gpa_load_archive: Failed to read data for file %u\n", i);
            return GPA_ERR_IO;
        }

        if (file_entry->encrypted)
        {
            int32_t decrypt_result = gpa_decrypt_data(file_entry->data, file_entry->size);
            if (decrypt_result != GPA_OK)
            {
                for (uint32_t j = 0; j <= i; j++)
                {
                    free(g_gpa_archive->files[j]->path);
                    free(g_gpa_archive->files[j]->data);
                    free(g_gpa_archive->files[j]);
                }
                free(g_gpa_archive->files);
                free(g_gpa_archive->filename);
                free(g_gpa_archive);
                fclose(file);
                gpa_log("gpa_load_archive: Decryption failed for file %u, result=%d\n", i, decrypt_result);
                return decrypt_result;
            }
        }
    }

    fclose(file);
    g_archive_active = true;
    gpa_log("gpa_load_archive: Loaded archive %s with %u files\n", filename, g_gpa_archive->file_count);
    return GPA_OK;
}

GPA_API int32_t gpa_save_archive(void)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_save_archive: Archive not active\n");
        return GPA_ERR_ARCHIVE_NOT_LOADED;
    }

    if (!g_gpa_archive->filename || strlen(g_gpa_archive->filename) == 0)
    {
        gpa_log("gpa_save_archive: Invalid or empty filename\n");
        return GPA_ERR_INVALID_PATH;
    }

    FILE *file = fopen(g_gpa_archive->filename, "wb");
    if (!file)
    {
        gpa_log("gpa_save_archive: Failed to open file %s (errno=%d)\n", g_gpa_archive->filename, errno);
        return GPA_ERR_IO;
    }

    // Записываем заголовок
    if (fwrite(g_gpa_archive->id, 1, 4, file) != 4 ||
        fwrite(g_gpa_archive->version, 1, 4, file) != 4 ||
        fwrite(&g_gpa_archive->file_count, 4, 1, file) != 1)
    {
        fclose(file);
        gpa_log("gpa_save_archive: Failed to write header\n");
        return GPA_ERR_IO;
    }

    // Вычисляем смещения для всех файлов
    uint64_t *offsets = malloc(g_gpa_archive->file_count * sizeof(uint64_t));
    if (!offsets)
    {
        fclose(file);
        gpa_log("gpa_save_archive: Memory allocation failed for offsets\n");
        return GPA_ERR_MEMORY;
    }

    // Смещение начинается после заголовка и всех метаданных
    uint64_t current_offset = 4 + 4 + 4; // id + version + file_count
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        GPAFile *file_entry = g_gpa_archive->files[i];
        if (!file_entry->path || !file_entry->data || file_entry->path_len == 0 || file_entry->path_len > MAX_PATH_LEN)
        {
            free(offsets);
            fclose(file);
            gpa_log("gpa_save_archive: Invalid file entry %u (path=%s, data=%p, path_len=%u)\n",
                    i, file_entry->path ? file_entry->path : "NULL", file_entry->data, file_entry->path_len);
            return GPA_ERR_INVALID_FORMAT;
        }

        // Проверяем корректность size и real_size
        if (file_entry->size == 0 || file_entry->size > (1ULL << 40) ||
            file_entry->real_size == 0 || file_entry->real_size > (1ULL << 40))
        {
            free(offsets);
            fclose(file);
            gpa_log("gpa_save_archive: Invalid size %llu or real_size %llu for file %u\n",
                    file_entry->size, file_entry->real_size, i);
            return GPA_ERR_INVALID_FORMAT;
        }

        // Увеличиваем смещение на размер метаданных
        current_offset += 1 + file_entry->path_len + 8 + 8 + 8 + 1 + 1; // path_len + path + offset + size + real_size + compression_type + encrypted
    }

    // Записываем метаданные и сохраняем смещения
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        GPAFile *file_entry = g_gpa_archive->files[i];
        long metadata_pos = ftell(file);
        gpa_log("gpa_save_archive: Writing metadata for file %u at position %ld\n", i, metadata_pos);

        // Сохраняем смещение для данных файла
        offsets[i] = current_offset;
        file_entry->offset = current_offset;

        // Записываем метаданные
        if (fwrite(&file_entry->path_len, 1, 1, file) != 1 ||
            fwrite(file_entry->path, 1, file_entry->path_len, file) != file_entry->path_len ||
            fwrite(&file_entry->offset, 8, 1, file) != 1 ||
            fwrite(&file_entry->size, 8, 1, file) != 1 ||
            fwrite(&file_entry->real_size, 8, 1, file) != 1 ||
            fwrite(&file_entry->compression_type, 1, 1, file) != 1 ||
            fwrite(&file_entry->encrypted, 1, 1, file) != 1)
        {
            free(offsets);
            fclose(file);
            gpa_log("gpa_save_archive: Failed to write metadata for file %u\n", i);
            return GPA_ERR_IO;
        }

        // Увеличиваем смещение для следующего файла
        current_offset += file_entry->size;
        gpa_log("gpa_save_archive: Metadata for file %u written, offset=%llu, size=%llu, real_size=%llu\n",
                i, file_entry->offset, file_entry->size, file_entry->real_size);
    }

    // Записываем данные файлов
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        GPAFile *file_entry = g_gpa_archive->files[i];
        long data_pos = ftell(file);
        if (data_pos != file_entry->offset)
        {
            free(offsets);
            fclose(file);
            gpa_log("gpa_save_archive: Incorrect file position %ld for file %u, expected offset %llu\n",
                    data_pos, i, file_entry->offset);
            return GPA_ERR_IO;
        }

        gpa_log("gpa_save_archive: Writing data for file %u at offset %llu\n", i, file_entry->offset);
        if (fwrite(file_entry->data, 1, file_entry->size, file) != file_entry->size)
        {
            free(offsets);
            fclose(file);
            gpa_log("gpa_save_archive: Failed to write data for file %u\n", i);
            return GPA_ERR_IO;
        }
    }

    free(offsets);
    fclose(file);
    g_gpa_archive->changed = false;
    gpa_log("gpa_save_archive: Saved archive %s with %u files\n", g_gpa_archive->filename, g_gpa_archive->file_count);
    return GPA_OK;
}

GPA_API int32_t gpa_close_archive(bool save)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_close_archive: Archive not active\n");
        return GPA_ERR_ARCHIVE_NOT_LOADED;
    }

    if (save)
    {
        int32_t result = gpa_save_archive();
        if (result != GPA_OK)
            return result;
    }

    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        free(g_gpa_archive->files[i]->path);
        free(g_gpa_archive->files[i]->data);
        free(g_gpa_archive->files[i]);
    }
    free(g_gpa_archive->files);
    free(g_gpa_archive->filename);
    free(g_gpa_archive);
    g_gpa_archive = NULL;
    g_archive_active = false;
    if (g_encryption_context)
    {
        if (g_encryption_context->key)
            free(g_encryption_context->key);
        free(g_encryption_context);
        g_encryption_context = NULL;
    }

    if (g_compression_context)
    {
        free(g_compression_context);
        g_compression_context = NULL;
    }
    gpa_log("gpa_close_archive: Archive closed\n");
    return GPA_OK;
}

GPA_API int32_t gpa_set_encryption(GPAEncryptFunc encrypt_func, GPADecryptFunc decrypt_func, const uint8_t *key, uint64_t key_size)
{
    // Инициализируем g_encryption_context, если он ещё не создан
    if (!g_encryption_context)
    {
        g_encryption_context = calloc(1, sizeof(GPAEncryptionContext));
        if (!g_encryption_context)
        {
            gpa_log("gpa_set_encryption: Failed to allocate g_encryption_context\n");
            return GPA_ERR_MEMORY;
        }
    }

    // Очищаем старый ключ, если он был
    if (g_encryption_context->key)
    {
        free(g_encryption_context->key);
        g_encryption_context->key = NULL;
        g_encryption_context->key_size = 0;
    }

    // Устанавливаем новые настройки
    g_encryption_context->encrypt_func = encrypt_func;
    g_encryption_context->decrypt_func = decrypt_func;

    if (key && key_size > 0)
    {
        g_encryption_context->key = malloc(key_size);
        if (!g_encryption_context->key)
        {
            gpa_log("gpa_set_encryption: Memory allocation failed for key\n");
            return GPA_ERR_MEMORY;
        }
        memcpy(g_encryption_context->key, key, key_size);
        g_encryption_context->key_size = key_size;
        gpa_log("gpa_set_encryption: Encryption set, key_size=%llu\n", key_size);
    }
    else
    {
        g_encryption_context->key = NULL;
        g_encryption_context->key_size = 0;
        gpa_log("gpa_set_encryption: Encryption set, no key provided\n");
    }

    return GPA_OK;
}

GPA_API int32_t gpa_set_compression(GPACompressFunc compress_func, GPADecompressFunc decompress_func, void *user_data)
{
    if (!g_compression_context)
    {
        g_compression_context = calloc(1, sizeof(GPACompressionContext));
        if (!g_compression_context)
        {
            gpa_log("gpa_set_compression: Failed to allocate g_compression_context\n");
            return GPA_ERR_MEMORY;
        }
    }

    g_compression_context->compress_func = compress_func;
    g_compression_context->decompress_func = decompress_func;
    g_compression_context->user_data = user_data;

    gpa_log("gpa_set_compression: Compression set, compress_func=%p, decompress_func=%p\n",
            compress_func, decompress_func);
    return GPA_OK;
}

GPA_API int32_t gpa_add_from_file(const char *filename, const char *base_path, uint8_t compression_type, bool encrypt)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_add_from_file: Archive not active\n");
        return GPA_ERR_ARCHIVE_NOT_LOADED;
    }
    if (!filename || !base_path)
    {
        gpa_log("gpa_add_from_file: NULL filename or base_path\n");
        return GPA_ERR_INVALID_PATH;
    }

    FILE *file = fopen(filename, "rb");
    if (!file)
    {
        gpa_log("gpa_add_from_file: Failed to open file %s\n", filename);
        return GPA_ERR_FILE_NOT_FOUND;
    }

    fseek(file, 0, SEEK_END);
    uint64_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (file_size == 0)
    {
        fclose(file);
        gpa_log("gpa_add_from_file: Empty file %s\n", filename);
        return GPA_ERR_INVALID_PATH;
    }

    uint8_t *buffer = malloc(file_size);
    if (!buffer)
    {
        fclose(file);
        gpa_log("gpa_add_from_file: Memory allocation failed for %llu bytes\n", file_size);
        return GPA_ERR_MEMORY;
    }

    if (fread(buffer, 1, file_size, file) != file_size)
    {
        free(buffer);
        fclose(file);
        gpa_log("gpa_add_from_file: Failed to read file %s\n", filename);
        return GPA_ERR_IO;
    }
    fclose(file);

    char *relative_path = gpa_get_relative_path(filename, base_path);
    if (!relative_path)
    {
        free(buffer);
        gpa_log("gpa_add_from_file: Failed to get relative path for %s relative to %s\n", filename, base_path);
        return GPA_ERR_INVALID_PATH;
    }

    int32_t result = gpa_add_from_stream(buffer, file_size, relative_path, compression_type, encrypt);
    free(relative_path);
    free(buffer);
    gpa_log("gpa_add_from_file: Added file %s as %s, result=%d\n", filename, relative_path, result);
    return result;
}

GPA_API int32_t gpa_add_from_stream(const uint8_t *data, uint64_t size, const char *path, uint8_t compression_type, bool encrypt)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_add_from_stream: Archive not active\n");
        return GPA_ERR_ARCHIVE_NOT_LOADED;
    }
    if (!data || size == 0 || !path)
    {
        gpa_log("gpa_add_from_stream: Invalid parameters\n");
        return GPA_ERR_INVALID_PATH;
    }

    char *normalized_path = gpa_normalize_path(path);
    if (!normalized_path)
    {
        gpa_log("gpa_add_from_stream: Failed to normalize path %s\n", path);
        return GPA_ERR_MEMORY;
    }

    uint8_t *final_data;
    uint64_t final_size;
    int32_t result;

    result = gpa_compress(data, size, &final_data, &final_size, compression_type);
    if (result != GPA_OK)
    {
        free(normalized_path);
        gpa_log("gpa_add_from_stream: Compression failed for path %s, result=%d\n", path, result);
        return result;
    }

    if (encrypt)
    {
        result = gpa_encrypt_data(final_data, final_size);
        if (result != GPA_OK)
        {
            free(final_data);
            free(normalized_path);
            gpa_log("gpa_add_from_stream: Encryption failed for path %s, result=%d\n", path, result);
            return result;
        }
    }

    GPAFile *file_entry = calloc(1, sizeof(GPAFile));
    if (!file_entry)
    {
        free(final_data);
        free(normalized_path);
        gpa_log("gpa_add_from_stream: Memory allocation failed for GPAFile\n");
        return GPA_ERR_MEMORY;
    }

    file_entry->path_len = strlen(normalized_path);
    file_entry->path = normalized_path;
    file_entry->size = final_size;
    file_entry->real_size = size;
    file_entry->data = final_data;
    file_entry->compression_type = compression_type;
    file_entry->encrypted = encrypt;

    if (g_gpa_archive->file_count >= g_gpa_archive->files_capacity)
    {
        uint32_t new_capacity = g_gpa_archive->files_capacity ? g_gpa_archive->files_capacity * 2 : INITIAL_FILES_CAPACITY;
        GPAFile **new_files = realloc(g_gpa_archive->files, new_capacity * sizeof(GPAFile *));
        if (!new_files)
        {
            free(file_entry->path);
            free(file_entry->data);
            free(file_entry);
            gpa_log("gpa_add_from_stream: Memory allocation failed for files array\n");
            return GPA_ERR_MEMORY;
        }
        g_gpa_archive->files = new_files;
        g_gpa_archive->files_capacity = new_capacity;
    }

    g_gpa_archive->files[g_gpa_archive->file_count++] = file_entry;
    g_gpa_archive->changed = true;
    gpa_refresh_offsets();
    gpa_log("gpa_add_from_stream: Added file %s, size=%llu, real_size=%llu, compression_type=%d, encrypted=%d\n",
            path, final_size, size, compression_type, encrypt);
    return GPA_OK;
}

static int32_t gpa_add_directory_recursive(const char *dir_path, const char *base_path, uint8_t compression_type, bool encrypt)
{
    if (!dir_path || !base_path)
    {
        gpa_log("gpa_add_directory_recursive: NULL dir_path or base_path\n");
        return GPA_ERR_INVALID_PATH;
    }

    char *normalized_dir = gpa_normalize_path(dir_path);
    if (!normalized_dir)
    {
        gpa_log("gpa_add_directory_recursive: Failed to normalize path %s\n", dir_path);
        return GPA_ERR_MEMORY;
    }

    DIR *dir = opendir(normalized_dir);
    if (!dir)
    {
        free(normalized_dir);
        gpa_log("gpa_add_directory_recursive: Failed to open directory %s\n", normalized_dir);
        return GPA_ERR_FILE_NOT_FOUND;
    }

    struct dirent *entry;
    int32_t result = GPA_OK;

    while ((entry = readdir(dir)) != NULL)
    {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
            continue;

        char *full_path = malloc(strlen(normalized_dir) + strlen(entry->d_name) + 2);
        if (!full_path)
        {
            closedir(dir);
            free(normalized_dir);
            gpa_log("gpa_add_directory_recursive: Memory allocation failed for full path\n");
            return GPA_ERR_MEMORY;
        }

        sprintf(full_path, "%s%c%s", normalized_dir, PATH_SEPARATOR, entry->d_name);

        struct stat st;
        if (stat(full_path, &st) != 0)
        {
            free(full_path);
            gpa_log("gpa_add_directory_recursive: Failed to stat %s\n", full_path);
            continue;
        }

        if (S_ISREG(st.st_mode))
        {
            char *relative_path = gpa_get_relative_path(full_path, base_path);
            if (!relative_path)
            {
                free(full_path);
                closedir(dir);
                free(normalized_dir);
                gpa_log("gpa_add_directory_recursive: Failed to get relative path for %s\n", full_path);
                return GPA_ERR_MEMORY;
            }

            FILE *file = fopen(full_path, "rb");
            if (!file)
            {
                free(relative_path);
                free(full_path);
                gpa_log("gpa_add_directory_recursive: Failed to open file %s\n", full_path);
                continue;
            }

            fseek(file, 0, SEEK_END);
            uint64_t file_size = ftell(file);
            fseek(file, 0, SEEK_SET);

            if (file_size == 0)
            {
                free(relative_path);
                free(full_path);
                fclose(file);
                gpa_log("gpa_add_directory_recursive: Empty file %s\n", full_path);
                continue;
            }

            uint8_t *buffer = malloc(file_size);
            if (!buffer)
            {
                free(relative_path);
                free(full_path);
                fclose(file);
                closedir(dir);
                free(normalized_dir);
                gpa_log("gpa_add_directory_recursive: Memory allocation failed for %llu bytes\n", file_size);
                return GPA_ERR_MEMORY;
            }

            if (fread(buffer, 1, file_size, file) != file_size)
            {
                free(buffer);
                free(relative_path);
                free(full_path);
                fclose(file);
                closedir(dir);
                free(normalized_dir);
                gpa_log("gpa_add_directory_recursive: Failed to read file %s\n", full_path);
                return GPA_ERR_IO;
            }
            fclose(file);

            result = gpa_add_from_stream(buffer, file_size, relative_path, compression_type, encrypt);
            free(buffer);
            free(relative_path);
            free(full_path);
            if (result != GPA_OK)
            {
                gpa_log("gpa_add_directory_recursive: Failed to add file %s\n", full_path);
                break;
            }
        }
        else if (S_ISDIR(st.st_mode))
        {
            result = gpa_add_directory_recursive(full_path, base_path, compression_type, encrypt);
            free(full_path);
            if (result != GPA_OK)
            {
                gpa_log("gpa_add_directory_recursive: Failed to process directory %s\n", full_path);
                break;
            }
        }
        else
        {
            free(full_path);
        }
    }

    closedir(dir);
    free(normalized_dir);
    return result;
}

GPA_API int32_t gpa_add_directory(const char *dir_path, uint8_t compression_type, bool recursive, bool encrypt)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_add_directory: Archive not active\n");
        return GPA_ERR_ARCHIVE_NOT_LOADED;
    }
    if (!dir_path)
    {
        gpa_log("gpa_add_directory: NULL dir_path\n");
        return GPA_ERR_INVALID_PATH;
    }

    char *normalized_dir = gpa_normalize_path(dir_path);
    if (!normalized_dir)
    {
        gpa_log("gpa_add_directory: Failed to normalize path %s\n", dir_path);
        return GPA_ERR_MEMORY;
    }

    // Получаем родительский каталог
    char *base_path = gpa_get_parent_path(dir_path);
    if (!base_path)
    {
        free(normalized_dir);
        gpa_log("gpa_add_directory: Failed to get parent path for %s\n", dir_path);
        return GPA_ERR_MEMORY;
    }

    int32_t result;
    if (recursive)
    {
        result = gpa_add_directory_recursive(normalized_dir, base_path, compression_type, encrypt);
    }
    else
    {
        DIR *dir = opendir(normalized_dir);
        if (!dir)
        {
            free(normalized_dir);
            free(base_path);
            gpa_log("gpa_add_directory: Failed to open directory %s\n", normalized_dir);
            return GPA_ERR_FILE_NOT_FOUND;
        }

        struct dirent *entry;
        result = GPA_OK;

        while ((entry = readdir(dir)) != NULL)
        {
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;

            char *full_path = malloc(strlen(normalized_dir) + strlen(entry->d_name) + 2);
            if (!full_path)
            {
                closedir(dir);
                free(normalized_dir);
                free(base_path);
                gpa_log("gpa_add_directory: Memory allocation failed for full path\n");
                return GPA_ERR_MEMORY;
            }

            sprintf(full_path, "%s%c%s", normalized_dir, PATH_SEPARATOR, entry->d_name);

            struct stat st;
            if (stat(full_path, &st) != 0 || !S_ISREG(st.st_mode))
            {
                free(full_path);
                continue;
            }

            result = gpa_add_from_file(full_path, base_path, compression_type, encrypt);
            free(full_path);
            if (result != GPA_OK)
            {
                gpa_log("gpa_add_directory: Failed to add file %s, result=%d\n", full_path, result);
                break;
            }
        }

        closedir(dir);
    }

    free(normalized_dir);
    free(base_path);
    gpa_log("gpa_add_directory: Added directory %s, result=%d\n", dir_path, result);
    return result;
}

GPA_API int32_t gpa_extract_to_file_by_id(uint32_t id, const char *filename)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_extract_to_file_by_id: Invalid ID %u or archive not active\n", id);
        return GPA_ERR_ARCHIVE_FILE_NOT_FOUND;
    }
    if (!filename)
    {
        gpa_log("gpa_extract_to_file_by_id: NULL filename\n");
        return GPA_ERR_INVALID_PATH;
    }

    GPAFile *file_entry = g_gpa_archive->files[id];
    uint8_t *final_data;
    uint64_t final_size = file_entry->real_size;
    int32_t result;

    result = gpa_decompress(file_entry->data, file_entry->size, file_entry->real_size, &final_data, file_entry->compression_type);
    if (result != GPA_OK)
    {
        gpa_log("gpa_extract_to_file_by_id: Decompression failed for ID %u, result=%d\n", id, result);
        return result;
    }

    char *normalized_filename = gpa_normalize_path(filename);
    if (!normalized_filename)
    {
        free(final_data);
        gpa_log("gpa_extract_to_file_by_id: Failed to normalize path %s\n", filename);
        return GPA_ERR_MEMORY;
    }

    FILE *file = fopen(normalized_filename, "wb");
    if (!file)
    {
        free(final_data);
        free(normalized_filename);
        gpa_log("gpa_extract_to_file_by_id: Failed to open output file %s\n", normalized_filename);
        return GPA_ERR_IO;
    }

    if (fwrite(final_data, 1, final_size, file) != final_size)
    {
        free(final_data);
        free(normalized_filename);
        fclose(file);
        gpa_log("gpa_extract_to_file_by_id: Failed to write to %s\n", normalized_filename);
        return GPA_ERR_IO;
    }

    free(final_data);
    free(normalized_filename);
    fclose(file);
    gpa_log("gpa_extract_to_file_by_id: Extracted file ID %u to %s\n", id, filename);
    return GPA_OK;
}

GPA_API int32_t gpa_extract_to_stream_by_id(uint32_t id, uint8_t **data, uint64_t *size)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_extract_to_stream_by_id: Invalid ID %u or archive not active\n", id);
        return GPA_ERR_ARCHIVE_FILE_NOT_FOUND;
    }
    if (!data || !size)
    {
        gpa_log("gpa_extract_to_stream_by_id: NULL data or size pointers\n");
        return GPA_ERR_INVALID_PATH;
    }

    GPAFile *file_entry = g_gpa_archive->files[id];
    *size = file_entry->real_size;
    int32_t result = gpa_decompress(file_entry->data, file_entry->size, file_entry->real_size, data, file_entry->compression_type);
    gpa_log("gpa_extract_to_stream_by_id: Extracted stream ID %u, size=%llu, result=%d\n", id, *size, result);
    return result;
}

GPA_API int32_t gpa_extract_directory(const char *dir_path, const char *output_path)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_extract_directory: Archive not active\n");
        return GPA_ERR_ARCHIVE_NOT_LOADED;
    }
    if (!dir_path || !output_path)
    {
        gpa_log("gpa_extract_directory: NULL dir_path or output_path\n");
        return GPA_ERR_INVALID_PATH;
    }

    char *normalized_dir = gpa_normalize_path(dir_path);
    if (!normalized_dir)
    {
        gpa_log("gpa_extract_directory: Failed to normalize path %s\n", dir_path);
        return GPA_ERR_MEMORY;
    }

    char *normalized_output = gpa_normalize_path(output_path);
    if (!normalized_output)
    {
        free(normalized_dir);
        gpa_log("gpa_extract_directory: Failed to normalize path %s\n", output_path);
        return GPA_ERR_MEMORY;
    }

    int32_t result = GPA_OK;
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        if (strncmp(g_gpa_archive->files[i]->path, normalized_dir, strlen(normalized_dir)) == 0)
        {
            char *relative_path = g_gpa_archive->files[i]->path + strlen(normalized_dir);
            if (*relative_path == PATH_SEPARATOR)
                relative_path++;

            char *output_file = malloc(strlen(normalized_output) + strlen(relative_path) + 2);
            if (!output_file)
            {
                free(normalized_dir);
                free(normalized_output);
                gpa_log("gpa_extract_directory: Memory allocation failed for output path\n");
                return GPA_ERR_MEMORY;
            }

            sprintf(output_file, "%s%c%s", normalized_output, PATH_SEPARATOR, relative_path);
            result = gpa_extract_to_file_by_id(i, output_file);
            free(output_file);
            if (result != GPA_OK)
            {
                gpa_log("gpa_extract_directory: Failed to extract file ID %u, result=%d\n", i, result);
                break;
            }
        }
    }

    free(normalized_dir);
    free(normalized_output);
    gpa_log("gpa_extract_directory: Extracted directory %s to %s, result=%d\n", dir_path, output_path, result);
    return result;
}

GPA_API int32_t gpa_delete_by_id(uint32_t id)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_delete_by_id: Invalid ID %u or archive not active\n", id);
        return GPA_ERR_ARCHIVE_FILE_NOT_FOUND;
    }

    free(g_gpa_archive->files[id]->path);
    free(g_gpa_archive->files[id]->data);
    free(g_gpa_archive->files[id]);

    for (uint32_t i = id; i < g_gpa_archive->file_count - 1; i++)
    {
        g_gpa_archive->files[i] = g_gpa_archive->files[i + 1];
    }
    g_gpa_archive->file_count--;
    g_gpa_archive->changed = true;
    gpa_refresh_offsets();
    gpa_log("gpa_delete_by_id: Deleted file ID %u\n", id);
    return GPA_OK;
}

GPA_API int32_t gpa_rename_by_id(uint32_t id, const char *new_path)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_rename_by_id: Invalid ID %u or archive not active\n", id);
        return GPA_ERR_ARCHIVE_FILE_NOT_FOUND;
    }
    if (!new_path)
    {
        gpa_log("gpa_rename_by_id: NULL new_path\n");
        return GPA_ERR_INVALID_PATH;
    }

    char *new_path_copy = gpa_normalize_path(new_path);
    if (!new_path_copy)
    {
        gpa_log("gpa_rename_by_id: Failed to normalize path %s\n", new_path);
        return GPA_ERR_MEMORY;
    }

    free(g_gpa_archive->files[id]->path);
    g_gpa_archive->files[id]->path = new_path_copy;
    g_gpa_archive->files[id]->path_len = strlen(new_path_copy);
    g_gpa_archive->changed = true;
    gpa_log("gpa_rename_by_id: Renamed file ID %u to %s\n", id, new_path);
    return GPA_OK;
}

GPA_API int32_t gpa_get_id_by_name(const char *path, bool case_sensitive)
{
    if (!g_archive_active || g_gpa_archive->file_count == 0)
    {
        gpa_log("gpa_get_id_by_name: Archive not active or empty\n");
        return -1;
    }
    if (!path)
    {
        gpa_log("gpa_get_id_by_name: NULL path\n");
        return -1;
    }

    char *normalized_path = gpa_normalize_path(path);
    if (!normalized_path)
    {
        gpa_log("gpa_get_id_by_name: Failed to normalize path %s\n", path);
        return -1;
    }

    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        if (case_sensitive)
        {
            if (strcmp(normalized_path, g_gpa_archive->files[i]->path) == 0)
            {
                free(normalized_path);
                gpa_log("gpa_get_id_by_name: Found file %s at ID %u\n", path, i);
                return i;
            }
        }
        else
        {
            size_t len = strlen(normalized_path);
            if (len == g_gpa_archive->files[i]->path_len)
            {
                bool match = true;
                for (size_t j = 0; j < len; j++)
                {
                    if (tolower(normalized_path[j]) != tolower(g_gpa_archive->files[i]->path[j]))
                    {
                        match = false;
                        break;
                    }
                }
                if (match)
                {
                    free(normalized_path);
                    gpa_log("gpa_get_id_by_name: Found file %s at ID %u (case-insensitive)\n", path, i);
                    return i;
                }
            }
        }
    }
    free(normalized_path);
    gpa_log("gpa_get_id_by_name: File %s not found\n", path);
    return -1;
}

GPA_API const char *gpa_get_name_by_id(uint32_t id)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_get_name_by_id: Invalid ID %u or archive not active\n", id);
        return "";
    }
    return g_gpa_archive->files[id]->path;
}

GPA_API uint64_t gpa_get_size_by_id(uint32_t id)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_get_size_by_id: Invalid ID %u or archive not active\n", id);
        return 0;
    }
    return g_gpa_archive->files[id]->size;
}

GPA_API uint64_t gpa_get_real_size_by_id(uint32_t id)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_get_real_size_by_id: Invalid ID %u or archive not active\n", id);
        return 0;
    }
    return g_gpa_archive->files[id]->real_size;
}

GPA_API uint8_t gpa_get_compression_type_by_id(uint32_t id)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_get_compression_type_by_id: Invalid ID %u or archive not active\n", id);
        return GPA_COMPRESS_NONE;
    }
    return g_gpa_archive->files[id]->compression_type;
}

GPA_API bool gpa_is_encrypted_by_id(uint32_t id)
{
    if (!g_archive_active || id >= g_gpa_archive->file_count)
    {
        gpa_log("gpa_is_encrypted_by_id: Invalid ID %u or archive not active\n", id);
        return false;
    }
    return g_gpa_archive->files[id]->encrypted;
}

GPA_API uint64_t gpa_get_archive_size(void)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_get_archive_size: Archive not active\n");
        return 0;
    }
    uint64_t total = 0;
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        total += g_gpa_archive->files[i]->size;
    }
    return total;
}

GPA_API uint64_t gpa_get_archive_real_size(void)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_get_archive_real_size: Archive not active\n");
        return 0;
    }
    uint64_t total = 0;
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        total += g_gpa_archive->files[i]->real_size;
    }
    return total;
}

GPA_API const char *gpa_get_archive_name(void)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_get_archive_name: Archive not active\n");
        return "";
    }
    return g_gpa_archive->filename;
}

GPA_API uint32_t gpa_get_total_files(void)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_get_total_files: Archive not active\n");
        return 0;
    }
    return g_gpa_archive->file_count;
}

GPA_API bool gpa_archive_active(void)
{
    return g_archive_active;
}

GPA_API bool gpa_archive_changed(void)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_archive_changed: Archive not active\n");
        return false;
    }
    return g_gpa_archive->changed;
}

GPA_API uint32_t gpa_get_files_in_directory(const char *dir_path, uint32_t **ids)
{
    if (!g_archive_active)
    {
        gpa_log("gpa_get_files_in_directory: Archive not active\n");
        return 0;
    }
    if (!dir_path || !ids)
    {
        gpa_log("gpa_get_files_in_directory: NULL dir_path or ids\n");
        return 0;
    }

    char *normalized_dir = gpa_normalize_path(dir_path);
    if (!normalized_dir)
    {
        gpa_log("gpa_get_files_in_directory: Failed to normalize path %s\n", dir_path);
        return 0;
    }

    uint32_t count = 0;
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        if (strncmp(g_gpa_archive->files[i]->path, normalized_dir, strlen(normalized_dir)) == 0)
        {
            count++;
        }
    }

    if (count == 0)
    {
        free(normalized_dir);
        *ids = NULL;
        gpa_log("gpa_get_files_in_directory: No files found in directory %s\n", dir_path);
        return 0;
    }

    *ids = malloc(count * sizeof(uint32_t));
    if (!*ids)
    {
        free(normalized_dir);
        gpa_log("gpa_get_files_in_directory: Memory allocation failed for %u IDs\n", count);
        return 0;
    }

    uint32_t index = 0;
    for (uint32_t i = 0; i < g_gpa_archive->file_count; i++)
    {
        if (strncmp(g_gpa_archive->files[i]->path, normalized_dir, strlen(normalized_dir)) == 0)
        {
            (*ids)[index++] = i;
        }
    }

    free(normalized_dir);
    gpa_log("gpa_get_files_in_directory: Found %u files in directory %s\n", count, dir_path);
    return count;
}

GPA_API void gpa_free_file_ids(uint32_t *ids)
{
    if (ids == NULL)
    {
        gpa_log("gpa_free_file_ids: Attempt to free NULL pointer\n");
        return;
    }

    free(ids);
    gpa_log("gpa_free_file_ids: Freed memory at %p\n", ids);
}
