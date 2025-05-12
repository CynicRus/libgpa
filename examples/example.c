#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "gpa.h"

// Пример пользовательской функции сжатия (заглушка)
static int32_t my_custom_compress(const uint8_t *data, uint64_t size, uint8_t **out, uint64_t *out_size, void *user_data)
{
    printf("Custom compression called with size=%llu\n", size);
    *out = malloc(size);
    if (!*out)
        return GPA_ERR_MEMORY;
    memcpy(*out, data, size);
    *out_size = size;
    return GPA_OK;
}

// Пример пользовательской функции распаковки (заглушка)
static int32_t my_custom_decompress(const uint8_t *data, uint64_t size, uint64_t real_size, uint8_t **out, void *user_data)
{
    printf("Custom decompression called with size=%llu, real_size=%llu\n", size, real_size);
    *out = malloc(real_size);
    if (!*out)
        return GPA_ERR_MEMORY;
    memcpy(*out, data, real_size);
    return GPA_OK;
}

// Проверка результата операции и вывод ошибки
void check_result(int32_t result, const char *operation)
{
    if (result != GPA_OK)
    {
        fprintf(stderr, "Error in %s: %d\n", operation, result);
        exit(1);
    }
}

int main()
{
    // Включение логирования
    gpa_init_log("samples.log");

    // Пример 1: Создание нового архива
    printf("Example 1: Creating a new archive\n");
    check_result(gpa_create_archive("test.gpa"), "create archive");

    // Пример 2: Добавление файла без сжатия и шифрования
    printf("Example 2: Adding a file without compression or encryption\n");
    check_result(gpa_load_archive("test.gpa"), "load archive");
    check_result(gpa_add_from_file("sample.txt", "./", GPA_COMPRESS_NONE, false), "add file");
    check_result(gpa_save_archive(), "save archive");
    check_result(gpa_close_archive(false), "close archive");

    // Пример 3: Добавление файла с Zlib-сжатием и шифрованием
    printf("Example 3: Adding a file with Zlib compression and encryption\n");
    const char *key = "mysecretkey";
    check_result(gpa_load_archive("test.gpa"), "load archive");
    check_result(gpa_set_encryption(NULL, NULL, (const uint8_t *)key, strlen(key)), "set encryption");
    check_result(gpa_add_from_file("sample.txt", "./", GPA_COMPRESS_ZLIB, true), "add file");
    check_result(gpa_save_archive(), "save archive");
    check_result(gpa_close_archive(false), "close archive");

    // Пример 4: Добавление файла с пользовательским сжатием
    printf("Example 4: Adding a file with custom compression\n");
    check_result(gpa_load_archive("test.gpa"), "load archive");
    check_result(gpa_set_compression(my_custom_compress, my_custom_decompress, NULL), "set custom compression");
    check_result(gpa_add_from_file("sample.txt", "./", GPA_COMPRESS_CUSTOM, false), "add file");
    check_result(gpa_save_archive(), "save archive");
    check_result(gpa_close_archive(false), "close archive");

    // Пример 5: Извлечение файла по ID
    printf("Example 5: Extracting a file by ID\n");
    check_result(gpa_load_archive("test.gpa"), "load archive");
    check_result(gpa_set_encryption(NULL, NULL, (const uint8_t *)key, strlen(key)), "set encryption");
    check_result(gpa_extract_to_file_by_id(1, "extracted.txt"), "extract file");
    check_result(gpa_close_archive(false), "close archive");

    // Пример 6: Извлечение файла в память
    printf("Example 6: Extracting a file to memory\n");
    check_result(gpa_load_archive("test.gpa"), "load archive");
    uint8_t *data = NULL;
    uint64_t size = 0;
    check_result(gpa_extract_to_stream_by_id(0, &data, &size), "extract to stream");
    printf("Extracted %llu bytes to memory\n", size);
    free(data);
    check_result(gpa_close_archive(false), "close archive");

    // Пример 7: Получение ID файла по имени
    printf("Example 7: Getting file ID by name\n");
    check_result(gpa_load_archive("test.gpa"), "load archive");
    int32_t id = gpa_get_id_by_name("sample.txt", true);
    if (id >= 0)
        printf("Found file 'sample.txt' with ID %d\n", id);
    else
        printf("File 'sample.txt' not found\n");
    check_result(gpa_close_archive(false), "close archive");

    // Пример 8: Список файлов в архиве
    printf("Example 8: Listing files in archive\n");
    check_result(gpa_load_archive("test.gpa"), "load archive");
    uint32_t *ids = NULL;
    uint32_t count = gpa_get_files_in_directory("", &ids);
    printf("Files in archive:\n");
    for (uint32_t i = 0; i < count; i++)
    {
        printf("ID %u: %s (Size: %llu, Real Size: %llu, Compression: %u, Encrypted: %d)\n",
               ids[i], gpa_get_name_by_id(ids[i]), gpa_get_size_by_id(ids[i]),
               gpa_get_real_size_by_id(ids[i]), gpa_get_compression_type_by_id(ids[i]),
               gpa_is_encrypted_by_id(ids[i]));
    }
    free(ids);
    check_result(gpa_close_archive(false), "close archive");

    // Закрытие лога
    gpa_close_log();
    printf("All examples completed successfully.\n");
    return 0;
}