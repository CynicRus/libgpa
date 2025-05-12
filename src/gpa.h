#ifndef GPA_H
#define GPA_H

/*MIT License

Copyright (c) 2025 Aleksand Vorobev aka CynicRus

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnishedogra do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*/

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <zlib.h>
#ifdef GPA_WITH_LZMA
#include <lzma.h>
#endif
#ifdef GPA_WITH_ZSTD
#include <zstd.h>
#endif
#ifdef GPA_WITH_LZ4
#include <lz4.h>
#endif
#ifdef GPA_WITH_BZ2
#include <bzlib.h>
#endif

#ifdef _WIN32
    #ifdef GPA_BUILD_DLL
        #define GPA_API __declspec(dllexport)
    #else
        #define GPA_API __declspec(dllimport)
    #endif
#else
    #define GPA_API __attribute__((visibility("default")))
#endif

#define GPA_VERSION_MAJOR 1
#define GPA_VERSION_MINOR 6
#define GPA_VERSION_PATCH 0

// Коды ошибок
enum GPAError {
    GPA_OK = 0,
    GPA_ERR_ARCHIVE_NOT_LOADED = 1,
    GPA_ERR_ARCHIVE_ALREADY_LOADED = 2,
    GPA_ERR_ARCHIVE_EMPTY = 3,
    GPA_ERR_FILE_NOT_FOUND = 4,
    GPA_ERR_ARCHIVE_FILE_NOT_FOUND = 5,
    GPA_ERR_INVALID_FORMAT = 6,
    GPA_ERR_IO = 7,
    GPA_ERR_MEMORY = 8,
    GPA_ERR_COMPRESSION = 9,
    GPA_ERR_ENCRYPTION = 10,
    GPA_ERR_INVALID_PATH = 11,
    GPA_ERR_BUFFER_TO_SMALL = 12,
    GPA_ERR_DECRYPTION_FAILED = 13,
    GPA_ERR_INVALID_PARAM = 14
};

// Типы компрессии
enum GPACompressionType {
    GPA_COMPRESS_NONE = 0,
    GPA_COMPRESS_ZLIB = 1,
    GPA_COMPRESS_LZMA = 2,
    GPA_COMPRESS_ZSTD = 3,
    GPA_COMPRESS_LZ4 = 4,
    GPA_COMPRESS_BZ2 = 5,
    GPA_COMPRESS_CUSTOM = 6
};

// Типы функций шифрования/дешифрования
typedef void (*GPAEncryptFunc)(uint8_t* data, uint64_t size, const uint8_t* key, uint64_t key_size);
typedef void (*GPADecryptFunc)(uint8_t* data, uint64_t size, const uint8_t* key, uint64_t key_size);

// Типы функций компрессии/декомпрессии
typedef int32_t (*GPACompressFunc)(const uint8_t* input, uint64_t input_size, uint8_t** output, uint64_t* output_size, void* user_data);
typedef int32_t (*GPADecompressFunc)(const uint8_t* input, uint64_t input_size, uint64_t real_size, uint8_t** output, void* user_data);

// Структура записи файла
typedef struct {
    char* path;         // Относительный путь файла
    uint8_t path_len;   // Длина пути
    uint64_t offset;    // Смещение в архиве
    uint64_t size;      // Сжатый размер
    uint64_t real_size; // Несжатый размер
    uint8_t* data;      // Данные (сжатые или несжатые)
    uint8_t compression_type; // Тип компрессии (GPACompressionType)
    bool encrypted;     // Флаг шифрования
} GPAFile;

// Структура архива
typedef struct {
    char* filename;     // Имя файла архива
    FILE* file;         // Дескриптор файла
    char id[4];         // Идентификатор ("GPA ")
    uint8_t version[4]; // Версия (1.6.0.0)
    uint32_t file_count;// Количество файлов
    bool changed;       // Флаг изменения
    GPAFile** files;    // Массив файлов
    uint32_t files_capacity; // Выделенная емкость
} GPAArchive;

typedef struct {
    GPAEncryptFunc encrypt_func;  // Пользовательская функция шифрования
    GPADecryptFunc decrypt_func;  // Пользовательская функция дешифрования
    uint8_t* key;                // Ключ шифрования (копия)
    uint64_t key_size;           // Размер ключа
} GPAEncryptionContext;

typedef struct {
    GPACompressFunc compress_func;
    GPADecompressFunc decompress_func;
    void *user_data; // Пользовательские данные для компрессии
} GPACompressionContext;

// Глобальное состояние архива
GPA_API extern GPAArchive* g_gpa_archive;
GPA_API extern bool g_archive_active;
GPA_API extern GPAEncryptionContext* g_encryption_context;
GPA_API extern GPACompressionContext* g_compression_context;
extern FILE* g_log_file;

static char *gpa_normalize_path(const char *path);

GPA_API void gpa_init_log(const char* log_path);
GPA_API void gpa_close_log();
GPA_API int32_t gpa_get_version(uint8_t* buffer, size_t buffer_size);
GPA_API int32_t gpa_create_archive(const char* filename);
GPA_API int32_t gpa_load_archive(const char* filename);
GPA_API int32_t gpa_save_archive(void);
GPA_API int32_t gpa_close_archive(bool save);
GPA_API int32_t gpa_set_encryption(GPAEncryptFunc encrypt_func, GPADecryptFunc decrypt_func, const uint8_t* key, uint64_t key_size);
GPA_API int32_t gpa_set_compression(GPACompressFunc compress_func, GPADecompressFunc decompress_func, void* user_data);
GPA_API int32_t gpa_add_from_file(const char *filename, const char *base_path, uint8_t compression_type, bool encrypt);
GPA_API int32_t gpa_add_from_stream(const uint8_t* data, uint64_t size, const char* path, uint8_t compression_type, bool encrypt);
GPA_API int32_t gpa_add_directory(const char* dir_path, uint8_t compression_type, bool recursive, bool encrypt);
GPA_API int32_t gpa_extract_to_file_by_id(uint32_t id, const char* filename);
GPA_API int32_t gpa_extract_to_stream_by_id(uint32_t id, uint8_t** data, uint64_t* size);
GPA_API int32_t gpa_extract_directory(const char* dir_path, const char* output_path);
GPA_API int32_t gpa_delete_by_id(uint32_t id);
GPA_API int32_t gpa_rename_by_id(uint32_t id, const char* new_path);
GPA_API int32_t gpa_get_id_by_name(const char* path, bool case_sensitive);
GPA_API const char* gpa_get_name_by_id(uint32_t id);
GPA_API uint64_t gpa_get_size_by_id(uint32_t id);
GPA_API uint64_t gpa_get_real_size_by_id(uint32_t id);
GPA_API uint8_t gpa_get_compression_type_by_id(uint32_t id);
GPA_API bool gpa_is_encrypted_by_id(uint32_t id);
GPA_API uint64_t gpa_get_archive_size(void);
GPA_API uint64_t gpa_get_archive_real_size(void);
GPA_API const char* gpa_get_archive_name(void);
GPA_API uint32_t gpa_get_total_files(void);
GPA_API bool gpa_archive_active(void);
GPA_API bool gpa_archive_changed(void);
GPA_API uint32_t gpa_get_files_in_directory(const char* dir_path, uint32_t** ids);
GPA_API void gpa_free_file_ids(uint32_t *ids);

#endif // GPA_H