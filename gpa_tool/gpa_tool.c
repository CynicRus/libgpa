/*MIT License

Copyright (c) 2025 Aleksandr Vorobev aka CynicRus

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "gpa.h"

// #define MAX_PATH 1024
#define VERSION "1.6.0"
// #define DEBUG
void print_error(int32_t err);

const char *extract_path(const char *fullPath)
{
    if (fullPath == NULL)
    {
        return NULL;
    }

    const char *lastSlash = strrchr(fullPath, '/');
    const char *lastBackslash = strrchr(fullPath, '\\');

    const char *lastSeparator = lastSlash > lastBackslash ? lastSlash : lastBackslash;

    if (lastSeparator == NULL)
    {
        char *result = (char *)malloc(3);
        if (result == NULL)
        {
            return NULL;
        }
        strcpy(result, "./");
        return result;
    }

    size_t pathLength = lastSeparator - fullPath + 1;
    char *path = (char *)malloc(pathLength + 1);
    if (path == NULL)
    {
        return NULL;
    }

    strncpy(path, fullPath, pathLength);
    path[pathLength] = '\0';
    return path;
}

void print_help(const char *prog_name)
{
    printf("\n");
    printf("gpa_tool by CynicRus version %s\n", VERSION);
    printf("\n");
    printf("Usage: %s <command> [options]\n", prog_name);
    printf("Commands:\n");
    printf("  create <archive>                     Create a new archive\n");
    printf("  add <archive> <file/dir> [compression] [encrypt] [key]  Add file or directory to archive\n");
    printf("  extract <archive> <id> <output> [key] Extract file by ID to output path\n");
    printf("  list <archive> [key]                List files in archive (key for encrypted archives)\n");
    printf("  delete <archive> <id>               Delete file by ID\n");
    printf("  rename <archive> <id> <newpath>     Rename file by ID\n");
    printf("  info <archive>                      Show archive information\n");
    printf("Options:\n");
    printf("  compression: 0 (None), 1 (Zlib)");
#ifdef GPA_WITH_LZMA
    printf(", 2 (LZMA)");
#endif
#ifdef GPA_WITH_ZSTD
    printf(", 3 (Zstd)");
#endif
#ifdef GPA_WITH_LZ4
    printf(", 4 (LZ4)");
#endif
#ifdef GPA_WITH_BZ2
    printf(", 5 (BZ2)");
#endif
    printf(", 6 (Custom)\n");
    printf("  encrypt: 0 (No encryption), 1 (Encrypt)\n");
    printf("  key: Encryption key (string, used if encrypt=1 or for extraction/listing)\n");
    printf("Examples:\n");
    printf("  %s create myarchive.gpa\n", prog_name);
    printf("  %s add myarchive.gpa file.txt 1 1 mysecretkey\n", prog_name);
    printf("  %s extract myarchive.gpa 1 output.txt mysecretkey\n", prog_name);
    printf("  %s list myarchive.gpa mysecretkey\n", prog_name);
}

uint8_t parse_compression(const char *comp_str)
{
    if (!comp_str)
        return GPA_COMPRESS_ZLIB;
    int comp = atoi(comp_str);
    if (comp < 0 || comp > 6)
        goto invalid_compression;

    // Check LZMA support
    if (comp == GPA_COMPRESS_LZMA)
    {
#ifdef GPA_WITH_LZMA
        return GPA_COMPRESS_LZMA;
#else
        fprintf(stderr, "LZMA compression not supported in this build\n");
        exit(1);
#endif
    }

    // Check Zstd support
    if (comp == GPA_COMPRESS_ZSTD)
    {
#ifdef GPA_WITH_ZSTD
        return GPA_COMPRESS_ZSTD;
#else
        fprintf(stderr, "Zstd compression not supported in this build\n");
        exit(1);
#endif
    }

    if (comp == GPA_COMPRESS_LZ4)
    {
#ifdef GPA_WITH_LZ4
        return GPA_COMPRESS_LZ4;
#else
        fprintf(stderr, "LZ4 compression not supported in this build\n");
        exit(1);
#endif
    }
    if (comp == GPA_COMPRESS_BZ2)
    {
#ifdef GPA_WITH_BZ2
        return GPA_COMPRESS_BZ2;
#else
        fprintf(stderr, "BZ2 compression not supported in this build\n");
        exit(1);
#endif
    }

    return (uint8_t)comp;

invalid_compression:
    fprintf(stderr, "Invalid compression type. Use 0 (None), 1 (Zlib)");
#ifdef GPA_WITH_LZMA
    fprintf(stderr, ", 2 (LZMA)");
#endif
#ifdef GPA_WITH_ZSTD
    fprintf(stderr, ", 3 (Zstd)");
#endif
#ifdef GPA_WITH_LZ4
    fprintf(stderr, ", 4 (LZ4)");
#endif
#ifdef GPA_WITH_BZ2
    fprintf(stderr, ", 5 (BZ2)");
#endif
    fprintf(stderr, ", 6 (Custom)\n");
    exit(1);
}

bool parse_encryption(const char *encrypt_str)
{
    if (!encrypt_str)
        return false;
    int encrypt = atoi(encrypt_str);
    if (encrypt != 0 && encrypt != 1)
    {
        fprintf(stderr, "Invalid encryption flag. Use 0 (No encryption) or 1 (Encrypt)\n");
        exit(1);
    }
    return encrypt == 1;
}

void set_encryption(const char *key_str, bool encrypt)
{
    if (encrypt && key_str)
    {
        size_t key_len = strlen(key_str);
        int32_t result = gpa_set_encryption(NULL, NULL, (const uint8_t *)key_str, key_len);
        if (result != GPA_OK)
        {
            print_error(result);
            exit(1);
        }
    }
}

// Заглушка для пользовательского сжатия
static int32_t custom_compress(const uint8_t *data, uint64_t size, uint8_t **out, uint64_t *out_size, void *user_data)
{
    // Пример: просто копируем данные
    *out = malloc(size);
    if (!*out)
        return GPA_ERR_MEMORY;
    memcpy(*out, data, size);
    *out_size = size;
    return GPA_OK;
}

static int32_t custom_decompress(const uint8_t *data, uint64_t size, uint64_t real_size, uint8_t **out, void *user_data)
{
    // Пример: просто копируем данные
    *out = malloc(real_size);
    if (!*out)
        return GPA_ERR_MEMORY;
    memcpy(*out, data, real_size);
    return GPA_OK;
}

void set_custom_compression(uint8_t compression_type)
{
    if (compression_type == GPA_COMPRESS_CUSTOM)
    {
        int32_t result = gpa_set_compression(custom_compress, custom_decompress, NULL);
        if (result != GPA_OK)
        {
            print_error(result);
            exit(1);
        }
    }
}

void print_error(int32_t err)
{
    const char *errors[] = {
        "Success",
        "Archive not loaded",
        "Archive already loaded",
        "Archive is empty",
        "File not found",
        "Archive file not found",
        "Invalid format",
        "I/O error",
        "Memory error",
        "Compression error",
        "Encryption error",
        "Invalid path",
        "Buffer too small",
        "Decryption failed"};
    if (err >= 0 && err <= 13)
    {
        fprintf(stderr, "Error: %s\n", errors[err]);
    }
    else
    {
        fprintf(stderr, "Unknown error: %d\n", err);
    }
}

int cmd_create(const char *archive)
{
    int32_t result = gpa_create_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }
    printf("Archive '%s' created successfully.\n", archive);
    gpa_close_archive(false);
    return 0;
}

int cmd_add(const char *archive, const char *path, const char *comp_str, const char *encrypt_str, const char *key_str)
{
    uint8_t compression = parse_compression(comp_str);
    bool encrypt = parse_encryption(encrypt_str);
    set_custom_compression(compression);
    set_encryption(key_str, encrypt);
    int32_t result = gpa_load_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }

    FILE *file = fopen(path, "rb");
    bool is_dir = (file == NULL);
    if (file)
        fclose(file);

    if (is_dir)
    {
        result = gpa_add_directory(path, compression, true, encrypt);
    }
    else
    {
        result = gpa_add_from_file(path, extract_path(path), compression, encrypt);
    }

    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    result = gpa_save_archive();
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    printf("Added '%s' to archive '%s' with compression %d and encryption %d.\n", path, archive, compression, encrypt);
    gpa_close_archive(false);
    return 0;
}

int cmd_extract(const char *archive, const char *id_str, const char *output, const char *key_str)
{
    uint32_t id = atoi(id_str);
    set_encryption(key_str, true);
    int32_t result = gpa_load_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }

    result = gpa_extract_to_file_by_id(id, output);
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    printf("Extracted file ID %u to '%s'.\n", id, output);
    gpa_close_archive(false);
    return 0;
}

int cmd_list(const char *archive, const char *key_str)
{
    if (key_str)
    {
        set_encryption(key_str, true);
    }
    int32_t result = gpa_load_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }

    uint32_t *ids = NULL;
    uint32_t count = gpa_get_files_in_directory("", &ids);
    if (count == 0)
    {
        printf("Archive is empty.\n");
        gpa_close_archive(false);
        return 0;
    }

    printf("ID\tSize\tReal Size\tCompression\tEncrypted\tName\n");
    for (uint32_t i = 0; i < count; i++)
    {
        uint64_t size = gpa_get_size_by_id(ids[i]);
        uint64_t real_size = gpa_get_real_size_by_id(ids[i]);
        const char *name = gpa_get_name_by_id(ids[i]);
        uint8_t compression_type = gpa_get_compression_type_by_id(ids[i]);
        bool encrypted = gpa_is_encrypted_by_id(ids[i]);
        const char *comp_str;
        switch (compression_type)
        {
        case GPA_COMPRESS_NONE:
            comp_str = "None";
            break;
        case GPA_COMPRESS_ZLIB:
            comp_str = "Zlib";
            break;
        case GPA_COMPRESS_LZMA:
            comp_str = "LZMA";
            break;
        case GPA_COMPRESS_ZSTD:
            comp_str = "Zstd";
            break;
        case GPA_COMPRESS_LZ4:
            comp_str = "LZ4";
            break;
        case GPA_COMPRESS_BZ2:
            comp_str = "BZ2";
            break;
        case GPA_COMPRESS_CUSTOM:
            comp_str = "Custom";
            break;
        default:
            comp_str = "Unknown";
        }
        printf("%u\t%llu\t%llu\t%s\t%s\t%s\n", ids[i], size, real_size, comp_str, encrypted ? "Yes" : "No", name ? name : "<null>");
    }

    free(ids);
    gpa_close_archive(false);
    return 0;
}

int cmd_delete(const char *archive, const char *id_str)
{
    uint32_t id = atoi(id_str);
    int32_t result = gpa_load_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }

    result = gpa_delete_by_id(id);
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    result = gpa_save_archive();
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    printf("Deleted file ID %u from archive '%s'.\n", id, archive);
    gpa_close_archive(false);
    return 0;
}

int cmd_rename(const char *archive, const char *id_str, const char *new_path)
{
    uint32_t id = atoi(id_str);
    int32_t result = gpa_load_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }

    result = gpa_rename_by_id(id, new_path);
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    result = gpa_save_archive();
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    printf("Renamed file ID %u to '%s' in archive '%s'.\n", id, new_path, archive);
    gpa_close_archive(false);
    return 0;
}

int cmd_info(const char *archive)
{
    int32_t result = gpa_load_archive(archive);
    if (result != GPA_OK)
    {
        print_error(result);
        return 1;
    }

    uint8_t version[16];
    result = gpa_get_version(version, sizeof(version));
    if (result != GPA_OK)
    {
        print_error(result);
        gpa_close_archive(false);
        return 1;
    }

    printf("Archive: %s\n", gpa_get_archive_name());
    printf("Version: %s\n", version);
    printf("Total Files: %u\n", gpa_get_total_files());
    printf("Total Size (Compressed): %llu bytes\n", gpa_get_archive_size());
    printf("Total Size (Uncompressed): %llu bytes\n", gpa_get_archive_real_size());
    printf("Changed: %s\n", gpa_archive_changed() ? "Yes" : "No");

    gpa_close_archive(false);
    return 0;
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        print_help(argv[0]);
        return 1;
    }

#ifdef DEBUG
    gpa_init_log("gpa_tool.log");
#endif

    const char *command = argv[1];

    if (strcmp(command, "create") == 0)
    {
        if (argc != 3)
        {
            fprintf(stderr, "Usage: %s create <archive>\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_create(argv[2]);
    }
    else if (strcmp(command, "add") == 0)
    {
        if (argc < 4 || argc > 7)
        {
            fprintf(stderr, "Usage: %s add <archive> <file/dir> [compression] [encrypt] [key]\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_add(argv[2], argv[3], argc > 4 ? argv[4] : NULL, argc > 5 ? argv[5] : NULL, argc > 6 ? argv[6] : NULL);
    }
    else if (strcmp(command, "extract") == 0)
    {
        if (argc < 5 || argc > 6)
        {
            fprintf(stderr, "Usage: %s extract <archive> <id> <output> [key]\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_extract(argv[2], argv[3], argv[4], argc > 5 ? argv[5] : NULL);
    }
    else if (strcmp(command, "list") == 0)
    {
        if (argc < 3 || argc > 4)
        {
            fprintf(stderr, "Usage: %s list <archive> [key]\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_list(argv[2], argc > 3 ? argv[3] : NULL);
    }
    else if (strcmp(command, "delete") == 0)
    {
        if (argc != 4)
        {
            fprintf(stderr, "Usage: %s delete <archive> <id>\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_delete(argv[2], argv[3]);
    }
    else if (strcmp(command, "rename") == 0)
    {
        if (argc != 5)
        {
            fprintf(stderr, "Usage: %s rename <archive> <id> <newpath>\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_rename(argv[2], argv[3], argv[4]);
    }
    else if (strcmp(command, "info") == 0)
    {
        if (argc != 3)
        {
            fprintf(stderr, "Usage: %s info <archive>\n", argv[0]);
#ifdef DEBUG
            gpa_close_log();
#endif
            return 1;
        }
        return cmd_info(argv[2]);
    }
    else
    {
        fprintf(stderr, "Unknown command: %s\n", command);
        print_help(argv[0]);
#ifdef DEBUG
        gpa_close_log();
#endif
        return 1;
    }

#ifdef DEBUG
    gpa_close_log();
#endif
    return 0;
}
