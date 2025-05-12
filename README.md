# GPA Library and Tool Documentation

## License
This documentation and the associated code are licensed under the MIT License.

## GPA Library

### Overview
The GPA (General Purpose Archive) library is a C-based library designed for creating, managing, and extracting archives. It supports multiple compression algorithms (Zlib, LZMA, Zstd, LZ4, BZip2, and custom compression) and optional encryption using XOR-based encryption or user-provided encryption functions. The library is cross-platform, supporting both Windows and Unix-like systems, and can be built as either a shared or static library.

### Features

Archive Management: Create, load, save, and close archives.

File Operations: Add files or directories, extract files, delete files, and rename files within an archive.

Compression Support:
- None (no compression)
- Zlib
- LZMA (optional, if enabled)
- Zstd (optional, if enabled)
- LZ4 (optional, if enabled)
- BZip2 (optional, if enabled)
- Custom compression (user-defined)

Encryption: Optional XOR-based encryption with a default or user-provided key, or custom encryption functions.

Logging: Optional logging to a file for debugging purposes.

Cross-Platform: Compatible with Windows and Unix-like systems.

Flexible API: Supports both file-based and stream-based operations.

### Build Instructions
The GPA library uses CMake for building. The following instructions assume you have CMake and a compatible C compiler installed.

#### Prerequisites

CMake: Version 3.10 or higher.

C Compiler: GCC, Clang, or MSVC.

Dependencies:
- Zlib: Required for Zlib compression.
- LibLZMA: Optional for LZMA compression (enable with ENABLE_LZMA).
- Zstd: Optional for Zstd compression (enable with ENABLE_ZSTD).
- LZ4: Optional for LZ4 compression (enable with ENABLE_LZ4).
- BZip2: Optional for BZip2 compression (enable with ENABLE_BZ2).

#### Build Steps

1. Clone the Repository:
```bash
git clone https://github.com/CynicRus/libgpa
cd gpa
```

2. Create a Build Directory:
```bash
mkdir build
cd build
```

3. Configure the Build:
Run CMake to configure the build. You can customize options as needed:
```bash
cmake .. -DBUILD_SHARED=ON -DENABLE_LZMA=ON -DENABLE_ZSTD=ON -DENABLE_LZ4=ON -DENABLE_BZ2=ON -DBUILD_EXAMPLES=ON
```

Available options:
- `-DBUILD_SHARED=ON|OFF`: Build as a shared library (default: ON).
- `-DENABLE_LZMA=ON|OFF`: Enable LZMA compression (default: ON).
- `-DENABLE_ZSTD=ON|OFF`: Enable Zstd compression (default: ON).
- `-DENABLE_LZ4=ON|OFF`: Enable LZ4 compression (default: ON).
- `-DENABLE_BZ2=ON|OFF`: Enable BZip2 compression (default: ON).
- `-DBUILD_EXAMPLES=ON|OFF`: Build example applications (default: ON).

If a dependency is not found and its corresponding option is enabled, CMake will fail with an error. Ensure dependencies are installed or disable the corresponding option.

4. Build the Library:
```bash
cmake --build . --config Release
```

5. Install the Library (Optional):
```bash
cmake --install .
```

This installs the library, headers, and CMake configuration files to the system (e.g., /usr/local on Unix-like systems or the specified install prefix).

#### Output

- Library: libgpa.so (Unix) or gpa.dll (Windows) in build/lib.
- Executable: gpa_tool in build/bin.
- Example: gpa_example in build/bin (if BUILD_EXAMPLES=ON).
- Headers: Installed to include/gpa.
- CMake Config: Installed to lib/cmake/gpa.

### Bindings
GPA also provides bindings for the FreePascal language, located in the fpc directory. There is also a graphical utility called GPA Manager that allows you to manage data in the archive through a graphical interface. This also serves as an example of using the library in FPC/Delphi.

![test image](https://raw.githubusercontent.com/CynicRus/libgpa/main/images/test.png)

### Usage Examples
The following examples demonstrate the core functionality of the GPA library using C code. Ensure the library is linked and the gpa.h header is included.

#### Example 1: Creating and Adding a File to an Archive
This example creates a new archive, adds a file with Zlib compression and encryption, and saves the archive.

```c
#include <gpa.h>
#include <stdio.h>

int main() {
    // Initialize logging (optional)
    gpa_init_log("gpa.log");

    // Create a new archive
    if (gpa_create_archive("test.gpa") != GPA_OK) {
        printf("Failed to create archive\n");
        return 1;
    }

    // Set encryption with a custom key
    const char* key = "mysecretkey";
    if (gpa_set_encryption(NULL, NULL, (const uint8_t*)key, strlen(key)) != GPA_OK) {
        printf("Failed to set encryption\n");
        gpa_close_archive(false);
        return 1;
    }

    // Add a file with Zlib compression and encryption
    if (gpa_add_from_file("example.txt", ".", GPA_COMPRESS_ZLIB, true) != GPA_OK) {
        printf("Failed to add file\n");
        gpa_close_archive(false);
        return 1;
    }

    // Save and close the archive
    if (gpa_save_archive() != GPA_OK) {
        printf("Failed to save archive\n");
        gpa_close_archive(false);
        return 1;
    }

    gpa_close_archive(false);
    gpa_close_log();
    printf("Archive created successfully\n");
    return 0;
}
```

#### Example 2: Extracting a File from an Archive
This example loads an archive, extracts a file by ID, and saves it to a specified path.

```c
#include <gpa.h>
#include <stdio.h>

int main() {
    // Initialize logging (optional)
    gpa_init_log("gpa.log");

    // Set encryption key for decryption
    const char* key = "mysecretkey";
    if (gpa_set_encryption(NULL, NULL, (const uint8_t*)key, strlen(key)) != GPA_OK) {
        printf("Failed to set encryption\n");
        return 1;
    }

    // Load an existing archive
    if (gpa_load_archive("test.gpa") != GPA_OK) {
        printf("Failed to load archive\n");
        return 1;
    }

    // Extract file with ID 0 to output.txt
    if (gpa_extract_to_file_by_id(0, "output.txt") != GPA_OK) {
        printf("Failed to extract file\n");
        gpa_close_archive(false);
        return 1;
    }

    gpa_close_archive(false);
    gpa_close_log();
    printf("File extracted successfully\n");
    return 0;
}
```

#### Example 3: Listing Files in an Archive
This example lists all files in an archive, displaying their IDs, sizes, compression types, and encryption status.

```c
#include <gpa.h>
#include <stdio.h>

int main() {
    // Initialize logging (optional)
    gpa_init_log("gpa.log");

    // Load an existing archive
    if (gpa_load_archive("test.gpa") != GPA_OK) {
        printf("Failed to load archive\n");
        return 1;
    }

    // Get all file IDs
    uint32_t* ids = NULL;
    uint32_t count = gpa_get_files_in_directory("", &ids);
    if (count == 0) {
        printf("Archive is empty\n");
        gpa_close_archive(false);
        return 0;
    }

    // Print file details
    printf("ID\tSize\tReal Size\tCompression\tEncrypted\tName\n");
    for (uint32_t i = 0; i < count; i++) {
        uint32_t id = ids[i];
        uint64_t size = gpa_get_size_by_id(id);
        uint64_t real_size = gpa_get_real_size_by_id(id);
        const char* name = gpa_get_name_by_id(id);
        uint8_t comp_type = gpa_get_compression_type_by_id(id);
        bool encrypted = gpa_is_encrypted_by_id(id);
        const char* comp_str = "Unknown";
        switch (comp_type) {
            case GPA_COMPRESS_NONE: comp_str = "None"; break;
            case GPA_COMPRESS_ZLIB: comp_str = "Zlib"; break;
            case GPA_COMPRESS_LZMA: comp_str = "LZMA"; break;
            case GPA_COMPRESS_ZSTD: comp_str = "Zstd"; break;
            case GPA_COMPRESS_LZ4: comp_str = "LZ4"; break;
            case GPA_COMPRESS_BZ2: comp_str = "BZ2"; break;
            case GPA_COMPRESS_CUSTOM: comp_str = "Custom"; break;
        }
        printf("%u\t%llu\t%llu\t%s\t%s\t%s\n", id, size, real_size, comp_str, encrypted ? "Yes" : "No", name);
    }

    free(ids);
    gpa_close_archive(false);
    gpa_close_log();
    return 0;
}
```

#### Example 4: Using Custom Compression
This example demonstrates how to define and use a custom compression function.

```c
#include <gpa.h>
#include <stdio.h>
#include <string.h>

// Custom compression function (example: simple copy)
int32_t custom_compress(const uint8_t* data, uint64_t size, uint8_t** out, uint64_t* out_size, void* user_data) {
    *out = malloc(size);
    if (!*out) return GPA_ERR_MEMORY;
    memcpy(*out, data, size);
    *out_size = size;
    return GPA_OK;
}

// Custom decompression function (example: simple copy)
int32_t custom_decompress(const uint8_t* data, uint64_t size, uint64_t real_size, uint8_t** out, void* user_data) {
    *out = malloc(real_size);
    if (!*out) return GPA_ERR_MEMORY;
    memcpy(*out, data, real_size);
    return GPA_OK;
}

int main() {
    // Initialize logging (optional)
    gpa_init_log("gpa.log");

    // Create a new archive
    if (gpa_create_archive("custom.gpa") != GPA_OK) {
        printf("Failed to create archive\n");
        return 1;
    }

    // Set custom compression
    if (gpa_set_compression(custom_compress, custom_decompress, NULL) != GPA_OK) {
        printf("Failed to set custom compression\n");
        gpa_close_archive(false);
        return 1;
    }

    // Add a file with custom compression
    if (gpa_add_from_file("example.txt", ".", GPA_COMPRESS_CUSTOM, false) != GPA_OK) {
        printf("Failed to add file\n");
        gpa_close_archive(false);
        return 1;
    }

    // Save and close the archive
    if (gpa_save_archive() != GPA_OK) {
        printf("Failed to save archive\n");
        gpa_close_archive(false);
        return 1;
    }

    gpa_close_archive(false);
    gpa_close_log();
    printf("Archive with custom compression created successfully\n");
    return 0;
}
```

### API Reference
The GPA library provides a rich set of functions for archive manipulation. Below is a summary of the key functions. Refer to gpa.h for the full API.

Archive Management:

- `gpa_create_archive(const char* filename)`: Create a new archive.
- `gpa_load_archive(const char* filename)`: Load an existing archive.
- `gpa_save_archive()`: Save the current archive.
- `gpa_close_archive(bool save)`: Close the archive, optionally saving changes.

File Operations:

- `gpa_add_from_file(const char* filename, const char* base_path, uint8_t compression_type, bool encrypt)`: Add a file to the archive.
- `gpa_add_from_stream(const uint8_t* data, uint64_t size, const char* path, uint8_t compression_type, bool encrypt)`: Add data from a memory buffer.
- `gpa_add_directory(const char* dir_path, uint8_t compression_type, bool recursive, bool encrypt)`: Add a directory (optionally recursive).
- `gpa_extract_to_file_by_id(uint32_t id, const char* filename)`: Extract a file by ID to a file.
- `gpa_extract_to_stream_by_id(uint32_t id, uint8_t** data, uint64_t* size)`: Extract a file by ID to a memory buffer.
- `gpa_extract_directory(const char* dir_path, const char* output_path)`: Extract a directory.
- `gpa_delete_by_id(uint32_t id)`: Delete a file by ID.
- `gpa_rename_by_id(uint32_t id, const char* new_path)`: Rename a file by ID.


Metadata:

- `gpa_get_id_by_name(const char* path, bool case_sensitive)`: Get the ID of a file by its path.
- `gpa_get_name_by_id(uint32_t id)`: Get the path of a file by its ID.
- `gpa_get_size_by_id(uint32_t id)`: Get the compressed size of a file.
- `gpa_get_real_size_by_id(uint32_t id)`: Get the uncompressed size of a file.
- `gpa_get_compression_type_by_id(uint32_t id)`: Get the compression type of a file.
- `gpa_is_encrypted_by_id(uint32_t id)`: Check if a file is encrypted.
- `gpa_get_total_files()`: Get the total number of files in the archive.
- `gpa_get_files_in_directory(const char* dir_path, uint32_t** ids)`: Get IDs of files in a directory.
- `gpa_free_file_ids(uint32_t *ids)`: release memory allocated for ids in gpa_get_files_in_directory.

Encryption and Compression:

- `gpa_set_encryption(GPAEncryptFunc encrypt_func, GPADecryptFunc decrypt_func, const uint8_t* key, uint64_t key_size)`: Set encryption functions and key.
- `gpa_set_compression(GPACompressFunc compress_func, GPADecompressFunc decompress_func, void* user_data)`: Set custom compression functions.

Logging:

- `gpa_init_log(const char* log_path)`: Initialize logging to a file.
- `gpa_close_log()`: Close the log file

Error Codes

The library returns error codes defined in enum GPAError:

  - `GPA_OK: Success.
  - `GPA_ERR_ARCHIVE_NOT_LOADED: No archive is loaded.
  - `GPA_ERR_ARCHIVE_ALREADY_LOADED: An archive is already loaded.
  - `GPA_ERR_ARCHIVE_EMPTY: The archive is empty.
  - `GPA_ERR_FILE_NOT_FOUND: File not found.
  - `GPA_ERR_ARCHIVE_FILE_NOT_FOUND: File not found in the archive.
  - `GPA_ERR_INVALID_FORMAT: Invalid archive format.
  - `GPA_ERR_IO: Input/output error.
  - `GPA_ERR_MEMORY: Memory allocation error.
  - `GPA_ERR_COMPRESSION: Compression/decompression error.
  - `GPA_ERR_ENCRYPTION: Encryption error.
  - `GPA_ERR_INVALID_PATH: Invalid file path.
  - `GPA_ERR_BUFFER_TO_SMALL: Buffer too small.
  - `GPA_ERR_DECRYPTION_FAILED: Decryption failed.
  - `GPA_ERR_INVALID_PARAM: Invalid parameter.

## GPA Tool

### Overview

The gpa_tool is a command-line utility built on top of the GPA library. It provides a user-friendly interface for interacting with GPA archives, allowing users to create archives, add files or directories, extract files, list archive contents, delete files, rename files, and display archive information.

### Build Instructions

The gpa_tool is built automatically as part of the GPA library build process (see the "Build Instructions" section above). The resulting executable is located in `build/bin/gpa_tool`.

### Usage

Run gpa_tool with the following syntax:

```
gpa_tool <command> [options]
```

#### Commands

- **create \<archive\>**: Create a new archive.
- **add \<archive\> \<file/dir\> [compression] [encrypt] [key]**: Add a file or directory to an archive.
- **extract \<archive\> \<id\> \<output\> [key]**: Extract a file by ID to an output path.
- **list \<archive\> [key]**: List all files in the archive.
- **delete \<archive\> \<id\>**: Delete a file by ID.
- **rename \<archive\> \<id\> \<newpath\>**: Rename a file by ID.
- **info \<archive\>**: Display archive information.

#### Options

- **compression**: Compression type (default: 1):
  - 0: None
  - 1: Zlib
  - 2: LZMA (if enabled)
  - 3: Zstd (if enabled)
  - 4: LZ4 (if enabled)
  - 5: BZip2 (if enabled)
  - 6: Custom

- **encrypt**: Encryption flag (default: 0):
  - 0: No encryption
  - 1: Encrypt

- **key**: Encryption key (string, required if encrypt=1 or for extracting/listing encrypted archives).

### Examples

#### Create a New Archive:
```
gpa_tool create myarchive.gpa
```
Output: Archive 'myarchive.gpa' created successfully.

#### Add a File with Zlib Compression and Encryption:
```
gpa_tool add myarchive.gpa file.txt 1 1 mysecretkey
```
Output: Added 'file.txt' to archive 'myarchive.gpa' with compression 1 and encryption 1.

#### Add a Directory Recursively:
```
gpa_tool add myarchive.gpa myfolder 3 0
```
Output: Added 'myfolder' to archive 'myarchive.gpa' with compression 3 and encryption 0.

#### Extract a File by ID:
```
gpa_tool extract myarchive.gpa 0 output.txt mysecretkey
```
Output: Extracted file ID 0 to 'output.txt'.

#### List Archive Contents:
```
gpa_tool list myarchive.gpa mysecretkey
```
Output:
```
ID  Size  Real Size  Compression  Encrypted  Name
0   1024  2048       Zlib         Yes        file.txt
1   512   1024       Zstd         No         myfolder/doc.txt
```

#### Delete a File by ID:
```
gpa_tool delete myarchive.gpa 0
```
Output: Deleted file ID 0 from archive 'myarchive.gpa'.

#### Rename a File by ID:
```
gpa_tool rename myarchive.gpa 1 newpath/doc.txt
```
Output: Renamed file ID 1 to 'newpath/doc.txt' in archive 'myarchive.gpa'.

### Display Archive Information:
```
gpa_tool info myarchive.gpa
```
Output:
```
Archive: myarchive.gpa
Version: 1.6.0
Total Files: 2
Total Size (Compressed): 1536 bytes
Total Size (Uncompressed): 3072 bytes
Changed: No
```

### Error Handling

The tool prints descriptive error messages based on the GPAError codes (see the "Error Codes" section above). For example:

```
gpa_tool extract myarchive.gpa 999 output.txt
Error: Archive file not found
```

### Debugging

To enable logging for debugging, compile the tool with the DEBUG macro defined:

```c
#define DEBUG
```

This creates a log file named `gpa_tool.log` in the current directory, containing detailed information about the tool's operations.

### Contributing

Contributions to the GPA library and tool are welcome! Please submit pull requests or issues to the project's repository. Ensure that any contributions adhere to the MIT License.

### Contact

For questions or support, open an issue on the project's repository.
