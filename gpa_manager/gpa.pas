unit gpa;

{$ifdef FPC}
  {$mode Delphi}
{$endif}

interface

uses
  Classes, SysUtils
  {$IFNDEF GPA_STATIC}
  , DynLibs
  {$ENDIF};

const
  GPA_VERSION_MAJOR = 1;
  GPA_VERSION_MINOR = 5;
  GPA_VERSION_PATCH = 0;

  // Коды ошибок
  GPA_OK = 0;
  GPA_ERR_ARCHIVE_NOT_LOADED = 1;
  GPA_ERR_ARCHIVE_ALREADY_LOADED = 2;
  GPA_ERR_ARCHIVE_EMPTY = 3;
  GPA_ERR_FILE_NOT_FOUND = 4;
  GPA_ERR_ARCHIVE_FILE_NOT_FOUND = 5;
  GPA_ERR_INVALID_FORMAT = 6;
  GPA_ERR_IO = 7;
  GPA_ERR_MEMORY = 8;
  GPA_ERR_COMPRESSION = 9;
  GPA_ERR_ENCRYPTION = 10;
  GPA_ERR_INVALID_PATH = 11;
  GPA_ERR_BUFFER_TO_SMALL = 12;
  GPA_ERR_DECRYPTION_FAILED = 13;
  GPA_ERR_INVALID_PARAM = 14;

  // Типы компрессии
  GPA_COMPRESS_NONE = 0;
  GPA_COMPRESS_ZLIB = 1;
{$IFDEF GPA_WITH_LZMA}
  GPA_COMPRESS_LZMA = 2;
{$ENDIF}
{$IFDEF GPA_WITH_ZSTD}
  GPA_COMPRESS_ZSTD = 3;
{$ENDIF}
{$IFDEF GPA_WITH_LZ4}
  GPA_COMPRESS_LZ4 = 4;
{$ENDIF}
{$IFDEF GPA_WITH_BZ2}
  GPA_COMPRESS_BZ2 = 5;
{$ENDIF}
  GPA_COMPRESS_CUSTOM = 6;

type
  TByteArray = array of Byte;
  TInt32 = LongInt;
  TUInt8 = Byte;
  TUInt32 = LongWord;
  TUInt64 = QWord;
  PByte = ^Byte;
  PPByte = ^PByte;
  PUInt32 = ^TUInt32;
  PPUInt32 = ^PUInt32;
  TUIntArray = array of UInt32;

  // Типы функций шифрования/дешифрования
  TGPAEncryptFunc = procedure(data: PByte; size: TUInt64; key: PByte; key_size: TUInt64); cdecl;
  TGPADecryptFunc = procedure(data: PByte; size: TUInt64; key: PByte; key_size: TUInt64); cdecl;

  // Типы функций компрессии/декомпрессии
  TGPACompressFunc = function(input: PByte; input_size: TUInt64; output: PPByte; output_size: PUInt64; user_data: Pointer): TInt32; cdecl;
  TGPADecompressFunc = function(input: PByte; input_size: TUInt64; real_size: TUInt64; output: PPByte; user_data: Pointer): TInt32; cdecl;

{$IFNDEF GPA_STATIC}
var
  LibHandle: TLibHandle = NilHandle;
  gpa_init_log: function(log_path: PChar): TInt32; cdecl;
  gpa_close_log: procedure; cdecl;
  gpa_get_version: function(buffer: PByte; buffer_size: NativeUInt): TInt32; cdecl;
  gpa_create_archive: function(filename: PChar): TInt32; cdecl;
  gpa_load_archive: function(filename: PChar): TInt32; cdecl;
  gpa_save_archive: function: TInt32; cdecl;
  gpa_close_archive: function(save: Boolean): TInt32; cdecl;
  gpa_set_encryption: function(encrypt_func: TGPAEncryptFunc; decrypt_func: TGPADecryptFunc; key: PByte; key_size: TUInt64): TInt32; cdecl;
  gpa_set_compression: function(compress_func: TGPACompressFunc; decompress_func: TGPADecompressFunc; user_data: Pointer): TInt32; cdecl;
  gpa_add_from_file: function(filename: PChar;base_path:Pchar; compression_type: TUInt8; encrypt: Boolean): TInt32; cdecl;
  gpa_add_from_stream: function(data: PByte; size: TUInt64; path: PChar; compression_type: TUInt8; encrypt: Boolean): TInt32; cdecl;
  gpa_add_directory: function(dir_path: PChar; compression_type: TUInt8; recursive: Boolean; encrypt: Boolean): TInt32; cdecl;
  gpa_extract_to_file_by_id: function(id: TUInt32; filename: PChar): TInt32; cdecl;
  gpa_extract_to_stream_by_id: function(id: TUInt32; data: PPByte; size: PUInt64): TInt32; cdecl;
  gpa_extract_directory: function(dir_path: PChar; output_path: PChar): TInt32; cdecl;
  gpa_delete_by_id: function(id: TUInt32): TInt32; cdecl;
  gpa_rename_by_id: function(id: TUInt32; new_path: PChar): TInt32; cdecl;
  gpa_get_id_by_name: function(path: PChar; case_sensitive: Boolean): TInt32; cdecl;
  gpa_get_name_by_id: function(id: TUInt32): PChar; cdecl;
  gpa_get_size_by_id: function(id: TUInt32): TUInt64; cdecl;
  gpa_get_real_size_by_id: function(id: TUInt32): TUInt64; cdecl;
  gpa_get_archive_size: function: TUInt64; cdecl;
  gpa_get_archive_real_size: function: TUInt64; cdecl;
  gpa_get_archive_name: function: PChar; cdecl;
  gpa_get_total_files: function: TUInt32; cdecl;
  gpa_archive_active: function: Boolean; cdecl;
  gpa_archive_changed: function: Boolean; cdecl;
  gpa_get_files_in_directory: function(dir_path: PChar; ids: PPUInt32): TUInt32; cdecl;
  gpa_get_compression_type_by_id: function(id: TUInt32): TUInt8; cdecl;
  gpa_is_encrypted_by_id: function(id: TUInt32): Boolean; cdecl;
  gpa_free_file_ids: procedure(Ids: PUint32); cdecl;

  function LoadGPALibrary(const LibName: string): Boolean;
  procedure UnloadGPALibrary;
{$ELSE}
function gpa_init_log(log_path: PChar): TInt32; cdecl; external 'gpa' name 'gpa_init_log';
procedure gpa_close_log; cdecl; external 'gpa' name 'gpa_close_log';
function gpa_get_version(buffer: PByte; buffer_size: NativeUInt): TInt32; cdecl; external 'gpa' name 'gpa_get_version';
function gpa_create_archive(filename: PChar): TInt32; cdecl; external 'gpa' name 'gpa_create_archive';
function gpa_load_archive(filename: PChar): TInt32; cdecl; external 'gpa' name 'gpa_load_archive';
function gpa_save_archive: TInt32; cdecl; external 'gpa' name 'gpa_save_archive';
function gpa_close_archive(save: Boolean): TInt32; cdecl; external 'gpa' name 'gpa_close_archive';
function gpa_set_encryption(encrypt_func: TGPAEncryptFunc; decrypt_func: TGPADecryptFunc; key: PByte; key_size: TUInt64): TInt32; cdecl; external 'gpa' name 'gpa_set_encryption';
function gpa_set_compression(compress_func: TGPACompressFunc; decompress_func: TGPADecompressFunc; user_data: Pointer): TInt32; cdecl; external 'gpa' name 'gpa_set_compression';
function gpa_add_from_file(filename: PChar; base_path:PChar; compression_type: TUInt8; encrypt: Boolean): TInt32; cdecl; external 'gpa' name 'gpa_add_from_file';
function gpa_add_from_stream(data: PByte; size: TUInt64; path: PChar; compression_type: TUInt8; encrypt: Boolean): TInt32; cdecl; external 'gpa' name 'gpa_add_from_stream';
function gpa_add_directory(dir_path: PChar; compression_type: TUInt8; recursive: Boolean; encrypt: Boolean): TInt32; cdecl; external 'gpa' name 'gpa_add_directory';
function gpa_extract_to_file_by_id(id: TUInt32; filename: PChar): TInt32; cdecl; external 'gpa' name 'gpa_extract_to_file_by_id';
function gpa_extract_to_stream_by_id(id: TUInt32; data: PPByte; size: PUInt64): TInt32; cdecl; external 'gpa' name 'gpa_extract_to_stream_by_id';
function gpa_extract_directory(dir_path: PChar; output_path: PChar): TInt32; cdecl; external 'gpa' name 'gpa_extract_directory';
function gpa_delete_by_id(id: TUInt32): TInt32; cdecl; external 'gpa' name 'gpa_delete_by_id';
function gpa_rename_by_id(id: TUInt32; new_path: PChar): TInt32; cdecl; external 'gpa' name 'gpa_rename_by_id';
function gpa_get_id_by_name(path: PChar; case_sensitive: Boolean): TInt32; cdecl; external 'gpa' name 'gpa_get_id_by_name';
function gpa_get_name_by_id(id: TUInt32): PChar; cdecl; external 'gpa' name 'gpa_get_name_by_id';
function gpa_get_size_by_id(id: TUInt32): TUInt64; cdecl; external 'gpa' name 'gpa_get_size_by_id';
function gpa_get_real_size_by_id(id: TUInt32): TUInt64; cdecl; external 'gpa' name 'gpa_get_real_size_by_id';
function gpa_get_archive_size: TUInt64; cdecl; external 'gpa' name 'gpa_get_archive_size';
function gpa_get_archive_real_size: TUInt64; cdecl; external 'gpa' name 'gpa_get_archive_real_size';
function gpa_get_archive_name: PChar; cdecl; external 'gpa' name 'gpa_get_archive_name';
function gpa_get_total_files: TUInt32; cdecl; external 'gpa' name 'gpa_get_total_files';
function gpa_archive_active: Boolean; cdecl; external 'gpa' name 'gpa_archive_active';
function gpa_archive_changed: Boolean; cdecl; external 'gpa' name 'gpa_archive_changed';
function gpa_get_files_in_directory(dir_path: PChar; ids: PPUInt32): TUInt32; cdecl; external 'gpa' name 'gpa_get_files_in_directory';
function gpa_get_compression_type_by_id(id: TUInt32): TUInt8; cdecl; external 'gpa' name 'gpa_get_compression_type_by_id';
function gpa_is_encrypted_by_id(id: TUInt32): Boolean; cdecl; external 'gpa' name 'gpa_is_encrypted_by_id';
procedure gpa_free_file_ids(ids: PUint32); cdecl; external 'gpa' name 'gpa_free_file_ids';
{$ENDIF}

implementation

{$IFNDEF GPA_STATIC}
function LoadGPALibrary(const LibName: string): Boolean;
var
  FullLibName: string;
begin
  if LibHandle <> NilHandle then
    Exit(True);

  {$IFDEF WINDOWS}
  FullLibName := LibName + '.dll';
  {$ELSE}
  FullLibName := 'lib' + LibName + '.so';
  {$ENDIF}
  LibHandle := LoadLibrary(FullLibName);
  if LibHandle = NilHandle then
    Exit(False);

  @gpa_init_log := GetProcAddress(LibHandle, 'gpa_init_log');
  @gpa_close_log := GetProcAddress(LibHandle, 'gpa_close_log');
  @gpa_get_version := GetProcAddress(LibHandle, 'gpa_get_version');
  @gpa_create_archive := GetProcAddress(LibHandle, 'gpa_create_archive');
  @gpa_load_archive := GetProcAddress(LibHandle, 'gpa_load_archive');
  @gpa_save_archive := GetProcAddress(LibHandle, 'gpa_save_archive');
  @gpa_close_archive := GetProcAddress(LibHandle, 'gpa_close_archive');
  @gpa_set_encryption := GetProcAddress(LibHandle, 'gpa_set_encryption');
  @gpa_set_compression := GetProcAddress(LibHandle, 'gpa_set_compression');
  @gpa_add_from_file := GetProcAddress(LibHandle, 'gpa_add_from_file');
  @gpa_add_from_stream := GetProcAddress(LibHandle, 'gpa_add_from_stream');
  @gpa_add_directory := GetProcAddress(LibHandle, 'gpa_add_directory');
  @gpa_extract_to_file_by_id := GetProcAddress(LibHandle, 'gpa_extract_to_file_by_id');
  @gpa_extract_to_stream_by_id := GetProcAddress(LibHandle, 'gpa_extract_to_stream_by_id');
  @gpa_extract_directory := GetProcAddress(LibHandle, 'gpa_extract_directory');
  @gpa_delete_by_id := GetProcAddress(LibHandle, 'gpa_delete_by_id');
  @gpa_rename_by_id := GetProcAddress(LibHandle, 'gpa_rename_by_id');
  @gpa_get_id_by_name := GetProcAddress(LibHandle, 'gpa_get_id_by_name');
  @gpa_get_name_by_id := GetProcAddress(LibHandle, 'gpa_get_name_by_id');
  @gpa_get_size_by_id := GetProcAddress(LibHandle, 'gpa_get_size_by_id');
  @gpa_get_real_size_by_id := GetProcAddress(LibHandle, 'gpa_get_real_size_by_id');
  @gpa_get_archive_size := GetProcAddress(LibHandle, 'gpa_get_archive_size');
  @gpa_get_archive_real_size := GetProcAddress(LibHandle, 'gpa_get_archive_real_size');
  @gpa_get_archive_name := GetProcAddress(LibHandle, 'gpa_get_archive_name');
  @gpa_get_total_files := GetProcAddress(LibHandle, 'gpa_get_total_files');
  @gpa_archive_active := GetProcAddress(LibHandle, 'gpa_archive_active');
  @gpa_archive_changed := GetProcAddress(LibHandle, 'gpa_archive_changed');
  @gpa_get_files_in_directory := GetProcAddress(LibHandle, 'gpa_get_files_in_directory');
  @gpa_get_compression_type_by_id := GetProcAddress(LibHandle, 'gpa_get_compression_type_by_id');
  @gpa_is_encrypted_by_id := GetProcAddress(LibHandle, 'gpa_is_encrypted_by_id');
  @gpa_free_file_ids := GetProcAddress(LibHandle, 'gpa_free_file_ids');

  Result := (@gpa_init_log <> nil) and (@gpa_close_log <> nil) and
            (@gpa_get_version <> nil) and (@gpa_create_archive <> nil) and
            (@gpa_load_archive <> nil) and (@gpa_save_archive <> nil) and
            (@gpa_close_archive <> nil) and (@gpa_set_encryption <> nil) and
            (@gpa_set_compression <> nil) and (@gpa_add_from_file <> nil) and
            (@gpa_add_from_stream <> nil) and (@gpa_add_directory <> nil) and
            (@gpa_extract_to_file_by_id <> nil) and (@gpa_extract_to_stream_by_id <> nil) and
            (@gpa_extract_directory <> nil) and (@gpa_delete_by_id <> nil) and
            (@gpa_rename_by_id <> nil) and (@gpa_get_id_by_name <> nil) and
            (@gpa_get_name_by_id <> nil) and (@gpa_get_size_by_id <> nil) and
            (@gpa_get_real_size_by_id <> nil) and (@gpa_get_archive_size <> nil) and
            (@gpa_get_archive_real_size <> nil) and (@gpa_get_archive_name <> nil) and
            (@gpa_get_total_files <> nil) and (@gpa_archive_active <> nil) and
            (@gpa_archive_changed <> nil) and (@gpa_get_files_in_directory <> nil) and
            (@gpa_free_file_ids<> nil);

  if not Result then
  begin
    UnloadGPALibrary;
    Exit(False);
  end;
end;

procedure UnloadGPALibrary;
begin
  if LibHandle <> NilHandle then
  begin
    FreeLibrary(LibHandle);
    LibHandle := NilHandle;
    @gpa_init_log := nil;
    @gpa_close_log := nil;
    @gpa_get_version := nil;
    @gpa_create_archive := nil;
    @gpa_load_archive := nil;
    @gpa_save_archive := nil;
    @gpa_close_archive := nil;
    @gpa_set_encryption := nil;
    @gpa_set_compression := nil;
    @gpa_add_from_file := nil;
    @gpa_add_from_stream := nil;
    @gpa_add_directory := nil;
    @gpa_extract_to_file_by_id := nil;
    @gpa_extract_to_stream_by_id := nil;
    @gpa_extract_directory := nil;
    @gpa_delete_by_id := nil;
    @gpa_rename_by_id := nil;
    @gpa_get_id_by_name := nil;
    @gpa_get_name_by_id := nil;
    @gpa_get_size_by_id := nil;
    @gpa_get_real_size_by_id := nil;
    @gpa_get_archive_size := nil;
    @gpa_get_archive_real_size := nil;
    @gpa_get_archive_name := nil;
    @gpa_get_total_files := nil;
    @gpa_archive_active := nil;
    @gpa_archive_changed := nil;
    @gpa_get_files_in_directory := nil;
    @gpa_get_compression_type_by_id := nil;
    @gpa_is_encrypted_by_id := nil;
  end;
end;
{$ENDIF}

end.
