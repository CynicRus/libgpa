unit gpa_wrapper;

{$ifdef FPC}
  {$mode Delphi}
{$endif}

interface

uses
  Classes, SysUtils, gpa;

type
  TByteArray = array of byte;
  TCompressionType = (ctNone, ctZlib, ctLZMA, ctZstd, ctLZ4, ctBZ2, ctCustom);

  { TGPAArchive }

  TGPAArchive = class
  private
    FArchiveName: string;
    FErrorCode: TInt32;
    function GetArchiveRealSize: int64;
    function GetArchiveSize: int64;
    function GetFileCount: integer;
    function GetIsArchiveActive: boolean;
    function GetIsArchiveChanged: boolean;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure CreateArchive(const FileName: string);
    function LoadArchive(const FileName: string): boolean;
    function SaveArchive: boolean;
    function CloseArchive(Save: boolean): boolean;
    function AddFile(const FileName, BasePath: string; Compression: TCompressionType;
      Encrypt: boolean): boolean;
    function AddDirectory(const DirName: string; Compression: TCompressionType;
      Recursive: boolean; Encrypt: boolean): boolean;
    function ExtractFileById(FileID: integer; const DestFileName: string): boolean;
    function DeleteFileById(FileID: integer): boolean;
    function RenameFileById(FileID: integer; const NewName: string): boolean;
    function GetFilesInDirectory(const DirPath: string): TUintArray;
    function GetFileNameById(FileID: integer): string;
    function GetFileSizeById(FileID: integer): int64;
    function GetFileRealSizeById(FileID: integer): int64;
    function GetCompressionTypeById(FileID: integer): TCompressionType;
    function IsFileEncryptedById(FileID: integer): boolean;
    procedure SetEncryption(EncryptFunc: TGPAEncryptFunc;
      DecryptFunc: TGPADecryptFunc; const Key: TByteArray);
    procedure SetCompression(CompressFunc: TGPACompressFunc;
      DecompressFunc: TGPADecompressFunc; UserData: Pointer);
    procedure InitLog(const LogFileName: string);
    procedure CloseLog();
    function GetLastError: TInt32;

    property ArchiveName: string read FArchiveName;
    property FileCount: integer read GetFileCount;
    property ArchiveSize: int64 read GetArchiveSize;
    property ArchiveRealSize: int64 read GetArchiveRealSize;
    property IsArchiveActive: boolean read GetIsArchiveActive;
    property IsArchiveChanged: boolean read GetIsArchiveChanged;
  end;

implementation

{ TGPAArchive }

constructor TGPAArchive.Create;
begin
  {$IFNDEF GPA_STATIC}
  if not LoadGPALibrary('gpa') then
    raise Exception.Create('Failed to load GPA library');
  {$ENDIF}
  FArchiveName := '';
end;

procedure TGPAArchive.CreateArchive(const FileName: string);
var
  res: TInt32;
begin
  FArchiveName := FileName;
  res := gpa_create_archive(PChar(ArchiveName));
  FErrorCode := res;
end;

destructor TGPAArchive.Destroy;
begin
  if IsArchiveActive then
    CloseArchive(False);
  {$IFNDEF GPA_STATIC}
     UnloadGPALibrary;
  {$ENDIF}
  inherited Destroy;
end;

function TGPAArchive.GetArchiveRealSize: int64;
begin
  Result := gpa_get_archive_real_size;
end;

function TGPAArchive.GetArchiveSize: int64;
begin
  Result := gpa_get_archive_size;
end;

function TGPAArchive.GetFileCount: integer;
begin
  Result := gpa_get_total_files;
end;

function TGPAArchive.GetIsArchiveActive: boolean;
begin
  Result := gpa_archive_active;
end;

function TGPAArchive.GetIsArchiveChanged: boolean;
begin
  Result := gpa_archive_changed;
end;

function TGPAArchive.LoadArchive(const FileName: string): boolean;
begin
  FErrorCode := gpa_load_archive(PChar(FileName));
  if FErrorCode = GPA_OK then
    FArchiveName := FileName;
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.SaveArchive: boolean;
begin
  FErrorCode := gpa_save_archive;
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.CloseArchive(Save: boolean): boolean;
begin
  if Save then
    SaveArchive;
  FErrorCode := gpa_close_archive(Save);
  if FErrorCode = GPA_OK then
    FArchiveName := '';
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.AddFile(const FileName, BasePath: string; Compression: TCompressionType;
  Encrypt: boolean): boolean;
begin
  FErrorCode := gpa_add_from_file(PChar(FileName),PChar(BasePath), Ord(Compression), Encrypt);
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.AddDirectory(const DirName: string;
  Compression: TCompressionType; Recursive: boolean; Encrypt: boolean): boolean;
begin
  FErrorCode := gpa_add_directory(PChar(DirName), Ord(Compression), Recursive, Encrypt);
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.ExtractFileById(FileID: integer;
  const DestFileName: string): boolean;
begin
  FErrorCode := gpa_extract_to_file_by_id(FileID, PChar(DestFileName));
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.DeleteFileById(FileID: integer): boolean;
begin
  FErrorCode := gpa_delete_by_id(FileID);
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.RenameFileById(FileID: integer; const NewName: string): boolean;
begin
  FErrorCode := gpa_rename_by_id(FileID, PChar(NewName));
  Result := FErrorCode = GPA_OK;
end;

function TGPAArchive.GetFilesInDirectory(const DirPath: string): TUintArray;
var
  ids: PUInt32;
  Count: uint32;
  i: integer;
  pIds: PUInt32;
begin
  ids := nil;
  Count := gpa_get_files_in_directory(PChar(DirPath), @ids);
  if (Count > 0) and (ids = nil) then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  SetLength(Result, Count);
  if Count > 0 then
  begin
    pIds := ids;
    for i := 0 to Count - 1 do
    begin
      Result[i] := pIds^;
      Inc(pIds);
    end;
  end;
  if ids <> nil then
  begin
    gpa_free_file_ids(ids);
    ids := nil;
  end;
end;

function TGPAArchive.GetFileNameById(FileID: integer): string;
begin
  Result := string(PChar(gpa_get_name_by_id(FileID)));
end;

function TGPAArchive.GetFileSizeById(FileID: integer): int64;
begin
  Result := gpa_get_size_by_id(FileID);
end;

function TGPAArchive.GetFileRealSizeById(FileID: integer): int64;
begin
  Result := gpa_get_real_size_by_id(FileID);
end;

function TGPAArchive.GetCompressionTypeById(FileID: integer): TCompressionType;
begin
  Result := TCompressionType(gpa_get_compression_type_by_id(FileID));
end;

function TGPAArchive.IsFileEncryptedById(FileID: integer): boolean;
begin
  Result := gpa_is_encrypted_by_id(FileID);
end;

procedure TGPAArchive.SetEncryption(EncryptFunc: TGPAEncryptFunc;
  DecryptFunc: TGPADecryptFunc; const Key: TByteArray);
begin
  gpa_set_encryption(EncryptFunc, DecryptFunc, @Key[0], Length(Key));
end;

procedure TGPAArchive.SetCompression(CompressFunc: TGPACompressFunc;
  DecompressFunc: TGPADecompressFunc; UserData: Pointer);
begin
  gpa_set_compression(CompressFunc, DecompressFunc, UserData);
end;

procedure TGPAArchive.InitLog(const LogFileName: string);
begin
  gpa_init_log(PChar(LogFileName));
end;

procedure TGPAArchive.CloseLog();
begin
  gpa_close_log();
end;

function TGPAArchive.GetLastError: TInt32;
begin
  Result := FErrorCode;
end;

end.
