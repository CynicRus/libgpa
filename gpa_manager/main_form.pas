unit main_form;

{$mode objfpc}{$H+}

{$define GPA_WITH_ZSTD}
{$define GPA_WITH_LZMA}
{$define GPA_WITH_LZ4}
{$define GPA_WITH_BZ2}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls, LCLType, settings_form, gpa, gpa_wrapper;

type
  { TMainForm }
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ArchiveMenu: TMenuItem;
    SettingsMenu: TMenuItem;
    FileNew: TMenuItem;
    FileOpen: TMenuItem;
    FileSave: TMenuItem;
    FileClose: TMenuItem;
    FileExit: TMenuItem;
    ArchiveAddFile: TMenuItem;
    ArchiveAddDir: TMenuItem;
    ArchiveExtract: TMenuItem;
    ArchiveDelete: TMenuItem;
    ArchiveRename: TMenuItem;
    SettingsItem: TMenuItem;
    Toolbar: TToolBar;
    BtnNew: TToolButton;
    BtnOpen: TToolButton;
    BtnSave: TToolButton;
    BtnAddFile: TToolButton;
    BtnAddDir: TToolButton;
    BtnExtract: TToolButton;
    BtnDelete: TToolButton;
    BtnRename: TToolButton;
    BtnSettings: TToolButton;
    TreeView: TTreeView;
    ListView: TListView;
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SelectDirDialog: TSelectDirectoryDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileNewClick(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure FileSaveClick(Sender: TObject);
    procedure FileCloseClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure ArchiveAddFileClick(Sender: TObject);
    procedure ArchiveAddDirClick(Sender: TObject);
    procedure ArchiveExtractClick(Sender: TObject);
    procedure ArchiveDeleteClick(Sender: TObject);
    procedure ArchiveRenameClick(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure TreeViewSelectionChanged(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: boolean);
  private
    FArchive: TGPAArchive;
    FSettings: TSettingsForm;
    procedure UpdateUI;
    procedure UpdateStatusBar;
    procedure LoadArchiveTree;
    procedure LoadDirectoryFiles(const DirPath: string);
    procedure ShowError(ErrorCode: longint);
    function GetSelectedFileID: integer;
    procedure ApplyArchiveSettings;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FArchive := TGPAArchive.Create;
  FSettings := TSettingsForm.Create(nil);
  ListView.OnSelectItem := @ListViewSelectItem;
  UpdateUI;
  UpdateStatusBar;
  //FArchive.InitLog('C:\Development\GPAManager\gpa_lib.log');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  Node: TTreeNode;
begin
  for Node in TreeView.Items do
    if Node.Data <> nil then
      StrDispose(PChar(Node.Data));
  FSettings.Free;
  //FArchive.CloseLog();
  FArchive.Free;
end;

procedure TMainForm.FileNewClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    if FArchive.IsArchiveActive then
      FArchive.CloseArchive(False);
    try
      FArchive.CreateArchive(SaveDialog.FileName);
      if FArchive.GetLastError = GPA_OK then
      begin
        ApplyArchiveSettings;
        LoadArchiveTree;
        UpdateUI;
        UpdateStatusBar;
      end
      else
        ShowError(FArchive.GetLastError);
    except
      on E: Exception do
        MessageDlg('Error', E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.FileOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if FArchive.IsArchiveActive then
      FArchive.CloseArchive(False);
    ApplyArchiveSettings;
    try
      if FArchive.LoadArchive(OpenDialog.FileName) then
      begin
        LoadArchiveTree;
        UpdateUI;
        UpdateStatusBar;
      end
      else
        ShowError(FArchive.GetLastError);
    except
      on E: Exception do
        MessageDlg('Error', 'Cannot open the arhive: ' + E.Message,
          mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.FileSaveClick(Sender: TObject);
begin
  if not FArchive.IsArchiveActive then
  begin
    MessageDlg('Error', 'No archive is open.', mtError, [mbOK], 0);
    Exit;
  end;
  if FArchive.SaveArchive then
  begin
    UpdateStatusBar;
    MessageDlg('Success', 'Archive saved successfully.', mtInformation, [mbOK], 0);
  end
  else
    ShowError(FArchive.GetLastError);
end;

procedure TMainForm.FileCloseClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  if FArchive.IsArchiveActive then
  begin
    if FArchive.IsArchiveChanged then
      if MessageDlg('Save Changes', 'Save changes to archive before closing?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        FArchive.SaveArchive;
    FArchive.CloseArchive(False);
    for Node in TreeView.Items do
      if Node.Data <> nil then
        StrDispose(PChar(Node.Data));
    TreeView.Items.Clear;
    ListView.Items.Clear;
    UpdateUI;
    UpdateStatusBar;
  end;
end;

procedure TMainForm.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ArchiveAddFileClick(Sender: TObject);
var
  Compression: TCompressionType;
  Key: TByteArray;
  Encrypt: boolean;
begin
  if not FArchive.IsArchiveActive then
  begin
    MessageDlg('Error', 'No archive is open.', mtError, [mbOK], 0);
    Exit;
  end;
  if OpenDialog.Execute then
  begin
    Compression := FSettings.GetCompressionType;
    Encrypt := FSettings.UseEncryption;
    if Encrypt then
    begin
      Key := FSettings.GetEncryptionKey;
      FArchive.SetEncryption(nil, nil, Key);
    end;
    if FArchive.AddFile(OpenDialog.FileName, ExtractFilePath(OpenDialog.FileName),
      Compression, Encrypt) then
    begin
      LoadArchiveTree;
      UpdateStatusBar;
    end
    else
      ShowError(FArchive.GetLastError);
  end;
end;

procedure TMainForm.ArchiveAddDirClick(Sender: TObject);
var
  Compression: TCompressionType;
  Key: TByteArray;
  Encrypt: boolean;
begin
  if not FArchive.IsArchiveActive then
  begin
    MessageDlg('Error', 'No archive is open.', mtError, [mbOK], 0);
    Exit;
  end;
  if SelectDirDialog.Execute then
  begin
    Compression := FSettings.GetCompressionType;
    Encrypt := FSettings.UseEncryption;
    if Encrypt then
    begin
      Key := FSettings.GetEncryptionKey;
      FArchive.SetEncryption(nil, nil, Key);
    end;
    if FArchive.AddDirectory(SelectDirDialog.FileName, Compression, True, Encrypt) then
    begin
      LoadArchiveTree;
      UpdateStatusBar;
    end
    else
      ShowError(FArchive.GetLastError);
  end;
end;

procedure TMainForm.ArchiveExtractClick(Sender: TObject);
var
  FileID: integer;
begin
  if not FArchive.IsArchiveActive then
  begin
    MessageDlg('Error', 'No archive is open.', mtError, [mbOK], 0);
    Exit;
  end;
  FileID := GetSelectedFileID;
  if FileID < 0 then
  begin
    MessageDlg('Error', 'No file selected.', mtError, [mbOK], 0);
    Exit;
  end;
  if SaveDialog.Execute then
  begin
    if FArchive.ExtractFileById(FileID, SaveDialog.FileName) then
      MessageDlg('Success', 'File extracted successfully.', mtInformation, [mbOK], 0)
    else
      ShowError(FArchive.GetLastError);
  end;
end;

procedure TMainForm.ArchiveDeleteClick(Sender: TObject);
var
  FileID: integer;
begin
  if not FArchive.IsArchiveActive then
  begin
    MessageDlg('Error', 'No archive is open.', mtError, [mbOK], 0);
    Exit;
  end;
  FileID := GetSelectedFileID;
  if FileID < 0 then
  begin
    MessageDlg('Error', 'No file selected.', mtError, [mbOK], 0);
    Exit;
  end;
  if MessageDlg('Confirm', 'Delete selected file?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    if FArchive.DeleteFileById(FileID) then
    begin
      LoadArchiveTree;
      UpdateStatusBar;
    end
    else
      ShowError(FArchive.GetLastError);
  end;
end;

procedure TMainForm.ArchiveRenameClick(Sender: TObject);
var
  FileID: integer;
  NewName: string;
begin
  if not FArchive.IsArchiveActive then
  begin
    MessageDlg('Error', 'No archive is open.', mtError, [mbOK], 0);
    Exit;
  end;
  FileID := GetSelectedFileID;
  if FileID < 0 then
  begin
    MessageDlg('Error', 'No file selected.', mtError, [mbOK], 0);
    Exit;
  end;
  NewName := InputBox('Rename File', 'Enter new file name:',
    FArchive.GetFileNameById(FileID));
  if NewName <> '' then
  begin
    if FArchive.RenameFileById(FileID, NewName) then
    begin
      LoadArchiveTree;
      UpdateStatusBar;
    end
    else
      ShowError(FArchive.GetLastError);
  end;
end;

procedure TMainForm.SettingsClick(Sender: TObject);
begin
  if FSettings.ShowModal = mrOk then
  begin
    FSettings.SaveSettings;
    if FArchive.IsArchiveActive then
      ApplyArchiveSettings;
  end;
end;

procedure TMainForm.TreeViewSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
  Path: string;
begin
  if TreeView.Selected = nil then
    Exit;

  Node := TreeView.Selected;

  if Node.Level = 0 then
    Path := ''
  else
    Path := string(PChar(Node.Data));

  if (Path <> '') and (Path[Length(Path)] <> '/') then
    Path := Path + '/';

  LoadDirectoryFiles(Path);
end;

procedure TMainForm.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  UpdateUI;
end;

procedure TMainForm.UpdateUI;
var
  ArchiveActive: boolean;
begin
  ArchiveActive := FArchive.IsArchiveActive;
  FileSave.Enabled := ArchiveActive;
  FileClose.Enabled := ArchiveActive;
  ArchiveAddFile.Enabled := ArchiveActive;
  ArchiveAddDir.Enabled := ArchiveActive;
  ArchiveExtract.Enabled := ArchiveActive and (ListView.Selected <> nil);
  ArchiveDelete.Enabled := ArchiveActive and (ListView.Selected <> nil);
  ArchiveRename.Enabled := ArchiveActive and (ListView.Selected <> nil);
  BtnSave.Enabled := ArchiveActive;
  BtnAddFile.Enabled := ArchiveActive;
  BtnAddDir.Enabled := ArchiveActive;
  BtnExtract.Enabled := ArchiveActive and (ListView.Selected <> nil);
  BtnDelete.Enabled := ArchiveActive and (ListView.Selected <> nil);
  BtnRename.Enabled := ArchiveActive and (ListView.Selected <> nil);
end;

procedure TMainForm.UpdateStatusBar;
begin
  if FArchive.IsArchiveActive then
    StatusBar.SimpleText := Format(
      'Archive: %s | Files: %d | Size: %d bytes | Real Size: %d bytes',
      [FArchive.ArchiveName, FArchive.FileCount, FArchive.ArchiveSize,
      FArchive.ArchiveRealSize])
  else
    StatusBar.SimpleText := 'No archive open';
end;
{$define DEBUG}
procedure TMainForm.LoadArchiveTree;
var
  IDs: array of uint32;
  I, J: integer;
  FileName, DirPath: string;
  PathParts: TStringList;
  CurrentNode, RootNode, ChildNode: TTreeNode;
  UniqueDirs: TStringList;
  Path: string;
  ChildFound: boolean;
  NodeData: pchar;
begin
  TreeView.Items.BeginUpdate;
  try
    try
      if TreeView.Items <> nil then
         TreeView.Items.Clear;
      if ListView.Items <> nil then
         ListView.Items.Clear;

      if not FArchive.IsArchiveActive then
        Exit;

      RootNode := TreeView.Items.Add(nil, ExtractFileName(FArchive.ArchiveName));
      RootNode.Data := nil;

      UniqueDirs := TStringList.Create;
      try
        IDs := FArchive.GetFilesInDirectory('');
        if Length(IDs) = 0 then
          Exit;

        for I := 0 to High(IDs) do
        begin
          FileName := FArchive.GetFileNameById(IDs[I]);
          if FileName = '' then
            Continue;
          DirPath := ExtractFilePath(FileName);
          if DirPath <> '' then
          begin
            if DirPath[Length(DirPath)] = '/' then
              DirPath := Copy(DirPath, 1, Length(DirPath) - 1);
            if UniqueDirs.IndexOf(DirPath) = -1 then
              UniqueDirs.Add(DirPath);
          end;
        end;

        UniqueDirs.Sort;

        PathParts := TStringList.Create;
        try
          PathParts.Delimiter := '/';
          PathParts.StrictDelimiter := True;

          for I := 0 to UniqueDirs.Count - 1 do
          begin
            Path := UniqueDirs[I];
            PathParts.DelimitedText := Path;

            CurrentNode := RootNode;

            for J := 0 to PathParts.Count - 1 do
            begin
              ChildFound := False;
              ChildNode := CurrentNode.GetFirstChild;

              while Assigned(ChildNode) do
              begin
                if ChildNode.Text = PathParts[J] then
                begin
                  CurrentNode := ChildNode;
                  ChildFound := True;
                  Break;
                end;
                ChildNode := ChildNode.GetNextSibling;
              end;

              if not ChildFound then
              begin
                CurrentNode := TreeView.Items.AddChild(CurrentNode, PathParts[J]);
                try
                  if J = 0 then
                    NodeData := StrNew(PChar(PathParts[J]))
                  else if CurrentNode.Parent.Data = nil then
                    NodeData := StrNew(PChar(PathParts[J]))
                  else
                    NodeData :=
                      StrNew(PChar(string(PChar(CurrentNode.Parent.Data)) + '/' + PathParts[J]));
                  CurrentNode.Data := NodeData;
                except
                  on E: Exception do
                  begin
                    raise;
                  end;
                end;
              end;
            end;
          end;
        finally
          PathParts.Free;
        end;

      finally
        UniqueDirs.Free;
      end;

      RootNode.Expand(False);
    except
      on E: Exception do
      begin
        MessageDlg('Error',
          'Error of build the archive tree: ' +
          E.Message, mtError, [mbOK], 0);
      end;

    end;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TMainForm.LoadDirectoryFiles(const DirPath: string);
var
  IDs: array of uint32;
  I: integer;
  FileName, FilePath: string;
  Item: TListItem;
  CompStr: string;
begin
  ListView.Items.Clear;
  if not FArchive.IsArchiveActive then
    Exit;

  IDs := FArchive.GetFilesInDirectory(DirPath);
  ListView.BeginUpdate;
  try
    for I := 0 to High(IDs) do
    begin
      FileName := FArchive.GetFileNameById(IDs[I]);
      FilePath := ExtractFilePath(FileName);

      if ((DirPath = '') and (Pos('/', FileName) = 0)) or
        ((DirPath <> '') and (FilePath = DirPath)) then
      begin
        Item := ListView.Items.Add;
        Item.Caption := ExtractFileName(FileName);
        Item.SubItems.Add(IntToStr(IDs[I]));
        Item.SubItems.Add(IntToStr(FArchive.GetFileSizeById(IDs[I])));
        Item.SubItems.Add(IntToStr(FArchive.GetFileRealSizeById(IDs[I])));
        case FArchive.GetCompressionTypeById(IDs[I]) of
          ctNone: CompStr := 'None';
          ctZlib: CompStr := 'Zlib';
          {$IFDEF GPA_WITH_LZMA}
          ctLZMA: CompStr := 'LZMA';
          {$ENDIF}
          {$IFDEF GPA_WITH_ZSTD}
          ctZstd: CompStr := 'Zstd';
          {$ENDIF}
          {$IFDEF GPA_WITH_LZ4}
          ctLZ4: CompStr := 'LZ4';
          {$ENDIF}
          {$IFDEF GPA_WITH_BZ2}
          ctBZ2: CompStr := 'BZ2';
          {$ENDIF}
          ctCustom: CompStr := 'Custom';
        end;
        Item.SubItems.Add(CompStr);
        if FArchive.IsFileEncryptedById(IDs[I]) then
          Item.SubItems.Add('Yes')
        else
          Item.SubItems.Add('No');
        Item.Data := Pointer(PtrInt(IDs[I]));
      end;
    end;
  finally
    ListView.EndUpdate;
    UpdateUI;
  end;
end;

procedure TMainForm.ShowError(ErrorCode: longint);
const
  ErrorMessages: array[0..14] of string = (
    'Success',
    'Archive not loaded',
    'Archive already loaded',
    'Archive is empty',
    'File not found',
    'Archive file not found',
    'Invalid format',
    'I/O error',
    'Memory error',
    'Compression error',
    'Encryption error',
    'Invalid path',
    'Buffer too small',
    'Decryption failed',
    'Invalid parameter'
    );
begin
  if ErrorCode <> GPA_OK then
    MessageDlg('Error', ErrorMessages[ErrorCode], mtError, [mbOK], 0);
end;

function TMainForm.GetSelectedFileID: integer;
begin
  if ListView.Selected <> nil then
    Result := PtrInt(ListView.Selected.Data)
  else
    Result := -1;
end;

procedure TMainForm.ApplyArchiveSettings;
var
  Key: TByteArray;
begin
  if FSettings.UseEncryption then
  begin
    Key := FSettings.GetEncryptionKey;
    if Length(Key) > 0 then
      FArchive.SetEncryption(nil, nil, Key);
  end;
end;

end.
