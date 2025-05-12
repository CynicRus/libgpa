unit settings_form;

{$mode objfpc}{$H+}
{$define GPA_WITH_ZSTD}
{$define GPA_WITH_LZMA}
{$define GPA_WITH_LZ4}
{$define GPA_WITH_BZ2}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  gpa_wrapper, gpa;

type
  { TSettingsForm }
  TSettingsForm = class(TForm)
    LabelKey: TLabel;
    EditKey: TEdit;
    CheckUseEncryption: TCheckBox;
    LabelCompression: TLabel;
    ComboCompression: TComboBox;
    CheckUseCompression: TCheckBox;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    procedure PopulateCompressionTypes;
  public
    procedure LoadSettings;
    procedure SaveSettings;
    function GetEncryptionKey: gpa.TByteArray;
    function GetCompressionType: TCompressionType;
    function UseEncryption: Boolean;
    function UseCompression: Boolean;
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
  IniFiles;

{$R *.lfm}

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  PopulateCompressionTypes;
  LoadSettings;
end;

procedure TSettingsForm.PopulateCompressionTypes;
begin
  ComboCompression.Items.Clear;
  ComboCompression.Items.Add('None');
  ComboCompression.Items.Add('Zlib');
  {$IFDEF GPA_WITH_LZMA}
  ComboCompression.Items.Add('LZMA');
  {$ENDIF}
  {$IFDEF GPA_WITH_ZSTD}
  ComboCompression.Items.Add('Zstd');
  {$ENDIF}
  {$IFDEF GPA_WITH_LZ4}
  ComboCompression.Items.Add('LZ4');
  {$ENDIF}
  {$IFDEF GPA_WITH_BZ2}
  ComboCompression.Items.Add('BZ2');
  {$ENDIF}
  ComboCompression.Items.Add('Custom');
  ComboCompression.ItemIndex := 1; // Zlib по умолчанию
end;

procedure TSettingsForm.LoadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('settings.ini');
  try
    EditKey.Text := Ini.ReadString('Archive', 'EncryptionKey', '');
    CheckUseEncryption.Checked := Ini.ReadBool('Archive', 'UseEncryption', False);
    ComboCompression.ItemIndex := Ini.ReadInteger('Archive', 'CompressionType', 1);
    CheckUseCompression.Checked := Ini.ReadBool('Archive', 'UseCompression', True);
  finally
    Ini.Free;
  end;
end;

procedure TSettingsForm.SaveSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('settings.ini');
  try
    Ini.WriteString('Archive', 'EncryptionKey', EditKey.Text);
    Ini.WriteBool('Archive', 'UseEncryption', CheckUseEncryption.Checked);
    Ini.WriteInteger('Archive', 'CompressionType', ComboCompression.ItemIndex);
    Ini.WriteBool('Archive', 'UseCompression', CheckUseCompression.Checked);
  finally
    Ini.Free;
  end;
end;

function TSettingsForm.GetEncryptionKey: TByteArray;
var
  KeyStr: string;
  I: Integer;
begin
  KeyStr := EditKey.Text;
  if KeyStr = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  SetLength(Result, Length(KeyStr));
  for I := 1 to Length(KeyStr) do
    Result[I-1] := Ord(KeyStr[I]);
end;

function TSettingsForm.GetCompressionType: TCompressionType;
begin
  if not CheckUseCompression.Checked then
  begin
    Result := ctNone;
    Exit;
  end;
  case ComboCompression.ItemIndex of
    0: Result := ctNone;
    1: Result := ctZlib;
    {$IFDEF GPA_WITH_LZMA}
    2: Result := ctLZMA;
    {$IFDEF GPA_WITH_ZSTD}
    3: Result := ctZstd;
    {$IFDEF GPA_WITH_LZ4}
    4: Result := ctLZ4;
    {$IFDEF GPA_WITH_BZ2}
    5: Result := ctBZ2;
    6: Result := ctCustom;
    {$ELSE}
    5: Result := ctCustom;
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_BZ2}
    4: Result := ctBZ2;
    5: Result := ctCustom;
    {$ELSE}
    4: Result := ctCustom;
    {$ENDIF}
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_LZ4}
    3: Result := ctLZ4;
    {$IFDEF GPA_WITH_BZ2}
    4: Result := ctBZ2;
    5: Result := ctCustom;
    {$ELSE}
    4: Result := ctCustom;
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_BZ2}
    3: Result := ctBZ2;
    4: Result := ctCustom;
    {$ELSE}
    3: Result := ctCustom;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_ZSTD}
    2: Result := ctZstd;
    {$IFDEF GPA_WITH_LZ4}
    3: Result := ctLZ4;
    {$IFDEF GPA_WITH_BZ2}
    4: Result := ctBZ2;
    5: Result := ctCustom;
    {$ELSE}
    4: Result := ctCustom;
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_BZ2}
    3: Result := ctBZ2;
    4: Result := ctCustom;
    {$ELSE}
    3: Result := ctCustom;
    {$ENDIF}
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_LZ4}
    2: Result := ctLZ4;
    {$IFDEF GPA_WITH_BZ2}
    3: Result := ctBZ2;
    4: Result := ctCustom;
    {$ELSE}
    3: Result := ctCustom;
    {$ENDIF}
    {$ELSE}
    {$IFDEF GPA_WITH_BZ2}
    2: Result := ctBZ2;
    3: Result := ctCustom;
    {$ELSE}
    2: Result := ctCustom;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
    else
      Result := ctZlib;
  end;
end;

function TSettingsForm.UseEncryption: Boolean;
begin
  Result := CheckUseEncryption.Checked and (EditKey.Text <> '');
end;

function TSettingsForm.UseCompression: Boolean;
begin
  Result := CheckUseCompression.Checked;
end;

end.
