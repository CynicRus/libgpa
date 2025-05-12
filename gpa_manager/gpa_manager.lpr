program gpa_manager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main_form, settings_form{,windows};

{$R *.res}
{procedure CreateConsole;
var
  ConsoleAllocated: Boolean;
begin
  ConsoleAllocated := AllocConsole;
  if ConsoleAllocated then
  begin
    // Перенаправляем стандартные потоки в консоль
    AssignFile(Output, 'CONOUT$');
    Rewrite(Output);
    AssignFile(Input, 'CONIN$');
    Reset(Input);
  end;
end;}
begin
  //CreateConsole;
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;
end.

