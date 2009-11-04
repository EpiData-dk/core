program minimalcore;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  {$IFDEF UNIX}cwstring,{$ENDIF}
  {$IFNDEF EPI_DEBUG}
  UWarning,
  Controls,
  {$ENDIF EPI_DEBUG}
  UMain, ucommon, UPWform, uimportform, Settings;

{$IFDEF WINDOWS}{$R minimalcore.rc}{$ENDIF}

{$IFNDEF EPI_DEBUG}
var
  mr: integer;
{$ENDIF}
begin
  Application.Title := 'EpiData Software Core Test Project';
  Application.Initialize;
{$IFNDEF EPI_DEBUG}
  Application.CreateForm(TWarningForm, WarningForm);
  mr := WarningForm.ShowModal;
  if mr <> mrOk then
    exit;
  WarningForm.Free;
{$ENDIF EPI_DEBUG}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

