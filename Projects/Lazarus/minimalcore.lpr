program minimalcore;

{$mode objfpc}{$H+}

{.$DEFINE EPIWARNING}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, LResources, UValueLabels, UEpiDataFile,
  UDataFileTypes, SHA1, DCPcrypt, Base64, UUtilTypes, UStringUtils, UEpiUtils,
  UEpiDataConstants, UDebug, UCommon, UPWform, UImportExport,
  {$IFDEF EPIWARNING}
  UWarning,
  Controls,
  {$ENDIF EPIWARNING}
  UMain;

{$IFDEF WINDOWS}{$R minimalcore.rc}{$ENDIF}

{$IFDEF EPIWARNING}
var
  mr: integer;
{$ENDIF}
begin
  Application.Title := 'EpiData Software Core Test Project';
  Application.Initialize;
{$IFDEF EPIWARNING}
  Application.CreateForm(TWarningForm, WarningForm);
  mr := WarningForm.ShowModal;
  if mr <> mrOk then
    exit;
  WarningForm.Free;
{$ENDIF EPIWARNING}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

