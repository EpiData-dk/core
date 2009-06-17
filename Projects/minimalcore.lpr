program minimalcore;

{$mode objfpc}{$H+}

{.$DEFINE EPIWARNING}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, LResources,
  {$IFDEF EPIWARNING}
  UWarning,
  Controls,
  {$ENDIF EPIWARNING}
  UMain, ucommon, UPWform, UCheckFileCmds, UCheckFileIO,
  UCheckFileTypes, UDataFileTypes, UEpiDataFile, uimportexport, UValueLabels,
  Base64, DCPcrypt, Rijndael, SHA1, UDateUtils, UDebug, UEpiDataConstants,
  UEpiUtils, UStringUtils, UUtilTypes, UQesHandler;

{$IFDEF WINDOWS}{$R minimalcore.rc}{$ENDIF}

{$IFDEF EPIWARNING}
var
  mr: integer;
{$ENDIF}
begin
  {$I minimalcore.lrs}
//  Application.Title := 'EpiData Software Core Test Project';
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

