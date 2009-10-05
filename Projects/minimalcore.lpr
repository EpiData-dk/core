program minimalcore;

{$codepage UTF8}
{$mode objfpc}{$H+}

{$IFDEF EPI_DEBUG}

{$ELSE}
  {$DEFINE EPIWARNING}
{$ENDIF EPI_DEBUG}



uses
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, LResources,
  {$IFDEF UNIX}cwstring,{$ENDIF}
  {$IFDEF EPIWARNING}
  UWarning,
  Controls,
  {$ENDIF EPIWARNING}
  UMain, ucommon, UPWform, UCheckFileCmds, UCheckFileIO, UCheckFileTypes,
  UDataFileTypes, UEpiDataFile, UValueLabels, Base64, DCPcrypt,
  Rijndael, SHA1, UDateUtils, UEpiUtils, UStringUtils, UUtilTypes, UEpiLog,
  uepidataglobals, UQesHandler, UImportExport, uimportform, Settings;

{$IFDEF WINDOWS}{$R minimalcore.rc}{$ENDIF}

{$IFDEF EPIWARNING}
var
  mr: integer;
{$ENDIF}
begin
  {$I minimalcore.lrs}
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

