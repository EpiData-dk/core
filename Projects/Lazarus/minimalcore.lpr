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
  UEpiDataConstants, ucommon, upwform, UImportExport,
  {$IFDEF EPIWARNING}
  Uwarning,
  Controls,
  {$ENDIF EPIWARNING}
  umain;

{$IFDEF WINDOWS}{$R minimalcore.rc}{$ENDIF}

{$IFDEF EPIWARNING}
var
  mr: integer;
{$ENDIF}
begin
  Application.Initialize;
{$IFDEF EPIWARNING}
  Application.CreateForm(TWarningForm, WarningForm);
  mr := WarningForm.ShowModal;
  if mr <> mrOk then
    exit;
  WarningForm.Free;
{$ENDIF EPIWARNING}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

