program MinimalCoreTest;

{$DEFINE EPIWARNING}

uses
  FastMM4,
  Forms,
  UEpiDataFile in '..\..\Datafile\UEpiDataFile.pas',
  UDataFileTypes in '..\..\Datafile\UDataFileTypes.pas',
  UValueLabels in '..\..\Datafile\UValueLabels.pas',
  UEpiUtils in '..\..\Utils\UEpiUtils.pas',
  UEpiDataConstants in '..\..\Utils\UEpiDataConstants.pas',
  UStringUtils in '..\..\Utils\UStringUtils.pas',
  Rijndael in '..\..\Encryption\Rijndael.pas',
  Base64 in '..\..\Encryption\Base64.pas',
  UCheckFileCmds in '..\..\Datafile\UCheckFileCmds.pas',
  UCheckFileTypes in '..\..\Datafile\UCheckFileTypes.pas',
  UCheckFileIO in '..\..\Datafile\UCheckFileIO.pas',
  UDateUtils in '..\..\Utils\UDateUtils.pas',
  ucommon in '..\ucommon.pas',
  UDebug in '..\..\Utils\UDebug.pas',
  UPWform in '..\upwform.pas' {formPW},
{$IFDEF EPIWARNING}
  Controls,
  uwarning in '..\uwarning.pas' {WarningForm},
{$ENDIF}
  UMain in '..\umain.pas' {MainForm},
  UImportExport in '..\..\Datafile\uimportexport.pas';

{$R *.res}

{$IFDEF EPIWARNING}
var
  mr: integer;
{$ENDIF}
begin
  Application.Initialize;
{$IFDEF EPIWARNING}
  Application.Title := 'EpiData Software Core Test Application';
  Application.CreateForm(TWarningForm, WarningForm);
  mr := WarningForm.ShowModal;
  if mr <> mrOk then
    exit;
  WarningForm.Free;
{$ENDIF EPIWARNING}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
