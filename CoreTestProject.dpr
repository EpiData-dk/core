program CoreTestProject;

uses
  Forms,
  UTestMain in 'UTestMain.pas' {Form1},
  UEpiDataConstants in 'UEpiDataConstants.pas',
  UEpiTypes in 'UEpiTypes.pas',
  UEpiUtils in 'UEpiUtils.pas',
  UeFields in 'Datafile\UeFields.pas',
  UEpiDataFile in 'Datafile\UEpiDataFile.pas',
  UExport in 'Datafile\UExport.pas',
  UImport in 'Datafile\UImport.pas',
  UValueLabels in 'Datafile\UValueLabels.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
