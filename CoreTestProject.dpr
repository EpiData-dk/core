program CoreTestProject;

uses
  Forms,
  UTestMain in 'UTestMain.pas' {Form1},
  UEpiDataFile in 'UEpiDataFile.pas',
  UeFields in 'UeFields.pas',
  UEpiTypes in 'UEpiTypes.pas',
  UValueLabels in 'UValueLabels.pas',
  UEpiUtils in 'UEpiUtils.pas',
  UEpiDataConstants in 'UEpiDataConstants.pas',
  UImport in 'UImport.pas',
  UExport in 'UExport.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
