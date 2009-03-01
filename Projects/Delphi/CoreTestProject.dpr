program CoreTestProject;

uses
  Forms,
  UTestMain in 'UTestMain.pas' {Form1},
  UEpiDataFile in '..\..\Datafile\UEpiDataFile.pas',
  UeFields in '..\..\Datafile\UeFields.pas',
  UEpiUtils in '..\..\UEpiUtils.pas',
  UEpiDataConstants in '..\..\UEpiDataConstants.pas',
  UEpiTypes in '..\..\UEpiTypes.pas',
  UValueLabels in '..\..\Datafile\UValueLabels.pas',
  SHA1 in '..\..\encryption\SHA1.pas',
  Base64 in '..\..\encryption\Base64.pas',
  DCPcrypt in '..\..\encryption\DCPcrypt.pas',
  Rijndael in '..\..\encryption\Rijndael.pas',
  CheckObjUnit in '..\..\Datafile\CheckObjUnit.pas',
  epiUDFTypes in '..\..\Datafile\epiUDFTypes.pas',
  UExport in '..\..\Datafile\UExport.pas',
  UExtUDF in '..\..\Datafile\UExtUDF.pas',
  UImport in '..\..\Datafile\UImport.pas',
  UPWform in 'UPWform.pas' {formPW};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
