program CoreTestProject;

uses
  Forms,
  {$IFDEF FPC}
  Interfaces, // This includes the WidgetSet for Lazarus
  {$ENDIF}
  UTestMain in 'UTestMain.pas' {Form1},
  UEpiDataFile in '../../Datafile/UEpiDataFile.pas',
  UeFields in '../../Datafile/UeFields.pas',
  UEpiUtils in '../../UEpiUtils.pas',
  UEpiDataConstants in '../../UEpiDataConstants.pas',
  UEpiTypes in '../../UEpiTypes.pas',
  UValueLabels in '../../Datafile/UValueLabels.pas',
  SHA1 in '../../encryption/SHA1.pas',
  Base64 in '../../encryption/Base64.pas',
  DCPcrypt in '../../encryption/DCPcrypt.pas',
  Rijndael in '../../encryption/Rijndael.pas',
  CheckObjUnit in '../../Datafile/CheckObjUnit.pas',
  epiUDFTypes in '../../Datafile/epiUDFTypes.pas',
  UExport in '../../Datafile/UExport.pas',
  {$IFNDEF FPC}
  UExtUDF in '../../Datafile/UExtUDF.pas',
  {$ENDIF}
  UImport in '../../Datafile/UImport.pas',
  UPWform in 'UPWform.pas' {formPW};

//{$R *.res}

{$IFDEF WINDOWS}{$R CoreTestProject.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
