program CoreTestProject;

uses
  Forms,
  UTestMain in 'UTestMain.pas' {Form1},
  URecFileHandler in 'URecFileHandler.pas',
  UCustomFileHandler in 'UCustomFileHandler.pas',
  UEpiDataFile in 'UEpiDataFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
