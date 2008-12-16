program CoreTestProject;

uses
  Forms,
  UTestMain in 'UTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
