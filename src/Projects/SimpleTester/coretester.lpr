program coretester;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, epidatacore, CustApp, test1, customtest, test2;

type

  { TEpiCoreTester }

  TEpiCoreTester = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TEpiCoreTester }

procedure TEpiCoreTester.DoRun;
var
  ErrorMsg: String;
  i: Integer;
  Test: TCustomTest;
  Failed: Integer;
  Succeeded: Integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  if Assigned(Tests) then
  begin
    Failed := 0;
    Succeeded := 0;
    for i := 0 to Tests.Count - 1 do
    begin
      Test := TCustomTestClass(Tests[i]).Create;
      Test.Initialize;
      if not Test.Run then
      begin
        Inc(Failed);
        WriteLn('Failed test: ', Test.ClassName);
      end else
        Inc(Succeeded);
    end;
    WriteLn('Number of tests: ', Tests.Count);
    WriteLn('Succeeded tests: ', Succeeded);
    WriteLn('Failed tests:    ', Failed);
  end;

  // stop program loop
  Terminate;
end;

constructor TEpiCoreTester.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TEpiCoreTester.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiCoreTester.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TEpiCoreTester;

{$R *.res}

begin
  Application:=TEpiCoreTester.Create(nil);
  Application.Title:='EpiData Core Tester';
  Application.Run;
  Application.Free;
end.

