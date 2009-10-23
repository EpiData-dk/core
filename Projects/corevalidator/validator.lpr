program validator;

{$encoding utf8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  FileUtil, validationunit,
  UEpiUtils, UEpiLog;

type

  { TEpiValidator }

  TEpiValidator = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    Procedure MyExceptHandler(Sender : TObject; E : Exception);
  end;

{ TEpiValidator }

procedure TEpiValidator.DoRun;
var
  ErrorMsg: String;
  FileSearch: TFileSearcher;
  DFValidator: TDatafileValidator;
  CSI: TCoreSystemInformation;
begin
  // quick check parameters
  OnException := @MyExceptHandler;

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
  GetCoreSystemInformation(CSI);
  EpiLogger.Reset;

  DFValidator := TDatafileValidator.Create;
  FileSearch := TFileSearcher.Create;
  FileSearch.OnDirectoryFound := @DFValidator.DirectoryHandler;
  FileSearch.OnFileFound := @DFValidator.FileHandler;

  // TODO : Include switches and options.
  FileSearch.Search(GetCurrentDirUTF8, '*.recxml;*.rec;*.dta;*.ods;*.txt;*.csv;*.dbf');

  // stop program loop
  Terminate;
end;

constructor TEpiValidator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=False;
end;

destructor TEpiValidator.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiValidator.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

procedure TEpiValidator.MyExceptHandler(Sender: TObject; E: Exception);
begin
  writeln('Something horrible happened. Aborting program.');
  Terminate;
end;

var
  Application: TEpiValidator;

{$IFDEF WINDOWS}{$R validator.rc}{$ENDIF}

begin
  Application:=TEpiValidator.Create(nil);
  Application.Title:='EpiData Datafile Validator';
  Application.Run;
  Application.Free;
end.

