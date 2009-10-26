program validator;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  FileUtil, validationunit,
  UEpiUtils, UEpiLog, settings;

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
    procedure Terminate; override;
  end;

{ TEpiValidator }

procedure TEpiValidator.DoRun;
var
  ErrorMsg: String;
  FileSearch: TFileSearcher;
  DFValidator: TDatafileValidator;
  CSI: TCoreSystemInformation;
  NonOpts, Opts, LongOpts: TStrings;
begin
  // quick check parameters
  OnException := @MyExceptHandler;
  CaseSensitiveOptions := true;

  Reporter := TReporter.Create;
  Opts := TStringList.Create;
  NonOpts := TStringList.Create;
  LongOpts := TStringList.Create;
  LongOpts.CommaText := 'help,fatal,recursive,logfile:';

  ErrorMsg:=CheckOptions('hl:FR', LongOpts, Opts, NonOpts);
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

  VSettings.ErrorsAreFatal := HasOption('F', 'fatal');
  VSettings.IncludeSubDirs := HasOption('R', 'recursive');
  if HasOption('l', 'logfile') then
    VSettings.LogFile := GetOptionValue('l', 'logfile')
  else
    VSettings.LogFile := GetCurrentDirUTF8() + DirectorySeparator + 'validation.result';

  { add your program here }
  GetCoreSystemInformation(CSI);
  EpiLogger.Reset;

  DFValidator := TDatafileValidator.Create;
  FileSearch := TFileSearcher.Create;
  FileSearch.OnDirectoryFound := @DFValidator.DirectoryHandler;
  FileSearch.OnFileFound := @DFValidator.FileHandler;

  if (NonOpts.Count = 0) then
  begin
    Reporter.ReportEvent(rtFatal, 'No start directory entered.');
    Terminate;
    Exit;
  end;

  if not DirectoryExistsUTF8(NonOpts[0]) then
  begin
    Reporter.ReportEvent(rtFatal, 'Directory does not exist: %s', [NonOpts[0]]);
    Terminate;
    Exit;
  end;

  FileSearch.Search(NonOpts[0], '*.recxml;*.rec;*.dta;*.ods;*.txt;*.csv;*.dbf', VSettings.IncludeSubDirs);

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
  writeln('Usage: ', ExeName,' [OPTION]... [PATH]');
  writeln('');
  writeln('OPTIONS:');
  writeln(' -h, --help', #9#9, 'Print this help and terminate');
  writeln(' -F, --fatal', #9#9, 'Make any error fatal and abort further progress');
  writeln(' -l, --logfile=FILE', #9, 'Save output to this file (Default is <exe-name>.result in <exe-dir>)');
  writeln(' -R, --recursive', #9, 'Recurse into subdirectories');
end;

procedure TEpiValidator.MyExceptHandler(Sender: TObject; E: Exception);
begin
  if not (E is EAbort) then
    writeln('Something horrible happened. Aborting program.');
  Terminate;
end;

procedure TEpiValidator.Terminate;
begin
  if Assigned(Reporter) then
  begin
    Reporter.SaveToFile(VSettings.LogFile);
    FreeAndNil(Reporter);
  end;
  inherited Terminate;
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

