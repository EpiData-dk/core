unit UEpiLog;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  TEpiLogEvent = procedure(Sender: TObject; Msg: string) of object;

  TEpiLog = class(TObject)
  private
    FDebugLevel: word;
    FData: TStringList;
    FIndentLevel: integer;  
    FDebugEvent: TEpiLogEvent;
    procedure SetDebugLevel(Level: word);
    function AddIndent: string;
  protected
    constructor Create(DebugLevel: word);
    destructor Destroy; override;
  public
    class function GetInstance: TEpiLog;
    class procedure DestroyInstance;
    procedure IncIndent;
    procedure DecIndent;
    procedure Reset;
    procedure Add(Msg: string; DebugLevel: Word); overload;
    procedure Add(aClassName, aMethodName: string; DebugLevel: Word; Msg: string = ''); overload;
    procedure AddError(aClassName, aMethodName: string; Msg: string; LangCode: Integer = 0);
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
    property  DebugLevel: word read fDebugLevel write SetDebugLevel;
    property  OnDebugEvent: TEpiLogEvent read FDebugEvent write FDebugEvent;
  end;

function EpiLogger: TEpiLog;

implementation

uses SysUtils, StrUtils, UEpiUtils;

var
  OLog: TEpiLog = nil;

const
  MaxLevel: word = 4;

function EpiLogger: TEpiLog;
begin
  Result := TEpiLog.GetInstance;
end;

constructor TEpiLog.Create(DebugLevel: word);
begin
  fDebugLevel := DebugLevel;
  fIndentLevel := 0;
  fData := TStringList.Create();
end;

destructor TEpiLog.Destroy;
begin
  if Assigned(fData) then FreeAndNil(fData);
end;

function TEpiLog.AddIndent: string;
begin
  Result := DupeString('  ', fIndentLevel);
end;

procedure TEpiLog.IncIndent;
begin
  inc(fIndentLevel);
end;

procedure TEpiLog.DecIndent;
begin
  Dec(fIndentLevel);
end;

procedure TEpiLog.Reset;
var
  S: string;
  Csi: TCoreSystemInformation;
begin
  FData.Clear;
  Add('Log Started: ' + DateTimeToStr(Now), 1);
  GetCoreSystemInformation(Csi);
  S := Format('Application: %s; OS: %s', ['', Csi.OSName]);
  Add(S, 1);
  S := Format('Program Version: %d.%d.%d.%d; Core version: %d; Revision: %d',
         [Csi.PrgVersion.Major, Csi.PrgVersion.Minor, Csi.PrgVersion.Release, Csi.PrgVersion.Build, Csi.CoreVersion, Csi.CoreRevision]);
  Add(S, 1);
  S := Format('Memory Size: %d MB; Memory in Use: %d%%', [Csi.MemSize div (1024*1024), Csi.MemUsage]);
  Add(S, 1);
  Add('======================================================', 1);
end;

procedure TEpiLog.Add(Msg: string; DebugLevel: Word);
begin
  if DebugLevel <= fDebugLevel then
  begin
    fData.Add(AddIndent + Msg);
    if Assigned(OnDebugEvent) then
      OnDebugEvent(Self, AddIndent + Msg);
  end;
end;

procedure TEpiLog.Add(aClassName, aMethodName: string; DebugLevel: Word; Msg: string = '');
var
  s: string;
begin
  S := aClassName + '.' + aMethodName;
  if Trim(Msg) <> '' then
    S := S + ': ' + Msg;
  Add(S, DebugLevel);
end;

procedure TEpiLog.AddError(aClassName, aMethodName: string; Msg: string; LangCode: Integer = 0);
begin
  Add(aClassName, aMethodName, 1, 'Error (' + IntToStr(LangCode) + '): ' + Msg);
end;

procedure TEpiLog.SetDebugLevel(Level: word);
begin
  if Level > MaxLevel then
    fDebugLevel := MaxLevel
  else
    fDebugLevel := Level;
end;

class function TEpiLog.GetInstance: TEpiLog;
begin
  if not Assigned(OLog) then
  begin
    OLog := TEpiLog.Create(1);
    OLog.Reset;
  end;
  result := OLog;
end;

class procedure TEpiLog.DestroyInstance;
begin
  if Assigned(OLog) then FreeAndNil(OLog);
end;

procedure TEpiLog.SaveToFile(FileName: string);
var
  FS: TFileStream;
begin
  FS := nil;
  try
    FS := TFileStream.Create(FileName, fmCreate);
    SaveToStream(FS);
  finally
    if Assigned(FS) then FreeAndNil(FS);
  end;
end;

procedure TEpiLog.SaveToStream(Stream: TStream);
begin
  FData.SaveToStream(Stream);
end;

end.



