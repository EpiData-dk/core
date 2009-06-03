unit UDebug;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes;

type

  TDebugEvent = procedure(Sender: TObject; Msg: String) of object;

  TDebug = class(TObject)
  private
    FDebugLevel: word;
    FData: TStringList;
    FIndentLevel: integer;  
    FDebugEvent: TDebugEvent;
    procedure SetDebugLevel(Level: word);
    function AddIndent(): string;
  protected
    //
    constructor Create(DebugLevel: word);
    destructor Destroy(); override;
  public
    class function GetInstance(): TDebug;
    class procedure DestroyInstance();
    procedure IncIndent();
    procedure DecIndent();
    procedure Reset();
    procedure Add(Msg: String; DebugLevel: Word); overload;
    procedure Add(aClassName, aMethodName: string; DebugLevel: Word; Msg: String = ''); overload;
    procedure AddError(aClassName, aMethodName: string; Msg: String; LangCode: Integer = 0);
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
    property  DebugLevel: word read fDebugLevel write SetDebugLevel;
    property  OnDebugEvent: TDebugEvent read FDebugEvent write FDebugEvent;
  end;

function Debugger(): TDebug;

implementation

uses SysUtils, StrUtils, UEpiUtils;

var
  ODebug: TDebug = nil;

const
  MaxLevel: word = 3;

function Debugger(): TDebug;
begin
  Result := TDebug.GetInstance;
end;

constructor TDebug.Create(DebugLevel: word);
begin
  fDebugLevel := DebugLevel;
  fIndentLevel := 0;
  fData := TStringList.Create();
end;

destructor TDebug.Destroy();
begin
  if Assigned(fData) then FreeAndNil(fData);
end;

function TDebug.AddIndent(): string;
begin
  Result := DupeString('  ', fIndentLevel);
end;

procedure TDebug.IncIndent();
begin
  inc(fIndentLevel);
end;

procedure TDebug.DecIndent();
begin
  Dec(fIndentLevel);
end;

procedure TDebug.Reset();
var
  S: String;
  Csi: TCoreSystemInformation;
begin
  FData.Clear;
  Add('Log Started: ' + DateTimeToStr(Now), 1);
  GetCoreSystemInformation(Csi);
  S := Format('OS: %s; Application: %s; Core version: %d; Revision: %d', [Csi.OSName, '', Csi.CoreVersion, Csi.CoreRevision]);
  Add(S, 1);
  S := Format('Memory Size: %d MB; Memory in Use: %d%%', [Csi.MemSize div (1024*1024), Csi.MemUsage]);
  Add(S, 1);
  Add('======================================================', 1);
end;

procedure TDebug.Add(Msg: String; DebugLevel: Word);
begin
  if DebugLevel <= fDebugLevel then
  begin
    fData.Add(AddIndent + Msg);
    if Assigned(OnDebugEvent) then
      OnDebugEvent(Self, AddIndent + Msg);
  end;
end;

procedure TDebug.Add(aClassName, aMethodName: string; DebugLevel: Word; Msg: String = '');
var
  s: string;
begin
  S := aClassName + '.' + aMethodName;
  if Trim(Msg) <> '' then
    S := S + ': ' + Msg;
  Add(S, DebugLevel);
end;

procedure TDebug.AddError(aClassName, aMethodName: string; Msg: String; LangCode: Integer = 0);
begin
  Add(aClassName, aMethodName, 1, 'Error (' + IntToStr(LangCode) + '): ' + Msg);
end;

procedure TDebug.SetDebugLevel(Level: word);
begin
  if Level > MaxLevel then
    fDebugLevel := MaxLevel
  else
    fDebugLevel := Level;
end;

class function TDebug.GetInstance: TDebug;
begin
  if not Assigned(ODebug) then
  begin
    ODebug := TDebug.Create(1);
    ODebug.Reset;
  end;
  result := ODebug;
end;

class procedure TDebug.DestroyInstance;
begin
  if Assigned(ODebug) then FreeAndNil(ODebug);
end;

procedure TDebug.SaveToFile(FileName: string);
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

procedure TDebug.SaveToStream(Stream: TStream);
begin
  FData.SaveToStream(Stream);
end;

end.



