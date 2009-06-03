unit UEpiUtils;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

USES
  {$IFNDEF FPC} Windows,{$ENDIF}
  SysUtils, UEpiDataConstants, UDataFileTypes;

type
  PCoreSystemInformation = ^TCoreSystemInformation;
  TCoreSystemInformation = record
    OSName:         String;     // Windows, Linux (or Mac?).
    OSMajorVersion: Word;       //
    OSMinorVersion: Word;       //
    MemSize:        Int64;      // Physical memory in Bytes.
    MemUsage:       Integer;    // Usage level of memory in percent.
    CoreVersion:    Cardinal;   // Version of Core and utils.
    CoreRevision:   Cardinal;   // Subversion revision.
  end;

  // Checks validity of variable name.  
  function CheckVariableName(Const VarName: string; ValidChars: TCharSet): boolean;
  function FieldTypeToFieldTypeName(FieldType: TFieldType; Lang: TTranslateEvent): widestring;
  function IsCompliant(Value: String; Ft: TFieldType):Boolean;

  function IsInteger(Value: String): boolean;
  function IsFloat(Value: String): boolean;

  function PreInc(Var I: Integer; Const N: Integer = 1): Integer;
  function PostInc(Var I: Integer; Const N: Integer = 1): Integer;

  procedure GetCoreSystemInformation(var CSI: TCoreSystemInformation);

{$IFNDEF LINUX}
type
  DWORDLONG = Int64;

  PMemoryStatusEx = ^TMemoryStatusEx;
  _MEMORYSTATUSEX = record
    dwLength:                DWORD;
    dwMemoryLoad:            DWORD;
    ullTotalPhys:            DWORDLONG;
    ullAvailPhys:            DWORDLONG;
    ullTotalPageFile:        DWORDLONG;
    ullAvailPageFile:        DWORDLONG;
    ullTotalVirtual:         DWORDLONG;
    ullAvailVirtual:         DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG
  end;
  {$EXTERNALSYM _MEMORYSTATUSEX}
  TMemoryStatusEx = _MEMORYSTATUSEX;
  MEMORYSTATUSEx = _MEMORYSTATUSEX;
  {$EXTERNALSYM MEMORYSTATUSEX}


  procedure GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx); stdcall;
  {$EXTERNALSYM GlobalMemoryStatusEx}
{$ENDIF}
  
implementation

uses
  UDateUtils;

{$IFNDEF LINUX}
procedure GlobalMemoryStatusEx; external kernel32 name 'GlobalMemoryStatusEx';
{$ENDIf}
  
function CheckVariableName(Const VarName: string; ValidChars: TCharSet): boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to Length(VarName) do
    if not (Varname[i] in ValidChars) THEN exit;
  result := true;
end;

function FieldTypeToFieldTypeName(FieldType: TFieldType; Lang: TTranslateEvent): widestring;
var
  i: integer;
begin
  i := ORD(Fieldtype);
  if i > 20 then
    Result := 'Unknown type'
  else
    Result := FieldTypeNames[i];

  if not Assigned(Lang) then exit;

  case i of
    0: result := lang(50100, Result);
    1: result := lang(50101, Result);
    2: result := lang(50102, Result);
    3: result := lang(50103, Result);
    4: result := Result;
    5: result := lang(50105, Result);
    6: result := lang(50100, Result);
    7: result := Result;
    8: result := Result;
    9: result := Result;
    10: result := lang(50110, Result);
    11: result := lang(50111, Result);
    12: result := lang(50112, Result);
    15: result := lang(50115, Result);
    16: result := lang(50116, Result);
    17: result := lang(50117, Result);
    18: result := lang(50118, Result);
    19: result := lang(50119, Result);
    20: result := lang(50120, Result);
  end;
end;

function IsCompliant(Value: String; Ft: TFieldType):Boolean;
begin
  Result := True;
  case Ft of
    ftInteger, ftIDNUM: Result := IsInteger(Value);
    ftFloat:            Result := IsFloat(Value);
    ftBoolean:          Result := (Value[1] in BooleanChars);
    ftToday, ftEuroDate,
    ftDate, ftEuroToday,
    ftYMDDate,
    ftYMDToday:         Result := IsDate(Value, Ft);
    ftUpperAlfa:        Result := AnsiUpperCase(Value) = Value;
  end;
end;

function IsInteger(Value: String): boolean;
var
  V, Code: integer;
begin
  Val(Value, V, Code);
  Result := (Code = 0);
end;

function IsFloat(Value: String): boolean;
var
  Code: integer;
  V: Extended;
begin
  Val(Value, V, Code);
  Result := (Code = 0);
end;

function PreInc(Var I: Integer; Const N: Integer = 1): Integer;
begin
  Inc(I, N);
  Result := I;
end;

function PostInc(Var I: Integer; Const N: Integer = 1): Integer;
begin
  Result := I;
  Inc(I, N);
end;

procedure GetCoreSystemInformation(var CSI: TCoreSystemInformation);
var
  {$IFNDEF LINUX}
  Ms: TMemoryStatus;
  MsEx: TMemoryStatusEx;
  Ovi: TOSVersionInfo;
  {$ENDIF}
  Dummy: integer;
begin
  {$IFDEF LINUX}
  CSI.OSName := '';
  CSI.OSMajorVersion := 0;
  CSI.OSMinorVersion := 0;
  CSI.MemSize := 0;
  CSI.MemUsage := 0;
  {$ELSE}
  Ovi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Ovi);

  CSI.OSName         := 'Windows';
  CSI.OSMajorVersion := Ovi.dwMajorVersion;
  CSI.OSMinorVersion := Ovi.dwMinorVersion;

  if (Ovi.dwMajorVersion < 5) or (Ovi.dwPlatformId <> VER_PLATFORM_WIN32_NT) then
  begin
    Ms.dwLength := SizeOf(TMemoryStatus);
    GlobalMemoryStatus(Ms);
    CSI.MemSize := Ms.dwTotalPhys;
    CSI.MemUsage := Ms.dwMemoryLoad;
  end else begin
    MsEx.dwLength := SizeOf(TMemoryStatusEx);
    GlobalMemoryStatusEx(MsEx);
    CSI.MemSize := MsEx.ullTotalPhys;
    CSI.MemUsage := MsEx.dwMemoryLoad;
  end;
  CSI.CoreVersion := CoreVersion;
  // TODO -o Torsten : Get Subversion revision!
  CSI.CoreRevision := 0;
  {$ENDIF}
end;

end.
