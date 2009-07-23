unit UEpiUtils;

{$mode objfpc}{$H+}

interface

USES
  SysUtils, UEpiDataGlobals, UDataFileTypes;

type
  TPrgVersionInfo = record
    Major:    Word;
    Minor:    Word;
    Release:  Word;
    Build:    Word;
  end;

  PCoreSystemInformation = ^TCoreSystemInformation;
  TCoreSystemInformation = record
    OSName:         string;     // Windows, Linux (or Mac?).
    OSMajorVersion: Word;       //
    OSMinorVersion: Word;       //
    MemSize:        Int64;      // Physical memory in Bytes.
    MemUsage:       Integer;    // Usage level of memory in percent.
    PrgVersion:     TPrgVersionInfo;
    CoreVersion:    Cardinal;   // Version of Core and utils.
    CoreRevision:   Cardinal;   // Subversion revision.
  end;

  // Validity checks!
  function CheckVariableName(Const VarName: string; ValidChars: TCharSet): boolean;
  function FieldTypeToFieldTypeName(FieldType: TFieldType; Lang: TTranslateEvent): widestring;
  function IsCompliant(Value: string; Ft: TFieldType):Boolean;
  function IsInteger(Value: string): boolean;
  function IsFloat(Value: string): boolean;
  function FindFieldType(var Value: String; const PrevFT: TFieldType = ftInteger): TFieldType;

  // Custom operators.
  function PreInc(Var I: Integer; Const N: Integer = 1): Integer;
  function PostInc(Var I: Integer; Const N: Integer = 1): Integer;

  // Misc. conversion.
  function BoolStrToInt(Const AValue: string): integer;
  {$IFNDEF VER2_3}
  function BoolToStr(B: Boolean;const TrueS,FalseS:string): string; inline; overload;
  {$ENDIF}

  function GetEncodedLength(decodedlength: byte): byte;
  function GetDecodedLength(encodedlength: byte): byte;

  procedure GetCoreSystemInformation(var CSI: TCoreSystemInformation);


{$IFNDEF FPC}
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
  {$IFDEF LINUX} Linux, baseunix, {$ENDIF}
  UDateUtils, Math;

{$IFNDEF FPC}
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

function IsCompliant(Value: string; Ft: TFieldType):Boolean;
begin
  Result := True;
  case Ft of
    ftInteger, ftIDNUM: Result := IsInteger(Value);
    ftFloat:            Result := IsFloat(Value);
    ftBoolean:          Result := (Value[1] in BooleanChars);
    ftToday, ftEuroDate,
    ftDate, ftEuroToday,
    ftYMDDate,
    ftYMDToday:         Result := EpiIsDate(Value, Ft);
    ftUpperAlfa:        Result := AnsiUpperCase(Value) = Value;
  end;
end;

function IsInteger(Value: string): boolean;
var
  V, Code: integer;
begin
  Val(Value, V, Code);
  Result := (Code = 0);
end;

function IsFloat(Value: string): boolean;
var
  Code: integer;
  V: Extended;
begin
  Val(Value, V, Code);
  Result := (Code = 0);
end;

(*
  TFieldType   = (ftInteger, ftAlfa, ftDate, ...,
                  ftFloat, ..., ftEuroDate, ...,
                  ftYMDDate, ...); *)

// FindFieldType:
//  - Tries to find the field type based on the following precedence: (lowest first)
//  - Interger, Float, Date(MDY, DMY, YMD), String.

function FindFieldType(var Value: String; const PrevFT: TFieldType = ftInteger): TFieldType;
begin
  if (PrevFT = ftInteger)                                 and IsInteger(Value)             then result :=ftInteger
  else if (PrevFT in [ftInteger, ftFloat])                and IsFloat(Value)               then result :=ftFloat
  else if (PrevFT in [ftInteger, ftFloat, ftDate])        and EpiIsDate(Value, ftDate)     then result := ftDate
  else if ((PrevFT <> ftAlfa) and (PrevFT <= ftEuroDate)) and EpiIsDate(Value, ftEuroDate) then result := ftEuroDate
  else if ((PrevFT <> ftAlfa) and (PrevFT <= ftYMDDate))  and EpiIsDate(Value, ftYMDDate)  then result := ftYMDDate
  else result := ftAlfa;
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

{$IFNDEF FPC}
procedure GetBuildInfo(var PrgInfo: TPrgVersionInfo);
var
  VerInfoSize:  DWORD;
  VerInfo:      Pointer;
  VerValueSize: DWORD;
  VerValue:     PVSFixedFileInfo;
  Dummy:        DWORD;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    PrgInfo.Major   := dwFileVersionMS shr 16;
    PrgInfo.Minor   := dwFileVersionMS and $FFFF;
    PrgInfo.Release := dwFileVersionLS shr 16;
    PrgInfo.Build   := dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;
{$ENDIF FPC}

function BoolStrToInt(const AValue: string): integer;
begin
  Result := 0;
  if Length(AValue) = 0 then exit;
  Result := Integer(AValue[1] in BooleanYesChars);
end;

{$IFNDEF VER2_3}
function BoolToStr(B: boolean; const TrueS, FalseS: string): string;
begin
  if B then Result:=TrueS else BoolToStr:=FalseS;
end;
{$ENDIF}

function GetEncodedLength(decodedlength: byte): byte;
begin
  result := (decodedlength div 3) * 4;
end;

function GetDecodedLength(encodedlength: byte): byte;
begin
  result := (encodedlength div 4) * 3;
end;

procedure GetCoreSystemInformation(var CSI: TCoreSystemInformation);
var
  {$IFNDEF LINUX}
  {$IFNDEF FPC}
  Ms: TMemoryStatus;
  MsEx: TMemoryStatusEx;
  Ovi: TOSVersionInfo;
  {$ENDIF FPC}
  {$ELSE LINUX}
  PInfo: PSysInfo;
  Info: TSysInfo;
  UName: UtsName;
//  ResHandle: TFPResourceHandle;
//  ResMan: TResourceManager;
  {$ENDIF LINUX}
  Dummy: integer;
begin
  {$IFDEF LINUX}
//  GetResourceManager(ResMan);
//  FindResource(ResMan.HINSTANCEFunc(), RT_VERSION, RT_VERSION);
  FpUname(UName);
  CSI.OSName := string(UName.Sysname);
  PInfo := new(PSysInfo);
  Sysinfo(PInfo);
  Info := PInfo^;
  CSI.MemSize := (Info.totalram * Info.mem_unit);
  CSI.MemUsage := Floor(100 * (Info.totalram - (Info.freeram)) / Info.totalram);
  {$ELSE}
  CSI.OSName := 'Windows';
  CSI.MemSize := 0;
  CSI.MemUsage := 0;
  {$ENDIF}
  CSI.OSMajorVersion := 0;
  CSI.OSMinorVersion := 0;

  CSI.PrgVersion.Major   := 0;
  CSI.PrgVersion.Minor   := 1;
  CSI.PrgVersion.Release := 1;
  CSI.PrgVersion.Build   := 94;



{  {$ELSE}
  {$IFNDEF FPC}
  GetBuildInfo(CSI.PrgVersion);
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
  {$ENDIF}
  {$ENDIF}             }
  CSI.CoreVersion := CoreVersion;
  // TODO -o Torsten : Get Subversion revision!
  CSI.CoreRevision := 0;
end;

end.
