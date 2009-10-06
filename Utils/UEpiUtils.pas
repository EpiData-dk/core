unit UEpiUtils;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

USES
  SysUtils, UEpiDataGlobals, UDataFileTypes, Classes;

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
  function CreateUniqueAnsiVariableName(Const Varname: string; MaxLength: integer;
    CompareList: TStrings = nil): String;
  function FieldTypeToFieldTypeName(FieldType: TFieldType; Lang: TTranslateEvent): string;
  function IsCompliant(Value: string; Ft: TFieldType):Boolean;
  function IsInteger(const Value: string): boolean;
  function IsFloat(var Value: string): boolean;
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

implementation

uses
  {$IFDEF LINUX} Linux, baseunix, {$ENDIF}
  UDateUtils, Math, UStringUtils;

{$IFDEF MSWINDOWS}
type
  MEMORYSTATUSEX = record
    dwLength :     Cardinal;
    dwMemoryLoad : Cardinal;
    ullTotalPhys : UInt64;
    ullAvailPhys : UInt64;
    ullTotalPageFile : UInt64;
    ullAvailPageFile : UInt64;
    ullTotalVirtual : UInt64;
    ullAvailVirtual : UInt64;
    ullAvailExtendedVirtual: UInt64;
  end;

  procedure GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX); stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';
{$ENDIF}


function CheckVariableName(Const VarName: string; ValidChars: TCharSet): boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to Length(VarName) do
    if not (Varname[i] in ValidChars) THEN exit;
  result := true;
end;

function CreateUniqueAnsiVariableName(const Varname: string;
  MaxLength: integer; CompareList: TStrings): String;
var
  TmpStr: String;
  Number: Integer;
begin
  TmpStr := EpiUtf8ToAnsi(Varname);

  if Length(TmpStr) > MaxLength then
    TmpStr := Copy(TmpStr, 1, MaxLength);

  if Assigned(CompareList) then
  begin
    Number := 1;
    while (CompareList.IndexOf(TmpStr) <> -1) and (not CheckVariableName(TmpStr, AlfaNumChars + ['_'])) do
    begin
      TmpStr := 'V' + IntToStr(Number);
      Inc(Number)
    end;
    CompareList.Add(TmpStr);
  end else if (not CheckVariableName(TmpStr, AlfaNumChars + ['_'])) then
    TmpStr := 'V1';

  Result := TmpStr;
end;

function FieldTypeToFieldTypeName(FieldType: TFieldType; Lang: TTranslateEvent): string;
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

{$IFOPT R+}
{$DEFINE EPI_R_DEFINED}
{$ENDIF}
{$R-}
function IsInteger(const Value: string): boolean;
var
  V, Code: integer;
begin
  Val(Value, V, Code);
  Result := (Code = 0);
end;

function IsFloat(var Value: string): boolean;
var
  Code: integer;
  V: Extended;
begin
  Value := StringReplace(Value, ',', EpiInternalFormatSettings.DecimalSeparator, [rfReplaceAll]);
  Val(Value, V, Code);
  if Value[Length(Value)] = EpiInternalFormatSettings.DecimalSeparator then
    Code := Length(Value);
  Result := (Code = 0);
end;
{$IFDEF EPI_R_DEFINED}
{$UNDEF EPI_R_DEFINED}
{$R+}
{$ENDIF}

// FindFieldType:
//  - Tries to find the field type based on the following precedence: (lowest first)
//  - Interger, Float, Date(MDY, DMY, YMD), String.

function FindFieldType(var Value: String; const PrevFT: TFieldType = ftInteger): TFieldType;
begin
  if Trim(Value) = '' then exit(PrevFt);

  if (PrevFT = ftInteger)                                 and IsInteger(Value)             then result :=ftInteger
  else if (PrevFT in [ftInteger, ftFloat])                and IsFloat(Value)               then result :=ftFloat
  else if (PrevFT in [ftInteger, ftFloat, ftDate])        and EpiIsDate(Value, ftDate)     then result := ftDate
  else if ((PrevFT <> ftString) and (PrevFT <= ftEuroDate)) and EpiIsDate(Value, ftEuroDate) then result := ftEuroDate
  else if ((PrevFT <> ftString) and (PrevFT <= ftYMDDate))  and EpiIsDate(Value, ftYMDDate)  then result := ftYMDDate
  else result := ftString;
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
  {$IFDEF LINUX}
  PInfo: PSysInfo;
  Info: TSysInfo;
  UName: UtsName;
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  WinMem: MEMORYSTATUSEX;
  {$ENDIF WINDOWS}
  Dummy: integer;
begin
  CSI.OSName := 'Mac';
  CSI.MemSize := 0;
  CSI.MemUsage := 0;
  {$IFDEF LINUX}
  FpUname(UName);
  CSI.OSName := string(UName.Sysname);
  PInfo := new(PSysInfo);
  Sysinfo(PInfo);
  Info := PInfo^;
  CSI.MemSize := (Info.totalram * Info.mem_unit);
  CSI.MemUsage := Floor(100 * (Info.totalram - (Info.freeram)) / Info.totalram);
  {$ENDIF LINUX}
  {$IFDEF WINDOWS}
  WinMem.dwLength := SizeOf(WinMem);
  GlobalMemoryStatusEx(WinMem);
  CSI.OSName := 'Windows';
  CSI.MemSize := WinMem.ullTotalPhys;
  CSI.MemUsage := WinMem.dwMemoryLoad;
  {$ENDIF WINDOWS}
  CSI.OSMajorVersion := 0;
  CSI.OSMinorVersion := 0;

  CSI.PrgVersion.Major   := 0;
  CSI.PrgVersion.Minor   := 2;
  CSI.PrgVersion.Release := 1;
  CSI.PrgVersion.Build   := 98;

  CSI.CoreVersion := CoreVersion;
  // TODO -o Torsten : Get Subversion revision!
  CSI.CoreRevision := 0;
end;

end.

