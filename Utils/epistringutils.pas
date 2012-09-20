unit epistringutils;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes,  SysUtils;


type

  { TString }

  TString = class(TObject)
  private
    fStr: string;
  public
    constructor Create(const AStr: string);
    destructor Destroy; override;
    property Str: string read FStr write FStr;
  end;

  TCharSet = Set of Char;

  function CountChar(Const UTF8String: string; Const WChar: WideChar): integer;
  function ExtractStrBetween(const Source: string; BeginChar, EndChar: Char): string;
  procedure SplitString(const Source: string; var List: TStrings;
    const Splitters: TCharset = [' ']; const QuoteChars: TCharSet = ['"']);

  // Conversion routines regarding UTF-8 etc.
  function EpiUnknownStrToUTF8(const Source: string): string;
  procedure EpiUnknownStringsToUTF8(Source: TStrings);
  function EpiUtf8ToAnsi(Const Source: string): string;

  // Other UTF8 routines.
  function EpiCutString(Const S: string; Const MaxLength: integer; Const AddDots: boolean = true): string;

  function FirstWord(Const S: string; MaxLength: Cardinal = (MaxInt-1)): string;
  function AutoFieldName(Const S: string): string;
  function ValidateIdentifierUTF8(Const AValue: string): boolean;
  function CheckVariableName(Const VarName: string; ValidChars: TCharSet): boolean;
  function CreateUniqueAnsiVariableName(Const Varname: string; MaxLength: integer;
    CompareList: TStrings = nil): String;

implementation

uses
  LConvEncoding, FileUtil, math, LazUTF8, lazutf16;

function CountChar(const UTF8String: string; Const WChar: WideChar): integer;
var
  WS: WideString;
  L: LongInt;
  i: Integer;
begin
  WS := UTF8ToUTF16(UTF8String);
  L := UTF16Length(WS);

  result := 0;
  for i := 1 to L do
    if WS[i] = WChar then
      inc(result);
end;

function ExtractStrBetween(const Source: string; BeginChar, EndChar: Char): string;
var
  PB, PE: PChar;
begin
  Result := '';
  // Work with PChars.
  PB := PChar(Source);
  // Skip until first occurance of BeginChar.
  while not (PB^ in [#0, BeginChar]) do PB := PB + 1;
  // We may have reached the end.
  if PB^ = #0 then exit;
  // Start search from next char, else if Begin and EndChar are the same
  // the return is an empty string.
  PE := PB + 1;
  // Skip until end or EndChar.
  while not (PE^ in [#0, EndChar]) do PE := PE + 1;
  // If end is reached here, there were no termination to "between" section.
  if PE^= #0 then exit;
  SetString(result, PB + 1, PE - PB - 1);
end;

procedure SplitString(const Source: string; var List: TStrings;
  const Splitters: TCharset = [' ']; const QuoteChars: TCharSet = ['"']);
var
  P, P1: PChar;
  S: string;
begin
  if not Assigned(List) then
    List := TStringList.Create;
  list.BeginUpdate;
  try
    list.Clear;
    P := PChar(source);
    while P^ in [#1..' '] do P := P + 1;
    while P^ <> #0 do
    begin
      if P^ in QuoteChars then
        S := AnsiExtractQuotedStr(P, P^)
      else
      begin
        P1 := P;
        while (not (P^ in Splitters)) and (P^ <> #0) do P := P + 1;
        SetString(S, P1, P - P1);
      end;
      list.Add(S);
      while P^ in Splitters do P := P + 1;
    end;
  finally
    list.EndUpdate;
  end;
end;

function EpiUnknownStrToUTF8(const Source: string): string;
var
  EncStr: String;
begin
  Result := '';
  if Trim(Source) = '' then Exit;
  EncStr := GuessEncoding(Source);
  Result := ConvertEncoding(Source, EncStr, 'utf8');
end;

procedure EpiUnknownStringsToUTF8(Source: TStrings);
var
  i: Integer;
begin
  for i := 0 to Source.Count -1 do
    Source[i] := EpiUnknownStrToUTF8(Source[i]);
end;

function EpiUtf8ToAnsi(const Source: string): string;
begin
  {$IFDEF MSWINDOWS}
  result := UTF8ToSys(Source);
  {$ELSE}
  result := UTF8ToISO_8859_1(Source);
  {$ENDIF}
end;

function EpiCutString(const S: string; const MaxLength: integer;
  const AddDots: boolean): string;
begin
  if (UTF8Length(S) > MaxLength) then
    Result := UTF8Copy(S, 1, MaxLength - 3) + '...'
  else
    Result := S;
end;

function FirstWord(Const S: string; MaxLength: Cardinal): string;
var
  n: Integer;
begin
  Result := UTF8Decode(S);
  Result := TrimLeft(Result);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
  n := Math.Min(Pos(' ',Result), MaxLength + 1);
  if n = 0 then
    n := Length(Result) + 1;
  Result := Copy(Result, 1, n-1);
  Result := UTF8Encode(Result);
end;

function AutoFieldName(const S: string): string;
begin
  Result := UTF8Encode(StringReplace(Trim(UTF8Decode(S)), ' ', '_', [rfReplaceAll]));
end;

function ValidateIdentifierUTF8(const AValue: string): boolean;
begin
  result := UTF8Length(AValue) > 0;
  if FindInvalidUTF8Character(PChar(@AValue[1]), Length(AValue)) <> -1 then
    exit(false);
  if UTF8Pos(' ', AValue) > 0 then
    exit(false);
end;

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
    while (CompareList.IndexOf(TmpStr) <> -1) or (not CheckVariableName(TmpStr, ['a'..'z', 'A'..'Z','0'..'9','_'])) do
    begin
      TmpStr := 'V' + IntToStr(Number);
      Inc(Number)
    end;
    CompareList.Add(TmpStr);
  end else if (not CheckVariableName(TmpStr, ['a'..'z', 'A'..'Z','0'..'9','_'])) then
    TmpStr := 'V1';

  Result := TmpStr;
end;

{ TString }

constructor TString.Create(const AStr: string);
begin
  fStr := AStr;
end;

destructor TString.Destroy;
begin
  fStr := '';
  inherited Destroy;
end;

end.
