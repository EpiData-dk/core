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

  function ExtractStrBetween(const Source: string; BeginChar, EndChar: Char): string;
  procedure SplitString(const Source: string; var List: TStrings;
    const Splitters: TCharset = [' ']; const QuoteChars: TCharSet = ['"']);

  // Conversion routines regarding UTF-8 etc.
  function EpiUnknownStrToUTF8(const Source: string): string;
  procedure EpiUnknownStringsToUTF8(Source: TStrings);
  function EpiUtf8ToAnsi(Const Source: string): string;

  function FirstWord(Const S: string; MaxLength: Cardinal = (MaxInt-1)): string;
  function AutoFieldName(Const S: string): string;
  function ValidateIdentifierUTF8(Const AValue: string): boolean;

implementation

uses
  LConvEncoding, FileUtil, math, LCLProc;

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
  result := true;
  if FindInvalidUTF8Character(PChar(@AValue[1]), Length(AValue)) <> -1 then
    exit(false);
  if UTF8Pos(' ', AValue) > 0 then
    exit(false);
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
