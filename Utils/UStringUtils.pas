unit UStringUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes,  SysUtils, UEpiDataGlobals, UUtilTypes;

type

  TString = class(TObject)
  private
    fStr: string;
  public
    constructor Create(const AStr: string) ;
    property Str: string read FStr write FStr;
  end;

  function FirstWord(Const S: string; MaxLength: Cardinal): string;
  Function FitLength(Const S: string; L: Integer):string;
  procedure SplitString(const Source: string; var List: TStrings;
    const Splitters: TCharset = [' ']; const QuoteChars: TCharSet = ['"']);
  function StrCountChars(const Source: string; const FindChars: TCharSet): integer;
  function ExtractStrBetween(const Source: string; BeginChar, EndChar: Char): string;
  function StripWordsFromString(Const Source: string; StripWords: array of string): string;

  // Conversion routines regarding UTF-8 etc.
  function EpiUnknownStrToUTF8(const Source: string): string;
  procedure EpiUnknownStringsToUTF8(Source: TStrings);
  function EpiUtf8ToAnsi(Const Source: string): string;


implementation

uses
  Math, LConvEncoding,
  StrUtils, FileUtil;

{ TString }

constructor TString.Create(const AStr: string) ;
begin
   inherited Create;
   FStr := AStr;
end;



function FirstWord(Const S: string; MaxLength: Cardinal): string;
var
  n: Integer;
begin
  Result := StringReplace(S, #9, ' ', [rfReplaceAll]);
  n := Math.Min(Pos(' ',s), MaxLength + 1);
  if n = 0 then
    n := Length(s) + 1;
  Result := Copy(s, 1, n-1);
end;

Function FitLength(Const S: string; L: Integer):string;
{Makes sure that a string is exactly L chars in length}
begin
  Result := s;
  if Length(s) > L then
    Result := Copy(s,1,L)
  else if Length(s) < L then
    Result := s + DupeString(' ', L-Length(s));
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

function StrCountChars(const Source: string; const FindChars: TCharSet): integer;
var
  i: integer;
begin
  result := 0;
  i := 1;
  while i < Length(Source) do
  begin
    if Source[i] in FindChars then
      inc(result);
    // TODO : Change to variable quote.
    if Source[i] = '"' then
    begin
      repeat inc(i) until (i >= Length(Source)) or (Source[i] = '"');
    end;
    inc(i);
  end;
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
  PE := PB;
  // Skip until end or EndChar.
  while not (PE^ in [#0, EndChar]) do PE := PE + 1;
  // If end is reached here, there were no termination to "between" section.
  if PE^= #0 then exit;
  SetString(result, PB, PE - PB);
end;

function StripWordsFromString(const Source: string; StripWords: array of string
  ): string;
var
  NumWords: Integer;
  i: Integer;
begin
  Result := Source;
  for i := 0 to NumWords -1 do
    Result := StringReplace(Result, StripWords[i], '', [rfIgnoreCase, rfReplaceAll]);
end;

function EpiUnknownStrToUTF8(const Source: string): string; overload;
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
       
end.
