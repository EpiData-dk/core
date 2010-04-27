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

implementation

uses
  LConvEncoding, FileUtil;

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
