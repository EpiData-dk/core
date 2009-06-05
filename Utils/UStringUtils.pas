unit UStringUtils;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFNDEF FPC} Windows, {$ENDIF}
  Classes,  SysUtils, UEpiDataConstants;

type

  TString = class(TObject)
  private
    fStr: String;
  public
    constructor Create(const AStr: String) ;
    property Str: String read FStr write FStr;
  end;

  function FirstWord(Const S: String; MaxLength: Cardinal): string;
  Function FitLength(Const S: String; L: Integer):String;
  procedure SplitString(const Source: string; var List: TStrings; const Splitters: TCharset = [' ']);

implementation

uses
  Math,
  StrUtils;
{ TString }

constructor TString.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
end;



function FirstWord(Const S: String; MaxLength: Cardinal): string;
var
  n: Integer;
begin
  Result := StringReplace(S, #9, ' ', [rfReplaceAll]);
  n := Math.Min(Pos(' ',s), MaxLength + 1);
  if n = 0 then
    n := Length(s) + 1;
  Result := Copy(s, 1, n-1);
end;

Function FitLength(Const S: String; L: Integer):String;
{Makes sure that a string is exactly L chars in length}
begin
  Result := s;
  if Length(s) > L then
    Result := Copy(s,1,L)
  else if Length(s) < L then
    Result := s + DupeString(' ', L-Length(s));
end;


procedure SplitString(const Source: string; var List: TStrings; const Splitters: TCharset = [' ']);
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
      if P^ = '"' then
        S := AnsiExtractQuotedStr(P, '"')
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
       
end.