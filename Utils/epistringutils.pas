unit epistringutils;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes,  SysUtils;

  function ExtractStrBetween(const Source: string; BeginChar, EndChar: Char): string;

implementation

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

end.
