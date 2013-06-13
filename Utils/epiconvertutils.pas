unit epiconvertutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, epidatafilestypes;


function EpiStrToDate(Const Str: string; Const Separator: Char;
  Const FT: TEpiFieldType; out D, M, Y: Word; out ErrMsg: string): boolean; overload;
function EpiStrToDate(Const Str: string; Const Separator: Char;
  Const FT: TEpiFieldType; out ErrMsg: string): EpiDate; overload;
function EpiStrToDate(Const Str: string; Const Separator: Char;
  Const FT: TEpiFieldType; out TheDate: EpiDate; out ErrMsg: string): boolean; overload;
function EpiStrToDateGuess(Const Str: string; out TheDate: EpiDate;
  out ErrMsg: string): boolean;


function EpiStrToTime(Const Str: string; Const Separator: Char;
  out H, M, S: Word; out ErrMsg: string): boolean; overload;
function EpiStrToTime(Const Str: string; Const Separator: Char;
  out ErrMsg: string): EpiTime; overload;
function EpiStrToTime(Const Str: string; Const Separator: Char;
  out TheTime: EpiTime; out ErrMsg: string): boolean; overload;
function EpiStrToTimeGues(Const Str: string; out TheTime: EpiTime;
  out ErrMsg: string): boolean; overload;

implementation

uses
  epimiscutils, dateutils, epidatafiles;

function EpiStrToDate(Const Str: string; Const Separator: Char;
  Const FT: TEpiFieldType; out D, M, Y: Word; out ErrMsg: string): boolean; overload;
var
  A, B, C: String;
  Al, Bl, Cl: Integer;
  DateStr: String;
  Mis: EpiString;

  function ValidateError(Const Msg: string): boolean;
  begin
    ErrMsg := Msg;
    result := false;
  end;

begin
  DateStr := Str;
  A := Copy2SymbDel(DateStr, Separator);
  Al := Length(A);
  B := Copy2SymbDel(DateStr, Separator);
  Bl := Length(B);
  C := Copy2SymbDel(DateStr, Separator);
  Cl := Length(C);

  // Handle a missing in text:
  Mis := TEpiStringField.DefaultMissing;
  if (A = Mis) or (B = Mis) or (C = Mis) then
    Exit(ValidateError(Mis));

  DecodeDate(Date, Y, M, D);

  case Ft of
    ftDMYDate, ftDMYAuto,
    ftMDYDate, ftMDYAuto:
      begin
        if (Al > 2) or (Bl > 2) or (Cl > 4) then
        begin
          ErrMsg := Str + ' is not a valid date.' + LineEnding + 'Forgot separators?';
          exit(false);
        end;

        if Ft in [ftDMYDate, ftDMYAuto] then
        begin
          if (Al > 0) then D := StrToInt(A);
          if (Bl > 0) then M := StrToInt(B);
        end else begin
          // Only 2 digits enteres -> any case, it's considered to be the day.
          if (Al > 0) and (Bl = 0) then D := StrToInt(A);
          if (Al > 0) and (Bl > 0) then M := StrToInt(A);
          if (Bl > 0) then D := StrToInt(B);
        end;
        if (Cl > 0) then Y := StrToInt(C);
      end;
    ftYMDDate, ftYMDAuto:
      begin
        if (Al > 4) or (Bl > 2) or (Cl > 2) then
        begin
          ErrMsg := Str + ' is not a valid date.' + LineEnding + 'Forgot separators?';
          exit(false);
        end;
        if (Al > 0) and (Al <= 2) and (Bl+Cl = 0) then
          D := StrToInt(A)
        else begin
          if (Al > 0) then Y := StrToInt(A);
          if (Bl > 0) then M := StrToInt(B);
          if (Cl > 0) then D := StrToInt(C);
        end;
      end;
  else
    exit(ValidateError(Format('EpiStrToDate: Invalid field type (%s)', [EpiTypeNames[Ft]])));
  end;

  // 2 year digit conversion.
  if Y < 100 then
    if Y <= (YearOf(Date)-2000) then
      Y += 2000
    else
      Y += 1900;

  // I don't what to use try-except... :)
  if (Y <= 0) or (Y >= 2100) then exit(ValidateError(Format('Incorrect year: %d', [Y])));
  if (M <= 0) or (M > 12) then exit(ValidateError(Format('Incorrect month: %d', [M])));
  case M of
    1,3,5,7,8,10,12:
      if (D <= 0) or (D > 31) then exit(ValidateError(Format('Incorrect day: %d', [D])));
    4,6,9,11:
      if (D <= 0) or (D > 30) then exit(ValidateError(Format('Incorrect day: %d', [D])));
    2:
      if IsLeapYear(Y) then
        begin if (D <= 0) or (D > 29) then exit(ValidateError(Format('Incorrect day: %d', [D]))); end
      else
        begin if (D <= 0) or (D > 28) then exit(ValidateError(Format('Incorrect day: %d', [D]))); end;
  end;
  Result := true;
end;

function EpiStrToDate(const Str: string; const Separator: Char;
  const FT: TEpiFieldType; out ErrMsg: string): EpiDate;
var
  D, M, Y: Word;
begin
  if EpiStrToDate(Str, Separator, Ft, D, M, Y, ErrMsg) then
    Result := Trunc(EncodeDate(Y, M, D))
  else
    Result := 0;
end;

function EpiStrToDate(const Str: string; const Separator: Char;
  const FT: TEpiFieldType; out TheDate: EpiDate; out ErrMsg: string): boolean;
var
  D, M, Y: Word;
begin
  result := EpiStrToDate(Str, Separator, Ft, D, M, Y, ErrMsg);
  if result then
    TheDate := Trunc(EncodeDate(Y, M, D));
end;

function EpiStrToDateGuess(const Str: string; out TheDate: EpiDate; out
  ErrMsg: string): boolean;
const
  Separators: array[0..2] of Char =
    ('-', '/', '.');
var
  Sep: Char;
  Ft: TEpiFieldType;
begin
  Result := true;
  for Sep in Separators do
    for Ft in (DateFieldTypes - AutoFieldTypes) do
      begin
        if EpiStrToDate(Str, Sep, Ft, TheDate, ErrMsg) then
          Exit;
      end;
  TheDate := 0;
  ErrMsg := 'Could not guess String-to-Date';
  Result := false;
end;

function EpiStrToTime(const Str: string; const Separator: Char; out H, M,
  S: Word; out ErrMsg: string): boolean;
var
  TimeStr: String;
  A,B,C: String;
  Al,Bl,Cl: Integer;

  function ValidateError(Const Msg: string): boolean;
  begin
    ErrMsg := Msg;
    result := false;
  end;

begin
  TimeStr := Str;
  A := Copy2SymbDel(TimeStr, Separator);
  Al := Length(A);
  B := Copy2SymbDel(TimeStr, Separator);
  Bl := Length(B);
  C := Copy2SymbDel(TimeStr, Separator);
  Cl := Length(C);

  H := 0; M := 0; S := 0;
  try
    if Al > 0 then H := StrToInt(A);
    if Bl > 0 then M := StrToInt(B);
    if Cl > 0 then S := StrToInt(C);
  except
    Result := false;
    Exit;
  end;

  if H > 23 then exit(ValidateError(Format('Incorrect hour: %d', [H])));
  if M > 59 then exit(ValidateError(Format('Incorrect minut: %d', [M])));
  if S > 59 then exit(ValidateError(Format('Incorrect second: %d', [S])));
  Result := true;
end;

function EpiStrToTime(const Str: string; const Separator: Char; out
  ErrMsg: string): EpiTime;
var
  H, M, S: Word;
begin
  if EpiStrToTime(Str, Separator, H, M, S, ErrMsg) then
    Result := EncodeTime(H, M, S, 0)
  else
    Result := 0;
end;

function EpiStrToTime(const Str: string; const Separator: Char;
  out TheTime: EpiTime; out ErrMsg: string): boolean;
var
  H, M, S: Word;
begin
  result := EpiStrToTime(Str, Separator, H, M, S, ErrMsg);
  if result then
    TheTime := EncodeTime(H, M, S, 0);
end;

function EpiStrToTimeGues(const Str: string; out TheTime: EpiTime; out
  ErrMsg: string): boolean;
const
  Separators: array[0..1] of Char =
    ('.', ':');
var
  Sep: Char;
begin
  Result := True;
  for Sep in Separators do
    if EpiStrToTime(Str, Sep, TheTime, ErrMsg) then
      Exit;
  TheTime := 0;
  ErrMsg := 'Could not guess String-to-Time';
  Result := false;
end;

end.

