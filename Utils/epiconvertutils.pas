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

function EpiStrToTime(Const Str: string; Const Separator: Char;
  out H, M, S: Word; out ErrMsg: string): boolean; overload;
function EpiStrToTime(Const Str: string; Const Separator: Char;
  out ErrMsg: string): EpiTime; overload;
function EpiStrToTime(Const Str: string; Const Separator: Char;
  out TheTime: EpiTime; out ErrMsg: string): boolean; overload;

implementation

uses
  epimiscutils, dateutils;

function EpiStrToDate(Const Str: string; Const Separator: Char;
  Const FT: TEpiFieldType; out D, M, Y: Word; out ErrMsg: string): boolean; overload;
var
  A, B, C: String;
  Al, Bl, Cl: Integer;
  DateStr: String;

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

  DecodeDate(Date, Y, M, D);

  case Ft of
    ftDMYDate, ftDMYToday,
    ftMDYDate, ftMDYToday:
      begin
        if (Al > 2) or (Bl > 2) or (Cl > 4) then exit(false);

        if Ft in [ftDMYDate, ftDMYToday] then
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
    ftYMDDate, ftYMDToday:
      begin
        if (Al > 4) or (Bl > 2) or (Cl > 2) then exit(false);
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
  if Al > 0 then H := StrToInt(A);
  if Bl > 0 then M := StrToInt(B);
  if Cl > 0 then S := StrToInt(C);

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

end.

