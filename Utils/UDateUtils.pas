unit UDateUtils;

interface

uses
  SysUtils, UDataFileTypes;

  function IsDate(Const Value: String; Ft: TFieldType): boolean;
  function EpiDateToDateTime(Const Str: String; Ft: TFieldType; Len: Integer): TDateTime;

implementation


function IsDate(Const Value: String; Ft: TFieldType): boolean;
begin
  // TODO -o Torsten : Implement IsDate...
  result := true;
end;

function EpiDateToDateTime(Const Str: String; Ft: TFieldType; Len: Integer): TDateTime;
var
  eDay,eMonth,eYear: Word;
BEGIN
//  DecodeDate(Date, eYear, eMonth, eDay);
  Case Len OF
    8:  IF StrToInt(Copy(Str, 7, 2)) < 50 THEN
          eYear := 2000 + StrToInt(Copy(Str, 7, 2))
        ELSE
          eYear := 1900 + StrToInt(Copy(Str, 7, 2));
    10: IF (Ft = ftYMDDate) or (Ft = ftYMDToday) THEN
          eYear := StrToInt(Copy(Str, 1, 4))
        ELSE
          eYear := StrToInt(Copy(Str, 7, 4));
  END;
  TRY
    CASE Ft OF
      ftDate, ftToday:
        BEGIN
          eMonth := StrToInt(Copy(Str, 1, 2));
          eDay   := StrToInt(Copy(Str, 4, 2));
        END;
      ftEuroDate,ftEuroToday:
        BEGIN
          eMonth := StrToInt(Copy(Str, 4, 2));
          eDay   := StrToInt(Copy(Str, 1, 2));
        END;
      ftYMDDate, ftYMDToday:
        BEGIN
          eMonth := StrToInt(Copy(Str, 6, 2));
          eDay   := StrToInt(Copy(Str, 9, 2));
        END;
    END;
    Result := EncodeDate(eYear, eMonth, eDay);
  EXCEPT
    Result := 0;
  END;
END;
end.
