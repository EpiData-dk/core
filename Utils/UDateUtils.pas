unit UDateUtils;

interface

uses
  SysUtils, UDataFileTypes, UEpiDataConstants;

  function IsDate(Const Value: String; Ft: TFieldType): boolean;
//  function IsDate(var Value: String; Ft: TFieldType; aDateSeparators: TCharSet = DateSeparators): boolean;
  function EpiDateToDateTime(Const Str: String; Ft: TFieldType; Len: Integer): TDateTime;

implementation


// IsDate tries to guess the date construction

function IsDate(Const Value: String; Ft: TFieldType): boolean;
//function IsDate(var Value: String; Ft: TFieldType; aDateSeparators: TCharSet = DateSeparators): boolean;
var
  TmpS: string;
begin
  Result := true;
  TmpS := Value;
{
  IF Trim(TmpS) = '' THEN
    Exit;

  // Posible date constructions (displayed with "/" as date seperator:) from
  // string that we recognize:
  // 01/01/01  -


  IF pos('/',tmpS)<>0 THEN
    BEGIN   //first slash is found
      IF (Style=ftYMDDate) OR (Style=ftYMDToday) THEN  //&&
        BEGIN
          qq:=pos('/',tmpS);
          tmpS[qq]:='�';
          IF pos('/',tmpS)>0 THEN
            BEGIN
              //String has two slashes meaning year is included
              eYearStr:=Copy(tmpS,1,pos('�',tmpS)-1);
              Delete(tmpS,1,pos('�',tmpS));    //deletes year and separator
              eMonthStr:=copy(tmpS,1,pos('/',tmpS)-1);
              Delete(tmpS,1,pos('/',tmpS));   //deletes month and second separator
              eDayStr:=tmpS;
            END
          ELSE
            BEGIN
              //String has one slash meaning year is not included
              eYearStr:='';
              eMonthStr:=copy(tmpS,1,pos('�',tmpS)-1);
              Delete(tmpS,1,pos('�',tmpS));  //deletes month and separator
              eDayStr:=tmpS;
            END;
        END  //if ftYMDDate
      ELSE
        BEGIN
          eDayStr:=Copy(tmpS,1,pos('/',tmpS)-1);
          Delete(tmpS,1,pos('/',tmpS));
          IF pos('/',tmpS)<>0 THEN
            BEGIN  //second slash is found
              eMonthStr:=Copy(tmpS,1,pos('/',tmpS)-1);
              Delete(tmpS,1,pos('/',tmpS));
              eYearStr:=tmpS;
              IF trim(eYearStr)='' THEN eYearStr:='';
            END
          ELSE
            BEGIN
              eMonthStr:=tmpS;
              IF trim(eDayStr)='' THEN eDayStr:='';
              eYearStr:='';
            END;   //if there is a second slash
        END;  //if not YMDDate
    END   //if there is a first slash
  ELSE
    BEGIN   //the string contains no slash
      IF (Style=ftYMDDate) OR (Style=ftYMDToday) THEN  //&&
        BEGIN
          eMonthStr:='';
          eDayStr:='';
          eYearStr:='';
          CASE Length(tmpS) OF
            1,2: eDayStr:=trim(tmpS);
            4:   BEGIN
                   eMonthStr:=Copy(tmpS,1,2);
                   eDayStr:=Copy(tmpS,3,2);
                 END;
            6:   BEGIN
                   eYearStr:=Copy(tmpS,1,2);
                   eMonthStr:=Copy(tmpS,3,2);
                   eDayStr:=Copy(tmpS,5,2);
                 END;
            8:   BEGIN
                   eYearStr:=Copy(tmpS,1,4);
                   eMonthStr:=Copy(tmpS,5,2);
                   eDayStr:=Copy(tmpS,7,2);
                 END;
          ELSE
            result:=False;
          END;  //case
        END  //if ftYMDDate
      ELSE
        BEGIN
          While Length(tmpS)<8 DO tmpS:=tmpS+' ';
          eDayStr:=Copy(tmpS,1,2);
          eMonthStr:=Copy(tmpS,3,2);
          eYearStr:=Copy(tmpS,5,4);
        END;
    END;  //if string has no slash
  IF (trim(eMonthStr)<>'') AND (isInteger(eMonthStr))
    THEN Month:=StrToInt(trim(eMonthStr)) ELSE Result:=False;
  IF (trim(eDayStr)<>'') AND (IsInteger(eDayStr))
    THEN Day:=StrToInt(trim(eDayStr)) ELSE Result:=False;
  IF (trim(eYearStr)='') THEN
    BEGIN
      DecodeDate(Date,Year,m2,d2);
      eYearStr:=IntToStr(Year);
    END
  ELSE
    IF IsInteger(eYearStr)
      THEN Year:=StrToInt(trim(eYearStr))
    ELSE
      BEGIN
        Result:=False;
        Year:=0;
      END;
  IF (Style=ftDate) or (Style=ftToday) THEN
    BEGIN
      tmpDay:=Day;
      Day:=Month;
      Month:=tmpDay;
    END;
  IF (Year>=0)  AND (Year<50)  THEN Year:=Year+2000;
  IF (Year>=50) AND (Year<100) THEN Year:=Year+1900;
  IF (Month>12) OR  (Month<1)  THEN Result:=False
  ELSE
    BEGIN
      IF (Day<1) OR (Day>DaysInMonth[Month]) THEN Result:=False;
      IF (Result) AND (Day=29) AND (Month=2)
        THEN IF IsLeapYear(Year) THEN Result:=True ELSE Result:=False;
    END;
  
  IF Result THEN  //legal date entered
    BEGIN
      tmpDate:=EncodeDate(Year,Month,Day);
      s:=mibDateToStr(tmpDate,Style);
    END;     }
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