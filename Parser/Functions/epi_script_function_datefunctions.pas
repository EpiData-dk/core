unit epi_script_function_datefunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes, epi_parser_types;

type

  { TEpiScriptFunction_DateFunctions }

  TEpiScriptFunction_DateFunctions = class(TFunction)
  private
    FOp: TParserOperationType;
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; override;
  public
    constructor Create(Const AOperation: TParserOperationType; const ParamList: TParamList);
    function ResultType: TParserResultType; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

implementation

uses
  epi_script_function_resourcestrings, dateutils;

{ TEpiScriptFunction_DateFunctions }

function TEpiScriptFunction_DateFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  case FOp of
    otFuncToday:       ;                      // Current date
    otFuncDay,                                // Day part of a date
    otFuncMonth,                              // Month part of a date
    otFuncYear,                               // Year part of a date
    otFuncDayOfWeek,                          // Day of the week, result is a number between 1-7
    otFuncWeek:        Result[0] := 1;        // Week number
  end;
end;

function TEpiScriptFunction_DateFunctions.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  case FOp of
    otFuncToday:     Result := [];                  // Current date
    otFuncDay,                                      // Day part of a date
    otFuncMonth,                                    // Month part of a date
    otFuncYear,                                     // Year part of a date
    otFuncDayOfWeek,                                // Day of the week, result is a number between 1-7
    otFuncWeek:      Result := [rtAny, rtInteger];  // Week number
  end;
end;

constructor TEpiScriptFunction_DateFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_DateFunctions.ResultType: TParserResultType;
begin
  result := rtInteger;
end;

function TEpiScriptFunction_DateFunctions.AsInteger: EpiInteger;
begin
  case FOp of
    otFuncToday:
      result := Trunc(Today);
    otFuncDay:
      result := DayOf(Param[0].AsInteger);
    otFuncMonth:
      result := MonthOf(Param[0].AsInteger);
    otFuncYear:
      result := YearOf(Param[0].AsInteger);
    otFuncDayOfWeek:
      result := DayOfWeek(Param[0].AsInteger);
    otFuncWeek:
      result := WeekOf(Param[0].AsInteger);
  end;
end;

function TEpiScriptFunction_DateFunctions.AsFloat: EpiFloat;
begin
  Result := AsInteger;
end;

function TEpiScriptFunction_DateFunctions.AsString: EpiString;
begin
  if FOp = otFuncToday then
    Result := DateToStr(AsInteger)
  else
    Result := IntToStr(AsInteger);
end;

end.

