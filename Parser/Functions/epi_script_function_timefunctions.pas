unit epi_script_function_timefunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes, epi_parser_types;

type

  { TEpiScriptFunction_TimeFunctions }

  TEpiScriptFunction_TimeFunctions = class(TFunction)
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

{ TEpiScriptFunction_TimeFunctions }

function TEpiScriptFunction_TimeFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  case FOp of
    otFuncNow:    ;                      // Current time
    otFuncHour,                          // Hour part of a time
    otFuncMinut,                         // Minut part of a time
    otFuncSecond: result[0] := 1;        // Seconds part of a time
  end;
end;

function TEpiScriptFunction_TimeFunctions.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  case FOp of
    otFuncNow:    result := [];  // Current time
    otFuncHour,                 // Hour part of a time
    otFuncMinut,                // Minut part of a time
    otFuncSecond: result := [rtAny, rtInteger];  // Seconds part of a time
  end;
end;

constructor TEpiScriptFunction_TimeFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_TimeFunctions.ResultType: TParserResultType;
begin
  case FOp of
    otFuncNow:
      result := rtFloat;
    otFuncHour,
    otFuncMinut,
    otFuncSecond:
      result := rtInteger;
  end;
end;

function TEpiScriptFunction_TimeFunctions.AsInteger: EpiInteger;
begin
  Result := trunc(AsInteger);
end;

function TEpiScriptFunction_TimeFunctions.AsFloat: EpiFloat;
begin
  case FOp of
    otFuncNow:
      result := Now;
    otFuncHour:
      result := HourOf(Param[0].AsFloat);
    otFuncMinut:
      result := MinuteOf(Param[0].AsFloat);
    otFuncSecond:
      result := SecondOf(Param[0].AsFloat);
  end;
end;

function TEpiScriptFunction_TimeFunctions.AsString: EpiString;
begin
  case FOp of
    otFuncNow:
      Result := TimeToStr(AsFloat);
    otFuncHour,
    otFuncMinut,
    otFuncSecond:
      result := IntToStr(AsInteger);
  end;
end;

end.

