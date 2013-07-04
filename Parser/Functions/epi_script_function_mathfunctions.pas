unit epi_script_function_mathfunctions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes,
    epi_parser_types;

type

  { TEpiScriptFunction_MathFunctions }

  TEpiScriptFunction_MathFunctions = class(TFunction)
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
    function IsMissing: Boolean; override;
  end;

implementation

uses
  math, epi_script_function_resourcestrings;

{ TEpiScriptFunction_MathFunctions }

function TEpiScriptFunction_MathFunctions.ParamCounts: TBoundArray;
begin
  Result := inherited ParamCounts;
  case FOp of
    otFuncAbs,
    otFuncExp,
    otFuncFraction,
    otFuncLn,
    otFuncLog,
    otFuncSqrt,
    otFuncRandom:
      Result[0] := 1;
    otFuncRound:
      Result[0] := 2;
  end;
end;

function TEpiScriptFunction_MathFunctions.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  case FOp of
    otFuncAbs,
    otFuncExp,
    otFuncFraction,
    otFuncLn,
    otFuncLog,
    otFuncSqrt:
      result := [rtAny, rtInteger, rtFloat];
    otFuncRandom:
      result := [rtAny, rtInteger];
    otFuncRound:
      if ParamNo = 0 then
        result := [rtAny, rtInteger, rtFloat]
      else
        result := [rtInteger];
  end;
end;

constructor TEpiScriptFunction_MathFunctions.Create(
  const AOperation: TParserOperationType; const ParamList: TParamList);
begin
  inherited Create(ParamList);
  FOp := AOperation;
end;

function TEpiScriptFunction_MathFunctions.ResultType: TParserResultType;
begin
  case FOp of
    otFuncAbs:
      Result := Param[0].ResultType;
    otFuncExp,
    otFuncFraction,
    otFuncLn,
    otFuncLog,
    otFuncSqrt:
      Result := rtFloat;
    otFuncRandom:
      result := rtInteger;
    otFuncRound:
      result := rtFloat;
  end;
end;

function TEpiScriptFunction_MathFunctions.AsInteger: EpiInteger;
begin
  result := inherited AsInteger;

  case FOp of
    otFuncAbs:
      Result := Abs(Param[0].AsInteger);
    otFuncRandom:
      result := Random(Param[0].AsInteger);
  end;
end;

function TEpiScriptFunction_MathFunctions.AsFloat: EpiFloat;
begin
  // Do not call inherited here, because we may end up calling Abs()
  // twice - one time in AsInteger and one time here. This is NOT allowed
  // if the calling of Param[0]... result in computational changes.
  // Hence we need to set ASTCurrentExecutionObject manually!
  ASTCurrentExecutionObject := self;

  case FOp of
    otFuncAbs:
      Result := Abs(Param[0].AsFloat);
    otFuncExp:
      Result := exp(Param[0].AsFloat);
    otFuncFraction:
      Result := frac(Param[0].AsFloat);
    otFuncLn:
      result := ln(Param[0].AsFloat);
    otFuncLog:
      result := log10(Param[0].AsFloat);
    otFuncSqrt:
      Result := sqrt(Param[0].AsFloat);
    otFuncRound:
      result := RoundTo(Param[0].AsFloat, -Param[1].AsInteger);
  else
    result := inherited AsFloat;
  end;
end;

function TEpiScriptFunction_MathFunctions.IsMissing: Boolean;
begin
  ASTCurrentExecutionObject := self;

  case FOp of
    otFuncAbs,
    otFuncExp,
    otFuncFraction,
    otFuncLn,
    otFuncLog,
    otFuncSqrt,
    otFuncRandom,
    otFuncRound:
      result := Param[0].IsMissing;
  end;
end;

end.

