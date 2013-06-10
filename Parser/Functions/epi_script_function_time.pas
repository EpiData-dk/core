unit epi_script_function_time;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes,
    epi_parser_types;

type

  { TEpiScriptFunction_Time }

  TEpiScriptFunction_Time = class(TFunction)
  protected
    function MinParamCount: Integer; override;
    function MaxParamCount: Integer; override;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; override;
  public
    function ResultType: TParserResultType; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

implementation

uses
  epidatafiles;

{ TEpiScriptFunction_Time }

function TEpiScriptFunction_Time.MinParamCount: Integer;
begin
  Result := 3;
end;

function TEpiScriptFunction_Time.MaxParamCount: Integer;
begin
  Result := 3;
end;

function TEpiScriptFunction_Time.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  Result := [rtAny, rtInteger];
end;

function TEpiScriptFunction_Time.ResultType: TParserResultType;
begin
  Result := rtFloat;
end;

function TEpiScriptFunction_Time.AsInteger: EpiInteger;
begin
  Result := trunc(AsFloat);
end;

function TEpiScriptFunction_Time.AsFloat: EpiFloat;
var
  Hour, Minut, Sec: TExpr;
  M, S: EpiInteger;
begin
  Hour := Param[0];
  Minut := Param[1];
  Sec   := Param[2];

  if Sec.IsMissing then
    S := 0
  else
    S := Sec.AsInteger;

  if Minut.IsMissing then
    M := 0
  else
    M := Minut.AsInteger;

  if Hour.IsMissing then
    result := inherited AsFloat
  else
    result := EncodeTime(Hour.AsInteger, M, S, 0);
end;

function TEpiScriptFunction_Time.AsString: EpiString;
begin
  Result := TimeToStr(AsFloat);
end;

end.

