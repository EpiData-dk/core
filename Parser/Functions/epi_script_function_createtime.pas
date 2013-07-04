unit epi_script_function_createtime;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes,
    epi_parser_types;

type

  { TEpiScriptFunction_CreateTime }

  TEpiScriptFunction_CreateTime = class(TFunction)
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; override;
  public
    function ResultType: TParserResultType; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  EEpiScriptFunction_CreateTime = class(Exception);

implementation

uses
  epidatafiles, epiconvertutils;

{ TEpiScriptFunction_CreateTime }

function TEpiScriptFunction_CreateTime.ParamCounts: TBoundArray;
begin
  SetLength(Result, 2);
  Result[0] := 1;
  Result[1] := 3;
end;

function TEpiScriptFunction_CreateTime.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  if FParamList.Count = 1 then
    result := [rtString]
  else
    Result := [rtAny, rtInteger];
end;

function TEpiScriptFunction_CreateTime.ResultType: TParserResultType;
begin
  Result := rtFloat;
end;

function TEpiScriptFunction_CreateTime.AsInteger: EpiInteger;
begin
  Result := trunc(AsFloat);
end;

function TEpiScriptFunction_CreateTime.AsFloat: EpiFloat;
var
  Hour, Minut, Sec: TExpr;
  M, S: EpiInteger;
  ResultTime: EpiTime;
  Msg: string;
begin
  result := inherited;

  if FParamList.Count = 1 then
  begin
    if not EpiStrToTimeGues(Param[0].AsString, ResultTime, Msg) then
      RuntimeError(EEpiScriptFunction_CreateTime, Msg);

    Result := ResultTime;
    Exit;
  end;

  if FParamList.Count = 3 then
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

    Exit;
  end;
end;

function TEpiScriptFunction_CreateTime.AsString: EpiString;
begin
  Result := TimeToStr(AsFloat);
end;

end.

