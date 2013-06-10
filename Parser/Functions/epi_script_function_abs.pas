unit epi_script_function_abs;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes,
    epi_parser_types;

type

  { TEpiScriptFunction_ABS }

  TEpiScriptFunction_ABS = class(TFunction)
  protected
    function MinParamCount: Integer; override;
    function MaxParamCount: Integer; override;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; override;
  public
    constructor Create(const ParamList: TParamList); override;
    function ResultType: TParserResultType; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

implementation

uses
  epi_script_function_resourcestrings;

{ TEpiScriptFunction_ABS }

function TEpiScriptFunction_ABS.MinParamCount: Integer;
begin
  Result := 1;
end;

function TEpiScriptFunction_ABS.MaxParamCount: Integer;
begin
  Result := 1;
end;

function TEpiScriptFunction_ABS.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  Result := [rtAny, rtInteger, rtFloat]
end;

constructor TEpiScriptFunction_ABS.Create(const ParamList: TParamList);
begin
  inherited Create(ParamList);
end;

function TEpiScriptFunction_ABS.ResultType: TParserResultType;
begin
  Result := inherited ResultType;
  if FParamList.Count > 0 then
    Result := Param[0].ResultType;
end;

function TEpiScriptFunction_ABS.AsInteger: EpiInteger;
begin
  Result := Abs(FParamList.Param[0].AsInteger);
end;

function TEpiScriptFunction_ABS.AsFloat: EpiFloat;
begin
  Result := Abs(FParamList.Param[0].AsFloat);
end;

function TEpiScriptFunction_ABS.AsString: EpiString;
begin
  Result := FloatToStr(AsFloat);
end;

end.

