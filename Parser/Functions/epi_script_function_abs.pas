unit epi_script_function_abs;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes;

type

  { TEpiScriptFunction_ABS }

  TEpiScriptFunction_ABS = class(TFunction)
  protected
    function TestParameters: Boolean; override;
  public
    constructor Create(const ParamList: TParamList); override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

implementation

{ TEpiScriptFunction_ABS }

function TEpiScriptFunction_ABS.TestParameters: Boolean;
begin
  Result := (FParamList.Count = 1);
end;

constructor TEpiScriptFunction_ABS.Create(const ParamList: TParamList);
begin
  inherited Create(ParamList);
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

