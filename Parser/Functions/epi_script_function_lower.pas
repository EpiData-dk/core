unit epi_script_function_lower;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes, epi_parser_types;

type

  { TEpiScriptFunction_ABS }

  { TEpiScriptFunction_Lower }

  TEpiScriptFunction_Lower = class(TFunction)
  protected
    function MinParamCount: Integer; override;
    function MaxParamCount: Integer; override;
  public
    constructor Create(const ParamList: TParamList); override;
    function ResultType: TParserResultType; override;
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function AsString: EpiString; override;
  end;

implementation

uses
  LazUTF8;

{ TEpiScriptFunction_Lower }

function TEpiScriptFunction_Lower.MinParamCount: Integer;
begin
  Result := 1;
end;

function TEpiScriptFunction_Lower.MaxParamCount: Integer;
begin
  Result := 1;
end;

constructor TEpiScriptFunction_Lower.Create(const ParamList: TParamList);
begin
  inherited Create(ParamList);
end;

function TEpiScriptFunction_Lower.ResultType: TParserResultType;
begin
  Result := rtString;
end;

function TEpiScriptFunction_Lower.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);

  Result := Result and (Param[0].ResultType = rtString);
end;

function TEpiScriptFunction_Lower.AsString: EpiString;
begin
  Result := UTF8LowerCase(Param[0].AsString);
end;

end.

