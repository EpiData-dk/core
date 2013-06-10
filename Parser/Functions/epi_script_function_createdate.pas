unit epi_script_function_createdate;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes,
    epi_parser_types;

type

  { TEpiScriptFunction_CreateDate }

  TEpiScriptFunction_CreateDate = class(TFunction)
  private
    FDateType: TEpiFieldType;
  protected
    function MinParamCount: Integer; override;
    function MaxParamCount: Integer; override;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; override;
  public
    constructor Create(const ParamList: TParamList;
      Const DateType: string);
    function ResultType: TParserResultType; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

implementation

uses
  epidatafiles;

{ TEpiScriptFunction_CreateDate }

function TEpiScriptFunction_CreateDate.MinParamCount: Integer;
begin
  Result := 3;
end;

function TEpiScriptFunction_CreateDate.MaxParamCount: Integer;
begin
  Result := 3;
end;

function TEpiScriptFunction_CreateDate.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  result := [rtAny, rtInteger];
end;

constructor TEpiScriptFunction_CreateDate.Create(const ParamList: TParamList;
  const DateType: string);
begin
  inherited Create(ParamList);
  case DateType of
    'dmy': FDateType := ftDMYDate;
    'mdy': FDateType := ftMDYDate;
    'ymd': FDateType := ftYMDDate;
  end;
end;

function TEpiScriptFunction_CreateDate.ResultType: TParserResultType;
begin
  Result := rtInteger;
end;

function TEpiScriptFunction_CreateDate.AsInteger: EpiInteger;
var
  Day, Month, Year: TExpr;
  D, M: Integer;
begin
  case FDateType of
    ftDMYDate:
      begin
        Day   := Param[0];
        Month := Param[1];
        Year  := Param[2];
      end;
    ftMDYDate:
      begin
        Month := Param[0];
        Day   := Param[1];
        Year  := Param[2];
      end;
    ftYMDDate:
      begin
        Year  := Param[0];
        Month := Param[1];
        Day   := Param[2];
      end;
  end;
  if Day.IsMissing then
    D := 1
  else
    D := Day.AsInteger;

  if Month.IsMissing then
    M := 1
  else
    M := Month.AsInteger;

  if Day.IsMissing then
    D := 1
  else
    D := Day.AsInteger;

  if Year.IsMissing then
    Result := inherited AsInteger
  else
    Result := Trunc(EncodeDate(Year.AsInteger, M, D));
end;

function TEpiScriptFunction_CreateDate.AsFloat: EpiFloat;
begin
  Result := AsInteger;
end;

function TEpiScriptFunction_CreateDate.AsString: EpiString;
begin
  Result := inherited AsString;
end;

end.

