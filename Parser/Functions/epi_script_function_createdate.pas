unit epi_script_function_createdate;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, epi_script_AST, epidatafilestypes,
    epi_parser_types;

type

  { TEpiScriptFunction_CreateDate }

  TEpiScriptFunction_CreateDate = class(TFunction)
  protected
    function ParamCounts: TBoundArray; override;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; override;
  public
    function ResultType: TParserResultType; override;
    function AsInteger: EpiInteger; override;
  end;

  EEpiScriptFunction_CreateDate = class(Exception);

implementation

uses
  epidatafiles, epiconvertutils;

{ TEpiScriptFunction_CreateDate }

function TEpiScriptFunction_CreateDate.ParamCounts: TBoundArray;
begin
  SetLength(Result, 3);
  Result[0] := 1;
  Result[1] := 2;
  Result[2] := 3;
end;

function TEpiScriptFunction_CreateDate.ParamAcceptType(ParamNo: Integer
  ): TParserResultTypes;
begin
  case FParamList.Count of
    1, 2:
      result := [rtString];
    3:
      result := [rtAny, rtInteger];
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
  S: String;
  Ft: TEpiFieldType;
  Msg: string;
  DateResult: EpiDate;
begin
  result := inherited;

  if FParamList.Count = 1 then
  begin
    if (not EpiStrToDateGuess(Param[0].AsString, DateResult, Msg)) then
      RuntimeError(EEpiScriptFunction_CreateDate, Msg);

    Result := DateResult;
    Exit;
  end;

  if FParamList.Count = 2 then
  begin
    S := LowerCase(Param[1].AsString);
    case S of
      'dmy': Ft := ftDMYDate;
      'mdy': Ft := ftMDYDate;
      'ymd': Ft := ftYMDDate;
    else
      RuntimeError(EEpiScriptFunction_CreateDate, 'Incorrect format specified: ' + Param[1].AsString);
    end;

    if (not EpiStrToDate(Param[0].AsString, '-', ft, DateResult, Msg)) then
      RuntimeError(EEpiScriptFunction_CreateDate, Msg);

    Result := DateResult;
    Exit;
  end;

  if FParamList.Count = 3 then
  begin
    Day   := Param[0];
    Month := Param[1];
    Year  := Param[2];

    if Day.IsMissing then
      D := 1
    else
      D := Day.AsInteger;

    if Month.IsMissing then
      M := 1
    else
      M := Month.AsInteger;

    if Year.IsMissing then
      Result := inherited AsInteger
    else
      Result := Trunc(EncodeDate(Year.AsInteger, M, D));

    Exit;
  end;
end;

end.

