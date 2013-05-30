unit epi_script_executor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_ast, epidatafiles, epi_parser_types,
  contnrs;

type

  { TEpiScriptExecutor }

  TEpiScriptExecutor = class(TObject, IEpiScriptParser)
  private
    FOnError: TExecutorError;
    FVariables: TFPObjectHashTable;
    FDataFile: TEpiDataFile;
    FOnGetRecordIndex: TExecutorGetRecordIndex;
    procedure SetDataFile(AValue: TEpiDataFile);
  protected
    procedure ProcessAssignment(Assignment: TAssignment);
    procedure ProcessCustomStatement(Stm: TCustomStatement);
    procedure ProcessDefine(Define: TDefine);
//    procedure ProcessGoto();
    procedure ProcessIfThenElse(IfThen: TIfThen);
//    procedure ProcessInfo();
    procedure ProcessStatementList(List: TStatementList);
  public
    constructor Create;
    destructor Destroy; override;
    function RunScript(Lines: TStrings): boolean;
    function ExecuteScript(StatementList: TStatementList): boolean;
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property OnGetRecordIndex: TExecutorGetRecordIndex read FOnGetRecordIndex write FOnGetRecordIndex;
    property OnError: TExecutorError read FOnError write FOnError;
  public
    { IEpiScriptParser }
    function VariableExists(const Ident: string): boolean;
    procedure AddVariable(const Variable: TCustomVariable);
    function FindVariable(const Ident: string): TCustomVariable;
    procedure ParseError(const Msg: string; const LineNo,
      ColNo: integer; const TextFound: string);
    function RecordIndex: Integer;
  end;

implementation

uses
  epi_script_parser;

{ TEpiScriptExecutor }

procedure TEpiScriptExecutor.SetDataFile(AValue: TEpiDataFile);
var
  F: TEpiField;
  FV: TFieldVariable;
  i: Integer;
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;

  // Assigned a TFieldVariable for all Fields;
  for i := 0 to FDataFile.Fields.Count - 1 do
  begin
    F := FDataFile.Field[i];
    AddVariable(TFieldVariable.Create(F, Self));
  end;
end;

procedure TEpiScriptExecutor.ProcessAssignment(Assignment: TAssignment);
begin
  with Assignment.Variable do
  case ResultType of
    rtBoolean: SetBoolean(Assignment.Expr.AsBoolean);
    rtInteger: SetInteger(Assignment.Expr.AsInteger);
    rtFloat:   SetFloat(Assignment.Expr.AsFloat);
  end;
end;

procedure TEpiScriptExecutor.ProcessDefine(Define: TDefine);
begin
  //  Do nothing - handling during parsing.
end;

procedure TEpiScriptExecutor.ProcessIfThenElse(IfThen: TIfThen);
begin
  if IfThen.Expr.asBoolean then
    ProcessCustomStatement(IfThen.ThenStatement)
  else
    ProcessCustomStatement(IfThen.ElseStatement);
end;

procedure TEpiScriptExecutor.ProcessStatementList(List: TStatementList);
begin
  while Assigned(List) do
  begin
    ProcessCustomStatement(List.Statement);
    List := List.StatementList;
  end;
end;

procedure TEpiScriptExecutor.ProcessCustomStatement(Stm: TCustomStatement);
begin
  if not Assigned(Stm) then exit;

  if Stm is TDefine then
    ProcessDefine(TDefine(Stm));

  if Stm is TAssignment then
    ProcessAssignment(TAssignment(Stm));

  if Stm is TIfThen then
    ProcessIfThenElse(TIfThen(Stm));

  if Stm is TStatementList then
    ProcessStatementList(TStatementList(Stm));
end;

constructor TEpiScriptExecutor.Create;
begin
  FVariables := TFPObjectHashTable.Create(False);
end;

destructor TEpiScriptExecutor.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TEpiScriptExecutor.RunScript(Lines: TStrings): boolean;
var
  Parser: TEpiScriptParser;
  Stm: TStatementList;
begin
  Parser := TEpiScriptParser.Create(Self);
  if Parser.Parse(Lines, Stm) then
    ExecuteScript(Stm);
  Parser.Free;
end;

function TEpiScriptExecutor.ExecuteScript(StatementList: TStatementList
  ): boolean;
var
  Stm: TCustomStatement;
begin
  Result := false;

  if not Assigned(StatementList) then exit;
  ProcessStatementList(StatementList);

  result := true;
end;

function TEpiScriptExecutor.VariableExists(const Ident: string): boolean;
begin
  result := Assigned(FindVariable(Ident));
end;

procedure TEpiScriptExecutor.AddVariable(const Variable: TCustomVariable);
begin
  FVariables.Add(Variable.Ident, Variable);
end;

function TEpiScriptExecutor.FindVariable(const Ident: string): TCustomVariable;
begin
  Result := TCustomVariable(FVariables.Items[Ident]);
end;

procedure TEpiScriptExecutor.ParseError(const Msg: string; const LineNo,
  ColNo: integer; const TextFound: string);
begin
  if Assigned(OnError) then
    FOnError(Msg, LineNo, ColNo, TextFound)
  else
    if IsConsole then
      writeln('(', LineNo, ',', ColNo, '): ', Msg);
end;

function TEpiScriptExecutor.RecordIndex: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    result := OnGetRecordIndex(Self)
  else
    result := -1;
end;

end.

