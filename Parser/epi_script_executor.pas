unit epi_script_executor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_ast, epidatafiles, epi_parser_types,
  contnrs, epidatafilestypes;

type

  { TEpiScriptExecutor }

  TEpiScriptExecutor = class(TObject, IEpiScriptParser)
  private
    FOnError: TExecutorError;
    FOnSetFieldValue: TExecutorSetFieldValue;
    FVariables: TFPObjectHashTable;
    FDataFile: TEpiDataFile;
    FOnGetRecordIndex: TExecutorGetRecordIndex;
    procedure SetDataFile(AValue: TEpiDataFile);
  protected
    procedure ProcessAssignment(Assignment: TAssignment); virtual;
    procedure ProcessCustomStatement(Stm: TCustomStatement); virtual;
    procedure ProcessDefine(Define: TDefine); virtual;
    procedure ProcessGoto(AGoto: TGoto); virtual;
    procedure ProcessIfThenElse(IfThen: TIfThen); virtual;
//    procedure ProcessInfo();
    procedure ProcessStatementList(List: TStatementList); virtual;
  private
    FOnGetFieldValue: TExecutorGetFieldValue;
    FStatementList: TStatementList;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseScript(Lines: TStrings): boolean;
    function RunScript(Lines: TStrings): boolean;
    function ExecuteScript(StatementList: TStatementList = nil): boolean;
  public
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property OnGetRecordIndex: TExecutorGetRecordIndex read FOnGetRecordIndex write FOnGetRecordIndex;
    property OnError: TExecutorError read FOnError write FOnError;
    property OnSetFieldValue: TExecutorSetFieldValue read FOnSetFieldValue write FOnSetFieldValue;
    property OnGetFieldValue: TExecutorGetFieldValue read FOnGetFieldValue write FOnGetFieldValue;
  public
    { IEpiScriptParser }
    function VariableExists(const Ident: string): boolean;
    procedure AddVariable(const Variable: TCustomVariable);
    function FindVariable(const Ident: string): TCustomVariable;
    procedure ParseError(const Msg: string; const LineNo,
      ColNo: integer; const TextFound: string);
    function RecordIndex: Integer;
    procedure SetFieldInteger(Const F: TEpiField; Const Value: EpiInteger);
    procedure SetFieldFloat(Const F: TEpiField; Const Value: EpiFloat);
    procedure SetFieldBoolean(Const F: TEpiField; Const Value: Boolean);
    function  GetFieldInteger(Const F: TEpiField): EpiInteger;
    function  GetFieldFloat(Const F: TEpiField): EpiFloat;
    function  GetFieldBoolean(Const F: TEpiField): EpiBool;
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

procedure TEpiScriptExecutor.ProcessGoto(AGoto: TGoto);
begin
  // Do nothing - should be overridden in classes who has a GUI.
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

function TEpiScriptExecutor.ParseScript(Lines: TStrings): boolean;
var
  Parser: TEpiScriptParser;
  Stm: TStatementList;
begin
  Parser := TEpiScriptParser.Create(Self);
  if Parser.Parse(Lines, Stm) then
    FStatementList := Stm;
  Parser.Free;
end;

function TEpiScriptExecutor.RunScript(Lines: TStrings): boolean;
begin
  ParseScript(Lines);
  ExecuteScript(FStatementList);
end;

function TEpiScriptExecutor.ExecuteScript(StatementList: TStatementList
  ): boolean;
begin
  Result := false;

  if not Assigned(StatementList) then
    StatementList := FStatementList;

  if not Assigned(StatementList) then
    Exit;

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

procedure TEpiScriptExecutor.SetFieldInteger(const F: TEpiField;
  const Value: EpiInteger);
begin
  if Assigned(FOnSetFieldValue) then
    FOnSetFieldValue(Self, F, Value);
end;

procedure TEpiScriptExecutor.SetFieldFloat(const F: TEpiField;
  const Value: EpiFloat);
begin
  if Assigned(FOnSetFieldValue) then
    FOnSetFieldValue(Self, F, Value);
end;

procedure TEpiScriptExecutor.SetFieldBoolean(const F: TEpiField;
  const Value: Boolean);
begin
  if Assigned(FOnSetFieldValue) then
    FOnSetFieldValue(Self, F, Value);
end;

function TEpiScriptExecutor.GetFieldInteger(const F: TEpiField): EpiInteger;
begin
  if Assigned(FOnGetFieldValue) then
    Result := FOnGetFieldValue(Self, F);
end;

function TEpiScriptExecutor.GetFieldFloat(const F: TEpiField): EpiFloat;
begin
  if Assigned(FOnGetFieldValue) then
    Result := FOnGetFieldValue(Self, F);
end;

function TEpiScriptExecutor.GetFieldBoolean(const F: TEpiField): EpiBool;
begin
  if Assigned(FOnGetFieldValue) then
    Result := FOnGetFieldValue(Self, F);
end;

end.

