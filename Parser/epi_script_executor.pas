unit epi_script_executor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_ast, epidatafiles, epi_parser_types,
  contnrs, epidatafilestypes;

type

  { TEpiScriptExecutor }

  TEpiScriptExecutor = class(TObject, IEpiScriptParser, IEpiScriptExecutor)
  private
    FStopExecuting: boolean;
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
    procedure ProcessWrite(Write: TWrite); virtual;
  private
    FStatementList: TStatementList;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseScript(Lines: TStrings): boolean; virtual;
    function RunScript(Lines: TStrings): boolean; virtual;
    function ExecuteScript(StatementList: TStatementList = nil): boolean; virtual;
  public
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property OnGetRecordIndex: TExecutorGetRecordIndex read FOnGetRecordIndex write FOnGetRecordIndex;
    property OnError: TExecutorError read FOnError write FOnError;
  public
    { IEpiScriptParser }
    function VariableExists(const Ident: string): boolean;
    procedure AddVariable(const Variable: TCustomVariable);
    function FindVariable(const Ident: string): TCustomVariable;
    procedure ParseError(const Msg: string; const LineNo,
      ColNo: integer; const TextFound: string); virtual;
    function CreateFunction(const FunctionName: string;
       const ParamList: TParamList): TFunction;
  public
    { IEpiScriptExecutor }
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiBool); virtual; overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiInteger); virtual; overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiFloat); virtual; overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiString); virtual; overload;
    function GetFieldValueBool(Const Sender: TObject; Const F: TEpiField): EpiBool; virtual;
    function GetFieldValueInt(Const Sender: TObject; Const F: TEpiField): EpiInteger; virtual;
    function GetFieldValueFloat(Const Sender: TObject; Const F: TEpiField): EpiFloat; virtual;
    function GetFieldValueString(Const Sender: TObject; Const F: TEpiField): EpiString; virtual;
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
    rtString:  SetString(Assignment.Expr.AsString);
  end;
end;

procedure TEpiScriptExecutor.ProcessDefine(Define: TDefine);
begin
  //  Do nothing - handling during parsing.
end;

procedure TEpiScriptExecutor.ProcessGoto(AGoto: TGoto);
begin
  // Do nothing other than mark STOP execution.
  // Should be overridden in classes who has a GUI.
  FStopExecuting := true;
end;

procedure TEpiScriptExecutor.ProcessIfThenElse(IfThen: TIfThen);
begin
  if IfThen.Expr.AsTrueBoolean then
    ProcessCustomStatement(IfThen.ThenStatement)
  else
    ProcessCustomStatement(IfThen.ElseStatement);
end;

procedure TEpiScriptExecutor.ProcessStatementList(List: TStatementList);
begin
  while Assigned(List) and
        (not FStopExecuting)
  do
  begin
    ProcessCustomStatement(List.Statement);
    List := List.StatementList;
  end;
end;

procedure TEpiScriptExecutor.ProcessWrite(Write: TWrite);
begin
  if not Assigned(Write) then exit;
  if not Assigned(Write.Expr) then exit;
  if not IsConsole then exit;

  Writeln(Write.Expr.AsString);
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

  if Stm is TGoto then
    ProcessGoto(TGoto(Stm));

  if Stm is TWrite then
    ProcessWrite(TWrite(Stm));
end;

constructor TEpiScriptExecutor.Create;
begin
  FVariables := TFPObjectHashTable.Create(False);
  FVariables.OwnsObjects := false;
  FStopExecuting := false;
end;

destructor TEpiScriptExecutor.Destroy;
begin
  FVariables.Free;
  FStatementList.Free;
  inherited Destroy;
end;

function TEpiScriptExecutor.ParseScript(Lines: TStrings): boolean;
var
  Parser: TEpiScriptParser;
  Stm: TStatementList;
begin
  Parser := TEpiScriptParser.Create(Self);
  result := Parser.Parse(Lines, FStatementList);
  Parser.Free;
end;

function TEpiScriptExecutor.RunScript(Lines: TStrings): boolean;
begin
  result := ParseScript(Lines);
  if result then
    ExecuteScript(FStatementList);
end;

function TEpiScriptExecutor.ExecuteScript(StatementList: TStatementList
  ): boolean;
begin
  Result := false;
  FStopExecuting := false;

  if not Assigned(StatementList) then
    StatementList := FStatementList;

  if not Assigned(StatementList) then
    Exit;

  try
    ProcessStatementList(StatementList);
    result := true;
  except
    On E: Exception do
    begin
      ParseError(E.Message, ASTCurrentExecutionObject.LineNo, ASTCurrentExecutionObject.ColNo, ASTCurrentExecutionObject.Line);
      ASTCurrentExecutionObject := nil;
    end;
  end;
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

function TEpiScriptExecutor.CreateFunction(const FunctionName: string;
  const ParamList: TParamList): TFunction;
begin
  result := nil;
end;

procedure TEpiScriptExecutor.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiBool);
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  F.AsBoolean[Idx] := Value;
end;

procedure TEpiScriptExecutor.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiInteger);
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  F.AsInteger[Idx] := Value;
end;

procedure TEpiScriptExecutor.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiFloat);
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  F.AsFloat[Idx] := Value;
end;

procedure TEpiScriptExecutor.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiString);
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  F.AsString[Idx] := Value;
end;

function TEpiScriptExecutor.GetFieldValueBool(const Sender: TObject;
  const F: TEpiField): EpiBool;
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  Result := F.AsBoolean[Idx];
end;

function TEpiScriptExecutor.GetFieldValueInt(const Sender: TObject;
  const F: TEpiField): EpiInteger;
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  Result := F.AsInteger[Idx];
end;

function TEpiScriptExecutor.GetFieldValueFloat(const Sender: TObject;
  const F: TEpiField): EpiFloat;
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  Result := F.AsFloat[Idx];
end;

function TEpiScriptExecutor.GetFieldValueString(const Sender: TObject;
  const F: TEpiField): EpiString;
var
  Idx: Integer;
begin
  if Assigned(OnGetRecordIndex) then
    Idx := FOnGetRecordIndex(Self)
  else
    Exit;
  Result := F.AsString[Idx];
end;

end.

