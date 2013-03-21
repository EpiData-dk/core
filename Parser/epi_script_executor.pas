unit epi_script_executor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_ast, epidatafiles, typetable, epi_parser_types;

type

  { TEpiScriptExecutor }

  TEpiScriptExecutor = class(TObject)
  private
    FDataFile: TEpiDataFile;
    FOnGetRecordIndex: TExecutorGetRecordIndex;
    FSymbolTable: TTypeTable;
    procedure SetDataFile(AValue: TEpiDataFile);
    procedure SetSymbolTable(AValue: TTypeTable);
  protected
    procedure ProcessAssignment(Assignment: TAssignment);
    procedure ProcessCustomStatement(Stm: TCustomStatement);
    procedure ProcessDefine(Define: TDefine);
    procedure ProcessIfThenElse(IfThen: TIfThen);
    procedure ProcessStatementList(List: TStatementList);
  public
    constructor Create;
    function ExecuteScript(StatementList: TStatementList): boolean;
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property SymbolTable: TTypeTable read FSymbolTable write SetSymbolTable;
    property OnGetRecordIndex: TExecutorGetRecordIndex read FOnGetRecordIndex write FOnGetRecordIndex;
  end;

implementation

{ TEpiScriptExecutor }

procedure TEpiScriptExecutor.SetDataFile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;
end;

procedure TEpiScriptExecutor.SetSymbolTable(AValue: TTypeTable);
begin
  if FSymbolTable = AValue then Exit;
  FSymbolTable := AValue;
end;

procedure TEpiScriptExecutor.ProcessAssignment(Assignment: TAssignment);
begin
//  Assignment.Variable;
end;

procedure TEpiScriptExecutor.ProcessDefine(Define: TDefine);
begin
  //  Define.;
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
  //
end;

function TEpiScriptExecutor.ExecuteScript(StatementList: TStatementList
  ): boolean;
var
  Stm: TCustomStatement;
begin
  Result := false;

  if not Assigned(FDataFile) then exit;
  if not Assigned(FSymbolTable) then exit;
  if not Assigned(StatementList) then exit;
  if not Assigned(FOnGetRecordIndex) then exit;

  ProcessStatementList(StatementList);
end;

end.

