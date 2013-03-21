unit epiparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, parser_core, AST, epidatafiles,
  parser_types, typetable, epi_scriptexecutor;

type

  { TEpiParser }

  TEpiParser = class(TObject)
  private
    FDataFile: TEpiDataFile;
    FExecutor: TEpiScriptExecutor;
    FSymbolTable: TTypeTable;
    function InternalParse: boolean;
    procedure ParseError(Const Msg: string; Const LineNo, ColNo: integer; Const Text: string);
    function GetIdentType(Const VarName: string): TParserResultType;
    function GetSymbolTable: TTypeTable;
    procedure ResetFile;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(Const Line: String; out StatementList: TStatementList): boolean; overload;
    function Parse(Const Lines: TStrings; out StatementList: TStatementList): boolean; overload;
    property DataFile: TEpiDataFile read FDataFile write FDataFile;
    property SymbolTable: TTypeTable read FSymbolTable write FSymbolTable;
    property Executor: TEpiScriptExecutor read FExecutor;
  end;

implementation

uses
  LexLib, epidatafilestypes;

{ TEpiParser }

function TEpiParser.InternalParse: boolean;
begin
  Flush(yyinput);
  Reset(yyinput);
  result := yyparse = 0;
end;

procedure TEpiParser.ParseError(const Msg: string; const LineNo, ColNo: integer;
  const Text: string);
begin
  if IsConsole then
    writeln('(', yylineno, ',', yycolno, '): ', msg, ' at or before ''', yytext, '''.')
end;

function TEpiParser.GetIdentType(const VarName: string): TParserResultType;
var
  F: TEpiField;
const
  FieldTypeToParserType: array[TEpiFieldType] of TParserResultType =
    (
//    ftBoolean,
      rtBoolean,

//    ftInteger, ftAutoInc, ftFloat,
      rtInteger, rtInteger, rtFloat,

//    ftDMYDate, ftMDYDate, ftYMDDate,
      rtInteger, rtInteger, rtInteger,

//    ftDMYAuto, ftMDYAuto, ftYMDAuto,
      rtInteger, rtInteger, rtInteger,

//    ftTime, ftTimeAuto,
      rtFloat, rtFloat,

//    ftString, ftUpperString
      rtString, rtString
    );

begin
  result := rtUndefined;

  if Assigned(DataFile) then
    F := DataFile.Fields.FieldByName[VarName];

  if Assigned(F) then
    result := FieldTypeToParserType[F.FieldType];
end;

function TEpiParser.GetSymbolTable: TTypeTable;
begin
  result := FSymbolTable;
end;

constructor TEpiParser.Create;
begin
  Assign(yyinput, GetTempFileName('', 'epidata_parser'));
  Rewrite(yyinput);

  OnParseError := @ParseError;
  OnGetIdentType := @GetIdentType;
  OnGetSymbolTable := @GetSymbolTable;
end;

destructor TEpiParser.Destroy;
begin
//  Close(yyinput);
//  Erase(yyinput);
  inherited Destroy;
end;

procedure TEpiParser.ResetFile;
begin
  //Rewrite(yyinput);
end;

function TEpiParser.Parse(const Line: String; out StatementList: TStatementList
  ): boolean;
begin
  ResetFile;
  WriteLn(yyinput, line);
  result := InternalParse;
  StatementList := StmList;
end;

function TEpiParser.Parse(const Lines: TStrings; out StatementList: TStatementList
  ): boolean;
var
  i: Integer;
  F: TextFile;
begin
  ResetFile;
  for i := 0 to Lines.Count -1 do
    WriteLn(yyinput, Lines[i]);
  result := InternalParse;
  StatementList := StmList;
end;

end.

