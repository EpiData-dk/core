unit epi_script_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_parser_core, epi_script_ast, epidatafiles,
  epi_parser_types, typetable, epi_script_executor;

type

  { TEpiScriptParser }

  TEpiScriptParser = class(TObject)
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

{ TEpiScriptParser }

function TEpiScriptParser.InternalParse: boolean;
begin
  Flush(yyinput);
  Reset(yyinput);
  result := yyparse = 0;
end;

procedure TEpiScriptParser.ParseError(const Msg: string; const LineNo, ColNo: integer;
  const Text: string);
begin
  if IsConsole then
    writeln('(', yylineno, ',', yycolno, '): ', msg, ' at or before ''', yytext, '''.')
end;

function TEpiScriptParser.GetIdentType(const VarName: string): TParserResultType;
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

function TEpiScriptParser.GetSymbolTable: TTypeTable;
begin
  result := FSymbolTable;
end;

constructor TEpiScriptParser.Create;
begin
  Assign(yyinput, GetTempFileName('', 'epidata_parser'));
  Rewrite(yyinput);

  OnParseError := @ParseError;
  OnGetIdentType := @GetIdentType;
  OnGetSymbolTable := @GetSymbolTable;
end;

destructor TEpiScriptParser.Destroy;
begin
//  Close(yyinput);
//  Erase(yyinput);
  inherited Destroy;
end;

procedure TEpiScriptParser.ResetFile;
begin
  //Rewrite(yyinput);
end;

function TEpiScriptParser.Parse(const Line: String; out StatementList: TStatementList
  ): boolean;
begin
  ResetFile;
  WriteLn(yyinput, line);
  result := InternalParse;
  StatementList := StmList;
end;

function TEpiScriptParser.Parse(const Lines: TStrings; out StatementList: TStatementList
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

