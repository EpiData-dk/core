unit epi_script_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_AST, epidatafiles,
  epi_parser_types, epi_script_executor;

type

  { TEpiScriptParser }

  TEpiScriptParser = class(TObject)
  private
    FEpiExecutor: IEpiScriptParser;
    function InternalParse(Out ResultAST: TStatementList): boolean;
    procedure ResetFile;
  public
    constructor Create(Const EpiExecutor: IEpiScriptParser);
    destructor Destroy; override;
    function Parse(Const Line: String; out StatementList: TStatementList): boolean; overload;
    function Parse(Const Lines: TStrings; out StatementList: TStatementList): boolean; overload;
    property EpiExecutor: IEpiScriptParser read FEpiExecutor;
  end;

implementation

uses
  LexLib, epi_parser_core, epidatafilestypes;

{ TEpiScriptParser }

function TEpiScriptParser.InternalParse(out ResultAST: TStatementList): boolean;
begin
  Flush(yyinput);
  Reset(yyinput);
  result := yyparse(FEpiExecutor, ResultAST);
end;

constructor TEpiScriptParser.Create(const EpiExecutor: IEpiScriptParser);
begin
  Assign(yyinput, GetTempFileName('', 'epidata_parser'));
  FEpiExecutor := EpiExecutor;
end;

destructor TEpiScriptParser.Destroy;
begin
  if TextRec(yyinput).Mode <> fmClosed then
    Close(yyinput);
  Erase(yyinput);
  inherited Destroy;
end;

procedure TEpiScriptParser.ResetFile;
begin
  Rewrite(yyinput);
end;

function TEpiScriptParser.Parse(const Line: String; out StatementList: TStatementList
  ): boolean;
begin
  ResetFile;
  WriteLn(yyinput, line);
  result :=
    InternalParse(StatementList) and
    StatementList.TypeCheck(FEpiExecutor);
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

  result :=
    InternalParse(StatementList) and
    StatementList.TypeCheck(FEpiExecutor);
end;

end.

