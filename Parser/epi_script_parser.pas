unit epi_script_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_AST,
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
  LexLib, YaccLib, epi_parser_core;

{ TEpiScriptParser }

function TEpiScriptParser.InternalParse(out ResultAST: TStatementList): boolean;
var
  I: Word;
begin
  Flush(yyinput);
  Reset(yyinput);
  Rewrite(yyoutput);
  yy_set_start_state;
  result := yyparse(FEpiExecutor, ResultAST);
  yyclearin;
  yyclear;
  I := IOResult;
  if I <> 0 then
    halt;
end;

constructor TEpiScriptParser.Create(const EpiExecutor: IEpiScriptParser);
var
  I: Word;
begin
  Assign(yyinput, GetTempFileName('', 'epidata_parser'));
  I := IOResult;
  if I <> 0 then
    halt;
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
  result := InternalParse(StatementList);
  if Assigned(StatementList) then
    result := result and StatementList.TypeCheck(FEpiExecutor);
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

  result := InternalParse(StatementList);
  if Assigned(StatementList) then
    result := result and StatementList.TypeCheck(FEpiExecutor);
end;

end.

