unit parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, parser_core, AST;

type

  { TParser }

  TParser = class(TObject)
   public
     constructor Create;
     destructor Destroy; override;
     function Parse(Const Line: String; out StatementList: TStatementList): boolean; overload;
     function Parse(Const Lines: TStrings; out StatementList: TStatementList): boolean; overload;
  end;

implementation

uses
  LexLib;
{ TParser }

constructor TParser.Create;
begin
  Assign(yyinput, GetTempFileName('', 'epidata_parser'));
  Rewrite(yyinput);
end;

destructor TParser.Destroy;
begin
  Erase(yyinput);
  inherited Destroy;
end;

function TParser.Parse(const Line: String; out StatementList: TStatementList
  ): boolean;
begin
  WriteLn(yyinput, line);
  if yyparse = 0 then
    StmList := parser_core.StmList;
end;

function TParser.Parse(const Lines: TStrings; out StatementList: TStatementList
  ): boolean;
var
  i: Integer;
  F: TextFile;
begin
  for i := 0 to Lines.Count -1 do
    WriteLn(yyinput, Lines[i]);
  if yyparse = 0 then
    StatementList := StmList;
end;

end.

