unit parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, parser_core, AST, epidatafiles,
  parser_types;

type

  { TParser }

  TParser = class(TObject)
  private
    FDataFile: TEpiDataFile;
    function InternalParse: boolean;
    procedure ParseError(Const Msg: string; Const LineNo, ColNo: integer; Const Text: string);
    function GetIdentType(Const VarName: string): TParserResultType;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(Const Line: String; out StatementList: TStatementList): boolean; overload;
    function Parse(Const Lines: TStrings; out StatementList: TStatementList): boolean; overload;
    property DataFile: TEpiDataFile read FDataFile write FDataFile;
  end;

implementation

uses
  LexLib, epidatafilestypes;
{ TParser }

function TParser.InternalParse: boolean;
begin
  Flush(yyinput);
  Reset(yyinput);
  result := yyparse = 0;
end;

procedure TParser.ParseError(const Msg: string; const LineNo, ColNo: integer;
  const Text: string);
begin
  if IsConsole then
    writeln('(', yylineno, ',', yycolno, '): ', msg, ' at or before ''', yytext, '''.')
end;

function TParser.GetIdentType(const VarName: string): TParserResultType;
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

constructor TParser.Create;
begin
  Assign(yyinput, GetTempFileName('', 'epidata_parser'));
  Rewrite(yyinput);

  OnParseError := @ParseError;
  OnGetIdentType := @GetIdentType;
end;

destructor TParser.Destroy;
begin
  Close(yyinput);
  Erase(yyinput);
  inherited Destroy;
end;

function TParser.Parse(const Line: String; out StatementList: TStatementList
  ): boolean;
begin
  WriteLn(yyinput, line);
  result := InternalParse;
  StatementList := StmList;
end;

function TParser.Parse(const Lines: TStrings; out StatementList: TStatementList
  ): boolean;
var
  i: Integer;
  F: TextFile;
begin
  for i := 0 to Lines.Count -1 do
    WriteLn(yyinput, Lines[i]);
  result := InternalParse;
  StatementList := StmList;
end;

end.

