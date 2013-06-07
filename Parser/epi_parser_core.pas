
unit epi_parser_core;

{$mode objfpc}{$H+}
{$GOTO ON}

interface

uses
  sysutils,
  yacclib,
  epi_parser_types,
  epi_script_parser,
  epi_script_AST;

function yyparse(Const EpiParser: IEpiScriptParser; Out AST: TStatementList): boolean;
procedure yyerror(msg : string);
procedure yy_set_start_state;

implementation

uses
  lexlib, epiconvertutils, epidatafilestypes;

var
  FParser: IEpiScriptParser;
  DummyStr: string;

const OPDefine = 257;
const OPInfo = 258;
const OPNote = 259;
const OPWarning = 260;
const OPGoto = 261;
const OPClear = 262;
const OPMissing = 263;
const OPBegin = 264;
const OPEnd = 265;
const OPTrue = 266;
const OPFalse = 267;
const OPOr = 268;
const OPAnd = 269;
const OPMod = 270;
const OPDiv = 271;
const OPMult = 272;
const OPPlus = 273;
const OPMinus = 274;
const OPDivide = 275;
const OPNot = 276;
const OPEQ = 277;
const OPNEQ = 278;
const OPLT = 279;
const OPLTE = 280;
const OPGT = 281;
const OPGTE = 282;
const OPIf = 283;
const OPThen = 284;
const OPElse = 285;
const OPStringType = 286;
const OPIntegerType = 287;
const OPFloatType = 288;
const OPBooleanType = 289;
const OPDateType = 290;
const OPOpenParan = 291;
const OPCloseParan = 292;
const OPSemicolon = 293;
const OPComma = 294;
const OPPeriod = 295;
const OPAssign = 296;
const OPColon = 297;
const OPFloatLiteral = 298;
const OPIntegerLiteral = 299;
const OPIdentifier = 300;
const OPWrite = 301;
const OPStringLiteral = 302;
const OPIllegal = 303;
const UMINUS = 304;


procedure yyerror(msg : string);
  begin
    if Assigned(FParser) then
      FParser.ParseError(Msg, yylineno, yycolno, yytext)
    else if IsConsole then
      writeln('(', yylineno, ',', yycolno, '): ', msg, ' at or before ''', yytext, '''.')
  end(*yyerror*);

var
  IdentList: array of IdString;

procedure ClearIdentList;
begin
  SetLength(IdentList, 0);
end;

procedure AddToIdentList(Ident: IdString);
var
  l: integer;
begin
  l := Length(IdentList);
  SetLength(IdentList, l+1);
  IdentList[l] := Ident;
end;


type YYSType = record case Integer of
                 1 : ( yyBoolean : Boolean );
                 2 : ( yyEpiFloat : EpiFloat );
                 3 : ( yyEpiInteger : EpiInteger );
                 4 : ( yyIdString : IdString );
                 5 : ( yyPString : PString );
                 6 : ( yyTCustomStatement : TCustomStatement );
                 7 : ( yyTCustomVariable : TCustomVariable );
                 8 : ( yyTExpr : TExpr );
                 9 : ( yyTGotoOption : TGotoOption );
                10 : ( yyTParserOperationType : TParserOperationType );
                11 : ( yyTParserResultType : TParserResultType );
                12 : ( yyTStatementList : TStatementList );
                13 : ( yyWord : Word );
               end(*YYSType*);

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse(Const EpiParser: IEpiScriptParser; Out AST: TStatementList): boolean;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         AST := yyv[yysp-0].yyTStatementList 
       end;
   2 : begin
         yyval.yyTStatementList := TStatementList.Create(yyv[yysp-2].yyTCustomStatement, yyv[yysp-0].yyTStatementList); 
       end;
   3 : begin
         yyval.yyTStatementList := nil; 
       end;
   4 : begin
         yyval.yyTCustomStatement := yyv[yysp-1].yyTStatementList; 
       end;
   5 : begin
         yyval.yyTCustomStatement := TIfThen.Create(yyv[yysp-3].yyTExpr, yyv[yysp-1].yyTCustomStatement, yyv[yysp-0].yyTCustomStatement); 
       end;
   6 : begin
         yyval.yyTCustomStatement := TAssignment.Create(yyv[yysp-2].yyTCustomVariable, yyv[yysp-0].yyTExpr); 
       end;
   7 : begin
         
         yyval.yyTCustomStatement := TDefine.Create(yyv[yysp-0].yyTParserResultType, IdentList, FParser);
         ClearIdentList;
         
       end;
   8 : begin
         yyval.yyTCustomStatement := TGoto.Create(yyv[yysp-1].yyTCustomVariable, yyv[yysp-0].yyTGotoOption); 
       end;
   9 : begin
         yyval.yyTCustomStatement := TWrite.Create(yyv[yysp-0].yyTExpr); 
       end;
  10 : begin
         yyval.yyTCustomStatement := yyv[yysp-0].yyTCustomStatement; 
       end;
  11 : begin
         yyval.yyTCustomStatement := nil; 
       end;
  12 : begin
         AddToIdentList(yyv[yysp-0].yyIdString); 
       end;
  13 : begin
         AddToIdentList(yyv[yysp-0].yyIdString); 
       end;
  14 : begin
         yyval.yyTParserResultType := rtInteger; 
       end;
  15 : begin
         yyval.yyTParserResultType := rtString; 
       end;
  16 : begin
         yyval.yyTParserResultType := rtFloat; 
       end;
  17 : begin
         yyval.yyTParserResultType := rtBoolean; 
       end;
  18 : begin
         yyval.yyTCustomVariable := yyv[yysp-0].yyTCustomVariable; 
       end;
  19 : begin
         yyval.yyTCustomVariable := nil; 
       end;
  20 : begin
         yyval.yyTGotoOption := goClear; 
       end;
  21 : begin
         yyval.yyTGotoOption := goMissing; 
       end;
  22 : begin
         yyval.yyTGotoOption := goNoOpt; 
       end;
  23 : begin
         yyval.yyTCustomVariable := TCustomVariable.FindVariable(yyv[yysp-0].yyIdString, FParser); 
       end;
  24 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  25 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otNEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  26 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  27 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  28 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  29 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  30 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otPlus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  31 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMinus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  32 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otOr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  33 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMult, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  34 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDivide, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  35 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDiv, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  36 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMod, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  37 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otAnd, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  38 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otNot,  yyv[yysp-0].yyTExpr, nil); 
       end;
  39 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otMinus, yyv[yysp-0].yyTExpr, nil); 
       end;
  40 : begin
         yyval := yyv[yysp-0];
       end;
  41 : begin
         yyval.yyTExpr := yyv[yysp-1].yyTExpr; 
       end;
  42 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil) 
       end;
  43 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  44 : begin
         yyval.yyTExpr := TIntegerLiteral.Create(yyv[yysp-0].yyEpiInteger);  
       end;
  45 : begin
         yyval.yyTExpr := TFloatLiteral.Create(yyv[yysp-0].yyEpiFloat);  
       end;
  46 : begin
         
         									  if Assigned(yyv[yysp-0].yyPString) then
         									    yyval.yyTExpr := TStringLiteral.Create(yyv[yysp-0].yyPString^)
         									  else
         									    yyval.yyTExpr := TStringLiteral.Create('');
         									  DisposeStr(yyv[yysp-0].yyPString);
         									
       end;
  47 : begin
         yyval.yyTExpr := TMissingLiteral.Create; 
       end;
  48 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(true); 
       end;
  49 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(false); 
       end;
  50 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  51 : begin
         yyval.yyTParserOperationType := otStringCast 
       end;
  52 : begin
         yyval.yyTParserOperationType := otFloatCast 
       end;
  53 : begin
         yyval.yyTParserOperationType := otBoolCast 
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 552;
yyngotos  = 104;
yynstates = 91;
yynrules  = 53;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 296; act: 11 ),
{ 2: }
  ( sym: 293; act: 12 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
{ 5: }
  ( sym: 300; act: 14 ),
{ 6: }
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 17 ),
{ 7: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
  ( sym: 265; act: -3 ),
{ 8: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 9: }
{ 10: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 11: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 12: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
  ( sym: 0; act: -3 ),
  ( sym: 265; act: -3 ),
{ 13: }
  ( sym: 294; act: 39 ),
  ( sym: 297; act: 40 ),
{ 14: }
{ 15: }
  ( sym: 262; act: 42 ),
  ( sym: 263; act: 43 ),
  ( sym: 285; act: -22 ),
  ( sym: 293; act: -22 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: 265; act: 44 ),
{ 19: }
  ( sym: 291; act: 45 ),
{ 20: }
{ 21: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 277; act: 54 ),
  ( sym: 278; act: 55 ),
  ( sym: 279; act: 56 ),
  ( sym: 280; act: 57 ),
  ( sym: 281; act: 58 ),
  ( sym: 282; act: 59 ),
  ( sym: 284; act: 60 ),
{ 22: }
{ 23: }
{ 24: }
{ 25: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 26: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 277; act: 54 ),
  ( sym: 278; act: 55 ),
  ( sym: 279; act: 56 ),
  ( sym: 280; act: 57 ),
  ( sym: 281; act: 58 ),
  ( sym: 282; act: 59 ),
  ( sym: 285; act: -9 ),
  ( sym: 293; act: -9 ),
{ 37: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 277; act: 54 ),
  ( sym: 278; act: 55 ),
  ( sym: 279; act: 56 ),
  ( sym: 280; act: 57 ),
  ( sym: 281; act: 58 ),
  ( sym: 282; act: 59 ),
  ( sym: 285; act: -6 ),
  ( sym: 293; act: -6 ),
{ 38: }
{ 39: }
  ( sym: 300; act: 64 ),
{ 40: }
  ( sym: 286; act: 66 ),
  ( sym: 287; act: 67 ),
  ( sym: 288; act: 68 ),
  ( sym: 289; act: 69 ),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 46: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 47: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 48: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 49: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 50: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 51: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 52: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 53: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 54: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 55: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 56: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 57: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 58: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 59: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 295; act: 32 ),
  ( sym: 298; act: 33 ),
  ( sym: 299; act: 34 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 35 ),
{ 60: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
{ 61: }
{ 62: }
{ 63: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 277; act: 54 ),
  ( sym: 278; act: 55 ),
  ( sym: 279; act: 56 ),
  ( sym: 280; act: 57 ),
  ( sym: 281; act: 58 ),
  ( sym: 282; act: 59 ),
  ( sym: 292; act: 86 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 277; act: 54 ),
  ( sym: 278; act: 55 ),
  ( sym: 279; act: 56 ),
  ( sym: 280; act: 57 ),
  ( sym: 281; act: 58 ),
  ( sym: 282; act: 59 ),
  ( sym: 292; act: 87 ),
{ 71: }
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 275; act: 53 ),
  ( sym: 268; act: -32 ),
  ( sym: 273; act: -32 ),
  ( sym: 274; act: -32 ),
  ( sym: 277; act: -32 ),
  ( sym: 278; act: -32 ),
  ( sym: 279; act: -32 ),
  ( sym: 280; act: -32 ),
  ( sym: 281; act: -32 ),
  ( sym: 282; act: -32 ),
  ( sym: 284; act: -32 ),
  ( sym: 285; act: -32 ),
  ( sym: 292; act: -32 ),
  ( sym: 293; act: -32 ),
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 275; act: 53 ),
  ( sym: 268; act: -30 ),
  ( sym: 273; act: -30 ),
  ( sym: 274; act: -30 ),
  ( sym: 277; act: -30 ),
  ( sym: 278; act: -30 ),
  ( sym: 279; act: -30 ),
  ( sym: 280; act: -30 ),
  ( sym: 281; act: -30 ),
  ( sym: 282; act: -30 ),
  ( sym: 284; act: -30 ),
  ( sym: 285; act: -30 ),
  ( sym: 292; act: -30 ),
  ( sym: 293; act: -30 ),
{ 77: }
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 275; act: 53 ),
  ( sym: 268; act: -31 ),
  ( sym: 273; act: -31 ),
  ( sym: 274; act: -31 ),
  ( sym: 277; act: -31 ),
  ( sym: 278; act: -31 ),
  ( sym: 279; act: -31 ),
  ( sym: 280; act: -31 ),
  ( sym: 281; act: -31 ),
  ( sym: 282; act: -31 ),
  ( sym: 284; act: -31 ),
  ( sym: 285; act: -31 ),
  ( sym: 292; act: -31 ),
  ( sym: 293; act: -31 ),
{ 78: }
{ 79: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 284; act: -24 ),
  ( sym: 285; act: -24 ),
  ( sym: 292; act: -24 ),
  ( sym: 293; act: -24 ),
{ 80: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 284; act: -25 ),
  ( sym: 285; act: -25 ),
  ( sym: 292; act: -25 ),
  ( sym: 293; act: -25 ),
{ 81: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 284; act: -26 ),
  ( sym: 285; act: -26 ),
  ( sym: 292; act: -26 ),
  ( sym: 293; act: -26 ),
{ 82: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 284; act: -28 ),
  ( sym: 285; act: -28 ),
  ( sym: 292; act: -28 ),
  ( sym: 293; act: -28 ),
{ 83: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 284; act: -27 ),
  ( sym: 285; act: -27 ),
  ( sym: 292; act: -27 ),
  ( sym: 293; act: -27 ),
{ 84: }
  ( sym: 268; act: 46 ),
  ( sym: 269; act: 47 ),
  ( sym: 270; act: 48 ),
  ( sym: 271; act: 49 ),
  ( sym: 272; act: 50 ),
  ( sym: 273; act: 51 ),
  ( sym: 274; act: 52 ),
  ( sym: 275; act: 53 ),
  ( sym: 284; act: -29 ),
  ( sym: 285; act: -29 ),
  ( sym: 292; act: -29 ),
  ( sym: 293; act: -29 ),
{ 85: }
  ( sym: 285; act: 89 ),
  ( sym: 293; act: -11 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 )
{ 90: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -3; act: 3 ),
  ( sym: -2; act: 4 ),
{ 1: }
{ 2: }
{ 3: }
{ 4: }
{ 5: }
  ( sym: -16; act: 13 ),
{ 6: }
  ( sym: -10; act: 15 ),
  ( sym: -9; act: 16 ),
{ 7: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 18 ),
{ 8: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 21 ),
  ( sym: -9; act: 22 ),
{ 9: }
{ 10: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 36 ),
  ( sym: -9; act: 22 ),
{ 11: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 37 ),
  ( sym: -9; act: 22 ),
{ 12: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 38 ),
{ 13: }
{ 14: }
{ 15: }
  ( sym: -15; act: 41 ),
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
{ 25: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 61 ),
  ( sym: -9; act: 22 ),
{ 26: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 62 ),
  ( sym: -9; act: 22 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 63 ),
  ( sym: -9; act: 22 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
  ( sym: -8; act: 65 ),
{ 41: }
{ 42: }
{ 43: }
{ 44: }
{ 45: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 70 ),
  ( sym: -9; act: 22 ),
{ 46: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 71 ),
  ( sym: -9; act: 22 ),
{ 47: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 72 ),
  ( sym: -9; act: 22 ),
{ 48: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 73 ),
  ( sym: -9; act: 22 ),
{ 49: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 74 ),
  ( sym: -9; act: 22 ),
{ 50: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 75 ),
  ( sym: -9; act: 22 ),
{ 51: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 76 ),
  ( sym: -9; act: 22 ),
{ 52: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 77 ),
  ( sym: -9; act: 22 ),
{ 53: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 78 ),
  ( sym: -9; act: 22 ),
{ 54: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 79 ),
  ( sym: -9; act: 22 ),
{ 55: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 80 ),
  ( sym: -9; act: 22 ),
{ 56: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 81 ),
  ( sym: -9; act: 22 ),
{ 57: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 82 ),
  ( sym: -9; act: 22 ),
{ 58: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 83 ),
  ( sym: -9; act: 22 ),
{ 59: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 84 ),
  ( sym: -9; act: 22 ),
{ 60: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 85 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: -5; act: 88 ),
{ 86: }
{ 87: }
{ 88: }
{ 89: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 90 )
{ 90: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } -1,
{ 5: } 0,
{ 6: } 0,
{ 7: } 0,
{ 8: } 0,
{ 9: } -23,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } 0,
{ 14: } -13,
{ 15: } 0,
{ 16: } -18,
{ 17: } -19,
{ 18: } 0,
{ 19: } 0,
{ 20: } -40,
{ 21: } 0,
{ 22: } -43,
{ 23: } -48,
{ 24: } -49,
{ 25: } 0,
{ 26: } 0,
{ 27: } -51,
{ 28: } -50,
{ 29: } -52,
{ 30: } -53,
{ 31: } 0,
{ 32: } -47,
{ 33: } -45,
{ 34: } -44,
{ 35: } -46,
{ 36: } 0,
{ 37: } 0,
{ 38: } -2,
{ 39: } 0,
{ 40: } 0,
{ 41: } -8,
{ 42: } -20,
{ 43: } -21,
{ 44: } -4,
{ 45: } 0,
{ 46: } 0,
{ 47: } 0,
{ 48: } 0,
{ 49: } 0,
{ 50: } 0,
{ 51: } 0,
{ 52: } 0,
{ 53: } 0,
{ 54: } 0,
{ 55: } 0,
{ 56: } 0,
{ 57: } 0,
{ 58: } 0,
{ 59: } 0,
{ 60: } 0,
{ 61: } -39,
{ 62: } -38,
{ 63: } 0,
{ 64: } -12,
{ 65: } -7,
{ 66: } -15,
{ 67: } -14,
{ 68: } -16,
{ 69: } -17,
{ 70: } 0,
{ 71: } 0,
{ 72: } -37,
{ 73: } -36,
{ 74: } -35,
{ 75: } -33,
{ 76: } 0,
{ 77: } 0,
{ 78: } -34,
{ 79: } 0,
{ 80: } 0,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } 0,
{ 85: } 0,
{ 86: } -41,
{ 87: } -42,
{ 88: } -5,
{ 89: } 0,
{ 90: } -10
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 8,
{ 2: } 9,
{ 3: } 10,
{ 4: } 11,
{ 5: } 11,
{ 6: } 12,
{ 7: } 14,
{ 8: } 21,
{ 9: } 35,
{ 10: } 35,
{ 11: } 49,
{ 12: } 63,
{ 13: } 71,
{ 14: } 73,
{ 15: } 73,
{ 16: } 77,
{ 17: } 77,
{ 18: } 77,
{ 19: } 78,
{ 20: } 79,
{ 21: } 79,
{ 22: } 94,
{ 23: } 94,
{ 24: } 94,
{ 25: } 94,
{ 26: } 108,
{ 27: } 122,
{ 28: } 122,
{ 29: } 122,
{ 30: } 122,
{ 31: } 122,
{ 32: } 136,
{ 33: } 136,
{ 34: } 136,
{ 35: } 136,
{ 36: } 136,
{ 37: } 152,
{ 38: } 168,
{ 39: } 168,
{ 40: } 169,
{ 41: } 173,
{ 42: } 173,
{ 43: } 173,
{ 44: } 173,
{ 45: } 173,
{ 46: } 187,
{ 47: } 201,
{ 48: } 215,
{ 49: } 229,
{ 50: } 243,
{ 51: } 257,
{ 52: } 271,
{ 53: } 285,
{ 54: } 299,
{ 55: } 313,
{ 56: } 327,
{ 57: } 341,
{ 58: } 355,
{ 59: } 369,
{ 60: } 383,
{ 61: } 389,
{ 62: } 389,
{ 63: } 389,
{ 64: } 404,
{ 65: } 404,
{ 66: } 404,
{ 67: } 404,
{ 68: } 404,
{ 69: } 404,
{ 70: } 404,
{ 71: } 419,
{ 72: } 437,
{ 73: } 437,
{ 74: } 437,
{ 75: } 437,
{ 76: } 437,
{ 77: } 455,
{ 78: } 473,
{ 79: } 473,
{ 80: } 485,
{ 81: } 497,
{ 82: } 509,
{ 83: } 521,
{ 84: } 533,
{ 85: } 545,
{ 86: } 547,
{ 87: } 547,
{ 88: } 547,
{ 89: } 547,
{ 90: } 553
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 7,
{ 1: } 8,
{ 2: } 9,
{ 3: } 10,
{ 4: } 10,
{ 5: } 11,
{ 6: } 13,
{ 7: } 20,
{ 8: } 34,
{ 9: } 34,
{ 10: } 48,
{ 11: } 62,
{ 12: } 70,
{ 13: } 72,
{ 14: } 72,
{ 15: } 76,
{ 16: } 76,
{ 17: } 76,
{ 18: } 77,
{ 19: } 78,
{ 20: } 78,
{ 21: } 93,
{ 22: } 93,
{ 23: } 93,
{ 24: } 93,
{ 25: } 107,
{ 26: } 121,
{ 27: } 121,
{ 28: } 121,
{ 29: } 121,
{ 30: } 121,
{ 31: } 135,
{ 32: } 135,
{ 33: } 135,
{ 34: } 135,
{ 35: } 135,
{ 36: } 151,
{ 37: } 167,
{ 38: } 167,
{ 39: } 168,
{ 40: } 172,
{ 41: } 172,
{ 42: } 172,
{ 43: } 172,
{ 44: } 172,
{ 45: } 186,
{ 46: } 200,
{ 47: } 214,
{ 48: } 228,
{ 49: } 242,
{ 50: } 256,
{ 51: } 270,
{ 52: } 284,
{ 53: } 298,
{ 54: } 312,
{ 55: } 326,
{ 56: } 340,
{ 57: } 354,
{ 58: } 368,
{ 59: } 382,
{ 60: } 388,
{ 61: } 388,
{ 62: } 388,
{ 63: } 403,
{ 64: } 403,
{ 65: } 403,
{ 66: } 403,
{ 67: } 403,
{ 68: } 403,
{ 69: } 403,
{ 70: } 418,
{ 71: } 436,
{ 72: } 436,
{ 73: } 436,
{ 74: } 436,
{ 75: } 436,
{ 76: } 454,
{ 77: } 472,
{ 78: } 472,
{ 79: } 484,
{ 80: } 496,
{ 81: } 508,
{ 82: } 520,
{ 83: } 532,
{ 84: } 544,
{ 85: } 546,
{ 86: } 546,
{ 87: } 546,
{ 88: } 546,
{ 89: } 552,
{ 90: } 552
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 5,
{ 2: } 5,
{ 3: } 5,
{ 4: } 5,
{ 5: } 5,
{ 6: } 6,
{ 7: } 8,
{ 8: } 11,
{ 9: } 15,
{ 10: } 15,
{ 11: } 19,
{ 12: } 23,
{ 13: } 26,
{ 14: } 26,
{ 15: } 26,
{ 16: } 27,
{ 17: } 27,
{ 18: } 27,
{ 19: } 27,
{ 20: } 27,
{ 21: } 27,
{ 22: } 27,
{ 23: } 27,
{ 24: } 27,
{ 25: } 27,
{ 26: } 31,
{ 27: } 35,
{ 28: } 35,
{ 29: } 35,
{ 30: } 35,
{ 31: } 35,
{ 32: } 39,
{ 33: } 39,
{ 34: } 39,
{ 35: } 39,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 39,
{ 40: } 39,
{ 41: } 40,
{ 42: } 40,
{ 43: } 40,
{ 44: } 40,
{ 45: } 40,
{ 46: } 44,
{ 47: } 48,
{ 48: } 52,
{ 49: } 56,
{ 50: } 60,
{ 51: } 64,
{ 52: } 68,
{ 53: } 72,
{ 54: } 76,
{ 55: } 80,
{ 56: } 84,
{ 57: } 88,
{ 58: } 92,
{ 59: } 96,
{ 60: } 100,
{ 61: } 102,
{ 62: } 102,
{ 63: } 102,
{ 64: } 102,
{ 65: } 102,
{ 66: } 102,
{ 67: } 102,
{ 68: } 102,
{ 69: } 102,
{ 70: } 102,
{ 71: } 102,
{ 72: } 102,
{ 73: } 102,
{ 74: } 102,
{ 75: } 102,
{ 76: } 102,
{ 77: } 102,
{ 78: } 102,
{ 79: } 102,
{ 80: } 102,
{ 81: } 102,
{ 82: } 102,
{ 83: } 102,
{ 84: } 102,
{ 85: } 102,
{ 86: } 103,
{ 87: } 103,
{ 88: } 103,
{ 89: } 103,
{ 90: } 105
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 4,
{ 1: } 4,
{ 2: } 4,
{ 3: } 4,
{ 4: } 4,
{ 5: } 5,
{ 6: } 7,
{ 7: } 10,
{ 8: } 14,
{ 9: } 14,
{ 10: } 18,
{ 11: } 22,
{ 12: } 25,
{ 13: } 25,
{ 14: } 25,
{ 15: } 26,
{ 16: } 26,
{ 17: } 26,
{ 18: } 26,
{ 19: } 26,
{ 20: } 26,
{ 21: } 26,
{ 22: } 26,
{ 23: } 26,
{ 24: } 26,
{ 25: } 30,
{ 26: } 34,
{ 27: } 34,
{ 28: } 34,
{ 29: } 34,
{ 30: } 34,
{ 31: } 38,
{ 32: } 38,
{ 33: } 38,
{ 34: } 38,
{ 35: } 38,
{ 36: } 38,
{ 37: } 38,
{ 38: } 38,
{ 39: } 38,
{ 40: } 39,
{ 41: } 39,
{ 42: } 39,
{ 43: } 39,
{ 44: } 39,
{ 45: } 43,
{ 46: } 47,
{ 47: } 51,
{ 48: } 55,
{ 49: } 59,
{ 50: } 63,
{ 51: } 67,
{ 52: } 71,
{ 53: } 75,
{ 54: } 79,
{ 55: } 83,
{ 56: } 87,
{ 57: } 91,
{ 58: } 95,
{ 59: } 99,
{ 60: } 101,
{ 61: } 101,
{ 62: } 101,
{ 63: } 101,
{ 64: } 101,
{ 65: } 101,
{ 66: } 101,
{ 67: } 101,
{ 68: } 101,
{ 69: } 101,
{ 70: } 101,
{ 71: } 101,
{ 72: } 101,
{ 73: } 101,
{ 74: } 101,
{ 75: } 101,
{ 76: } 101,
{ 77: } 101,
{ 78: } 101,
{ 79: } 101,
{ 80: } 101,
{ 81: } 101,
{ 82: } 101,
{ 83: } 101,
{ 84: } 101,
{ 85: } 102,
{ 86: } 102,
{ 87: } 102,
{ 88: } 102,
{ 89: } 104,
{ 90: } 104
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -3 ),
{ 2: } ( len: 3; sym: -2 ),
{ 3: } ( len: 0; sym: -2 ),
{ 4: } ( len: 3; sym: -4 ),
{ 5: } ( len: 5; sym: -4 ),
{ 6: } ( len: 3; sym: -4 ),
{ 7: } ( len: 4; sym: -4 ),
{ 8: } ( len: 3; sym: -4 ),
{ 9: } ( len: 2; sym: -4 ),
{ 10: } ( len: 2; sym: -5 ),
{ 11: } ( len: 0; sym: -5 ),
{ 12: } ( len: 3; sym: -16 ),
{ 13: } ( len: 1; sym: -16 ),
{ 14: } ( len: 1; sym: -8 ),
{ 15: } ( len: 1; sym: -8 ),
{ 16: } ( len: 1; sym: -8 ),
{ 17: } ( len: 1; sym: -8 ),
{ 18: } ( len: 1; sym: -10 ),
{ 19: } ( len: 1; sym: -10 ),
{ 20: } ( len: 1; sym: -15 ),
{ 21: } ( len: 1; sym: -15 ),
{ 22: } ( len: 0; sym: -15 ),
{ 23: } ( len: 1; sym: -9 ),
{ 24: } ( len: 3; sym: -12 ),
{ 25: } ( len: 3; sym: -12 ),
{ 26: } ( len: 3; sym: -12 ),
{ 27: } ( len: 3; sym: -12 ),
{ 28: } ( len: 3; sym: -12 ),
{ 29: } ( len: 3; sym: -12 ),
{ 30: } ( len: 3; sym: -12 ),
{ 31: } ( len: 3; sym: -12 ),
{ 32: } ( len: 3; sym: -12 ),
{ 33: } ( len: 3; sym: -12 ),
{ 34: } ( len: 3; sym: -12 ),
{ 35: } ( len: 3; sym: -12 ),
{ 36: } ( len: 3; sym: -12 ),
{ 37: } ( len: 3; sym: -12 ),
{ 38: } ( len: 2; sym: -12 ),
{ 39: } ( len: 2; sym: -12 ),
{ 40: } ( len: 1; sym: -12 ),
{ 41: } ( len: 3; sym: -13 ),
{ 42: } ( len: 4; sym: -13 ),
{ 43: } ( len: 1; sym: -13 ),
{ 44: } ( len: 1; sym: -13 ),
{ 45: } ( len: 1; sym: -13 ),
{ 46: } ( len: 1; sym: -13 ),
{ 47: } ( len: 1; sym: -13 ),
{ 48: } ( len: 1; sym: -13 ),
{ 49: } ( len: 1; sym: -13 ),
{ 50: } ( len: 1; sym: -14 ),
{ 51: } ( len: 1; sym: -14 ),
{ 52: } ( len: 1; sym: -14 ),
{ 53: } ( len: 1; sym: -14 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  FParser := EpiParser;

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := true; exit;

abort:

  yyparse := false; exit;

end(*yyparse*);


{$I epi_parser_core.inc}

procedure yy_set_start_state;
begin
  start(normal);
end;
end.