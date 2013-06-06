
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
         yyval.yyTExpr := TBooleanLiteral.Create(true); 
       end;
  48 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(false); 
       end;
  49 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  50 : begin
         yyval.yyTParserOperationType := otStringCast 
       end;
  51 : begin
         yyval.yyTParserOperationType := otFloatCast 
       end;
  52 : begin
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

yynacts   = 531;
yyngotos  = 104;
yynstates = 90;
yynrules  = 52;

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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 294; act: 38 ),
  ( sym: 297; act: 39 ),
{ 14: }
{ 15: }
  ( sym: 262; act: 41 ),
  ( sym: 263; act: 42 ),
  ( sym: 285; act: -22 ),
  ( sym: 293; act: -22 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: 265; act: 43 ),
{ 19: }
  ( sym: 291; act: 44 ),
{ 20: }
{ 21: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 277; act: 53 ),
  ( sym: 278; act: 54 ),
  ( sym: 279; act: 55 ),
  ( sym: 280; act: 56 ),
  ( sym: 281; act: 57 ),
  ( sym: 282; act: 58 ),
  ( sym: 284; act: 59 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 277; act: 53 ),
  ( sym: 278; act: 54 ),
  ( sym: 279; act: 55 ),
  ( sym: 280; act: 56 ),
  ( sym: 281; act: 57 ),
  ( sym: 282; act: 58 ),
  ( sym: 285; act: -9 ),
  ( sym: 293; act: -9 ),
{ 36: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 277; act: 53 ),
  ( sym: 278; act: 54 ),
  ( sym: 279; act: 55 ),
  ( sym: 280; act: 56 ),
  ( sym: 281; act: 57 ),
  ( sym: 282; act: 58 ),
  ( sym: 285; act: -6 ),
  ( sym: 293; act: -6 ),
{ 37: }
{ 38: }
  ( sym: 300; act: 63 ),
{ 39: }
  ( sym: 286; act: 65 ),
  ( sym: 287; act: 66 ),
  ( sym: 288; act: 67 ),
  ( sym: 289; act: 68 ),
{ 40: }
{ 41: }
{ 42: }
{ 43: }
{ 44: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
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
  ( sym: 298; act: 32 ),
  ( sym: 299; act: 33 ),
  ( sym: 300; act: 9 ),
  ( sym: 302; act: 34 ),
{ 59: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
{ 60: }
{ 61: }
{ 62: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 277; act: 53 ),
  ( sym: 278; act: 54 ),
  ( sym: 279; act: 55 ),
  ( sym: 280; act: 56 ),
  ( sym: 281; act: 57 ),
  ( sym: 282; act: 58 ),
  ( sym: 292; act: 85 ),
{ 63: }
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
{ 69: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 277; act: 53 ),
  ( sym: 278; act: 54 ),
  ( sym: 279; act: 55 ),
  ( sym: 280; act: 56 ),
  ( sym: 281; act: 57 ),
  ( sym: 282; act: 58 ),
  ( sym: 292; act: 86 ),
{ 70: }
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 275; act: 52 ),
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
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 275; act: 52 ),
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
{ 76: }
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 275; act: 52 ),
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
{ 77: }
{ 78: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 284; act: -24 ),
  ( sym: 285; act: -24 ),
  ( sym: 292; act: -24 ),
  ( sym: 293; act: -24 ),
{ 79: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 284; act: -25 ),
  ( sym: 285; act: -25 ),
  ( sym: 292; act: -25 ),
  ( sym: 293; act: -25 ),
{ 80: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 284; act: -26 ),
  ( sym: 285; act: -26 ),
  ( sym: 292; act: -26 ),
  ( sym: 293; act: -26 ),
{ 81: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 284; act: -28 ),
  ( sym: 285; act: -28 ),
  ( sym: 292; act: -28 ),
  ( sym: 293; act: -28 ),
{ 82: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 284; act: -27 ),
  ( sym: 285; act: -27 ),
  ( sym: 292; act: -27 ),
  ( sym: 293; act: -27 ),
{ 83: }
  ( sym: 268; act: 45 ),
  ( sym: 269; act: 46 ),
  ( sym: 270; act: 47 ),
  ( sym: 271; act: 48 ),
  ( sym: 272; act: 49 ),
  ( sym: 273; act: 50 ),
  ( sym: 274; act: 51 ),
  ( sym: 275; act: 52 ),
  ( sym: 284; act: -29 ),
  ( sym: 285; act: -29 ),
  ( sym: 292; act: -29 ),
  ( sym: 293; act: -29 ),
{ 84: }
  ( sym: 285; act: 88 ),
  ( sym: 293; act: -11 ),
{ 85: }
{ 86: }
{ 87: }
{ 88: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 )
{ 89: }
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
  ( sym: -12; act: 35 ),
  ( sym: -9; act: 22 ),
{ 11: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 36 ),
  ( sym: -9; act: 22 ),
{ 12: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 37 ),
{ 13: }
{ 14: }
{ 15: }
  ( sym: -15; act: 40 ),
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
  ( sym: -12; act: 60 ),
  ( sym: -9; act: 22 ),
{ 26: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 61 ),
  ( sym: -9; act: 22 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 62 ),
  ( sym: -9; act: 22 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
  ( sym: -8; act: 64 ),
{ 40: }
{ 41: }
{ 42: }
{ 43: }
{ 44: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 69 ),
  ( sym: -9; act: 22 ),
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
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 84 ),
{ 60: }
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
  ( sym: -5; act: 87 ),
{ 85: }
{ 86: }
{ 87: }
{ 88: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 89 )
{ 89: }
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
{ 23: } -47,
{ 24: } -48,
{ 25: } 0,
{ 26: } 0,
{ 27: } -50,
{ 28: } -49,
{ 29: } -51,
{ 30: } -52,
{ 31: } 0,
{ 32: } -45,
{ 33: } -44,
{ 34: } -46,
{ 35: } 0,
{ 36: } 0,
{ 37: } -2,
{ 38: } 0,
{ 39: } 0,
{ 40: } -8,
{ 41: } -20,
{ 42: } -21,
{ 43: } -4,
{ 44: } 0,
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
{ 60: } -39,
{ 61: } -38,
{ 62: } 0,
{ 63: } -12,
{ 64: } -7,
{ 65: } -15,
{ 66: } -14,
{ 67: } -16,
{ 68: } -17,
{ 69: } 0,
{ 70: } 0,
{ 71: } -37,
{ 72: } -36,
{ 73: } -35,
{ 74: } -33,
{ 75: } 0,
{ 76: } 0,
{ 77: } -34,
{ 78: } 0,
{ 79: } 0,
{ 80: } 0,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } 0,
{ 85: } -41,
{ 86: } -42,
{ 87: } -5,
{ 88: } 0,
{ 89: } -10
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
{ 9: } 34,
{ 10: } 34,
{ 11: } 47,
{ 12: } 60,
{ 13: } 68,
{ 14: } 70,
{ 15: } 70,
{ 16: } 74,
{ 17: } 74,
{ 18: } 74,
{ 19: } 75,
{ 20: } 76,
{ 21: } 76,
{ 22: } 91,
{ 23: } 91,
{ 24: } 91,
{ 25: } 91,
{ 26: } 104,
{ 27: } 117,
{ 28: } 117,
{ 29: } 117,
{ 30: } 117,
{ 31: } 117,
{ 32: } 130,
{ 33: } 130,
{ 34: } 130,
{ 35: } 130,
{ 36: } 146,
{ 37: } 162,
{ 38: } 162,
{ 39: } 163,
{ 40: } 167,
{ 41: } 167,
{ 42: } 167,
{ 43: } 167,
{ 44: } 167,
{ 45: } 180,
{ 46: } 193,
{ 47: } 206,
{ 48: } 219,
{ 49: } 232,
{ 50: } 245,
{ 51: } 258,
{ 52: } 271,
{ 53: } 284,
{ 54: } 297,
{ 55: } 310,
{ 56: } 323,
{ 57: } 336,
{ 58: } 349,
{ 59: } 362,
{ 60: } 368,
{ 61: } 368,
{ 62: } 368,
{ 63: } 383,
{ 64: } 383,
{ 65: } 383,
{ 66: } 383,
{ 67: } 383,
{ 68: } 383,
{ 69: } 383,
{ 70: } 398,
{ 71: } 416,
{ 72: } 416,
{ 73: } 416,
{ 74: } 416,
{ 75: } 416,
{ 76: } 434,
{ 77: } 452,
{ 78: } 452,
{ 79: } 464,
{ 80: } 476,
{ 81: } 488,
{ 82: } 500,
{ 83: } 512,
{ 84: } 524,
{ 85: } 526,
{ 86: } 526,
{ 87: } 526,
{ 88: } 526,
{ 89: } 532
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
{ 8: } 33,
{ 9: } 33,
{ 10: } 46,
{ 11: } 59,
{ 12: } 67,
{ 13: } 69,
{ 14: } 69,
{ 15: } 73,
{ 16: } 73,
{ 17: } 73,
{ 18: } 74,
{ 19: } 75,
{ 20: } 75,
{ 21: } 90,
{ 22: } 90,
{ 23: } 90,
{ 24: } 90,
{ 25: } 103,
{ 26: } 116,
{ 27: } 116,
{ 28: } 116,
{ 29: } 116,
{ 30: } 116,
{ 31: } 129,
{ 32: } 129,
{ 33: } 129,
{ 34: } 129,
{ 35: } 145,
{ 36: } 161,
{ 37: } 161,
{ 38: } 162,
{ 39: } 166,
{ 40: } 166,
{ 41: } 166,
{ 42: } 166,
{ 43: } 166,
{ 44: } 179,
{ 45: } 192,
{ 46: } 205,
{ 47: } 218,
{ 48: } 231,
{ 49: } 244,
{ 50: } 257,
{ 51: } 270,
{ 52: } 283,
{ 53: } 296,
{ 54: } 309,
{ 55: } 322,
{ 56: } 335,
{ 57: } 348,
{ 58: } 361,
{ 59: } 367,
{ 60: } 367,
{ 61: } 367,
{ 62: } 382,
{ 63: } 382,
{ 64: } 382,
{ 65: } 382,
{ 66: } 382,
{ 67: } 382,
{ 68: } 382,
{ 69: } 397,
{ 70: } 415,
{ 71: } 415,
{ 72: } 415,
{ 73: } 415,
{ 74: } 415,
{ 75: } 433,
{ 76: } 451,
{ 77: } 451,
{ 78: } 463,
{ 79: } 475,
{ 80: } 487,
{ 81: } 499,
{ 82: } 511,
{ 83: } 523,
{ 84: } 525,
{ 85: } 525,
{ 86: } 525,
{ 87: } 525,
{ 88: } 531,
{ 89: } 531
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
{ 40: } 40,
{ 41: } 40,
{ 42: } 40,
{ 43: } 40,
{ 44: } 40,
{ 45: } 44,
{ 46: } 48,
{ 47: } 52,
{ 48: } 56,
{ 49: } 60,
{ 50: } 64,
{ 51: } 68,
{ 52: } 72,
{ 53: } 76,
{ 54: } 80,
{ 55: } 84,
{ 56: } 88,
{ 57: } 92,
{ 58: } 96,
{ 59: } 100,
{ 60: } 102,
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
{ 85: } 103,
{ 86: } 103,
{ 87: } 103,
{ 88: } 103,
{ 89: } 105
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
{ 39: } 39,
{ 40: } 39,
{ 41: } 39,
{ 42: } 39,
{ 43: } 39,
{ 44: } 43,
{ 45: } 47,
{ 46: } 51,
{ 47: } 55,
{ 48: } 59,
{ 49: } 63,
{ 50: } 67,
{ 51: } 71,
{ 52: } 75,
{ 53: } 79,
{ 54: } 83,
{ 55: } 87,
{ 56: } 91,
{ 57: } 95,
{ 58: } 99,
{ 59: } 101,
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
{ 84: } 102,
{ 85: } 102,
{ 86: } 102,
{ 87: } 102,
{ 88: } 104,
{ 89: } 104
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
{ 49: } ( len: 1; sym: -14 ),
{ 50: } ( len: 1; sym: -14 ),
{ 51: } ( len: 1; sym: -14 ),
{ 52: } ( len: 1; sym: -14 )
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