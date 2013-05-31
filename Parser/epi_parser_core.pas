
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
  lexlib;

var
  FParser: IEpiScriptParser;

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
const OPString = 286;
const OPInteger = 287;
const OPFloat = 288;
const OPBoolean = 289;
const OPOpenParan = 290;
const OPCloseParan = 291;
const OPSemicolon = 292;
const OPComma = 293;
const OPPeriod = 294;
const OPAssign = 295;
const OPColon = 296;
const OPNumber = 297;
const OPHexNumber = 298;
const OPStringText = 299;
const OPIdentifier = 300;
const OPWrite = 301;
const OPIllegal = 302;
const UMINUS = 303;


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
                 2 : ( yyExtended : Extended );
                 3 : ( yyIdString : IdString );
                 4 : ( yyInteger : Integer );
                 5 : ( yyTCustomStatement : TCustomStatement );
                 6 : ( yyTCustomVariable : TCustomVariable );
                 7 : ( yyTExpr : TExpr );
                 8 : ( yyTGotoOption : TGotoOption );
                 9 : ( yyTParserOperationType : TParserOperationType );
                10 : ( yyTParserResultType : TParserResultType );
                11 : ( yyTStatementList : TStatementList );
                12 : ( yyWord : Word );
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
         yyval.yyTCustomStatement := yyv[yysp-0].yyTCustomStatement; 
       end;
  10 : begin
         yyval.yyTCustomStatement := nil; 
       end;
  11 : begin
         AddToIdentList(yyv[yysp-0].yyIdString); 
       end;
  12 : begin
         AddToIdentList(yyv[yysp-0].yyIdString); 
       end;
  13 : begin
         yyval.yyTParserResultType := rtInteger; 
       end;
  14 : begin
         yyval.yyTParserResultType := rtFloat; 
       end;
  15 : begin
         yyval.yyTParserResultType := rtBoolean; 
       end;
  16 : begin
         yyval.yyTCustomVariable := yyv[yysp-0].yyTCustomVariable; 
       end;
  17 : begin
         yyval.yyTCustomVariable := nil; 
       end;
  18 : begin
         yyval.yyTGotoOption := goClear; 
       end;
  19 : begin
         yyval.yyTGotoOption := goMissing; 
       end;
  20 : begin
         yyval.yyTGotoOption := goNoOpt; 
       end;
  21 : begin
         yyval.yyTCustomVariable := TCustomVariable.FindVariable(yyv[yysp-0].yyIdString, FParser); 
       end;
  22 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  23 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otNEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  24 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  25 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  26 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  27 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  28 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otPlus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  29 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMinus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  30 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otOr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  31 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMult, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  32 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDivide, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  33 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDiv, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  34 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMod, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  35 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otAnd, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  36 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otNot,  yyv[yysp-0].yyTExpr, nil); 
       end;
  37 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otMinus, yyv[yysp-0].yyTExpr, nil); 
       end;
  38 : begin
         yyval := yyv[yysp-0];
       end;
  39 : begin
         yyval.yyTExpr := yyv[yysp-1].yyTExpr; 
       end;
  40 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil) 
       end;
  41 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  42 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  43 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  44 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyExtended);  
       end;
  45 : begin
         yyval.yyTExpr := TLiteral.Create(true); 
       end;
  46 : begin
         yyval.yyTExpr := TLiteral.Create(false); 
       end;
  47 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  48 : begin
         yyval.yyTParserOperationType := otFloatCast 
       end;
  49 : begin
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

yynacts   = 475;
yyngotos  = 100;
yynstates = 85;
yynrules  = 49;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 295; act: 10 ),
{ 2: }
  ( sym: 292; act: 11 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
{ 5: }
  ( sym: 300; act: 13 ),
{ 6: }
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 16 ),
{ 7: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 265; act: -3 ),
{ 8: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 9: }
{ 10: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 11: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 0; act: -3 ),
  ( sym: 265; act: -3 ),
{ 12: }
  ( sym: 293; act: 34 ),
  ( sym: 296; act: 35 ),
{ 13: }
{ 14: }
  ( sym: 262; act: 37 ),
  ( sym: 263; act: 38 ),
  ( sym: 285; act: -20 ),
  ( sym: 292; act: -20 ),
{ 15: }
{ 16: }
{ 17: }
  ( sym: 265; act: 39 ),
{ 18: }
  ( sym: 290; act: 40 ),
{ 19: }
{ 20: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 277; act: 49 ),
  ( sym: 278; act: 50 ),
  ( sym: 279; act: 51 ),
  ( sym: 280; act: 52 ),
  ( sym: 281; act: 53 ),
  ( sym: 282; act: 54 ),
  ( sym: 284; act: 55 ),
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 25: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 26: }
{ 27: }
  ( sym: 268; act: -44 ),
  ( sym: 269; act: -44 ),
  ( sym: 270; act: -44 ),
  ( sym: 271; act: -44 ),
  ( sym: 272; act: -44 ),
  ( sym: 273; act: -44 ),
  ( sym: 274; act: -44 ),
  ( sym: 275; act: -44 ),
  ( sym: 277; act: -44 ),
  ( sym: 278; act: -44 ),
  ( sym: 279; act: -44 ),
  ( sym: 280; act: -44 ),
  ( sym: 281; act: -44 ),
  ( sym: 282; act: -44 ),
  ( sym: 284; act: -44 ),
  ( sym: 285; act: -44 ),
  ( sym: 291; act: -44 ),
  ( sym: 292; act: -44 ),
  ( sym: 290; act: -48 ),
{ 28: }
{ 29: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 30: }
{ 31: }
{ 32: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 277; act: 49 ),
  ( sym: 278; act: 50 ),
  ( sym: 279; act: 51 ),
  ( sym: 280; act: 52 ),
  ( sym: 281; act: 53 ),
  ( sym: 282; act: 54 ),
  ( sym: 285; act: -6 ),
  ( sym: 292; act: -6 ),
{ 33: }
{ 34: }
  ( sym: 300; act: 59 ),
{ 35: }
  ( sym: 287; act: 61 ),
  ( sym: 288; act: 62 ),
  ( sym: 289; act: 63 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 41: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 42: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 43: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 44: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 45: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 46: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 47: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 48: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 49: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 50: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 51: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 52: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 53: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 54: }
  ( sym: 266; act: 22 ),
  ( sym: 267; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 276; act: 25 ),
  ( sym: 287; act: 26 ),
  ( sym: 288; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 300; act: 9 ),
{ 55: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
{ 56: }
{ 57: }
{ 58: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 277; act: 49 ),
  ( sym: 278; act: 50 ),
  ( sym: 279; act: 51 ),
  ( sym: 280; act: 52 ),
  ( sym: 281; act: 53 ),
  ( sym: 282; act: 54 ),
  ( sym: 291; act: 80 ),
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 277; act: 49 ),
  ( sym: 278; act: 50 ),
  ( sym: 279; act: 51 ),
  ( sym: 280; act: 52 ),
  ( sym: 281; act: 53 ),
  ( sym: 282; act: 54 ),
  ( sym: 291; act: 81 ),
{ 65: }
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 275; act: 48 ),
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
  ( sym: 291; act: -30 ),
  ( sym: 292; act: -30 ),
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 275; act: 48 ),
  ( sym: 268; act: -28 ),
  ( sym: 273; act: -28 ),
  ( sym: 274; act: -28 ),
  ( sym: 277; act: -28 ),
  ( sym: 278; act: -28 ),
  ( sym: 279; act: -28 ),
  ( sym: 280; act: -28 ),
  ( sym: 281; act: -28 ),
  ( sym: 282; act: -28 ),
  ( sym: 284; act: -28 ),
  ( sym: 285; act: -28 ),
  ( sym: 291; act: -28 ),
  ( sym: 292; act: -28 ),
{ 71: }
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 275; act: 48 ),
  ( sym: 268; act: -29 ),
  ( sym: 273; act: -29 ),
  ( sym: 274; act: -29 ),
  ( sym: 277; act: -29 ),
  ( sym: 278; act: -29 ),
  ( sym: 279; act: -29 ),
  ( sym: 280; act: -29 ),
  ( sym: 281; act: -29 ),
  ( sym: 282; act: -29 ),
  ( sym: 284; act: -29 ),
  ( sym: 285; act: -29 ),
  ( sym: 291; act: -29 ),
  ( sym: 292; act: -29 ),
{ 72: }
{ 73: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 284; act: -22 ),
  ( sym: 285; act: -22 ),
  ( sym: 291; act: -22 ),
  ( sym: 292; act: -22 ),
{ 74: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 284; act: -23 ),
  ( sym: 285; act: -23 ),
  ( sym: 291; act: -23 ),
  ( sym: 292; act: -23 ),
{ 75: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 284; act: -24 ),
  ( sym: 285; act: -24 ),
  ( sym: 291; act: -24 ),
  ( sym: 292; act: -24 ),
{ 76: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 284; act: -26 ),
  ( sym: 285; act: -26 ),
  ( sym: 291; act: -26 ),
  ( sym: 292; act: -26 ),
{ 77: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 284; act: -25 ),
  ( sym: 285; act: -25 ),
  ( sym: 291; act: -25 ),
  ( sym: 292; act: -25 ),
{ 78: }
  ( sym: 268; act: 41 ),
  ( sym: 269; act: 42 ),
  ( sym: 270; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 284; act: -27 ),
  ( sym: 285; act: -27 ),
  ( sym: 291; act: -27 ),
  ( sym: 292; act: -27 ),
{ 79: }
  ( sym: 285; act: 83 ),
  ( sym: 292; act: -10 ),
{ 80: }
{ 81: }
{ 82: }
{ 83: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 )
{ 84: }
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
  ( sym: -16; act: 12 ),
{ 6: }
  ( sym: -10; act: 14 ),
  ( sym: -9; act: 15 ),
{ 7: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 17 ),
{ 8: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 20 ),
  ( sym: -9; act: 21 ),
{ 9: }
{ 10: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 32 ),
  ( sym: -9; act: 21 ),
{ 11: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 33 ),
{ 12: }
{ 13: }
{ 14: }
  ( sym: -15; act: 36 ),
{ 15: }
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 56 ),
  ( sym: -9; act: 21 ),
{ 25: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 57 ),
  ( sym: -9; act: 21 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 58 ),
  ( sym: -9; act: 21 ),
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -8; act: 60 ),
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 64 ),
  ( sym: -9; act: 21 ),
{ 41: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 65 ),
  ( sym: -9; act: 21 ),
{ 42: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 66 ),
  ( sym: -9; act: 21 ),
{ 43: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 67 ),
  ( sym: -9; act: 21 ),
{ 44: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 68 ),
  ( sym: -9; act: 21 ),
{ 45: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 69 ),
  ( sym: -9; act: 21 ),
{ 46: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 70 ),
  ( sym: -9; act: 21 ),
{ 47: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 71 ),
  ( sym: -9; act: 21 ),
{ 48: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 72 ),
  ( sym: -9; act: 21 ),
{ 49: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 73 ),
  ( sym: -9; act: 21 ),
{ 50: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 74 ),
  ( sym: -9; act: 21 ),
{ 51: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 75 ),
  ( sym: -9; act: 21 ),
{ 52: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 76 ),
  ( sym: -9; act: 21 ),
{ 53: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 77 ),
  ( sym: -9; act: 21 ),
{ 54: }
  ( sym: -14; act: 18 ),
  ( sym: -13; act: 19 ),
  ( sym: -12; act: 78 ),
  ( sym: -9; act: 21 ),
{ 55: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 79 ),
{ 56: }
{ 57: }
{ 58: }
{ 59: }
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
  ( sym: -5; act: 82 ),
{ 80: }
{ 81: }
{ 82: }
{ 83: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 84 )
{ 84: }
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
{ 9: } -21,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } -12,
{ 14: } 0,
{ 15: } -16,
{ 16: } -17,
{ 17: } 0,
{ 18: } 0,
{ 19: } -38,
{ 20: } 0,
{ 21: } -41,
{ 22: } -45,
{ 23: } -46,
{ 24: } 0,
{ 25: } 0,
{ 26: } -47,
{ 27: } 0,
{ 28: } -49,
{ 29: } 0,
{ 30: } -42,
{ 31: } -43,
{ 32: } 0,
{ 33: } -2,
{ 34: } 0,
{ 35: } 0,
{ 36: } -8,
{ 37: } -18,
{ 38: } -19,
{ 39: } -4,
{ 40: } 0,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
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
{ 56: } -37,
{ 57: } -36,
{ 58: } 0,
{ 59: } -11,
{ 60: } -7,
{ 61: } -13,
{ 62: } -14,
{ 63: } -15,
{ 64: } 0,
{ 65: } 0,
{ 66: } -35,
{ 67: } -34,
{ 68: } -33,
{ 69: } -31,
{ 70: } 0,
{ 71: } 0,
{ 72: } -32,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } -39,
{ 81: } -40,
{ 82: } -5,
{ 83: } 0,
{ 84: } -9
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 7,
{ 2: } 8,
{ 3: } 9,
{ 4: } 10,
{ 5: } 10,
{ 6: } 11,
{ 7: } 13,
{ 8: } 19,
{ 9: } 30,
{ 10: } 30,
{ 11: } 41,
{ 12: } 48,
{ 13: } 50,
{ 14: } 50,
{ 15: } 54,
{ 16: } 54,
{ 17: } 54,
{ 18: } 55,
{ 19: } 56,
{ 20: } 56,
{ 21: } 71,
{ 22: } 71,
{ 23: } 71,
{ 24: } 71,
{ 25: } 82,
{ 26: } 93,
{ 27: } 93,
{ 28: } 112,
{ 29: } 112,
{ 30: } 123,
{ 31: } 123,
{ 32: } 123,
{ 33: } 139,
{ 34: } 139,
{ 35: } 140,
{ 36: } 143,
{ 37: } 143,
{ 38: } 143,
{ 39: } 143,
{ 40: } 143,
{ 41: } 154,
{ 42: } 165,
{ 43: } 176,
{ 44: } 187,
{ 45: } 198,
{ 46: } 209,
{ 47: } 220,
{ 48: } 231,
{ 49: } 242,
{ 50: } 253,
{ 51: } 264,
{ 52: } 275,
{ 53: } 286,
{ 54: } 297,
{ 55: } 308,
{ 56: } 313,
{ 57: } 313,
{ 58: } 313,
{ 59: } 328,
{ 60: } 328,
{ 61: } 328,
{ 62: } 328,
{ 63: } 328,
{ 64: } 328,
{ 65: } 343,
{ 66: } 361,
{ 67: } 361,
{ 68: } 361,
{ 69: } 361,
{ 70: } 361,
{ 71: } 379,
{ 72: } 397,
{ 73: } 397,
{ 74: } 409,
{ 75: } 421,
{ 76: } 433,
{ 77: } 445,
{ 78: } 457,
{ 79: } 469,
{ 80: } 471,
{ 81: } 471,
{ 82: } 471,
{ 83: } 471,
{ 84: } 476
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 6,
{ 1: } 7,
{ 2: } 8,
{ 3: } 9,
{ 4: } 9,
{ 5: } 10,
{ 6: } 12,
{ 7: } 18,
{ 8: } 29,
{ 9: } 29,
{ 10: } 40,
{ 11: } 47,
{ 12: } 49,
{ 13: } 49,
{ 14: } 53,
{ 15: } 53,
{ 16: } 53,
{ 17: } 54,
{ 18: } 55,
{ 19: } 55,
{ 20: } 70,
{ 21: } 70,
{ 22: } 70,
{ 23: } 70,
{ 24: } 81,
{ 25: } 92,
{ 26: } 92,
{ 27: } 111,
{ 28: } 111,
{ 29: } 122,
{ 30: } 122,
{ 31: } 122,
{ 32: } 138,
{ 33: } 138,
{ 34: } 139,
{ 35: } 142,
{ 36: } 142,
{ 37: } 142,
{ 38: } 142,
{ 39: } 142,
{ 40: } 153,
{ 41: } 164,
{ 42: } 175,
{ 43: } 186,
{ 44: } 197,
{ 45: } 208,
{ 46: } 219,
{ 47: } 230,
{ 48: } 241,
{ 49: } 252,
{ 50: } 263,
{ 51: } 274,
{ 52: } 285,
{ 53: } 296,
{ 54: } 307,
{ 55: } 312,
{ 56: } 312,
{ 57: } 312,
{ 58: } 327,
{ 59: } 327,
{ 60: } 327,
{ 61: } 327,
{ 62: } 327,
{ 63: } 327,
{ 64: } 342,
{ 65: } 360,
{ 66: } 360,
{ 67: } 360,
{ 68: } 360,
{ 69: } 360,
{ 70: } 378,
{ 71: } 396,
{ 72: } 396,
{ 73: } 408,
{ 74: } 420,
{ 75: } 432,
{ 76: } 444,
{ 77: } 456,
{ 78: } 468,
{ 79: } 470,
{ 80: } 470,
{ 81: } 470,
{ 82: } 470,
{ 83: } 475,
{ 84: } 475
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
{ 12: } 22,
{ 13: } 22,
{ 14: } 22,
{ 15: } 23,
{ 16: } 23,
{ 17: } 23,
{ 18: } 23,
{ 19: } 23,
{ 20: } 23,
{ 21: } 23,
{ 22: } 23,
{ 23: } 23,
{ 24: } 23,
{ 25: } 27,
{ 26: } 31,
{ 27: } 31,
{ 28: } 31,
{ 29: } 31,
{ 30: } 35,
{ 31: } 35,
{ 32: } 35,
{ 33: } 35,
{ 34: } 35,
{ 35: } 35,
{ 36: } 36,
{ 37: } 36,
{ 38: } 36,
{ 39: } 36,
{ 40: } 36,
{ 41: } 40,
{ 42: } 44,
{ 43: } 48,
{ 44: } 52,
{ 45: } 56,
{ 46: } 60,
{ 47: } 64,
{ 48: } 68,
{ 49: } 72,
{ 50: } 76,
{ 51: } 80,
{ 52: } 84,
{ 53: } 88,
{ 54: } 92,
{ 55: } 96,
{ 56: } 98,
{ 57: } 98,
{ 58: } 98,
{ 59: } 98,
{ 60: } 98,
{ 61: } 98,
{ 62: } 98,
{ 63: } 98,
{ 64: } 98,
{ 65: } 98,
{ 66: } 98,
{ 67: } 98,
{ 68: } 98,
{ 69: } 98,
{ 70: } 98,
{ 71: } 98,
{ 72: } 98,
{ 73: } 98,
{ 74: } 98,
{ 75: } 98,
{ 76: } 98,
{ 77: } 98,
{ 78: } 98,
{ 79: } 98,
{ 80: } 99,
{ 81: } 99,
{ 82: } 99,
{ 83: } 99,
{ 84: } 101
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
{ 11: } 21,
{ 12: } 21,
{ 13: } 21,
{ 14: } 22,
{ 15: } 22,
{ 16: } 22,
{ 17: } 22,
{ 18: } 22,
{ 19: } 22,
{ 20: } 22,
{ 21: } 22,
{ 22: } 22,
{ 23: } 22,
{ 24: } 26,
{ 25: } 30,
{ 26: } 30,
{ 27: } 30,
{ 28: } 30,
{ 29: } 34,
{ 30: } 34,
{ 31: } 34,
{ 32: } 34,
{ 33: } 34,
{ 34: } 34,
{ 35: } 35,
{ 36: } 35,
{ 37: } 35,
{ 38: } 35,
{ 39: } 35,
{ 40: } 39,
{ 41: } 43,
{ 42: } 47,
{ 43: } 51,
{ 44: } 55,
{ 45: } 59,
{ 46: } 63,
{ 47: } 67,
{ 48: } 71,
{ 49: } 75,
{ 50: } 79,
{ 51: } 83,
{ 52: } 87,
{ 53: } 91,
{ 54: } 95,
{ 55: } 97,
{ 56: } 97,
{ 57: } 97,
{ 58: } 97,
{ 59: } 97,
{ 60: } 97,
{ 61: } 97,
{ 62: } 97,
{ 63: } 97,
{ 64: } 97,
{ 65: } 97,
{ 66: } 97,
{ 67: } 97,
{ 68: } 97,
{ 69: } 97,
{ 70: } 97,
{ 71: } 97,
{ 72: } 97,
{ 73: } 97,
{ 74: } 97,
{ 75: } 97,
{ 76: } 97,
{ 77: } 97,
{ 78: } 97,
{ 79: } 98,
{ 80: } 98,
{ 81: } 98,
{ 82: } 98,
{ 83: } 100,
{ 84: } 100
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
{ 9: } ( len: 2; sym: -5 ),
{ 10: } ( len: 0; sym: -5 ),
{ 11: } ( len: 3; sym: -16 ),
{ 12: } ( len: 1; sym: -16 ),
{ 13: } ( len: 1; sym: -8 ),
{ 14: } ( len: 1; sym: -8 ),
{ 15: } ( len: 1; sym: -8 ),
{ 16: } ( len: 1; sym: -10 ),
{ 17: } ( len: 1; sym: -10 ),
{ 18: } ( len: 1; sym: -15 ),
{ 19: } ( len: 1; sym: -15 ),
{ 20: } ( len: 0; sym: -15 ),
{ 21: } ( len: 1; sym: -9 ),
{ 22: } ( len: 3; sym: -12 ),
{ 23: } ( len: 3; sym: -12 ),
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
{ 36: } ( len: 2; sym: -12 ),
{ 37: } ( len: 2; sym: -12 ),
{ 38: } ( len: 1; sym: -12 ),
{ 39: } ( len: 3; sym: -13 ),
{ 40: } ( len: 4; sym: -13 ),
{ 41: } ( len: 1; sym: -13 ),
{ 42: } ( len: 1; sym: -13 ),
{ 43: } ( len: 1; sym: -13 ),
{ 44: } ( len: 1; sym: -13 ),
{ 45: } ( len: 1; sym: -13 ),
{ 46: } ( len: 1; sym: -13 ),
{ 47: } ( len: 1; sym: -14 ),
{ 48: } ( len: 1; sym: -14 ),
{ 49: } ( len: 1; sym: -14 )
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