
unit parser_core;

{$GOTO ON}

interface

uses
  AST,
  sysutils,
  yacclib,
  lexlib;

type
  IdString = String[64];
  TParserError = procedure (Const Msg: string; Const LineNo, ColNo: integer; Const Text: string);

var
  StmList: TStatementList;
  OnParseError: TParserError;

function yyparse: integer;

implementation

uses
  parser_types;
  

const OPBegin = 257;
const OPEnd = 258;
const OPClear = 259;
const OPDefine = 260;
const OPExecute = 261;
const OPTrue = 262;
const OPFalse = 263;
const OPAnd = 264;
const OPMod = 265;
const OPDiv = 266;
const OPShl = 267;
const OPShr = 268;
const OPOr = 269;
const OPXor = 270;
const OPMult = 271;
const OPPlus = 272;
const OPMinus = 273;
const OPDivide = 274;
const OPNot = 275;
const OPEQ = 276;
const OPNEQ = 277;
const OPLT = 278;
const OPLTE = 279;
const OPGT = 280;
const OPGTE = 281;
const OPIf = 282;
const OPThen = 283;
const OPElse = 284;
const OPStringCast = 285;
const OPIntegerCast = 286;
const OPFloatCast = 287;
const OPOpenParan = 288;
const OPCloseParan = 289;
const OPOpenBracket = 290;
const OPCloseBracket = 291;
const OPHash = 292;
const OPSemicolon = 293;
const OPComma = 294;
const OPPeriod = 295;
const OPAssign = 296;
const OPFloat = 297;
const OPNumber = 298;
const OPHexNumber = 299;
const OPString = 300;
const OPIdentifier = 301;
const OPIllegal = 302;
const UMINUS = 303;


procedure yyerror(msg : string);
  begin
    if Assigned(OnParseError) then
      OnParseError(Msg, yylineno, yycolno, yytext)
    else if IsConsole then
      writeln('(', yylineno, ',', yycolno, '): ', msg, ' at or before ''', yytext, '''.')
  end(*yyerror*);


type YYSType = record case Integer of
                 1 : ( yyBoolean : Boolean );
                 2 : ( yyExtended : Extended );
                 3 : ( yyIdString : IdString );
                 4 : ( yyInteger : Integer );
                 5 : ( yyTCustomStatement : TCustomStatement );
                 6 : ( yyTExpr : TExpr );
                 7 : ( yyTOptElse : TOptElse );
                 8 : ( yyTParserOperationType : TParserOperationType );
                 9 : ( yyTStatementList : TStatementList );
                10 : ( yyTTerm : TTerm );
                11 : ( yyTVarList : TVarList );
                12 : ( yyTVariable : TVariable );
                13 : ( yyWord : Word );
               end(*YYSType*);

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse : Integer;

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
         StmList := yyv[yysp-0].yyTStatementList 
       end;
   2 : begin
         yyval.yyTStatementList := TStatementList.Create(yyv[yysp-2].yyTCustomStatement, yyv[yysp-0].yyTStatementList); 
       end;
   3 : begin
         yyval.yyTStatementList := nil; 
       end;
   4 : begin
         yyval.yyTCustomStatement := TStatementList.Create(yyv[yysp-1].yyTStatementList, nil); 
       end;
   5 : begin
         yyval.yyTCustomStatement := TIfThen.Create(yyv[yysp-3].yyTExpr, yyv[yysp-1].yyTCustomStatement, yyv[yysp-0].yyTOptElse); 
       end;
   6 : begin
         yyval.yyTCustomStatement := TAssignment.Create(yyv[yysp-2].yyTVariable, yyv[yysp-0].yyTExpr); 
       end;
   7 : begin
         yyval.yyTCustomStatement := TStatement.Create(yyv[yysp-1].yyWord, yyv[yysp-0].yyTVarList); 
       end;
   8 : begin
         yyval.yyTCustomStatement := TDefine.Create(yyv[yysp-1].yyWord, yyv[yysp-0].yyIdString); 
       end;
   9 : begin
         yyval.yyTOptElse := TOptElse.Create(yyv[yysp-0].yyTCustomStatement); 
       end;
  10 : begin
         yyval.yyTOptElse := nil; 
       end;
  11 : begin
         yyval.yyWord := OPClear; 
       end;
  12 : begin
         yyval.yyWord := OPIntegerCast; 
       end;
  13 : begin
         yyval.yyTVarList := TVarlist.Create(yyv[yysp-1].yyTVariable, yyv[yysp-0].yyTVarList); 
       end;
  14 : begin
         yyval.yyTVarList := nil; 
       end;
  15 : begin
         yyval.yyTVariable := TVariable.Create(yyv[yysp-0].yyIdString); 
       end;
  16 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  17 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otNEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  18 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  19 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  20 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  21 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  22 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otPlus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  23 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otMinus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  24 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otOr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  25 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otXor, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  26 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMult, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  27 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDivide, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  28 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDiv, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  29 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMod, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  30 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otAnd, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  31 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otShl, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  32 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otShr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  33 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otNot,  yyv[yysp-0].yyTExpr, nil); 
       end;
  34 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otMinus, yyv[yysp-0].yyTExpr, nil); 
       end;
  35 : begin
         yyval := yyv[yysp-0];
       end;
  36 : begin
         yyval.yyTTerm := TParen.Create(yyv[yysp-1].yyTExpr); 
       end;
  37 : begin
         yyval.yyTTerm := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr) 
       end;
  38 : begin
         yyval.yyTTerm := TTermVar.Create(yyv[yysp-0].yyTVariable); 
       end;
  39 : begin
         yyval.yyTTerm := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  40 : begin
         yyval.yyTTerm := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  41 : begin
         yyval.yyTTerm := TLiteral.Create(yyv[yysp-0].yyExtended);  
       end;
  42 : begin
         yyval.yyTTerm := TLiteral.Create(yyv[yysp-0].yyBoolean); 
       end;
  43 : begin
         yyval.yyTTerm := TLiteral.Create(yyv[yysp-0].yyBoolean); 
       end;
  44 : begin
         yyval.yyTParserOperationType := otStringCast 
       end;
  45 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  46 : begin
         yyval.yyTParserOperationType := otFloatCast 
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

yynacts   = 567;
yyngotos  = 117;
yynstates = 84;
yynrules  = 46;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 6 ),
  ( sym: 259; act: 7 ),
  ( sym: 260; act: 8 ),
  ( sym: 282; act: 9 ),
  ( sym: 301; act: 10 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 296; act: 11 ),
{ 2: }
  ( sym: 301; act: 10 ),
  ( sym: 284; act: -14 ),
  ( sym: 293; act: -14 ),
{ 3: }
  ( sym: 293; act: 14 ),
{ 4: }
  ( sym: 0; act: 0 ),
{ 5: }
{ 6: }
  ( sym: 257; act: 6 ),
  ( sym: 259; act: 7 ),
  ( sym: 260; act: 8 ),
  ( sym: 282; act: 9 ),
  ( sym: 301; act: 10 ),
  ( sym: 258; act: -3 ),
{ 7: }
{ 8: }
  ( sym: 286; act: 17 ),
{ 9: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 10: }
{ 11: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 12: }
  ( sym: 301; act: 10 ),
  ( sym: 284; act: -14 ),
  ( sym: 293; act: -14 ),
{ 13: }
{ 14: }
  ( sym: 257; act: 6 ),
  ( sym: 259; act: 7 ),
  ( sym: 260; act: 8 ),
  ( sym: 282; act: 9 ),
  ( sym: 301; act: 10 ),
  ( sym: 0; act: -3 ),
  ( sym: 258; act: -3 ),
{ 15: }
  ( sym: 258; act: 36 ),
{ 16: }
  ( sym: 301; act: 37 ),
{ 17: }
{ 18: }
  ( sym: 288; act: 38 ),
{ 19: }
{ 20: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 276; act: 50 ),
  ( sym: 277; act: 51 ),
  ( sym: 278; act: 52 ),
  ( sym: 279; act: 53 ),
  ( sym: 280; act: 54 ),
  ( sym: 281; act: 55 ),
  ( sym: 283; act: 56 ),
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 25: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 276; act: 50 ),
  ( sym: 277; act: 51 ),
  ( sym: 278; act: 52 ),
  ( sym: 279; act: 53 ),
  ( sym: 280; act: 54 ),
  ( sym: 281; act: 55 ),
  ( sym: 284; act: -6 ),
  ( sym: 293; act: -6 ),
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 39: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 40: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 41: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 42: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 43: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 44: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 45: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 46: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 47: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 48: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 49: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 50: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 51: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 52: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 53: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 54: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 55: }
  ( sym: 262; act: 22 ),
  ( sym: 263; act: 23 ),
  ( sym: 273; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 285; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 297; act: 30 ),
  ( sym: 298; act: 31 ),
  ( sym: 299; act: 32 ),
  ( sym: 301; act: 10 ),
{ 56: }
  ( sym: 257; act: 6 ),
  ( sym: 259; act: 7 ),
  ( sym: 260; act: 8 ),
  ( sym: 282; act: 9 ),
  ( sym: 301; act: 10 ),
{ 57: }
{ 58: }
{ 59: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 276; act: 50 ),
  ( sym: 277; act: 51 ),
  ( sym: 278; act: 52 ),
  ( sym: 279; act: 53 ),
  ( sym: 280; act: 54 ),
  ( sym: 281; act: 55 ),
  ( sym: 289; act: 79 ),
{ 60: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 276; act: 50 ),
  ( sym: 277; act: 51 ),
  ( sym: 278; act: 52 ),
  ( sym: 279; act: 53 ),
  ( sym: 280; act: 54 ),
  ( sym: 281; act: 55 ),
  ( sym: 289; act: 80 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 271; act: 46 ),
  ( sym: 274; act: 49 ),
  ( sym: 269; act: -24 ),
  ( sym: 270; act: -24 ),
  ( sym: 272; act: -24 ),
  ( sym: 273; act: -24 ),
  ( sym: 276; act: -24 ),
  ( sym: 277; act: -24 ),
  ( sym: 278; act: -24 ),
  ( sym: 279; act: -24 ),
  ( sym: 280; act: -24 ),
  ( sym: 281; act: -24 ),
  ( sym: 283; act: -24 ),
  ( sym: 284; act: -24 ),
  ( sym: 289; act: -24 ),
  ( sym: 293; act: -24 ),
{ 67: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 271; act: 46 ),
  ( sym: 274; act: 49 ),
  ( sym: 269; act: -25 ),
  ( sym: 270; act: -25 ),
  ( sym: 272; act: -25 ),
  ( sym: 273; act: -25 ),
  ( sym: 276; act: -25 ),
  ( sym: 277; act: -25 ),
  ( sym: 278; act: -25 ),
  ( sym: 279; act: -25 ),
  ( sym: 280; act: -25 ),
  ( sym: 281; act: -25 ),
  ( sym: 283; act: -25 ),
  ( sym: 284; act: -25 ),
  ( sym: 289; act: -25 ),
  ( sym: 293; act: -25 ),
{ 68: }
{ 69: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 271; act: 46 ),
  ( sym: 274; act: 49 ),
  ( sym: 269; act: -22 ),
  ( sym: 270; act: -22 ),
  ( sym: 272; act: -22 ),
  ( sym: 273; act: -22 ),
  ( sym: 276; act: -22 ),
  ( sym: 277; act: -22 ),
  ( sym: 278; act: -22 ),
  ( sym: 279; act: -22 ),
  ( sym: 280; act: -22 ),
  ( sym: 281; act: -22 ),
  ( sym: 283; act: -22 ),
  ( sym: 284; act: -22 ),
  ( sym: 289; act: -22 ),
  ( sym: 293; act: -22 ),
{ 70: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 271; act: 46 ),
  ( sym: 274; act: 49 ),
  ( sym: 269; act: -23 ),
  ( sym: 270; act: -23 ),
  ( sym: 272; act: -23 ),
  ( sym: 273; act: -23 ),
  ( sym: 276; act: -23 ),
  ( sym: 277; act: -23 ),
  ( sym: 278; act: -23 ),
  ( sym: 279; act: -23 ),
  ( sym: 280; act: -23 ),
  ( sym: 281; act: -23 ),
  ( sym: 283; act: -23 ),
  ( sym: 284; act: -23 ),
  ( sym: 289; act: -23 ),
  ( sym: 293; act: -23 ),
{ 71: }
{ 72: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 283; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 289; act: -16 ),
  ( sym: 293; act: -16 ),
{ 73: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 283; act: -17 ),
  ( sym: 284; act: -17 ),
  ( sym: 289; act: -17 ),
  ( sym: 293; act: -17 ),
{ 74: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 283; act: -18 ),
  ( sym: 284; act: -18 ),
  ( sym: 289; act: -18 ),
  ( sym: 293; act: -18 ),
{ 75: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 283; act: -20 ),
  ( sym: 284; act: -20 ),
  ( sym: 289; act: -20 ),
  ( sym: 293; act: -20 ),
{ 76: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 283; act: -19 ),
  ( sym: 284; act: -19 ),
  ( sym: 289; act: -19 ),
  ( sym: 293; act: -19 ),
{ 77: }
  ( sym: 264; act: 39 ),
  ( sym: 265; act: 40 ),
  ( sym: 266; act: 41 ),
  ( sym: 267; act: 42 ),
  ( sym: 268; act: 43 ),
  ( sym: 269; act: 44 ),
  ( sym: 270; act: 45 ),
  ( sym: 271; act: 46 ),
  ( sym: 272; act: 47 ),
  ( sym: 273; act: 48 ),
  ( sym: 274; act: 49 ),
  ( sym: 283; act: -21 ),
  ( sym: 284; act: -21 ),
  ( sym: 289; act: -21 ),
  ( sym: 293; act: -21 ),
{ 78: }
  ( sym: 284; act: 82 ),
  ( sym: 293; act: -10 ),
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: 257; act: 6 ),
  ( sym: 259; act: 7 ),
  ( sym: 260; act: 8 ),
  ( sym: 282; act: 9 ),
  ( sym: 301; act: 10 )
{ 83: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -9; act: 1 ),
  ( sym: -5; act: 2 ),
  ( sym: -4; act: 3 ),
  ( sym: -3; act: 4 ),
  ( sym: -2; act: 5 ),
{ 1: }
{ 2: }
  ( sym: -9; act: 12 ),
  ( sym: -8; act: 13 ),
{ 3: }
{ 4: }
{ 5: }
{ 6: }
  ( sym: -9; act: 1 ),
  ( sym: -5; act: 2 ),
  ( sym: -4; act: 3 ),
  ( sym: -2; act: 15 ),
{ 7: }
{ 8: }
  ( sym: -7; act: 16 ),
{ 9: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 20 ),
  ( sym: -9; act: 21 ),
{ 10: }
{ 11: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 33 ),
  ( sym: -9; act: 21 ),
{ 12: }
  ( sym: -9; act: 12 ),
  ( sym: -8; act: 34 ),
{ 13: }
{ 14: }
  ( sym: -9; act: 1 ),
  ( sym: -5; act: 2 ),
  ( sym: -4; act: 3 ),
  ( sym: -2; act: 35 ),
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
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 57 ),
  ( sym: -9; act: 21 ),
{ 25: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 58 ),
  ( sym: -9; act: 21 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 59 ),
  ( sym: -9; act: 21 ),
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 60 ),
  ( sym: -9; act: 21 ),
{ 39: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 61 ),
  ( sym: -9; act: 21 ),
{ 40: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 62 ),
  ( sym: -9; act: 21 ),
{ 41: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 63 ),
  ( sym: -9; act: 21 ),
{ 42: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 64 ),
  ( sym: -9; act: 21 ),
{ 43: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 65 ),
  ( sym: -9; act: 21 ),
{ 44: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 66 ),
  ( sym: -9; act: 21 ),
{ 45: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 67 ),
  ( sym: -9; act: 21 ),
{ 46: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 68 ),
  ( sym: -9; act: 21 ),
{ 47: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 69 ),
  ( sym: -9; act: 21 ),
{ 48: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 70 ),
  ( sym: -9; act: 21 ),
{ 49: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 71 ),
  ( sym: -9; act: 21 ),
{ 50: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 72 ),
  ( sym: -9; act: 21 ),
{ 51: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 73 ),
  ( sym: -9; act: 21 ),
{ 52: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 74 ),
  ( sym: -9; act: 21 ),
{ 53: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 75 ),
  ( sym: -9; act: 21 ),
{ 54: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 76 ),
  ( sym: -9; act: 21 ),
{ 55: }
  ( sym: -14; act: 18 ),
  ( sym: -12; act: 19 ),
  ( sym: -11; act: 77 ),
  ( sym: -9; act: 21 ),
{ 56: }
  ( sym: -9; act: 1 ),
  ( sym: -5; act: 2 ),
  ( sym: -4; act: 78 ),
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
  ( sym: -13; act: 81 ),
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: -9; act: 1 ),
  ( sym: -5; act: 2 ),
  ( sym: -4; act: 83 )
{ 83: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } 0,
{ 4: } 0,
{ 5: } -1,
{ 6: } 0,
{ 7: } -11,
{ 8: } 0,
{ 9: } 0,
{ 10: } -15,
{ 11: } 0,
{ 12: } 0,
{ 13: } -7,
{ 14: } 0,
{ 15: } 0,
{ 16: } 0,
{ 17: } -12,
{ 18: } 0,
{ 19: } -35,
{ 20: } 0,
{ 21: } -38,
{ 22: } -42,
{ 23: } -43,
{ 24: } 0,
{ 25: } 0,
{ 26: } -44,
{ 27: } -45,
{ 28: } -46,
{ 29: } 0,
{ 30: } -41,
{ 31: } -39,
{ 32: } -40,
{ 33: } 0,
{ 34: } -13,
{ 35: } -2,
{ 36: } -4,
{ 37: } -8,
{ 38: } 0,
{ 39: } 0,
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
{ 56: } 0,
{ 57: } -34,
{ 58: } -33,
{ 59: } 0,
{ 60: } 0,
{ 61: } -30,
{ 62: } -29,
{ 63: } -28,
{ 64: } -31,
{ 65: } -32,
{ 66: } 0,
{ 67: } 0,
{ 68: } -26,
{ 69: } 0,
{ 70: } 0,
{ 71: } -27,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } -36,
{ 80: } -37,
{ 81: } -5,
{ 82: } 0,
{ 83: } -9
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 7,
{ 2: } 8,
{ 3: } 11,
{ 4: } 12,
{ 5: } 13,
{ 6: } 13,
{ 7: } 19,
{ 8: } 19,
{ 9: } 20,
{ 10: } 32,
{ 11: } 32,
{ 12: } 44,
{ 13: } 47,
{ 14: } 47,
{ 15: } 54,
{ 16: } 55,
{ 17: } 56,
{ 18: } 56,
{ 19: } 57,
{ 20: } 57,
{ 21: } 75,
{ 22: } 75,
{ 23: } 75,
{ 24: } 75,
{ 25: } 87,
{ 26: } 99,
{ 27: } 99,
{ 28: } 99,
{ 29: } 99,
{ 30: } 111,
{ 31: } 111,
{ 32: } 111,
{ 33: } 111,
{ 34: } 130,
{ 35: } 130,
{ 36: } 130,
{ 37: } 130,
{ 38: } 130,
{ 39: } 142,
{ 40: } 154,
{ 41: } 166,
{ 42: } 178,
{ 43: } 190,
{ 44: } 202,
{ 45: } 214,
{ 46: } 226,
{ 47: } 238,
{ 48: } 250,
{ 49: } 262,
{ 50: } 274,
{ 51: } 286,
{ 52: } 298,
{ 53: } 310,
{ 54: } 322,
{ 55: } 334,
{ 56: } 346,
{ 57: } 351,
{ 58: } 351,
{ 59: } 351,
{ 60: } 369,
{ 61: } 387,
{ 62: } 387,
{ 63: } 387,
{ 64: } 387,
{ 65: } 387,
{ 66: } 387,
{ 67: } 408,
{ 68: } 429,
{ 69: } 429,
{ 70: } 450,
{ 71: } 471,
{ 72: } 471,
{ 73: } 486,
{ 74: } 501,
{ 75: } 516,
{ 76: } 531,
{ 77: } 546,
{ 78: } 561,
{ 79: } 563,
{ 80: } 563,
{ 81: } 563,
{ 82: } 563,
{ 83: } 568
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 6,
{ 1: } 7,
{ 2: } 10,
{ 3: } 11,
{ 4: } 12,
{ 5: } 12,
{ 6: } 18,
{ 7: } 18,
{ 8: } 19,
{ 9: } 31,
{ 10: } 31,
{ 11: } 43,
{ 12: } 46,
{ 13: } 46,
{ 14: } 53,
{ 15: } 54,
{ 16: } 55,
{ 17: } 55,
{ 18: } 56,
{ 19: } 56,
{ 20: } 74,
{ 21: } 74,
{ 22: } 74,
{ 23: } 74,
{ 24: } 86,
{ 25: } 98,
{ 26: } 98,
{ 27: } 98,
{ 28: } 98,
{ 29: } 110,
{ 30: } 110,
{ 31: } 110,
{ 32: } 110,
{ 33: } 129,
{ 34: } 129,
{ 35: } 129,
{ 36: } 129,
{ 37: } 129,
{ 38: } 141,
{ 39: } 153,
{ 40: } 165,
{ 41: } 177,
{ 42: } 189,
{ 43: } 201,
{ 44: } 213,
{ 45: } 225,
{ 46: } 237,
{ 47: } 249,
{ 48: } 261,
{ 49: } 273,
{ 50: } 285,
{ 51: } 297,
{ 52: } 309,
{ 53: } 321,
{ 54: } 333,
{ 55: } 345,
{ 56: } 350,
{ 57: } 350,
{ 58: } 350,
{ 59: } 368,
{ 60: } 386,
{ 61: } 386,
{ 62: } 386,
{ 63: } 386,
{ 64: } 386,
{ 65: } 386,
{ 66: } 407,
{ 67: } 428,
{ 68: } 428,
{ 69: } 449,
{ 70: } 470,
{ 71: } 470,
{ 72: } 485,
{ 73: } 500,
{ 74: } 515,
{ 75: } 530,
{ 76: } 545,
{ 77: } 560,
{ 78: } 562,
{ 79: } 562,
{ 80: } 562,
{ 81: } 562,
{ 82: } 567,
{ 83: } 567
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 6,
{ 2: } 6,
{ 3: } 8,
{ 4: } 8,
{ 5: } 8,
{ 6: } 8,
{ 7: } 12,
{ 8: } 12,
{ 9: } 13,
{ 10: } 17,
{ 11: } 17,
{ 12: } 21,
{ 13: } 23,
{ 14: } 23,
{ 15: } 27,
{ 16: } 27,
{ 17: } 27,
{ 18: } 27,
{ 19: } 27,
{ 20: } 27,
{ 21: } 27,
{ 22: } 27,
{ 23: } 27,
{ 24: } 27,
{ 25: } 31,
{ 26: } 35,
{ 27: } 35,
{ 28: } 35,
{ 29: } 35,
{ 30: } 39,
{ 31: } 39,
{ 32: } 39,
{ 33: } 39,
{ 34: } 39,
{ 35: } 39,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 43,
{ 40: } 47,
{ 41: } 51,
{ 42: } 55,
{ 43: } 59,
{ 44: } 63,
{ 45: } 67,
{ 46: } 71,
{ 47: } 75,
{ 48: } 79,
{ 49: } 83,
{ 50: } 87,
{ 51: } 91,
{ 52: } 95,
{ 53: } 99,
{ 54: } 103,
{ 55: } 107,
{ 56: } 111,
{ 57: } 114,
{ 58: } 114,
{ 59: } 114,
{ 60: } 114,
{ 61: } 114,
{ 62: } 114,
{ 63: } 114,
{ 64: } 114,
{ 65: } 114,
{ 66: } 114,
{ 67: } 114,
{ 68: } 114,
{ 69: } 114,
{ 70: } 114,
{ 71: } 114,
{ 72: } 114,
{ 73: } 114,
{ 74: } 114,
{ 75: } 114,
{ 76: } 114,
{ 77: } 114,
{ 78: } 114,
{ 79: } 115,
{ 80: } 115,
{ 81: } 115,
{ 82: } 115,
{ 83: } 118
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 5,
{ 1: } 5,
{ 2: } 7,
{ 3: } 7,
{ 4: } 7,
{ 5: } 7,
{ 6: } 11,
{ 7: } 11,
{ 8: } 12,
{ 9: } 16,
{ 10: } 16,
{ 11: } 20,
{ 12: } 22,
{ 13: } 22,
{ 14: } 26,
{ 15: } 26,
{ 16: } 26,
{ 17: } 26,
{ 18: } 26,
{ 19: } 26,
{ 20: } 26,
{ 21: } 26,
{ 22: } 26,
{ 23: } 26,
{ 24: } 30,
{ 25: } 34,
{ 26: } 34,
{ 27: } 34,
{ 28: } 34,
{ 29: } 38,
{ 30: } 38,
{ 31: } 38,
{ 32: } 38,
{ 33: } 38,
{ 34: } 38,
{ 35: } 38,
{ 36: } 38,
{ 37: } 38,
{ 38: } 42,
{ 39: } 46,
{ 40: } 50,
{ 41: } 54,
{ 42: } 58,
{ 43: } 62,
{ 44: } 66,
{ 45: } 70,
{ 46: } 74,
{ 47: } 78,
{ 48: } 82,
{ 49: } 86,
{ 50: } 90,
{ 51: } 94,
{ 52: } 98,
{ 53: } 102,
{ 54: } 106,
{ 55: } 110,
{ 56: } 113,
{ 57: } 113,
{ 58: } 113,
{ 59: } 113,
{ 60: } 113,
{ 61: } 113,
{ 62: } 113,
{ 63: } 113,
{ 64: } 113,
{ 65: } 113,
{ 66: } 113,
{ 67: } 113,
{ 68: } 113,
{ 69: } 113,
{ 70: } 113,
{ 71: } 113,
{ 72: } 113,
{ 73: } 113,
{ 74: } 113,
{ 75: } 113,
{ 76: } 113,
{ 77: } 113,
{ 78: } 114,
{ 79: } 114,
{ 80: } 114,
{ 81: } 114,
{ 82: } 117,
{ 83: } 117
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -3 ),
{ 2: } ( len: 3; sym: -2 ),
{ 3: } ( len: 0; sym: -2 ),
{ 4: } ( len: 3; sym: -4 ),
{ 5: } ( len: 5; sym: -4 ),
{ 6: } ( len: 3; sym: -4 ),
{ 7: } ( len: 2; sym: -4 ),
{ 8: } ( len: 3; sym: -4 ),
{ 9: } ( len: 2; sym: -13 ),
{ 10: } ( len: 0; sym: -13 ),
{ 11: } ( len: 1; sym: -5 ),
{ 12: } ( len: 1; sym: -7 ),
{ 13: } ( len: 2; sym: -8 ),
{ 14: } ( len: 0; sym: -8 ),
{ 15: } ( len: 1; sym: -9 ),
{ 16: } ( len: 3; sym: -11 ),
{ 17: } ( len: 3; sym: -11 ),
{ 18: } ( len: 3; sym: -11 ),
{ 19: } ( len: 3; sym: -11 ),
{ 20: } ( len: 3; sym: -11 ),
{ 21: } ( len: 3; sym: -11 ),
{ 22: } ( len: 3; sym: -11 ),
{ 23: } ( len: 3; sym: -11 ),
{ 24: } ( len: 3; sym: -11 ),
{ 25: } ( len: 3; sym: -11 ),
{ 26: } ( len: 3; sym: -11 ),
{ 27: } ( len: 3; sym: -11 ),
{ 28: } ( len: 3; sym: -11 ),
{ 29: } ( len: 3; sym: -11 ),
{ 30: } ( len: 3; sym: -11 ),
{ 31: } ( len: 3; sym: -11 ),
{ 32: } ( len: 3; sym: -11 ),
{ 33: } ( len: 2; sym: -11 ),
{ 34: } ( len: 2; sym: -11 ),
{ 35: } ( len: 1; sym: -11 ),
{ 36: } ( len: 3; sym: -12 ),
{ 37: } ( len: 4; sym: -12 ),
{ 38: } ( len: 1; sym: -12 ),
{ 39: } ( len: 1; sym: -12 ),
{ 40: } ( len: 1; sym: -12 ),
{ 41: } ( len: 1; sym: -12 ),
{ 42: } ( len: 1; sym: -12 ),
{ 43: } ( len: 1; sym: -12 ),
{ 44: } ( len: 1; sym: -14 ),
{ 45: } ( len: 1; sym: -14 ),
{ 46: } ( len: 1; sym: -14 )
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

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);


{$I parser_core.inc}

initialization
  start(normal);

end.