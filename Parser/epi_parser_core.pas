
unit epi_parser_core;

{$mode objfpc}{$H+}
{$GOTO ON}

interface

uses
  sysutils,
  epi_parser_types,
  epi_script_parser,
  epi_script_AST;

function yyparse(Const EpiParser: IEpiScriptParser; Out AST: TStatementList): boolean;

implementation

uses
  yacclib,
  lexlib;

var
  FParser: IEpiScriptParser;

const OPDefine = 257;
const OPInfo = 258;
const OPNote = 259;
const OPWarning = 260;
const OPBegin = 261;
const OPEnd = 262;
const OPTrue = 263;
const OPFalse = 264;
const OPOr = 265;
const OPAnd = 266;
const OPMod = 267;
const OPDiv = 268;
const OPMult = 269;
const OPPlus = 270;
const OPMinus = 271;
const OPDivide = 272;
const OPNot = 273;
const OPEQ = 274;
const OPNEQ = 275;
const OPLT = 276;
const OPLTE = 277;
const OPGT = 278;
const OPGTE = 279;
const OPIf = 280;
const OPThen = 281;
const OPElse = 282;
const OPString = 283;
const OPInteger = 284;
const OPFloat = 285;
const OPBoolean = 286;
const OPOpenParan = 287;
const OPCloseParan = 288;
const OPSemicolon = 289;
const OPComma = 290;
const OPPeriod = 291;
const OPAssign = 292;
const OPColon = 293;
const OPNumber = 294;
const OPHexNumber = 295;
const OPStringText = 296;
const OPIdentifier = 297;
const OPIllegal = 298;
const UMINUS = 299;


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
                 8 : ( yyTParserOperationType : TParserOperationType );
                 9 : ( yyTParserResultType : TParserResultType );
                10 : ( yyTStatementList : TStatementList );
                11 : ( yyTVarList : TVarList );
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
         yyval.yyTStatementList := TStatementList.Create(nil, nil); 
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
         yyval := yyv[yysp-0];
       end;
  14 : begin
         yyval := yyv[yysp-0];
       end;
  15 : begin
         yyval.yyTParserResultType := rtInteger; 
       end;
  16 : begin
         yyval.yyTParserResultType := rtFloat; 
       end;
  17 : begin
         yyval.yyTParserResultType := rtBoolean; 
       end;
  18 : begin
         yyval.yyTCustomVariable := TCustomVariable.FindVariable(yyv[yysp-0].yyIdString, FParser); 
       end;
  19 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  20 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otNEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  21 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  22 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  23 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  24 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  25 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otPlus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  26 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMinus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  27 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otOr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  28 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMult, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  29 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDivide, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  30 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDiv, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  31 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMod, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  32 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otAnd, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
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
         yyval.yyTExpr := yyv[yysp-1].yyTExpr; 
       end;
  37 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil) 
       end;
  38 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  39 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  40 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  41 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyExtended);  
       end;
  42 : begin
         yyval.yyTExpr := TLiteral.Create(true); 
       end;
  43 : begin
         yyval.yyTExpr := TLiteral.Create(false); 
       end;
  44 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  45 : begin
         yyval.yyTParserOperationType := otFloatCast 
       end;
  46 : begin
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

yynacts   = 472;
yyngotos  = 98;
yynstates = 83;
yynrules  = 46;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 5 ),
  ( sym: 258; act: 6 ),
  ( sym: 261; act: 7 ),
  ( sym: 280; act: 8 ),
  ( sym: 297; act: 9 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 292; act: 10 ),
{ 2: }
  ( sym: 289; act: 11 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
{ 5: }
  ( sym: 297; act: 13 ),
{ 6: }
  ( sym: 296; act: 14 ),
{ 7: }
  ( sym: 257; act: 5 ),
  ( sym: 258; act: 6 ),
  ( sym: 261; act: 7 ),
  ( sym: 280; act: 8 ),
  ( sym: 297; act: 9 ),
  ( sym: 262; act: -3 ),
{ 8: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 9: }
{ 10: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 11: }
  ( sym: 257; act: 5 ),
  ( sym: 258; act: 6 ),
  ( sym: 261; act: 7 ),
  ( sym: 280; act: 8 ),
  ( sym: 297; act: 9 ),
  ( sym: 0; act: -3 ),
  ( sym: 262; act: -3 ),
{ 12: }
  ( sym: 290; act: 32 ),
  ( sym: 293; act: 33 ),
{ 13: }
{ 14: }
  ( sym: 259; act: 35 ),
  ( sym: 260; act: 36 ),
{ 15: }
  ( sym: 262; act: 37 ),
{ 16: }
  ( sym: 287; act: 38 ),
{ 17: }
{ 18: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 277; act: 50 ),
  ( sym: 278; act: 51 ),
  ( sym: 279; act: 52 ),
  ( sym: 281; act: 53 ),
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 23: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 24: }
{ 25: }
  ( sym: 265; act: -41 ),
  ( sym: 266; act: -41 ),
  ( sym: 267; act: -41 ),
  ( sym: 268; act: -41 ),
  ( sym: 269; act: -41 ),
  ( sym: 270; act: -41 ),
  ( sym: 271; act: -41 ),
  ( sym: 272; act: -41 ),
  ( sym: 274; act: -41 ),
  ( sym: 275; act: -41 ),
  ( sym: 276; act: -41 ),
  ( sym: 277; act: -41 ),
  ( sym: 278; act: -41 ),
  ( sym: 279; act: -41 ),
  ( sym: 281; act: -41 ),
  ( sym: 282; act: -41 ),
  ( sym: 288; act: -41 ),
  ( sym: 289; act: -41 ),
  ( sym: 287; act: -45 ),
{ 26: }
{ 27: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 28: }
{ 29: }
{ 30: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 277; act: 50 ),
  ( sym: 278; act: 51 ),
  ( sym: 279; act: 52 ),
  ( sym: 282; act: -6 ),
  ( sym: 289; act: -6 ),
{ 31: }
{ 32: }
  ( sym: 297; act: 57 ),
{ 33: }
  ( sym: 284; act: 59 ),
  ( sym: 285; act: 60 ),
  ( sym: 286; act: 61 ),
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 39: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 40: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 41: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 42: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 43: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 44: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 45: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 46: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 47: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 48: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 49: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 50: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 51: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 52: }
  ( sym: 263; act: 20 ),
  ( sym: 264; act: 21 ),
  ( sym: 271; act: 22 ),
  ( sym: 273; act: 23 ),
  ( sym: 284; act: 24 ),
  ( sym: 285; act: 25 ),
  ( sym: 286; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 294; act: 28 ),
  ( sym: 295; act: 29 ),
  ( sym: 297; act: 9 ),
{ 53: }
  ( sym: 257; act: 5 ),
  ( sym: 258; act: 6 ),
  ( sym: 261; act: 7 ),
  ( sym: 280; act: 8 ),
  ( sym: 297; act: 9 ),
{ 54: }
{ 55: }
{ 56: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 277; act: 50 ),
  ( sym: 278; act: 51 ),
  ( sym: 279; act: 52 ),
  ( sym: 288; act: 78 ),
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 277; act: 50 ),
  ( sym: 278; act: 51 ),
  ( sym: 279; act: 52 ),
  ( sym: 288; act: 79 ),
{ 63: }
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 272; act: 46 ),
  ( sym: 265; act: -27 ),
  ( sym: 270; act: -27 ),
  ( sym: 271; act: -27 ),
  ( sym: 274; act: -27 ),
  ( sym: 275; act: -27 ),
  ( sym: 276; act: -27 ),
  ( sym: 277; act: -27 ),
  ( sym: 278; act: -27 ),
  ( sym: 279; act: -27 ),
  ( sym: 281; act: -27 ),
  ( sym: 282; act: -27 ),
  ( sym: 288; act: -27 ),
  ( sym: 289; act: -27 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
{ 68: }
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 272; act: 46 ),
  ( sym: 265; act: -25 ),
  ( sym: 270; act: -25 ),
  ( sym: 271; act: -25 ),
  ( sym: 274; act: -25 ),
  ( sym: 275; act: -25 ),
  ( sym: 276; act: -25 ),
  ( sym: 277; act: -25 ),
  ( sym: 278; act: -25 ),
  ( sym: 279; act: -25 ),
  ( sym: 281; act: -25 ),
  ( sym: 282; act: -25 ),
  ( sym: 288; act: -25 ),
  ( sym: 289; act: -25 ),
{ 69: }
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 272; act: 46 ),
  ( sym: 265; act: -26 ),
  ( sym: 270; act: -26 ),
  ( sym: 271; act: -26 ),
  ( sym: 274; act: -26 ),
  ( sym: 275; act: -26 ),
  ( sym: 276; act: -26 ),
  ( sym: 277; act: -26 ),
  ( sym: 278; act: -26 ),
  ( sym: 279; act: -26 ),
  ( sym: 281; act: -26 ),
  ( sym: 282; act: -26 ),
  ( sym: 288; act: -26 ),
  ( sym: 289; act: -26 ),
{ 70: }
{ 71: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 281; act: -19 ),
  ( sym: 282; act: -19 ),
  ( sym: 288; act: -19 ),
  ( sym: 289; act: -19 ),
{ 72: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 281; act: -20 ),
  ( sym: 282; act: -20 ),
  ( sym: 288; act: -20 ),
  ( sym: 289; act: -20 ),
{ 73: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 281; act: -21 ),
  ( sym: 282; act: -21 ),
  ( sym: 288; act: -21 ),
  ( sym: 289; act: -21 ),
{ 74: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 281; act: -23 ),
  ( sym: 282; act: -23 ),
  ( sym: 288; act: -23 ),
  ( sym: 289; act: -23 ),
{ 75: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 281; act: -22 ),
  ( sym: 282; act: -22 ),
  ( sym: 288; act: -22 ),
  ( sym: 289; act: -22 ),
{ 76: }
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 270; act: 44 ),
  ( sym: 271; act: 45 ),
  ( sym: 272; act: 46 ),
  ( sym: 281; act: -24 ),
  ( sym: 282; act: -24 ),
  ( sym: 288; act: -24 ),
  ( sym: 289; act: -24 ),
{ 77: }
  ( sym: 282; act: 81 ),
  ( sym: 289; act: -10 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
  ( sym: 257; act: 5 ),
  ( sym: 258; act: 6 ),
  ( sym: 261; act: 7 ),
  ( sym: 280; act: 8 ),
  ( sym: 297; act: 9 )
{ 82: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -3; act: 3 ),
  ( sym: -2; act: 4 ),
{ 1: }
{ 2: }
{ 3: }
{ 4: }
{ 5: }
  ( sym: -15; act: 12 ),
{ 6: }
{ 7: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 15 ),
{ 8: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 18 ),
  ( sym: -10; act: 19 ),
{ 9: }
{ 10: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 30 ),
  ( sym: -10; act: 19 ),
{ 11: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 31 ),
{ 12: }
{ 13: }
{ 14: }
  ( sym: -16; act: 34 ),
{ 15: }
{ 16: }
{ 17: }
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 54 ),
  ( sym: -10; act: 19 ),
{ 23: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 55 ),
  ( sym: -10; act: 19 ),
{ 24: }
{ 25: }
{ 26: }
{ 27: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 56 ),
  ( sym: -10; act: 19 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: -8; act: 58 ),
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 62 ),
  ( sym: -10; act: 19 ),
{ 39: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 63 ),
  ( sym: -10; act: 19 ),
{ 40: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 64 ),
  ( sym: -10; act: 19 ),
{ 41: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 65 ),
  ( sym: -10; act: 19 ),
{ 42: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 66 ),
  ( sym: -10; act: 19 ),
{ 43: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 67 ),
  ( sym: -10; act: 19 ),
{ 44: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 68 ),
  ( sym: -10; act: 19 ),
{ 45: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 69 ),
  ( sym: -10; act: 19 ),
{ 46: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 70 ),
  ( sym: -10; act: 19 ),
{ 47: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 71 ),
  ( sym: -10; act: 19 ),
{ 48: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 72 ),
  ( sym: -10; act: 19 ),
{ 49: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 73 ),
  ( sym: -10; act: 19 ),
{ 50: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 74 ),
  ( sym: -10; act: 19 ),
{ 51: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 75 ),
  ( sym: -10; act: 19 ),
{ 52: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 76 ),
  ( sym: -10; act: 19 ),
{ 53: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 77 ),
{ 54: }
{ 55: }
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
  ( sym: -5; act: 80 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 82 )
{ 82: }
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
{ 9: } -18,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } -12,
{ 14: } 0,
{ 15: } 0,
{ 16: } 0,
{ 17: } -35,
{ 18: } 0,
{ 19: } -38,
{ 20: } -42,
{ 21: } -43,
{ 22: } 0,
{ 23: } 0,
{ 24: } -44,
{ 25: } 0,
{ 26: } -46,
{ 27: } 0,
{ 28: } -39,
{ 29: } -40,
{ 30: } 0,
{ 31: } -2,
{ 32: } 0,
{ 33: } 0,
{ 34: } -8,
{ 35: } -13,
{ 36: } -14,
{ 37: } -4,
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
{ 54: } -34,
{ 55: } -33,
{ 56: } 0,
{ 57: } -11,
{ 58: } -7,
{ 59: } -15,
{ 60: } -16,
{ 61: } -17,
{ 62: } 0,
{ 63: } 0,
{ 64: } -32,
{ 65: } -31,
{ 66: } -30,
{ 67: } -28,
{ 68: } 0,
{ 69: } 0,
{ 70: } -29,
{ 71: } 0,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -36,
{ 79: } -37,
{ 80: } -5,
{ 81: } 0,
{ 82: } -9
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 7,
{ 2: } 8,
{ 3: } 9,
{ 4: } 10,
{ 5: } 10,
{ 6: } 11,
{ 7: } 12,
{ 8: } 18,
{ 9: } 29,
{ 10: } 29,
{ 11: } 40,
{ 12: } 47,
{ 13: } 49,
{ 14: } 49,
{ 15: } 51,
{ 16: } 52,
{ 17: } 53,
{ 18: } 53,
{ 19: } 68,
{ 20: } 68,
{ 21: } 68,
{ 22: } 68,
{ 23: } 79,
{ 24: } 90,
{ 25: } 90,
{ 26: } 109,
{ 27: } 109,
{ 28: } 120,
{ 29: } 120,
{ 30: } 120,
{ 31: } 136,
{ 32: } 136,
{ 33: } 137,
{ 34: } 140,
{ 35: } 140,
{ 36: } 140,
{ 37: } 140,
{ 38: } 140,
{ 39: } 151,
{ 40: } 162,
{ 41: } 173,
{ 42: } 184,
{ 43: } 195,
{ 44: } 206,
{ 45: } 217,
{ 46: } 228,
{ 47: } 239,
{ 48: } 250,
{ 49: } 261,
{ 50: } 272,
{ 51: } 283,
{ 52: } 294,
{ 53: } 305,
{ 54: } 310,
{ 55: } 310,
{ 56: } 310,
{ 57: } 325,
{ 58: } 325,
{ 59: } 325,
{ 60: } 325,
{ 61: } 325,
{ 62: } 325,
{ 63: } 340,
{ 64: } 358,
{ 65: } 358,
{ 66: } 358,
{ 67: } 358,
{ 68: } 358,
{ 69: } 376,
{ 70: } 394,
{ 71: } 394,
{ 72: } 406,
{ 73: } 418,
{ 74: } 430,
{ 75: } 442,
{ 76: } 454,
{ 77: } 466,
{ 78: } 468,
{ 79: } 468,
{ 80: } 468,
{ 81: } 468,
{ 82: } 473
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 6,
{ 1: } 7,
{ 2: } 8,
{ 3: } 9,
{ 4: } 9,
{ 5: } 10,
{ 6: } 11,
{ 7: } 17,
{ 8: } 28,
{ 9: } 28,
{ 10: } 39,
{ 11: } 46,
{ 12: } 48,
{ 13: } 48,
{ 14: } 50,
{ 15: } 51,
{ 16: } 52,
{ 17: } 52,
{ 18: } 67,
{ 19: } 67,
{ 20: } 67,
{ 21: } 67,
{ 22: } 78,
{ 23: } 89,
{ 24: } 89,
{ 25: } 108,
{ 26: } 108,
{ 27: } 119,
{ 28: } 119,
{ 29: } 119,
{ 30: } 135,
{ 31: } 135,
{ 32: } 136,
{ 33: } 139,
{ 34: } 139,
{ 35: } 139,
{ 36: } 139,
{ 37: } 139,
{ 38: } 150,
{ 39: } 161,
{ 40: } 172,
{ 41: } 183,
{ 42: } 194,
{ 43: } 205,
{ 44: } 216,
{ 45: } 227,
{ 46: } 238,
{ 47: } 249,
{ 48: } 260,
{ 49: } 271,
{ 50: } 282,
{ 51: } 293,
{ 52: } 304,
{ 53: } 309,
{ 54: } 309,
{ 55: } 309,
{ 56: } 324,
{ 57: } 324,
{ 58: } 324,
{ 59: } 324,
{ 60: } 324,
{ 61: } 324,
{ 62: } 339,
{ 63: } 357,
{ 64: } 357,
{ 65: } 357,
{ 66: } 357,
{ 67: } 357,
{ 68: } 375,
{ 69: } 393,
{ 70: } 393,
{ 71: } 405,
{ 72: } 417,
{ 73: } 429,
{ 74: } 441,
{ 75: } 453,
{ 76: } 465,
{ 77: } 467,
{ 78: } 467,
{ 79: } 467,
{ 80: } 467,
{ 81: } 472,
{ 82: } 472
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 5,
{ 2: } 5,
{ 3: } 5,
{ 4: } 5,
{ 5: } 5,
{ 6: } 6,
{ 7: } 6,
{ 8: } 9,
{ 9: } 13,
{ 10: } 13,
{ 11: } 17,
{ 12: } 20,
{ 13: } 20,
{ 14: } 20,
{ 15: } 21,
{ 16: } 21,
{ 17: } 21,
{ 18: } 21,
{ 19: } 21,
{ 20: } 21,
{ 21: } 21,
{ 22: } 21,
{ 23: } 25,
{ 24: } 29,
{ 25: } 29,
{ 26: } 29,
{ 27: } 29,
{ 28: } 33,
{ 29: } 33,
{ 30: } 33,
{ 31: } 33,
{ 32: } 33,
{ 33: } 33,
{ 34: } 34,
{ 35: } 34,
{ 36: } 34,
{ 37: } 34,
{ 38: } 34,
{ 39: } 38,
{ 40: } 42,
{ 41: } 46,
{ 42: } 50,
{ 43: } 54,
{ 44: } 58,
{ 45: } 62,
{ 46: } 66,
{ 47: } 70,
{ 48: } 74,
{ 49: } 78,
{ 50: } 82,
{ 51: } 86,
{ 52: } 90,
{ 53: } 94,
{ 54: } 96,
{ 55: } 96,
{ 56: } 96,
{ 57: } 96,
{ 58: } 96,
{ 59: } 96,
{ 60: } 96,
{ 61: } 96,
{ 62: } 96,
{ 63: } 96,
{ 64: } 96,
{ 65: } 96,
{ 66: } 96,
{ 67: } 96,
{ 68: } 96,
{ 69: } 96,
{ 70: } 96,
{ 71: } 96,
{ 72: } 96,
{ 73: } 96,
{ 74: } 96,
{ 75: } 96,
{ 76: } 96,
{ 77: } 96,
{ 78: } 97,
{ 79: } 97,
{ 80: } 97,
{ 81: } 97,
{ 82: } 99
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 4,
{ 1: } 4,
{ 2: } 4,
{ 3: } 4,
{ 4: } 4,
{ 5: } 5,
{ 6: } 5,
{ 7: } 8,
{ 8: } 12,
{ 9: } 12,
{ 10: } 16,
{ 11: } 19,
{ 12: } 19,
{ 13: } 19,
{ 14: } 20,
{ 15: } 20,
{ 16: } 20,
{ 17: } 20,
{ 18: } 20,
{ 19: } 20,
{ 20: } 20,
{ 21: } 20,
{ 22: } 24,
{ 23: } 28,
{ 24: } 28,
{ 25: } 28,
{ 26: } 28,
{ 27: } 32,
{ 28: } 32,
{ 29: } 32,
{ 30: } 32,
{ 31: } 32,
{ 32: } 32,
{ 33: } 33,
{ 34: } 33,
{ 35: } 33,
{ 36: } 33,
{ 37: } 33,
{ 38: } 37,
{ 39: } 41,
{ 40: } 45,
{ 41: } 49,
{ 42: } 53,
{ 43: } 57,
{ 44: } 61,
{ 45: } 65,
{ 46: } 69,
{ 47: } 73,
{ 48: } 77,
{ 49: } 81,
{ 50: } 85,
{ 51: } 89,
{ 52: } 93,
{ 53: } 95,
{ 54: } 95,
{ 55: } 95,
{ 56: } 95,
{ 57: } 95,
{ 58: } 95,
{ 59: } 95,
{ 60: } 95,
{ 61: } 95,
{ 62: } 95,
{ 63: } 95,
{ 64: } 95,
{ 65: } 95,
{ 66: } 95,
{ 67: } 95,
{ 68: } 95,
{ 69: } 95,
{ 70: } 95,
{ 71: } 95,
{ 72: } 95,
{ 73: } 95,
{ 74: } 95,
{ 75: } 95,
{ 76: } 95,
{ 77: } 96,
{ 78: } 96,
{ 79: } 96,
{ 80: } 96,
{ 81: } 98,
{ 82: } 98
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
{ 11: } ( len: 3; sym: -15 ),
{ 12: } ( len: 1; sym: -15 ),
{ 13: } ( len: 1; sym: -16 ),
{ 14: } ( len: 1; sym: -16 ),
{ 15: } ( len: 1; sym: -8 ),
{ 16: } ( len: 1; sym: -8 ),
{ 17: } ( len: 1; sym: -8 ),
{ 18: } ( len: 1; sym: -10 ),
{ 19: } ( len: 3; sym: -12 ),
{ 20: } ( len: 3; sym: -12 ),
{ 21: } ( len: 3; sym: -12 ),
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
{ 33: } ( len: 2; sym: -12 ),
{ 34: } ( len: 2; sym: -12 ),
{ 35: } ( len: 1; sym: -12 ),
{ 36: } ( len: 3; sym: -13 ),
{ 37: } ( len: 4; sym: -13 ),
{ 38: } ( len: 1; sym: -13 ),
{ 39: } ( len: 1; sym: -13 ),
{ 40: } ( len: 1; sym: -13 ),
{ 41: } ( len: 1; sym: -13 ),
{ 42: } ( len: 1; sym: -13 ),
{ 43: } ( len: 1; sym: -13 ),
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

initialization
  start(normal);

end.