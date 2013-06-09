
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
  classes, lexlib, epiconvertutils, epidatafilestypes;

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
  ParamList: TList;

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

procedure AddToParamList(Expr: TExpr);
begin
  if not Assigned(ParamList) then
    ParamList := TList.Create;
  ParamList.Add(Expr);
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
                10 : ( yyTParamList : TParamList );
                11 : ( yyTParserOperationType : TParserOperationType );
                12 : ( yyTParserResultType : TParserResultType );
                13 : ( yyTStatementList : TStatementList );
                14 : ( yyWord : Word );
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
         yyval.yyTExpr := TFunction.CreateFunction(yyv[yysp-3].yyIdString, yyv[yysp-1].yyTParamList, FParser);
       end;
  43 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil); 
       end;
  44 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  45 : begin
         yyval.yyTExpr := TIntegerLiteral.Create(yyv[yysp-0].yyEpiInteger);  
       end;
  46 : begin
         yyval.yyTExpr := TFloatLiteral.Create(yyv[yysp-0].yyEpiFloat);  
       end;
  47 : begin
         
         									  if Assigned(yyv[yysp-0].yyPString) then
         									    yyval.yyTExpr := TStringLiteral.Create(yyv[yysp-0].yyPString^)
         									  else
         									    yyval.yyTExpr := TStringLiteral.Create('');
         									  DisposeStr(yyv[yysp-0].yyPString);
         									
       end;
  48 : begin
         yyval.yyTExpr := TMissingLiteral.Create; 
       end;
  49 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(true); 
       end;
  50 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(false); 
       end;
  51 : begin
         yyval.yyTParamList := TParamList.Create(ParamList); FreeAndNil(ParamList); 
       end;
  52 : begin
         yyval.yyTParamList := TParamList.Create(nil); 
       end;
  53 : begin
         AddToParamList(yyv[yysp-2].yyTExpr); 
       end;
  54 : begin
         AddToParamList(yyv[yysp-0].yyTExpr); 
       end;
  55 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  56 : begin
         yyval.yyTParserOperationType := otStringCast 
       end;
  57 : begin
         yyval.yyTParserOperationType := otFloatCast 
       end;
  58 : begin
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

yynacts   = 627;
yyngotos  = 115;
yynstates = 99;
yynrules  = 58;

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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 294; act: 40 ),
  ( sym: 297; act: 41 ),
{ 14: }
{ 15: }
  ( sym: 262; act: 43 ),
  ( sym: 263; act: 44 ),
  ( sym: 285; act: -22 ),
  ( sym: 293; act: -22 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: 265; act: 45 ),
{ 19: }
  ( sym: 291; act: 46 ),
{ 20: }
{ 21: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 277; act: 55 ),
  ( sym: 278; act: 56 ),
  ( sym: 279; act: 57 ),
  ( sym: 280; act: 58 ),
  ( sym: 281; act: 59 ),
  ( sym: 282; act: 60 ),
  ( sym: 284; act: 61 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: 291; act: 65 ),
  ( sym: 268; act: -23 ),
  ( sym: 269; act: -23 ),
  ( sym: 270; act: -23 ),
  ( sym: 271; act: -23 ),
  ( sym: 272; act: -23 ),
  ( sym: 273; act: -23 ),
  ( sym: 274; act: -23 ),
  ( sym: 275; act: -23 ),
  ( sym: 277; act: -23 ),
  ( sym: 278; act: -23 ),
  ( sym: 279; act: -23 ),
  ( sym: 280; act: -23 ),
  ( sym: 281; act: -23 ),
  ( sym: 282; act: -23 ),
  ( sym: 284; act: -23 ),
  ( sym: 285; act: -23 ),
  ( sym: 292; act: -23 ),
  ( sym: 293; act: -23 ),
  ( sym: 294; act: -23 ),
{ 36: }
{ 37: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 277; act: 55 ),
  ( sym: 278; act: 56 ),
  ( sym: 279; act: 57 ),
  ( sym: 280; act: 58 ),
  ( sym: 281; act: 59 ),
  ( sym: 282; act: 60 ),
  ( sym: 285; act: -9 ),
  ( sym: 293; act: -9 ),
{ 38: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 277; act: 55 ),
  ( sym: 278; act: 56 ),
  ( sym: 279; act: 57 ),
  ( sym: 280; act: 58 ),
  ( sym: 281; act: 59 ),
  ( sym: 282; act: 60 ),
  ( sym: 285; act: -6 ),
  ( sym: 293; act: -6 ),
{ 39: }
{ 40: }
  ( sym: 300; act: 66 ),
{ 41: }
  ( sym: 286; act: 68 ),
  ( sym: 287; act: 69 ),
  ( sym: 288; act: 70 ),
  ( sym: 289; act: 71 ),
{ 42: }
{ 43: }
{ 44: }
{ 45: }
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
{ 60: }
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
{ 61: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
{ 62: }
{ 63: }
{ 64: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 277; act: 55 ),
  ( sym: 278; act: 56 ),
  ( sym: 279; act: 57 ),
  ( sym: 280; act: 58 ),
  ( sym: 281; act: 59 ),
  ( sym: 282; act: 60 ),
  ( sym: 292; act: 88 ),
{ 65: }
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 ),
  ( sym: 292; act: -52 ),
{ 66: }
{ 67: }
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 277; act: 55 ),
  ( sym: 278; act: 56 ),
  ( sym: 279; act: 57 ),
  ( sym: 280; act: 58 ),
  ( sym: 281; act: 59 ),
  ( sym: 282; act: 60 ),
  ( sym: 292; act: 92 ),
{ 73: }
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 275; act: 54 ),
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
  ( sym: 294; act: -32 ),
{ 74: }
{ 75: }
{ 76: }
{ 77: }
{ 78: }
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 275; act: 54 ),
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
  ( sym: 294; act: -30 ),
{ 79: }
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 275; act: 54 ),
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
  ( sym: 294; act: -31 ),
{ 80: }
{ 81: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 284; act: -24 ),
  ( sym: 285; act: -24 ),
  ( sym: 292; act: -24 ),
  ( sym: 293; act: -24 ),
  ( sym: 294; act: -24 ),
{ 82: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 284; act: -25 ),
  ( sym: 285; act: -25 ),
  ( sym: 292; act: -25 ),
  ( sym: 293; act: -25 ),
  ( sym: 294; act: -25 ),
{ 83: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 284; act: -26 ),
  ( sym: 285; act: -26 ),
  ( sym: 292; act: -26 ),
  ( sym: 293; act: -26 ),
  ( sym: 294; act: -26 ),
{ 84: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 284; act: -28 ),
  ( sym: 285; act: -28 ),
  ( sym: 292; act: -28 ),
  ( sym: 293; act: -28 ),
  ( sym: 294; act: -28 ),
{ 85: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 284; act: -27 ),
  ( sym: 285; act: -27 ),
  ( sym: 292; act: -27 ),
  ( sym: 293; act: -27 ),
  ( sym: 294; act: -27 ),
{ 86: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 284; act: -29 ),
  ( sym: 285; act: -29 ),
  ( sym: 292; act: -29 ),
  ( sym: 293; act: -29 ),
  ( sym: 294; act: -29 ),
{ 87: }
  ( sym: 285; act: 94 ),
  ( sym: 293; act: -11 ),
{ 88: }
{ 89: }
{ 90: }
  ( sym: 292; act: 95 ),
{ 91: }
  ( sym: 268; act: 47 ),
  ( sym: 269; act: 48 ),
  ( sym: 270; act: 49 ),
  ( sym: 271; act: 50 ),
  ( sym: 272; act: 51 ),
  ( sym: 273; act: 52 ),
  ( sym: 274; act: 53 ),
  ( sym: 275; act: 54 ),
  ( sym: 277; act: 55 ),
  ( sym: 278; act: 56 ),
  ( sym: 279; act: 57 ),
  ( sym: 280; act: 58 ),
  ( sym: 281; act: 59 ),
  ( sym: 282; act: 60 ),
  ( sym: 294; act: 96 ),
  ( sym: 292; act: -54 ),
{ 92: }
{ 93: }
{ 94: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 300; act: 9 ),
  ( sym: 301; act: 10 ),
{ 95: }
{ 96: }
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
  ( sym: 300; act: 35 ),
  ( sym: 302; act: 36 )
{ 97: }
{ 98: }
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
  ( sym: -17; act: 13 ),
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
  ( sym: -12; act: 37 ),
  ( sym: -9; act: 22 ),
{ 11: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 38 ),
  ( sym: -9; act: 22 ),
{ 12: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 39 ),
{ 13: }
{ 14: }
{ 15: }
  ( sym: -15; act: 42 ),
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
  ( sym: -12; act: 62 ),
  ( sym: -9; act: 22 ),
{ 26: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 63 ),
  ( sym: -9; act: 22 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 64 ),
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
{ 41: }
  ( sym: -8; act: 67 ),
{ 42: }
{ 43: }
{ 44: }
{ 45: }
{ 46: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 72 ),
  ( sym: -9; act: 22 ),
{ 47: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 73 ),
  ( sym: -9; act: 22 ),
{ 48: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 74 ),
  ( sym: -9; act: 22 ),
{ 49: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 75 ),
  ( sym: -9; act: 22 ),
{ 50: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 76 ),
  ( sym: -9; act: 22 ),
{ 51: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 77 ),
  ( sym: -9; act: 22 ),
{ 52: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 78 ),
  ( sym: -9; act: 22 ),
{ 53: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 79 ),
  ( sym: -9; act: 22 ),
{ 54: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 80 ),
  ( sym: -9; act: 22 ),
{ 55: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 81 ),
  ( sym: -9; act: 22 ),
{ 56: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 82 ),
  ( sym: -9; act: 22 ),
{ 57: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 83 ),
  ( sym: -9; act: 22 ),
{ 58: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 84 ),
  ( sym: -9; act: 22 ),
{ 59: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 85 ),
  ( sym: -9; act: 22 ),
{ 60: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 86 ),
  ( sym: -9; act: 22 ),
{ 61: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 87 ),
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: -18; act: 89 ),
  ( sym: -16; act: 90 ),
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 91 ),
  ( sym: -9; act: 22 ),
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
{ 86: }
{ 87: }
  ( sym: -5; act: 93 ),
{ 88: }
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 97 ),
{ 95: }
{ 96: }
  ( sym: -18; act: 98 ),
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 91 ),
  ( sym: -9; act: 22 )
{ 97: }
{ 98: }
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
{ 22: } -44,
{ 23: } -49,
{ 24: } -50,
{ 25: } 0,
{ 26: } 0,
{ 27: } -56,
{ 28: } -55,
{ 29: } -57,
{ 30: } -58,
{ 31: } 0,
{ 32: } -48,
{ 33: } -46,
{ 34: } -45,
{ 35: } 0,
{ 36: } -47,
{ 37: } 0,
{ 38: } 0,
{ 39: } -2,
{ 40: } 0,
{ 41: } 0,
{ 42: } -8,
{ 43: } -20,
{ 44: } -21,
{ 45: } -4,
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
{ 61: } 0,
{ 62: } -39,
{ 63: } -38,
{ 64: } 0,
{ 65: } 0,
{ 66: } -12,
{ 67: } -7,
{ 68: } -15,
{ 69: } -14,
{ 70: } -16,
{ 71: } -17,
{ 72: } 0,
{ 73: } 0,
{ 74: } -37,
{ 75: } -36,
{ 76: } -35,
{ 77: } -33,
{ 78: } 0,
{ 79: } 0,
{ 80: } -34,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } 0,
{ 85: } 0,
{ 86: } 0,
{ 87: } 0,
{ 88: } -41,
{ 89: } -51,
{ 90: } 0,
{ 91: } 0,
{ 92: } -43,
{ 93: } -5,
{ 94: } 0,
{ 95: } -42,
{ 96: } 0,
{ 97: } -10,
{ 98: } -53
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
{ 36: } 156,
{ 37: } 156,
{ 38: } 172,
{ 39: } 188,
{ 40: } 188,
{ 41: } 189,
{ 42: } 193,
{ 43: } 193,
{ 44: } 193,
{ 45: } 193,
{ 46: } 193,
{ 47: } 207,
{ 48: } 221,
{ 49: } 235,
{ 50: } 249,
{ 51: } 263,
{ 52: } 277,
{ 53: } 291,
{ 54: } 305,
{ 55: } 319,
{ 56: } 333,
{ 57: } 347,
{ 58: } 361,
{ 59: } 375,
{ 60: } 389,
{ 61: } 403,
{ 62: } 409,
{ 63: } 409,
{ 64: } 409,
{ 65: } 424,
{ 66: } 439,
{ 67: } 439,
{ 68: } 439,
{ 69: } 439,
{ 70: } 439,
{ 71: } 439,
{ 72: } 439,
{ 73: } 454,
{ 74: } 473,
{ 75: } 473,
{ 76: } 473,
{ 77: } 473,
{ 78: } 473,
{ 79: } 492,
{ 80: } 511,
{ 81: } 511,
{ 82: } 524,
{ 83: } 537,
{ 84: } 550,
{ 85: } 563,
{ 86: } 576,
{ 87: } 589,
{ 88: } 591,
{ 89: } 591,
{ 90: } 591,
{ 91: } 592,
{ 92: } 608,
{ 93: } 608,
{ 94: } 608,
{ 95: } 614,
{ 96: } 614,
{ 97: } 628,
{ 98: } 628
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
{ 35: } 155,
{ 36: } 155,
{ 37: } 171,
{ 38: } 187,
{ 39: } 187,
{ 40: } 188,
{ 41: } 192,
{ 42: } 192,
{ 43: } 192,
{ 44: } 192,
{ 45: } 192,
{ 46: } 206,
{ 47: } 220,
{ 48: } 234,
{ 49: } 248,
{ 50: } 262,
{ 51: } 276,
{ 52: } 290,
{ 53: } 304,
{ 54: } 318,
{ 55: } 332,
{ 56: } 346,
{ 57: } 360,
{ 58: } 374,
{ 59: } 388,
{ 60: } 402,
{ 61: } 408,
{ 62: } 408,
{ 63: } 408,
{ 64: } 423,
{ 65: } 438,
{ 66: } 438,
{ 67: } 438,
{ 68: } 438,
{ 69: } 438,
{ 70: } 438,
{ 71: } 438,
{ 72: } 453,
{ 73: } 472,
{ 74: } 472,
{ 75: } 472,
{ 76: } 472,
{ 77: } 472,
{ 78: } 491,
{ 79: } 510,
{ 80: } 510,
{ 81: } 523,
{ 82: } 536,
{ 83: } 549,
{ 84: } 562,
{ 85: } 575,
{ 86: } 588,
{ 87: } 590,
{ 88: } 590,
{ 89: } 590,
{ 90: } 591,
{ 91: } 607,
{ 92: } 607,
{ 93: } 607,
{ 94: } 613,
{ 95: } 613,
{ 96: } 627,
{ 97: } 627,
{ 98: } 627
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
{ 41: } 39,
{ 42: } 40,
{ 43: } 40,
{ 44: } 40,
{ 45: } 40,
{ 46: } 40,
{ 47: } 44,
{ 48: } 48,
{ 49: } 52,
{ 50: } 56,
{ 51: } 60,
{ 52: } 64,
{ 53: } 68,
{ 54: } 72,
{ 55: } 76,
{ 56: } 80,
{ 57: } 84,
{ 58: } 88,
{ 59: } 92,
{ 60: } 96,
{ 61: } 100,
{ 62: } 102,
{ 63: } 102,
{ 64: } 102,
{ 65: } 102,
{ 66: } 108,
{ 67: } 108,
{ 68: } 108,
{ 69: } 108,
{ 70: } 108,
{ 71: } 108,
{ 72: } 108,
{ 73: } 108,
{ 74: } 108,
{ 75: } 108,
{ 76: } 108,
{ 77: } 108,
{ 78: } 108,
{ 79: } 108,
{ 80: } 108,
{ 81: } 108,
{ 82: } 108,
{ 83: } 108,
{ 84: } 108,
{ 85: } 108,
{ 86: } 108,
{ 87: } 108,
{ 88: } 109,
{ 89: } 109,
{ 90: } 109,
{ 91: } 109,
{ 92: } 109,
{ 93: } 109,
{ 94: } 109,
{ 95: } 111,
{ 96: } 111,
{ 97: } 116,
{ 98: } 116
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
{ 40: } 38,
{ 41: } 39,
{ 42: } 39,
{ 43: } 39,
{ 44: } 39,
{ 45: } 39,
{ 46: } 43,
{ 47: } 47,
{ 48: } 51,
{ 49: } 55,
{ 50: } 59,
{ 51: } 63,
{ 52: } 67,
{ 53: } 71,
{ 54: } 75,
{ 55: } 79,
{ 56: } 83,
{ 57: } 87,
{ 58: } 91,
{ 59: } 95,
{ 60: } 99,
{ 61: } 101,
{ 62: } 101,
{ 63: } 101,
{ 64: } 101,
{ 65: } 107,
{ 66: } 107,
{ 67: } 107,
{ 68: } 107,
{ 69: } 107,
{ 70: } 107,
{ 71: } 107,
{ 72: } 107,
{ 73: } 107,
{ 74: } 107,
{ 75: } 107,
{ 76: } 107,
{ 77: } 107,
{ 78: } 107,
{ 79: } 107,
{ 80: } 107,
{ 81: } 107,
{ 82: } 107,
{ 83: } 107,
{ 84: } 107,
{ 85: } 107,
{ 86: } 107,
{ 87: } 108,
{ 88: } 108,
{ 89: } 108,
{ 90: } 108,
{ 91: } 108,
{ 92: } 108,
{ 93: } 108,
{ 94: } 110,
{ 95: } 110,
{ 96: } 115,
{ 97: } 115,
{ 98: } 115
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
{ 12: } ( len: 3; sym: -17 ),
{ 13: } ( len: 1; sym: -17 ),
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
{ 43: } ( len: 4; sym: -13 ),
{ 44: } ( len: 1; sym: -13 ),
{ 45: } ( len: 1; sym: -13 ),
{ 46: } ( len: 1; sym: -13 ),
{ 47: } ( len: 1; sym: -13 ),
{ 48: } ( len: 1; sym: -13 ),
{ 49: } ( len: 1; sym: -13 ),
{ 50: } ( len: 1; sym: -13 ),
{ 51: } ( len: 1; sym: -16 ),
{ 52: } ( len: 0; sym: -16 ),
{ 53: } ( len: 3; sym: -18 ),
{ 54: } ( len: 1; sym: -18 ),
{ 55: } ( len: 1; sym: -14 ),
{ 56: } ( len: 1; sym: -14 ),
{ 57: } ( len: 1; sym: -14 ),
{ 58: } ( len: 1; sym: -14 )
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
