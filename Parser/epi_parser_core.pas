
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
const OPTimeType = 291;
const OPOpenParan = 292;
const OPCloseParan = 293;
const OPSemicolon = 294;
const OPComma = 295;
const OPPeriod = 296;
const OPAssign = 297;
const OPColon = 298;
const OPFloatLiteral = 299;
const OPIntegerLiteral = 300;
const OPIdentifier = 301;
const OPWrite = 302;
const OPStringLiteral = 303;
const OPIllegal = 304;
const UMINUS = 305;


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
                 8 : ( yyTEpiFieldType : TEpiFieldType );
                 9 : ( yyTExpr : TExpr );
                10 : ( yyTGotoOption : TGotoOption );
                11 : ( yyTParamList : TParamList );
                12 : ( yyTParserOperationType : TParserOperationType );
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
         
         yyval.yyTCustomStatement := TDefine.Create(yyv[yysp-0].yyTEpiFieldType, IdentList, FParser);
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
         yyval.yyTEpiFieldType := ftInteger; 
       end;
  15 : begin
         yyval.yyTEpiFieldType := ftString; 
       end;
  16 : begin
         yyval.yyTEpiFieldType := ftFloat; 
       end;
  17 : begin
         yyval.yyTEpiFieldType := ftBoolean; 
       end;
  18 : begin
         yyval.yyTEpiFieldType := ftDMYDate; 
       end;
  19 : begin
         yyval.yyTEpiFieldType := ftTime; 
       end;
  20 : begin
         yyval.yyTCustomVariable := yyv[yysp-0].yyTCustomVariable; 
       end;
  21 : begin
         yyval.yyTCustomVariable := nil; 
       end;
  22 : begin
         yyval.yyTGotoOption := goClear; 
       end;
  23 : begin
         yyval.yyTGotoOption := goMissing; 
       end;
  24 : begin
         yyval.yyTGotoOption := goNoOpt; 
       end;
  25 : begin
         yyval.yyTCustomVariable := TCustomVariable.FindVariable(yyv[yysp-0].yyIdString, FParser); 
       end;
  26 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  27 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otNEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  28 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  29 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  30 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  31 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  32 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otPlus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  33 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMinus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  34 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otOr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  35 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMult, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  36 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDivide, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  37 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDiv, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  38 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMod, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  39 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otAnd, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  40 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otNot,  yyv[yysp-0].yyTExpr, nil); 
       end;
  41 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otMinus, yyv[yysp-0].yyTExpr, nil); 
       end;
  42 : begin
         yyval := yyv[yysp-0];
       end;
  43 : begin
         yyval.yyTExpr := yyv[yysp-1].yyTExpr; 
       end;
  44 : begin
         yyval.yyTExpr := TFunction.CreateFunction(yyv[yysp-3].yyIdString, yyv[yysp-1].yyTParamList, FParser); 
       end;
  45 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil); 
       end;
  46 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  47 : begin
         yyval.yyTExpr := TIntegerLiteral.Create(yyv[yysp-0].yyEpiInteger);  
       end;
  48 : begin
         yyval.yyTExpr := TFloatLiteral.Create(yyv[yysp-0].yyEpiFloat);  
       end;
  49 : begin
         
         									  if Assigned(yyv[yysp-0].yyPString) then
         									    yyval.yyTExpr := TStringLiteral.Create(yyv[yysp-0].yyPString^)
         									  else
         									    yyval.yyTExpr := TStringLiteral.Create('');
         									  DisposeStr(yyv[yysp-0].yyPString);
         									
       end;
  50 : begin
         yyval.yyTExpr := TMissingLiteral.Create; 
       end;
  51 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(true); 
       end;
  52 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(false); 
       end;
  53 : begin
         yyval.yyTParamList := TParamList.Create(ParamList); FreeAndNil(ParamList); 
       end;
  54 : begin
         yyval.yyTParamList := TParamList.Create(nil); 
       end;
  55 : begin
         AddToParamList(yyv[yysp-0].yyTExpr); 
       end;
  56 : begin
         AddToParamList(yyv[yysp-0].yyTExpr); 
       end;
  57 : begin
         yyval.yyTParserOperationType := otIntegerCast; 
       end;
  58 : begin
         yyval.yyTParserOperationType := otStringCast; 
       end;
  59 : begin
         yyval.yyTParserOperationType := otFloatCast; 
       end;
  60 : begin
         yyval.yyTParserOperationType := otBoolCast; 
       end;
  61 : begin
         yyval.yyTParserOperationType := otIntegerCast; 
       end;
  62 : begin
         yyval.yyTParserOperationType := otFloatCast; 
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

yynacts   = 693;
yyngotos  = 114;
yynstates = 103;
yynrules  = 62;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 301; act: 9 ),
  ( sym: 302; act: 10 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 297; act: 11 ),
{ 2: }
  ( sym: 294; act: 12 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
{ 5: }
  ( sym: 301; act: 14 ),
{ 6: }
  ( sym: 301; act: 9 ),
  ( sym: 302; act: 17 ),
{ 7: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 301; act: 9 ),
  ( sym: 302; act: 10 ),
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
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
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
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 11: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 12: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 301; act: 9 ),
  ( sym: 302; act: 10 ),
  ( sym: 0; act: -3 ),
  ( sym: 265; act: -3 ),
{ 13: }
  ( sym: 295; act: 42 ),
  ( sym: 298; act: 43 ),
{ 14: }
{ 15: }
  ( sym: 262; act: 45 ),
  ( sym: 263; act: 46 ),
  ( sym: 285; act: -24 ),
  ( sym: 294; act: -24 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: 265; act: 47 ),
{ 19: }
  ( sym: 292; act: 48 ),
{ 20: }
{ 21: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 284; act: 63 ),
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
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 26: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 34: }
{ 35: }
{ 36: }
{ 37: }
  ( sym: 292; act: 67 ),
  ( sym: 268; act: -25 ),
  ( sym: 269; act: -25 ),
  ( sym: 270; act: -25 ),
  ( sym: 271; act: -25 ),
  ( sym: 272; act: -25 ),
  ( sym: 273; act: -25 ),
  ( sym: 274; act: -25 ),
  ( sym: 275; act: -25 ),
  ( sym: 277; act: -25 ),
  ( sym: 278; act: -25 ),
  ( sym: 279; act: -25 ),
  ( sym: 280; act: -25 ),
  ( sym: 281; act: -25 ),
  ( sym: 282; act: -25 ),
  ( sym: 284; act: -25 ),
  ( sym: 285; act: -25 ),
  ( sym: 293; act: -25 ),
  ( sym: 294; act: -25 ),
  ( sym: 295; act: -25 ),
{ 38: }
{ 39: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 285; act: -9 ),
  ( sym: 294; act: -9 ),
{ 40: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 285; act: -6 ),
  ( sym: 294; act: -6 ),
{ 41: }
{ 42: }
  ( sym: 301; act: 68 ),
{ 43: }
  ( sym: 286; act: 70 ),
  ( sym: 287; act: 71 ),
  ( sym: 288; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 291; act: 75 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 49: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 50: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 51: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 52: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 53: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 54: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 55: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 56: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 57: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 58: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 59: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 60: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 61: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 62: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 63: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 301; act: 9 ),
  ( sym: 302; act: 10 ),
{ 64: }
{ 65: }
{ 66: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 293; act: 92 ),
{ 67: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
  ( sym: 293; act: -54 ),
{ 68: }
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 293; act: 96 ),
{ 77: }
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 275; act: 56 ),
  ( sym: 268; act: -34 ),
  ( sym: 273; act: -34 ),
  ( sym: 274; act: -34 ),
  ( sym: 277; act: -34 ),
  ( sym: 278; act: -34 ),
  ( sym: 279; act: -34 ),
  ( sym: 280; act: -34 ),
  ( sym: 281; act: -34 ),
  ( sym: 282; act: -34 ),
  ( sym: 284; act: -34 ),
  ( sym: 285; act: -34 ),
  ( sym: 293; act: -34 ),
  ( sym: 294; act: -34 ),
  ( sym: 295; act: -34 ),
{ 78: }
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 275; act: 56 ),
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
  ( sym: 293; act: -32 ),
  ( sym: 294; act: -32 ),
  ( sym: 295; act: -32 ),
{ 83: }
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 275; act: 56 ),
  ( sym: 268; act: -33 ),
  ( sym: 273; act: -33 ),
  ( sym: 274; act: -33 ),
  ( sym: 277; act: -33 ),
  ( sym: 278; act: -33 ),
  ( sym: 279; act: -33 ),
  ( sym: 280; act: -33 ),
  ( sym: 281; act: -33 ),
  ( sym: 282; act: -33 ),
  ( sym: 284; act: -33 ),
  ( sym: 285; act: -33 ),
  ( sym: 293; act: -33 ),
  ( sym: 294; act: -33 ),
  ( sym: 295; act: -33 ),
{ 84: }
{ 85: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 284; act: -26 ),
  ( sym: 285; act: -26 ),
  ( sym: 293; act: -26 ),
  ( sym: 294; act: -26 ),
  ( sym: 295; act: -26 ),
{ 86: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 284; act: -27 ),
  ( sym: 285; act: -27 ),
  ( sym: 293; act: -27 ),
  ( sym: 294; act: -27 ),
  ( sym: 295; act: -27 ),
{ 87: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 284; act: -28 ),
  ( sym: 285; act: -28 ),
  ( sym: 293; act: -28 ),
  ( sym: 294; act: -28 ),
  ( sym: 295; act: -28 ),
{ 88: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 284; act: -30 ),
  ( sym: 285; act: -30 ),
  ( sym: 293; act: -30 ),
  ( sym: 294; act: -30 ),
  ( sym: 295; act: -30 ),
{ 89: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 284; act: -29 ),
  ( sym: 285; act: -29 ),
  ( sym: 293; act: -29 ),
  ( sym: 294; act: -29 ),
  ( sym: 295; act: -29 ),
{ 90: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 284; act: -31 ),
  ( sym: 285; act: -31 ),
  ( sym: 293; act: -31 ),
  ( sym: 294; act: -31 ),
  ( sym: 295; act: -31 ),
{ 91: }
  ( sym: 285; act: 98 ),
  ( sym: 294; act: -11 ),
{ 92: }
{ 93: }
  ( sym: 295; act: 99 ),
  ( sym: 293; act: -53 ),
{ 94: }
  ( sym: 293; act: 100 ),
{ 95: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 293; act: -56 ),
  ( sym: 295; act: -56 ),
{ 96: }
{ 97: }
{ 98: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 283; act: 8 ),
  ( sym: 301; act: 9 ),
  ( sym: 302; act: 10 ),
{ 99: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 286; act: 27 ),
  ( sym: 287; act: 28 ),
  ( sym: 288; act: 29 ),
  ( sym: 289; act: 30 ),
  ( sym: 290; act: 31 ),
  ( sym: 291; act: 32 ),
  ( sym: 292; act: 33 ),
  ( sym: 296; act: 34 ),
  ( sym: 299; act: 35 ),
  ( sym: 300; act: 36 ),
  ( sym: 301; act: 37 ),
  ( sym: 303; act: 38 ),
{ 100: }
{ 101: }
{ 102: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 277; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 293; act: -55 ),
  ( sym: 295; act: -55 )
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
  ( sym: -12; act: 39 ),
  ( sym: -9; act: 22 ),
{ 11: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 40 ),
  ( sym: -9; act: 22 ),
{ 12: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 41 ),
{ 13: }
{ 14: }
{ 15: }
  ( sym: -15; act: 44 ),
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
  ( sym: -12; act: 64 ),
  ( sym: -9; act: 22 ),
{ 26: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 65 ),
  ( sym: -9; act: 22 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 66 ),
  ( sym: -9; act: 22 ),
{ 34: }
{ 35: }
{ 36: }
{ 37: }
{ 38: }
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
  ( sym: -8; act: 69 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 76 ),
  ( sym: -9; act: 22 ),
{ 49: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 77 ),
  ( sym: -9; act: 22 ),
{ 50: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 78 ),
  ( sym: -9; act: 22 ),
{ 51: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 79 ),
  ( sym: -9; act: 22 ),
{ 52: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 80 ),
  ( sym: -9; act: 22 ),
{ 53: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 81 ),
  ( sym: -9; act: 22 ),
{ 54: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 82 ),
  ( sym: -9; act: 22 ),
{ 55: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 83 ),
  ( sym: -9; act: 22 ),
{ 56: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 84 ),
  ( sym: -9; act: 22 ),
{ 57: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 85 ),
  ( sym: -9; act: 22 ),
{ 58: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 86 ),
  ( sym: -9; act: 22 ),
{ 59: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 87 ),
  ( sym: -9; act: 22 ),
{ 60: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 88 ),
  ( sym: -9; act: 22 ),
{ 61: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 89 ),
  ( sym: -9; act: 22 ),
{ 62: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 90 ),
  ( sym: -9; act: 22 ),
{ 63: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 91 ),
{ 64: }
{ 65: }
{ 66: }
{ 67: }
  ( sym: -18; act: 93 ),
  ( sym: -16; act: 94 ),
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 95 ),
  ( sym: -9; act: 22 ),
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
{ 88: }
{ 89: }
{ 90: }
{ 91: }
  ( sym: -5; act: 97 ),
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 101 ),
{ 99: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 102 ),
  ( sym: -9; act: 22 )
{ 100: }
{ 101: }
{ 102: }
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
{ 9: } -25,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } 0,
{ 14: } -13,
{ 15: } 0,
{ 16: } -20,
{ 17: } -21,
{ 18: } 0,
{ 19: } 0,
{ 20: } -42,
{ 21: } 0,
{ 22: } -46,
{ 23: } -51,
{ 24: } -52,
{ 25: } 0,
{ 26: } 0,
{ 27: } -58,
{ 28: } -57,
{ 29: } -59,
{ 30: } -60,
{ 31: } -61,
{ 32: } -62,
{ 33: } 0,
{ 34: } -50,
{ 35: } -48,
{ 36: } -47,
{ 37: } 0,
{ 38: } -49,
{ 39: } 0,
{ 40: } 0,
{ 41: } -2,
{ 42: } 0,
{ 43: } 0,
{ 44: } -8,
{ 45: } -22,
{ 46: } -23,
{ 47: } -4,
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
{ 62: } 0,
{ 63: } 0,
{ 64: } -41,
{ 65: } -40,
{ 66: } 0,
{ 67: } 0,
{ 68: } -12,
{ 69: } -7,
{ 70: } -15,
{ 71: } -14,
{ 72: } -16,
{ 73: } -17,
{ 74: } -18,
{ 75: } -19,
{ 76: } 0,
{ 77: } 0,
{ 78: } -39,
{ 79: } -38,
{ 80: } -37,
{ 81: } -35,
{ 82: } 0,
{ 83: } 0,
{ 84: } -36,
{ 85: } 0,
{ 86: } 0,
{ 87: } 0,
{ 88: } 0,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } -43,
{ 93: } 0,
{ 94: } 0,
{ 95: } 0,
{ 96: } -45,
{ 97: } -5,
{ 98: } 0,
{ 99: } 0,
{ 100: } -44,
{ 101: } -10,
{ 102: } 0
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
{ 9: } 37,
{ 10: } 37,
{ 11: } 53,
{ 12: } 69,
{ 13: } 77,
{ 14: } 79,
{ 15: } 79,
{ 16: } 83,
{ 17: } 83,
{ 18: } 83,
{ 19: } 84,
{ 20: } 85,
{ 21: } 85,
{ 22: } 100,
{ 23: } 100,
{ 24: } 100,
{ 25: } 100,
{ 26: } 116,
{ 27: } 132,
{ 28: } 132,
{ 29: } 132,
{ 30: } 132,
{ 31: } 132,
{ 32: } 132,
{ 33: } 132,
{ 34: } 148,
{ 35: } 148,
{ 36: } 148,
{ 37: } 148,
{ 38: } 168,
{ 39: } 168,
{ 40: } 184,
{ 41: } 200,
{ 42: } 200,
{ 43: } 201,
{ 44: } 207,
{ 45: } 207,
{ 46: } 207,
{ 47: } 207,
{ 48: } 207,
{ 49: } 223,
{ 50: } 239,
{ 51: } 255,
{ 52: } 271,
{ 53: } 287,
{ 54: } 303,
{ 55: } 319,
{ 56: } 335,
{ 57: } 351,
{ 58: } 367,
{ 59: } 383,
{ 60: } 399,
{ 61: } 415,
{ 62: } 431,
{ 63: } 447,
{ 64: } 453,
{ 65: } 453,
{ 66: } 453,
{ 67: } 468,
{ 68: } 485,
{ 69: } 485,
{ 70: } 485,
{ 71: } 485,
{ 72: } 485,
{ 73: } 485,
{ 74: } 485,
{ 75: } 485,
{ 76: } 485,
{ 77: } 500,
{ 78: } 519,
{ 79: } 519,
{ 80: } 519,
{ 81: } 519,
{ 82: } 519,
{ 83: } 538,
{ 84: } 557,
{ 85: } 557,
{ 86: } 570,
{ 87: } 583,
{ 88: } 596,
{ 89: } 609,
{ 90: } 622,
{ 91: } 635,
{ 92: } 637,
{ 93: } 637,
{ 94: } 639,
{ 95: } 640,
{ 96: } 656,
{ 97: } 656,
{ 98: } 656,
{ 99: } 662,
{ 100: } 678,
{ 101: } 678,
{ 102: } 678
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
{ 8: } 36,
{ 9: } 36,
{ 10: } 52,
{ 11: } 68,
{ 12: } 76,
{ 13: } 78,
{ 14: } 78,
{ 15: } 82,
{ 16: } 82,
{ 17: } 82,
{ 18: } 83,
{ 19: } 84,
{ 20: } 84,
{ 21: } 99,
{ 22: } 99,
{ 23: } 99,
{ 24: } 99,
{ 25: } 115,
{ 26: } 131,
{ 27: } 131,
{ 28: } 131,
{ 29: } 131,
{ 30: } 131,
{ 31: } 131,
{ 32: } 131,
{ 33: } 147,
{ 34: } 147,
{ 35: } 147,
{ 36: } 147,
{ 37: } 167,
{ 38: } 167,
{ 39: } 183,
{ 40: } 199,
{ 41: } 199,
{ 42: } 200,
{ 43: } 206,
{ 44: } 206,
{ 45: } 206,
{ 46: } 206,
{ 47: } 206,
{ 48: } 222,
{ 49: } 238,
{ 50: } 254,
{ 51: } 270,
{ 52: } 286,
{ 53: } 302,
{ 54: } 318,
{ 55: } 334,
{ 56: } 350,
{ 57: } 366,
{ 58: } 382,
{ 59: } 398,
{ 60: } 414,
{ 61: } 430,
{ 62: } 446,
{ 63: } 452,
{ 64: } 452,
{ 65: } 452,
{ 66: } 467,
{ 67: } 484,
{ 68: } 484,
{ 69: } 484,
{ 70: } 484,
{ 71: } 484,
{ 72: } 484,
{ 73: } 484,
{ 74: } 484,
{ 75: } 484,
{ 76: } 499,
{ 77: } 518,
{ 78: } 518,
{ 79: } 518,
{ 80: } 518,
{ 81: } 518,
{ 82: } 537,
{ 83: } 556,
{ 84: } 556,
{ 85: } 569,
{ 86: } 582,
{ 87: } 595,
{ 88: } 608,
{ 89: } 621,
{ 90: } 634,
{ 91: } 636,
{ 92: } 636,
{ 93: } 638,
{ 94: } 639,
{ 95: } 655,
{ 96: } 655,
{ 97: } 655,
{ 98: } 661,
{ 99: } 677,
{ 100: } 677,
{ 101: } 677,
{ 102: } 693
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
{ 32: } 35,
{ 33: } 35,
{ 34: } 39,
{ 35: } 39,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 39,
{ 40: } 39,
{ 41: } 39,
{ 42: } 39,
{ 43: } 39,
{ 44: } 40,
{ 45: } 40,
{ 46: } 40,
{ 47: } 40,
{ 48: } 40,
{ 49: } 44,
{ 50: } 48,
{ 51: } 52,
{ 52: } 56,
{ 53: } 60,
{ 54: } 64,
{ 55: } 68,
{ 56: } 72,
{ 57: } 76,
{ 58: } 80,
{ 59: } 84,
{ 60: } 88,
{ 61: } 92,
{ 62: } 96,
{ 63: } 100,
{ 64: } 102,
{ 65: } 102,
{ 66: } 102,
{ 67: } 102,
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
{ 88: } 108,
{ 89: } 108,
{ 90: } 108,
{ 91: } 108,
{ 92: } 109,
{ 93: } 109,
{ 94: } 109,
{ 95: } 109,
{ 96: } 109,
{ 97: } 109,
{ 98: } 109,
{ 99: } 111,
{ 100: } 115,
{ 101: } 115,
{ 102: } 115
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
{ 31: } 34,
{ 32: } 34,
{ 33: } 38,
{ 34: } 38,
{ 35: } 38,
{ 36: } 38,
{ 37: } 38,
{ 38: } 38,
{ 39: } 38,
{ 40: } 38,
{ 41: } 38,
{ 42: } 38,
{ 43: } 39,
{ 44: } 39,
{ 45: } 39,
{ 46: } 39,
{ 47: } 39,
{ 48: } 43,
{ 49: } 47,
{ 50: } 51,
{ 51: } 55,
{ 52: } 59,
{ 53: } 63,
{ 54: } 67,
{ 55: } 71,
{ 56: } 75,
{ 57: } 79,
{ 58: } 83,
{ 59: } 87,
{ 60: } 91,
{ 61: } 95,
{ 62: } 99,
{ 63: } 101,
{ 64: } 101,
{ 65: } 101,
{ 66: } 101,
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
{ 87: } 107,
{ 88: } 107,
{ 89: } 107,
{ 90: } 107,
{ 91: } 108,
{ 92: } 108,
{ 93: } 108,
{ 94: } 108,
{ 95: } 108,
{ 96: } 108,
{ 97: } 108,
{ 98: } 110,
{ 99: } 114,
{ 100: } 114,
{ 101: } 114,
{ 102: } 114
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
{ 18: } ( len: 1; sym: -8 ),
{ 19: } ( len: 1; sym: -8 ),
{ 20: } ( len: 1; sym: -10 ),
{ 21: } ( len: 1; sym: -10 ),
{ 22: } ( len: 1; sym: -15 ),
{ 23: } ( len: 1; sym: -15 ),
{ 24: } ( len: 0; sym: -15 ),
{ 25: } ( len: 1; sym: -9 ),
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
{ 38: } ( len: 3; sym: -12 ),
{ 39: } ( len: 3; sym: -12 ),
{ 40: } ( len: 2; sym: -12 ),
{ 41: } ( len: 2; sym: -12 ),
{ 42: } ( len: 1; sym: -12 ),
{ 43: } ( len: 3; sym: -13 ),
{ 44: } ( len: 4; sym: -13 ),
{ 45: } ( len: 4; sym: -13 ),
{ 46: } ( len: 1; sym: -13 ),
{ 47: } ( len: 1; sym: -13 ),
{ 48: } ( len: 1; sym: -13 ),
{ 49: } ( len: 1; sym: -13 ),
{ 50: } ( len: 1; sym: -13 ),
{ 51: } ( len: 1; sym: -13 ),
{ 52: } ( len: 1; sym: -13 ),
{ 53: } ( len: 1; sym: -16 ),
{ 54: } ( len: 0; sym: -16 ),
{ 55: } ( len: 3; sym: -18 ),
{ 56: } ( len: 1; sym: -18 ),
{ 57: } ( len: 1; sym: -14 ),
{ 58: } ( len: 1; sym: -14 ),
{ 59: } ( len: 1; sym: -14 ),
{ 60: } ( len: 1; sym: -14 ),
{ 61: } ( len: 1; sym: -14 ),
{ 62: } ( len: 1; sym: -14 )
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