
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
const OPExponential = 276;
const OPNot = 277;
const OPEQ = 278;
const OPNEQ = 279;
const OPLT = 280;
const OPLTE = 281;
const OPGT = 282;
const OPGTE = 283;
const OPIf = 284;
const OPThen = 285;
const OPElse = 286;
const OPStringType = 287;
const OPIntegerType = 288;
const OPFloatType = 289;
const OPBooleanType = 290;
const OPDateType = 291;
const OPTimeType = 292;
const OPOpenParan = 293;
const OPCloseParan = 294;
const OPSemicolon = 295;
const OPComma = 296;
const OPPeriod = 297;
const OPAssign = 298;
const OPColon = 299;
const OPFloatLiteral = 300;
const OPIntegerLiteral = 301;
const OPIdentifier = 302;
const OPWrite = 303;
const OPStringLiteral = 304;
const OPIllegal = 305;
const UMINUS = 306;


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
         yyval.yyTExpr := TBinaryExpr.Create(otExponential, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  41 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otNot,  yyv[yysp-0].yyTExpr, nil); 
       end;
  42 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otMinus, yyv[yysp-0].yyTExpr, nil); 
       end;
  43 : begin
         yyval := yyv[yysp-0];
       end;
  44 : begin
         yyval.yyTExpr := yyv[yysp-1].yyTExpr; 
       end;
  45 : begin
         yyval.yyTExpr := TFunction.CreateFunction(yyv[yysp-3].yyIdString, yyv[yysp-1].yyTParamList, FParser); 
       end;
  46 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil); 
       end;
  47 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  48 : begin
         yyval.yyTExpr := TIntegerLiteral.Create(yyv[yysp-0].yyEpiInteger);  
       end;
  49 : begin
         yyval.yyTExpr := TFloatLiteral.Create(yyv[yysp-0].yyEpiFloat);  
       end;
  50 : begin
         
         									  if Assigned(yyv[yysp-0].yyPString) then
         									    yyval.yyTExpr := TStringLiteral.Create(yyv[yysp-0].yyPString^)
         									  else
         									    yyval.yyTExpr := TStringLiteral.Create('');
         									  DisposeStr(yyv[yysp-0].yyPString);
         									
       end;
  51 : begin
         yyval.yyTExpr := TMissingLiteral.Create; 
       end;
  52 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(true); 
       end;
  53 : begin
         yyval.yyTExpr := TBooleanLiteral.Create(false); 
       end;
  54 : begin
         yyval.yyTParamList := TParamList.Create(ParamList); FreeAndNil(ParamList); 
       end;
  55 : begin
         yyval.yyTParamList := TParamList.Create(nil); 
       end;
  56 : begin
         AddToParamList(yyv[yysp-0].yyTExpr); 
       end;
  57 : begin
         AddToParamList(yyv[yysp-0].yyTExpr); 
       end;
  58 : begin
         yyval.yyTParserOperationType := otIntegerCast; 
       end;
  59 : begin
         yyval.yyTParserOperationType := otStringCast; 
       end;
  60 : begin
         yyval.yyTParserOperationType := otFloatCast; 
       end;
  61 : begin
         yyval.yyTParserOperationType := otBoolCast; 
       end;
  62 : begin
         yyval.yyTParserOperationType := otIntegerCast; 
       end;
  63 : begin
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

yynacts   = 826;
yyngotos  = 118;
yynstates = 105;
yynrules  = 63;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 284; act: 8 ),
  ( sym: 302; act: 9 ),
  ( sym: 303; act: 10 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 298; act: 11 ),
{ 2: }
  ( sym: 295; act: 12 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
{ 5: }
  ( sym: 302; act: 14 ),
{ 6: }
  ( sym: 302; act: 9 ),
  ( sym: 303; act: 17 ),
{ 7: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 284; act: 8 ),
  ( sym: 302; act: 9 ),
  ( sym: 303; act: 10 ),
  ( sym: 265; act: -3 ),
{ 8: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 9: }
{ 10: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 11: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 12: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 284; act: 8 ),
  ( sym: 302; act: 9 ),
  ( sym: 303; act: 10 ),
  ( sym: 0; act: -3 ),
  ( sym: 265; act: -3 ),
{ 13: }
  ( sym: 296; act: 42 ),
  ( sym: 299; act: 43 ),
{ 14: }
{ 15: }
  ( sym: 262; act: 45 ),
  ( sym: 263; act: 46 ),
  ( sym: 286; act: -24 ),
  ( sym: 295; act: -24 ),
{ 16: }
{ 17: }
{ 18: }
  ( sym: 265; act: 47 ),
{ 19: }
  ( sym: 293; act: 48 ),
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
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 285; act: 64 ),
{ 22: }
{ 23: }
{ 24: }
{ 25: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 26: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
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
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 34: }
{ 35: }
{ 36: }
{ 37: }
  ( sym: 293; act: 68 ),
  ( sym: 268; act: -25 ),
  ( sym: 269; act: -25 ),
  ( sym: 270; act: -25 ),
  ( sym: 271; act: -25 ),
  ( sym: 272; act: -25 ),
  ( sym: 273; act: -25 ),
  ( sym: 274; act: -25 ),
  ( sym: 275; act: -25 ),
  ( sym: 276; act: -25 ),
  ( sym: 278; act: -25 ),
  ( sym: 279; act: -25 ),
  ( sym: 280; act: -25 ),
  ( sym: 281; act: -25 ),
  ( sym: 282; act: -25 ),
  ( sym: 283; act: -25 ),
  ( sym: 285; act: -25 ),
  ( sym: 286; act: -25 ),
  ( sym: 294; act: -25 ),
  ( sym: 295; act: -25 ),
  ( sym: 296; act: -25 ),
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
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 286; act: -9 ),
  ( sym: 295; act: -9 ),
{ 40: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 286; act: -6 ),
  ( sym: 295; act: -6 ),
{ 41: }
{ 42: }
  ( sym: 302; act: 69 ),
{ 43: }
  ( sym: 287; act: 71 ),
  ( sym: 288; act: 72 ),
  ( sym: 289; act: 73 ),
  ( sym: 290; act: 74 ),
  ( sym: 291; act: 75 ),
  ( sym: 292; act: 76 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 49: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 50: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 51: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 52: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 53: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 54: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 55: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 56: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 57: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 58: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 59: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 60: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 61: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 62: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 63: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 64: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 284; act: 8 ),
  ( sym: 302; act: 9 ),
  ( sym: 303; act: 10 ),
{ 65: }
{ 66: }
{ 67: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 294; act: 94 ),
{ 68: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
  ( sym: 294; act: -55 ),
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 294; act: 98 ),
{ 78: }
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -34 ),
  ( sym: 273; act: -34 ),
  ( sym: 274; act: -34 ),
  ( sym: 278; act: -34 ),
  ( sym: 279; act: -34 ),
  ( sym: 280; act: -34 ),
  ( sym: 281; act: -34 ),
  ( sym: 282; act: -34 ),
  ( sym: 283; act: -34 ),
  ( sym: 285; act: -34 ),
  ( sym: 286; act: -34 ),
  ( sym: 294; act: -34 ),
  ( sym: 295; act: -34 ),
  ( sym: 296; act: -34 ),
{ 79: }
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -39 ),
  ( sym: 269; act: -39 ),
  ( sym: 270; act: -39 ),
  ( sym: 271; act: -39 ),
  ( sym: 272; act: -39 ),
  ( sym: 273; act: -39 ),
  ( sym: 274; act: -39 ),
  ( sym: 275; act: -39 ),
  ( sym: 278; act: -39 ),
  ( sym: 279; act: -39 ),
  ( sym: 280; act: -39 ),
  ( sym: 281; act: -39 ),
  ( sym: 282; act: -39 ),
  ( sym: 283; act: -39 ),
  ( sym: 285; act: -39 ),
  ( sym: 286; act: -39 ),
  ( sym: 294; act: -39 ),
  ( sym: 295; act: -39 ),
  ( sym: 296; act: -39 ),
{ 80: }
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -38 ),
  ( sym: 269; act: -38 ),
  ( sym: 270; act: -38 ),
  ( sym: 271; act: -38 ),
  ( sym: 272; act: -38 ),
  ( sym: 273; act: -38 ),
  ( sym: 274; act: -38 ),
  ( sym: 275; act: -38 ),
  ( sym: 278; act: -38 ),
  ( sym: 279; act: -38 ),
  ( sym: 280; act: -38 ),
  ( sym: 281; act: -38 ),
  ( sym: 282; act: -38 ),
  ( sym: 283; act: -38 ),
  ( sym: 285; act: -38 ),
  ( sym: 286; act: -38 ),
  ( sym: 294; act: -38 ),
  ( sym: 295; act: -38 ),
  ( sym: 296; act: -38 ),
{ 81: }
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -37 ),
  ( sym: 269; act: -37 ),
  ( sym: 270; act: -37 ),
  ( sym: 271; act: -37 ),
  ( sym: 272; act: -37 ),
  ( sym: 273; act: -37 ),
  ( sym: 274; act: -37 ),
  ( sym: 275; act: -37 ),
  ( sym: 278; act: -37 ),
  ( sym: 279; act: -37 ),
  ( sym: 280; act: -37 ),
  ( sym: 281; act: -37 ),
  ( sym: 282; act: -37 ),
  ( sym: 283; act: -37 ),
  ( sym: 285; act: -37 ),
  ( sym: 286; act: -37 ),
  ( sym: 294; act: -37 ),
  ( sym: 295; act: -37 ),
  ( sym: 296; act: -37 ),
{ 82: }
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -35 ),
  ( sym: 269; act: -35 ),
  ( sym: 270; act: -35 ),
  ( sym: 271; act: -35 ),
  ( sym: 272; act: -35 ),
  ( sym: 273; act: -35 ),
  ( sym: 274; act: -35 ),
  ( sym: 275; act: -35 ),
  ( sym: 278; act: -35 ),
  ( sym: 279; act: -35 ),
  ( sym: 280; act: -35 ),
  ( sym: 281; act: -35 ),
  ( sym: 282; act: -35 ),
  ( sym: 283; act: -35 ),
  ( sym: 285; act: -35 ),
  ( sym: 286; act: -35 ),
  ( sym: 294; act: -35 ),
  ( sym: 295; act: -35 ),
  ( sym: 296; act: -35 ),
{ 83: }
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -32 ),
  ( sym: 273; act: -32 ),
  ( sym: 274; act: -32 ),
  ( sym: 278; act: -32 ),
  ( sym: 279; act: -32 ),
  ( sym: 280; act: -32 ),
  ( sym: 281; act: -32 ),
  ( sym: 282; act: -32 ),
  ( sym: 283; act: -32 ),
  ( sym: 285; act: -32 ),
  ( sym: 286; act: -32 ),
  ( sym: 294; act: -32 ),
  ( sym: 295; act: -32 ),
  ( sym: 296; act: -32 ),
{ 84: }
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -33 ),
  ( sym: 273; act: -33 ),
  ( sym: 274; act: -33 ),
  ( sym: 278; act: -33 ),
  ( sym: 279; act: -33 ),
  ( sym: 280; act: -33 ),
  ( sym: 281; act: -33 ),
  ( sym: 282; act: -33 ),
  ( sym: 283; act: -33 ),
  ( sym: 285; act: -33 ),
  ( sym: 286; act: -33 ),
  ( sym: 294; act: -33 ),
  ( sym: 295; act: -33 ),
  ( sym: 296; act: -33 ),
{ 85: }
  ( sym: 276; act: 57 ),
  ( sym: 268; act: -36 ),
  ( sym: 269; act: -36 ),
  ( sym: 270; act: -36 ),
  ( sym: 271; act: -36 ),
  ( sym: 272; act: -36 ),
  ( sym: 273; act: -36 ),
  ( sym: 274; act: -36 ),
  ( sym: 275; act: -36 ),
  ( sym: 278; act: -36 ),
  ( sym: 279; act: -36 ),
  ( sym: 280; act: -36 ),
  ( sym: 281; act: -36 ),
  ( sym: 282; act: -36 ),
  ( sym: 283; act: -36 ),
  ( sym: 285; act: -36 ),
  ( sym: 286; act: -36 ),
  ( sym: 294; act: -36 ),
  ( sym: 295; act: -36 ),
  ( sym: 296; act: -36 ),
{ 86: }
{ 87: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 285; act: -26 ),
  ( sym: 286; act: -26 ),
  ( sym: 294; act: -26 ),
  ( sym: 295; act: -26 ),
  ( sym: 296; act: -26 ),
{ 88: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 285; act: -27 ),
  ( sym: 286; act: -27 ),
  ( sym: 294; act: -27 ),
  ( sym: 295; act: -27 ),
  ( sym: 296; act: -27 ),
{ 89: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 285; act: -28 ),
  ( sym: 286; act: -28 ),
  ( sym: 294; act: -28 ),
  ( sym: 295; act: -28 ),
  ( sym: 296; act: -28 ),
{ 90: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 285; act: -30 ),
  ( sym: 286; act: -30 ),
  ( sym: 294; act: -30 ),
  ( sym: 295; act: -30 ),
  ( sym: 296; act: -30 ),
{ 91: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 285; act: -29 ),
  ( sym: 286; act: -29 ),
  ( sym: 294; act: -29 ),
  ( sym: 295; act: -29 ),
  ( sym: 296; act: -29 ),
{ 92: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 285; act: -31 ),
  ( sym: 286; act: -31 ),
  ( sym: 294; act: -31 ),
  ( sym: 295; act: -31 ),
  ( sym: 296; act: -31 ),
{ 93: }
  ( sym: 286; act: 100 ),
  ( sym: 295; act: -11 ),
{ 94: }
{ 95: }
  ( sym: 296; act: 101 ),
  ( sym: 294; act: -54 ),
{ 96: }
  ( sym: 294; act: 102 ),
{ 97: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 294; act: -57 ),
  ( sym: 296; act: -57 ),
{ 98: }
{ 99: }
{ 100: }
  ( sym: 257; act: 5 ),
  ( sym: 261; act: 6 ),
  ( sym: 264; act: 7 ),
  ( sym: 284; act: 8 ),
  ( sym: 302; act: 9 ),
  ( sym: 303; act: 10 ),
{ 101: }
  ( sym: 266; act: 23 ),
  ( sym: 267; act: 24 ),
  ( sym: 274; act: 25 ),
  ( sym: 277; act: 26 ),
  ( sym: 287; act: 27 ),
  ( sym: 288; act: 28 ),
  ( sym: 289; act: 29 ),
  ( sym: 290; act: 30 ),
  ( sym: 291; act: 31 ),
  ( sym: 292; act: 32 ),
  ( sym: 293; act: 33 ),
  ( sym: 297; act: 34 ),
  ( sym: 300; act: 35 ),
  ( sym: 301; act: 36 ),
  ( sym: 302; act: 37 ),
  ( sym: 304; act: 38 ),
{ 102: }
{ 103: }
{ 104: }
  ( sym: 268; act: 49 ),
  ( sym: 269; act: 50 ),
  ( sym: 270; act: 51 ),
  ( sym: 271; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 273; act: 54 ),
  ( sym: 274; act: 55 ),
  ( sym: 275; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 278; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 280; act: 60 ),
  ( sym: 281; act: 61 ),
  ( sym: 282; act: 62 ),
  ( sym: 283; act: 63 ),
  ( sym: 294; act: -56 ),
  ( sym: 296; act: -56 )
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
  ( sym: -12; act: 65 ),
  ( sym: -9; act: 22 ),
{ 26: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 66 ),
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
  ( sym: -12; act: 67 ),
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
  ( sym: -8; act: 70 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 77 ),
  ( sym: -9; act: 22 ),
{ 49: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 78 ),
  ( sym: -9; act: 22 ),
{ 50: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 79 ),
  ( sym: -9; act: 22 ),
{ 51: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 80 ),
  ( sym: -9; act: 22 ),
{ 52: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 81 ),
  ( sym: -9; act: 22 ),
{ 53: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 82 ),
  ( sym: -9; act: 22 ),
{ 54: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 83 ),
  ( sym: -9; act: 22 ),
{ 55: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 84 ),
  ( sym: -9; act: 22 ),
{ 56: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 85 ),
  ( sym: -9; act: 22 ),
{ 57: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 86 ),
  ( sym: -9; act: 22 ),
{ 58: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 87 ),
  ( sym: -9; act: 22 ),
{ 59: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 88 ),
  ( sym: -9; act: 22 ),
{ 60: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 89 ),
  ( sym: -9; act: 22 ),
{ 61: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 90 ),
  ( sym: -9; act: 22 ),
{ 62: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 91 ),
  ( sym: -9; act: 22 ),
{ 63: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 92 ),
  ( sym: -9; act: 22 ),
{ 64: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 93 ),
{ 65: }
{ 66: }
{ 67: }
{ 68: }
  ( sym: -18; act: 95 ),
  ( sym: -16; act: 96 ),
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 97 ),
  ( sym: -9; act: 22 ),
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
{ 92: }
{ 93: }
  ( sym: -5; act: 99 ),
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
  ( sym: -9; act: 1 ),
  ( sym: -4; act: 103 ),
{ 101: }
  ( sym: -14; act: 19 ),
  ( sym: -13; act: 20 ),
  ( sym: -12; act: 104 ),
  ( sym: -9; act: 22 )
{ 102: }
{ 103: }
{ 104: }
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
{ 20: } -43,
{ 21: } 0,
{ 22: } -47,
{ 23: } -52,
{ 24: } -53,
{ 25: } 0,
{ 26: } 0,
{ 27: } -59,
{ 28: } -58,
{ 29: } -60,
{ 30: } -61,
{ 31: } -62,
{ 32: } -63,
{ 33: } 0,
{ 34: } -51,
{ 35: } -49,
{ 36: } -48,
{ 37: } 0,
{ 38: } -50,
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
{ 64: } 0,
{ 65: } -42,
{ 66: } -41,
{ 67: } 0,
{ 68: } 0,
{ 69: } -12,
{ 70: } -7,
{ 71: } -15,
{ 72: } -14,
{ 73: } -16,
{ 74: } -17,
{ 75: } -18,
{ 76: } -19,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } 0,
{ 81: } 0,
{ 82: } 0,
{ 83: } 0,
{ 84: } 0,
{ 85: } 0,
{ 86: } -40,
{ 87: } 0,
{ 88: } 0,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } 0,
{ 94: } -44,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } -46,
{ 99: } -5,
{ 100: } 0,
{ 101: } 0,
{ 102: } -45,
{ 103: } -10,
{ 104: } 0
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
{ 22: } 101,
{ 23: } 101,
{ 24: } 101,
{ 25: } 101,
{ 26: } 117,
{ 27: } 133,
{ 28: } 133,
{ 29: } 133,
{ 30: } 133,
{ 31: } 133,
{ 32: } 133,
{ 33: } 133,
{ 34: } 149,
{ 35: } 149,
{ 36: } 149,
{ 37: } 149,
{ 38: } 170,
{ 39: } 170,
{ 40: } 187,
{ 41: } 204,
{ 42: } 204,
{ 43: } 205,
{ 44: } 211,
{ 45: } 211,
{ 46: } 211,
{ 47: } 211,
{ 48: } 211,
{ 49: } 227,
{ 50: } 243,
{ 51: } 259,
{ 52: } 275,
{ 53: } 291,
{ 54: } 307,
{ 55: } 323,
{ 56: } 339,
{ 57: } 355,
{ 58: } 371,
{ 59: } 387,
{ 60: } 403,
{ 61: } 419,
{ 62: } 435,
{ 63: } 451,
{ 64: } 467,
{ 65: } 473,
{ 66: } 473,
{ 67: } 473,
{ 68: } 489,
{ 69: } 506,
{ 70: } 506,
{ 71: } 506,
{ 72: } 506,
{ 73: } 506,
{ 74: } 506,
{ 75: } 506,
{ 76: } 506,
{ 77: } 506,
{ 78: } 522,
{ 79: } 542,
{ 80: } 562,
{ 81: } 582,
{ 82: } 602,
{ 83: } 622,
{ 84: } 642,
{ 85: } 662,
{ 86: } 682,
{ 87: } 682,
{ 88: } 696,
{ 89: } 710,
{ 90: } 724,
{ 91: } 738,
{ 92: } 752,
{ 93: } 766,
{ 94: } 768,
{ 95: } 768,
{ 96: } 770,
{ 97: } 771,
{ 98: } 788,
{ 99: } 788,
{ 100: } 788,
{ 101: } 794,
{ 102: } 810,
{ 103: } 810,
{ 104: } 810
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
{ 21: } 100,
{ 22: } 100,
{ 23: } 100,
{ 24: } 100,
{ 25: } 116,
{ 26: } 132,
{ 27: } 132,
{ 28: } 132,
{ 29: } 132,
{ 30: } 132,
{ 31: } 132,
{ 32: } 132,
{ 33: } 148,
{ 34: } 148,
{ 35: } 148,
{ 36: } 148,
{ 37: } 169,
{ 38: } 169,
{ 39: } 186,
{ 40: } 203,
{ 41: } 203,
{ 42: } 204,
{ 43: } 210,
{ 44: } 210,
{ 45: } 210,
{ 46: } 210,
{ 47: } 210,
{ 48: } 226,
{ 49: } 242,
{ 50: } 258,
{ 51: } 274,
{ 52: } 290,
{ 53: } 306,
{ 54: } 322,
{ 55: } 338,
{ 56: } 354,
{ 57: } 370,
{ 58: } 386,
{ 59: } 402,
{ 60: } 418,
{ 61: } 434,
{ 62: } 450,
{ 63: } 466,
{ 64: } 472,
{ 65: } 472,
{ 66: } 472,
{ 67: } 488,
{ 68: } 505,
{ 69: } 505,
{ 70: } 505,
{ 71: } 505,
{ 72: } 505,
{ 73: } 505,
{ 74: } 505,
{ 75: } 505,
{ 76: } 505,
{ 77: } 521,
{ 78: } 541,
{ 79: } 561,
{ 80: } 581,
{ 81: } 601,
{ 82: } 621,
{ 83: } 641,
{ 84: } 661,
{ 85: } 681,
{ 86: } 681,
{ 87: } 695,
{ 88: } 709,
{ 89: } 723,
{ 90: } 737,
{ 91: } 751,
{ 92: } 765,
{ 93: } 767,
{ 94: } 767,
{ 95: } 769,
{ 96: } 770,
{ 97: } 787,
{ 98: } 787,
{ 99: } 787,
{ 100: } 793,
{ 101: } 809,
{ 102: } 809,
{ 103: } 809,
{ 104: } 826
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
{ 64: } 104,
{ 65: } 106,
{ 66: } 106,
{ 67: } 106,
{ 68: } 106,
{ 69: } 112,
{ 70: } 112,
{ 71: } 112,
{ 72: } 112,
{ 73: } 112,
{ 74: } 112,
{ 75: } 112,
{ 76: } 112,
{ 77: } 112,
{ 78: } 112,
{ 79: } 112,
{ 80: } 112,
{ 81: } 112,
{ 82: } 112,
{ 83: } 112,
{ 84: } 112,
{ 85: } 112,
{ 86: } 112,
{ 87: } 112,
{ 88: } 112,
{ 89: } 112,
{ 90: } 112,
{ 91: } 112,
{ 92: } 112,
{ 93: } 112,
{ 94: } 113,
{ 95: } 113,
{ 96: } 113,
{ 97: } 113,
{ 98: } 113,
{ 99: } 113,
{ 100: } 113,
{ 101: } 115,
{ 102: } 119,
{ 103: } 119,
{ 104: } 119
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
{ 63: } 103,
{ 64: } 105,
{ 65: } 105,
{ 66: } 105,
{ 67: } 105,
{ 68: } 111,
{ 69: } 111,
{ 70: } 111,
{ 71: } 111,
{ 72: } 111,
{ 73: } 111,
{ 74: } 111,
{ 75: } 111,
{ 76: } 111,
{ 77: } 111,
{ 78: } 111,
{ 79: } 111,
{ 80: } 111,
{ 81: } 111,
{ 82: } 111,
{ 83: } 111,
{ 84: } 111,
{ 85: } 111,
{ 86: } 111,
{ 87: } 111,
{ 88: } 111,
{ 89: } 111,
{ 90: } 111,
{ 91: } 111,
{ 92: } 111,
{ 93: } 112,
{ 94: } 112,
{ 95: } 112,
{ 96: } 112,
{ 97: } 112,
{ 98: } 112,
{ 99: } 112,
{ 100: } 114,
{ 101: } 118,
{ 102: } 118,
{ 103: } 118,
{ 104: } 118
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
{ 40: } ( len: 3; sym: -12 ),
{ 41: } ( len: 2; sym: -12 ),
{ 42: } ( len: 2; sym: -12 ),
{ 43: } ( len: 1; sym: -12 ),
{ 44: } ( len: 3; sym: -13 ),
{ 45: } ( len: 4; sym: -13 ),
{ 46: } ( len: 4; sym: -13 ),
{ 47: } ( len: 1; sym: -13 ),
{ 48: } ( len: 1; sym: -13 ),
{ 49: } ( len: 1; sym: -13 ),
{ 50: } ( len: 1; sym: -13 ),
{ 51: } ( len: 1; sym: -13 ),
{ 52: } ( len: 1; sym: -13 ),
{ 53: } ( len: 1; sym: -13 ),
{ 54: } ( len: 1; sym: -16 ),
{ 55: } ( len: 0; sym: -16 ),
{ 56: } ( len: 3; sym: -18 ),
{ 57: } ( len: 1; sym: -18 ),
{ 58: } ( len: 1; sym: -14 ),
{ 59: } ( len: 1; sym: -14 ),
{ 60: } ( len: 1; sym: -14 ),
{ 61: } ( len: 1; sym: -14 ),
{ 62: } ( len: 1; sym: -14 ),
{ 63: } ( len: 1; sym: -14 )
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