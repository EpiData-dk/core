
unit epi_parser_core;

{$GOTO ON}

interface

uses
  epi_script_AST,
  sysutils,
  yacclib,
  lexlib,
  epi_parser_types,
  typetable;

type
  IdString = String[64];
  TParserError = procedure (Const Msg: string; Const LineNo, ColNo: integer; Const Text: string) of object;
  TParserGetIdentType = function (Const VarName: string): TParserResultType of object;
  TParserGetSymbolTable = function: TTypeTable of object;

var
  StmList: TStatementList;
  OnParseError: TParserError;
  OnGetIdentType: TParserGetIdentType;
  OnGetSymbolTable: TParserGetSymbolTable;

function yyparse: integer;
procedure yyerror(msg: string);

implementation

const OPBegin = 257;
const OPEnd = 258;
const OPDefine = 259;
const OPTrue = 260;
const OPFalse = 261;
const OPOr = 262;
const OPAnd = 263;
const OPMod = 264;
const OPDiv = 265;
const OPMult = 266;
const OPPlus = 267;
const OPMinus = 268;
const OPDivide = 269;
const OPNot = 270;
const OPEQ = 271;
const OPNEQ = 272;
const OPLT = 273;
const OPLTE = 274;
const OPGT = 275;
const OPGTE = 276;
const OPIf = 277;
const OPThen = 278;
const OPElse = 279;
const OPStringCast = 280;
const OPIntegerCast = 281;
const OPFloatCast = 282;
const OPOpenParan = 283;
const OPCloseParan = 284;
const OPSemicolon = 285;
const OPComma = 286;
const OPPeriod = 287;
const OPAssign = 288;
const OPFloat = 289;
const OPNumber = 290;
const OPHexNumber = 291;
const OPString = 292;
const OPIdentifier = 293;
const OPIllegal = 294;
const UMINUS = 295;


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
         yyval.yyTCustomStatement := yyv[yysp-1].yyTStatementList; 
       end;
   5 : begin
         yyval.yyTCustomStatement := TIfThen.Create(yyv[yysp-3].yyTExpr, yyv[yysp-1].yyTCustomStatement, yyv[yysp-0].yyTCustomStatement); 
       end;
   6 : begin
         yyval.yyTCustomStatement := TAssignment.Create(yyv[yysp-2].yyTCustomVariable, yyv[yysp-0].yyTExpr); 
       end;
   7 : begin
         yyval.yyTCustomStatement := TDefine.Create(yyv[yysp-1].yyTParserResultType, yyv[yysp-0].yyIdString); 
       end;
   8 : begin
         yyval.yyTCustomStatement := yyv[yysp-0].yyTCustomStatement; 
       end;
   9 : begin
         yyval.yyTCustomStatement := nil; 
       end;
  10 : begin
         yyval.yyTParserResultType := rtInteger; 
       end;
  11 : begin
         yyval.yyTParserResultType := rtString; 
       end;
  12 : begin
         yyval.yyTParserResultType := rtFloat; 
       end;
  13 : begin
         yyval.yyTCustomVariable := TCustomVariable.CreateVariable(yyv[yysp-0].yyIdString); 
       end;
  14 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  15 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otNEQ, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  16 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  17 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGT, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  18 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otLTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  19 : begin
         yyval.yyTExpr := TRelationalExpr.Create(otGTE, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  20 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otPlus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  21 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMinus, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  22 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otOr, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  23 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMult, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  24 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDivide, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  25 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otDiv, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  26 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otMod, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  27 : begin
         yyval.yyTExpr := TBinaryExpr.Create(otAnd, yyv[yysp-2].yyTExpr, yyv[yysp-0].yyTExpr); 
       end;
  28 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otNot,  yyv[yysp-0].yyTExpr, nil); 
       end;
  29 : begin
         yyval.yyTExpr := TUnaryExpr.Create(otMinus, yyv[yysp-0].yyTExpr, nil); 
       end;
  30 : begin
         yyval := yyv[yysp-0];
       end;
  31 : begin
         yyval.yyTExpr := yyv[yysp-1].yyTExpr; 
       end;
  32 : begin
         yyval.yyTExpr := TTypeCast.Create(yyv[yysp-3].yyTParserOperationType, yyv[yysp-1].yyTExpr, nil) 
       end;
  33 : begin
         yyval.yyTExpr := yyv[yysp-0].yyTCustomVariable; 
       end;
  34 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  35 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyInteger);  
       end;
  36 : begin
         yyval.yyTExpr := TLiteral.Create(yyv[yysp-0].yyExtended);  
       end;
  37 : begin
         yyval.yyTExpr := TLiteral.Create(true); 
       end;
  38 : begin
         yyval.yyTExpr := TLiteral.Create(false); 
       end;
  39 : begin
         yyval.yyTParserOperationType := otStringCast 
       end;
  40 : begin
         yyval.yyTParserOperationType := otIntegerCast 
       end;
  41 : begin
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

yynacts   = 462;
yyngotos  = 96;
yynstates = 75;
yynrules  = 41;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 257; act: 5 ),
  ( sym: 259; act: 6 ),
  ( sym: 277; act: 7 ),
  ( sym: 293; act: 8 ),
  ( sym: 0; act: -3 ),
{ 1: }
  ( sym: 288; act: 9 ),
{ 2: }
  ( sym: 285; act: 10 ),
{ 3: }
  ( sym: 0; act: 0 ),
{ 4: }
{ 5: }
  ( sym: 257; act: 5 ),
  ( sym: 259; act: 6 ),
  ( sym: 277; act: 7 ),
  ( sym: 293; act: 8 ),
  ( sym: 258; act: -3 ),
{ 6: }
  ( sym: 280; act: 13 ),
  ( sym: 281; act: 14 ),
  ( sym: 282; act: 15 ),
{ 7: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 8: }
{ 9: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 10: }
  ( sym: 257; act: 5 ),
  ( sym: 259; act: 6 ),
  ( sym: 277; act: 7 ),
  ( sym: 293; act: 8 ),
  ( sym: 0; act: -3 ),
  ( sym: 258; act: -3 ),
{ 11: }
  ( sym: 258; act: 33 ),
{ 12: }
  ( sym: 293; act: 34 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
  ( sym: 283; act: 35 ),
{ 17: }
{ 18: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 278; act: 50 ),
{ 19: }
{ 20: }
{ 21: }
{ 22: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 23: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 24: }
{ 25: }
{ 26: }
{ 27: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 279; act: -6 ),
  ( sym: 285; act: -6 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 36: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 37: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 38: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 39: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 40: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 41: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 42: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 43: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 44: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 45: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 46: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 47: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 48: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 49: }
  ( sym: 260; act: 20 ),
  ( sym: 261; act: 21 ),
  ( sym: 268; act: 22 ),
  ( sym: 270; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 289; act: 28 ),
  ( sym: 290; act: 29 ),
  ( sym: 291; act: 30 ),
  ( sym: 293; act: 8 ),
{ 50: }
  ( sym: 257; act: 5 ),
  ( sym: 259; act: 6 ),
  ( sym: 277; act: 7 ),
  ( sym: 293; act: 8 ),
{ 51: }
{ 52: }
{ 53: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 284; act: 70 ),
{ 54: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 271; act: 44 ),
  ( sym: 272; act: 45 ),
  ( sym: 273; act: 46 ),
  ( sym: 274; act: 47 ),
  ( sym: 275; act: 48 ),
  ( sym: 276; act: 49 ),
  ( sym: 284; act: 71 ),
{ 55: }
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 269; act: 43 ),
  ( sym: 262; act: -22 ),
  ( sym: 267; act: -22 ),
  ( sym: 268; act: -22 ),
  ( sym: 271; act: -22 ),
  ( sym: 272; act: -22 ),
  ( sym: 273; act: -22 ),
  ( sym: 274; act: -22 ),
  ( sym: 275; act: -22 ),
  ( sym: 276; act: -22 ),
  ( sym: 278; act: -22 ),
  ( sym: 279; act: -22 ),
  ( sym: 284; act: -22 ),
  ( sym: 285; act: -22 ),
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 269; act: 43 ),
  ( sym: 262; act: -20 ),
  ( sym: 267; act: -20 ),
  ( sym: 268; act: -20 ),
  ( sym: 271; act: -20 ),
  ( sym: 272; act: -20 ),
  ( sym: 273; act: -20 ),
  ( sym: 274; act: -20 ),
  ( sym: 275; act: -20 ),
  ( sym: 276; act: -20 ),
  ( sym: 278; act: -20 ),
  ( sym: 279; act: -20 ),
  ( sym: 284; act: -20 ),
  ( sym: 285; act: -20 ),
{ 61: }
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 269; act: 43 ),
  ( sym: 262; act: -21 ),
  ( sym: 267; act: -21 ),
  ( sym: 268; act: -21 ),
  ( sym: 271; act: -21 ),
  ( sym: 272; act: -21 ),
  ( sym: 273; act: -21 ),
  ( sym: 274; act: -21 ),
  ( sym: 275; act: -21 ),
  ( sym: 276; act: -21 ),
  ( sym: 278; act: -21 ),
  ( sym: 279; act: -21 ),
  ( sym: 284; act: -21 ),
  ( sym: 285; act: -21 ),
{ 62: }
{ 63: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 278; act: -14 ),
  ( sym: 279; act: -14 ),
  ( sym: 284; act: -14 ),
  ( sym: 285; act: -14 ),
{ 64: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 278; act: -15 ),
  ( sym: 279; act: -15 ),
  ( sym: 284; act: -15 ),
  ( sym: 285; act: -15 ),
{ 65: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 278; act: -16 ),
  ( sym: 279; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 285; act: -16 ),
{ 66: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 278; act: -18 ),
  ( sym: 279; act: -18 ),
  ( sym: 284; act: -18 ),
  ( sym: 285; act: -18 ),
{ 67: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 278; act: -17 ),
  ( sym: 279; act: -17 ),
  ( sym: 284; act: -17 ),
  ( sym: 285; act: -17 ),
{ 68: }
  ( sym: 262; act: 36 ),
  ( sym: 263; act: 37 ),
  ( sym: 264; act: 38 ),
  ( sym: 265; act: 39 ),
  ( sym: 266; act: 40 ),
  ( sym: 267; act: 41 ),
  ( sym: 268; act: 42 ),
  ( sym: 269; act: 43 ),
  ( sym: 278; act: -19 ),
  ( sym: 279; act: -19 ),
  ( sym: 284; act: -19 ),
  ( sym: 285; act: -19 ),
{ 69: }
  ( sym: 279; act: 73 ),
  ( sym: 285; act: -9 ),
{ 70: }
{ 71: }
{ 72: }
{ 73: }
  ( sym: 257; act: 5 ),
  ( sym: 259; act: 6 ),
  ( sym: 277; act: 7 ),
  ( sym: 293; act: 8 )
{ 74: }
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
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 11 ),
{ 6: }
  ( sym: -8; act: 12 ),
{ 7: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 18 ),
  ( sym: -10; act: 19 ),
{ 8: }
{ 9: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 31 ),
  ( sym: -10; act: 19 ),
{ 10: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 2 ),
  ( sym: -2; act: 32 ),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
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
  ( sym: -12; act: 51 ),
  ( sym: -10; act: 19 ),
{ 23: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 52 ),
  ( sym: -10; act: 19 ),
{ 24: }
{ 25: }
{ 26: }
{ 27: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 53 ),
  ( sym: -10; act: 19 ),
{ 28: }
{ 29: }
{ 30: }
{ 31: }
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 54 ),
  ( sym: -10; act: 19 ),
{ 36: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 55 ),
  ( sym: -10; act: 19 ),
{ 37: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 56 ),
  ( sym: -10; act: 19 ),
{ 38: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 57 ),
  ( sym: -10; act: 19 ),
{ 39: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 58 ),
  ( sym: -10; act: 19 ),
{ 40: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 59 ),
  ( sym: -10; act: 19 ),
{ 41: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 60 ),
  ( sym: -10; act: 19 ),
{ 42: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 61 ),
  ( sym: -10; act: 19 ),
{ 43: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 62 ),
  ( sym: -10; act: 19 ),
{ 44: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 63 ),
  ( sym: -10; act: 19 ),
{ 45: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 64 ),
  ( sym: -10; act: 19 ),
{ 46: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 65 ),
  ( sym: -10; act: 19 ),
{ 47: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 66 ),
  ( sym: -10; act: 19 ),
{ 48: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 67 ),
  ( sym: -10; act: 19 ),
{ 49: }
  ( sym: -14; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -12; act: 68 ),
  ( sym: -10; act: 19 ),
{ 50: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 69 ),
{ 51: }
{ 52: }
{ 53: }
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
  ( sym: -5; act: 72 ),
{ 70: }
{ 71: }
{ 72: }
{ 73: }
  ( sym: -10; act: 1 ),
  ( sym: -4; act: 74 )
{ 74: }
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
{ 8: } -13,
{ 9: } 0,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } -11,
{ 14: } -10,
{ 15: } -12,
{ 16: } 0,
{ 17: } -30,
{ 18: } 0,
{ 19: } -33,
{ 20: } -37,
{ 21: } -38,
{ 22: } 0,
{ 23: } 0,
{ 24: } -39,
{ 25: } -40,
{ 26: } -41,
{ 27: } 0,
{ 28: } -36,
{ 29: } -34,
{ 30: } -35,
{ 31: } 0,
{ 32: } -2,
{ 33: } -4,
{ 34: } -7,
{ 35: } 0,
{ 36: } 0,
{ 37: } 0,
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
{ 51: } -29,
{ 52: } -28,
{ 53: } 0,
{ 54: } 0,
{ 55: } 0,
{ 56: } -27,
{ 57: } -26,
{ 58: } -25,
{ 59: } -23,
{ 60: } 0,
{ 61: } 0,
{ 62: } -24,
{ 63: } 0,
{ 64: } 0,
{ 65: } 0,
{ 66: } 0,
{ 67: } 0,
{ 68: } 0,
{ 69: } 0,
{ 70: } -31,
{ 71: } -32,
{ 72: } -5,
{ 73: } 0,
{ 74: } -8
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 6,
{ 2: } 7,
{ 3: } 8,
{ 4: } 9,
{ 5: } 9,
{ 6: } 14,
{ 7: } 17,
{ 8: } 29,
{ 9: } 29,
{ 10: } 41,
{ 11: } 47,
{ 12: } 48,
{ 13: } 49,
{ 14: } 49,
{ 15: } 49,
{ 16: } 49,
{ 17: } 50,
{ 18: } 50,
{ 19: } 65,
{ 20: } 65,
{ 21: } 65,
{ 22: } 65,
{ 23: } 77,
{ 24: } 89,
{ 25: } 89,
{ 26: } 89,
{ 27: } 89,
{ 28: } 101,
{ 29: } 101,
{ 30: } 101,
{ 31: } 101,
{ 32: } 117,
{ 33: } 117,
{ 34: } 117,
{ 35: } 117,
{ 36: } 129,
{ 37: } 141,
{ 38: } 153,
{ 39: } 165,
{ 40: } 177,
{ 41: } 189,
{ 42: } 201,
{ 43: } 213,
{ 44: } 225,
{ 45: } 237,
{ 46: } 249,
{ 47: } 261,
{ 48: } 273,
{ 49: } 285,
{ 50: } 297,
{ 51: } 301,
{ 52: } 301,
{ 53: } 301,
{ 54: } 316,
{ 55: } 331,
{ 56: } 349,
{ 57: } 349,
{ 58: } 349,
{ 59: } 349,
{ 60: } 349,
{ 61: } 367,
{ 62: } 385,
{ 63: } 385,
{ 64: } 397,
{ 65: } 409,
{ 66: } 421,
{ 67: } 433,
{ 68: } 445,
{ 69: } 457,
{ 70: } 459,
{ 71: } 459,
{ 72: } 459,
{ 73: } 459,
{ 74: } 463
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 5,
{ 1: } 6,
{ 2: } 7,
{ 3: } 8,
{ 4: } 8,
{ 5: } 13,
{ 6: } 16,
{ 7: } 28,
{ 8: } 28,
{ 9: } 40,
{ 10: } 46,
{ 11: } 47,
{ 12: } 48,
{ 13: } 48,
{ 14: } 48,
{ 15: } 48,
{ 16: } 49,
{ 17: } 49,
{ 18: } 64,
{ 19: } 64,
{ 20: } 64,
{ 21: } 64,
{ 22: } 76,
{ 23: } 88,
{ 24: } 88,
{ 25: } 88,
{ 26: } 88,
{ 27: } 100,
{ 28: } 100,
{ 29: } 100,
{ 30: } 100,
{ 31: } 116,
{ 32: } 116,
{ 33: } 116,
{ 34: } 116,
{ 35: } 128,
{ 36: } 140,
{ 37: } 152,
{ 38: } 164,
{ 39: } 176,
{ 40: } 188,
{ 41: } 200,
{ 42: } 212,
{ 43: } 224,
{ 44: } 236,
{ 45: } 248,
{ 46: } 260,
{ 47: } 272,
{ 48: } 284,
{ 49: } 296,
{ 50: } 300,
{ 51: } 300,
{ 52: } 300,
{ 53: } 315,
{ 54: } 330,
{ 55: } 348,
{ 56: } 348,
{ 57: } 348,
{ 58: } 348,
{ 59: } 348,
{ 60: } 366,
{ 61: } 384,
{ 62: } 384,
{ 63: } 396,
{ 64: } 408,
{ 65: } 420,
{ 66: } 432,
{ 67: } 444,
{ 68: } 456,
{ 69: } 458,
{ 70: } 458,
{ 71: } 458,
{ 72: } 458,
{ 73: } 462,
{ 74: } 462
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 5,
{ 2: } 5,
{ 3: } 5,
{ 4: } 5,
{ 5: } 5,
{ 6: } 8,
{ 7: } 9,
{ 8: } 13,
{ 9: } 13,
{ 10: } 17,
{ 11: } 20,
{ 12: } 20,
{ 13: } 20,
{ 14: } 20,
{ 15: } 20,
{ 16: } 20,
{ 17: } 20,
{ 18: } 20,
{ 19: } 20,
{ 20: } 20,
{ 21: } 20,
{ 22: } 20,
{ 23: } 24,
{ 24: } 28,
{ 25: } 28,
{ 26: } 28,
{ 27: } 28,
{ 28: } 32,
{ 29: } 32,
{ 30: } 32,
{ 31: } 32,
{ 32: } 32,
{ 33: } 32,
{ 34: } 32,
{ 35: } 32,
{ 36: } 36,
{ 37: } 40,
{ 38: } 44,
{ 39: } 48,
{ 40: } 52,
{ 41: } 56,
{ 42: } 60,
{ 43: } 64,
{ 44: } 68,
{ 45: } 72,
{ 46: } 76,
{ 47: } 80,
{ 48: } 84,
{ 49: } 88,
{ 50: } 92,
{ 51: } 94,
{ 52: } 94,
{ 53: } 94,
{ 54: } 94,
{ 55: } 94,
{ 56: } 94,
{ 57: } 94,
{ 58: } 94,
{ 59: } 94,
{ 60: } 94,
{ 61: } 94,
{ 62: } 94,
{ 63: } 94,
{ 64: } 94,
{ 65: } 94,
{ 66: } 94,
{ 67: } 94,
{ 68: } 94,
{ 69: } 94,
{ 70: } 95,
{ 71: } 95,
{ 72: } 95,
{ 73: } 95,
{ 74: } 97
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 4,
{ 1: } 4,
{ 2: } 4,
{ 3: } 4,
{ 4: } 4,
{ 5: } 7,
{ 6: } 8,
{ 7: } 12,
{ 8: } 12,
{ 9: } 16,
{ 10: } 19,
{ 11: } 19,
{ 12: } 19,
{ 13: } 19,
{ 14: } 19,
{ 15: } 19,
{ 16: } 19,
{ 17: } 19,
{ 18: } 19,
{ 19: } 19,
{ 20: } 19,
{ 21: } 19,
{ 22: } 23,
{ 23: } 27,
{ 24: } 27,
{ 25: } 27,
{ 26: } 27,
{ 27: } 31,
{ 28: } 31,
{ 29: } 31,
{ 30: } 31,
{ 31: } 31,
{ 32: } 31,
{ 33: } 31,
{ 34: } 31,
{ 35: } 35,
{ 36: } 39,
{ 37: } 43,
{ 38: } 47,
{ 39: } 51,
{ 40: } 55,
{ 41: } 59,
{ 42: } 63,
{ 43: } 67,
{ 44: } 71,
{ 45: } 75,
{ 46: } 79,
{ 47: } 83,
{ 48: } 87,
{ 49: } 91,
{ 50: } 93,
{ 51: } 93,
{ 52: } 93,
{ 53: } 93,
{ 54: } 93,
{ 55: } 93,
{ 56: } 93,
{ 57: } 93,
{ 58: } 93,
{ 59: } 93,
{ 60: } 93,
{ 61: } 93,
{ 62: } 93,
{ 63: } 93,
{ 64: } 93,
{ 65: } 93,
{ 66: } 93,
{ 67: } 93,
{ 68: } 93,
{ 69: } 94,
{ 70: } 94,
{ 71: } 94,
{ 72: } 94,
{ 73: } 96,
{ 74: } 96
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -3 ),
{ 2: } ( len: 3; sym: -2 ),
{ 3: } ( len: 0; sym: -2 ),
{ 4: } ( len: 3; sym: -4 ),
{ 5: } ( len: 5; sym: -4 ),
{ 6: } ( len: 3; sym: -4 ),
{ 7: } ( len: 3; sym: -4 ),
{ 8: } ( len: 2; sym: -5 ),
{ 9: } ( len: 0; sym: -5 ),
{ 10: } ( len: 1; sym: -8 ),
{ 11: } ( len: 1; sym: -8 ),
{ 12: } ( len: 1; sym: -8 ),
{ 13: } ( len: 1; sym: -10 ),
{ 14: } ( len: 3; sym: -12 ),
{ 15: } ( len: 3; sym: -12 ),
{ 16: } ( len: 3; sym: -12 ),
{ 17: } ( len: 3; sym: -12 ),
{ 18: } ( len: 3; sym: -12 ),
{ 19: } ( len: 3; sym: -12 ),
{ 20: } ( len: 3; sym: -12 ),
{ 21: } ( len: 3; sym: -12 ),
{ 22: } ( len: 3; sym: -12 ),
{ 23: } ( len: 3; sym: -12 ),
{ 24: } ( len: 3; sym: -12 ),
{ 25: } ( len: 3; sym: -12 ),
{ 26: } ( len: 3; sym: -12 ),
{ 27: } ( len: 3; sym: -12 ),
{ 28: } ( len: 2; sym: -12 ),
{ 29: } ( len: 2; sym: -12 ),
{ 30: } ( len: 1; sym: -12 ),
{ 31: } ( len: 3; sym: -13 ),
{ 32: } ( len: 4; sym: -13 ),
{ 33: } ( len: 1; sym: -13 ),
{ 34: } ( len: 1; sym: -13 ),
{ 35: } ( len: 1; sym: -13 ),
{ 36: } ( len: 1; sym: -13 ),
{ 37: } ( len: 1; sym: -13 ),
{ 38: } ( len: 1; sym: -13 ),
{ 39: } ( len: 1; sym: -14 ),
{ 40: } ( len: 1; sym: -14 ),
{ 41: } ( len: 1; sym: -14 )
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