%{
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

%}

 /* ================= */
 /* Statement tokens  */
 /* ================= */

 /* Define statment   */
%token OPDefine
 /* Info statement  */
%token OPInfo OPNote OPWarning 
 /* Goto statement  */
%token OPGoto OPClear OPMissing

 /* General tokens 		                           *
  * Do not edit anything below this line unless you know   *
  * what you are doing!                                    *
  * ====================================================== */

 /*   */
%token OPBegin OPEnd 

 /* Constants tokens */
%token <Boolean>OPTrue OPFalse

 /* Binary OPerator tokens */
%token OPOr OPAnd OPMod OPDiv /* OPShl OPShr OPXor */
%token OPMult OPPlus OPMinus OPDivide 

 /* Unary OPerators */
%token OPNot

 /* Comparison tokens */
%token OPEQ OPNEQ OPLT OPLTE OPGT OPGTE

 /* Keyword tokens */
%token OPIf OPThen OPElse

 /* Type tokens */
%token OPStringType OPIntegerType OPFloatType OPBooleanType OPDateType OPTimeType

 /* Misc. tokens */
%token OPOpenParan OPCloseParan
%token OPSemicolon OPComma OPPeriod OPAssign
%token OPColon
 /* %token OPOpenBracket OPCloseBracket OPHash */

 /* Special case tokens */
%token <EpiFloat> OPFloatLiteral
%token <EpiInteger> OPIntegerLiteral
%token <IdString> OPIdentifier OPWrite
%token <PString> OPStringLiteral

%token OPIllegal		/* illegal token */


/* Declare types */
%type <TStatementList>		statementlist program
%type <TCustomStatement>	statement opt_else
%type <Word>			typicalcommands emptycommands
%type <TEpiFieldType>		definetype
%type <TCustomVariable>		variable ident_or_write
%type <TExpr>			opt_bracket expr term
%type <TParserOperationType>	typecast
%type <TGotoOption>		goto_opt
%type <TParamList>		expr_list




  /*   Precedence rulse:                       */
  /*   =, <>, <, >, <=, >=             Lowest  */
  /*   +, -, or, xor                   Second  */
  /*   *, /, div, mod, and, shl, shr   Third   */
  /*   not, unaryminus                 Highest */ 

%nonassoc OPEQ OPNEQ OPLT OPLTE OPGT OPGTE
%left OPPlus OPMinus OPOr /*OPXor*/
%left OPMult OPDivide OPDiv OPMod OPAnd /*OPShl OPShr*/
%right OPNot UMINUS

%{

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

%}

%%


 /* %start commandlist */

program		:	statementlist					{ AST := $1 }
		;

statementlist	:	statement OPSemicolon statementlist		{ $$ := TStatementList.Create($1, $3); }
		| 	/* empty */					{ $$ := nil; }
		;

statement 	:	OPBegin statementlist OPEnd			{ $$ := $2; }  
		| 	OPIf expr OPThen statement opt_else		{ $$ := TIfThen.Create($2, $4, $5); } 
		|	variable OPAssign expr				{ $$ := TAssignment.Create($1, $3); } 
                |       OPDefine ident_list OPColon definetype
{
  $$ := TDefine.Create($4, IdentList, FParser);
  ClearIdentList;
} 
/*		|	OPInfo OPStringText infotype			{ }  */
		|	OPGoto ident_or_write goto_opt			{ $$ := TGoto.Create($2, $3); }
 /* DEBUG ONLY - DELETE FOR RELEASE */
		|	OPWrite expr					{ $$ := TWrite.Create($2); }
		;

opt_else	:	OPElse statement				{ $$ := $2; }
		|	/* empty */					{ $$ := nil; }
		;

ident_list	:	ident_list OPComma OPIdentifier			{ AddToIdentList($3); }
		|	OPIdentifier					{ AddToIdentList($1); }
		;

/*
infotype	:	OPNote
		|	OPWarning
		;
*/

definetype	:	OPIntegerType					{ $$ := ftInteger; }
		|	OPStringType					{ $$ := ftString; } 
		|	OPFloatType					{ $$ := ftFloat; }
		|	OPBooleanType					{ $$ := ftBoolean; }
		|	OPDateType					{ $$ := ftDMYDate; } 
		|	OPTimeType					{ $$ := ftTime; } 
		;

ident_or_write	:	variable					{ $$ := $1; }
		|	OPWrite						{ $$ := nil; }
		;

goto_opt	:	OPClear						{ $$ := goClear; }
		|	OPMissing					{ $$ := goMissing; }
		|	/* empty */					{ $$ := goNoOpt; }
		;

variable	:	OPIdentifier					{ $$ := TCustomVariable.FindVariable($1, FParser); }
		;

expr		:	expr OPEQ expr					{ $$ := TRelationalExpr.Create(otEQ, $1, $3); }
		|	expr OPNEQ expr					{ $$ := TRelationalExpr.Create(otNEQ, $1, $3); }
		|	expr OPLT expr					{ $$ := TRelationalExpr.Create(otLT, $1, $3); }
		|	expr OPGT expr					{ $$ := TRelationalExpr.Create(otGT, $1, $3); }
		|	expr OPLTE expr					{ $$ := TRelationalExpr.Create(otLTE, $1, $3); }
		|	expr OPGTE expr					{ $$ := TRelationalExpr.Create(otGTE, $1, $3); }
		|	expr OPPlus expr       				{ $$ := TBinaryExpr.Create(otPlus, $1, $3); }
		|	expr OPMinus expr				{ $$ := TBinaryExpr.Create(otMinus, $1, $3); }
		|	expr OPOr expr					{ $$ := TBinaryExpr.Create(otOr, $1, $3); }
/*		|	expr OPXor expr					{ $$ := TBinaryExpr.Create(otXor, $1, $3); } */
		|       expr OPMult expr       				{ $$ := TBinaryExpr.Create(otMult, $1, $3); }
		|	expr OPDivide expr     				{ $$ := TBinaryExpr.Create(otDivide, $1, $3); }
		|	expr OPDiv expr					{ $$ := TBinaryExpr.Create(otDiv, $1, $3); }
		|       expr OPMod expr					{ $$ := TBinaryExpr.Create(otMod, $1, $3); }
		|	expr OPAnd expr					{ $$ := TBinaryExpr.Create(otAnd, $1, $3); }
/*		|       expr OPShl expr					{ $$ := TBinaryExpr.Create(otShl, $1, $3); } */
/*		|	expr OPShr expr					{ $$ := TBinaryExpr.Create(otShr, $1, $3); } */
                |       OPNot expr					{ $$ := TUnaryExpr.Create(otNot,  $2, nil); }
                |       OPMinus expr					{ $$ := TUnaryExpr.Create(otMinus, $2, nil); }
                        %prec UMINUS
		|	term						
		;

term		:	OPOpenParan expr OPCloseParan			{ $$ := $2; }
		|	OPIdentifier OPOpenParan expr_list OPCloseParan	{ $$ := TFunction.CreateFunction($1, $3, FParser); }
		|	typecast OPOpenParan expr OPCloseParan		{ $$ := TTypeCast.Create($1, $3, nil); }
                |       variable					{ $$ := $1; }
		|	OPIntegerLiteral				{ $$ := TIntegerLiteral.Create($1);  }
		|	OPFloatLiteral					{ $$ := TFloatLiteral.Create($1);  }
		|	OPStringLiteral					{
									  if Assigned($1) then
									    $$ := TStringLiteral.Create($1^)
									  else
									    $$ := TStringLiteral.Create('');
									  DisposeStr($1);
									}
		|	OPPeriod					{ $$ := TMissingLiteral.Create; }
		|	OPTrue						{ $$ := TBooleanLiteral.Create(true); }
		|	OPFalse						{ $$ := TBooleanLiteral.Create(false); }
		;

expr_list	:	expr_list_wrap					{ $$ := TParamList.Create(ParamList); FreeAndNil(ParamList); }
		|	/* empty */					{ $$ := TParamList.Create(nil); }
		;	

expr_list_wrap  :       expr_list_wrap OPComma expr 			{ AddToParamList($3); }
		|	expr						{ AddToParamList($1); }
		;

typecast	:	OPIntegerType					{ $$ := otIntegerCast; }
		|	OPStringType					{ $$ := otStringCast; } 
		|	OPFloatType					{ $$ := otFloatCast; }
		|	OPBooleanType					{ $$ := otBoolCast; }
		|	OPDateType					{ $$ := otIntegerCast; }
		|	OPTimeType					{ $$ := otFloatCast; }
		;

%%

{$I epi_parser_core.inc}

procedure yy_set_start_state;
begin
  start(normal);
end;
end.
