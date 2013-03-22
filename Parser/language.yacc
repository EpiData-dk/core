%{
unit epi_parser_core;

{$mode objfpc}{$H+}
{$GOTO ON}

interface

uses
  sysutils,
  epi_parser_types,
  epi_script_parser,
  epi_script_AST;

type
  IdString = String[64];

function yyparse(Const EpiParser: IEpiScriptParser; Out AST: TStatementList): boolean;

implementation

uses
  yacclib,
  lexlib;

var
  FParser: IEpiScriptParser;

%}

  /* Command tokens */
%token OPBegin OPEnd OPDefine

 /* General tokens 		                           *
  * Do not edit anything below this line unless you know   *
  * what you are doing!                                    *
  * ====================================================== */

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
%token OPString OPInteger OPFloat OPBoolean

 /* Misc. tokens */
%token OPOpenParan OPCloseParan
%token OPSemicolon OPComma OPPeriod OPAssign
 /* %token OPOpenBracket OPCloseBracket OPHash */

 /* Special case tokens */
%token <Extended> OPFloat
%token <Integer> OPNumber OPHexNumber
%token <IdString> OPString OPIdentifier

%token OPIllegal		/* illegal token */


/* Declare types */
%type <TStatementList>		statementlist program
%type <TCustomStatement>	statement opt_else
%type <Word>			typicalcommands emptycommands
%type <TParserResultType>	definetype
%type <TVarList>		varlist
%type <TCustomVariable>		variable
%type <TExpr>			opt_bracket expr term
%type <TParserOperationType>	typecast




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
/*		|	typicalcommands varlist				{ $$ := TStatement.Create($1, $2); }  */
                |       OPDefine definetype OPIdentifier		{ $$ := TDefine.Create($2, $3, FParser); } 
		;

opt_else	:	OPElse statement				{ $$ := $2; }
		|	/* empty */					{ $$ := nil; }
		;

/*
typicalcommands	:       OPClear 					{ $$ := OPClear; }
		;
*/

definetype	:	OPInteger					{ $$ := rtInteger; }
/*		|	OPString					{ $$ := rtString; } */
		|	OPFloat						{ $$ := rtFloat; }
		|	OPBoolean					{ $$ := rtBoolean; }
		;

/*
varlist		:	variable varlist    				{ $$ := TVarlist.Create($1, $2); }
		|							{ $$ := nil; }
		;
*/

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
		|	typecast OPOpenParan expr OPCloseParan		{ $$ := TTypeCast.Create($1, $3, nil) }
                |       variable					{ $$ := $1; }
		|	OPNumber					{ $$ := TLiteral.Create($1);  }
		|	OPHexNumber             			{ $$ := TLiteral.Create($1);  }
		|	OPFloat						{ $$ := TLiteral.Create($1);  }
		|	OPTrue						{ $$ := TLiteral.Create(true); }
		|	OPFalse						{ $$ := TLiteral.Create(false); }
		;

typecast	:	OPInteger					{ $$ := otIntegerCast }
/*		|	OPString					{ $$ := otStringCast } */
		|	OPFloat						{ $$ := otFloatCast }
		|	OPBoolean					{ $$ := otBoolCast }
		;

%%

{$I epi_parser_core.inc}

initialization
  start(normal);

end.
