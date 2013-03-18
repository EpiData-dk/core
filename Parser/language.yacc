%{
unit parser_core;

{$GOTO ON}

interface

uses
  AST,
  sysutils,
  yacclib,
  lexlib,
  parser_types,
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

 /* Typecast tokens */
%token OPStringCast OPIntegerCast OPFloatCast

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
%type <TCustomStatement>	statement
%type <Word>			typicalcommands emptycommands
%type <TParserResultType>	definetype
%type <TVarList>		varlist
%type <TVariable>		variable
%type <TExpr>			opt_bracket expr
%type <TTerm>			term
%type <TOptElse>		opt_else
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
    if Assigned(OnParseError) then
      OnParseError(Msg, yylineno, yycolno, yytext)
    else if IsConsole then
      writeln('(', yylineno, ',', yycolno, '): ', msg, ' at or before ''', yytext, '''.')
  end(*yyerror*);

%}

%%


 /* %start commandlist */

program		:	statementlist					{ StmList := $1 }
		;

statementlist	:	statement OPSemicolon statementlist		{ $$ := TStatementList.Create($1, $3); }
		| 	/* empty */					{ $$ := nil; }
		;

statement 	:	OPBegin statementlist OPEnd			{ $$ := TStatementList.Create($2, nil); }  
		| 	OPIf expr OPThen statement opt_else		{ $$ := TIfThen.Create($2, $4, $5); } 
		|	variable OPAssign expr				{ $$ := TAssignment.Create($1, $3); } 
/*		|	typicalcommands varlist				{ $$ := TStatement.Create($1, $2); }  */
                |       OPDefine definetype OPIdentifier		{ $$ := TDefine.Create($2, $3); } 
		;

opt_else	:	OPElse statement				{ $$ := TOptElse.Create($2); }
		|	/* empty */					{ $$ := nil; }
		;

/*
typicalcommands	:       OPClear 					{ $$ := OPClear; }
		;
*/

definetype	:	OPIntegerCast					{ $$ := rtInteger; }
		|	OPStringCast					{ $$ := rtString; }
		|	OPFloatCast					{ $$ := rtFloat; }
		;

/*
varlist		:	variable varlist    				{ $$ := TVarlist.Create($1, $2); }
		|							{ $$ := nil; }
		;
*/

variable	:	OPIdentifier					{ $$ := TVariable.Create($1); }
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

term		:	OPOpenParan expr OPCloseParan			{ $$ := TParen.Create($2); }
		|	typecast OPOpenParan expr OPCloseParan		{ $$ := TTypeCast.Create($1, $3) }
                |       variable					{ $$ := TTermVar.Create($1); }
		|	OPNumber					{ $$ := TLiteral.Create($1);  }
		|	OPHexNumber             			{ $$ := TLiteral.Create($1);  }
		|	OPFloat						{ $$ := TLiteral.Create($1);  }
		|	OPTrue						{ $$ := TLiteral.Create($1); }
		|	OPFalse						{ $$ := TLiteral.Create($1); }
		;

typecast	:	OPStringCast					{ $$ := otStringCast }
		|	OPIntegerCast					{ $$ := otIntegerCast }
		|	OPFloatCast					{ $$ := otFloatCast }
		;

%%

{$I parser_core.inc}

initialization
  start(normal);

end.
