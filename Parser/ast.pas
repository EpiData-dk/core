unit AST;

interface

uses
  Classes, SysUtils,
  lexlib,
  parser_types;

type
  TExpr = class;
  TVariable = class;
  TStatement = class;

  { TAbstractSyntaxTreeBase }

  TAbstractSyntaxTreeBase = class(TObject)
  private
    FLineNo: integer;
    FColNo: integer;
    FLine: string;
  protected
    constructor Create; virtual;
  public
    procedure PrettyPrint; virtual;
    function ResultType: TParserResultType; virtual;
    function TypeCheck(out Msg: String): boolean; virtual;
  end;

  TCustomStatement = class(TAbstractSyntaxTreeBase);

  { TAssignment }

  TAssignment = class(TCustomStatement)
  private
    FVAriable: TVariable;
    FExpr: TExpr;
  public
    constructor Create(Const Variable: TVariable; Const Expr: TExpr);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  { TOptElse }

  TOptElse = class(TAbstractSyntaxTreeBase)
  private
    FStatement: TCustomStatement;
  public
    constructor Create(Const Statement: TCustomStatement);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  { TIfThen }

  TIfThen = class(TCustomStatement)
  private
    FExpr: TExpr;
    FStatement: TCustomStatement;
    FOptElse: TOptElse;
  public
    constructor Create(Const Expr: TExpr; Const Statement: TCustomStatement;
      Const OptElse: TOptElse);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  TTerm = class(TAbstractSyntaxTreeBase);

  { TTypeCast }

  TTypeCast = class(TTerm)
  private
    FOp: TParserOperationType;
    FExpr: TExpr;
  public
    constructor Create(Const Op: TParserOperationType; Const Expr: TExpr);
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TParen }

  TParen = class(TTerm)
  private
    FExpr: TExpr;
  public
    constructor Create(Const Expr: TExpr);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TTermVar }

  TTermVar = class(TTerm)
  private
    FVariable: TVariable;
  public
    constructor Create(Const Variable: TVariable);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TLiteral }

  TLiteral = class(TTerm)
  private
    FValueType: TParserResultType;
    FIntVal: integer;
    FExtVal: extended;
    FBoolVal: boolean;
  public
    constructor Create(Const Value: Integer); overload;
    constructor Create(Const Value: Extended); overload;
    constructor Create(Const Value: Boolean); overload;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TExpr }

  TExpr = class(TAbstractSyntaxTreeBase)
  private
    FOp: TParserOperationType;
    FL:  TExpr;
    FR:  TExpr;
  public
    constructor Create(Const Op: TParserOperationType; Const L, R: TExpr);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  { TUnaryExpr }

  TUnaryExpr = class(TExpr)
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TExpr)
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TRelationalExpr }

  TRelationalExpr = class(TExpr)
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TVariable }

  TVariable = class(TAbstractSyntaxTreeBase)
  private
    FIdent: string;
  public
    constructor Create(Const Ident: string);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  end;

  { TVarList }

  TVarList = class(TAbstractSyntaxTreeBase)
  private
    FVariable: TVariable;
    FVarList: TVarList;
  public
    constructor Create(Const Variable: TVariable; Const VarList: TVarList);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  { TDefine }

  TDefine = class(TCustomStatement)
  private
    FIdent: string;
    FType: TParserResultType;
  public
    constructor Create(Const DefineType: TParserResultType; Const Ident: string);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  { TStatement }

  TStatement = class(TCustomStatement)
  private
    FStatementType: integer;
    FVarList: TVarList;
  public
    constructor Create(Const StatementType: word; Const VarList: TVarList);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

  { TStatementList }

  TStatementList = class(TCustomStatement)
  private
    FStatement: TCustomStatement;
    FStatementList: TStatementList;
  public
    constructor Create(Const Statement: TCustomStatement; Const StatementList: TStatementList);
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
  end;

implementation

uses
  YaccLib, parser, typetable, parser_core, math;

var
  ASTTypeTable: TTypeTable = nil;


{ TRelationExpr }

function TRelationalExpr.TypeCheck(out Msg: String): boolean;
const
  RelationalOperationCheck: array[TParserResultType, TParserResultType] of Boolean =
           //    rtBoolean, rtInteger, rtFloat, rtString, rtObject, rtUndefined
               (
 {rtBoolean}    ( true,     false,     false,   false,    false,    false),
 {rtInteger}    (false,     true,      true,    false,    false,    false),
 {rtFloat}      (false,     true,      true,    false,    false,    false),
 {rtString}     (false,     false,     false,   true,     false,    false),
 {rtObject}     (false,     false,     false,   false,    true ,    false),
 {rtUndefined}  (false,     false,     false,   false,    false,    false)
               );
begin
  result := inherited TypeCheck(Msg);

  if result then
  begin
    Result := RelationalOperationCheck[FL.ResultType, FR.ResultType];
    if not result then
      Msg := 'Cannot compare...'
  end;
end;

function TRelationalExpr.ResultType: TParserResultType;
begin
  Result := rtBoolean;
end;

{ TBinaryExpr }

function TBinaryExpr.TypeCheck(out Msg: String): boolean;
var
  Lr: TParserResultType;
  Rr: TParserResultType;
begin
  Result := inherited TypeCheck(Msg);
  Lr := FL.ResultType;
  Rr := FR.ResultType;

  if result then
  case FOp of
    otAnd,
    otOr:
      result := (Lr = rtBoolean) and
                (Rr = rtBoolean);

    otMod,
    otDiv:
      result := (Lr = rtInteger) and
                (Rr = rtInteger);
    otMult,
    otPlus,
    otMinus,
    otDivide:
      result := (Lr in [rtInteger, rtFloat]) and
                (Rr in [rtInteger, rtFloat]);
{    otXor,
    otShl,
    otShr:         }
  end;
end;

function TBinaryExpr.ResultType: TParserResultType;
var
  Lr: TParserResultType;
  Rr: TParserResultType;
begin
  Lr := FL.ResultType;
  Rr := FR.ResultType;

  case FOp of
    otAnd,
    otOr:
      Result := rtBoolean;
    otMod,
    otDiv:
      Result := rtInteger;
    otMult,
    otPlus,
    otMinus,
    otDivide:
      result := TParserResultType(Max(Integer(Lr), Integer(RR)));
{      otShl,
      otShr,
      otXor,
      ;}
  end;
end;

{ TUnaryExpr }

function TUnaryExpr.TypeCheck(out Msg: String): boolean;
begin
  Result := inherited TypeCheck(Msg);

  if result then
  case FOp of
    otNot,
    otMinus:
      ;
  end;
end;

function TUnaryExpr.ResultType: TParserResultType;
begin
  Result := FL.ResultType;
end;

{ TDefine }

constructor TDefine.Create(const DefineType: TParserResultType;
  const Ident: string);
begin
  inherited Create;
  FType := DefineType;
  FIdent := Ident;

  if Assigned(OnGetSymbolTable) then
    ASTTypeTable := OnGetSymbolTable()
  else
    Exit;

  if ASTTypeTable.VariableExists(FIdent) then
  begin
    yyerror('Variable "' + Ident + '" already defined');
    yyabort
  end;

  if Assigned(OnGetIdentType) and
     (OnGetIdentType(FIdent) <> rtUndefined)
  then
  begin
    yyerror('Variable "' + Ident + '" already defined');
    yyabort;
  end
  else
    ASTTypeTable.AddVariable(FIdent, FType);
end;

procedure TDefine.PrettyPrint;
begin
  write('define ', FType, ' ', FIdent);
end;

function TDefine.TypeCheck(out Msg: String): boolean;
begin
  Msg := '';
  Result := true;
end;

{ TAssignment }

constructor TAssignment.Create(const Variable: TVariable; const Expr: TExpr);
begin
  inherited Create;
  FExpr := Expr;
  FVAriable := Variable;
end;

procedure TAssignment.PrettyPrint;
begin
  FVAriable.PrettyPrint;
  write(' := ');
  FExpr.PrettyPrint;
end;

function TAssignment.TypeCheck(out Msg: String): boolean;
begin
  Result := FVAriable.TypeCheck(Msg) and FExpr.TypeCheck(Msg);

  if Result then
  begin
    Result := Ord(FVAriable.ResultType) >= Ord(FExpr.ResultType);

    if not result then
      Msg := 'Incompatible types at line: ' + IntToStr(FExpr.FLineNo);
  end;
end;

{ TOptElse }

constructor TOptElse.Create(const Statement: TCustomStatement);
begin
  inherited Create;
  FStatement := Statement;
end;

procedure TOptElse.PrettyPrint;
begin
  FStatement.PrettyPrint;
end;

function TOptElse.TypeCheck(out Msg: String): boolean;
begin
  Result := FStatement.TypeCheck(Msg);
end;

{ TIfThen }

constructor TIfThen.Create(const Expr: TExpr;
  const Statement: TCustomStatement; const OptElse: TOptElse);
begin
  inherited Create;
  FExpr := Expr;
  FStatement := Statement;
  FOptElse := OptElse;
end;

procedure TIfThen.PrettyPrint;
begin
  write('if');
  FExpr.PrettyPrint;
  writeln(' then ');
  FStatement.PrettyPrint;
  if Assigned(FOptElse) then
  begin
    writeln;
    writeln('else ');
    FOptElse.PrettyPrint;
  end;
end;

function TIfThen.TypeCheck(out Msg: String): boolean;
begin
  Result := FExpr.TypeCheck(Msg) and FStatement.TypeCheck(Msg);

  if Result and Assigned(FOptElse) then
    FOptElse.TypeCheck(Msg);
end;

{ TTermVar }

constructor TTermVar.Create(const Variable: TVariable);
begin
  inherited Create;
  FVariable := Variable;
end;

procedure TTermVar.PrettyPrint;
begin
  FVariable.PrettyPrint;
end;

function TTermVar.TypeCheck(out Msg: String): boolean;
begin
  Result := FVariable.TypeCheck(Msg);
end;

function TTermVar.ResultType: TParserResultType;
begin
  Result := FVariable.ResultType;
end;

{ TLiteral }

constructor TLiteral.Create(const Value: Integer);
begin
  inherited Create;
  FValueType := rtInteger;
  FIntVal := Value;
end;

constructor TLiteral.Create(const Value: Extended);
begin
  inherited Create;
  FValueType := rtFloat;
  FExtVal := Value;
end;

constructor TLiteral.Create(const Value: Boolean);
begin
  inherited Create;
  FValueType := rtBoolean;
  FBoolVal := Value;
end;

procedure TLiteral.PrettyPrint;
begin
  case FValueType of
    rtInteger: write(FIntVal);
    rtFloat:   write(FExtVal);
    rtBoolean: write(FBoolVal);
  end;
end;

function TLiteral.TypeCheck(out Msg: String): boolean;
begin
  Msg := '';
  Result := true;
end;

function TLiteral.ResultType: TParserResultType;
begin
  Result := FValueType;
end;

{ TParen }

constructor TParen.Create(const Expr: TExpr);
begin
  inherited Create;
  FExpr := Expr;
end;

procedure TParen.PrettyPrint;
begin
  write('(');
  FExpr.PrettyPrint;
  write(')');
end;

function TParen.TypeCheck(out Msg: String): boolean;
begin
  Result := FExpr.TypeCheck(Msg);
end;

function TParen.ResultType: TParserResultType;
begin
  Result := FExpr.ResultType;
end;

{ TTypeCast }

constructor TTypeCast.Create(const Op: TParserOperationType; const Expr: TExpr);
begin
  inherited Create;
  FExpr := Expr;
  FOp := Op;
end;

function TTypeCast.TypeCheck(out Msg: String): boolean;
begin
  Result := FExpr.TypeCheck(Msg);

  if result then
  case FOp of
    // Can FExpr.ResultType cast to Desired type!;
    otStringCast,
    otIntegerCast,
    otFloatCast:
      ;
  end;
end;

function TTypeCast.ResultType: TParserResultType;
begin
  Case FOp of
    otStringCast,
    otIntegerCast,
    otFloatCast:
      ;
  end;
  Result := inherited ResultType;
end;

{ TExpr }

constructor TExpr.Create(const Op: TParserOperationType; const L, R: TExpr);
begin
  inherited Create;
  FOp := Op;
  FL := L;
  FR := R;
end;

procedure TExpr.PrettyPrint;
var
  S: String;
begin
  FL.PrettyPrint;
  write(S);
  if Assigned(FR) then
    FR.PrettyPrint;
end;

function TExpr.TypeCheck(out Msg: String): boolean;
begin
  result := FL.TypeCheck(Msg);

  if result and Assigned(FR) then
    result := FR.TypeCheck(Msg);
end;

{ TAbstractSyntaxTreeBase }

constructor TAbstractSyntaxTreeBase.Create;
begin
  FLineNo := yylineno;
  FColNo := yycolno;
  FLine := yyline;
end;

procedure TAbstractSyntaxTreeBase.PrettyPrint;
begin
  writeln('(', ClassName, ') Text: ', FLine, ' LineNo: ', FLineNo, ' ColNo: ', FColNo);
end;

function TAbstractSyntaxTreeBase.ResultType: TParserResultType;
begin
  // Default result type is rtUndefined
  result := rtUndefined;
end;

function TAbstractSyntaxTreeBase.TypeCheck(out Msg: String): boolean;
begin
  // All classes should override this method!
  Msg := 'Type Check Failed -> Reached "TAbstractSyntaxTreeBase" which it should not have done!';
  result := false;
end;

{ TVariable }

constructor TVariable.Create(const Ident: string);
begin
  inherited Create;
  FIdent := Ident;
end;

procedure TVariable.PrettyPrint;
begin
  write(' ', FIdent);
end;

function TVariable.TypeCheck(out Msg: String): boolean;
begin
  Msg := '';
  Result := true;
end;

function TVariable.ResultType: TParserResultType;
begin
  if Assigned(ASTTypeTable) then
    Result := ASTTypeTable.VariableType(FIdent);


  if (Result = rtUndefined) and
     (Assigned(OnGetIdentType))
  then
    Result := OnGetIdentType(FIdent)
end;

{ TVarList }

constructor TVarList.Create(const Variable: TVariable; const VarList: TVarList);
begin
  inherited Create;
  FVariable := Variable;
  FVarList := VarList;
end;

procedure TVarList.PrettyPrint;
begin
  FVariable.PrettyPrint;

  if Assigned(FVarList) then
    FVarList.PrettyPrint;
end;

function TVarList.TypeCheck(out Msg: String): boolean;
begin
  Result := FVariable.TypeCheck(Msg);

  if result and Assigned(FVarList) then
    result := FVarList.TypeCheck(Msg);
end;


{ TStatement }

constructor TStatement.Create(const StatementType: word; const VarList: TVarList
  );
begin
  inherited Create;
  FStatementType := StatementType;
  FVarList := VarList;
end;

procedure TStatement.PrettyPrint;
begin
  if Assigned(FVarList) then
    FVarList.PrettyPrint;
end;

function TStatement.TypeCheck(out Msg: String): boolean;
begin
  Result := FVarList.TypeCheck(Msg);
end;

{ TStatementList }

constructor TStatementList.Create(const Statement: TCustomStatement;
  const StatementList: TStatementList);
begin
  inherited Create;
  FStatement := Statement;
  FStatementList := StatementList;
end;

procedure TStatementList.PrettyPrint;
begin
  if FStatement is TStatementList then
    writeln('begin');

  FStatement.PrettyPrint;

  if FStatement is TStatementList then
    write('end');

  writeln(';');
  if Assigned(FStatementList) then
    FStatementList.PrettyPrint;
end;

function TStatementList.TypeCheck(out Msg: String): boolean;
begin
  Result := FStatement.TypeCheck(Msg);

  if Result and Assigned(FStatementList) then
    result := FStatementList.TypeCheck(Msg);
end;

end.

