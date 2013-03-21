unit AST;

interface

uses
  Classes, SysUtils,
  lexlib,
  parser_types,
  epidatafilestypes,
  epidatafiles;

type
  TExpr = class;
  TCustomVariable = class;
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
    destructor Destroy; override;
    procedure PrettyPrint; virtual;
    function ResultType: TParserResultType; virtual;
    function TypeCheck(out Msg: String): boolean; virtual;
    property LineNo: integer read FLineNo;
    property ColNo: integer read FColNo;
    property Line: string read FLine;
  end;

  TCustomStatement = class(TAbstractSyntaxTreeBase);

  { TAssignment }

  TAssignment = class(TCustomStatement)
  private
    FVAriable: TCustomVariable;
    FExpr: TExpr;
  public
    constructor Create(Const Variable: TCustomVariable; Const Expr: TExpr);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Variable: TCustomVariable read FVAriable;
    property Expr: TExpr read FExpr;
  end;

  { TIfThen }

  TIfThen = class(TCustomStatement)
  private
    FExpr: TExpr;
    FThenStatement: TCustomStatement;
    FElseStatement: TCustomStatement;
  public
    constructor Create(Const Expr: TExpr; Const ThenStatement: TCustomStatement;
      Const ElseStatement: TCustomStatement);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Expr: TExpr read FExpr;
    property ThenStatement: TCustomStatement read FThenStatement;
    property ElseStatement: TCustomStatement read FElseStatement;
  end;


  { TExpr }

  TExpr = class(TAbstractSyntaxTreeBase)
  private
    FOp: TParserOperationType;
    FL:  TExpr;
    FR:  TExpr;
  protected
    function CommonType(Const A, B: TExpr): TParserResultType;
  public
    constructor Create(Const Op: TParserOperationType; Const L, R: TExpr);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Operation: TParserOperationType read FOp;
    property Left: TExpr read FL;
    property Right: TExpr read FR;
  public
    function AsInteger: EpiInteger; virtual;
    function AsFloat: EpiFloat; virtual;
    function AsBoolean: Boolean; virtual;
  end;

  { TTypeCast }

  TTypeCast = class(TExpr)
  private
    function GetExpr: TExpr;
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
    property Expr: TExpr read GetExpr;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TLiteral }

  TLiteral = class(TExpr)
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
    property ValueType: TParserResultType read FValueType;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;


  { TUnaryExpr }

  TUnaryExpr = class(TExpr)
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TExpr)
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TRelationalExpr }

  TRelationalExpr = class(TExpr)
  public
    function TypeCheck(out Msg: String): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: Boolean; override;
  end;

  { TCustomVariable }

  TCustomVariable = class(TExpr)
  private
    FIdent: string;
  public
    class function CreateVariable(Const Ident: String): TCustomVariable;
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Ident: string read FIdent;
  end;

  { TFieldVariable }

  TFieldVariable = class(TCustomVariable)
  private
    FField: TEpiField;
  public
    constructor Create(Const Field: TEpiField);
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TDefineVariable }

  TDefineVariable = class(TCustomVariable)
  public
    constructor Create();
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TVarList }

  TVarList = class(TAbstractSyntaxTreeBase)
  private
    FVariable: TCustomVariable;
    FVarList: TVarList;
  public
    constructor Create(Const Variable: TCustomVariable; Const VarList: TVarList);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Variable: TCustomVariable read FVariable;
    property VarList: TVarList read FVarList;
  end;

  { TDefine }

  TDefine = class(TCustomStatement)
  private
    FIdent: string;
    FType: TParserResultType;
  public
    constructor Create(Const DefineType: TParserResultType; Const Ident: string);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Ident: string read FIdent;
    property IdentType: TParserResultType read FType;
  end;

  { TStatement }

  TStatement = class(TCustomStatement)
  private
    FStatementType: integer;
    FVarList: TVarList;
  public
    constructor Create(Const StatementType: word; Const VarList: TVarList);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property StatementType: Integer read FStatementType;
    property VarList: TVarList read FVarList;
  end;

  { TStatementList }

  TStatementList = class(TCustomStatement)
  private
    FStatement: TCustomStatement;
    FStatementList: TStatementList;
  public
    constructor Create(Const Statement: TCustomStatement; Const StatementList: TStatementList);
    destructor Destroy; override;
    procedure PrettyPrint; override;
    function TypeCheck(out Msg: String): boolean; override;
    property Statement: TCustomStatement read FStatement;
    property StatementList: TStatementList read FStatementList;
  end;

implementation

uses
  YaccLib, typetable, parser_core, math;

var
  ASTTypeTable: TTypeTable = nil;

{ TDefineVariable }

constructor TDefineVariable.Create;
begin
  inherited Create(otVariable, nil, nil);
end;

function TDefineVariable.ResultType: TParserResultType;
begin
  Result := inherited ResultType;
end;

function TDefineVariable.AsInteger: EpiInteger;
begin
  Result := inherited AsInteger;
end;

function TDefineVariable.AsFloat: EpiFloat;
begin
  Result := inherited AsFloat;
end;

function TDefineVariable.AsBoolean: Boolean;
begin
  Result := inherited AsBoolean;
end;

{ TFieldVariable }

constructor TFieldVariable.Create(const Field: TEpiField);
begin
  inherited Create(otVariable, nil, nil);
  FField := Field;
end;

function TFieldVariable.ResultType: TParserResultType;
begin
  Result := OnGetIdentType(Ident);
end;

function TFieldVariable.AsInteger: EpiInteger;
begin
  Result := FField.AsInteger[0];
end;

function TFieldVariable.AsFloat: EpiFloat;
begin
  Result := FField.AsFloat[0];
end;

function TFieldVariable.AsBoolean: Boolean;
begin
  Result := Boolean(FField.AsBoolean[0]);
end;


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

function TRelationalExpr.AsBoolean: Boolean;
var
  CType: TParserResultType;
begin
  CType := CommonType(Left, Right);
  case CType of
    rtBoolean:
    case Operation of
      otEQ:  result := Left.AsBoolean = Right.AsBoolean;
      otNEQ: result := Left.AsBoolean <> Right.AsBoolean;
    end;

    rtInteger:
    case Operation of
      otEQ:  result := Left.AsInteger = Right.AsInteger;
      otNEQ: result := Left.AsInteger <>Right.AsInteger;
      otLT:  result := Left.AsInteger < Right.AsInteger;
      otLTE: result := Left.AsInteger <= Right.AsInteger;
      otGT:  result := Left.AsInteger > Right.AsInteger;
      otGTE: result := Left.AsInteger >= Right.AsInteger;
    end;

    rtFloat:
    case Operation of
      otEQ:  result := SameValue(Left.AsFloat, Right.AsFloat, 0.0);
      otNEQ: result := not SameValue(Left.AsFloat, Right.AsFloat, 0.0);
      otLT:  result := Left.AsFloat < Right.AsFloat;
      otLTE: result := (Left.AsFloat < Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
      otGT:  result := Left.AsFloat > Right.AsFloat;
      otGTE: result := (Left.AsFloat < Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
    end;

{    rtString:
    case Operation of
      otEQ:  result := FL.AsS;
      otNEQ: ;
      otLT: ;
      otLTE: ;
      otGT: ;
      otGTE: ;
    end;}

{    rtObject:
    case Operation of
      otEQ: ;
      otNEQ: ;
      otLT: ;
      otLTE: ;
      otGT: ;
      otGTE: ;
    end;}

//    rtUndefined: ;
  end;
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
begin
  case FOp of
    otAnd,
    otOr:
      Result := rtBoolean;
    otMod,
    otDiv:
      Result := rtInteger;
    otMult,
    otPlus,
    otMinus:
      result := CommonType(FL, FR);
    otDivide:
      result := rtFloat;
{      otShl,
      otShr,
      otXor,
      ;}
  end;
end;

function TBinaryExpr.AsInteger: EpiInteger;
begin
  case Operation of
    otPlus:  result := Left.AsInteger + Right.AsInteger;
    otMinus: result := Left.AsInteger - Right.AsInteger;
    otMult:  result := Left.AsInteger * Right.AsInteger;
    otDiv:   result := Left.AsInteger div Right.AsInteger;
    otMod:   result := Left.AsInteger mod Right.AsInteger;
  else
    result := inherited AsInteger;
  end;
end;

function TBinaryExpr.AsFloat: EpiFloat;
begin
  case Operation of
    otPlus:   result := Left.AsFloat + Right.AsFloat;
    otMinus:  result := Left.AsFloat - Right.AsFloat;
    otMult:   result := Left.AsFloat * Right.AsFloat;
    otDivide: result := Left.AsFloat / Right.AsFloat;
  else
    result := inherited AsFloat;
  end;
end;

function TBinaryExpr.AsBoolean: Boolean;
begin
  case Operation of
    otOr:  result := Left.AsBoolean or Right.AsBoolean;
    otAnd: result := Left.AsBoolean and Right.AsBoolean;
  else
    result := inherited AsBoolean;
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

function TUnaryExpr.AsInteger: EpiInteger;
begin
  case Operation of
    otMinus: result := -Left.AsInteger;
  end;
end;

function TUnaryExpr.AsFloat: EpiFloat;
begin
  case Operation of
    otMinus: result := -Left.AsFloat;
  end;
end;

function TUnaryExpr.AsBoolean: Boolean;
begin
  case Operation of
    otNot: result := (not Left.AsBoolean);
  end;
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

destructor TDefine.Destroy;
begin
  FIdent := '';
  inherited Destroy;
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

constructor TAssignment.Create(const Variable: TCustomVariable; const Expr: TExpr);
begin
  inherited Create;
  FExpr := Expr;
  FVAriable := Variable;
end;

destructor TAssignment.Destroy;
begin
  FExpr.Free;
  FVAriable.Free;
  inherited Destroy;
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

{ TIfThen }

constructor TIfThen.Create(const Expr: TExpr;
  const ThenStatement: TCustomStatement; const ElseStatement: TCustomStatement);
begin
  inherited Create;
  FExpr := Expr;
  FThenStatement := ThenStatement;
  FElseStatement := ElseStatement;
end;

destructor TIfThen.Destroy;
begin
  FExpr.Free;
  FThenStatement.Free;
  FElseStatement.Free;
  inherited Destroy;
end;

procedure TIfThen.PrettyPrint;
begin
end;

function TIfThen.TypeCheck(out Msg: String): boolean;
begin
  Result := (Expr.TypeCheck(Msg)) and
            (Expr.ResultType = rtBoolean) and
            (ThenStatement.TypeCheck(Msg));

  if Result and Assigned(ElseStatement) then
    result := ElseStatement.TypeCheck(Msg);
end;

{ TLiteral }

constructor TLiteral.Create(const Value: Integer);
begin
  inherited Create(otNumber, nil, nil);
  FValueType := rtInteger;
  FIntVal := Value;
end;

constructor TLiteral.Create(const Value: Extended);
begin
  inherited Create(otFloat, nil, nil);
  FValueType := rtFloat;
  FExtVal := Value;
end;

constructor TLiteral.Create(const Value: Boolean);
begin
  inherited Create(otNumber, nil, nil);
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

function TLiteral.AsInteger: EpiInteger;
begin
  case FValueType of
    rtInteger: result := FIntVal;
    rtFloat:   result := Trunc(SimpleRoundTo(FExtVal, 0));
    rtBoolean: Result := Integer(FBoolVal);
  end;
end;

function TLiteral.AsFloat: EpiFloat;
begin
  case FValueType of
    rtInteger: result := FIntVal;
    rtFloat:   result := FExtVal;
    rtBoolean: if FBoolVal then
                 result := 1
               else
                 result := 0;
  end;
end;

function TLiteral.AsBoolean: Boolean;
begin
  case FValueType of
    rtInteger: result := (FIntVal <> 0);
    rtFloat:   result := (FExtVal <> 0);
    rtBoolean: Result := FBoolVal;
  end;
end;

{ TTypeCast }

function TTypeCast.GetExpr: TExpr;
begin
  Result := Left;
end;

function TTypeCast.TypeCheck(out Msg: String): boolean;
begin
  Result := Expr.TypeCheck(Msg);

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

function TTypeCast.AsInteger: EpiInteger;
begin
  Result := inherited AsInteger;
end;

function TTypeCast.AsFloat: EpiFloat;
begin
  Result := inherited AsFloat;
end;

function TTypeCast.AsBoolean: Boolean;
begin
  Result := inherited AsBoolean;
end;

{ TExpr }

function TExpr.CommonType(const A, B: TExpr): TParserResultType;
begin
  result := TParserResultType(Math.Max(Ord(A.ResultType), Ord(B.ResultType)));
end;

constructor TExpr.Create(const Op: TParserOperationType; const L, R: TExpr);
begin
  inherited Create;
  FOp := Op;
  FL := L;
  FR := R;
end;

destructor TExpr.Destroy;
begin
  FL.Free;
  FR.Free;
  inherited Destroy;
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
  result := true;

  if Assigned(FL) then
    result := FL.TypeCheck(Msg);

  if result and Assigned(FR) then
    result := FR.TypeCheck(Msg);
end;

function TExpr.AsInteger: EpiInteger;
begin
  result := TEpiIntField.DefaultMissing;
end;

function TExpr.AsFloat: EpiFloat;
begin
  result := TEpiFloatField.DefaultMissing;
end;

function TExpr.AsBoolean: Boolean;
begin
  result := Boolean(TEpiBoolField.DefaultMissing);
end;

{ TAbstractSyntaxTreeBase }

constructor TAbstractSyntaxTreeBase.Create;
begin
  FLineNo := yylineno;
  FColNo := yycolno;
  FLine := yyline;
end;

destructor TAbstractSyntaxTreeBase.Destroy;
begin
  FLine := '';
  inherited Destroy;
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

{ TCustomVariable }

class function TCustomVariable.CreateVariable(const Ident: String
  ): TCustomVariable;
begin
  if (not ASTTypeTable.VariableExists(Ident)) and
     (not (OnGetIdentType(Ident) <> rtUndefined))
  then
  begin
    yyerror('Variable "' + Ident + '" not defined or not in scope');
    yyabort;
    Exit;
  end;

  if ASTTypeTable.VariableExists(Ident) then
    Result := TDefineVariable.Create();

  if OnGetIdentType(Ident) <> rtUndefined then
    Result := TFieldVariable.Create(nil);

  Result.FIdent := Ident;
end;

destructor TCustomVariable.Destroy;
begin
  FIdent := '';
  inherited Destroy;
end;

procedure TCustomVariable.PrettyPrint;
begin
  write(' ', FIdent);
end;

function TCustomVariable.TypeCheck(out Msg: String): boolean;
begin
  Msg := '';
  Result := true;
end;

{ TVarList }

constructor TVarList.Create(const Variable: TCustomVariable; const VarList: TVarList);
begin
  inherited Create;
  FVariable := Variable;
  FVarList := VarList;
end;

destructor TVarList.Destroy;
begin
  FVariable.Free;
  FVarList.Free;
  inherited Destroy;
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

destructor TStatement.Destroy;
begin
  FVarList.Free;
  inherited Destroy;
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

destructor TStatementList.Destroy;
begin
  FStatement.Free;
  FStatementList.Free;
  inherited Destroy;
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

