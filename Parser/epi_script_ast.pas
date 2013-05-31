unit epi_script_AST;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,
  lexlib,
  epi_parser_types,
  epidatafilestypes,
  epidatafiles;

type
  TExpr = class;
  TCustomVariable = class;

  { IEpiScriptParser }

  IEpiScriptParser = interface ['IEpiScriptParser']
    procedure ParseError(Const Msg: string; Const LineNo, ColNo: integer;
      Const TextFound: string);
    function  VariableExists(Const Ident: string): boolean;
    procedure AddVariable(Const Variable: TCustomVariable);
    function  FindVariable(Const Ident: string): TCustomVariable;
  end;

  IEpiScriptExecutor = interface ['IEpiScriptExecutor']
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: Variant);
    function GetFieldValue(Const Sender: TObject; Const F: TEpiField): Variant;
  end;

  { TAbstractSyntaxTreeBase }

  TAbstractSyntaxTreeBase = class(TObject)
  private
    FLineNo: integer;
    FColNo: integer;
    FLine: string;
  protected
    constructor Create; virtual;
    procedure DoTypeCheckError(Const Msg: String; Parser: IEpiScriptParser);
    procedure DoTypeCheckError(Const Msg: String; Const Args: Array of const; Parser: IEpiScriptParser);
  public
    destructor Destroy; override;
    function ResultType: TParserResultType; virtual;
    function TypeCheck(Parser: IEpiScriptParser): boolean; virtual;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property Expr: TExpr read FExpr;
    property ThenStatement: TCustomStatement read FThenStatement;
    property ElseStatement: TCustomStatement read FElseStatement;
  end;

  { TGoto }

  TGoto = class(TCustomStatement)
  private
    FOption: TGotoOption;
    FVariable: TCustomVariable;
  public
    constructor Create(Const Variable: TCustomVariable;
      Const Option: TGotoOption);
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property Variable: TCustomVariable read FVariable;
    property Option: TGotoOption read FOption;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
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
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TExpr)
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TRelationalExpr }

  TRelationalExpr = class(TExpr)
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
  end;

  { TCustomVariable }

  TCustomVariable = class(TExpr)
  protected
    FIdent: string;
  public
    class function FindVariable(Const Ident: String;
      Parser: IEpiScriptParser): TCustomVariable;
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property Ident: string read FIdent;
  public
    procedure SetInteger(Const Value: EpiInteger); virtual; abstract;
    procedure SetFloat(Const Value: EpiFloat); virtual; abstract;
    procedure SetBoolean(Const Value: Boolean); virtual; abstract;
  end;

  { TFieldVariable }

  TFieldVariable = class(TCustomVariable)
  private
    FParser: IEpiScriptExecutor;
    FField: TEpiField;
  public
    constructor Create(Const Field: TEpiField;
      Parser: IEpiScriptExecutor);
    function ResultType: TParserResultType; override;
    property Field: TEpiField read FField;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
    procedure SetInteger(Const Value: EpiInteger); override;
    procedure SetFloat(Const Value: EpiFloat); override;
    procedure SetBoolean(Const Value: Boolean); override;
  end;

  { TScriptVariable }

  TScriptVariable = class(TCustomVariable)
  private
    FResultType: TParserResultType;
    FIntValue: EpiInteger;
    FFloatValue: EpiFloat;
    FBoolValue: Boolean;
  public
    constructor Create(Const AIdent: string; AResultType: TParserResultType);
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsBoolean: Boolean; override;
    procedure SetInteger(Const Value: EpiInteger); override;
    procedure SetFloat(Const Value: EpiFloat); override;
    procedure SetBoolean(Const Value: Boolean); override;
  end;

  { TVarList }

  TVarList = class(TAbstractSyntaxTreeBase)
  private
    FVariable: TCustomVariable;
    FVarList: TVarList;
  public
    constructor Create(Const Variable: TCustomVariable; Const VarList: TVarList);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property Variable: TCustomVariable read FVariable;
    property VarList: TVarList read FVarList;
  end;

  { TDefine }

  TDefine = class(TCustomStatement)
  private
    FIdent: string;
    FType: TParserResultType;
  public
    constructor Create(Const DefineType: TParserResultType;
      Const Ident: string; Const Parser: IEpiScriptParser);
    constructor Create(Const DefineType: TParserResultType;
      IdentList: array of IdString; Const Parser: IEpiScriptParser);
    destructor Destroy; override;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
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
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property Statement: TCustomStatement read FStatement;
    property StatementList: TStatementList read FStatementList;
  end;

implementation

uses
  YaccLib, epi_parser_core, math, variants;

resourcestring
  rsExpressionReturnType1 = 'Expression return type must be %s';
  rsExpressionReturnType2 = 'Expression return type must be %s or %s';

{ TGoto }

constructor TGoto.Create(const Variable: TCustomVariable;
  const Option: TGotoOption);
begin
  inherited Create;
  FVariable := Variable;
  FOption := Option;
end;

function TGoto.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Assigned(Variable) and
     (not (Variable is TFieldVariable))
  then
  begin
    DoTypeCheckError('Variable %1 is not a Field', [Variable.Ident], Parser);
    result := false;
  end;
end;


{ TAbstractSyntaxTreeBase }

constructor TAbstractSyntaxTreeBase.Create;
begin
  FLineNo := yylineno;
  FColNo := yycolno;
  FLine := yyline;
end;

procedure TAbstractSyntaxTreeBase.DoTypeCheckError(const Msg: String;
  Parser: IEpiScriptParser);
begin
  Parser.ParseError(
    Msg,
    FLineNo,
    FColNo,
    FLine
  );
end;

procedure TAbstractSyntaxTreeBase.DoTypeCheckError(const Msg: String;
  const Args: array of const; Parser: IEpiScriptParser);
begin
  DoTypeCheckError(
    Format(Msg, Args),
    Parser
  );
end;

destructor TAbstractSyntaxTreeBase.Destroy;
begin
  FLine := '';
  inherited Destroy;
end;

function TAbstractSyntaxTreeBase.ResultType: TParserResultType;
begin
  // Default result type is rtUndefined
  result := rtUndefined;
end;

function TAbstractSyntaxTreeBase.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  result := true;
end;

{ TScriptVariable }

constructor TScriptVariable.Create(const AIdent: string;
  AResultType: TParserResultType);
begin
  inherited Create(otVariable, nil, nil);
  FIdent := AIdent;
  FResultType := AResultType;
end;

function TScriptVariable.ResultType: TParserResultType;
begin
  Result := FResultType;
end;

function TScriptVariable.AsInteger: EpiInteger;
begin
  case FResultType of
    rtInteger: result := FIntValue;
    rtFloat:   result := Trunc(SimpleRoundTo(FFloatValue, 0));
    rtBoolean: Result := Integer(FBoolValue);
  end;
end;

function TScriptVariable.AsFloat: EpiFloat;
begin
  case FResultType of
    rtInteger: result := FIntValue;
    rtFloat:   result := FFloatValue;
    rtBoolean: if FBoolValue then
                 result := 1
               else
                 result := 0;
  end;
end;

function TScriptVariable.AsBoolean: Boolean;
begin
  case FResultType of
    rtInteger: result := (FIntValue <> 0);
    rtFloat:   result := (FFloatValue <> 0);
    rtBoolean: Result := FBoolValue;
  end;
end;

procedure TScriptVariable.SetInteger(const Value: EpiInteger);
begin
  case FResultType of
    rtInteger: FIntValue := Value;
    rtFloat:   FFloatValue := Value;
    rtBoolean: FBoolValue := Boolean(Value);
  end;
end;

procedure TScriptVariable.SetFloat(const Value: EpiFloat);
begin
  case FResultType of
    rtInteger: FIntValue := Trunc(SimpleRoundTo(Value, 0));
    rtFloat:   FFloatValue := Value;
    rtBoolean: FBoolValue := Boolean(Trunc(Value));
  end;
end;

procedure TScriptVariable.SetBoolean(const Value: Boolean);
begin

  case FResultType of
    rtInteger: FIntValue := Integer(Value);
    rtFloat:   FFloatValue := Integer(Value);
    rtBoolean: FBoolValue := Value;
  end;
end;

{ TFieldVariable }

constructor TFieldVariable.Create(const Field: TEpiField;
  Parser: IEpiScriptExecutor);
begin
  inherited Create(otVariable, nil, nil);
  FField := Field;
  FParser := Parser;
  FIdent := FField.Name;
end;

function TFieldVariable.ResultType: TParserResultType;
const
  FieldTypeToParserType: array[TEpiFieldType] of TParserResultType =
    (
//    ftBoolean,
      rtBoolean,

//    ftInteger, ftAutoInc, ftFloat,
      rtInteger, rtInteger, rtFloat,

//    ftDMYDate, ftMDYDate, ftYMDDate,
      rtInteger, rtInteger, rtInteger,

//    ftDMYAuto, ftMDYAuto, ftYMDAuto,
      rtInteger, rtInteger, rtInteger,

//    ftTime, ftTimeAuto,
      rtFloat, rtFloat,

//    ftString, ftUpperString
      rtString, rtString
    );
begin
  Result := FieldTypeToParserType[FField.FieldType];
end;

function TFieldVariable.AsInteger: EpiInteger;
var
  V: Variant;
begin
  V := FParser.GetFieldValue(Self, FField);
  if VarIsStr(V) and
     (V = '')
  then
    Result := TEpiIntField.DefaultMissing
  else
    Result := EpiInteger(V);
end;

function TFieldVariable.AsFloat: EpiFloat;
var
  V: Variant;
begin
  if VarIsStr(V) and
     (V = '')
  then
    Result := TEpiFloatField.DefaultMissing
  else
    Result := EpiFloat(V);
end;

function TFieldVariable.AsBoolean: Boolean;
var
  V: Variant;
begin
  if VarIsStr(V) and
     (V = '')
  then
    Result := Boolean(TEpiBoolField.DefaultMissing)
  else
    Result := Boolean(V);
end;

procedure TFieldVariable.SetInteger(const Value: EpiInteger);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;

procedure TFieldVariable.SetFloat(const Value: EpiFloat);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;

procedure TFieldVariable.SetBoolean(const Value: Boolean);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;


{ TRelationExpr }

function TRelationalExpr.TypeCheck(Parser: IEpiScriptParser): boolean;
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
  result := inherited TypeCheck(Parser);

  if result then
  begin
    Result := RelationalOperationCheck[FL.ResultType, FR.ResultType];

    if not result then
    begin
      DoTypeCheckError(
        'Left and Rigth cannot be compared. Incompatible types.',
        Parser
      );
    end;
  end;
end;

function TRelationalExpr.ResultType: TParserResultType;
begin
  Result := rtBoolean;
end;

function TRelationalExpr.AsInteger: EpiInteger;
begin
  Result := Integer(AsBoolean);
end;

function TRelationalExpr.AsFloat: EpiFloat;
begin
  Result := AsInteger;
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

function TBinaryExpr.TypeCheck(Parser: IEpiScriptParser): boolean;
var
  Lr: TParserResultType;
  Rr: TParserResultType;
begin
  Result := inherited TypeCheck(Parser);
  Lr := FL.ResultType;
  Rr := FR.ResultType;

  if result then
  case FOp of
    otAnd,
    otOr:
    begin
      result := (Lr = rtBoolean) and
                (Rr = rtBoolean);
      if not (Lr = rtBoolean) then
        DoTypeCheckError(
          'Left ' + rsExpressionReturnType1,
          [SParserResultType[rtBoolean]],
          Parser
        );
      if not (Rr = rtBoolean) then
        DoTypeCheckError(
          'Right ' + rsExpressionReturnType1,
          [SParserResultType[rtBoolean]],
          Parser
        );
    end;

    otMod,
    otDiv:
    begin
      result := (Lr = rtInteger) and
                (Rr = rtInteger);
      if not (Lr = rtInteger) then
        DoTypeCheckError(
          'Left ' + rsExpressionReturnType1,
          [SParserResultType[rtInteger]],
          Parser
        );
      if not (Rr = rtInteger) then
        DoTypeCheckError(
          'Right ' + rsExpressionReturnType1,
          [SParserResultType[rtInteger]],
          Parser
        );
    end;
    otMult,
    otPlus,
    otMinus,
    otDivide:
    begin
      result := (Lr in [rtInteger, rtFloat]) and
                (Rr in [rtInteger, rtFloat]);
      if not (Lr in [rtInteger, rtFloat]) then
        DoTypeCheckError(
          'Left ' + rsExpressionReturnType1,
          [SParserResultType[rtFloat]],
          Parser
        );
      if not (Rr in [rtInteger, rtFloat]) then
        DoTypeCheckError(
          'Right ' + rsExpressionReturnType1,
          [SParserResultType[rtFloat]],
          Parser
        );
    end;
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
var
  Tmp: EpiInteger;
begin
  case Operation of
    otPlus:  result := Left.AsInteger + Right.AsInteger;
    otMinus: result := Left.AsInteger - Right.AsInteger;
    otMult:  result := Left.AsInteger * Right.AsInteger;
    otDiv:
    begin
      // Catch ZeroDivide before is happens
      Tmp := Right.AsInteger;
      if Tmp = 0 then
        result := inherited AsInteger
      else
        result := Left.AsInteger div Tmp;
    end;
    otMod:   result := Left.AsInteger mod Right.AsInteger;
  else
    result := inherited AsInteger;
  end;
end;

function TBinaryExpr.AsFloat: EpiFloat;
var
  Tmp: EpiFloat;
begin
  case Operation of
    otPlus:   result := Left.AsFloat + Right.AsFloat;
    otMinus:  result := Left.AsFloat - Right.AsFloat;
    otMult:   result := Left.AsFloat * Right.AsFloat;
    otDivide:
    begin
      // Catch ZeroDivide before is happens
      Tmp := Right.AsFloat;
      if Tmp = 0 then
        result := inherited AsFloat
      else
        result := Left.AsFloat / Tmp;
    end;
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

function TUnaryExpr.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if result then
  case FOp of
    otNot:
      begin
        result := Left.ResultType = rtBoolean;
        if not Result then
          DoTypeCheckError(
            rsExpressionReturnType1,
            [SParserResultType[rtBoolean]],
            Parser
          );
      end;
    otMinus:
      begin
        result := Left.ResultType in [rtFloat, rtInteger];
        if not Result then
          DoTypeCheckError(
            rsExpressionReturnType1,
            [SParserResultType[rtFloat]],
            Parser
          );
      end;
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
  const Ident: string; const Parser: IEpiScriptParser);
begin
  inherited Create;
  FType := DefineType;
  FIdent := Ident;

  if Parser.VariableExists(Ident) then
  begin
    yyerror('Variable "' + Ident + '" already defined');
    yyabort
  end;

  Parser.AddVariable(TScriptVariable.Create(Ident, DefineType));
end;

constructor TDefine.Create(const DefineType: TParserResultType;
  IdentList: array of IdString; const Parser: IEpiScriptParser);
var
  i: Integer;
begin
  inherited Create;
  FType := DefineType;
//  FIdent := Ident;

  for i := Low(IdentList) to High(IdentList) do
  begin
    if Parser.VariableExists(IdentList[i]) then
    begin
      yyerror('Variable "' + IdentList[i] + '" already defined');
      yyabort
    end;
    Parser.AddVariable(TScriptVariable.Create(IdentList[i], DefineType));
  end;
end;

destructor TDefine.Destroy;
begin
  FIdent := '';
  inherited Destroy;
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

function TAssignment.TypeCheck(Parser: IEpiScriptParser): boolean;
var
  VarT: TParserResultType;
  ExpT: TParserResultType;
begin
  Result := FVAriable.TypeCheck(Parser) and FExpr.TypeCheck(Parser);

  if Result then
  begin
    VarT := FVAriable.ResultType;
    ExpT := FExpr.ResultType;
    Result := Ord(VarT) >= Ord(ExpT);

    if not result then
      DoTypeCheckError(
        'Incompatible types: ' + LineEnding +
        'Variable ' + FVAriable.FIdent + ' expect result to be of type ' +  SParserResultType[VarT] + LineEnding +
        'but expression is of type ' + SParserResultType[ExpT],
        Parser
      );
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

function TIfThen.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := (Expr.TypeCheck(Parser));

  if result and
     (not (Expr.ResultType = rtBoolean))
  then
    DoTypeCheckError(
      rsExpressionReturnType1,
      [SParserResultType[rtBoolean]],
      Parser
    );

  if Result and Assigned(ThenStatement) then
    result := ThenStatement.TypeCheck(Parser);

  if Result and Assigned(ElseStatement) then
    result := ElseStatement.TypeCheck(Parser);
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

function TLiteral.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  result := inherited TypeCheck(Parser);
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

function TTypeCast.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);
end;

function TTypeCast.ResultType: TParserResultType;
begin
  Case FOp of
    otIntegerCast: result := rtInteger;
    otFloatCast:   result := rtFloat;
    otBoolCast:    result := rtBoolean;
  end;
end;

function TTypeCast.AsInteger: EpiInteger;
begin
  Case FOp of
    otFloatCast:   ; // Get's caught in TypeChecking.
    otIntegerCast: result := Left.AsInteger;
    otBoolCast:    result := Integer(AsBoolean);
  end;
end;

function TTypeCast.AsFloat: EpiFloat;
begin
  Case FOp of
    otFloatCast:   result := Left.AsFloat;
    otIntegerCast: result := AsInteger;
    otBoolCast:    result := AsInteger;
  end;
end;

function TTypeCast.AsBoolean: Boolean;
begin
  Case FOp of
    otFloatCast:   ; // Get's caught in TypeChecking.
    otIntegerCast: ; // Get's caught in TypeChecking.
    otBoolCast:    result := Left.AsBoolean;
  end;
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

function TExpr.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  result := inherited TypeCheck(Parser);

  if Assigned(FL) then
    result := FL.TypeCheck(Parser);

  if result and Assigned(FR) then
    result := FR.TypeCheck(Parser);
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

{ TCustomVariable }

class function TCustomVariable.FindVariable(const Ident: String;
  Parser: IEpiScriptParser): TCustomVariable;
begin
  if not Parser.VariableExists(Ident) then
  begin
    yyerror('Variable "' + Ident + '" not defined');
    yyabort;
    exit;
  end;

  Result := Parser.FindVariable(Ident);
end;

destructor TCustomVariable.Destroy;
begin
  FIdent := '';
  inherited Destroy;
end;

function TCustomVariable.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  result := inherited TypeCheck(Parser);
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

function TVarList.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result :=
    inherited TypeCheck(Parser);
    FVariable.TypeCheck(Parser);

  if result and Assigned(FVarList) then
    result := FVarList.TypeCheck(Parser);
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

function TStatement.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result :=
    inherited TypeCheck(Parser) and
    FVarList.TypeCheck(Parser);
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

function TStatementList.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Result and Assigned(FStatement) then
    result := FStatement.TypeCheck(Parser);

  if Result and Assigned(FStatementList) then
    result := FStatementList.TypeCheck(Parser);
end;

end.

