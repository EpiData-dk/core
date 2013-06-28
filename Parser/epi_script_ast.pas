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
  TAbstractSyntaxTreeBase = class;
  TExpr = class;
  TCustomVariable = class;
  TParamList = class;
  TFunction = class;

var
  ASTCurrentExecutionObject: TAbstractSyntaxTreeBase;

type

  { IEpiScriptParser }

  IEpiScriptParser = interface ['IEpiScriptParser']
    procedure ParseError(Const Msg: string; Const LineNo, ColNo: integer;
      Const TextFound: string);
    function  VariableExists(Const Ident: string): boolean;
    procedure AddVariable(Const Variable: TCustomVariable);
    function  FindVariable(Const Ident: string): TCustomVariable;
    function  CreateFunction(Const FunctionName: string;
      Const ParamList: TParamList): TFunction;
  end;

  IEpiScriptExecutor = interface ['IEpiScriptExecutor']
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiBool); overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiInteger); overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiFloat); overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiString); overload;
    function GetFieldValueBool(Const Sender: TObject; Const F: TEpiField): EpiBool;
    function GetFieldValueInt(Const Sender: TObject; Const F: TEpiField): EpiInteger;
    function GetFieldValueFloat(Const Sender: TObject; Const F: TEpiField): EpiFloat;
    function GetFieldValueString(Const Sender: TObject; Const F: TEpiField): EpiString;
  end;

  { TAbstractSyntaxTreeBase }

  TAbstractSyntaxTreeBase = class(TObject, IFPObserver)
  private
    FLineNo: integer;
    FColNo: integer;
    FLine: string;
    FObservedList: TFpList;
  protected
    constructor Create; virtual;
    procedure DoTypeCheckError(Const Msg: String; Parser: IEpiScriptParser);
    procedure DoTypeCheckError(Const Msg: String; Const Args: Array of const; Parser: IEpiScriptParser);
  { Observer / IFPObserver }
  protected
    procedure ObserveObject(O: TObject);
    procedure DoObservedChange(Sender: TObject); virtual; abstract;
  public
    procedure FPOObservedChanged(ASender: TObject;
       Operation: TFPObservedOperation; Data: Pointer);
  public
    destructor Destroy; override;
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
  protected
    procedure DoObservedChange(Sender: TObject); override;
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
  protected
    procedure DoObservedChange(Sender: TObject); override;
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
  protected
    procedure DoObservedChange(Sender: TObject); override;
  public
    constructor Create(Const Variable: TCustomVariable;
      Const Option: TGotoOption);
    destructor Destroy; override;
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
    procedure DoObservedChange(Sender: TObject); override;
  public
    constructor Create(Const Op: TParserOperationType; Const L, R: TExpr); virtual;
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; virtual;
    property Operation: TParserOperationType read FOp;
    property Left: TExpr read FL;
    property Right: TExpr read FR;
  public
    function AsTrueBoolean: Boolean;
    function AsBoolean: EpiBool; virtual;
    function AsInteger: EpiInteger; virtual;
    function AsFloat: EpiFloat; virtual;
    function AsString: EpiString; virtual;
    function IsMissing: Boolean; virtual;
  end;

  { TTypeCast }

  TTypeCast = class(TExpr)
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
    function IsMissing: Boolean; override;
  end;

  { TLiteral }

  TLiteral = class(TExpr);

  { TBooleanLiteral }

  TBooleanLiteral = class(TLiteral)
  private
    FValue: Boolean;
  public
    constructor Create(const Value: Boolean); overload;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TIntegerLiteral }

  TIntegerLiteral = class(TLiteral)
  private
    FValue: EpiInteger;
  public
    constructor Create(const Value: EpiInteger);
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TFloatLiteral }

  TFloatLiteral = class(TLiteral)
  private
    FValue: EpiFloat;
  public
    constructor Create(const Value: EpiFloat);
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TStringLiteral }

  TStringLiteral = class(TLiteral)
  private
    FValue: EpiString;
  public
    constructor Create(const Value: EpiString);
    destructor Destroy; override;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TMissingLiteral }

  TMissingLiteral = class(TLiteral)
  public
    constructor Create;
    function ResultType: TParserResultType; override;
    function IsMissing: Boolean; override;
  end;

  { TUnaryExpr }

  TUnaryExpr = class(TExpr)
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function IsMissing: Boolean; override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TExpr)
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
    function IsMissing: Boolean; override;
  end;

  { TRelationalExpr }

  TRelationalExpr = class(TExpr)
  public
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    function ResultType: TParserResultType; override;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TParamList }

  TParamList = class(TAbstractSyntaxTreeBase)
  private
    FList: TList;
    function GetCount: Integer;
    function GetParam(const Index: Integer): TExpr;
  protected
    procedure DoObservedChange(Sender: TObject); override;
  public
    constructor Create(AList: TList);
    destructor  Destroy; override;
    property    Param[Const Index: Integer]: TExpr read GetParam;
    property    Count: Integer read GetCount;
  end;

  { TFunction }

  TFunction = class(TExpr)
  private
    function GetParam(const Index: integer): TExpr;
  protected
    FParamList: TParamList;
    constructor Create(Const ParamList: TParamList); virtual;
    function ParamCounts: TBoundArray; virtual;
    function ParamAcceptType(ParamNo: Integer): TParserResultTypes; virtual;
  public
    class function CreateFunction(Const FunctionName: string;
      Const ParamList: TParamList;
      Parser: IEpiScriptParser): TFunction;
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property  Param[Const Index: integer]: TExpr read GetParam;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TCustomVariable }

  TCustomVariable = class(TExpr, IFPObserved)
  protected
    FIdent: string;
    FObservers: TFPList;
    class function FieldTypeToParserType(FieldType: TEpiFieldType): TParserResultType;
  public
    class function FindVariable(Const Ident: String;
      Parser: IEpiScriptParser): TCustomVariable;
    destructor Destroy; override;
    property Ident: string read FIdent;
  public
    procedure SetInteger(Const Value: EpiInteger); virtual; abstract;
    procedure SetFloat(Const Value: EpiFloat); virtual; abstract;
    procedure SetBoolean(Const Value: EpiBool); virtual; abstract;
    procedure SetString(Const Value: EpiString); virtual; abstract;
  public
    { IFPObserved }
    procedure FPOAttachObserver(AObserver: TObject);
    procedure FPODetachObserver(AObserver: TObject);
    procedure FPONotifyObservers(ASender: TObject;
      AOperation: TFPObservedOperation; Data: Pointer);
  end;

  { TFieldVariable }

  TFieldVariable = class(TCustomVariable)
  private
    FParser: IEpiScriptExecutor;
    FField: TEpiField;
  public
    constructor Create(Const Field: TEpiField;
      Parser: IEpiScriptExecutor);
    destructor Destroy; override;
    function ResultType: TParserResultType; override;
    property Field: TEpiField read FField;
  public
    function AsBoolean: EpiBool; override;
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
    function IsMissing: Boolean; override;
    procedure SetBoolean(Const Value: EpiBool); override;
    procedure SetInteger(Const Value: EpiInteger); override;
    procedure SetFloat(Const Value: EpiFloat); override;
    procedure SetString(const Value: EpiString); override;
  end;

  { TScriptVariable }

  TScriptVariable = class(TCustomVariable)
  private
    FEpiFieldType: TEpiFieldType;
  public
    class function CreateVariable(Const AIdent: string;
      VarType: TEpiFieldType): TScriptVariable;
    function ResultType: TParserResultType; override;
  public
    function AsInteger: EpiInteger; override;
    function AsFloat: EpiFloat; override;
    function AsString: EpiString; override;
  end;

  { TBooleanVariable }

  TBooleanVariable = class(TScriptVariable)
  private
    FValue: EpiBool;
  public
    function IsMissing: Boolean; override;
    function AsBoolean: EpiBool; override;
    procedure SetBoolean(const Value: EpiBool); override;
  end;

  { TIntegerVariable }

  TIntegerVariable = class(TScriptVariable)
  private
    FValue: EpiInteger;
  public
    function IsMissing: Boolean; override;
    function AsInteger: EpiInteger; override;
    procedure SetInteger(const Value: EpiInteger); override;
  end;

  { TFloatVariable }

  TFloatVariable = class(TScriptVariable)
  private
    FValue: EpiFloat;
  public
    function IsMissing: Boolean; override;
    function AsFloat: EpiFloat; override;
    procedure SetFloat(const Value: EpiFloat); override;
  end;

  { TStringVariable }

  TStringVariable = class(TScriptVariable)
  private
    FValue: EpiString;
  public
    destructor Destroy; override;
    function IsMissing: Boolean; override;
    function AsString: EpiString; override;
    procedure SetString(const Value: EpiString); override;
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

  { TWrite }

  TWrite = class(TCustomStatement)
  private
    FExpr: TExpr;
  protected
    procedure DoObservedChange(Sender: TObject); override;
  public
    constructor Create(Expr: TExpr);
    destructor Destroy; override;
    function TypeCheck(Parser: IEpiScriptParser): boolean; override;
    property Expr: TExpr read FExpr;
  end;

  { TDefine }

  TDefine = class(TCustomStatement)
  private
    FType: TEpiFieldType;
  public
    constructor Create(Const DefineType: TEpiFieldType;
      IdentList: array of IdString; Const Parser: IEpiScriptParser);
    destructor Destroy; override;
    property IdentType: TEpiFieldType read FType;
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
  YaccLib, epi_parser_core, math, variants,
  epi_script_function_resourcestrings,


  // SCRIPT FUNCTIONS (placed ind ./functions/epi_script_function_<name>.pas
  epi_script_function_mathfunctions,
  epi_script_function_createdate,
  epi_script_function_createtime,
  epi_script_function_datefunctions,
  epi_script_function_timefunctions,
  epi_script_function_stringfunctions
  ;

{ TParamList }

function TParamList.GetParam(const Index: Integer): TExpr;
begin
  result := TExpr(FList.Items[Index]);
end;

function TParamList.GetCount: Integer;
begin
  result := FList.Count;
end;

procedure TParamList.DoObservedChange(Sender: TObject);
begin
  FList.Remove(Sender);
end;

constructor TParamList.Create(AList: TList);
var
  i: Integer;
begin
  FList := TList.Create;
  if Assigned(AList) then
    FList.Assign(AList);

  for i := 0 to FList.Count - 1 do
    ObserveObject(TObject(FList[i]));
end;

destructor TParamList.Destroy;
var
  Item: Pointer;
begin
  for Item in FList do
    TExpr(Item).Free;
  FList.Free;
  inherited Destroy;
end;

{ TMissingLiteral }

constructor TMissingLiteral.Create;
begin
  inherited Create(otMissingLiteral, nil, nil);
end;

function TMissingLiteral.ResultType: TParserResultType;
begin
  Result := rtAny;
end;

function TMissingLiteral.IsMissing: Boolean;
begin
  Result := true;
end;

{ TWrite }

procedure TWrite.DoObservedChange(Sender: TObject);
begin
  if Sender = FExpr then
    FExpr := nil;
end;

constructor TWrite.Create(Expr: TExpr);
begin
  FExpr := Expr;
  ObserveObject(FExpr);
end;

destructor TWrite.Destroy;
begin
  FExpr.Free;
  inherited Destroy;
end;

function TWrite.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser) and FExpr.TypeCheck(Parser);
end;

{ TGoto }

procedure TGoto.DoObservedChange(Sender: TObject);
begin
  if Sender = FVariable then
    FVariable := nil;
end;

constructor TGoto.Create(const Variable: TCustomVariable;
  const Option: TGotoOption);
begin
  inherited Create;
  FVariable := Variable;
  FOption := Option;
  ObserveObject(FVariable);
end;

destructor TGoto.Destroy;
begin
  FVariable.Free;
  inherited Destroy;
end;

function TGoto.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);

  if Assigned(Variable) and
     (not (Variable is TFieldVariable))
  then
  begin
    DoTypeCheckError('Variable %s is not a Field', [Variable.Ident], Parser);
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

procedure TAbstractSyntaxTreeBase.ObserveObject(O: TObject);
var
  Obs: IFPObserved;
begin
  if not Assigned(O) then exit;

  if O.GetInterface(SGUIDObserved, Obs) then
  begin
    Obs.FPOAttachObserver(Self);

    if not Assigned(FObservedList) then
      FObservedList := TFPList.Create;

    FObservedList.Add(O);
  end;
end;

procedure TAbstractSyntaxTreeBase.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  Obs: IFPObserved;
begin
  DoObservedChange(ASender);
  if Asender.GetInterface(SGUIDObserved, Obs) then
    Obs.FPODetachObserver(Self);
  FObservedList.Remove(ASender);
end;

destructor TAbstractSyntaxTreeBase.Destroy;
var
  Obs: IFPObserved;
  i: Integer;
begin
  FLine := '';

  if Assigned(FObservedList) then
  begin
    for i := 0 to FObservedList.Count - 1 do
      if TObject(FObservedList[i]).GetInterface(SGUIDObserved, Obs) then
        Obs.FPODetachObserver(Self);
    FreeAndNil(FObservedList);
  end;

  inherited Destroy;
end;

function TAbstractSyntaxTreeBase.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  result := true;
end;

{ TScriptVariable }

class function TScriptVariable.CreateVariable(const AIdent: string;
  VarType: TEpiFieldType): TScriptVariable;
begin
  case FieldTypeToParserType(VarType) of
    rtBoolean: Result := TBooleanVariable.Create(otVariable, nil, nil);
    rtInteger: Result := TIntegerVariable.Create(otVariable, nil, nil);
    rtFloat:   Result := TFloatVariable.Create(otVariable, nil, nil);
    rtString:  Result := TStringVariable.Create(otVariable, nil, nil);
  end;
  Result.FIdent := AIdent;
  Result.FEpiFieldType := VarType;
end;

function TScriptVariable.ResultType: TParserResultType;
begin
  Result := FieldTypeToParserType(FEpiFieldType);
end;

function TScriptVariable.AsInteger: EpiInteger;
begin
  if IsMissing then
    result := inherited AsInteger
  else
    if FEpiFieldType = ftBoolean then
      result := AsBoolean
    else
      inherited AsInteger;
end;

function TScriptVariable.AsFloat: EpiFloat;
begin
  if IsMissing then
    result := inherited AsFloat
  else
    case FEpiFieldType of
      ftBoolean: result := AsBoolean;
      ftInteger: result := AsInteger;
      ftDMYDate: result := AsInteger;
      ftMDYDate: ;
      ftYMDDate: ;
    else
      result := inherited AsFloat;
    end;
end;

function TScriptVariable.AsString: EpiString;
begin
  if IsMissing then
    result := inherited AsString
  else
    case FEpiFieldType of
      ftBoolean: result := BoolToStr(AsTrueBoolean, true);
      ftInteger: result := IntToStr(AsInteger);
      ftFloat:   result := FloatToStr(AsFloat);
      ftDMYDate: result := DateToStr(AsInteger);
      ftMDYDate: ;
      ftYMDDate: ;
      ftTime:    result := TimeToStr(AsFloat);
    else
      result := inherited AsString;
    end;
end;

{ TBooleanVariable }

function TBooleanVariable.IsMissing: Boolean;
begin
  Result := TEpiBoolField.CheckMissing(FValue);
end;

function TBooleanVariable.AsBoolean: EpiBool;
begin
  Result := FValue;
end;

procedure TBooleanVariable.SetBoolean(const Value: EpiBool);
begin
  FValue := Value;
end;

{ TIntegerVariable }

function TIntegerVariable.IsMissing: Boolean;
begin
  Result := TEpiIntField.CheckMissing(FValue);
end;

function TIntegerVariable.AsInteger: EpiInteger;
begin
  Result := FValue;
end;

procedure TIntegerVariable.SetInteger(const Value: EpiInteger);
begin
  FValue := Value;
end;

{ TFloatVariable }

function TFloatVariable.IsMissing: Boolean;
begin
  Result := TEpiFloatField.CheckMissing(FValue);
end;

function TFloatVariable.AsFloat: EpiFloat;
begin
  Result := FValue;
end;

procedure TFloatVariable.SetFloat(const Value: EpiFloat);
begin
  FValue := Value;
end;

{ TStringVariable }

destructor TStringVariable.Destroy;
begin
  FValue := '';
  inherited Destroy;
end;

function TStringVariable.IsMissing: Boolean;
begin
  Result := TEpiStringField.CheckMissing(FValue);
end;

function TStringVariable.AsString: EpiString;
begin
  Result := FValue;
end;

procedure TStringVariable.SetString(const Value: EpiString);
begin
  FValue := Value;
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

destructor TFieldVariable.Destroy;
begin
  FField := nil;
  FParser := nil;
  inherited Destroy;
end;

function TFieldVariable.ResultType: TParserResultType;
begin
  Result := FieldTypeToParserType(FField.FieldType);
end;

function TFieldVariable.AsBoolean: EpiBool;
begin
  Result := FParser.GetFieldValueBool(Self, Field);
end;

function TFieldVariable.AsInteger: EpiInteger;
begin
  Result := FParser.GetFieldValueInt(Self, Field);
end;

function TFieldVariable.AsFloat: EpiFloat;
begin
  Result := FParser.GetFieldValueFloat(Self, Field);
end;

function TFieldVariable.AsString: EpiString;
begin
  Result := FParser.GetFieldValueString(Self, Field);
end;

function TFieldVariable.IsMissing: Boolean;
begin
  Case FField.FieldType of
    ftBoolean:
      result :=  TEpiBoolField.CheckMissing(FParser.GetFieldValueBool(Self, FField));
    ftInteger,
    ftAutoInc:
      result :=  TEpiIntField.CheckMissing(FParser.GetFieldValueInt(Self, FField));
    ftFloat:
      result :=  TEpiFloatField.CheckMissing(FParser.GetFieldValueFloat(Self, FField));
    ftDMYDate,
    ftMDYDate,
    ftYMDDate,
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      result :=  TEpiDateField.CheckMissing(FParser.GetFieldValueInt(Self, FField));
    ftTime,
    ftTimeAuto:
      result :=  TEpiDateTimeField.CheckMissing(FParser.GetFieldValueFloat(Self, FField));
    ftString,
    ftUpperString:
      result :=  TEpiStringField.CheckMissing(FParser.GetFieldValueString(Self, FField));
  end;
end;

procedure TFieldVariable.SetBoolean(const Value: EpiBool);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;

procedure TFieldVariable.SetInteger(const Value: EpiInteger);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;

procedure TFieldVariable.SetFloat(const Value: EpiFloat);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;

procedure TFieldVariable.SetString(const Value: EpiString);
begin
  FParser.SetFieldValue(Self, FField, Value);
end;


{ TRelationExpr }

(*  Backup if rtDate is needed...
RelationalOperationCheck: array[TParserResultType, TParserResultType] of Boolean =
         //    rtBoolean, rtInteger, rtDate, rtFloat, rtString, rtObject, rtUndefined
             (
{rtBoolean}    ( true,     false,     false,  false,   false,    false,    false),
{rtInteger}    (false,     true,      true,   true,    false,    false,    false),
{rtDate}       (false,     true,      true,   true,    false,    false,    false),
{rtFloat}      (false,     true,      true,   true,    false,    false,    false),
{rtString}     (false,     false,     false,  false,   true,     false,    false),
{rtObject}     (false,     false,     false,  false,   false,    true ,    false),
{rtUndefined}  (false,     false,     false,  false,   false,    false,    false)
             );
*)

function TRelationalExpr.TypeCheck(Parser: IEpiScriptParser): boolean;
const
  RelationalOperationCheck: array[TParserResultType, TParserResultType] of Boolean =
           //    rtAny,     rtBoolean, rtInteger, rtFloat, rtString, rtObject, rtUndefined
               (
 {rtAny}        ( true,     true,      true,      true,    true,     true,     false),
 {rtBoolean}    ( true,     true,      false,     false,   false,    false,    false),
 {rtInteger}    ( true,     false,     true,      true,    false,    false,    false),
 {rtFloat}      ( true,     false,     true,      true,    false,    false,    false),
 {rtString}     ( true,     false,     false,     false,   true,     false,    false),
 {rtObject}     ( true,     false,     false,     false,   false,    true ,    false),
 {rtUndefined}  ( false,    false,     false,     false,   false,    false,    false)
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

function TRelationalExpr.AsBoolean: EpiBool;
var
  CType: TParserResultType;
  Res: PtrInt;
  BoolResult: Boolean;
begin
  Result := inherited AsBoolean;

  if Left.IsMissing or Right.IsMissing then
  begin
    case Operation of
      otEQ:  BoolResult := not (Left.IsMissing xor Right.IsMissing);
      otNEQ: BoolResult := Left.IsMissing xor Right.IsMissing;
      otLT:  BoolResult := not Left.IsMissing;
      otLTE: BoolResult := Right.IsMissing;
      otGT:  BoolResult := not Right.IsMissing;
      otGTE: BoolResult := Left.IsMissing;
    end;
    Exit(EpiBool(BoolResult));
  end;

  CType := CommonType(Left, Right);
  case CType of
    rtBoolean:
    case Operation of
      otEQ:  BoolResult := Left.AsBoolean = Right.AsBoolean;
      otNEQ: BoolResult := Left.AsBoolean <> Right.AsBoolean;
      otLT:  BoolResult := Left.AsBoolean < Right.AsBoolean;
      otLTE: BoolResult := Left.AsBoolean <= Right.AsBoolean;
      otGT:  BoolResult := Left.AsBoolean > Right.AsBoolean;
      otGTE: BoolResult := Left.AsBoolean >= Right.AsBoolean;
    end;

    rtInteger:
    case Operation of
      otEQ:  BoolResult := Left.AsInteger = Right.AsInteger;
      otNEQ: BoolResult := Left.AsInteger <>Right.AsInteger;
      otLT:  BoolResult := Left.AsInteger < Right.AsInteger;
      otLTE: BoolResult := Left.AsInteger <= Right.AsInteger;
      otGT:  BoolResult := Left.AsInteger > Right.AsInteger;
      otGTE: BoolResult := Left.AsInteger >= Right.AsInteger;
    end;

    rtFloat:
    case Operation of
      otEQ:  BoolResult := SameValue(Left.AsFloat, Right.AsFloat, 0.0);
      otNEQ: BoolResult := not SameValue(Left.AsFloat, Right.AsFloat, 0.0);
      otLT:  BoolResult := Left.AsFloat < Right.AsFloat;
      otLTE: BoolResult := (Left.AsFloat < Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
      otGT:  BoolResult := Left.AsFloat > Right.AsFloat;
      otGTE: BoolResult := (Left.AsFloat < Right.AsFloat) or (SameValue(Left.AsFloat, Right.AsFloat, 0.0));
    end;

    rtString:
    begin
      Res := UnicodeCompareStr(UTF8Decode(Left.AsString), UTF8Decode(Right.AsString));
      case Operation of
        otEQ:  BoolResult := Res = 0;
        otNEQ: BoolResult := Res <> 0;
        otLT:  BoolResult := Res < 0;
        otLTE: BoolResult := Res <= 0;
        otGT:  BoolResult := Res > 0;
        otGTE: BoolResult := Res >= 0;
      end;
    end;

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

  Result := EpiBool(BoolResult);
end;

function TRelationalExpr.AsInteger: EpiInteger;
begin
  Result := EpiInteger(AsBoolean);
end;

function TRelationalExpr.AsFloat: EpiFloat;
begin
  Result := AsInteger;
end;

function TRelationalExpr.AsString: EpiString;
begin
  inherited AsString;
  if IsMissing then
    Result := TEpiStringField.DefaultMissing
  else
    Result := BoolToStr(AsTrueBoolean, true);
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
    otMinus,
    otDivide:
    begin
      result := (Lr in [rtInteger, rtFloat]) and
                (Rr in [rtInteger, rtFloat]);
      if not (Lr in [rtInteger, rtFloat]) then
        DoTypeCheckError(
          'Left ' + rsExpressionReturnType2,
          [SParserResultType[rtInteger], SParserResultType[rtFloat]],
          Parser
        );
      if not (Rr in [rtInteger, rtFloat]) then
        DoTypeCheckError(
          'Right ' + rsExpressionReturnType2,
          [SParserResultType[rtInteger], SParserResultType[rtFloat]],
          Parser
        );
    end;
    otPlus:
      begin
        result := (Lr in [rtInteger, rtFloat, rtString]) and
                  (Rr in [rtInteger, rtFloat, rtString]);
        if not (Lr in [rtInteger, rtFloat, rtString]) then
          DoTypeCheckError(
            'Left ' + rsExpressionReturnType3,
            [SParserResultType[rtInteger], SParserResultType[rtFloat], SParserResultType[rtString]],
            Parser
          );
        if not (Rr in [rtInteger, rtFloat, rtString]) then
          DoTypeCheckError(
            'Right ' + rsExpressionReturnType3,
            [SParserResultType[rtInteger], SParserResultType[rtFloat], SParserResultType[rtString]],
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
    otMinus,
    otExponential:
      result := CommonType(FL, FR);
    otDivide:
      result := rtFloat;
{      otShl,
      otShr,
      otXor,
      ;}
  end;
end;

function TBinaryExpr.AsBoolean: EpiBool;
begin
  Result := inherited AsBoolean;

  if IsMissing then
    Exit;

  case Operation of
    otOr:  result := Left.AsBoolean or Right.AsBoolean;
    otAnd: result := Left.AsBoolean and Right.AsBoolean;
  end;
end;

function TBinaryExpr.AsInteger: EpiInteger;
var
  Tmp: EpiInteger;
begin
  Result := inherited AsInteger;

  if IsMissing then
    Exit;

  case ResultType of
    rtBoolean:
      Result := AsBoolean;
    rtInteger:
      case Operation of
        otPlus:        result := Left.AsInteger + Right.AsInteger;
        otMinus:       result := Left.AsInteger - Right.AsInteger;
        otMult:        result := Left.AsInteger * Right.AsInteger;
        otDiv:         result := Left.AsInteger div Right.AsInteger;
        otMod:         result := Left.AsInteger mod Right.AsInteger;
        otExponential: result := Left.AsInteger ** Right.AsInteger;
      end;
  end;
end;

function TBinaryExpr.AsFloat: EpiFloat;
var
  Tmp: EpiFloat;
begin
  Result := inherited AsFloat;

  if IsMissing then
    Exit;

  case ResultType of
    rtBoolean:
      result := AsBoolean;
    rtInteger:
      result := AsInteger;
    rtFloat:
      case Operation of
        otPlus:        result := Left.AsFloat + Right.AsFloat;
        otMinus:       result := Left.AsFloat - Right.AsFloat;
        otMult:        result := Left.AsFloat * Right.AsFloat;
        otDivide:      result := Left.AsFloat / Right.AsFloat;
        otExponential: result := Left.AsFloat ** Right.AsFloat;
      end;
  end;
end;

function TBinaryExpr.AsString: EpiString;
begin
  Result := inherited AsString;

  if IsMissing and
     (not (ResultType = rtString))
  then
    Exit;

  case ResultType of
    rtBoolean:
      result := BoolToStr(AsTrueBoolean);

    rtInteger:
      result := IntToStr(AsInteger);

    rtFloat:
      result := FloatToStr(AsFloat);

    rtString:
      case Operation of
        otPlus: Result := Left.AsString + Right.AsString;
      end;
  end;
end;

function TBinaryExpr.IsMissing: Boolean;
begin
  Result := Left.IsMissing or Right.IsMissing;
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

function TUnaryExpr.AsBoolean: EpiBool;
begin
  if IsMissing then
    Result := inherited AsBoolean
  else
  case Operation of
    otNot: result := (not Left.AsBoolean);
  end;
end;

function TUnaryExpr.AsInteger: EpiInteger;
begin
  if IsMissing then
    Result := inherited AsInteger
  else
  case Operation of
    otMinus: result := -Left.AsInteger;
  end;
end;

function TUnaryExpr.AsFloat: EpiFloat;
begin
  if IsMissing then
    Result := inherited AsFloat
  else
  case Operation of
    otMinus: result := -Left.AsFloat;
  end;
end;

function TUnaryExpr.IsMissing: Boolean;
begin
  Result := Left.IsMissing;
end;

{ TFunction }

function TFunction.GetParam(const Index: integer): TExpr;
begin
  result := FParamList.Param[Index];
end;

constructor TFunction.Create(const ParamList: TParamList);
begin
  inherited Create(otFunction, nil, nil);
  FParamList := ParamList;
end;

destructor TFunction.Destroy;
begin
  FParamList.Free;
  inherited Destroy;
end;

function TFunction.ParamCounts: TBoundArray;
begin
  SetLength(Result, 1);
  Result[0] := 0;
end;

function TFunction.ParamAcceptType(ParamNo: Integer): TParserResultTypes;
begin
  result := [rtUndefined];
end;

class function TFunction.CreateFunction(const FunctionName: string;
  const ParamList: TParamList; Parser: IEpiScriptParser): TFunction;
var
  Func: String;
begin
  Func := LowerCase(FunctionName);

  result := Parser.CreateFunction(FunctionName, ParamList);

  if Assigned(Result) then exit;

  case Func of
    { Math }
    'abs':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncAbs, ParamList);
    'exp':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncExp, ParamList);
    'fraction':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncFraction, ParamList);
    'ln':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncLn, ParamList);
    'log':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncLog, ParamList);
    'round':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncRound, ParamList);
    'sqrt':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncSqrt, ParamList);
    'random':
      result := TEpiScriptFunction_MathFunctions.Create(otFuncRandom, ParamList);

    { Date functions }
    'createdate':
      result := TEpiScriptFunction_CreateDate.Create(ParamList);
    'today':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncToday, ParamList);
    'day':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncDay, ParamList);
    'month':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncMonth, ParamList);
    'year':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncYear, ParamList);
    'dayofweek':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncDayOfWeek, ParamList);
    'week':
      result := TEpiScriptFunction_DateFunctions.Create(otFuncWeek, ParamList);

    { Time functions }
    'createtime':
      result := TEpiScriptFunction_CreateTime.Create(ParamList);
    'now':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncNow, ParamList);
    'hour':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncHour, ParamList);
    'minut':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncMinut, ParamList);
    'second':
      result := TEpiScriptFunction_TimeFunctions.Create(otFuncSecond, ParamList);

    { String }
    'substring':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncSubString, ParamList);
    'length':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncLength, ParamList);
    'pos':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncPos, ParamList);
    'trim':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncTrim, ParamList);
    'lower':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncLower, ParamList);
    'upper':
      result := TEpiScriptFunction_StringFunctions.Create(otFuncUpper, ParamList);
  else
    result := nil;
  end;

  if not Assigned(Result) then
  begin
    yyerror('Function "' + FunctionName + '" not found!');
    yyabort;
  end;
end;

function TFunction.TypeCheck(Parser: IEpiScriptParser): boolean;
var
  AcceptTypes: TParserResultTypes;
  AType: TParserResultType;
  i: Integer;
  S: String;
  AllowedCounts: TBoundArray;
  CurrentCount: Integer;
begin
  Result := inherited TypeCheck(Parser);
  if not Result then exit;
{
  if FParamList.Count < MinParamCount then
  begin
    DoTypeCheckError('Too few parameters. Required: %d Given %d',
      [MinParamCount, FParamList.Count],
      Parser
    );
    Exit(False);
  end;

  if FParamList.Count > MaxParamCount then
  begin
    DoTypeCheckError('Too many parameters. Required: %d Given %d',
      [MinParamCount, FParamList.Count],
      Parser
    );
    Exit(False);
  end;       }

  S := '';
  Result := false;
  AllowedCounts := ParamCounts;
  CurrentCount := FParamList.Count;
  for i := Low(AllowedCounts) to High(AllowedCounts) do
  begin
    if AllowedCounts[i] = CurrentCount then
    begin
      Result := true;
      break;
    end;
    S := S + IntToStr(AllowedCounts[i]) + ',';
  end;
  Delete(S,Length(S),1);

  if not result then
  begin
    DoTypeCheckError('Incorrect number of parameters given. Accepts: %s Given %d',
      [S, FParamList.Count],
      Parser
    );
    Exit(False);
  end;


  for i := 0 to FParamList.Count -1 do
  begin
    AcceptTypes := ParamAcceptType(i);
    if not (Param[i].ResultType in AcceptTypes) then
    begin
      S := '';
      for AType in AcceptTypes do
      begin
        if AType = rtAny then
          S += 'missing,'
        else
          S += SParserResultType[AType] + ',';
      end;

      Delete(S, Length(S), 1);


      DoTypeCheckError(
        'Parameter no. %d accept types: %s' + LineEnding +
        'But was given: %s',
        [i + 1, S, SParserResultType[Param[i].ResultType]],
        Parser
      );

      Exit(False);
    end;
  end;
end;

function TFunction.AsInteger: EpiInteger;
begin
  if IsMissing then
    Result := inherited AsInteger
  else
  case ResultType of
    rtBoolean:
      result := AsBoolean;
  else
    Result := inherited AsInteger;
  end;
end;

function TFunction.AsFloat: EpiFloat;
begin
  if IsMissing then
    Result := inherited AsFloat
  else
  case ResultType of
    rtBoolean:
      result := AsBoolean;
    rtInteger:
      result := AsInteger;
  else
    Result := inherited AsFloat;
  end;
end;

function TFunction.AsString: EpiString;
begin
  if IsMissing then
    Result := inherited AsString
  else
  case ResultType of
    rtBoolean:
      result := BoolToStr(AsTrueBoolean, True);
    rtInteger:
      result := IntToStr(AsInteger);
    rtFloat:
      result := FloatToStr(AsFloat);
  else
    Result := inherited AsString;
  end;
end;

{ TDefine }

constructor TDefine.Create(const DefineType: TEpiFieldType;
  IdentList: array of IdString; const Parser: IEpiScriptParser);
var
  i: Integer;
begin
  inherited Create;
  FType := DefineType;

  for i := Low(IdentList) to High(IdentList) do
  begin
    if Parser.VariableExists(IdentList[i]) then
    begin
      yyerror('Variable "' + IdentList[i] + '" already defined');
      yyabort;
    end;
    Parser.AddVariable(TScriptVariable.CreateVariable(IdentList[i], DefineType));
  end;
end;

destructor TDefine.Destroy;
begin
  inherited Destroy;
end;

{ TAssignment }

procedure TAssignment.DoObservedChange(Sender: TObject);
begin
  if Sender = FExpr then
    FExpr := nil;
  if Sender = FVAriable then
    FVAriable := nil;
end;

constructor TAssignment.Create(const Variable: TCustomVariable; const Expr: TExpr);
begin
  inherited Create;
  FExpr := Expr;
  FVAriable := Variable;
  ObserveObject(FExpr);
  ObserveObject(FVAriable);
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

procedure TIfThen.DoObservedChange(Sender: TObject);
begin
  if Sender = FExpr then
    FExpr := nil;
end;

constructor TIfThen.Create(const Expr: TExpr;
  const ThenStatement: TCustomStatement; const ElseStatement: TCustomStatement);
begin
  inherited Create;
  FExpr := Expr;
  ObserveObject(FExpr);
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

{ TBooleanLiteral }

constructor TBooleanLiteral.Create(const Value: Boolean);
begin
  inherited Create(otBoolLiteral, nil, nil);
  FValue := Value;
end;

function TBooleanLiteral.ResultType: TParserResultType;
begin
  Result := rtBoolean;
end;

function TBooleanLiteral.AsBoolean: EpiBool;
begin
  Result := EpiBool(FValue);
end;

function TBooleanLiteral.AsInteger: EpiInteger;
begin
  Result := EpiInteger(AsBoolean);
end;

function TBooleanLiteral.AsFloat: EpiFloat;
begin
  Result := EpiFloat(AsInteger);
end;

function TBooleanLiteral.AsString: EpiString;
begin
  Result := BoolToStr(AsTrueBoolean);
end;

{ TIntegerLiteral }

constructor TIntegerLiteral.Create(const Value: EpiInteger);
begin
  inherited Create(otIntegerLiteral, nil, nil);
  FValue := Value;
end;

function TIntegerLiteral.ResultType: TParserResultType;
begin
  Result := rtInteger;
end;

function TIntegerLiteral.AsBoolean: EpiBool;
begin
  Result := EpiBool(AsInteger);
end;

function TIntegerLiteral.AsInteger: EpiInteger;
begin
  Result := FValue;
end;

function TIntegerLiteral.AsFloat: EpiFloat;
begin
  Result := AsInteger;
end;

function TIntegerLiteral.AsString: EpiString;
begin
  Result := IntToStr(AsInteger);
end;


{ TFloatLiteral }

constructor TFloatLiteral.Create(const Value: EpiFloat);
begin
  inherited Create(otFloatLiteral, nil, nil);
  FValue := Value;
end;

function TFloatLiteral.ResultType: TParserResultType;
begin
  Result := rtFloat;
end;

function TFloatLiteral.AsBoolean: EpiBool;
begin
  Result := EpiBool(AsInteger);
end;

function TFloatLiteral.AsInteger: EpiInteger;
begin
  Result := Trunc(AsFloat)
end;

function TFloatLiteral.AsFloat: EpiFloat;
begin
  Result := FValue;
end;

function TFloatLiteral.AsString: EpiString;
begin
  Result := FloatToStr(AsFloat);
end;

{ TStringLiteral }

constructor TStringLiteral.Create(const Value: EpiString);
begin
  inherited Create(otStringLiteral, nil, nil);
  FValue := Value;
end;

destructor TStringLiteral.Destroy;
begin
  FValue := '';
  inherited Destroy;
end;

function TStringLiteral.ResultType: TParserResultType;
begin
  Result := rtString;
end;

function TStringLiteral.AsBoolean: EpiBool;
var
  Val: EpiInteger;
begin
  Val := AsInteger;
  if TEpiIntField.CheckMissing(Val) then
    Result := TEpiBoolField.DefaultMissing
  else
    Result := EpiBool(Val);
end;

function TStringLiteral.AsInteger: EpiInteger;
begin
  if not TryStrToInt64(FValue, Result) then
    Result := TEpiIntField.DefaultMissing;
end;

function TStringLiteral.AsFloat: EpiFloat;
begin
  if not TryStrToFloat(FValue, Result) then
    Result := TEpiFloatField.DefaultMissing;
end;

function TStringLiteral.AsString: EpiString;
begin
  Result := FValue;
end;

{ TTypeCast }

function TTypeCast.TypeCheck(Parser: IEpiScriptParser): boolean;
begin
  Result := inherited TypeCheck(Parser);
end;

function TTypeCast.ResultType: TParserResultType;
begin
  Case FOp of
    otBoolCast:    result := rtBoolean;
    otIntegerCast: result := rtInteger;
    otFloatCast:   result := rtFloat;
    otStringCast:  result := rtString;
  end;
end;

function TTypeCast.AsBoolean: EpiBool;
begin
  Case FOp of
    otBoolCast:    result := Left.AsBoolean;
    otIntegerCast: ; // Get's caught in TypeChecking.
    otFloatCast:   ; // Get's caught in TypeChecking.
    otStringCast:  ; // Get's caught in TypeChecking.
  end;
end;

function TTypeCast.AsInteger: EpiInteger;
begin
  if Left.IsMissing then
    Exit(Left.AsInteger);

  Case FOp of
    otBoolCast:    result := Integer(AsBoolean);
    otIntegerCast: result := Left.AsInteger;
    otFloatCast:   ; // Get's caught in TypeChecking.
    otStringCast:  ; // Get's caught in TypeChecking.
  end;
end;

function TTypeCast.AsFloat: EpiFloat;
begin
  if Left.IsMissing then
    Exit(Left.AsFloat);

  Case FOp of
    otBoolCast:    result := AsInteger;
    otIntegerCast: result := AsInteger;
    otFloatCast:   result := Left.AsFloat;
    otStringCast:  ; // Get's caught in TypeChecking.
  end;
end;


function TTypeCast.AsString: EpiString;
begin
  if Left.IsMissing then
    Exit(Left.AsString);

  Case FOp of
    otBoolCast:    result := BoolToStr(Left.AsTrueBoolean);
    otIntegerCast: result := IntToStr(AsInteger);
    otFloatCast:   result := FloatToStr(Left.AsFloat);
    otStringCast:  result := Left.AsString;
  end;
end;

function TTypeCast.IsMissing: Boolean;
begin
  Result := Left.IsMissing;
end;

{ TExpr }

function TExpr.CommonType(const A, B: TExpr): TParserResultType;
begin
  result := TParserResultType(Math.Max(Ord(A.ResultType), Ord(B.ResultType)));
end;

procedure TExpr.DoObservedChange(Sender: TObject);
begin
  if Sender = FL then
    FL := nil;
  if Sender = FR then
    FR := nil;
end;

constructor TExpr.Create(const Op: TParserOperationType; const L, R: TExpr);
begin
  inherited Create;
  FOp := Op;
  FL := L;
  FR := R;
  ObserveObject(L);
  ObserveObject(R);
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

function TExpr.ResultType: TParserResultType;
begin
  // Default result type is rtUndefined
  result := rtUndefined;
end;

function TExpr.AsTrueBoolean: Boolean;
var
  Val: EpiBool;
begin
  Val := AsBoolean;
  if TEpiBoolField.CheckMissing(Val) then
    result := false
  else
    result := Boolean(Val);
end;

function TExpr.AsBoolean: EpiBool;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiBoolField.DefaultMissing;
end;

function TExpr.AsInteger: EpiInteger;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiIntField.DefaultMissing;
end;

function TExpr.AsFloat: EpiFloat;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiFloatField.DefaultMissing;
end;

function TExpr.AsString: EpiString;
begin
  ASTCurrentExecutionObject := self;
  result := TEpiStringField.DefaultMissing;
end;

function TExpr.IsMissing: Boolean;
begin
  result := false;
end;

{ TCustomVariable }

class function TCustomVariable.FieldTypeToParserType(FieldType: TEpiFieldType
  ): TParserResultType;
const
  FieldTypeToParserTypeTable: array[TEpiFieldType] of TParserResultType =
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
  result := FieldTypeToParserTypeTable[FieldType];
end;

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
  FPONotifyObservers(Self, ooFree, nil);
  If Assigned(FObservers) then
    FreeAndNil(FObservers);
  inherited Destroy;
end;

procedure TCustomVariable.FPOAttachObserver(AObserver: TObject);
begin
  if not Assigned(FObservers) then
    FObservers := TFPList.Create;

  FObservers.Add(AObserver);
end;

procedure TCustomVariable.FPODetachObserver(AObserver: TObject);
begin
  if not Assigned(FObservers) then exit;

  FObservers.Remove(AObserver);
end;

procedure TCustomVariable.FPONotifyObservers(ASender: TObject;
  AOperation: TFPObservedOperation; Data: Pointer);
Var
  O : TObject;
  I : Integer;
  Obs : IFPObserver;
begin
  If Assigned(FObservers) then
    For I := FObservers.Count - 1 downto 0 do
      begin
        O := TObject(FObservers[i]);
        If O.GetInterface(SGUIDObserver, Obs) then
          Obs.FPOObservedChanged(Self, AOperation, Data);
      end;
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

