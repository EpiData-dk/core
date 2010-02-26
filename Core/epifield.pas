unit epifield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TEpiField = class;
  TEpiFields = class;

  { TEpiFields }

  TEpiFields = class(TObject)
  private
    FOwned:     Boolean;
    FDataFile:  TEpiDataFile;
    FList:      TList;
    FReportOnChange: Boolean;  // Used to report back to DataFile if field is added, deleted, sorted.
    function    GetField(Index: Integer): TEpiField;
    function    GetCount: Cardinal;
  protected
    procedure   Sort(Cmp: TListSortCompare);
    property    ReportOnChange: Boolean read FReportOnChange write FReportOnChange;
  public
    constructor Create(aOwner: TEpiDataFile); virtual;
    destructor  Destroy; override;
    procedure   Add(aField: TEpiField);
    procedure   Delete(aField: TEpiField);
    function    FieldByName(Const aFieldName: string): TEpiField;
    function    FieldById(Const aId: string): TEpiField;
    function    FieldExists(Const aFieldName: string): boolean;
    function    IndexOf(Const aFieldName: string): integer;
    Property    Field[Index: integer]: TEpiField read GetField; default;
    Property    Count: Cardinal read GetCount;
    Property    Owned: Boolean read FOwned write FOwned;
  end;

  { TEpiFieldProperties }

  TEpiFieldProperties = class(TObject)
  private
    FEntryType:        TEntryType;
    FTopOfScreen:      Integer;
    FDoRepeat:         boolean;
    FConfirm:          boolean;
    FBeforeCmds:       TChkCommands;
    FAfterCmds:        TChkCommands;
    FRanges:           TStrings;
    FJumps:            TStrings;
    FJumpResetValue:   String;
    FShowValueLabel:   Boolean;
    FTypeField:        TEpiField;
    FTypeType:         TTypeType;
    FTypeColour:        Integer;
    FHasGlobalDefaultVal: Boolean;
    FFieldScope:       TFieldScope;
    FFieldComments:    TStrings;
  protected
    procedure          InternalReset; virtual;
  public
    Constructor Create;
    Destructor  Destroy; override;
    procedure   Reset;
    procedure   Clone(Var Dest: TEpiFieldProperties);
    property    EntryType:    TEntryType read FEntryType write FEntryType;
    property    TopOfScreen:  Integer read FTopOfScreen write FTopOfScreen;
    property    DoRepeat:     boolean read FDoRepeat write FDoRepeat;
    property    Confirm:      boolean read FConfirm write FConfirm;
    property    BeforeCmds:   TChkCommands read FBeforeCmds;
    property    AfterCmds:    TChkCommands read FAfterCmds;
    property    Ranges:       TStrings read FRanges;
    property    Jumps:        TStrings read FJumps write FJumps;
    property    JumpResetValue: string read FJumpResetValue write FJumpResetValue;
    property    ShowValueLabel: Boolean read FShowValueLabel write FShowValueLabel;
    property    TypeType:     TTypeType read FTypeType write FTypeType;
    property    TypeField:    TEpiField read FTypeField write FTypeField;
    property    TypeColour:   Integer read FTypeColour write FTypeColour;
    property    FieldScope:   TFieldScope read FFieldScope write FFieldScope;
    property    FieldComments: TStrings read FFieldComments;
  end;

  { TEpiField }

  TEpiField = class(TObject)
  private
    FFieldLeft: Integer;
    FFieldTop: Integer;
    FId: string;
    FOwner:         TEpiFields;
    FDataFile:      TEpiDataFile;
    FCapacity:      Integer;
    FScreenProps:   TEpiScreenProperty;
    FSize:          Integer;
    FFieldType:     TFieldType;
    FFieldName:     string;
    FFieldLength:   Cardinal;
    FFieldDecimals: Cardinal;
    FVariableLabel: string;
    FDefaultValue:  string;
    FFieldProperties: TEpiFieldProperties;
    FValueLabelSet: TValueLabelSet;
    FValueLabelIsFieldRef: Boolean;
    FVarLabelLeft: Integer;
    FVarLabelScreenProps: TEpiScreenProperty;
    FVarLabelTop: Integer;
    function GetFieldProperties: TEpiFieldProperties;
    function GetHasFieldProperties: boolean;
    procedure SetFieldDecimals(const AValue: Cardinal);
    procedure SetFieldLeft(const AValue: Integer);
    procedure SetFieldLength(const AValue: Cardinal);
    procedure SetFieldName(const AValue: string);
    procedure SetFieldTop(const AValue: Integer);
    procedure SetVariableLabel(const AValue: string);
    procedure SetVarLabelLeft(const AValue: Integer);
    procedure SetVarLabelTop(const AValue: Integer);
  protected
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); virtual;
    procedure DoChange(EventType: TEpiFieldChangeEventType; OldValue: Pointer);
    procedure CheckIndex(Const index: integer); virtual;
    procedure AssignData(Const Source: TEpiField); virtual; abstract;
    function GetAsBoolean(const index: Integer): EpiBool; virtual; abstract;
    function GetAsDate(const index: Integer): EpiDate; virtual; abstract;
    function GetAsFloat(const index: Integer): EpiFloat; virtual; abstract;
    function GetAsInteger(const index: Integer): EpiInteger; virtual; abstract;
    function GetAsString(const index: Integer): EpiString; virtual; abstract;
    function GetAsValue(const index: Integer): EpiVariant; virtual; abstract;
    function GetAsValueLabel(const index: Integer): string; virtual;
    function GetCapacity: Integer; virtual; abstract;
    function GetIsMissing(const index: Integer): boolean; virtual; abstract;
    function GetIsMissingValue(const index: Integer): boolean; virtual;
    function GetSize: Integer; virtual;
    procedure Grow; virtual;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); virtual; abstract;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); virtual; abstract;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); virtual; abstract;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); virtual; abstract;
    procedure SetAsString(const index: Integer; const AValue: EpiString); virtual; abstract;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); virtual; abstract;
    procedure SetCapacity(AValue: Integer); virtual; abstract;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); virtual; abstract;
    procedure SetSize(const AValue: Integer); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    class function CreateField(aFieldType: TFieldType; aSize: Cardinal = 0): TEpiField;
    destructor  Destroy; override;
    procedure Reset;
    function Clone(DstDataFile: TEpiDataFile = nil; CloneData: boolean = true): TEpiField;
    procedure Exchange(i,j: integer); virtual; abstract;
    function  Compare(i,j: integer): integer; virtual; abstract;
    procedure NewRecords(ACount: Integer = 1); virtual;
    Property  Id:            string read FId write FId;
    property  Owner:         TEpiFields read FOwner;
    property  DataFile:      TEpiDataFile read FDataFile;
    property  Size:          Integer read GetSize write SetSize;
    property  FieldType:     TFieldType read FFieldType;
    property  FieldName:     string read FFieldName write SetFieldName;
    property  FieldLength:   Cardinal read FFieldLength write SetFieldLength;
    property  FieldDecimals: Cardinal read FFieldDecimals write SetFieldDecimals;
    Property  FieldTop:      Integer read FFieldTop write SetFieldTop;
    Property  FieldLeft:     Integer read FFieldLeft write SetFieldLeft;
    property  ScreenProps:   TEpiScreenProperty read FScreenProps write FScreenProps;
    property  VariableLabel: string read FVariableLabel write SetVariableLabel;
    Property  VarLabelTop:   Integer read FVarLabelTop write SetVarLabelTop;
    Property  VarLabelLeft:  Integer read FVarLabelLeft write SetVarLabelLeft;
    property  VarLabelScreenProps: TEpiScreenProperty read FVarLabelScreenProps write FVarLabelScreenProps;
    property  DefaultValue: string read FDefaultValue write FDefaultValue;
    property  HasFieldProperties: boolean read GetHasFieldProperties;
    property  FieldProperties: TEpiFieldProperties read GetFieldProperties;
    property  ValueLabelSet: TValueLabelSet read FValueLabelSet write FValueLabelSet;
    Property  ValueLabelIsFieldRef: Boolean read FValueLabelIsFieldRef write FValueLabelIsFieldRef;
    property  IsMissing[const index: Integer]: boolean read GetIsMissing write SetIsMissing;
    property  IsMissingValue[const index: Integer]: boolean read GetIsMissingValue;
    property  AsBoolean[const index: Integer]: EpiBool read GetAsBoolean write SetAsBoolean;
    property  AsInteger[const index: Integer]: EpiInteger read GetAsInteger write SetAsInteger; default;
    property  AsFloat[const index: Integer]: EpiFloat read GetAsFloat write SetAsFloat;
    property  AsDate[const index: Integer]: EpiDate read GetAsDate write SetAsDate;
//    property AsTime[const index: Integer]: EpiTime read GetAsTime write SetAsTime;
//    property AsDateTime[const index: Integer]: EpiDateTime read GetAsDateTime write SetAsDateTime;
    property  AsString[const index: Integer]: EpiString read GetAsString write SetAsString;
    property  AsValue[const index: Integer]: EpiVariant read GetAsValue write SetAsValue;
    property  AsValueLabel[const index: Integer]: string read GetAsValueLabel;
  private
    // Events (and control):
    FOnChange:     ^TEpiFieldChangeEvent;
    FOnChangeCount: Integer;
    FOnChangeData: ^TEpiFieldChangeDataEvent;
    FOnChangeDataCount: Integer;
    FUpdateCount: Integer;
  public
    // Events (and control):
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RegisterOnChangeHook(Event: TEpiFieldChangeEvent);
    procedure UnRegisterOnChangeHook(Event: TEpiFieldChangeEvent);
    procedure RegisterOnChangeDataHook(Event: TEpiFieldChangeDataEvent);
    procedure UnRegisterOnChangeDataHook(Event: TEpiFieldChangeDataEvent);
  end;

  { TEpiIntField }
  // Handles following field types:
  // ftInteger, ftIDNum,
  TEpiIntField = class(TEpiField)
  private
    FData: array of EpiInteger;
  protected
    procedure AssignData(const Source: TEpiField); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
  public
    class function CheckMissing(AValue: EpiInteger): boolean;
    class function DefaultMissing: EpiInteger;
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); override;
    function Compare(i, j: integer): integer; override;
    destructor Destroy; override;
    procedure Exchange(i, j: integer); override;
  published

  end;

  { TEpiFloatField }
  // Handles following field types:
  // ftFloat
  TEpiFloatField = class(TEpiField)
  private
    FData: Array of EpiFloat;
  protected
    procedure AssignData(const Source: TEpiField); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiFloat): boolean;
    class function DefaultMissing: EpiFloat;
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); override;
    function Compare(i, j: integer): integer; override;
    destructor Destroy; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiBoolField }
  // Handles following field types:
  // ftBoolean
  TEpiBoolField = class(TEpiField)
  private
    FData: array of EpiBool;
  protected
    procedure AssignData(const Source: TEpiField); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiBool): boolean;
    class function DefaultMissing: EpiBool;
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); override;
    destructor Destroy; override;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiStringField }
  // Handles following field types:
  // ftString, ftUpperAlfa, ftSoundex, ftCrypt
  TEpiStringField = class(TEpiField)
  private
    FData: array of string;
  protected
    procedure AssignData(const Source: TEpiField); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiString): boolean;
    class function DefaultMissing: EpiString;
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); override;
    function Compare(i, j: integer): integer; override;
    destructor Destroy; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiDateField }
  // Handles following field types:
  // ftDate, ftToday, ftEuroDate, ftEuroToday, ftYMDDate, ftYMDToday
  TEpiDateField = class(TEpiField)
  private
     FData: array of EpiDate;
  protected
    procedure AssignData(const Source: TEpiField); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiDate): boolean;
    class function DefaultMissing: EpiDate;
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); override;
    function Compare(i, j: integer): integer; override;
    destructor Destroy; override;
    procedure Exchange(i, j: integer); override;
  end;

implementation

const
  NA_INT       = MaxInt;
  NA_FLOAT     = MaxFloat;
  NA_DATE      = NA_INT;
  NA_TIME      = MaxDouble;
  NA_DATETIME  = NA_TIME;
  NA_STRING    = '.';
  NA_BOOL      = $7F;

  Field_Growth_Factor = 1.20;


{ TEpiFields }

function TEpiFields.GetField(Index: integer): TEpiField;
begin
  result := TEpiField(FList[Index]);
end;

function TEpiFields.GetCount: Cardinal;
begin
  result := FList.Count
end;

procedure TEpiFields.Sort(Cmp: TListSortCompare);
begin
  FList.Sort(Cmp);

  if ReportOnChange and Assigned(FDataFile) then
    FDataFile.DoChange(dceFieldOrder, nil);
end;

constructor TEpiFields.Create(aOwner: TEpiDataFile);
begin
  FList := TList.Create();
  FDataFile := aOwner;
  FReportOnChange := false;
end;

destructor TEpiFields.Destroy;
var
  F: TEpiField;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      F := TEpiField(FList.Last);
      FreeAndNil(F);
    end;
    FList.Delete(FList.Count - 1);
  end;
  FreeAndNil(FList);
  inherited;
end;

function TEpiFields.FieldByName(Const aFieldName: string): TEpiField;
var
  i: LongInt;
begin
  Result := nil;
  i := IndexOf(aFieldName);
  if i >= 0 then
    Result := TEpiField(FList[i]);
end;

function TEpiFields.FieldById(const aId: string): TEpiField;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(TEpiField(FList[i]).Id, aId) = 0 then
    begin
      result := TEpiField(FList[i]);
      exit;
    end
end;

function TEpiFields.FieldExists(Const aFieldName: string): boolean;
begin
  result := Assigned(FieldByName(aFieldName));
end;

function TEpiFields.IndexOf(const aFieldName: string): integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(TEpiField(FList[i]).FieldName, aFieldName) = 0 then
    begin
      result := i;
      exit;
    end
end;

procedure TEpiFields.Add(aField: TEpiField);
begin
  if Owned then
  begin
    aField.FOwner := self;
    aField.FDataFile := Self.FDataFile;
  end;
  FList.Add(aField);
  if ReportOnChange and Assigned(FDataFile) then
    FDataFile.DoChange(dceAddField, aField);
end;

procedure TEpiFields.Delete(aField: TEpiField);
var
  Idx: Integer;
begin
  Idx := FList.IndexOf(aField);
  FList.Delete(Idx);

  if Owned then
  begin
    aField.FOwner := nil;
    aField.FDataFile := nil;
  end;

  if ReportOnChange and Assigned(FDataFile) then
    FDataFile.DoChange(dceRemoveField, aField);
end;

{ TEpiFieldProperties }

{function TEpiFieldProperties.GetAutoSearch: Boolean;
begin
  Result := AutoFields <> '';
end;       }

procedure TEpiFieldProperties.InternalReset;
begin
  if Assigned(FBeforeCmds)    then FreeAndNil(FBeforeCmds);
  if Assigned(FAfterCmds)     then FreeAndNil(FAfterCmds);
  if Assigned(FFieldComments) then FreeAndNil(FFieldComments);
  if Assigned(FRanges)        then FreeAndNil(FRanges);
  if Assigned(FJumps)         then FreeAndNil(FJumps);

  FEntryType         := entAny;
  FTopOfScreen       := -1;
  FDoRepeat          := false;
  FConfirm           := false;
//  FAutoFields        := '';
//  FAutoList          := false;
  FJumpResetValue    := '';
  FShowValueLabel    := false;
  FTypeType          := ttNone;
  FTypeColour         := 0;
  FTypeField         := nil;
  FHasGlobalDefaultVal := false;
  FieldScope         := scNone;
end;

constructor TEpiFieldProperties.Create;
begin
  Reset;
end;

destructor TEpiFieldProperties.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 3);
  try
    InternalReset();
    inherited;
  finally
    EpiLogger.DecIndent;
  end;
end;

procedure TEpiFieldProperties.Clone(var Dest: TEpiFieldProperties);
var
  TmpCmd: TChkCommands;
  i: Integer;
begin
  if Not Assigned(Dest) then
    Dest := TEpiFieldProperties.Create();

  Dest.Reset;

  Dest.FEntryType         := FEntryType;
  Dest.FTopOfScreen       := FTopOfScreen;
  Dest.FDoRepeat          := FDoRepeat;
  Dest.FConfirm           := FConfirm;
//  Dest.FAutoFields        := FAutoFields ;
//  Dest.FAutoList          := FAutoList;
  Dest.FJumpResetValue    := FJumpResetValue;
  Dest.FShowValueLabel    := FShowValueLabel;
  Dest.FTypeType          := FTypeType;
  Dest.FTypeColour         := FTypeColour;
  Dest.FTypeField         := FTypeField;
  Dest.FHasGlobalDefaultVal := FHasGlobalDefaultVal;
  Dest.FFieldScope        := FFieldScope;

  TmpCmd := Dest.FBeforeCmds;
  FBeforeCmds.Clone(TmpCmd);
  TmpCmd := Dest.FAfterCmds;
  FAfterCmds.Clone(TmpCmd);

  Dest.FFieldComments.Assign(FFieldComments);
  Dest.FRanges.Assign(FRanges);
  Dest.FJumps.Assign(FJumps);
end;

procedure TEpiFieldProperties.Reset;
begin
  InternalReset;

  FBeforeCmds := TChkCommands.Create;
  FAfterCmds  := TChkCommands.Create;
  FFieldComments := TStringList.Create;
  FRanges        := TStringList.Create;
  FJumps         := TStringList.Create;
end;

{ TEpiField }

procedure TEpiField.CheckIndex(const index: integer);
begin
  if (Index < 1) or (Index > Size) then
    Raise Exception.CreateFmt('Index out of bounds: %d', [Index]);
end;

function TEpiField.GetAsValueLabel(const index: Integer): string;
begin
  result := AsString[Index];
  if IsMissing[index] then exit;
  if Assigned(ValueLabelSet) then
    result := ValueLabelSet.ValueLabel[AsString[Index]];
end;

function TEpiField.GetIsMissingValue(const index: Integer): boolean;
begin
  result := false;
  if not Assigned(ValueLabelSet) then
    exit
  else
    result := ValueLabelSet.MissingValue[AsValue[Index]];
end;

{function TEpiField.GetAsFmtData: string;
begin
  case FieldType of
    ftDate, ftEuroDate, ftYMDDate:
      Result := StringReplace(AsData, EpiInternalFormatSettings.DateSeparator,
        EpiExternalFormatSettings.DateSeparator, [rfReplaceAll]);
    ftFloat:
      result := StringReplace(AsData, EpiInternalFormatSettings.DecimalSepator,
        EpiExternalFormatSettings.DecimalSepator, [rfReplaceAll]);
  else
    result := AsData;
  end;
end;}

function TEpiField.GetSize: Integer;
begin
  result := FSize;
end;

procedure TEpiField.Grow;
begin
  Capacity := Trunc(Capacity * Field_Growth_Factor);
end;

procedure TEpiField.SetSize(const AValue: Integer);
begin
  if AValue = Size then exit;
  if AValue > Capacity then
    Capacity := AValue;
  FSize := AValue;
end;

function TEpiField.GetHasFieldProperties: boolean;
begin
  result := Assigned(FFieldProperties);
end;

procedure TEpiField.SetFieldDecimals(const AValue: Cardinal);
var
  Val: Cardinal;
begin
  if FFieldDecimals = AValue then exit;
  Val := FieldDecimals;
  FFieldDecimals := AValue;
  DoChange(fceDecimals, @Val);
end;

procedure TEpiField.SetFieldLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if FFieldLeft = AValue then exit;
  Val := FFieldLeft;;
  FFieldLeft := AValue;
  DoChange(fceFLeft, @Val);
end;

procedure TEpiField.SetFieldLength(const AValue: Cardinal);
var
  Val: Cardinal;
begin
  if FFieldLength = AValue then exit;
  Val := FieldLength;
  FFieldLength := AValue;
  DoChange(fceLength, @Val);
end;

procedure TEpiField.SetFieldName(const AValue: string);
var
  S: String;
begin
  if FFieldName = AValue then exit;
  S := FieldName;
  FFieldName := AValue;
  DoChange(fceName, @S);
end;

procedure TEpiField.SetFieldTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if FFieldTop = AValue then exit;
  Val := FFieldTop;
  FFieldTop := AValue;
  DoChange(fceFTop, @Val);
end;

procedure TEpiField.SetVariableLabel(const AValue: string);
var
  Val: String;
begin
  if FVariableLabel = AValue then exit;
  Val := VariableLabel;
  FVariableLabel := AValue;
  DoChange(fceVarLabel, @Val);
end;

procedure TEpiField.SetVarLabelLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if FVarLabelLeft = AValue then exit;
  Val := FVarLabelLeft;
  FVarLabelLeft := AValue;
  DoChange(fceVLeft, @Val);
end;

procedure TEpiField.SetVarLabelTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if FVarLabelTop = AValue then exit;
  Val := FVarLabelTop;
  FVarLabelTop := AValue;
  DoChange(fceVTop, @Val);
end;

function TEpiField.GetFieldProperties: TEpiFieldProperties;
begin
  if not Assigned(FFieldProperties) then
    FFieldProperties := TEpiFieldProperties.Create;
  result := FFieldProperties;
end;

constructor TEpiField.Create(ASize: Cardinal; AFieldType: TFieldType);
var
  i: Integer;
begin
  Reset;
  Size := ASize;
  FFieldType := AFieldType;
end;

procedure TEpiField.DoChange(EventType: TEpiFieldChangeEventType;
  OldValue: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeCount - 1 do
    FOnChange[i](Self, EventType, OldValue);
end;

class function TEpiField.CreateField(aFieldType: TFieldType; aSize: Cardinal): TEpiField;
begin
  case aFieldType of
    ftInteger, ftIDNum:
      Result := TEpiIntField.Create(aSize, aFieldType);

    ftDate, ftToday, ftEuroDate,
    ftEuroToday, ftYMDDate,ftYMDToday:
      Result := TEpiDateField.Create(aSize, aFieldType);

    ftFloat:
      Result := TEpiFloatField.Create(aSize, aFieldType);

    ftBoolean:
      Result := TEpiBoolField.Create(aSize, aFieldType);

    ftString, ftUpperAlfa, ftSoundex, ftCrypt:
      Result := TEpiStringField.Create(aSize, aFieldType);
  else
    raise Exception.Create(Format('Invalid fieldtype: %s', [FieldTypeToFieldTypeName(aFieldType, nil)]));
  end;
end;

destructor TEpiField.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 3, 'Fieldname = ' + FieldName);
  try
    Reset();
    inherited;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiField.Clone(DstDataFile: TEpiDataFile = nil; CloneData: boolean = true): TEpiField;
var
  TmpCheckField: TEpiFieldProperties;
  I: Integer;
  Ptr: Pointer;
begin
  Result := TEpiField.CreateField(FieldType, Size);
  Result.FDataFile := DstDataFile;

  // Copy Field related values:
  Result.FFieldName     := FFieldName;
  Result.FFieldType     := FFieldType;
  Result.FFieldLength   := FFieldLength;
  Result.FFieldDecimals := FFieldDecimals;
  Result.FVariableLabel := FVariableLabel;
  Result.FDefaultValue  := FDefaultValue;

  // TODO : Clone screen properties.


  // Copy CheckFile if present:
  TmpCheckField := nil;
  if Assigned(FieldProperties) then
    FieldProperties.Clone(TmpCheckField);
  Result.FFieldProperties := TmpCheckField;

  // Assign data
  if CloneData then
    Result.AssignData(Self);

  // Scenarios:
  // - 1: Result field has a link to a Datafile. Then any potential valuelabels should be found
  //      through Result.Datafile's valuelabelsets.
  // - 2: Result has NO Datafile. This could be a temporary clone, etc.
  //      Copy the Valuelabelset, assign it to Result field and let
  //      TEpiDataFile.AddField handle Valuelabels.
  //      This will reset label type (to vlsLocal), since at present it is not known
  //      how the valuelabelset is related to anything else...
  if Assigned(ValueLabelSet) then
  begin
    if Assigned(Result.DataFile) then
    begin
      Result.ValueLabelSet := Result.DataFile.ValueLabels.ValueLabelSetByName(ValueLabelSet.Name)
    end else begin
      ValueLabelSet.Clone(Result.FValueLabelSet);
      Result.ValueLabelSet.LabelScope := vlsLocal;
    end;
  end;

{  // Index?
  if Assigned(DataFile) then
  begin
    I := DataFile.IndexFile.IndexNoByName(FieldName);
    if (I > 0) and (Assigned(Result.DataFile)) then
    begin
     Result.DataFile.IndexFile.IndexFields[I] := Result;
     Result.DataFile.IndexFile.IndexUnique[I] := DataFile.IndexFile.IndexUnique[I];
    end;
  end;    }
end;

procedure TEpiField.NewRecords(ACount: Integer = 1);
begin
  if ACount <= 0 then exit;
  if (Size + ACount) > Capacity then
    Grow;
  Size := Size + ACount;
end;

procedure TEpiField.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiField.EndUpdate;
begin
  Dec(FUpdateCount);

  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(fceUpdate, nil);
end;

procedure TEpiField.RegisterOnChangeHook(Event: TEpiFieldChangeEvent);
begin
  Inc(FOnChangeCount);
  ReAllocMem(FOnChange, FOnChangeCount * SizeOf(TEpiFieldChangeEvent));
  FOnChange[FOnChangeCount-1] := Event
end;

procedure TEpiField.UnRegisterOnChangeHook(Event: TEpiFieldChangeEvent);
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeCount -1 do
  begin
    if FOnChange[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeCount then exit;

  dec(FOnChangeCount);
  if FOnChangeCount > Idx then
    System.Move(FOnChange[Idx+1], FOnChange[Idx], (FOnChangeCount-Idx)*SizeOf(TEpiFieldChangeEvent));
  ReAllocMem(FOnChange, FOnChangeCount*SizeOf(TEpiFieldChangeEvent));
end;

procedure TEpiField.RegisterOnChangeDataHook(Event: TEpiFieldChangeDataEvent);
begin
  Inc(FOnChangeDataCount);
  ReAllocMem(FOnChangeData, FOnChangeDataCount * SizeOf(TEpiFieldChangeDataEvent));
  FOnChangeData[FOnChangeDataCount-1] := Event
end;

procedure TEpiField.UnRegisterOnChangeDataHook(Event: TEpiFieldChangeDataEvent
  );
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeDataCount -1 do
  begin
    if FOnChangeData[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeDataCount then exit;

  dec(FOnChangeDataCount);
  if FOnChangeDataCount > Idx then
    System.Move(FOnChangeData[Idx+1], FOnChangeData[Idx], (FOnChangeDataCount-Idx)*SizeOf(TEpiFieldChangeDataEvent));
  ReAllocMem(FOnChangeData, FOnChangeDataCount*SizeOf(TEpiFieldChangeDataEvent));
end;

procedure TEpiField.Reset;
begin
  if Assigned(FFieldProperties) then FreeAndNil(FFieldProperties);

  // System props:
  FOwner         := nil;
  FDataFile      := nil;
  FScreenProps   := nil;
  ReAllocMem(FOnChange, 0);
  ReAllocMem(FOnChangeData, 0);

  // Field props:
  FFieldName     := '';
  FFieldLength   := 0;
  FFieldDecimals := 0;
  FFieldLeft     := 0;
  FFieldTop      := 0;

  // Label props:
  FVariableLabel := '';
  FDefaultValue  := '';

  // Valuelabel props:
  FValueLabelSet := nil;
  FValueLabelIsFieldRef := false;
end;


{ TEpiIntField }

procedure TEpiIntField.AssignData(const Source: TEpiField);
var
  i: Integer;
begin
  if Source is TEpiIntField then
    Move(TEpiIntField(Source).FData[0], FData[0], Size * SizeOf(EpiInteger))
  else begin
    for i := 1 to Size do
      AsValue[i] := Source.AsValue[i];
  end;
end;

function TEpiIntField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsInteger[index] > 0 then
    result := 1
  else
    result := 0;
end;

function TEpiIntField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsInteger(const index: Integer): EpiInteger;
begin
  CheckIndex(Index);
  result := FData[Index - 1];
end;

function TEpiIntField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := IntToStr(AsInteger[Index]);
end;

function TEpiIntField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsInteger[index];
end;

function TEpiIntField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiIntField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsInteger[Index] = DefaultMissing;
end;

procedure TEpiIntField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := AValue;
end;

procedure TEpiIntField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := AValue;
end;

procedure TEpiIntField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
  // TODO : Rounding function.
    AsInteger[Index] := trunc(AValue);
end;

procedure TEpiIntField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  CheckIndex(Index);
  FData[Index - 1] := AValue;
end;

procedure TEpiIntField.SetAsString(const index: Integer; const AValue: EpiString
  );
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[index] := StrToIntDef(AValue, DefaultMissing);
end;

procedure TEpiIntField.SetAsValue(const index: Integer; const AValue: EpiVariant
  );
begin
  AsInteger[index] := AValue;
end;

procedure TEpiIntField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  SetLength(FData, AValue);
  FCapacity := AValue;
end;

procedure TEpiIntField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsInteger[index] := DefaultMissing;
end;

class function TEpiIntField.CheckMissing(AValue: EpiInteger): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiIntField.DefaultMissing: EpiInteger;
begin
  result := NA_INT;
end;

constructor TEpiIntField.Create(ASize: Cardinal; AFieldType: TFieldType);
begin
  if not (AFieldType in IntFieldTypes) then
    Raise Exception.Create(Format('Cannot create %s. Wrong fieldtype: %d', [ClassName, FieldTypeToFieldTypeName(AFieldType, nil)]));
  inherited Create(ASize, AFieldType);
end;

function TEpiIntField.Compare(i, j: integer): integer;
begin
  result := AsInteger[i] - AsInteger[j];
end;

destructor TEpiIntField.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiIntField.Exchange(i, j: integer);
var
  TmpInt: EpiInteger;
begin
  TmpInt := AsInteger[i];
  AsInteger[i] := AsInteger[j];
  AsInteger[j] := TmpInt;
end;


{ TEpiFloatField }

function TEpiFloatField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsFloat[index] > 0 then
    result := 1
  else
    result := 0;
end;

function TEpiFloatField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiFloatField.GetAsFloat(const index: Integer): EpiFloat;
begin
  CheckIndex(Index);
  Result := FData[index - 1];
end;

function TEpiFloatField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else   // TODO : Rounding function!!!
    result := Trunc(AsFloat[index]);
end;

function TEpiFloatField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := FloatToStr(AsFloat[Index]);
end;

function TEpiFloatField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsFloat[index];
end;

function TEpiFloatField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiFloatField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsFloat[index] = DefaultMissing;
end;

function TEpiFloatField.GetSize: Integer;
begin
  result := Length(FData);
end;

procedure TEpiFloatField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsDate(const index: Integer; const AValue: EpiDate
  );
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  CheckIndex(Index);
  FData[index - 1] := AValue;
end;

procedure TEpiFloatField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsString(const index: Integer;
  const AValue: EpiString);
var
  TmpStr: String;
begin
  TmpStr := StringReplace(AValue, ',', EpiInternalFormatSettings.DecimalSeparator, [rfReplaceAll]);
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := StrToFloatDef(TmpStr, DefaultMissing, EpiInternalFormatSettings);
end;

procedure TEpiFloatField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsFloat[index] := AValue;
end;

procedure TEpiFloatField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsFloat[index] := DefaultMissing;
end;

procedure TEpiFloatField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  SetLength(FData, AValue);
  FCapacity := AValue;
end;

procedure TEpiFloatField.AssignData(const Source: TEpiField);
var
  i: Integer;
begin
  if Source is TEpiFloatField then
    Move(TEpiFloatField(Source).FData[0], FData[0], Size * SizeOf(EpiFloat))
  else begin
    for i := 1 to Size do
      AsValue[i] := Source.AsValue[i];
  end;
end;

class function TEpiFloatField.CheckMissing(AValue: EpiFloat): boolean;
begin
  result := AValue = DefaultMissing;
end;

function TEpiFloatField.Compare(i, j: integer): integer;
var
  TmpFlt: EpiFloat;
begin
  result := 0;
  TmpFlt := AsFloat[i] - AsFloat[j];
  if TmpFlt < 0 then
    result := -1
  else if TmpFlt > 0 then
    result := 1;
end;

class function TEpiFloatField.DefaultMissing: EpiFloat;
begin
  result := NA_FLOAT;
end;

constructor TEpiFloatField.Create(ASize: Cardinal; AFieldType: TFieldType);
begin
  if not (AFieldType in FloatFieldTypes) then
    Raise Exception.Create(Format('Cannot create %s. Wrong fieldtype: %d', [ClassName, FieldTypeToFieldTypeName(AFieldType, nil)]));
  inherited Create(ASize, AFieldType);
end;

destructor TEpiFloatField.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiFloatField.Exchange(i, j: integer);
var
  TmpFlt: EpiFloat;
begin
  TmpFlt := AsFloat[i];
  AsFloat[i] := AsFloat[j];
  AsFloat[j] := TmpFlt;
end;

{ TEpiBoolField }

function TEpiBoolField.Compare(i, j: integer): integer;
begin
  Result := AsBoolean[i] - AsBoolean[j];
end;

procedure TEpiBoolField.Exchange(i, j: integer);
var
  TmpBool: EpiBool;
begin
  TmpBool := AsBoolean[i];
  AsBoolean[i] := AsBoolean[j];
  AsBoolean[j] := TmpBool;
end;

procedure TEpiBoolField.AssignData(const Source: TEpiField);
var
  i: Integer;
begin
  if Source is TEpiBoolField then
    Move(TEpiBoolField(Source).FData[0], FData[0], Size * SizeOf(EpiBool))
  else begin
    for i := 1 to Size do
      AsValue[i] := Source.AsValue[i];
  end;
end;

function TEpiBoolField.GetAsBoolean(const index: Integer): EpiBool;
begin
  CheckIndex(Index);
  result := FData[index - 1];
end;

function TEpiBoolField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else     // TODO: translation of boolean characters.
    result := BoolToStr(AsBoolean[index] = 0, 'Y', 'N')
end;

function TEpiBoolField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsBoolean[index];
end;

function TEpiBoolField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiBoolField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsBoolean[index] = DefaultMissing;
end;


function TEpiBoolField.GetSize: Integer;
begin
  result := Length(FData);
end;

procedure TEpiBoolField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  CheckIndex(Index);
  FData[index - 1] := AValue;
end;

procedure TEpiBoolField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if Length(AValue) > 0 then
  begin
    // TODO : Better recognition of boolean strings.
    if AValue[1] in BooleanYesChars then
      AsBoolean[Index] := 1
    else
      AsBoolean[Index] := 0;
  end else
    AsBoolean[Index] := 0;
end;

procedure TEpiBoolField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsBoolean[index] := AValue;
end;

procedure TEpiBoolField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsBoolean[index] := DefaultMissing;
end;

procedure TEpiBoolField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiBoolField.CheckMissing(AValue: EpiBool): boolean;
begin
  Result := AValue = DefaultMissing;
end;

class function TEpiBoolField.DefaultMissing: EpiBool;
begin
  Result := NA_BOOL;
end;

constructor TEpiBoolField.Create(ASize: Cardinal; AFieldType: TFieldType);
begin
  if not (AFieldType in BoolFieldTypes) then
    Raise Exception.Create(Format('Cannot create %s. Wrong fieldtype: %d', [ClassName, FieldTypeToFieldTypeName(AFieldType, nil)]));
  inherited Create(ASize, AFieldType);
end;

destructor TEpiBoolField.Destroy;
begin
  inherited Destroy;
end;

{ TEpiStringField }

procedure TEpiStringField.AssignData(const Source: TEpiField);
var
  i: Integer;
begin
  // Copying strings not recommended, as they are ref-counted.
  // Just use normal assignment.
  for i := 1 to Size do
    AsString[i] := Source.AsString[i];
end;

function TEpiStringField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if Trim(AsString[index]) <> '' then
    result := 1
  else
    result := 0;
end;

function TEpiStringField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else                   // TODO : Global date field type!!!
    result := Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := StrToFloatDef(AsString[index], TEpiFloatField.DefaultMissing);
end;

function TEpiStringField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := StrToIntDef(AsString[index], TEpiIntField.DefaultMissing);
end;

function TEpiStringField.GetAsString(const index: Integer): EpiString;
begin
  CheckIndex(Index);
  result := FData[index - 1];
end;

function TEpiStringField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsString[index];
end;

function TEpiStringField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiStringField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsString[index] = DefaultMissing;
end;

function TEpiStringField.GetSize: Integer;
begin
  result := Length(FData);
end;

procedure TEpiStringField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue >= 1 then
    AsString[Index] := 'Y' // TODO : Tranlation for boolean characters.
  else
    AsString[Index] := 'N' // TODO : Tranlation for boolean characters.
end;

procedure TEpiStringField.SetAsDate(const index: Integer; const AValue: EpiDate
  );
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                    // TODO : Define global date type.
    AsString[index] := EpiDateTimeToStr(AValue, ftDate);
end;

procedure TEpiStringField.SetAsFloat(const index: Integer;
  const AValue: EpiFloat);
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsString[index] := FloatToStr(AValue);
end;

procedure TEpiStringField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsString[index] := IntToStr(AValue);
end;

procedure TEpiStringField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  CheckIndex(Index);
  FData[index -1] := AValue;
end;

procedure TEpiStringField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsString[index] := AValue;
end;

procedure TEpiStringField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsString[index] := DefaultMissing;
end;

procedure TEpiStringField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiStringField.CheckMissing(AValue: EpiString): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiStringField.DefaultMissing: EpiString;
begin
  result := NA_STRING;
end;

constructor TEpiStringField.Create(ASize: Cardinal; AFieldType: TFieldType);
begin
  if not (AFieldType in StringFieldTypes) then
    Raise Exception.Create(Format('Cannot create %s. Wrong fieldtype: %d', [ClassName, FieldTypeToFieldTypeName(AFieldType, nil)]));
  inherited Create(ASize, AFieldType);
end;

function TEpiStringField.Compare(i, j: integer): integer;
begin
  result := AnsiCompareText(AsString[i], AsString[j]);
end;

destructor TEpiStringField.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiStringField.Exchange(i, j: integer);
var
  TmpStr: string;
begin
  TmpStr := AsString[i];
  AsString[i] := AsString[j];
  AsString[j] := TmpStr;
end;

{ TEpiDateField }

procedure TEpiDateField.AssignData(const Source: TEpiField);
var
  i: Integer;
begin
  if Source is TEpiDateField then
    Move(TEpiDateField(Source).FData[0], FData[0], Size * SizeOf(EpiDate))
  else begin
    for i := 1 to Size do
      AsValue[i] := Source.AsValue[i];
  end;
end;

function TEpiDateField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsDate[index] >= 1 then
    result := 1
  else
    result := 0;
end;

function TEpiDateField.GetAsDate(const index: Integer): EpiDate;
begin
  CheckIndex(Index);
  result := FData[index - 1];
end;

function TEpiDateField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsDate[index];
end;

function TEpiDateField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := AsDate[index];
end;

function TEpiDateField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := EpiDateTimeToStr(AsDate[index], FieldType);
end;

function TEpiDateField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsDate[index];
end;

function TEpiDateField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiDateField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsDate[index] = DefaultMissing;
end;

function TEpiDateField.GetSize: Integer;
begin
  result := Length(FData);
end;

procedure TEpiDateField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDate[index] := AValue;
end;

procedure TEpiDateField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  CheckIndex(Index);
  FData[index - 1] := AValue;
end;

procedure TEpiDateField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDate[index] := AValue;
end;

procedure TEpiDateField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                   // TODO : Date conversion routine.
    AsDate[index] := Trunc(EpiDateToDateTime(AValue, FieldType, Length(AValue)));
end;

procedure TEpiDateField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsDate[index] := AValue;
end;

procedure TEpiDateField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsDate[index] := DefaultMissing;
end;

procedure TEpiDateField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiDateField.CheckMissing(AValue: EpiDate): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateField.DefaultMissing: EpiDate;
begin
  result := NA_DATE;
end;

constructor TEpiDateField.Create(ASize: Cardinal; AFieldType: TFieldType);
begin
  if not (AFieldType in DateFieldTypes) then
    Raise Exception.Create(Format('Cannot create %s. Wrong fieldtype: %d', [ClassName, FieldTypeToFieldTypeName(AFieldType, nil)]));
  inherited Create(ASize, AFieldType);
end;

function TEpiDateField.Compare(i, j: integer): integer;
begin
  result := AsDate[i] - AsDate[j];
end;

destructor TEpiDateField.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiDateField.Exchange(i, j: integer);
var
  TmpDate: EpiDate;
begin
  TmpDate := AsDate[i];
  AsDate[i] := AsDate[j];
  AsDate[j] := TmpDate;
end;

end.

