{%MainUnit epidatafile.pas}

type

  { TEpiFields }

  TEpiFields = class(TEpiCustomClass)
  private
    FOwned:     Boolean;
    FDataFile:  TEpiDataFile;
    FList:      TList;
    function    GetField(Index: Integer): TEpiField;
    function    GetCount: Cardinal;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Sort(Cmp: TListSortCompare);
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

  TEpiFieldProperties = class(TEpiCustomClass)
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

  TEpiField = class(TEpiCustomClass)
  private
    FDataFile: TEpiDataFile;
    FFieldLeft: Integer;
    FFieldTop: Integer;
    FId: string;
    FOwner:         TEpiFields;
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
    destructor Destroy; override;
    procedure SaveToStream(St: TStream; Lvl: integer); override;
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Reset;
    function  Clone(DstDataFile: TEpiDataFile = nil; CloneData: boolean = true): TEpiField;
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

