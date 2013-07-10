{%MainUnit epidatafiles.pas}

type

  TEpiFieldsChangeEventType = (
    efceSetDecimal, efceSetLeft, efceSetLength,
    efceSetTop, efceSetSize, efceData, efceEntryMode, efceConfirmEntry,
    efceShowValueLabel, efceShowValueLabelNotes, efceRepeatValue, efceDefaultValue,
    efceValueLabelWriteTo, efceForcePickList, efceValueLabelSet
  );

  { TEpiFields }

  TEpiFields = class(TEpiCustomControlItemList)
  private
    function GetDataFile: TEpiDataFile;
    function GetField(index: integer): TEpiField;
    function GetFieldByName(const AName: string): TEpiField;
  protected
    procedure   DoAssignList(const EpiCustomList: TEpiCustomList); override;
    function    Prefix: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
      nil): TEpiCustomBase; override;

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    FieldExists(AField: TEpiField): boolean;
    property    DataFile: TEpiDataFile read GetDataFile;
    property    Field[index: integer]: TEpiField read GetField; default;
    property    FieldByName[Const AName: string]: TEpiField read GetFieldByName;
  end;

  { TEpiField }

  TEpiField = class(TEpiCustomControlItem)

  { Data access properties }
  private
    procedure   LocalEventHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function    GetIsMissingValue(const index: Integer): boolean;
    procedure   SetIsMissingValue(const index: Integer; const AValue: boolean);
  protected
    procedure   CheckIndex(Const index: integer); virtual;
    function    GetAsBoolean(const index: Integer): EpiBool; virtual; abstract;
    function    GetAsDate(const index: Integer): EpiDate; virtual; abstract;
    function    GetAsDateTime(const index: Integer): EpiDateTime; virtual; abstract;
    function    GetAsFloat(const index: Integer): EpiFloat; virtual; abstract;
    function    GetAsInteger(const index: Integer): EpiInteger; virtual; abstract;
    function    GetAsString(const index: Integer): EpiString; virtual; abstract;
    function    GetAsTime(const index: Integer): EpiTime; virtual; abstract;
    function    GetAsValue(const index: Integer): EpiVariant; virtual; abstract;
    function    GetCapacity: Integer; virtual; abstract;
    function    GetIsMissing(const index: Integer): boolean; virtual; abstract;
    procedure   SetAsBoolean(const index: Integer; const AValue: EpiBool); virtual; abstract;
    procedure   SetAsDate(const index: Integer; const AValue: EpiDate); virtual; abstract;
    procedure   SetAsDateTime(const index: Integer; const AValue: EpiDateTime); virtual; abstract;
    procedure   SetAsFloat(const index: Integer; const AValue: EpiFloat); virtual; abstract;
    procedure   SetAsInteger(const index: Integer; const AValue: EpiInteger); virtual; abstract;
    procedure   SetAsString(const index: Integer; const AValue: EpiString); virtual; abstract;
    procedure   SetAsTime(const index: Integer; const AValue: EpiTime); virtual; abstract;
    procedure   SetAsValue(const index: Integer; const AValue: EpiVariant); virtual; abstract;
    procedure   SetCapacity(AValue: Integer); virtual; abstract;
    procedure   SetIsMissing(const index: Integer; const AValue: boolean); virtual; abstract;
  public
    property    AsBoolean[const index: Integer]: EpiBool read GetAsBoolean write SetAsBoolean;
    property    AsInteger[const index: Integer]: EpiInteger read GetAsInteger write SetAsInteger; default;
    property    AsFloat[const index: Integer]: EpiFloat read GetAsFloat write SetAsFloat;
    property    AsDate[const index: Integer]: EpiDate read GetAsDate write SetAsDate;
    property    AsTime[const index: Integer]: EpiTime read GetAsTime write SetAsTime;
    property    AsDateTime[const index: Integer]: EpiDateTime read GetAsDateTime write SetAsDateTime;
    property    AsString[const index: Integer]: EpiString read GetAsString write SetAsString;
    property    AsValue[const index: Integer]: EpiVariant read GetAsValue write SetAsValue;
    property    IsMissing[const index: Integer]: boolean read GetIsMissing write SetIsMissing;
    property    IsMissingValue[const index: Integer]: boolean read GetIsMissingValue write SetIsMissingValue;

  { Field Properties }
  private
    FCalculation: TEpiCalculation;
    FComparison: TEpiComparison;
    FConfirmEntry: Boolean;
    FDecimals: Cardinal;
    FEntryMode: TEpiEntryMode;
    FForcePickList: Boolean;
    FJumps: TEpiJumps;
    FLength: Cardinal;
    FFieldType: TEpiFieldType;
    FNotes: TEpiTranslatedTextWrapper;
    FRanges: TEpiRanges;
    FRepeatValue: boolean;
    FShowValueLabel: Boolean;
    FShowValueLabelNotes: boolean;
    FValueLabelSet: TEpiValueLabelSet;
    FQuestion:  TEpiTranslatedTextWrapper;
    FValueLabelWriteField: TEpiField;
    function    GetDefaultValueAsString: string;
    procedure   SetConfirmEntry(const AValue: Boolean);
    procedure   SetDecimals(const AValue: Cardinal);
    procedure   SetDefaultValueAsString(const AValue: string);
    procedure   SetEntryMode(const AValue: TEpiEntryMode);
    procedure   SetForcePickList(const AValue: Boolean);
    procedure   SetLength(const AValue: Cardinal);
    procedure   SetRepeatValue(const AValue: boolean);
    procedure   SetShowValueLabel(const AValue: Boolean);
    procedure   SetShowValueLabelNotes(AValue: boolean);
    procedure   SetValueLabelSet(const AValue: TEpiValueLabelSet);
    procedure   SetValueLabelWriteField(const AValue: TEpiField);
  protected
    function    DoGetDefaultValueAsString: string; virtual; abstract;
    procedure   DoSetDefaultValueAsString(const AValue: string); virtual; abstract;
    function    GetHasDefaultValue: boolean; virtual; abstract;
    procedure   SetHasDefaultValue(Const AValue: boolean); virtual; abstract;
  public
    procedure   ResetDefaultValue; virtual; abstract;
    property    FieldType: TEpiFieldType read FFieldType;
    property    Length:    Cardinal read FLength write SetLength;
    property    Decimals:  Cardinal read FDecimals write SetDecimals;
    property    EntryMode: TEpiEntryMode read FEntryMode write SetEntryMode;
    property    ConfirmEntry: Boolean read FConfirmEntry write SetConfirmEntry;
    property    RepeatValue: boolean read FRepeatValue write SetRepeatValue;
    // Since DefaultValue is Type Specific - it's implemented in sub-classes
//    property    DefaultValue: Variant read FDefaultValue write SetDefaultValue;
    property    DefaultValueAsString: string read GetDefaultValueAsString write SetDefaultValueAsString;
    property    HasDefaultValue: boolean read GetHasDefaultValue write SetHasDefaultValue;
    property    Notes: TEpiTranslatedTextWrapper read FNotes;
    property    Question:  TEpiTranslatedTextWrapper read FQuestion;
    property    ValueLabelSet: TEpiValueLabelSet read FValueLabelSet write SetValueLabelSet;
    property    ShowValueLabel: Boolean read FShowValueLabel write SetShowValueLabel;
    property    ForcePickList: Boolean read FForcePickList write SetForcePickList;
    property    ShowValueLabelNotes: boolean read FShowValueLabelNotes write SetShowValueLabelNotes;
    property    ValueLabelWriteField: TEpiField read FValueLabelWriteField write SetValueLabelWriteField;
    property    Ranges:    TEpiRanges read FRanges write FRanges;
    property    Jumps:     TEpiJumps read FJumps write FJumps;
    property    Calculation: TEpiCalculation read FCalculation write FCalculation;
    property    Comparison:  TEpiComparison read FComparison write FComparison;

  { Pack Data}
  protected
    procedure   MovePackData(Const SrcIdx, DstIdx, Count: integer); virtual; abstract;

  { Helper methods }
  private
    FDataFile: TEpiDataFile;
    FSize: Integer;
    FCapacity: Integer;
    function    GetSection:  TEpiSection;
    function    GetSettings: TEpiXMLSettings;
  protected
    procedure   SetDataModified;
    function    GetSize: Integer; virtual;
    procedure   SetSize(const AValue: Integer); virtual;
    property    Capacity: Integer read GetCapacity write SetCapacity;
    property    Settings: TEpiXMLSettings read GetSettings;
    function    DoCompare(i, j: integer): integer; virtual; abstract;
  public
    procedure   Exchange(i,j: integer); virtual; abstract;
    function    Compare(i,j: integer): integer;
    function    FormatString(Const FillSpace: boolean = false): string; virtual; abstract;
    procedure   ResetData; virtual; abstract;
    property    Size:      Integer read GetSize write SetSize;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Section:  TEpiSection read GetSection;

  { Class inheritance, load/save, etc. }
  protected
    // Never ever create a field directly!
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType); virtual;
    procedure DoChange(EventGroup: TEpiEventGroup; EventType: Word;
       Data: Pointer); override;
  public
    class function CreateField(AOwner: TEpiCustomBase;
      AFieldType: TEpiFieldType): TEpiField;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    SaveAttributesToXml: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
    procedure PostCloneUpdate(Dest: TEpiField);
  end;
  TEpiFieldClass = class of TEpiField;

  { TEpiIntField }
  // Handles following field types:
  // ftInteger, ftAutoInc
  TEpiIntField = class(TEpiField)
  private
    FData: array of EpiInteger;
    FDefaultValue: EpiInteger;
    procedure SetDefaultValue(const AValue: EpiInteger);
  protected
    function DoGetDefaultValueAsString: string; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetHasDefaultValue: boolean; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
       override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function  DoCompare(i, j: integer): integer; override;
    function  DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    class function CheckMissing(AValue: EpiInteger): boolean;
    class function DefaultMissing: EpiInteger;
    procedure Exchange(i, j: integer); override;
    function  FormatString(Const FillSpace: boolean = false): string; override;
    procedure ResetData; override;
    property  DefaultValue: EpiInteger read FDefaultValue write SetDefaultValue;
  end;

  { TEpiFloatField }
  // Handles following field types:
  // ftFloat
  TEpiFloatField = class(TEpiField)
  private
    FData: array of EpiFloat;
    FDefaultValue: EpiFloat;
    procedure SetDefaultValue(const AValue: EpiFloat);
  protected
    function DoGetDefaultValueAsString: string; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
       override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function  DoCompare(i, j: integer): integer; override;
    function  DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    class function CheckMissing(AValue: EpiFloat): boolean;
    class function DefaultMissing: EpiFloat;
    procedure Exchange(i, j: integer); override;
    function  FormatString(Const FillSpace: boolean = false): string; override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    procedure ResetData; override;
    property  DefaultValue: EpiFloat read FDefaultValue write SetDefaultValue;
  end;

  { TEpiBoolField }
  // Handles following field types:
  // ftBoolean
  TEpiBoolField = class(TEpiField)
  private
    FData: array of EpiBool;
    FDefaultValue: EpiBool;
    procedure SetDefaultValue(const AValue: EpiBool);
  protected
    function DoGetDefaultValueAsString: string; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
       override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function  DoCompare(i, j: integer): integer; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    class function CheckMissing(AValue: EpiBool): boolean;
    class function DefaultMissing: EpiBool;
    procedure Exchange(i, j: integer); override;
    function FormatString(const FillSpace: boolean = false): string; override;
    procedure ResetData; override;
    property  DefaultValue: EpiBool read FDefaultValue write SetDefaultValue;
  end;

  { TEpiStringField }
  // Handles following field types:
  // ftString, ftUpperString
  TEpiStringField = class(TEpiField)
  private
    FData: array of EpiString;
    FDefaultValue: EpiString;
    procedure SetDefaultValue(const AValue: EpiString);
  protected
    function DoGetDefaultValueAsString: string; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
       override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function  DoCompare(i, j: integer): integer; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    class function CheckMissing(AValue: EpiString): boolean;
    class function DefaultMissing: EpiString;
    procedure Exchange(i, j: integer); override;
    function FormatString(const FillSpace: boolean = false): string; override;
    procedure ResetData; override;
    property  DefaultValue: EpiString read FDefaultValue write SetDefaultValue;
  end;


  { TEpiCustomAutoField }

  // State indication whether to update value on creation or on update/save.
  TEpiAutoUpdateMode = (umCreated, umFirstSave, umUpdated);
  TEpiCustomAutoField = class(TEpiField)
  private
    FAutoMode: TEpiAutoUpdateMode;
    procedure SetAutoMode(const AValue: TEpiAutoUpdateMode);
  protected
    function  SaveAttributesToXml: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property AutoMode: TEpiAutoUpdateMode read FAutoMode write SetAutoMode;
  end;


  { TEpiDateField }
  // Handles following field types:
  // ftDMYDate, ftDMYToday, ftMDYDate, ftMDYToday, ftYMDDate, ftYMDToday
  // - note: this is very much subject to change as we introduce time/timedate fields.
  TEpiDateField = class(TEpiCustomAutoField)
  private
    FData: array of EpiDate;
    FDefaultValue: EpiDate;
    procedure SetDefaultValue(const AValue: EpiDate);
  protected
    function DoGetDefaultValueAsString: string; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
       override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function  DoCompare(i, j: integer): integer; override;
    function  DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    class function CheckMissing(AValue: EpiDate): boolean;
    class function DefaultMissing: EpiDate;
    procedure Exchange(i, j: integer); override;
    function  FormatString(const FillSpace: boolean = false): string; override;
    procedure ResetData; override;
    property  DefaultValue: EpiDate read FDefaultValue write SetDefaultValue;
  end;

  { TEpiDateTimeField }
  // Handles following field types:
  // ftTime, ftTimeNow
  TEpiDateTimeField = class(TEpiCustomAutoField)
  private
    FData: array of EpiTime;
    FDefaultValue: EpiTime;
    procedure SetDefaultValue(const AValue: EpiTime);
  protected
    function DoGetDefaultValueAsString: string; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
       override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function  DoCompare(i, j: integer): integer; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    class function CheckMissing(AValue: EpiDateTime): boolean;
    class function DefaultMissing: EpiDateTime;
    procedure Exchange(i, j: integer); override;
    function  FormatString(const FillSpace: boolean = false): string; override;
    procedure ResetData; override;
    property  DefaultValue: EpiTime read FDefaultValue write SetDefaultValue;
  end;

