unit UEpiDataFile;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, UValueLabels, Rijndael, UCheckFileCmds, UEpiLog,
  UCheckFileTypes, UDataFileTypes, UEpiDataGlobals, DOM, sysutils;

type
  TEpiField = class;
  TEpiFields = class;
  TEpiDataFile = class;
  TEpiTextLabels = class;
  TEpiScreenProperties = class;


  { TEpiScreenProperties }

  { TEpiScreenProperty }

  TEpiScreenProperty = class(TObject)
  private
    FName: string;
    FOwner:    TEpiScreenProperties;
    FId:       string;
    FFgColour: integer;
    FBgColour: integer;
    FHlColour: integer;
  public
    constructor Create(AOwner: TEpiScreenProperties); virtual;
    destructor  Destroy; override;
    procedure   Clone(var Dest: TEpiScreenProperty);
    procedure   Reset;
    property    Id: string read FId write FId;
    property    Name: string read FName write FName;
    property    FgColour: integer read FFgColour write FFgColour;
    property    BgColour: integer read FBgColour write FBgColour;
    property    HlColour: integer read FHlColour write FHlColour;
  end;

  TEpiScreenProperties = class(TObject)
  private
    FOwned: Boolean;
    FOwner:   TEpiDataFile;
    FList:    TFPList;
    FReportOnChange: Boolean;
    FDefaultScreenProperty: TEpiScreenProperty;
    function GetCount: Cardinal;
    function GetDefaultScreenProperty: TEpiScreenProperty;
    function GetScreenProperty(Index: integer): TEpiScreenProperty;
  public
    constructor Create(AOwner: TEpiDataFile); virtual;
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    procedure   Add(aScreenProperty: TEpiScreenProperty);
    procedure   Delete(aScreenProperty: TEpiScreenProperty);
    function    ScreenPropertyById(Const aScreenPropertyId: string): TEpiScreenProperty;
    function    ScreenPropertyExists(Const aScreenPropertyId: string; var aScreenProperty: TEpiScreenProperty): boolean;
    function    IndexOf(Const aScreenPropertyId: string): integer;
    Property    DefaultScreenProperty: TEpiScreenProperty read GetDefaultScreenProperty;
    Property    ScreenProperty[Index: integer]: TEpiScreenProperty read GetScreenProperty; default;
    Property    Count: Cardinal read GetCount;
    Property    ReportOnChange: Boolean read FReportOnChange write FReportOnChange;
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

  { TEpiDataFileProperties }

  TEpiDataFileProperties = class(TObject)
  private
    FAutoFields:           TEpiFields;        // List of fields included in autosearch function.
    FAutoList:             Boolean;           // - flag indication is result found in above search is displayed as list (to the users).
    FBeforeFileCmds:       TChkCommands;      // Commands to be run when file is opened
    FAfterFileCmds:        TChkCommands;      // Commands to be run when file is closed
    FBeforeRecordCmds:     TChkCommands;      // Commands to be run before current record changes
    FAfterRecordCmds:      TChkCommands;      // Commands to be run when changing current record
    FRecodeCmds:           TChkCommands;      // Commands to be run during Recode Datafile
    FGlobalMissingValues:  TMissingValues;    // Globally defined missing values. TODO : Must be removed and incorporated into new valuelabel/missingvalue structure.
    FMissingAction:        TMissingAction;    // Flag to indicate if calculations involving missing evals to missing or 0 (zero).
    FShowLastRecord:       Boolean;           // if set, then last record is shown when datafile is opened; if false (default) then
    FErrorInFile:          Boolean;           // Flag indicating an error occured during read of .CHK file.
    FHasCheckFile:         Boolean;           // Flag indicating .REC file has a .CHK file.
    FFileName:             string;            // Filename of the benamed .CHK file (should be same as .REC both with .CHK extension.)
    FDefines:              TEpiFields;        // List of .CHK file defines fields.
    FHasTypeStatusBar:     Boolean;           // Flag indication if TYPE STATUSBAR is used (both .REC and XML file)
    FTypeStatusBarText:    String;            // - text to display if above flag is true
    FTypeStatusBarField:   TEpiField;         // - reference to field to display in statusbar.
    FTypeStatusBarColor:   Integer;           // - color of text displayed in statusbar.
    function    GetGlobMissing(Index: Integer): string;
    procedure   SetGlobMissing(Index: Integer; const Value: string);
    procedure   InternalReset;
  protected

  public
    Constructor Create;
    Destructor  Destroy; override;
    procedure   Reset;
    function    Clone: TEpiDataFileProperties;
    function    DefineExists(Const aName: string): Boolean;
    function    DefineByName(Const aName: string): TEpiField;
    procedure   AddDefine(Field: TEpiField);
    Property    BeforeFileCmds:     TChkCommands read FBeforeFileCmds write FBeforeFileCmds;
    Property    AfterFileCmds:      TChkCommands read FAfterFileCmds write FAfterFileCmds;
    Property    BeforeRecordCmds:   TChkCommands read FBeforeRecordCmds write FBeforeRecordCmds;
    Property    AfterRecordCmds:    TChkCommands read FAfterRecordCmds write FAfterRecordCmds;
    Property    RecodeCmds:         TChkCommands read FRecodeCmds write FRecodeCmds;
    Property    GlobalMissingVal[Index: Integer]: string read GetGlobMissing write SetGlobMissing;
    Property    MissingAction:      TMissingAction read FMissingAction write FMissingAction;
    Property    ShowLastRecord:     Boolean read FShowLastRecord write FShowLastRecord;
    Property    ErrorInFile:        Boolean read FErrorInFile write FErrorInFile;
    Property    HasCheckFile:       Boolean read FHasCheckFile write FHasCheckFile;
    Property    FileName:           string read FFileName write FFileName;
    property    HasTypeStatusBar:   boolean read FHasTypeStatusBar write FHasTypeStatusBar;
    property    TypeStatusBarText:  string read FTypeStatusBarText write FTypeStatusBarText;
    property    TypeStatusBarField: TEpiField read FTypeStatusBarField write FTypeStatusBarField;
    property    TypeStatusBarColor: Integer read FTypeStatusBarColor write FTypeStatusBarColor;
    property    AutoFields:         TEpiFields read FAutoFields;
    property    AutoList:           Boolean read FAutoList write FAutoList;
  end;

  { TEpiTextLabel }

  TEpiTextLabel = class(TObject)
  private
    FOwner:      TEpiTextLabels;
    FId:         string;
    FText:       string;
    FScreenProp: TEpiScreenProperty;
    FTextLeft: Integer;
    FTextTop: Integer;
    procedure SetId(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetTextLeft(const AValue: Integer);
    procedure SetTextTop(const AValue: Integer);
  public
    constructor Create(AOwner: TEpiTextLabels);
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    property    Id:         string read FId write SetId;
    property    Text:       string read FText write SetText;
    Property    TextTop:    Integer read FTextTop write SetTextTop;
    Property    TextLeft:   Integer read FTextLeft write SetTextLeft;
    property    ScreenProp: TEpiScreenProperty read FScreenProp write FScreenProp;
  private
    // Events (and control):
    FOnChange:     ^TEpiTextLabelChangeEvent;
    FOnChangeCount: Integer;
    FUpdateCount:   Integer;
    procedure DoChange(EventType: TEpiTextLabelChangeEventType; OldValue: Pointer);
  public
    // Events (and control):
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
    procedure UnRegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
  end;

  { TEpiTextLabels }

  TEpiTextLabels = class(TObject)
  private
    FOwned: Boolean;
    FOwner:     TEpiDataFile;
    FList:      TFPList;
    FReportOnChange: Boolean;
    function    GetCount: Cardinal;
    function    GetTextLabel(Index: integer): TEpiTextLabel;
  public
    constructor Create(AOwner: TEpiDataFile); virtual;
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    procedure   Add(aTextLabel: TEpiTextLabel);
    procedure   Delete(aTextLabel: TEpiTextLabel);
    function    TextLabelById(Const aTextLabelId: string): TEpiTextLabel;
    function    TextLabelExists(Const aTextLabelId: string): boolean;
    function    IndexOf(Const aTextLabelId: string): integer;
    Property    TextLabel[Index: integer]: TEpiTextLabel read GetTextLabel; default;
    Property    Count: Cardinal read GetCount;
    Property    ReportOnChange: Boolean read FReportOnChange write FReportOnChange;
    Property    Owned: Boolean read FOwned write FOwned;
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
//    function       GetAsFmtData: string;
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
//    property    AsFmtData:   string read GetAsFmtData;
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

  { TEpiIndexFile }

  TEpiIndexFile = class(TObject)
  private
    FFileName:       string;
    FIndexFields:    TList;
    FIndexUnique:    Byte;
    function         GetIndexField(Index: integer): TEpiField;
    procedure        SetIndexField(Index: Integer; aField: TEpiField);         
    function         GetIndexUnique(Index: integer): Boolean;
    procedure        SetIndexUnique(Index: Integer; aUnique: Boolean);         
    function         GetIndexCount: Integer;
    procedure        InternalReset;
  protected
  public
    constructor Create(Const aFileName: string);
    destructor  Destroy; override;
    function    Clone: TEpiIndexFile;
    procedure   Reset;
    function    IndexNoByName(Const Name: string): Integer;
    function    IndexFieldByName(Const Name: string): TEpiField;
    function    IndexExists(Const Name: string): Boolean;
    property    FileName: string read FFileName write FFileName;
    property    IndexFields[Index: integer]: TEpiField read GetIndexField write SetIndexField;
    property    IndexUnique[Index: integer]: Boolean read GetIndexUnique write SetIndexUnique;
    property    IndexCount: Integer read GetIndexCount;
  end;

  { TEpiDataFile }
  TEpiDataFile = class(TObject)
  private
    FDatafileType: TDataFileType;
    FFileName:     string;                  // Physical datafile name.
    FFileLabel:    string;                  // Label of datafile. (METADATA)
    FStudy:        string;                  // Study information (METADATA)
    FValueLabels:  TValueLabelSets;         // Valuelabels (METADATA)
    FFields:       TEpiFields;              // Container for all data fields.
    FTextLabels:   TEpiTextLabels;          // Container for all text headings on form.
    FScreenProperties: TEpiScreenProperties; // Container for all screen properties (for now just colours).
    FPassword:     string;                  // Datafile password
    FFieldNaming:  TFieldNaming;            // Datafile fieldnaming convention - Firstword or Auto.
    FCheckFile:    TEpiDataFileProperties;           // Container for global/datafile level information of the .CHK file.
    FOptions:      TEpiDataFileOptions;     // Options set by Open(...)
    FOnProgress:   TProgressEvent;          // Callback event for updating progress
    FOnTranslate:  TTranslateEvent;         // Callback event for translation
    FOnPassword:   TRequestPasswordEvent;   // Callback event for password request
    FBackgroundColour:     Integer;         // Background color up entry form.
    FIndexFile:    TEpiIndexFile;
    FErrorText:    string;
    FErrorCode:    Cardinal;
    FCrypter:      TDCP_rijndael;
    FFileVersion:  Cardinal;
    FRecordStatus: TEpiField;
    function   GetDeleted(Index: integer): boolean;
    function   GetField(Index: integer): TEpiField;
    function   GetIndexFile: TEpiIndexFile;
    function   GetSize: Integer;
    function   GetTextLabel(Index: integer): TEpiTextLabel;
    function GetTextLabelCount: Cardinal;
    function   GetVerified(Index: integer): boolean;
    procedure  InternalReset;
    procedure  SetDeleted(Index: integer; const AValue: boolean);
    procedure  SetFileLabel(const AValue: string);
    procedure  SetFileName(const AValue: string);
    procedure  SetSize(const AValue: Integer);
    procedure  SetStudy(const AValue: string);
    procedure  SetVerified(Index: integer; const AValue: boolean);
  private
    // XML Read Functions:
    // - read
    Procedure  ReportXmlError(ErrCode: Integer; LangCode: integer; Msg: String; Args: array of const);
    Procedure  ReadSettings(RootNode: TDOMElement; FmtSettings: TFormatSettings);
    Procedure  ReadMetaData(RootNode: TDOMElement; FmtSettings: TFormatSettings);
    Procedure  ReadValueLabels(RootNode: TDOMElement; LocalFmt: TFormatSettings);
    Procedure  ReadExternalValueLabels(RootNode: TDOMElement; ValueLabelSet: TValueLabelSet);
    Procedure  ReadInternalValueLabels(RootNode: TDOMElement; ValueLabelSet: TValueLabelSet; LocalFmt: TFormatSettings);
    Procedure  ReadScreen(RootNode: TDOMElement);
    Procedure  ReadScreenColours(RootNode: TDOMElement);
    Procedure  ReadTextLabels(RootNode: TDOMElement);
    Procedure  ReadFields(RootNode: TDOMElement);
    Procedure  ReadRecords(RootNode: TDOMElement; FmtSettings: TFormatSettings);
    // - write
    Function   Ins(Lvl: integer): string;
    Function   StringToXml(Const Src: String): string;
    Procedure  WriteSettings(St: TStream);
    Procedure  WriteMetaData(St: TStream);
    Procedure  WriteValueLabels(St: TStream);
{    Procedure  WriteScreen;
    Procedure  WriteScreenColours;
    Procedure  WriteTextLabels;
    Procedure  WriteFields;
    Procedure  WriteRecords(FmtSettings: TFormatSettings); }
  protected
    function   InternalOpen: boolean;
    function   InternalOpenOld: boolean;
    function   InternalSave: boolean;
    function   InternalSaveOld: boolean;
    function   Lang(LangCode: Integer; Const LangText: string): string;
    Function   UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
    Function   TextPos(var F: Textfile): Cardinal;
    function   GetFieldCount: Cardinal;
    procedure  DoChange(Event: TEpiDataFileChangeEventType; OldValue: Pointer);
  public
    constructor Create(ASize: Cardinal = 0); virtual;
    destructor Destroy; override;
    function   PrepareDataFile(FieldNames: TStrings): TEpiDataFile;
    function   Clone(CloneData: boolean = true): TEpiDataFile;
    function   RequestPassword(Const EncryptedString: string): boolean;
    function   Open(Const aFileName: string; aOptions: TEpiDataFileOptions = []): boolean;
    function   Save(Const aFileName: string; aOptions: TEpiDataFileOptions = []): boolean;
    procedure  Reset;
    procedure  AddField(AField: TEpiField);
    procedure  RemoveField(var AField: TEpiField; DoDestroy: boolean = false);
    function   FieldByName(Const aFieldName: string): TEpiField;
    function   FieldById(Const aId: string): TEpiField;
    function   FieldExists(Const aFieldName: string): boolean;
    function   FieldIndex(Const aFieldName: string): Integer;
    function   CreateUniqueFieldName(Const AText: string): string;
    procedure  AddTextLabel(ATextLabel: TEpiTextLabel);
    procedure  RemoveTextLabel(var ATextLabel: TEpiTextLabel; DoDestroy: boolean = false);
    function   TextLabelById(Const ATextLabelId: string): TEpiTextLabel;
    function   TextLabelExists(Const ATextLabelId: string): boolean;
    procedure  NewRecords(ACount: Integer = 1); virtual;
    procedure  SortFields(Cmp: TListSortCompare);
    function   DocumentDatafile: TStrings;
    property   Field[Index: integer]: TEpiField read GetField; default;
    property   Fields:      TEpiFields read FFields;
    property   TextLabel[Index: integer]: TEpiTextLabel read GetTextLabel;
    property   TextLabels:  TEpiTextLabels read FTextLabels;
    property   ValueLabels: TValueLabelSets read FValueLabels write FValueLabels;
    Property   ScreenProperties: TEpiScreenProperties read FScreenProperties;
    property   Deleted[Index: integer]: boolean read GetDeleted write SetDeleted;
    property   Verified[Index: integer]: boolean read GetVerified write SetVerified;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnPassword:  TRequestPasswordEvent read FOnPassword write FOnPassword;
    property   OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property   Options:     TEpiDataFileOptions read FOptions;
    property   FileName:    string read FFileName write SetFileName;
    property   FileLabel:   string read FFileLabel write SetFileLabel;
    property   Study:       string read FStudy write SetStudy;
    property   Password:    string read FPassword write FPassword;
    property   FieldNaming: TFieldNaming read FFieldNaming write FFieldNaming;
    Property   FieldCount:  Cardinal read GetFieldCount;
    Property   TextLabelCount: Cardinal read GetTextLabelCount;
    Property   Size:        Integer read GetSize write SetSize;
    Property   FileProperties:   TEpiDataFileProperties read FCheckFile;
    Property   IndexFile:   TEpiIndexFile read GetIndexFile;
    Property   ErrorCode:   Cardinal read FErrorCode write FErrorCode;
    Property   ErrorText:   string read FErrorText write FErrorText;
    Property   FileVersion: Cardinal read FFileVersion write FFileVersion;
    Property   DatafileType: TDataFileType read FDatafileType write FDatafileType;
    property   BackgroundColour: Integer read FBackgroundColour write FBackgroundColour;
  private
    // OnChange-hook privates
    FOnChangeList: ^TEpiDataFileChangeEvent;
    FOnChangeListCount: Integer;
    FUpdateCount: Integer;
  public
    // OnChange-hook methods
    procedure  BeginUpdate;
    procedure  EndUpdate;
    procedure  RegisterOnChangeHook(Event: TEpiDataFileChangeEvent);
    procedure  UnRegisterOnChangeHook(Event: TEpiDataFileChangeEvent);
  end;



implementation

uses
  UStringUtils, EpiBase64, UEpiUtils, FileUtil,
  StrUtils, UCheckFileIO, Math, UDateUtils, XMLRead,
  UImportExport;

const
  NA_INT       = MaxInt;
  NA_FLOAT     = MaxFloat;
  NA_DATE      = NA_INT;
  NA_TIME      = MaxDouble;
  NA_DATETIME  = NA_TIME;
  NA_STRING    = '.';
  NA_BOOL      = $7F;

  Field_Growth_Factor = 1.20;

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

{ TEpiDataFileProperties }

constructor TEpiDataFileProperties.Create;
begin
  Reset();
end;

destructor TEpiDataFileProperties.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 3);
  try
    InternalReset();
    inherited;
  finally
    EpiLogger.DecIndent;
  end
end;

procedure TEpiDataFileProperties.Reset;
begin
  InternalReset;

  FDefines        := TEpiFields.Create(nil);
  FDefines.Owned  := True;
  FAutoFields     := TEpiFields.Create(nil);
  FAutoFields.Owned := false;
end;

function TEpiDataFileProperties.Clone: TEpiDataFileProperties;
var
  i: Integer;
begin
  Result := TEpiDataFileProperties.Create;

  // Clone basic:
  Result.FShowLastRecord     := FShowLastRecord;
  Result.FMissingAction      := FMissingAction;

  // Command structures.
  if Assigned(FBeforeFileCmds) then
    FBeforeFileCmds.Clone(Result.FBeforeFileCmds);
  if Assigned(FAfterFileCmds) then
    FAfterFileCmds.Clone(Result.FAfterFileCmds);
  if Assigned(FBeforeRecordCmds) then
    FBeforeRecordCmds.Clone(Result.FBeforeRecordCmds);
  if Assigned(FAfterRecordCmds) then
    FAfterRecordCmds.Clone(Result.FAfterRecordCmds);
  if Assigned(FRecodeCmds) then
    FRecodeCmds.Clone(Result.FRecodeCmds);

  // Other
  for i := 0 to MaxDefinedMissingValues do
    Result.GlobalMissingVal[i] := GlobalMissingVal[i];

  for i := 0 to FDefines.Count - 1 do
    Result.FDefines.Add(FDefines[i].Clone());

  for i := 0 to FAutoFields.Count - 1 do
    Result.FAutoFields.Add(FAutoFields[i]);
end;

function TEpiDataFileProperties.DefineExists(const aName: string): Boolean;
begin
  result := FDefines.FieldExists(aName);
end;

function TEpiDataFileProperties.DefineByName(const aName: string): TEpiField;
begin
  result := FDefines.FieldByName(aName);
end;

procedure TEpiDataFileProperties.AddDefine(Field: TEpiField);
begin
  FDefines.Add(Field);
end;

function TEpiDataFileProperties.GetGlobMissing(Index: Integer): string;
begin
  Result := FGlobalMissingValues[Index];
end;

procedure TEpiDataFileProperties.SetGlobMissing(Index: Integer;
  const Value: string);
begin
  FGlobalMissingValues[Index] := Value;
end;

procedure TEpiDataFileProperties.InternalReset;
begin
  if Assigned(FBeforeFileCmds)   then FreeAndNil(FBeforeFileCmds);
  if Assigned(FAfterFileCmds)    then FreeAndNil(FAfterFileCmds);
  if Assigned(FBeforeRecordCmds) then FreeAndNil(FBeforeRecordCmds);
  if Assigned(FAfterRecordCmds)  then FreeAndNil(FAfterRecordCmds);
  if Assigned(FRecodeCmds)       then FreeAndNil(FRecodeCmds);
  if Assigned(FDefines)          then FreeAndNil(FDefines);
  if Assigned(FAutoFields)       then FreeAndNil(FAutoFields);

  FMissingAction       := maIgnoreMissing;
  FShowLastRecord      := false;
  FErrorInFile         := false;
  FHasCheckFile        := false;
  FFileName            := '';
  FHasTypeStatusBar    := false;
  FTypeStatusBarColor  := EpiColourBase;
  FTypeStatusBarText   := '';
  FTypeStatusBarField  := nil;
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

{ TEpiDataFile }

function TEpiDataFile.GetField(Index: Integer): TEpiField;
begin
  result := Fields[Index];
end;

function TEpiDataFile.GetDeleted(Index: integer): boolean;
begin
  result := FRecordStatus.AsInteger[Index] = Ord(rsDeleted);
end;

function TEpiDataFile.GetIndexFile: TEpiIndexFile;
begin
  if not Assigned(FIndexFile) then
    FIndexFile := TEpiIndexFile.Create(ChangeFileExt(FileName, '.EIX'));
  result := FIndexFile;
end;

function TEpiDataFile.GetSize: Integer;
begin
  result := FRecordStatus.Size;
end;

function TEpiDataFile.GetTextLabel(Index: integer): TEpiTextLabel;
begin
  result := TextLabels[Index];
end;

function TEpiDataFile.GetTextLabelCount: Cardinal;
begin
  result := TextLabels.Count;
end;

function TEpiDataFile.GetVerified(Index: integer): boolean;
begin
  result := FRecordStatus.AsInteger[index] = Ord(rsVerified);
end;

procedure TEpiDataFile.InternalReset;
begin
  if Assigned(FFields) then FreeAndNil(FFields);
  if Assigned(FTextLabels) then FreeAndNil(FTextLabels);
  if Assigned(FScreenProperties) then FreeAndNil(FScreenProperties);
  if Assigned(FCheckFile) then FreeAndNil(FCheckFile);
  if Assigned(FIndexFile) then FreeAndNil(FIndexFile);
  if Assigned(FCrypter) then FreeAndNil(FCrypter);
  if Assigned(FRecordStatus) then FreeAndNil(FRecordStatus);
  if Assigned(FValueLabels) then FreeAndNil(FValueLabels);

  FFileName       := '';
  FFileLabel      := '';
  FPassword       := '';
  FFieldNaming    := fnAuto;
  FOptions        := [];
  FOnProgress     := nil;
  FOnTranslate    := nil;
  FOnPassword     := nil;
  FErrorText      := '';
  FErrorCode      := 0;
  FFileVersion    := 0;
  FDatafileType   := dftNone;
end;

procedure TEpiDataFile.SetDeleted(Index: integer; const AValue: boolean);
begin
  if AValue then
    FRecordStatus.AsInteger[Index] := ord(rsDeleted)
  else
    FRecordStatus.AsInteger[Index] := ord(rsNormal);
end;

procedure TEpiDataFile.SetFileLabel(const AValue: string);
var
  OldVal: String;
begin
  if AValue = FileLabel then exit;
  OldVal := FileLabel;
  FFileLabel := AValue;
  DoChange(dceLabel, @OldVal);
end;

procedure TEpiDataFile.SetFileName(const AValue: string);
var
  OldVal: String;
begin
  if AValue = FileName then exit;
  OldVal := FileName;
  FFileName := AValue;
  DoChange(dceName, @OldVal);
end;

procedure TEpiDataFile.SetSize(const AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to FieldCount - 1 do
    Fields[i].Size := AValue;

  FRecordStatus.Size := AValue;
end;

procedure TEpiDataFile.SetStudy(const AValue: string);
var
  OldVal: String;
begin
  if AValue = Study then exit;
  OldVal := Study;
  FStudy := AValue;
  DoChange(dceStudy, @OldVal);
end;

procedure TEpiDataFile.SetVerified(Index: integer; const AValue: boolean);
begin
  if AValue then
    FRecordStatus.AsInteger[Index] := ord(rsVerified)
  else
    FRecordStatus.AsInteger[Index] := ord(rsNormal);
end;

procedure TEpiDataFile.ReportXmlError(ErrCode: Integer; LangCode: integer; Msg: String; Args: array of const);
begin
  ErrorCode := ErrCode;
  ErrorText := Format(Lang(LangCode, Msg), Args);
  EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, LangCode);
  Abort;
end;

procedure TEpiDataFile.ReadSettings(RootNode: TDOMElement;
  FmtSettings: TFormatSettings);
var
  ElemNode: TDOMElement;
begin
  // Version: so far we only got ver. 0
  ElemNode := TDOMElement(RootNode.FindNode('Version'));
  if not Assigned(ElemNode) then
    ReportXmlError(EPI_FILE_VERSION_ERROR, 0,
      'No format version specified for file: %s', [FileName]);

  // Date Separator
  ElemNode := TDOMElement(RootNode.FindNode('DateSeparator'));
  if Assigned(ElemNode) then
    FmtSettings.DateSeparator := UTF8Encode(ElemNode.TextContent)[1]
  else
    FmtSettings.DateSeparator := EpiInternalFormatSettings.DateSeparator;

  // Decimal Separator
  ElemNode := TDOMElement(RootNode.FindNode('DecimalSeparator'));
  if Assigned(ElemNode) then
    FmtSettings.DecimalSeparator := UTF8Encode(ElemNode.TextContent)[1]
  else
    FmtSettings.DecimalSeparator := EpiInternalFormatSettings.DecimalSeparator;

  // Missing mark
  ElemNode := TDOMElement(RootNode.FindNode('MissingMark'));
  if Assigned(ElemNode) then
    FmtSettings.TimeAMString := UTF8Encode(ElemNode.TextContent)
  else
    FmtSettings.TimeAMString := TEpiStringField.DefaultMissing;

  // Password
  ElemNode := TDOMElement(RootNode.FindNode('Password'));
  if Assigned(ElemNode) and (not RequestPassword(UTF8Encode(ElemNode.TextContent))) then
    ReportXmlError(EPI_INVALID_PASSWORD, 9020,
      'Incorrect password entered', []);
end;

procedure TEpiDataFile.ReadMetaData(RootNode: TDOMElement;
  FmtSettings: TFormatSettings);
var
  ElemNode: TDOMElement;
  SubSectionNode: TDOMNode;
begin
  // Filelabel
  ElemNode := TDOMElement(RootNode.FindNode('FileLabel'));
  if Assigned(ElemNode) then
    FileLabel := UTF8Encode(ElemNode.TextContent);

  // Study
  ElemNode := TDOMElement(RootNode.FindNode('Study'));
  if Assigned(ElemNode) then
    Study := UTF8Encode(ElemNode.TextContent);

  ElemNode := TDOMElement(RootNode.FindNode('ValueLabels'));
  if Assigned(SubSectionNode) then
    ReadValueLabels(ElemNode, FmtSettings);

  // TODO : User section
end;

procedure TEpiDataFile.ReadValueLabels(RootNode: TDOMElement;
  LocalFmt: TFormatSettings);
var
  ElemNode: TDOMElement;
  SubElem: TDOMNode;
  LocalValueLabel: TValueLabelSet;
  TmpStr: String;
begin
  // ElemNode = "ValueLabel"
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'ValueLabel' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in ValueLabels section: %s', [UTF8Encode(ElemNode.NodeName)]);

    SubElem := ElemNode.FindNode('Type');
    if not Assigned(SubElem) then
    begin
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Valuelabel Type not specified.', []);
      Exit;
    end;
    LocalValueLabel := TValueLabelSet.Create(XmlNameToFieldType(UTF8Encode(SubElem.TextContent)));

    TmpStr := UTF8Encode(ElemNode.GetAttribute('Id'));
    if TmpStr = '' then
    begin
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Valuelabel Id not specified.', []);
      Exit;
    end;
    LocalValueLabel.Id := UTF8Encode(TmpStr);

    SubElem := ElemNode.FindNode('Name');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Valuelabel name not specified.', []);
    LocalValueLabel.Name := TmpStr;

    SubElem := ElemNode.FindNode('External');
    if Assigned(SubElem) then
      ReadExternalValueLabels(TDOMElement(SubElem), LocalValueLabel);

    SubElem := ElemNode.FindNode('Internal');
    if Assigned(SubElem) then
      ReadInternalValueLabels(TDOMElement(SubElem), LocalValueLabel, LocalFmt);

    if LocalValueLabel.Count = 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Internal or External section missing from ValueLabel tag.', []);

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadExternalValueLabels(RootNode: TDOMElement;
  ValueLabelSet: TValueLabelSet);
var
  LocalDf: TEpiDataFile;
  Importer: TEpiImportExport;
  ValueField,
  LabelField: TEpiField;
  Idx: Integer;
  ElemNode: TDOMNode;
  TmpStr: String;
  i: Integer;
begin
  // EXTERNAL Value labels...
  // ------------------------
  ElemNode := RootNode.FindNode('File');
  if not Assigned(ElemNode) then
    ReportXmlError(EPI_XML_TAG_MISSING, 0,
      'External Valuelabel: File TAG not specified.', []);

  Importer := TEpiImportExport.Create;
  Importer.OnPassword := OnPassword;
  Importer.OnProgress := OnProgress;
  Importer.OnTranslate := OnTranslate;
  TmpStr := UTF8Encode(ElemNode.TextContent);
  if not Importer.Import(TmpStr, LocalDf, dftNone) then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s could not be opened', [TmpStr]);

  if LocalDf.FieldCount < 2 then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s must have at least 2 fields', [TmpStr]);

  if LocalDf.Size = 0 then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s does not contain any records', [TmpStr]);

  Idx := -1;
  ElemNode := RootNode.FindNode('ValueField');
  if Assigned(ElemNode) then
    ValueField := LocalDf.FieldByName(UTF8Decode(ElemNode.TextContent))
  else
    ValueField := LocalDf[++Idx];

  ElemNode := RootNode.FindNode('LabelField');
  if Assigned(ElemNode) then
    LabelField := LocalDf.FieldByName(UTF8Decode(ElemNode.TextContent))
  else
    LabelField := LocalDf[++Idx];

  // Check for same field.
  if (LabelField = ValueField) then
    // Case where the two tags are the same.
    if Idx = -1 then
      ReportXmlError(EPI_XML_ERROR, 0,
        'ValueField and LabelField must be different', [])
    else
    // Case where ValueField is defined, but not LabelField.
      LabelField := LocalDf[++Idx];

  for i := 1 to LocalDf.Size do
    ValueLabelSet.AddValueLabelPair(ValueField.AsValue[i], LabelField.AsString[i]);

  FreeAndNil(LocalDf);
  Importer.Free;
end;

procedure TEpiDataFile.ReadInternalValueLabels(RootNode: TDOMElement;
  ValueLabelSet: TValueLabelSet; LocalFmt: TFormatSettings);
var
  ElemNode: TDOMElement;
  Val: Variant;
  TmpStr: String;
begin
  // INTERNAL Value labels...
  // ------------------------
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'Set' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in Internal section: %s', [UTF8Encode(ElemNode.NodeName)]);

    TmpStr := UTF8Encode(ElemNode.GetAttribute('value'));
    if TmpStr = '' then
      ReportXmlError(EPI_XML_ATTR_MISSING, 0,
        'Attribute "value" is missing in valuelabel %s', [ValueLabelSet.Name]);

    Case ValueLabelSet.LabelType of
      ftInteger: Val := StrToInt(TmpStr);
      ftFloat:   Val := StrToFloat(TmpStr, LocalFmt);
      ftString,
      ftBoolean: Val := TmpStr;
    end;
    TmpStr := UTF8Encode(ElemNode.GetAttribute('label'));
    ValueLabelSet.AddValueLabelPair(Val, TmpStr, UTF8Encode(ElemNode.GetAttribute('missing')) = '1');

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadScreen(RootNode: TDOMElement);
var
  ElemNode: TDOMNode;
begin
  ElemNode := RootNode.FindNode('Form');
  if Assigned(ElemNode) then
  begin
    // TODO : Form Colours
  end;

  ElemNode := RootNode.FindNode('Colours');
  if Assigned(ElemNode) then
    ReadScreenColours(TDOMElement(ElemNode));
end;

procedure TEpiDataFile.ReadScreenColours(RootNode: TDOMElement);
var
  ElemNode: TDOMElement;
  SubElem: TDOMNode;
  LocalScreenProp: TEpiScreenProperty;
  TmpStr: String;
begin
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'Colour' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in Colours section: %s', [UTF8Encode(ElemNode.NodeName)]);
    LocalScreenProp := TEpiScreenProperty.Create(ScreenProperties);

    TmpStr := UTF8Encode(ElemNode.GetAttribute('id'));
    if TmpStr = '' then
      ReportXmlError(EPI_XML_ATTR_MISSING, 0,
        'Colour attribute Id not specified.', []);
    LocalScreenProp.Id := UTF8Encode(TmpStr);

    SubElem := ElemNode.FindNode('Name');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour Name tag not specified.', []);
    LocalScreenProp.Name := UTF8Encode(SubElem.TextContent);

    SubElem := ElemNode.FindNode('ForeGround');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour ForeGround tag not specified.', []);
    LocalScreenProp.FgColour := Hex2Dec(SubElem.TextContent);

    SubElem := ElemNode.FindNode('BackGround');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour BackGround tag not specified.', []);
    LocalScreenProp.BgColour := Hex2Dec(SubElem.TextContent);

    SubElem := ElemNode.FindNode('HighLight');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour HighLight tag not specified.', []);
    LocalScreenProp.HlColour := Hex2Dec(SubElem.TextContent);

    ScreenProperties.Add(LocalScreenProp);

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadTextLabels(RootNode: TDOMElement);
var
  ElemNode: TDOMElement;
  LocalTextLabel: TEpiTextLabel;
  TmpStr: String;
  SubElem: TDOMNode;
  LocalScreenProp: TEpiScreenProperty;
begin
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'TextLabel' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in TextLabels section: %s', [UTF8Encode(ElemNode.NodeName)]);
    LocalTextLabel := TEpiTextLabel.Create(TextLabels);

    TmpStr := UTF8Encode(ElemNode.GetAttribute('id'));
    if TmpStr = '' then
      ReportXmlError(EPI_XML_ATTR_MISSING, 0,
        'TextLabel attribute Id not specified.', []);
    LocalTextLabel.Id := UTF8Encode(TmpStr);

    SubElem := ElemNode.FindNode('ScreenRef');
    if Assigned(SubElem) then
    begin
      if not ScreenProperties.ScreenPropertyExists(UTF8Decode(SubElem.TextContent), LocalScreenProp) then
        ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
          'Textlabel ScreenRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
    end else
      LocalScreenProp := ScreenProperties.DefaultScreenProperty;
    LocalTextLabel.ScreenProp := LocalScreenProp;

    SubElem := ElemNode.FindNode('Text');
    if Assigned(SubElem) then
      LocalTextLabel.Text := UTF8Decode(SubElem.TextContent);

    TextLabels.Add(LocalTextLabel);

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadFields(RootNode: TDOMElement);
var
  List: TStringList;
  ElemNode: TDOMElement;
  SubElem: TDOMElement;
  TmpField: TEpiField;
  LocalScreenProp: TEpiScreenProperty;
  LocalValueLabel: TValueLabelSet;
  SubSection: TDOMNode;
begin
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'Field' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in Fields section: %s', [UTF8Encode(ElemNode.NodeName)]);

    // *****************
    // Must exists tags:
    // *****************
    SubElem := TDOMElement(ElemNode.FindNode('Type'));
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Field Type not specified.', []);
    TmpField := TEpiField.CreateField(XmlNameToFieldType(UTF8Encode(SubElem.TextContent)), 0);

    with TmpField do
    begin
      Id            := UTF8Encode(ElemNode.GetAttribute('id'));
      FieldName     := UTF8Encode(ElemNode.FindNode('Name').TextContent);
      FieldLength   := StrToInt(ElemNode.FindNode('Length').TextContent);
      FieldDecimals := StrToInt(ElemNode.FindNode('Decimals').TextContent);

      SubElem := TDOMElement(ElemNode.FindNode('ScreenRef'));
      if Assigned(SubElem) then
      begin
        if not ScreenProperties.ScreenPropertyExists(UTF8Decode(SubElem.TextContent), LocalScreenProp) then
          ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
            'Field ScreenRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
      end else
        LocalScreenProp := ScreenProperties.DefaultScreenProperty;
      ScreenProps := LocalScreenProp;

      // *****************
      // Optional:
      // *****************
      // - Variable label
      SubSection := ElemNode.FindNode('FieldLabel');
      if Assigned(SubSection) then
      begin
        SubElem := TDOMElement(SubSection.FindNode('ScreenRef'));
        if Assigned(SubElem) then
        begin
          if not ScreenProperties.ScreenPropertyExists(UTF8Decode(SubElem.TextContent), LocalScreenProp) then
            ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
              'FieldLabel ScreenRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
        end else
          LocalScreenProp := ScreenProperties.DefaultScreenProperty;
        VarLabelScreenProps := LocalScreenProp;

        SubElem := TDOMElement(ElemNode.FindNode('Text'));
        if Assigned(SubElem) then
          VariableLabel := UTF8Decode(SubElem.TextContent);
      end;

      // - Valuelabel
      SubElem :=  TDOMElement(ElemNode.FindNode('ValueLabelRef'));
      if Assigned(SubElem) then
      begin
        if not ValueLabels.ValueLabelSetExits(UTF8Encode(SubElem.TextContent), LocalValueLabel) then
          ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
            'Field ValueLabelRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
      end else
        LocalValueLabel := nil;
      ValueLabelSet := LocalValueLabel;

      // - Default value
      SubElem :=  TDOMElement(ElemNode.FindNode('DefaultValue'));
      if Assigned(SubElem) then
        DefaultValue := UTF8Encode(SubElem.TextContent);

      // Optional - requires FieldProperties.
      // - Confirm
      SubElem :=  TDOMElement(ElemNode.FindNode('Confirm'));
      if Assigned(SubElem) then
        FieldProperties.Confirm := true;
      // - Repeat
      SubElem :=  TDOMElement(ElemNode.FindNode('Repeat'));
      if Assigned(SubElem) then
        FieldProperties.DoRepeat := true;
      // - Enter
      SubElem :=  TDOMElement(ElemNode.FindNode('Enter'));
      if Assigned(SubElem) then
      begin
        if UTF8Encode(WideLowerCase(SubElem.TextContent)) = 'true' then
          FieldProperties.EntryType := entMust
        else
          FieldProperties.EntryType := entNone;
      end;
      // - Jumps
      SubSection := ElemNode.FindNode('Jump');
      if Assigned(SubSection) then
      begin
        SubElem := TDOMElement(SubSection.FirstChild);
        while Assigned(SubElem) do
        begin
          FieldProperties.Jumps.AddObject(UTF8Encode(SubElem.GetAttribute('on')),
            TString.Create(UTF8Encode(SubElem.GetAttribute('to'))));
          SubElem := TDOMElement(SubElem.NextSibling);
        end;
        if SubSection.Attributes.Length > 0 then
          FieldProperties.JumpResetValue := UTF8Encode(SubSection.Attributes[0].TextContent);
      end;
      // - Range
      SubElem := TDOMElement(ElemNode.FindNode('Range'));
      if Assigned(SubElem) then
      begin
        List := nil;
        SplitString(UTF8Encode(SubElem.TextContent), List, [',']);
        FieldProperties.Ranges.Assign(List);
      end;
      // - Top of Screen (reposition to top of screen)
      SubElem := TDOMElement(ElemNode.FindNode('TopOfScreen'));
      if Assigned(SubElem) then
        FieldProperties.TopOfScreen := StrToInt(SubElem.TextContent);
      // - TypeComment (place a label next to edit field)
      SubElem := TDOMElement(ElemNode.FindNode('TypeComment'));
      if Assigned(SubElem) then
      begin
        if SubElem.GetAttribute('colour') <> '' then
        begin
          FieldProperties.TypeType := ttComment;
          FieldProperties.TypeColour := Hex2Dec(SubElem.GetAttribute('colour'));
        end else
        if SubElem.GetAttribute('field') <> '' then
        begin
          FieldProperties.TypeType := ttField;
          FieldProperties.TypeField := FieldByName(UTF8Encode(SubElem.GetAttribute('colour')));
        end;
      end;
    end;

    AddField(TmpField);
    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadRecords(RootNode: TDOMElement;
  FmtSettings: TFormatSettings);
var
  ElemNode: TDOMElement;
  CurRec: Integer;
  i: Integer;
  TmpField: TEpiField;
  TmpStr: String;
begin
  CurRec := 1;
  Size := RootNode.ChildNodes.Count;
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    for i := 0 to ElemNode.Attributes.Length - 1 do
    begin
      if ElemNode.Attributes[i].NodeName = 'st' then
      begin
        if ElemNode.Attributes[i].NodeValue = '1' then
          Deleted[CurRec] := true
        else if ElemNode.Attributes[i].NodeValue = '2' then
          Verified[CurRec] := true;
      end else begin
        TmpField := FieldById(UTF8Decode(ElemNode.Attributes[i].NodeName));
        TmpStr := UTF8Encode(ElemNode.Attributes[i].NodeValue);
        if TmpField.FieldType = ftCrypt then
        begin
          TmpStr := B64Decode(TmpStr);
          FCrypter.DecryptCFB(TmpStr[1], TmpStr[1], Length(TmpStr));
          TmpStr := Trim(TmpStr);
          FCrypter.Reset;
        end;
        TmpField.AsString[CurRec] := TmpStr;;
      end;
    end;
    inc(CurRec);
    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

function TEpiDataFile.Ins(Lvl: integer): string;
begin
  result := DupeString(' ', Lvl * 2);
end;

function TEpiDataFile.StringToXml(Const Src: String): string;
var
  i: Integer;
begin
  for i := 1 to Length(Src) do
  begin
    case Src[i] of
      '&':
        Result := Result  + '&amp;';
      '"':
        Result := Result  + '&quot;';
      '<':
        Result := Result  + '&lt;';
      '>':
        Result := Result  + '&gt;';
    else
      Result := Result  + Src[i];
    end;
  end;
end;

procedure TEpiDataFile.WriteSettings(St: TStream);
var
  TmpStr: String;
  EncData: String;

  function RequirePassword: boolean;
  var
    i: Integer;
  begin
    result := true;
    for i := 0 to FieldCount -1 do
      if Fields[i].FieldType = ftCrypt then
        Exit;
    result := false;
  end;

begin
  TmpStr :=
    Ins(1) + '<Settings>' + LineEnding +
    Ins(2) + '<Version>' + IntToStr(FileVersion) + '</Version>' + LineEnding +
    Ins(2) + '<DateSeparator>' + EpiInternalFormatSettings.DateSeparator + '</DateSeparator>' + LineEnding +
    Ins(2) + '<DecimalSeparator>' + EpiInternalFormatSettings.DecimalSeparator + '</DecimalSeparator>' + LineEnding +
    Ins(2) + '<MissingMark>' +  StringToXml(TEpiStringField.DefaultMissing) + '</MissingMark>' + LineEnding;
  if RequirePassword then
  begin
    // TODO : What about UTF-8 encoding??
    if Assigned(OnPassword) then OnPassword(self, rpCreate, FPassWord);
    if Password = '' then
      raise Exception.Create('A password is needed for data files with encrypted fields');
    FCrypter.InitStr(Password);
    EncData := Trim(Password);
    FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
    EncData := B64Encode(EncData);
    FCrypter.Reset;
    TmpStr := TmpStr +
      Ins(2) + '<Password>' + EncData + '</Password>' + LineEnding;
  end;
  TmpStr := TmpStr +
    Ins(1) + '</Settings>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

procedure TEpiDataFile.WriteMetaData(St: TStream);
var
  TmpStr: String;
begin
  TmpStr :=
    Ins(1) + '<MetaData>' + LineEnding;
  if FileLabel <> '' then
    TmpStr := TmpStr +
      Ins(2) + '<FileLabel>' + StringToXml(FileLabel) + '</FileLabel>' + LineEnding;
  if Study <> '' then
    TmpStr := TmpStr +
      Ins(2) + '<Study>' + StringToXml(Study) + '</Study>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));

  if ValueLabels.Count > 0 then
    WriteValueLabels(St);

  // TODO : <USERDEFINED>
  TmpStr :=
    Ins(1) + '</MetaData>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

procedure TEpiDataFile.WriteValueLabels(St: TStream);
var
  TmpStr: String;
  i: Integer;
  j: Integer;
begin
  TmpStr := Ins(2) + '<ValueLabels>' + LineEnding;
  for i := 0 to ValueLabels.Count - 1 do
  with ValueLabels[i] do
  begin
    TmpStr := TmpStr +
      Ins(3) + '<ValueLabel Id="' + StringToXml(Id) + '">' + LineEnding;
    TmpStr := TmpStr +
      Ins(4) + '<Type>' + FieldTypeXmlNames[LabelType] + '</Type>' + LineEnding;
    case LabelScope of
      vlsFile:
        begin
          TmpStr := TmpStr +
            Ins(4) + '<External>' + LineEnding +
            Ins(5) + '<File>' + StringToXml(ExtName) + '</File>' + LineEnding +
            //Ins(5) + '<ValueField>' + StringToXml() + '</ValueField>' + LineEnding +
            //Ins(5) + '<LabelField>' + StringToXml(ExtName) + '</LabelField>' + LineEnding +
            Ins(4) + '</External>' + LineEnding;
        end;
      vlsGlobal,
      vlsLocal:
        Begin
          TmpStr := TmpStr +
            Ins(4) + '<Internal>' + LineEnding;
          for j := 0 to Count - 1 do
          begin
            TmpStr := TmpStr  +
              Ins(5) + '<Set value="' + StringToXml(String(Values[j])) + '"';
            if Labels[j] <> '' then
              TmpStr := TmpStr  + ' label="' + Labels[j] + '"';
            if MissingValues[j] then
              TmpStr := TmpStr  + ' missing="1"';
            TmpStr := TmpStr  +
              '/>' + LineEnding;
          end;
        end;
    end;
    TmpStr := TmpStr +
      Ins(3) + '</ValueLabel>' + LineEnding;
  end;
  TmpStr := TmpStr +
    Ins(2) + '</ValueLabels>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

function TEpiDataFile.InternalOpen: boolean;
var
  RecXml: TXMLDocument;
  RootNode: TDOMElement;
  ElemNode,
  SubElem: TDOMElement;
  PairNode: TDOMElement;
  SectionNode: TDOMNode;
  TmpStr: String;
  TmpFieldType: TFieldType;
  TmpField: TEpiField;
  CurRec: Integer;
  i: Integer;
  Idx: LongInt;
  LocalFmt: TFormatSettings;
  MissingStr: String;
  SubSectionNode: TDOMNode;
  LocalValueLabel: TValueLabelSet;
  LocalDf: TEpiDataFile;
  ValueField: TEpiField;
  TextField: TEpiField;
  Val: Variant;
  List: TStrings;
  TagList: TStringList;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'InternalOpen', 2, 'Filename = ' + Filename);
  result := false;

  DatafileType := dftEpiDataXml;

  try
    ReadXMLFile(RecXml, FileName);

    // **********************
    // Global <EpiData> structure
    // **********************
    RootNode := RecXml.DocumentElement;

    // **********************
    // <Settings> Section
    // **********************
    SectionNode := RootNode.FindNode('Settings');
    if not Assigned(SectionNode) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Settings section missing in file: %s', [FileName]);
    ReadSettings(TDOMElement(SectionNode), LocalFmt);

    // **********************
    // <MetaData> Section
    // **********************
    SectionNode := RootNode.FindNode('MetaData');
    if Assigned(SectionNode) then
      ReadMetaData(TDOMElement(SectionNode), LocalFmt);

    // **********************
    // <Screen> Section
    // **********************
    SectionNode := RootNode.FindNode('Screen');
    if Assigned(SectionNode) then
      ReadScreen(TDOMElement(SectionNode));

    // **********************
    // <TextLabels> Section
    // **********************
    SectionNode := RootNode.FindNode('TextLabels');
    if Assigned(SectionNode) then
      ReadTextLabels(TDOMElement(SectionNode));

    // **********************
    // <Fields> Section
    // **********************
    SectionNode := RootNode.FindNode('Fields');
    if Assigned(SectionNode) then
      ReadFields(TDOMElement(SectionNode));

    // **********************
    // <RECORDS> Section
    // **********************
    SectionNode := RootNode.FindNode('Records');
    if Assigned(SectionNode) then
      ReadRecords(TDOMElement(SectionNode), LocalFmt);

  finally
    EpiLogger.DecIndent;
    if Assigned(RecXml) then FreeAndNil(RecXml);
  end;
end;

function TEpiDataFile.InternalOpenOld: boolean;
var
  // Misc:
  TempInt, I, TotFieldLength: integer;
  TxtFile: TextFile;
  EField: TEpiField;
  FieldNumberCounter: cardinal;
  ChkIO: TCheckFileIO;
  CharBuf: Array of char;

  // Reading the textfile:
  TxtLine: string;
  HeaderLineCount: Integer;
  ValCode: Integer;
  CurrentLine: Integer;

  // Field lines:
  TmpFieldType: TFieldType;
  TmpFieldChar, Dummy: Char;
  TmpFieldTypeInt,
  TmpFieldColor, TmpQuestX, TmpQuestY, TmpLength,
  TmpFieldX, TmpFieldY, TmpQuestColor: Integer;
  TmpName: string[10];
  TmpLabel, TmpStr: string;
  CurRec: Integer;
  StrBuf: String;
  DataStream: TMemoryStream;
  BufPos: Integer;
  EncData: String;
  Stop: Boolean;
  ELabel: TEpiTextLabel;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'InternalOpenOld', 2, 'Filename = ' + Filename);
  result := false;

  DatafileType := dftEpiDataRec;

  try
    AssignFile(TxtFile, UTF8ToSys(Filename));
    {$I-}
    System.Reset(TxtFile);
    {$I+}
    if IOResult() > 0 then
    begin
      ErrorText := Format(Lang(20108,'Data file %s could not be opened.'),[Filename]) + #13 +
                           Lang(20208,'Please check if the file is in use and that the file name is legal.');
      ErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 20108);
      Exit;
    end;
    // --- Read "First Line" header ---
    ReadLn(TxtFile, TxtLine);

    // - Password
    TempInt := Pos('~KQ:', AnsiUpperCase(TxtLine));
    if TempInt > 0 then
    begin
      if not RequestPassword(Copy(TxtLine, TempInt + 4, Pos(':KQ~', AnsiUpperCase(TxtLine)) - (TempInt + 4))) then
      begin
        ErrorText := Lang(9020, 'Incorrect password entered');
        Errorcode := EPI_INVALID_PASSWORD;
        EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 9020);
        CloseFile(TxtFile);
        Exit;
      end;
    end;

    // - FileLabel
    if Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) > 0 then
      FileLabel :=  EpiUnknownStrToUTF8(Copy(TxtLine, Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) + Length('FILELABEL: ') , Length(TxtLine)));

    // - Autonaming or Firstword
    if Pos(' VLAB', TxtLine) > 0 then
      FieldNaming := fnFirstWord
    else
      FieldNaming := fnAuto;

    // - Header lines:
    Val(Copy(TxtLine, 1, Pos(' ', TxtLine)-1), HeaderLineCount, ValCode);
    if ValCode > 0 then
    begin
      ErrorText := Format(Lang(20112, 'Incorrect format of datafile %s'), [Filename]);
      ErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 20112);
      CloseFile(TxtFile);
      Exit;
    end;

    FieldNumberCounter := 1;
    TotFieldLength := 0;
    // Read field defining header lines.
    for CurrentLine := 1 to HeaderLineCount do
    begin
      EpiLogger.Add(ClassName, 'InternalOpenOld', 3, 'Reading headerline no: ' + IntToStr(CurrentLine));
      if UpdateProgress((CurrentLine*100) DIV HeaderLineCount, lang(0,'Opening data file')) = prCancel then
      begin
        ErrorText := Lang(0, 'Cancelled by user');
        Errorcode := EPI_USERCANCELLED;
        EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 0);
        CloseFile(TxtFile);
        Exit;
      end;

      ReadLn(TxtFile,
             TmpFieldChar, TmpName, TmpQuestX, TmpQuestY,
             TmpQuestColor, TmpFieldX, TmpFieldY, TmpFieldTypeInt, TmpLength,
             TmpFieldColor, dummy, TmpLabel);

      // Field types.
      if TmpFieldTypeInt >= 100 then
        // Type > 100 => float field
        TmpFieldType := ftFloat
      else begin
        // Normal field type recognition.
        TmpFieldType := ftInteger;
        WHILE TmpFieldTypeInt > ORD(TmpFieldType) DO
          TmpFieldType := Succ(TmpFieldType);
      end;

      // This is not a data field, but a question field.
      if TmpLength = 0 then TmpFieldType := ftQuestion;

      // Unsupported field are automatically converted to string (ftString) fields.
      if (not (TmpFieldType in SupportedFieldTypes)) or
         ((TmpFieldType in DateFieldTypes) and (TmpLength < 10)) then
        TmpFieldType := ftString;

      // Trim text information.
      TmpName := Trim(TmpName);
      TmpLabel := Trim(TmpLabel);

      if TmpFieldType = ftQuestion then
      begin
        ELabel := TEpiTextLabel.Create(Self.TextLabels);
        with ELabel do
        begin
          Id := TmpName;
          Text := TmpLabel;
          ScreenProp := ScreenProperties.DefaultScreenProperty;
          TextLeft   := TmpFieldX;
          TextTop    := TmpFieldY;
        end;
        AddTextLabel(ELabel);
        Continue;
      end;

      EField := TEpiField.CreateField(TmpFieldType);
      with EField do
      begin
        ScreenProps         := ScreenProperties.DefaultScreenProperty;
        FieldLeft           := TmpFieldX;
        FieldTop            := TmpFieldY;
        VarLabelScreenProps := ScreenProperties.DefaultScreenProperty;
        VarLabelLeft        := TmpQuestX;
        VarLabelTop         := TmpQuestY;

        FieldLength := TmpLength;
        FieldDecimals := 0;
        if TmpFieldTypeInt >= 100 then
          FieldDecimals := TmpFieldTypeInt - 100;
        VariableLabel := EpiUnknownStrToUTF8(StringReplace(TmpLabel, '_', '-', [rfReplaceAll]));

        // In old style .REC files, first word in label is the name of the field. Remove it.
        if Pos(TmpName, VariableLabel) > 0 then
          VariableLabel := Trim(Copy(VariableLabel, Length(TmpName)+1, Length(VariableLabel)));
        // Ensure valid variable name.
        FieldName := Trim(CreateUniqueFieldName(TmpName));
        // If the field name was invalid (not very likely) use it in variable label.
        IF FieldName <> TmpName THEN
          VariableLabel := TmpName + ' ' + VariableLabel;

        // Summerize field findings.
        TotFieldLength := TotFieldLength + FieldLength;
      end;  // With EField
      AddField(EField);
    end; // For CurrentLine

    // Position for reading and check for corruptness.
    TotFieldLength := TotFieldLength + (((TotFieldLength - 1) DIV MaxRecLineLength) + 1) * 3;
    TmpLength := TextPos(TxtFile);
    CloseFile(TxtFile);

    DataStream := TMemoryStream.Create;
    DataStream.LoadFromFile(UTF8ToSys(Filename));
    DataStream.Position := DataStream.Size;

    // Skip all lineendings / EOF chars.
    SetLength(CharBuf, 16);
    Stop := false;
    while DataStream.Position >= TmpLength do
    begin
      DataStream.Seek(-16, soCurrent);
      DataStream.Read(CharBuf[0], 16);

      i := 15;
      while i >= 0 do
      begin
        if (CharBuf[i] in ['!', '?', '^']) then
        begin
          Stop := true;
          break;
        end;
        Dec(i);
      end;
      if Stop then break;
      DataStream.Seek(-16, soCurrent);
    end;

    if DataStream.Position < TmpLength then
      TempInt := TmpLength  // This is an empty datafile!
    else
      TempInt := DataStream.Position - (16 - i) + 3; // + 3 is for "!#13#10" which all .REC file should end with??!?!?
    if ((TempInt - TmpLength) mod TotFieldLength) <> 0 then
    begin
      ErrorText := Format(Lang(20118, 'Error in datafile %s. One or more records are corrupted. Size: %d, Offset: %d, TotalLength: %d, i: %d'),
        [Filename, DataStream.Size, TmpLength, TotFieldLength, i]);
      ErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 20118);
      Exit;
    end;

    TempInt := ((TempInt - TmpLength) div TotFieldLength);
    Size := TempInt;
    DataStream.Position := TmpLength;

    SetLength(CharBuf, TotFieldLength);
    For CurRec := 1 to TempInt do
    begin
      I := DataStream.Read(CharBuf[0], TotFieldLength);
      if (I <> TotFieldLength) then
      begin
        ErrorText := Lang(20464, 'Error reading record');
        ErrorCode := EPI_READ_FILE_ERROR;
        EpiLogger.AddError(Classname, 'InternalOpenOld', ErrorText, 20464);
        raise Exception.Create('Error reading record');
      end;

      StrBuf := CharBuf[High(CharBuf) - 2];
      if StrBuf = '?' then
        Deleted[CurRec] := true
      else if StrBuf = '^' then
        Verified[CurRec] := true;

      StrBuf := StringReplace(string(CharBuf), EOLChars, '', [rfReplaceAll]);
      BufPos := 1;
      for i := 0 TO FieldCount - 1 DO
      with Fields[i] do begin
        TmpStr := Trim(Copy(StrBuf, BufPos, FieldLength));
        if TmpStr = '' then
          TmpStr := TEpiStringField.DefaultMissing;
        IF (fieldtype = ftCrypt) AND (FPassword <> '') THEN
        begin
          EncData := B64Decode(TmpStr);
          FCrypter.DecryptCFB(EncData[1], EncData[1], Length(EncData));
          TmpStr := Trim(EncData);
          FCrypter.Reset;
        end;
        AsString[CurRec] := EpiUnknownStrToUTF8(TmpStr);
        Inc(BufPos, FieldLength);
      end;
    end;

    result := true;
    if not (eoIgnoreChecks in Options) then
    begin
      try
        try
          ChkIO := TCheckFileIO.Create();
          ChkIO.OnTranslate := Self.OnTranslate;
          result := ChkIO.ReadCheckFile(ChangeFileExt(FileName, '.chk'), Self);
          if not Result then
          begin
            ErrorCode := EPI_CHECKFILE_ERROR;
            for i := 0 to ChkIO.ErrorLines.Count -1 do
              ErrorText := ErrorText + #13#10 + ChkIO.ErrorLines[i];
            EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 0);
          end;
        except
          ErrorCode := EPI_CHECKFILE_ERROR;
          result := false;
        end;
      finally
        FreeAndNil(ChkIO);
      end
    end;
  finally
    if Assigned(DataStream) then FreeAndNil(DataStream);
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.InternalSave: boolean;
var
  CurField: Integer;
  CurRec: Integer;
  TmpStr: String;
  DataStream: TFileStream;
  EncData: String;
  j: Integer;
  i: Integer;
  IdNo: Integer;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'InternalSave', 3);
  result := false;
  DataStream := nil;

  try
    DataStream := TFileStream.Create(FileName, fmCreate);
    DatafileType := dftEpiDataXml;
    UpdateProgress(0, Lang(0, 'Writing header.'));

    // **********************
    // Global <EPIDATA> structure
    // **********************
    TmpStr := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
      '<EpiData>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));


    // **********************
    // <SETTINGS> Section
    // **********************
    WriteSettings(DataStream);

    // **********************
    // <METADATA> Section
    // **********************
    WriteMetaData(DataStream);

    // **********************
    // <SCREEN> Section
    // **********************
    TmpStr := Ins(1) +
      '<Screen>' + LineEnding;
    // Form data:

    // Field


    // **********************
    // <FIELDS> Section
    // **********************
    TmpStr :=
      Ins(1) + '<Fields>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));
    IdNo := 1;
    for CurField := 0 to Fields.Count - 1 do
    with Fields[CurField] do
    begin
      TmpStr :=
        Ins(2) + '<Field Id="F' + IntToStr(IdNo) + '">' + LineEnding;
      TmpStr := TmpStr +
        Ins(3) + '<Type>' + FieldTypeXmlNames[FieldType] + '</Type>' + LineEnding;
      // Must exists tags!
      TmpStr +=
        Ins(3) + '<Name>' + StringToXml(FieldName) + '</Name>' + LineEnding +
        Ins(3) + '<Length>' + IntToStr(FieldLength) + '</Length>' + LineEnding +
        Ins(3) + '<Decimals>' + IntToStr(FieldDecimals) + '</Decimals>' + LineEnding +
        Ins(3) + '<ScreenRef>' + StringToXml(ScreenProps.Id) + '</ScreenRef>' + LineEnding +

      // Optional, but we choose to write out of courtesy.
        Ins(3) + '<FieldLabel>' + LineEnding +
        Ins(4) + '<ScreenRef>' + StringToXml(VarLabelScreenProps.Id) + '</ScreenRef>' + LineEnding +
        Ins(4) + '<Text>' + StringToXml(VariableLabel) + '</Text>' + LineEnding +
        Ins(3) + '</FieldLabel>' + LineEnding;
      Inc(IdNo);

      // Optional:
      if Assigned(ValueLabelSet) then
        TmpStr := TmpStr +
          Ins(3) + '<ValueLabelRef>' + StringToXml(ValueLabelSet.Id) + '</ValueLabelRef>' + LineEnding;
      if DefaultValue <> '' then
        TmpStr := TmpStr +
          Ins(3) + '<DefaultValue>' + StringToXml(DefaultValue) + '</DefaultValue>' + LineEnding;

      if HasFieldProperties then
      With FieldProperties Do
      begin
        if Confirm then
          TmpStr := TmpStr + Ins(3) + '<Confirm/>' + LineEnding;
        if DoRepeat then
          TmpStr := TmpStr + Ins(3) + '<Repeat/>' + LineEnding;
        Case EntryType of
          entMust: TmpStr := TmpStr + Ins(3) + '<Enter val="TRUE"/>' + LineEnding;
          entNone: TmpStr := TmpStr + Ins(3) + '<Enter val="FALSE"/>' + LineEnding;
        end;
        if Jumps.Count > 0 then
        begin
          TmpStr := TmpStr + Ins(3) + '<Jumps>' + LineEnding;
          For i := 0 to Jumps.Count - 1 do
            TmpStr := TmpStr + Ins(4) + '<Jump on="' + Jumps[i] +
                      '" to="' + StringToXml(TString(Jumps.Objects[i]).Str) + '"/>' + LineEnding;
          TmpStr := TmpStr + Ins(3) + '</Jumps>' + LineEnding;
        end;
        if Ranges.Count > 0 then
        begin
          TmpStr := TmpStr + Ins(3) + '<Range>';
          for i := 0 to Ranges.Count - 1 do
            TmpStr := TmpStr + StringToXml(Ranges[i]);
          TmpStr := TmpStr + '</Range>' + LineEnding;
        end;
        if TopOfScreen >= 0 then
          TmpStr := TmpStr +
            Ins(3) + '<TopOfScreen>' + IntToStr(TopOfScreen) + '</TopOfScreen>' + LineEnding;
        Case TypeType of
          ttComment: TmpStr := TmpStr +
            Ins(3) + '<TypeComment colour="' + IntToHex(TypeColour, 6) + '"/>' + LineEnding;
          ttField: TmpStr := TmpStr +
            Ins(3) + '<TypeComment field="' + StringToXml(TypeField.FieldName) + '"/>' + LineEnding;
        end;
        // TODO : Keys!
        //      if DataFile.IndexFile.IndexExists();
      End;

      // End tag:
      TmpStr := TmpStr + Ins(2) + '</Field>' + LineEnding;
      DataStream.Write(TmpStr[1], Length(TmpStr));
    end;
    TmpStr := Ins(1) +
      '</Fields>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));

    // **********************
    // <RECORDS> Section
    // **********************
    TmpStr :=
      Ins(1) + '<Records>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));
    for CurRec := 1 to Size do
    begin
      UpdateProgress(Trunc((CurRec / Size) * 100), Lang(0, 'Writing records.'));
      TmpStr :=
        Ins(2) + '<REC';
      for CurField := 0 to FieldCount - 1 do
      with Fields[CurField] do
      begin
        if FieldType = ftQuestion then continue;
        TmpStr += ' F' + IntToStr(CurField + 1) + '="';
        Case FieldType of
          ftCrypt:
            begin
              EncData := AsString[CurRec];
              FCrypter.InitStr(Password);
              FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
              TmpStr += B64Encode(EncData);
              FCrypter.Reset;
            end;
          ftString:
            TmpStr += StringToXml(AsString[CurRec]);
        else
          TmpStr += AsString[CurRec];
        end;
        TmpStr += '"';
      end;
      if Verified[CurRec] then
        TmpStr += ' st="2"'
      else if Deleted[CurRec] then
        TmpStr += ' st="1"';
      TmpStr += '/>' + LineEnding;
      DataStream.Write(TmpStr[1], Length(TmpStr));
    end;
    TmpStr := '  </Records>' + LineEnding;
    TmpStr := TmpStr +
      '</EpiData>';
    DataStream.Write(TmpStr[1], Length(TmpStr));
    UpdateProgress(100, Lang(0, 'Complete.'));
    Result := true;
  finally
    EpiLogger.DecIndent;
    if Assigned(DataStream) then FreeAndNil(DataStream);
  end;
end;

function TEpiDataFile.InternalSaveOld: boolean;
var
  Crypt: boolean;
  i: integer;
  S, EncData: string;
  Stream: TFileStream;
  ChkIO: TCheckFileIO;
  CurRec: Integer;
  T: String;
  Z: Integer;
  TmpStr: String;
  FieldNames: TStrings;
  Fmt: TFormatSettings;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'InternalSaveOld', 3);
  result := false;

  IF Fields.Count = 0 THEN
  BEGIN
    Raise Exception.Create('No fields defined');
    Exit;
  END;

  Stream := TFileStream.Create(FileName, fmCreate);
  ChkIO := nil;

  try
    FieldNames := TStringList.Create;
    Fmt.DecimalSeparator := '.';

    // - Encryption required:
    Crypt := false;
    for i := 0 to Fields.Count -1 do
      if Fields[i].FieldType = ftCrypt then
        Crypt := true;

    IF Crypt and (Password = '') THEN
    BEGIN
      if Assigned(OnPassword) then OnPassword(self, rpCreate, FPassWord);
      if Password = '' then
        raise Exception.Create('A password is needed for data files with encrypted fields');
      FCrypter.InitStr(Password);
    END;

    // - Header lines (and colour):
    S := IntToStr(FieldCount + TextLabelCount) + ' 1 ';

    // - Autonaming or Firstword
    IF FieldNaming = fnFirstWord THEN
      S := S + 'VLAB ';

    // - Password
    IF Password <> '' THEN
    begin
      EncData := Trim(Password);
      FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
      EncData := B64Encode(EncData);
      FCrypter.Reset;
      S := S + '~KQ:' + EncData + ':KQ~ ';
    end;

    // - FileLabel
    IF Trim(FileLabel) <> '' THEN
      S := S + 'Filelabel: ' + EpiUtf8ToAnsi(FileLabel);

    S := S + #13#10;
    Stream.Write(S[1], Length(S));

    FOR i := 0 TO FieldCount - 1 DO
    WITH Fields[i] DO
    BEGIN
      EpiLogger.Add(TEpiDataFile.Classname, 'InternalSaveOld', 3, 'Writing heading no. ' + IntToStr(i+1));

      // - Fieldchar
      IF (FieldType = ftInteger) OR (FieldType = ftFloat) OR
         (FieldType = ftIDNUM) THEN
        s := '#'
      ELSE
        s := '_';

      // Since format is not UTF8 add EpiUtf8ToAnsi here else length will be f*cked
      TmpStr := CreateUniqueAnsiVariableName(FieldName, MaxFieldNameLen, FieldNames);
      s := s + Format('%-10s', [TmpStr]);     //Name of field (left justified)
      s := s + ' ';                           //Space required for some unknown reason
      s := s + Format('%4d', [VarLabelLeft]);       //Question X-position
      s := s + Format('%4d', [VarLabelTop]);       //Question Y-position
      s := s + Format('%4s', ['30']);         //Question colorcode
      s := s + Format('%4d', [FieldLeft]);       //Entry X-position
      s := s + Format('%4d', [FieldTop]);       //Entry Y-position

      //Write FieldType
      // 0 = Question without entryfield, i.e. text only
      // 100+Number of decimals = Floating point number
      // For all other: use the fieldtype-code (fieldtype)
      IF FieldType = ftQuestion THEN
        s := s + Format('%4s', ['0'])
      ELSE IF (FieldType = ftFloat) AND (FieldDecimals > 0) THEN
        s := s + Format('%4d', [100 + FieldDecimals])
      ELSE if (FieldType = ftInteger) and (FieldLength > MaxIntegerLength) then
        S := S + Format('%4d', [ORD(ftFloat)])
      ELSE
        s := s + Format('%4d', [ORD(fieldtype)]);

      //Write length of field - use 0 for text only
      IF FieldType = ftQuestion THEN
        s := s + Format('%4s', ['0'])
      ELSE BEGIN
        s := s + Format('%4d', [FieldLength]);
      END;

      //write entry colorcode - special use in encrypted fields (holds entrylength of field)
      IF FieldType <> ftCrypt THEN
        s := s + Format('%4s', ['112'])
      ELSE
        IF FieldLength < 15 THEN
          s := s + Format('%4d', [111 + FieldLength])
        ELSE
          s := s + Format('%4d', [FieldLength]);

      s := s + ' ';                      //Another unnescessary blank
      s := s + EpiUtf8ToAnsi(VariableLabel);

      s := s + #13#10;
      Stream.Write(S[1], Length(S));
    END; // End With Field...

    // ******************
    //    Write Data
    // ******************
    for CurRec := 1 to Size do
    begin
      S := '';
      for i := 0 TO FieldCount - 1 DO
      with Field[i] do begin
        if IsMissing[CurRec] then
          T := DupeString(' ', FieldLength)
        else if FieldType = ftCrypt then
        begin
          EncData := EpiUtf8ToAnsi(Trim(AsString[CurRec]));
          FCrypter.InitStr(Password);
          FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
          EncData := B64Encode(EncData);
          FCrypter.Reset;
          T := Format('%-*s', [FieldLength, EncData])
        end else if FieldType in [ftString, ftUpperAlfa] then
          T := Format('%-*s', [FieldLength, EpiUtf8ToAnsi(AsString[CurRec])])
        else if FieldType = ftFloat then
          T := Format('%*.*f', [FieldLength, FieldDecimals, AsFloat[CurRec]], Fmt)
        else
          T := Format('%*s', [FieldLength, EpiUtf8ToAnsi(AsString[CurRec])]);
        S := S + T;
      end;
      Z := Length(S);
      if Z + 3 > MaxRecLineLength then
        for I := (Z div MaxRecLineLength) downto 1 do
          Insert(EOLchars, S, (MaxRecLineLength * I) + 1);

      if Deleted[CurRec] then
        S := S + '?' + #13#10
      else if Verified[CurRec] then
        S := S + '^' + #13#10
      else
        S := S + EOLchars;

      Stream.Write(S[1], Length(S));
    end;

    result := true;

    if not (eoIgnoreChecks in FOptions) then
    begin
      if Assigned(Stream) then FreeAndNil(Stream);
      FileProperties.FileName := ChangeFileExt(FileName, '.chk');
      Stream := TFileStream.Create(FileProperties.FileName, fmCreate);
      ChkIO := TCheckFileIO.Create();
      result := ChkIO.WriteCheckToStream(Stream, Self);
      if Stream.Size = 0 then
      begin
        FreeAndNil(Stream);
        DeleteFile(FileProperties.FileName);
        FileProperties.FileName := '';
      end;
    end;
  finally
    EpiLogger.DecIndent;
    if Assigned(Stream) then FreeAndNil(Stream);
    if Assigned(ChkIO) then FreeAndNil(ChkIO);
  end;
end;

function TEpiDataFile.Lang(LangCode: Integer; Const LangText: string): string;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TEpiDataFile.RequestPassword(Const EncryptedString: string): boolean;
var
  S: string;
begin
  result := false;
  if Trim(Password) = '' then
    if Assigned(FOnPassword) then
      FOnPassword(Self, rpOpen, FPassword);
  try
    S := B64Decode(EncryptedString);
    FCrypter.InitStr(Password);
    FCrypter.DecryptCFB(S[1], S[1], Length(S));
    FCrypter.Reset;
    Result := (AnsiCompareText(Password, S) = 0);
  except
    FErrorText := Lang(0, 'Fatal Error in decrypting password.');
    FErrorCode := EPI_INVALID_PASSWORD;
    EpiLogger.AddError(ClassName, 'RequestPassword', ErrorText, 0);
    Abort;
  end;
end;

function TEpiDataFile.UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

function TEpiDataFile.TextPos(var F: Textfile): Cardinal;
begin
  with TTextRec(F) do
  begin
    Result := FileSeek(Handle, 0, 1);
    if Mode = FMOutput then
      inc(Result, BufPos)
    else if BufEnd <> 0 then
      Dec(Result, BufEnd-BufPos);
  end;
end;

function TEpiDataFile.GetFieldCount: Cardinal;
begin
  result := Fields.Count;
end;

procedure TEpiDataFile.DoChange(Event: TEpiDataFileChangeEventType;
  OldValue: Pointer);
var
  i: Integer;
  func: TEpiDataFileChangeEvent;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeListCount - 1 do
    FOnChangeList[i](Self, Event, OldValue);
end;

constructor TEpiDataFile.Create(ASize: Cardinal = 0);
var
  p: pointer;
begin
  inherited Create;
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Create', 3);

  try
    Reset;
    FFieldNaming := fnFirstWord;
    FRecordStatus.Size := ASize;
  finally
    EpiLogger.DecIndent;
  end;
end;

destructor TEpiDataFile.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 2, 'Filename = ' + FileName);
  try
    InternalReset();

    ReAllocMem(FOnChangeList,0);
    FOnChangeListCount := 0;
    FUpdateCount := 0;

    inherited Destroy;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.PrepareDataFile(FieldNames: TStrings): TEpiDataFile;
begin
  //
end;

function TEpiDataFile.Clone(CloneData: boolean): TEpiDataFile;
var
  i: Integer;
begin
  // Create DataFile!
  Result := TEpiDataFile.Create(Size);

  // Clone Basic data:
  Result.FileLabel   := FileLabel;
  Result.FileVersion := FileVersion;
  Result.FieldNaming := FieldNaming;
  Result.Study       := Study;
  Result.Password    := Password;
  Result.FOptions    := Options;

  // Close Events!
  Result.OnPassword := OnPassword;
  Result.OnProgress := OnProgress;
  Result.OnTranslate := OnTranslate;

  // Clone status field.
  FreeAndNil(Result.FRecordStatus);
  Result.FRecordStatus := FRecordStatus.Clone(Result, CloneData);

  // Clone the check file!
  FreeAndNil(Result.FCheckFile);
  Result.FCheckFile := FileProperties.Clone;

  // Clone Index:
  FreeAndNil(Result.FIndexFile);
  Result.FIndexFile := IndexFile.Clone;

  // Clone ValueLabels.
  Result.ValueLabels.Assign(ValueLabels);

  // Clone Fields.
  for i := 0 To Fields.Count -1 do
    Result.AddField(Fields[i].Clone(Result, CloneData));
end;

function TEpiDataFile.Open(const aFileName: string; aOptions: TEpiDataFileOptions = []): boolean;
var
  TmpStream: TFileStream;
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Open', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;
    FOptions := aOptions;

    Ext := ExtractFileExt(FileName);
    if AnsiUpperCase(Ext) = '.REC' then
      result := InternalOpenOld
    else if AnsiUpperCase(Ext) = '.RECXML' then
      result := InternalOpen
    else begin
      FErrorText := Format(Lang(0, 'Unsupported file type for direct reading: %s'), [Ext]);
      FErrorCode := EPI_OPEN_FILE_ERROR;
      result := False;
      EpiLogger.AddError(Classname, 'Open', ErrorText, 0);
    end;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.Save(Const aFileName: string; aOptions: TEpiDataFileOptions = []): boolean;
var
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Save', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;
    FOptions := aOptions;

    if AnsiUpperCase(ExtractFileExt(FileName)) = '.RECXML' then
      result := InternalSave
    else
      Result := InternalSaveOld;
  finally
    EpiLogger.DecIndent;
  end;
end;

procedure TEpiDataFile.Reset;
begin
  InternalReset;

  FFields       := TEpiFields.Create(Self);
  FFields.Owned := true;
  FFields.ReportOnChange := true;

  FTextLabels   := TEpiTextLabels.Create(Self);
  FTextLabels.Owned := true;
  FTextLabels.ReportOnChange := true;

  FScreenProperties := TEpiScreenProperties.Create(Self);
  FScreenProperties.Owned := true;
  FScreenProperties.ReportOnChange := true;

  FValueLabels  := TValueLabelSets.Create;
  FCheckFile    := TEpiDataFileProperties.Create;
  FCrypter      := TDCP_rijndael.Create(nil);
  FRecordStatus := TEpiIntField.Create(0, ftInteger);
end;

function TEpiDataFile.FieldByName(Const aFieldName: string): TEpiField;
begin
  result := Fields.FieldByName(aFieldName);
end;

function TEpiDataFile.FieldById(const aId: string): TEpiField;
begin
  result := Fields.FieldById(aId);
end;

function TEpiDataFile.FieldExists(Const aFieldName: string): boolean;
begin
  result := Fields.FieldExists(aFieldName);
end;

function TEpiDataFile.FieldIndex(const aFieldName: string): Integer;
begin
  result := Fields.IndexOf(aFieldName);
end;

procedure TEpiDataFile.AddField(AField: TEpiField);
begin
  Fields.Add(AField);

  // Handle Valuelabels in accordance with Field Cloning rule 2.
  if Assigned(AField.ValueLabelSet) then
  begin
    // This Valuelabelset is already present!
    if ValueLabels.ValueLabelSetByName(AField.ValueLabelSet.Name) =
       AField.ValueLabelSet then
      exit;
    ValueLabels.AddValueLabelSet(AField.ValueLabelSet);
  end;
end;

procedure TEpiDataFile.RemoveField(var AField: TEpiField; DoDestroy: boolean);
begin
  if not Assigned(AField) then exit;

  Fields.Delete(AField);

  if DoDestroy then
    FreeAndNil(AField);
end;

procedure TEpiDataFile.AddTextLabel(ATextLabel: TEpiTextLabel);
begin
  TextLabels.Add(ATextLabel);
end;

procedure TEpiDataFile.RemoveTextLabel(var ATextLabel: TEpiTextLabel;
  DoDestroy: boolean);
begin
  if not Assigned(ATextLabel) then exit;

  TextLabels.Delete(ATextLabel);
  if DoDestroy then
    FreeAndNil(ATextLabel);
end;

function TEpiDataFile.TextLabelById(const ATextLabelId: string): TEpiTextLabel;
begin
  result := TextLabels.TextLabelById(ATextLabelId);
end;

function TEpiDataFile.TextLabelExists(const ATextLabelId: string): boolean;
begin
  result := TextLabels.TextLabelExists(ATextLabelId);
end;

function TEpiDataFile.CreateUniqueFieldName(const AText: string): string;
var
  Number: Integer;
  TmpStr: String;
begin
  TmpStr := AText;
  Result := AText;

  // If fieldname is unique, do nothing.
  Number := 1;
  while FieldExists(result) do
  begin
    // not unique, find a new.
    Result := TmpStr + IntToStr(Number);
    Inc(Number);
  end;
end;

procedure TEpiDataFile.NewRecords(ACount: Integer);
var
  i: Integer;
begin
  FRecordStatus.NewRecords(ACount);
  for i := 0 to FieldCount - 1 do
    Field[i].NewRecords(ACount);
end;

procedure TEpiDataFile.SortFields(Cmp: TListSortCompare);
begin
  Fields.Sort(Cmp);
end;

function TEpiDataFile.DocumentDatafile: TStrings;
var
  TmpStr: String;
  i: Integer;
  j: Integer;
begin
  Result := TStringList.Create;

  with Result do
  try
    {Header}
    TmpStr := FileName;
    if TmpStr = '' then
      TmpStr := '(not saved yet)';
    Append(Format('Datafile: %s', [TmpStr]));
    Append(Format('Filelabel: %s', [FileLabel]));
    if (FileName <> '') and (FileExistsUTF8(FileName)) then
      Append('Last revision: ' +
        FormatDateTime('d. mmm yyyy t', FileDateToDateTime(FileAge(UTF8ToSys(Filename)))));
    Append(Format(
      'Number of Fields: %d' + LineEnding +
      'Number of text labels: %d' + LineEnding +
      'Number of Records: %d' + LineEnding,
      [FieldCount, TextLabelCount, Size]));
    Append('');

    {Variable Information}
    { - header}
    Append('Fields in datafile:');
    Append(DupeString('-',102));
    Append(Format(
      '%-3s %-10s %-20s %-15s %-6s %-8s %-20s',
      ['No','Name','Variable label','Fieldtype','Length','Decimals','Value labels']));
    Append(DupeString('-',102));

    for i := 0 to FieldCount - 1 do
    with Field[i] do
    begin
      TmpStr := '';
      if Assigned(ValueLabelSet) then
        TmpStr := ValueLabelSet.Name;

      Append(
        UTF8Encode(
          WideFormat(
            '%3d %-10.10s %-20.20s %-15s %6d %8d %-20.20s',
            [i+1, UTF8Decode(FieldName), UTF8Decode(VariableLabel),
             UTF8Decode(FieldTypeToFieldTypeName(FieldType, nil)),
             FieldLength, FieldDecimals, UTF8Decode(TmpStr)]
          )
        )
      );

      if Assigned(ValueLabelSet) then
      for j := 0 to ValueLabelSet.Count -1 do
      begin
        // Left adjust at index 67.
        TmpStr := String(ValueLabelSet.Values[j]) + ': ' + ValueLabelSet.Labels[j];
        if ValueLabelSet.MissingValues[j] then
          TmpStr += '(missing)';
        Append(WideFormat('%-67.67s %-20.20s', ['', UTF8Decode(TmpStr)]));
      end;
    end;
  finally
  end;
end;

procedure TEpiDataFile.BeginUpdate;
var
  i: Integer;
begin
  Inc(FUpdateCount);
  for i := 0 to FieldCount -1 do
    Field[i].BeginUpdate;
end;

procedure TEpiDataFile.EndUpdate;
var
  i: Integer;
begin
  Dec(FUpdateCount);
  for i := 0 to FieldCount -1 do
    Field[i].EndUpdate;

  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(dceUpdate, nil);
end;

procedure TEpiDataFile.RegisterOnChangeHook(Event: TEpiDataFileChangeEvent);
begin
  Inc(FOnChangeListCount);
  ReAllocMem(FOnChangeList, FOnChangeListCount * SizeOf(TEpiDataFileChangeEvent));
  FOnChangeList[FOnChangeListCount-1] := Event
end;

procedure TEpiDataFile.UnRegisterOnChangeHook(Event: TEpiDataFileChangeEvent);
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeListCount -1 do
  begin
    if FOnChangeList[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeListCount then exit;

  dec(FOnChangeListCount);
  if FOnChangeListCount > Idx then
    System.Move(FOnChangeList[Idx+1],FOnChangeList[Idx],(FOnChangeListCount-Idx)*SizeOf(TEpiDataFileChangeEvent));
  ReAllocMem(FOnChangeList,FOnChangeListCount*SizeOf(TEpiDataFileChangeEvent));
end;

{ TEpiIndexFile }

function TEpiIndexFile.GetIndexField(Index: integer): TEpiField;
begin
  Result := nil;
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  Result := TEpiField(FIndexFields[Index-1]);
end;

procedure TEpiIndexFile.SetIndexField(Index: Integer; aField: TEpiField);
begin
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  FIndexFields[Index-1] := aField;
end;

function TEpiIndexFile.GetIndexUnique(Index: integer): Boolean;
begin
  // TODO -o Torsten : Implement using TBit!
  result := false;
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  Result := (FIndexUnique and ($01 shl Index) > 0);
end;

procedure TEpiIndexFile.SetIndexUnique(Index: Integer; aUnique: Boolean);
begin
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  FIndexUnique := FIndexUnique or (Integer(aUnique) shl Index);
end;


function TEpiIndexFile.GetIndexCount: Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) then Inc(Result);
end;

procedure TEpiIndexFile.InternalReset;
begin
  if Assigned(FIndexFields) then FreeAndNil(FIndexFields);
  FFileName := '';
  FIndexUnique := 0;
end;

constructor TEpiIndexFile.Create(const aFileName: string);
begin
  Reset;
  FFileName := aFileName;
end;

destructor TEpiIndexFile.Destroy;
begin
  InternalReset;
  inherited;
end;

function TEpiIndexFile.Clone: TEpiIndexFile;
begin
  Result := TEpiIndexFile.Create('');

  Result.FIndexFields.Assign(FIndexFields);
  Result.FIndexUnique := FIndexUnique;
end;

procedure TEpiIndexFile.Reset;
var
  i: integer;
begin
  InternalReset;
  FIndexFields := TList.Create;
  for i := 1 to MaxIndices do
    FIndexFields.Add(nil);
end;

function TEpiIndexFile.IndexNoByName(Const Name: string): Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) and
       (AnsiCompareText(IndexFields[i].FieldName, Name) = 0) then
      result := i;
end;

function TEpiIndexFile.IndexFieldByName(Const Name: string): TEpiField;
var
  i: integer;
begin
  result := nil;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) and
       (AnsiCompareText(IndexFields[i].FieldName, Name) = 0) then
      result := IndexFields[i];
end;

function TEpiIndexFile.IndexExists(Const Name: string): Boolean;
begin
  result := Assigned(IndexFieldByName(Name));
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

{ TEpiScreenProperty }

constructor TEpiScreenProperty.Create(AOwner: TEpiScreenProperties);
begin
  inherited Create;
  FOwner := AOwner;
  Reset;
end;

destructor TEpiScreenProperty.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TEpiScreenProperty.Clone(var Dest: TEpiScreenProperty);
begin
  if not Assigned(Dest) then
    Dest := TEpiScreenProperty.Create(nil);

  Dest.FId       := FId;
  Dest.FName     := FName;
  Dest.FFgColour := FFgColour;
  Dest.FBgColour := FBgColour;
  Dest.FHlColour := FHlColour;
end;

procedure TEpiScreenProperty.Reset;
begin
  Id       := '';
  Name     := '';
  FgColour := EpiColourBase;
  BgColour := EpiColourBase;
  HlColour := EpiColourBase;
end;

{ TEpiTextLabel }

procedure TEpiTextLabel.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(tceId, @Val);
end;

procedure TEpiTextLabel.SetText(const AValue: string);
var
  Val: String;
begin
  if FText = AValue then exit;
  Val := FText;
  FText := AValue;
  DoChange(tceText, @Val);
end;

procedure TEpiTextLabel.SetTextLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if FTextLeft = AValue then exit;
  Val := FTextLeft;
  FTextLeft := AValue;
  DoChange(tceLeft, @Val);
end;

procedure TEpiTextLabel.SetTextTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if FTextTop = AValue then exit;
  Val := FTextTop;
  FTextTop := AValue;
  DoChange(tceTop, @Val);
end;

constructor TEpiTextLabel.Create(AOwner: TEpiTextLabels);
begin
  FOwner := AOwner;
end;

destructor TEpiTextLabel.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiTextLabel.DoChange(EventType: TEpiTextLabelChangeEventType;
  OldValue: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeCount - 1 do
    FOnChange[i](Self, EventType, OldValue);
end;

procedure TEpiTextLabel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiTextLabel.EndUpdate;
begin
  Dec(FUpdateCount);

  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(tceUpdate, nil);
end;

procedure TEpiTextLabel.RegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
begin
  Inc(FOnChangeCount);
  ReAllocMem(FOnChange, FOnChangeCount * SizeOf(TEpiTextLabelChangeEvent));
  FOnChange[FOnChangeCount-1] := Event
end;

procedure TEpiTextLabel.UnRegisterOnChangeHook(Event: TEpiTextLabelChangeEvent
  );
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

{ TEpiTextLabels }

function TEpiTextLabels.GetCount: Cardinal;
begin
  result := FList.Count;
end;

function TEpiTextLabels.GetTextLabel(Index: integer): TEpiTextLabel;
begin
  result := TEpiTextLabel(FList[Index]);
end;

constructor TEpiTextLabels.Create(AOwner: TEpiDataFile);
begin
  FList := TFPList.Create;
  FOwner := AOwner;
  FReportOnChange := false;
end;

destructor TEpiTextLabels.Destroy;
var
  T: TEpiTextLabel;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      T := TEpiTextLabel(FList.Last);
      FreeAndNil(T);
    end;
    Flist.Delete(Flist.Count - 1);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEpiTextLabels.Add(aTextLabel: TEpiTextLabel);
begin
  FList.Add(aTextLabel);

  if Owned then
    aTextLabel.FOwner := Self;

  if ReportOnChange and Assigned(FOwner) then
    FOwner.DoChange(dceAddText, aTextLabel);
end;

procedure TEpiTextLabels.Delete(aTextLabel: TEpiTextLabel);
var
  Idx: LongInt;
begin
  Idx := IndexOf(aTextLabel.Id);
  FList.Delete(Idx);

  if Owned then
    aTextLabel.FOwner := nil;
  if ReportOnChange and Assigned(FOwner) then
    FOwner.DoChange(dceRemoveText, aTextLabel);
end;

function TEpiTextLabels.TextLabelById(const aTextLabelId: string): TEpiTextLabel;
var
  i: LongInt;
begin
  Result := nil;
  i := IndexOf(aTextLabelId);
  if i >= 0 then
    result := TEpiTextLabel(FList[i]);
end;

function TEpiTextLabels.TextLabelExists(const aTextLabelId: string): boolean;
begin
  result := Assigned(TextLabelById(aTextLabelId));
end;

function TEpiTextLabels.IndexOf(const aTextLabelId: string): integer;
begin
  for result := 0 to FList.Count - 1 do
    if CompareText(TEpiTextLabel(Flist[result]).Id, aTextLabelId) = 0 then
      exit;
  result := -1;
end;


{ TEpiScreenProperties }

function TEpiScreenProperties.GetCount: Cardinal;
begin
  Result := FList.Count;
end;

function TEpiScreenProperties.GetDefaultScreenProperty: TEpiScreenProperty;
begin
  if not Assigned(FDefaultScreenProperty) then
  begin
    FDefaultScreenProperty := TEpiScreenProperty.Create(Self);
    FDefaultScreenProperty.Id := '';
    FDefaultScreenProperty.BgColour := EpiColourBase;
    FDefaultScreenProperty.FgColour := EpiColourBase;
    FDefaultScreenProperty.HlColour := EpiColourBase;
  end;
  result := FDefaultScreenProperty;
end;

function TEpiScreenProperties.GetScreenProperty(Index: integer
  ): TEpiScreenProperty;
begin
  Result := TEpiScreenProperty(FList[Index]);
end;

constructor TEpiScreenProperties.Create(AOwner: TEpiDataFile);
begin
  FList := TFPList.Create;
  FOwner := AOwner;
  FReportOnChange := false;
  FDefaultScreenProperty := nil;
end;

destructor TEpiScreenProperties.Destroy;
var
  S: TEpiScreenProperty;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      S := TEpiScreenProperty(FList.Last);
      FreeAndNil(S);
    end;
    Flist.Delete(Flist.Count - 1);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEpiScreenProperties.Add(aScreenProperty: TEpiScreenProperty);
begin
  FList.Add(aScreenProperty);

  if Owned then
    aScreenProperty.FOwner := Self;

  if ReportOnChange and Assigned(FOwner) then
    FOwner.DoChange(dceAddScreenProp, nil);
end;

procedure TEpiScreenProperties.Delete(aScreenProperty: TEpiScreenProperty);
var
  Idx: LongInt;
begin
  Idx := IndexOf(aScreenProperty.Id);
  FList.Delete(Idx);

  if Owned then
    aScreenProperty.FOwner := nil;
  if ReportOnChange and Assigned(FOwner) then
    FOwner.DoChange(dceRemoveScreenProp, aScreenProperty);
end;

function TEpiScreenProperties.ScreenPropertyById(const aScreenPropertyId: string
  ): TEpiScreenProperty;
var
  i: LongInt;
begin
  Result := nil;
  i := IndexOf(aScreenPropertyId);
  if i >= 0 then
    result := TEpiScreenProperty(FList[i]);
end;

function TEpiScreenProperties.ScreenPropertyExists(
  const aScreenPropertyId: string; var aScreenProperty: TEpiScreenProperty
  ): boolean;
begin
  aScreenProperty := ScreenPropertyById(aScreenPropertyId);
  result := Assigned(aScreenProperty);
end;

function TEpiScreenProperties.IndexOf(const aScreenPropertyId: string
  ): integer;
begin
  for result := 0 to FList.Count - 1 do
    if CompareText(TEpiScreenProperty(Flist[result]).Id, aScreenPropertyId) = 0 then
      exit;
  result := -1;
end;

end.
