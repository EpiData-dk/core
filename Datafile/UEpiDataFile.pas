unit UEpiDataFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, UValueLabels, Rijndael, UCheckFileCmds, UEpiLog,
  UCheckFileTypes, Graphics, UDataFileTypes, UEpiDataGlobals;

type

  TEpiField = class;
  TEpiFields = class;
  TEpiDataFile = class;

  { TEpiCheckField }

  TEpiCheckField = class(TObject)
  private
    FMustEntert:       Boolean;
    FNoEnter:          Boolean;
    FTopOfScreen:      Boolean;
    FTopOfScreenLines: Integer;
    FDoRepeat:         boolean;
    FConfirm:          boolean;
    FBeforeCmds:       TChkCommands;
    FAfterCmds:        TChkCommands;
    FMin:              string;
    FMax:              string;
    FLegal:            string;
    FMissingValues:    TMissingValues;
    FDefaultValue:     string;
    FAutoFields:       string;
    FAutoList:         Boolean;
    FJumps:            string;
    FJumpResetChar:    Char;
    FValueLabel:       TValueLabelSet;
    FShowValueLabel:   Boolean;
    FValueLabelIsFieldRef: Boolean;
    FTypeType:         TTypeType;
    FTypeText:         string;
    FTypeColor:        TColor;
    FHasGlobalDefaultVal: Boolean;
    FFieldScope:       TFieldScope;
    FFieldComments:    TStrings;
    function           GetMissingValue(Index: integer): string;
    procedure          SetMissingValue(Index: integer; Value: string);
    function           GetAutoSearch(): Boolean;
    procedure          InternalReset();
  protected

  public
    Constructor Create();
    Destructor  Destroy(); override;
    procedure   Reset();
    procedure   Clone(Var Dest: TEpiCheckField);
    Property    ValueLabel:   TValueLabelSet read FValueLabel write FValueLabel;
    Property    ValueLabelIsFieldRef: Boolean read FValueLabelIsFieldRef write FValueLabelIsFieldRef;
    property    MustEnter:    boolean read FMustEntert write FMustEntert;
    property    NoEnter:      boolean read FNoEnter write FNoEnter;
    property    TopOfScreen:  Boolean read FTopOfScreen write FTopOfScreen;
    property    TopOfScreenLines: Integer read FTopOfScreenLines write FTopOfScreenLines;
    property    DoRepeat:     boolean read FDoRepeat write FDoRepeat;
    property    Confirm:      boolean read FConfirm write FConfirm;
    property    BeforeCmds:   TChkCommands read FBeforeCmds;
    property    AfterCmds:    TChkCommands read FAfterCmds;
    property    Min:          string read FMin write FMin;
    property    Max:          string read FMax write FMax;
    property    Legal:        string read FLegal write FLegal;
    property    MissingValues[Index: integer]: string read GetMissingValue write SetMissingValue;
    property    DefaultValue: string read FDefaultValue write FDefaultValue;
    property    AutoFields:   string read FAutoFields write FAutoFields;
    property    AutoSearch:   Boolean read GetAutoSearch;
    property    AutoList:     boolean read FAutoList write FAutoList;
    property    Jumps:        string read FJumps write FJumps;
    property    JumpResetChar: Char read FJumpResetChar write FJumpResetChar;
    property    ShowValueLabel: Boolean read FShowValueLabel write FShowValueLabel;
    property    TypeType:     TTypeType read FTypeType write FTypeType;
    property    TypeText:     string read FTypeText write FTypeText;
    property    TypeColour:   TColor read FTypeColor write FTypeColor;
    property    HasGlobalDefaultVal: Boolean read FHasGlobalDefaultVal write FHasGlobalDefaultVal;
    property    FieldScope:   TFieldScope read FFieldScope write FFieldScope;
    property    FieldComments: TStrings read FFieldComments;
  end;

  { TEpiCheckFile }

  TEpiCheckFile = class(TObject)
  private
    FValueLabelSets:       TValueLabelSets;
    FTopComments:          TStringList;       //Commentlines in the top of the checkfile
    FBeforeFileCmds:       TChkCommands;      //Commands to be run when file is opened
    FAfterFileCmds:        TChkCommands;      //Commands to be run when file is closed
    FBeforeRecordCmds:     TChkCommands;      //Commands to be run before current record changes
    FAfterRecordCmds:      TChkCommands;      //Commands to be run when changing current record
    FRecodeCmds:           TChkCommands;      //Commands to be run during Recode Datafile
    FAssertList:           TStringList;       //used only to store Asserts for checkfilemode
    FConfirm:              Boolean;           //If true then a field is not left automatically when filled out
    FAutoSave:             Boolean;           //IF true then user is not asked "Save record to disk?"
    FGlobalMissingValues:  TMissingValues;
    FGlobalDefaultValue:   string;            //Global default value defined by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FGlobalTypeCom:        Boolean;           //Show that all fields has a Type Comment Fieldname
    FGlobalTypeComColor:   TColor;
    FMissingAction:        TMissingAction;
    FShowLastRecord:       Boolean;           //if set, then last record is shown when datafile is opened; if false (default) then
    FFieldHighlightAct:    Boolean;           //highlight active field
    FFieldHighlightCol:    TColor;            //color af highlight of active field
    FBackupList:           TStringList;       //List of files to backup
    FErrorInFile:          Boolean;
    FHasCheckFile:         Boolean;
    FFileName:             string;
    FDefines:              TEpiFields;       
    function    GetGlobMissing(Index: Integer): string;
    procedure   SetGlobMissing(Index: Integer; const Value: string);
    procedure   InternalReset();
  protected

  public
    Constructor Create();
    Destructor  Destroy(); override;
    procedure   Reset();
    function    DefineExists(Const aName: string): Boolean;
    function    DefineByName(Const aName: string): TEpiField;
    procedure   AddDefine(Field: TEpiField);
    Property    ValueLabels:      TValueLabelSets read FValueLabelSets;
    Property    BeforeFileCmds:   TChkCommands read FBeforeFileCmds write FBeforeFileCmds;        //Commands to be run when file is opened
    Property    AfterFileCmds:    TChkCommands read FAfterFileCmds write FAfterFileCmds;          //Commands to be run when file is closed
    Property    BeforeRecordCmds: TChkCommands read FBeforeRecordCmds write FBeforeRecordCmds;    //Commands to be run before current record changes
    Property    AfterRecordCmds:  TChkCommands read FAfterRecordCmds write FAfterRecordCmds;      //Commands to be run when changing current record
    Property    RecodeCmds:       TChkCommands read FRecodeCmds write FRecodeCmds;                //Commands to be run during Recode Datafile
    Property    Confirm:          Boolean read FConfirm write FConfirm;
    Property    Autosave:         Boolean read FAutoSave write FAutoSave;
    Property    GlobalDefaultVal: string read FGlobalDefaultValue write FGlobalDefaultValue;
    Property    GlobalMissingVal[Index: Integer]: string read GetGlobMissing write SetGlobMissing;
    Property    MissingAction:    TMissingAction read FMissingAction write FMissingAction;
    Property    GlobalTypeCom:    Boolean read FGlobalTypeCom write FGlobalTypeCom;
    Property    GlobalTypeComColor: TColor read FGlobalTypeComColor write FGlobalTypeComColor;
    Property    ShowLastRecord:   Boolean read FShowLastRecord write FShowLastRecord;
    Property    FieldHighlightAct: Boolean read FFieldHighlightAct write FFieldHighlightAct;
    Property    FieldHighlightCol: TColor read FFieldHighlightCol write FFieldHighlightCol;
    Property    BackupList:       TStringList read FBackupList;
    Property    TopComments:      TStringList read FTopComments;
    Property    AssertList:       TStringList read FAssertList;
    Property    ErrorInFile:      Boolean read FErrorInFile write FErrorInFile;
    Property    HasCheckFile:     Boolean read FHasCheckFile write FHasCheckFile;
    Property    FileName:         string read FFileName write FFileName;
  end;

  { TEpiField }

  TEpiField = class(TObject)
  private
    FOwner:        TEpiFields;
    FDataFile:     TEpiDataFile;
    FCapacity:     Integer;
    FSize:         Integer;
    FFieldNo:      Cardinal;        
    FDisplayChar:  Char;
    FFieldName:    string;
    FQuestX:       Cardinal;
    FQuestY:       Cardinal;
    FQuestColor:   Cardinal;
    FFieldX:       Cardinal;
    FFieldY:       Cardinal;
    FFieldColor:   Cardinal;
    FFieldType:    TFieldType;
    FFieldLength:  Cardinal;
    FCryptLength:  Cardinal;
    FNumDecimals:  Cardinal;
    FQuestion:     string;
    FVariableLabel: string;
    FCheckField:   TEpiCheckField;
    function       GetValueLabel: TValueLabelSet;
//    function       GetAsFmtData: string;
  protected
    FCurrentData: String;
    FChanged: Boolean;
    function GetAsBoolean(const index: Integer): EpiBool; virtual; abstract;
    function GetAsDate(const index: Integer): EpiDate; virtual; abstract;
    function GetAsFloat(const index: Integer): EpiFloat; virtual; abstract;
    function GetAsInteger(const index: Integer): EpiInteger; virtual; abstract;
    function GetAsString(const index: Integer): EpiString; virtual; abstract;
    function GetAsValue(const index: Integer): string; virtual;
    function GetCapacity: Integer; virtual; abstract;
    function GetIsMissing(const index: Integer): boolean; virtual; abstract;
    function GetIsMissingValue(const index: Integer): boolean; virtual; abstract;
    function GetSize: Integer; virtual;
    procedure Grow; virtual;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); virtual; abstract;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); virtual; abstract;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); virtual; abstract;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); virtual; abstract;
    procedure SetAsString(const index: Integer; const AValue: EpiString); virtual; abstract;
    procedure SetCapacity(AValue: Integer); virtual; abstract;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); virtual; abstract;
    procedure SetSize(const AValue: Integer); virtual;
    constructor Create(ASize: Cardinal; AFieldType: TFieldType); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    destructor  Destroy; override;
    class function CreateField(aFieldType: TFieldType; aSize: Cardinal = 0): TEpiField;
    procedure   Reset();
    procedure   Clone(Var Dest: TEpiField);
//    property    AsFmtData:   string read GetAsFmtData;
    procedure Exchange(i,j: integer); virtual; abstract;
    function  Compare(i,j: integer): integer; virtual; abstract;
    procedure NewRecords(ACount: Integer = 1); virtual;
    property  FieldNo:     Cardinal read FFieldNo write FFieldNo;
    property  DisplayChar: Char read FDisplayChar write FDisplayChar;
    property  FieldName:   string read FFieldName write FFieldName;
    property  QuestX:      Cardinal read FQuestX write FQuestX;
    property  QuestY:      Cardinal read FQuestY write FQuestY;
    property  QuestColor:  Cardinal read FQuestColor write FQuestColor;
    property  FieldX:      Cardinal read FFieldX write FFieldX;
    property  FieldY:      Cardinal read FFieldY write FFieldY;
    property  FieldColor:  Cardinal read FFieldColor write FFieldColor;
    property  FieldType:   TFieldType read FFieldType;
    property  FieldLength: Cardinal read FFieldLength write FFieldLength;
    property  CryptLength: Cardinal read FCryptLength write FCryptLength;
    property  NumDecimals: Cardinal read FNumDecimals write FNumDecimals;
    property  Question:    string read FQuestion write FQuestion;
    property  VariableLabel: string read FVariableLabel write FVariableLabel;
    property  CheckField:  TEpiCheckField read FCheckField write FCheckField;
    property  ValueLabelSet: TValueLabelSet read GetValueLabel;
    property  Owner:       TEpiFields read FOwner;
    property  DataFile:    TEpiDataFile read FDataFile write FDataFile;
    property  Size: Integer read GetSize write SetSize;
    property  IsMissing[const index: Integer]: boolean read GetIsMissing write SetIsMissing;
    property  IsMissingValue[const index: Integer]: boolean read GetIsMissingValue;
    property  AsBoolean[const index: Integer]: EpiBool read GetAsBoolean write SetAsBoolean;
    property  AsInteger[const index: Integer]: EpiInteger read GetAsInteger write SetAsInteger; default;
    property  AsFloat[const index: Integer]: EpiFloat read GetAsFloat write SetAsFloat;
    property  AsDate[const index: Integer]: EpiDate read GetAsDate write SetAsDate;
//    property AsTime[const index: Integer]: EpiTime read GetAsTime write SetAsTime;
//    property AsDateTime[const index: Integer]: EpiDateTime read GetAsDateTime write SetAsDateTime;
    property  AsString[const index: Integer]: EpiString read GetAsString write SetAsString;
    property  AsValue[const index: Integer]: string read GetAsValue;
  end;

  { TEpiIntField }
  // Handles following field types:
  // ftInteger, ftIDNum,
  TEpiIntField = class(TEpiField)
  private
    FData: array of EpiInteger;
  protected
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetIsMissingValue(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
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
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetIsMissingValue(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
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
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetIsMissingValue(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
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
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetIsMissingValue(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
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
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetIsMissingValue(const index: Integer): boolean; override;
    function GetSize: Integer; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
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
    function    GetField(Index: Integer): TEpiField;
    function    GetCount(): Cardinal;
  protected
    procedure   Add(aField: TEpiField);
    procedure   Delete(aField: TEpiField);
  public
    constructor Create(aOwner: TEpiDataFile); virtual;
    destructor  Destroy(); override;
    function    FieldByName(Const aFieldName: string): TEpiField;
    function    FieldExists(Const aFieldName: string): boolean;
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
    function         GetIndexCount(): Integer;
    procedure        InternalReset();
  protected
  public
    constructor Create(Const aFileName: string);
    destructor  Destroy; override;
    procedure   Reset();
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
    FFileName:     string;                  // Physical datafile name.
    FFileLabel:    string;                  // Label of datafile.
    FFields:       TEpiFields;              // Container for all associated fields. (Owned)
    FDataFields:   TEpiFields;              // - holds list of data fields only. (Not Owned)
    FQuestFields:  TEpiFields;              // - holds list of question fields only. (Not Owned. FieldType = ftQuestion)
    FPassword:     string;                  // Datafile password
    FFieldNaming:  TFieldNaming;            // Datafile fieldnaming convention - Firstword or Auto.
    FCheckFile:    TEpiCheckFile;           // Container for global/datafile level information of the .CHK file.
    FOptions:      TEpiDataFileOptions;     // Options set by Open(...)
    FOnProgress:   TProgressEvent;          // Callback event for updating progress
    FOnTranslate:  TTranslateEvent;         // Callback event for translation
    FOnPassword:   TRequestPasswordEvent;   // Callback event for password request
    FIndexFile:    TEpiIndexFile;
    FErrorText:    string;
    FErrorCode:    Cardinal;
    FCrypter:      TDCP_rijndael;
    FFileVersion:  Cardinal;
    FRecordStatus: TEpiField;
    function   GetField(Index: integer): TEpiField;
    function   GetIndexFile: TEpiIndexFile;
    function   GetSize: Integer;
    function   GetValueLabels: TValueLabelSets;
    procedure  InternalReset;
    procedure  SetSize(const AValue: Integer);
  protected
    function   InternalOpen: boolean;
    function   InternalSave: boolean;
    function   Lang(LangCode: Integer; Const LangText: string): string;
    function   RequestPassword(Const EncryptedString: string): boolean;
    Function   UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
    Function   TextPos(var F: Textfile): Cardinal;
    function   GetNumFields: Cardinal;
    function   GetNumDataFields: Cardinal;
  public
    constructor Create(aOptions: TEpiDataFileOptions = []); virtual;
    destructor Destroy; override;
    function   Open(Const aFileName: string): boolean;
    function   Save(Const aFileName: string): boolean;
//    procedure  SaveMemToFile(Const aFileName: string);
//    procedure  Read(RecNumber: Integer);
//    procedure  Write(RecNumber: Integer = NewRecord);
    procedure  Reset;
    function   FieldByName(Const aFieldName: string): TEpiField;
    function   FieldExists(Const aFieldName: string): boolean;
    procedure  AddField(AField: TEpiField);
    procedure  RemoveField(var AField: TEpiField; DoDestroy: boolean = false);
    function   CreateUniqueFieldName(Const AText: string): string;
    property   Field[Index: integer]: TEpiField read GetField; default;
    property   Fields:      TEpiFields read FFields;
    property   DataFields:  TEpiFields read FDataFields;
    property   QuestFields: TEpiFields read FQuestFields;
    property   ValueLabels: TValueLabelSets read GetValueLabels;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnPassword:  TRequestPasswordEvent read FOnPassword write FOnPassword;
    property   OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property   Options:     TEpiDataFileOptions read FOptions;
    property   FileName:    string read FFileName write FFileName;
    property   FileLabel:   string read FFileLabel write FFileLabel;
    property   Password:    string read FPassword write FPassword;
    property   FieldNaming: TFieldNaming read FFieldNaming write FFieldNaming;
    Property   NumFields:   Cardinal read GetNumFields;
    Property   NumDataFields: Cardinal read GetNumDataFields;
    Property   Size:        Integer read GetSize write SetSize;
    Property   CheckFile:   TEpiCheckFile read FCheckFile;
    Property   IndexFile:   TEpiIndexFile read GetIndexFile;
    Property   ErrorCode:   Cardinal read FErrorCode write FErrorCode;
    Property   ErrorText:   string read FErrorText write FErrorText;
    Property   FileVersion: Cardinal read FFileVersion write FFileVersion;
  end;

implementation

uses
  SysUtils, UStringUtils, Base64, UEpiUtils,
  StrUtils, UCheckFileIO, Math, UDateUtils;

const
  NA_INT       = MaxInt;
  NA_FLOAT     = MaxExtended;
  NA_DATE      = NA_INT;
  NA_TIME      = MaxDouble;
  NA_DATETIME  = NA_TIME;
  NA_STRING    = '.';
  NA_BOOL      = $7F;

  Field_Growth_Factor = 1.20;


{ TEpiCheckField }

function TEpiCheckField.GetMissingValue(Index: integer): string;
begin
  Result := FMissingValues[Index];
end;

procedure TEpiCheckField.SetMissingValue(Index: integer; Value: string);
begin
  FMissingValues[Index] := Value; 
end;

function TEpiCheckField.GetAutoSearch(): Boolean;
begin
  Result := AutoFields <> '';
end;

procedure TEpiCheckField.InternalReset();
begin
  if Assigned(FBeforeCmds)    then FreeAndNil(FBeforeCmds);
  if Assigned(FAfterCmds)     then FreeAndNil(FAfterCmds);
  if Assigned(FFieldComments) then FreeAndNil(FFieldComments);

  FMustEntert        := false;
  FNoEnter           := false;
  FTopOfScreen       := false;
  FTopOfScreenLines  := 0;
  FDoRepeat          := false;
  FConfirm           := false;
  FMin               := '';
  FMax               := '';
  FLegal             := '';
  FDefaultValue      := '';
  FAutoFields        := '';
  FAutoList          := false;
  FJumps             := '';
  FJumpResetChar     := #32;
  // Destruction is ALWAYS HANDLED BY TEpiCheckFile!!!
  FValueLabel        := nil;
  FShowValueLabel    := false;
  FValueLabelIsFieldRef  := false;
  FTypeType           := ttNone;
  FTypeText          := '';
  FTypeColor         := clBlue;
  FHasGlobalDefaultVal := false;
  FieldScope         := scNone;
end;

constructor TEpiCheckField.Create;
begin
  Reset();
end;

destructor TEpiCheckField.Destroy;
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

procedure TEpiCheckField.Clone(var Dest: TEpiCheckField);
var
  TmpCmd: TChkCommands;
  i: Integer;
begin
  if Not Assigned(Dest) then
    Dest := TEpiCheckField.Create();

  Dest.Reset();
  
  Dest.FMustEntert        := FMustEntert;
  Dest.FNoEnter           := FNoEnter;
  Dest.FTopOfScreen       := FTopOfScreen;
  Dest.FTopOfScreenLines  := FTopOfScreenLines;
  Dest.FDoRepeat          := FDoRepeat;
  Dest.FConfirm           := FConfirm;
  Dest.FMin               := FMin;
  Dest.FMax               := FMax;
  Dest.FLegal             := FLegal;
  Dest.FDefaultValue      := FDefaultValue;
  Dest.FAutoFields        := FAutoFields ;
  Dest.FAutoList          := FAutoList;
  Dest.FJumps             := FJumps;
  Dest.FJumpResetChar     := FJumpResetChar;
  Dest.FShowValueLabel    := FShowValueLabel;
  Dest.FValueLabelIsFieldRef := FValueLabelIsFieldRef;
  Dest.FTypeType          := FTypeType;
  Dest.FTypeText          := FTypeText;
  Dest.FTypeColor         := FTypeColor;
  Dest.FHasGlobalDefaultVal := FHasGlobalDefaultVal;
  Dest.FFieldScope        := FFieldScope;

  TmpCmd := Dest.FBeforeCmds;
  FBeforeCmds.Clone(TmpCmd);
  TmpCmd := Dest.FAfterCmds;
  FAfterCmds.Clone(TmpCmd);

  Dest.FFieldComments.Assign(FFieldComments);
  for i := 0 to 2 do
    Dest.FMissingValues[i] := FMissingValues[i];
end;

procedure TEpiCheckField.Reset();
begin
  InternalReset();

  FBeforeCmds := TChkCommands.Create;
  FAfterCmds  := TChkCommands.Create;
  FFieldComments := TStringList.Create();
end;

{ TEpiCheckFile }

constructor TEpiCheckFile.Create;
begin
  Reset();
end;

destructor TEpiCheckFile.Destroy;
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

procedure TEpiCheckFile.Reset();
begin
  InternalReset();

  FValueLabelSets := TValueLabelSets.Create();
  FBackupList     := TStringList.Create;
  FTopComments    := TStringList.Create;
  FDefines        := TEpiFields.Create(nil);
  FDefines.Owned  := True;
end;

function TEpiCheckFile.DefineExists(const aName: string): Boolean;
begin
  result := FDefines.FieldExists(aName);
end;

function TEpiCheckFile.DefineByName(const aName: string): TEpiField;
begin
  result := FDefines.FieldByName(aName);
end;

procedure TEpiCheckFile.AddDefine(Field: TEpiField);
begin
  FDefines.Add(Field);
end;

function TEpiCheckFile.GetGlobMissing(Index: Integer): string;
begin
  Result := FGlobalMissingValues[Index];
end;

procedure TEpiCheckFile.SetGlobMissing(Index: Integer;
  const Value: string);
begin
  FGlobalMissingValues[Index] := Value;
end;

procedure TEpiCheckFile.InternalReset();
begin
  if Assigned(FValueLabelSets)   then FreeAndNil(FValueLabelSets);
  if Assigned(FTopComments)      then FreeAndNil(FTopComments);
  if Assigned(FBeforeFileCmds)   then FreeAndNil(FBeforeFileCmds);
  if Assigned(FAfterFileCmds)    then FreeAndNil(FAfterFileCmds);
  if Assigned(FBeforeRecordCmds) then FreeAndNil(FBeforeRecordCmds);
  if Assigned(FAfterRecordCmds)  then FreeAndNil(FAfterRecordCmds);
  if Assigned(FRecodeCmds)       then FreeAndNil(FRecodeCmds);
  if Assigned(FAssertList)       then FreeAndNil(FAssertList);
  if Assigned(FBackupList)       then FreeAndNil(FBackupList);
  if Assigned(FDefines)          then FreeAndNil(FDefines);

  FConfirm             := false;
  FAutoSave            := false;
  FGlobalDefaultValue  := '';
  FGlobalTypeCom       := false;
  FGlobalTypeComColor  := clBlue;
  FMissingAction       := maIgnoreMissing;
  FShowLastRecord      := false;
  FFieldHighlightAct   := false;
  FFieldHighlightCol   := clBlue;
  FErrorInFile         := false;
  FHasCheckFile        := false;
  FFileName            := '';
end;

{ TEpiField }

function TEpiField.GetValueLabel: TValueLabelSet;
begin
  result := nil;
  if Assigned(CheckField) then
    result := CheckField.ValueLabel;
end;

function TEpiField.GetAsValue(const index: Integer): string;
begin
  result := AsString[Index];
  if Assigned(ValueLabelSet) then
    result := ValueLabelSet.Value[AsString[Index]];
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

constructor TEpiField.Create(ASize: Cardinal; AFieldType: TFieldType);
var
  i: Integer;
begin
  Reset;
  Capacity := ASize;
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

    ftQuestion:
      Result := TEpiField.Create(0, aFieldType);
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

procedure TEpiField.Clone(var Dest: TEpiField);
var
  TmpCheckField: TEpiCheckField;
  I: Integer;  
  Ptr: Pointer;
begin
  if not Assigned(Dest) then
    Dest := TEpiField.CreateField(FieldType, Size);

  // Reset all values... but retain Datafile linkage.
  Ptr := Dest.DataFile;
  Dest.Reset();
  Dest.DataFile := TEpiDataFile(Ptr);

  // Copy Field related values:
  Dest.FDisplayChar := FDisplayChar;
  Dest.FFieldName   := FFieldName;
  Dest.FQuestX      := FQuestX;
  Dest.FQuestY      := FQuestY;
  Dest.FQuestColor  := FQuestColor;
  Dest.FFieldX      := FFieldX;     
  Dest.FFieldY      := FFieldY;
  Dest.FFieldColor  := FFieldColor;
  Dest.FFieldType   := FFieldType;
  Dest.FFieldLength := FFieldLength;
  Dest.FCryptLength := FCryptLength;
  Dest.FNumDecimals := FNumDecimals;
  Dest.FQuestion    := FQuestion;
  Dest.FVariableLabel := FVariableLabel;

  // Copy CheckFile if present:
  TmpCheckField := nil;
  if Assigned(CheckField) then
    CheckField.Clone(TmpCheckField);
  Dest.CheckField := TmpCheckField;

  // Set Valuelabels here since CheckField does not have Owner info.
  // Scenarios:
  // - 1: Dest field has a link to a Datafile. Then any potential valuelabels should be found
  //      through Dest.Datafile's valuelabelsets.
  // - 2: Dest has NO Datafile. This could be a temporary clone, etc.
  //      Copy the Valuelabelset, assign it to Dest field (and posibly also create
  //      the CheckField) and let and TEpiDataFile.AddField handle Valuelabels.
  //      This will reset label type (to vltLocal), since at present it is not known
  //      how the valuelabelset is related to anything else...
  if Assigned(ValueLabelSet) then
  begin
    if Assigned(Dest.DataFile) then
    begin
      Dest.CheckField.FValueLabel := Dest.DataFile.ValueLabels.ValueLabelSetByName(ValueLabelSet.Name)
    end else begin
      If not Assigned(Dest.CheckField) then
        Dest.CheckField := TEpiCheckField.Create();
      ValueLabelSet.Clone(Dest.CheckField.FValueLabel);
      Dest.ValueLabelSet.LabelType := vltLocal;
    end;
  end;

  // Index?
  I := Owner.FDataFile.IndexFile.IndexNoByName(FieldName);
  if (I > 0) and (Assigned(Dest.Owner)) then
  begin
   Dest.Owner.FDataFile.IndexFile.IndexFields[I] := Dest;
   Dest.Owner.FDataFile.IndexFile.IndexUnique[I] := Owner.FDataFile.IndexFile.IndexUnique[I];
  end;
end;

procedure TEpiField.NewRecords(ACount: Integer);
begin
  if ACount <= 0 then exit;
  if (Size + ACount) > Capacity then
    Grow;
  Size := Size + ACount;
end;

procedure TEpiField.Reset();
begin
  if Assigned(FCheckField) then FreeAndNil(FCheckField);

  FOwner         := nil;
  FDataFile      := nil;
  FFieldNo       := 0;
  FDisplayChar   := '_';
  FFieldName     := '';
  FQuestX        := 0;
  FQuestY        := 0;
  FQuestColor    := 0;
  FFieldX        := 0;
  FFieldY        := 0;
  FFieldColor    := 0;
//  FFieldType     := ftInteger;
  FFieldLength   := 0;
  FCryptLength   := 0;
  FNumDecimals   := 0;
  FQuestion      := '';
  FVariableLabel := '';
end;

{ TEpiFields }

function TEpiFields.GetField(Index: integer): TEpiField;
begin
  result := TEpiField(FList[Index]);
end;

function TEpiFields.GetCount(): Cardinal;
begin
  result := FList.Count
end;

constructor TEpiFields.Create(aOwner: TEpiDataFile);
begin
  FList := TList.Create();
  FDataFile := aOwner;
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
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(TEpiField(FList[i]).FieldName, aFieldName) = 0 then
    begin
      result := TEpiField(FList[i]);
      exit;
    end
end;

function TEpiFields.FieldExists(Const aFieldName: string): boolean;
begin
  result := Assigned(FieldByName(aFieldName)); 
end;

procedure TEpiFields.Add(aField: TEpiField);
begin
  if Owned then
  begin
    aField.FOwner := self;
    aField.FDataFile := Self.FDataFile;
  end;
  FList.Add(aField);
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
end;

{ TEpiDataFile }

function TEpiDataFile.GetField(Index: Integer): TEpiField;
begin
  result := Fields[Index];
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

function TEpiDataFile.GetValueLabels: TValueLabelSets;
begin
  result := nil;
  if Assigned(CheckFile) then
    Result := CheckFile.ValueLabels;
end;



procedure TEpiDataFile.InternalReset;
begin
  if Assigned(FFields) then FreeAndNil(FFields);
  if Assigned(FDataFields) then FreeAndNil(FDataFields);
  if Assigned(FQuestFields) then FreeAndNil(FQuestFields);
  if Assigned(FCheckFile) then FreeAndNil(FCheckFile);
  if Assigned(FIndexFile) then FreeAndNil(FIndexFile);
  if Assigned(FCrypter) then FreeAndNil(FCrypter);
  if Assigned(FRecordStatus) then FreeAndNil(FRecordStatus);

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
end;

procedure TEpiDataFile.SetSize(const AValue: Integer);
var
  i, aSize: Integer;
begin
  for i := 0 to DataFields.Count - 1 do
    DataFields[i].Size := AValue;

  aSize := FRecordStatus.Size;
  FRecordStatus.Size := AValue;
end;

function TEpiDataFile.InternalOpen: boolean;
begin
end;

function TEpiDataFile.InternalSave: boolean;
begin
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

function TEpiDataFile.GetNumFields: Cardinal;
begin
  result := Fields.Count;
end;

function TEpiDataFile.GetNumDataFields: Cardinal;
begin
  result := DataFields.Count;
end;

constructor TEpiDataFile.Create(aOptions: TEpiDataFileOptions);
var
  p: pointer;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Create', 3);
  try
    Reset();
    FOptions := aOptions;
    FFieldNaming := fnFirstWord;

    FRecordStatus := TEpiIntField.Create(0, ftInteger);
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

    inherited Destroy;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.Open(const aFileName: string): boolean;
var
  TmpStream: TFileStream;
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Open', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;

    Ext := ExtractFileExt(FileName);
    if AnsiUpperCase(Ext) = '.REC' then
      result := InternalOpen()
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

function TEpiDataFile.Save(Const aFileName: string): boolean;
var
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Save', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;

    Result := InternalSave();
  finally
    EpiLogger.DecIndent;
  end;
end;

{
procedure TEpiDataFile.SaveMemToFile(Const aFileName: string);
begin
  if Trim(aFileName) = '' then exit;

  if not (FDataStream is TMemoryStream) then exit;

  TMemoryStream(FDataStream).SaveToFile(aFileName); 
end;

procedure TEpiDataFile.Read(RecNumber: Integer);
var
  BufPos, I: integer;
  CharBuf: Array of char;
  StrBuf, EncData: string;
  ok: boolean;
  TmpStr: String;
begin
  // Sanity checks:
  if (RecNumber < 1) or (RecNumber > NumRecords) then
  begin
    FErrorText := Lang(0, 'Index error in reading.');
    FErrorCode := EPI_READ_FILE_ERROR;
    EpiLogger.AddError(Classname, 'Read', ErrorText, 0);
    raise Exception.Create('Index error in reading.');
  end;

  if FDataStream.Position <> (FOffset + ((RecNumber - 1) * FFullRecLength)) then
    FDataStream.Position := FOffset + ((RecNumber - 1) * FFullRecLength);

  SetLength(CharBuf, FFullRecLength);
  I := FDataStream.Read(CharBuf[0], FFullRecLength);
  if I <> FFullRecLength then
  begin
    FErrorText := Lang(20464, 'Error reading record');
    FErrorCode := EPI_READ_FILE_ERROR;
    EpiLogger.AddError(Classname, 'Open', ErrorText, 20464);
    raise Exception.Create('Error reading record');
  end;

  RecordState := rsNormal;
  StrBuf := CharBuf[High(CharBuf) - 2];
  if StrBuf = '?' then
    RecordState := rsDeleted
  else if StrBuf = '^' then
    RecordState := rsVerified;

  StrBuf := StringReplace(string(CharBuf), EOLChars, '', [rfReplaceAll]);

  BufPos := 1;
  for i := 0 TO DataFields.Count - 1 DO
  with DataFields[i] do begin
    TmpStr := Trim(Copy(StrBuf, BufPos, FieldLength));
    IF (fieldtype = ftCrypt) AND (FPassword <> '') THEN
    begin
      EncData := B64Decode(TmpStr);
      FCrypter.DecryptCFB(EncData[1], EncData[1], Length(EncData));
      TmpStr := Trim(EncData);
      FCrypter.Reset;
    end;
    AsString[RecNumber] := TmpStr;
    Inc(BufPos, FieldLength);
  end;

  FCurRecord := RecNumber;
end;

procedure TEpiDataFile.Write(RecNumber: Integer = NewRecord);
var
  Z, I: Integer;
  S, EncData: string;
  T: String;

begin
  // Sanity checks:
  if (RecNumber < 1) or (RecNumber > NumRecords) then
    RecNumber := NewRecord;

  if EOFMarker then Z := 1 else Z := 0;

  if RecNumber = NewRecord then
  begin
    RecNumber := ((FDataStream.Size - Z) - FOffSet) div FFullrecLength + 1;
    FDataStream.Position := FDataStream.Size - Z
  end else
    FDataStream.Position := FOffset + ((RecNumber - 1) * FFullRecLength);

  // TODO -O Torsten : Index on new record?.
  S := '';
  for i := 0 TO DataFields.Count - 1 DO
  with DataFields[i] do begin
    if FieldType = ftCrypt then
    begin
      EncData := Trim(AsString[RecNumber]);
      FCrypter.InitStr(Password);
      FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
      EncData := B64Encode(EncData);
      FCrypter.Reset;
      T := Format('%-*s', [FieldLength, EncData])
    end else if FieldType in [ftString, ftUpperAlfa] then
      T := Format('%-*s', [FieldLength, AsString[RecNumber]])
    else
      T := Format('%*s', [FieldLength, AsString[RecNumber]]);

    S := S + T;
    if FieldLength <> Length(T) then
    begin
      ErrorCode := EPI_WRITE_ERROR;
      ErrorText := Format(Lang(0, 'FieldLength (%d) does not fit length of data (%d). Field: %s. Record no: %d'),
        [FieldLength, Length(T), FieldName, RecNumber]);
      Abort;
    end;
  end;
  Z := Length(S);
  if Z + 3 > MaxRecLineLength then
    for I := (Z div MaxRecLineLength) downto 1 do
      Insert(EOLchars, S, (MaxRecLineLength * I) + 1);

  case RecordState of
    rsNormal:   S := S + EOLchars;
    rsDeleted:  S := S + '?' + #13#10;
    rsVerified: S := S + '^' + #13#10;
  end;

  FDataStream.Write(S[1], Length(S));

  FCurRecord := RecNumber;
end; }

procedure TEpiDataFile.Reset;
begin
  InternalReset();

  FFields       := TEpiFields.Create(Self);
  FFields.Owned := True;
  FDataFields   := TEpiFields.Create(Self);
  FQuestFields  := TEpiFields.Create(Self);
  FCheckFile    := TEpiCheckFile.Create();
  FCrypter      := TDCP_rijndael.Create(nil);
end;

function TEpiDataFile.FieldByName(Const aFieldName: string): TEpiField;
begin
  result := Fields.FieldByName(aFieldName);
end;

function TEpiDataFile.FieldExists(Const aFieldName: string): boolean;
begin
  result := Fields.FieldExists(aFieldName);
end;

procedure TEpiDataFile.AddField(AField: TEpiField);
begin
  Fields.Add(AField);
  if AField.FieldType = ftQuestion then
    QuestFields.Add(AField)
  else
    DataFields.Add(AField);

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

  if AField.FieldType = ftQuestion then
    QuestFields.Delete(AField)
  else
    DataFields.Delete(AField);

  Fields.Delete(AField);
  if DoDestroy then
    FreeAndNil(AField);
end;

function TEpiDataFile.CreateUniqueFieldName(const AText: string): string;
var
  Number: Integer;
begin
  result := AText;

  // If fieldname is unique, do nothing.
  Number := 0;
  while FieldExists(result) do
  begin
    // not unique, find a new.
    if Length(Result) = MaxFieldNameLen then
      Result := Copy(Result, 1, MaxFieldNameLen - Length(IntToStr(Number))) + IntToStr(Number)
    else
      Result := Result + IntToStr(Number);
    Inc(Number);
  end;
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


function TEpiIndexFile.GetIndexCount(): Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) then Inc(Result);
end;

procedure TEpiIndexFile.InternalReset();
begin
  if Assigned(FIndexFields) then FreeAndNil(FIndexFields);
  FFileName := '';
  FIndexUnique := 0;
end;

constructor TEpiIndexFile.Create(const aFileName: string);
begin
  Reset();
  FFileName := aFileName;
end;

destructor TEpiIndexFile.Destroy;
begin
  InternalReset();
  inherited;
end;

procedure TEpiIndexFile.Reset();
var
  i: integer;
begin
  InternalReset();
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
  result := FData[Index - 1];
end;

function TEpiIntField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := IntToStr(AsInteger[Index]);
end;

function TEpiIntField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiIntField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsInteger[Index] = DefaultMissing;
end;

function TEpiIntField.GetIsMissingValue(const index: Integer): boolean;
begin
  // TODO : TEpiIntField.GetIsMissingValue
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
    AsInteger[Index] := Trunc(AValue);
end;

procedure TEpiIntField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
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

function TEpiFloatField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiFloatField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsFloat[index] = DefaultMissing;
end;

function TEpiFloatField.GetIsMissingValue(const index: Integer): boolean;
begin
  // TODO : Implement TEpiFloatField.GetIsMissingValue
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
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := StrToFloatDef(AValue, DefaultMissing);
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

function TEpiBoolField.GetAsBoolean(const index: Integer): EpiBool;
begin
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

function TEpiBoolField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiBoolField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsBoolean[index] = DefaultMissing;
end;

function TEpiBoolField.GetIsMissingValue(const index: Integer): boolean;
begin
  // TODO : Implement TEpiBoolField.GetIsMissingValue
end;

function TEpiBoolField.GetSize: Integer;
begin
  result := Length(FData);
end;

procedure TEpiBoolField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  FData[index - 1] := AValue;
end;

procedure TEpiBoolField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue >= 1 then
    AsFloat[Index] := 1
  else
    AsFloat[Index] := 0;
end;

procedure TEpiBoolField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue >= 1 then
    AsFloat[Index] := 1
  else
    AsFloat[Index] := 0;
end;

procedure TEpiBoolField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue >= 1 then
    AsFloat[Index] := 1
  else
    AsFloat[Index] := 0;
end;

procedure TEpiBoolField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if Trim(AValue) <> '' then
    AsFloat[Index] := 1
  else
    AsFloat[Index] := 0;
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
  result := FData[index - 1];
end;

function TEpiStringField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiStringField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsString[index] = DefaultMissing;
end;

function TEpiStringField.GetIsMissingValue(const index: Integer): boolean;
begin
  // TODO : Implement TEpiStringField.GetIsMissingValue
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
  FData[index -1] := AValue;
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

function TEpiDateField.GetCapacity: Integer;
begin
  result := Length(FData);
end;

function TEpiDateField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsDate[index] = DefaultMissing;
end;

function TEpiDateField.GetIsMissingValue(const index: Integer): boolean;
begin
  // TODO : Implement TEpiDateField.GetIsMissingValue
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
