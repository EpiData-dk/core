{%MainUnit epidatafile.pas}

type


  { TEpiDataFiles }

  TEpiDataFiles = class(TEpiCustomClass)
  private
    FList: TFPList;
    FOwned: Boolean;
    function GetCount: Integer;
    function GetDataFile(Index: integer): TEpiDataFile;
    function GetSettings: TEpiSettings;
  protected
    Property    Settings: TEpiSettings read GetSettings;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    function    NewDatafile: TEpiDatafile;
    procedure   Add(aDataFile: TEpiDataFile);
    procedure   Delete(aDataFile: TEpiDataFile);
    function    DataFileById(Const aId: string): TEpiDataFile;
    function    DataFileExists(Const aId: string): boolean;
    function    IndexOf(Const aId: string): integer;
    procedure   SaveToStream(St: TStream; Lvl: Integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    Property    DataFile[Index: integer]: TEpiDataFile read GetDataFile; default;
    Property    Count: Integer read GetCount;
    Property    Owned: Boolean read FOwned write FOwned;
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

  { TEpiDataFile }

  TEpiDataFile = class(TEpiCustomClass)
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
    FOnPassword:   TOldRequestPasswordEvent;   // Callback event for password request
    FBackgroundColour:     Integer;         // Background color up entry form.
    FFileVersion:  Cardinal;
    FRecordStatus: TEpiField;
    function   GetDeleted(Index: integer): boolean;
    function   GetField(Index: integer): TEpiField;
    function   GetSize: Integer;
    function   GetTextLabel(Index: integer): TEpiTextLabel;
    function   GetTextLabelCount: Cardinal;
    function   GetVerified(Index: integer): boolean;
    procedure  InternalReset;
    procedure  SetDeleted(Index: integer; const AValue: boolean);
    procedure  SetFileLabel(const AValue: string);
    procedure  SetFileName(const AValue: string);
    procedure  SetSize(const AValue: Integer);
    procedure  SetStudy(const AValue: string);
    procedure  SetVerified(Index: integer; const AValue: boolean);
    procedure  SetId(const AValue: string);
  private
    FId: string;
  protected
    function   InternalOpenOld: boolean;
    function   InternalSaveOld: boolean;
    function   Lang(LangCode: Integer; Const LangText: string): string;
    Function   UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
    function   GetFieldCount: Cardinal;
    Procedure  SaveRecords(St: TStream; Lvl: Integer); virtual;
    Procedure  LoadRecords(Root: TDOMNode); virtual;
    procedure  DoChange(Event: TEpiDataFileChangeEventType; OldValue: Pointer);
  public
    constructor Create(AOwner: TObject; ASize: Cardinal = 0); virtual;
    destructor Destroy; override;
    function   PrepareDataFile(FieldNames: TStrings): TEpiDataFile;
    function   Clone(CloneData: boolean = true): TEpiDataFile;
    procedure  SaveToStream(St: TStream; Lvl: Integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  Reset;
    function   NewField(FieldType: TFieldType): TEpiField;
    procedure  AddField(AField: TEpiField);
    procedure  RemoveField(var AField: TEpiField; DoDestroy: boolean = false);
    function   FieldByName(Const aFieldName: string): TEpiField;
    function   FieldById(Const aId: string): TEpiField;
    function   FieldExists(Const aFieldName: string): boolean;
    function   FieldIndex(Const aFieldName: string): Integer;
    function   CreateUniqueFieldName(Const AText: string): string;
    function   NewTextLabel: TEpiTextLabel;
    procedure  AddTextLabel(ATextLabel: TEpiTextLabel);
    procedure  RemoveTextLabel(var ATextLabel: TEpiTextLabel; DoDestroy: boolean = false);
    function   TextLabelById(Const ATextLabelId: string): TEpiTextLabel;
    function   TextLabelExists(Const ATextLabelId: string): boolean;
    procedure  NewRecords(ACount: Integer = 1); virtual;
    procedure  SortFields(Cmp: TListSortCompare);
    function   DocumentDatafile: TStrings;
    Property   Id: string read FId write SetId;
    property   Field[Index: integer]: TEpiField read GetField; default;
    property   Fields:      TEpiFields read FFields;
    property   TextLabel[Index: integer]: TEpiTextLabel read GetTextLabel;
    property   TextLabels:  TEpiTextLabels read FTextLabels;
    property   ValueLabels: TValueLabelSets read FValueLabels write FValueLabels;
    Property   ScreenProperties: TEpiScreenProperties read FScreenProperties;
    property   Deleted[Index: integer]: boolean read GetDeleted write SetDeleted;
    property   Verified[Index: integer]: boolean read GetVerified write SetVerified;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnPassword:  TOldRequestPasswordEvent read FOnPassword write FOnPassword;
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

