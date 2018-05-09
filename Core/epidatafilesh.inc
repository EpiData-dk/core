{%MainUnit epidatafiles.pas}

type

  TEpiDataFileChangeEventType = (
    edceSize,                      // Size of datafile changed
    edceRecordStatus,              // Status of record changed
    edceStatusbarContentString,    // Statusbar Content string changed
    edcePack,                      // A pack procedure is starting: Data = Array of recnums to be deleted.
    edceBeginCommit,               // Sent (by user) before a commit is initiated. Data: 0 = new records, 1 = edited record.
    edceEndCommit,                 // Sent (by user) when a commit is completed. Data = index of record edited.
    edceLoadRecord                 // Sent (by user) when loading a record. Data = index of record viewed
  );

  TEpiDataFilesEnumerator = class;

  { TEpiDataFiles }

  TEpiDataFiles = class(TEpiCustomList)
  private
    function    GetDataFile(Index: integer): TEpiDataFile;
    function    GetSettings: TEpiXMLSettings;
  protected
    function    Prefix: string; override;
    function NewItemLoad(const AName: EpiString;
      AItemClass: TEpiCustomItemClass = nil): TEpiCustomItem; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function    ItemClass: TEpiCustomItemClass; override;
    function    XMLName: string; override;
    function    NewDataFile: TEpiDataFile;
    function    GetEnumerator: TEpiDataFilesEnumerator;
    function    GetDataFileByName(Const AName: string): TEpiDataFile;
    Property    DataFile[Index: integer]: TEpiDataFile read GetDataFile; default;
  end;

  EEpiDataFileHaltLoading = class(Exception);

  TEpiDataFileStatusRecord = record
    Index:    Integer;            // The index at which the record change happened.
    OldValue: TEpiRecordState;    // Record state prior to the change.
    NewValue: TEpiRecordState;    // Record state after the change.
  end;
  PEpiDataFileStatusRecord = ^TEpiDataFileStatusRecord;

  TEpiDataFileAfterRecordState = (
    arsNewRecord,             // If possible create a new record, else return to parent.
    arsReturnToParent,        // Return directly to parent aften record
    arsReturnToParentOnMax,   // Return to parent when max no. records reached, else start new record
    arsStayOnRecord           // Stay on record.
    );


  TEpiDataFilePackFunction = function(Sender: TEpiDataFile; Index: Integer): boolean of object;

  { TEpiDataFile }

  TEpiDataFile = class(TEpiCustomItem)
  { Data containers }
  private
    FAllFields: TEpiFields;
    FAllHeadings: TEpiHeadings;
    FAllControlItems: TEpiCustomControlItemList;
    FSections: TEpiSections;
    FMainSection: TEpiSection;
    function    GetField(Index: integer): TEpiField;
    function    GetSection(Index: integer): TEpiSection;
    function    GetHeading(Index: integer): TEpiHeading;
    function    GetValueLabelSet(Index: integer): TEpiValueLabelSet;

  { Private housekeeping }
  private
    FRecordStatus: TEpiField;
    function    GetControlItem(Index: integer): TEpiCustomControlItem;
    function    GetDeleted(Index: integer): boolean;
    function    GetDeletedCount: Integer;
    function    GetValueLabelSets: TEpiValueLabelSets;
    function    GetVerified(Index: integer): boolean;
    function    GetSize: Integer;
    function    GetVerifiedCount: Integer;
    procedure   LocalEventHook(Const Sender: TEpiCustomBase;
       Const Initiator: TEpiCustomBase; EventGrp: TEpiEventGroup;
       EventType: Word; Data: pointer);
    procedure   SetDeleted(Index: integer; const AValue: boolean);
    procedure   SetSize(const AValue: Integer);
    procedure   SetVerified(Index: integer; const AValue: boolean);
    function    InternalCompareRecords(Const SortList: TEpiFields; Const Idx1, Idx2: Integer; Desceding: Boolean): integer;
    procedure   InternalSortRecords(Const SortList: TEpiFields; L, R: Integer; Descending: Boolean; ObsNoField: TEpiField);
    function    InternalPackFunction(Sender: TEpiDataFile; Index: Integer): boolean;

  { Load/Save methods }
  private
    FLoadingRecs: Boolean;
    procedure   HaltLoading;
    procedure   LoadRecords(Root: TDOMNode);
    procedure   LoadKeyFields(Root: TDOMNode);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    procedure   SetModified(const AValue: Boolean); override;
    property    IsLoadingRecords: Boolean read FLoadingRecs;
  protected
    procedure   SaveKeyFieldsToDom(RootNode: TDOMElement);
    procedure   SaveRecordToDom(RootNode: TDOMElement);
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); virtual;
    destructor  Destroy; override;
    function    ValidateRename(Const NewName: string): boolean; overload;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField; virtual;
    function    NewSection: TEpiSection;
    function    NewHeading: TEpiHeading;
    // Returns the index to the "first" of the newly created records.
    function    NewRecords(Const Count: Cardinal = 1): integer; virtual;
    procedure   Pack; overload;   // Removes records marked for deletion.
    procedure   Pack(PackFunction: TEpiDataFilePackFunction); overload; // Removes records based on provided method
    procedure   ExchangeRecords(Const Index1, Index2: Integer);
    // Sorts records based on value in SortFields. If ObsNoField is assigned (must be int field) then
    // this vector is used to index into the observations sorted. Hence it is possible to sort
    // only parts of the entire Datafile.
    procedure   SortRecords(SortFields: TEpiFields; Descending: Boolean = false; ObsNoField: TEpiField = nil); overload;
    procedure   SortRecords(SortField: TEpiField); overload;
    property    MainSection: TEpiSection read FMainSection;
    property    Sections: TEpiSections read FSections;
    property    Section[Index: integer]: TEpiSection read GetSection;
    property    Fields: TEpiFields read FAllFields;
    property    Field[Index: integer]: TEpiField read GetField; default;
    property    ControlItems: TEpiCustomControlItemList read FAllControlItems;
    property    ControlItem[Index: integer]: TEpiCustomControlItem read GetControlItem;
    property    Headings: TEpiHeadings read FAllHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading;
    property    ValueLabels: TEpiValueLabelSets read GetValueLabelSets;
    property    ValueLabelSet[Index: integer]: TEpiValueLabelSet read GetValueLabelSet;
    Property    Size: Integer read GetSize write SetSize;
    property    DeletedCount: Integer read GetDeletedCount;
    property    VerifiedCount: Integer read GetVerifiedCount;
    property    Verified[Index: integer]: boolean read GetVerified write SetVerified;
    property    Deleted[Index: integer]: boolean read GetDeleted write SetDeleted;

  { Record load/commit: Logging related }
  public
    procedure  BeginCommitRecord(NewRecord: boolean = true);
    procedure  EndCommitRecord(RecordNo: Integer);
    // At the moment LoadRecord does nothing other than sending an event throught the
    // document, notifying listeners that RecordNo is currently being viewed.
    // There is no check for correct index, filtered dataset, etc.
    // Currently used for logging.
    procedure  LoadRecord(RecordNo: Integer);

  { DataFile properties }
  private
    FAfterRecordState: TEpiDataFileAfterRecordState;
    FCaption:   TEpiTranslatedTextWrapper;
    FCreated:   TDateTime;
    FGroupRights: TEpiGroupRights;
    FKeyFields: TEpiFields;
    FRecModified: boolean;
    FRecModifiedDate: TDateTime;
    FRelates: TEpiRelates;
    FNotes:     TEpiTranslatedTextWrapper;
    FStructureModified: boolean;
    FStructureModifiedDate: TDateTime;
    FVersion:   string;
    FStatusbarContentString: string;
    procedure   SetVersion(const AValue: string);
    procedure   SetStatusbarContentString(AValue: string);
  public
    property    AfterRecordState: TEpiDataFileAfterRecordState read FAfterRecordState write FAfterRecordState;
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    Created: TDateTime read FCreated;
    property    KeyFields: TEpiFields read FKeyFields;
    property    Notes: TEpiTranslatedTextWrapper read FNotes;
    property    RecModifiedDate: TDateTime read FRecModifiedDate write FRecModifiedDate;  // version 2
    property    Relates: TEpiRelates read FRelates;                                       // version 3
    property    GroupRights: TEpiGroupRights read FGroupRights;                           // version 4
    property    StructureModifiedDate: TDateTime read FStructureModifiedDate write FStructureModifiedDate;
    property    Version: string read FVersion write SetVersion;
    property    StatusbarContentString: string read FStatusbarContentString write SetStatusbarContentString;

  { Update Overrides}
  private
    procedure DoControlChange(Const Initiator: TEpiCustomBase;
       EventType: TEpiCustomChangeEventType; Data: TEpiCustomControlItem);
    procedure DoModifiedChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  protected
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;

  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;
  TEpiDataFileClass = class of TEpiDataFile;

  { TEpiDataFilesEnumeraton }

  TEpiDataFilesEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiDataFile; override;
  public
    property Current: TEpiDataFile read GetCurrent;
  end;

