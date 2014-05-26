{%MainUnit epidatafiles.pas}

type

  TEpiDataFileChangeEventType = (
    edceSize
  );

  { TEpiDataFiles }

  TEpiDataFiles = class(TEpiCustomList)
  private
    function    GetDataFile(Index: integer): TEpiDataFile;
    function    GetSettings: TEpiXMLSettings;
  protected
    Property    Settings: TEpiXMLSettings read GetSettings;
    function    ScrambleXml: boolean; override;
    function    Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    ItemClass: TEpiCustomItemClass; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewDataFile: TEpiDataFile;
    Property    DataFile[Index: integer]: TEpiDataFile read GetDataFile; default;
  end;

  EEpiDataFileHaltLoading = class(Exception);

  { TEpiDataFile }

  TEpiDataFile = class(TEpiCustomItem)
  private
    { Data containers }
    FAllFields: TEpiFields;
    FAllHeadings: TEpiHeadings;
    FAllControlItems: TEpiCustomControlItemList;
    FSections: TEpiSections;
    FMainSection: TEpiSection;
    function    GetField(Index: integer): TEpiField;
    function    GetSection(Index: integer): TEpiSection;
    function    GetHeading(Index: integer): TEpiHeading;
    function    GetValueLabelSet(Index: integer): TEpiValueLabelSet;
  private
    { Private housekeeping }
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
    function    InternalCompareRecords(Const SortList: TEpiFields; Const Idx1, Idx2: Integer): integer;
    procedure   InternalSortRecords(Const SortList: TEpiFields; L, R: Integer);
  { Load/Save methods }
  private
    FLoading:   boolean;
    procedure   HaltLoading;
    function    SaveRecords(Lvl: integer): string;
    procedure   LoadRecords(Root: TDOMNode);
    function    SaveKeyFields(Lvl: integer): string;
    procedure   LoadKeyFields(Root: TDOMNode);
  public
    property    Loading: boolean read FLoading;
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    procedure   SetModified(const AValue: Boolean); override;
  protected
    procedure   SaveKeyFieldsToDom(RootNode: TDOMElement);
    procedure   SaveRecordToDom(RootNode: TDOMElement);
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase; const Size: integer = 0);
    destructor  Destroy; override;
    function    ValidateRename(Const NewName: string): boolean;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    SaveAttributesToXml: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewSection: TEpiSection;
    function    NewHeading: TEpiHeading;
    procedure   NewRecords(Const Count: Cardinal = 1);
    procedure   Pack; // Removes records marked for deletion.
    procedure   ExchangeRecords(Const Index1, Index2: Integer);
    procedure   SortRecords(SortFields: TEpiFields); overload;
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

  { DataFile properties }
  private
    FCaption:   TEpiTranslatedTextWrapper;
    FCreated:   TDateTime;
    FRecModified: boolean;
    FRecModifiedDate: TDateTime;
    FStructureModified: boolean;
    FStructureModifiedDate: TDateTime;
    FKeyFields: TEpiFields;
    FNotes:     TEpiTranslatedText;
    FVersion:   string;
    procedure   SetVersion(const AValue: string);
  public
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    Created: TDateTime read FCreated;
    property    RecModifiedDate: TDateTime read FRecModifiedDate write FRecModifiedDate;  // version 2
    property    StructureModifiedDate: TDateTime read FStructureModifiedDate write FStructureModifiedDate;
    property    Version: string read FVersion write SetVersion;
    property    Notes: TEpiTranslatedText read FNotes;
    property    KeyFields: TEpiFields read FKeyFields;

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
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  end;
