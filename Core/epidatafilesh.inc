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
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewDataFile: TEpiDataFile;
    Property    DataFile[Index: integer]: TEpiDataFile read GetDataFile; default;
  end;

  { TEpiDataFile }

  TEpiDataFile = class(TEpiCustomItem)
  private
    { Data containers }
    FAllFields: TEpiFields;
    FAllHeadings: TEpiHeadings;
    FSections: TEpiSections;
    FMainSection: TEpiSection;
    function    GetField(Index: integer): TEpiField;
    function    GetSection(Index: integer): TEpiSection;
    function    GetHeading(Index: integer): TEpiHeading;
    function    GetValueLabelSet(Index: integer): TEpiValueLabelSet;
  private
    { Private housekeeping }
    FRecordStatus: TEpiField;
    function    GetDeleted(Index: integer): boolean;
    function    GetDeletedCount: Integer;
    function    GetValueLabelSets: TEpiValueLabelSets;
    function    GetVerified(Index: integer): boolean;
    function    GetSize: Integer;
    function    GetVerifiedCount: Integer;
    procedure   LocalEventHook(Sender: TObject; EventGrp: TEpiEventGroup; EventType: Word; Data: pointer);
    function    SaveRecords(Lvl: integer): string;
    procedure   LoadRecords(Root: TDOMNode);
    function    SaveKeyFields(Lvl: integer): string;
    procedure   LoadKeyFields(Root: TDOMNode);
    procedure   SetDeleted(Index: integer; const AValue: boolean);
    procedure   SetSize(const AValue: Integer);
    procedure   SetVerified(Index: integer; const AValue: boolean);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
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
    property    MainSection: TEpiSection read FMainSection;
    property    Sections: TEpiSections read FSections;
    property    Section[Index: integer]: TEpiSection read GetSection;
    property    Fields: TEpiFields read FAllFields;
    property    Field[Index: integer]: TEpiField read GetField; default;
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
    FKeyFields: TEpiFields;
    FNotes:     TEpiTranslatedText;
    FVersion:   string;
    procedure   SetVersion(const AValue: string);
  public
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    Created: TDateTime read FCreated;
    property    Version: string read FVersion write SetVersion;
    property    Notes: TEpiTranslatedText read FNotes;
    property    KeyFields: TEpiFields read FKeyFields;

  { Update Overrides}
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;

  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  end;
