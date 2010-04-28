{%MainUnit epidatafiles.pas}

type

  TEpiDataFiles = class(TEpiCustomList)
  private
    function    GetDataFile(Index: integer): TEpiDataFile;
    function    GetSettings: TEpiSettings;
  protected
    Property    Settings: TEpiSettings read GetSettings;
    function    ScrambleXml: boolean; override;
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

  TEpiDataFile = class(TEpiCustomNamedItem)
  private
    { Data containers }
    FAllFields: TEpiFields;
    FAllHeadings: TEpiHeadings;
    FSections: TEpiSections;
    FMainSection: TEpiSection;
    FValueLabels: TEpiValueLabelSets;
    function GetField(Index: integer): TEpiField;
    function GetSection(Index: integer): TEpiSection;
    function GetHeading(Index: integer): TEpiHeading;
    function GetValueLabelSet(Index: integer): TEpiValueLabelSet;
  private
    { Private housekeeping }
    FRecordStatus: TEpiField;
    function    GetDeleted(Index: integer): boolean;
    function    GetVerified(Index: integer): boolean;
    function    GetSize: Integer;
    procedure   LocalEventHook(Sender: TObject; EventGrp: TEpiEventGroup; EventType: Word; Data: pointer);
    function    SaveRecords(Lvl: integer): string;
    procedure   LoadRecords(Root: TDOMNode);
    procedure   SetDeleted(Index: integer; const AValue: boolean);
    procedure   SetSize(const AValue: Integer);
    procedure   SetVerified(Index: integer; const AValue: boolean);
  protected
    class function IdString: string; override;
    constructor Create(AOwner: TEpiCustomBase); override;
  public
    constructor Create(AOwner: TEpiCustomBase; const Size: integer = 0);
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewSection: TEpiSection;
    function    NewHeading: TEpiHeading;
    property    MainSection: TEpiSection read FMainSection;
    property    Sections: TEpiSections read FSections;
    property    Section[Index: integer]: TEpiSection read GetSection;
    property    Fields: TEpiFields read FAllFields;
    property    Field[Index: integer]: TEpiField read GetField; default;
    property    Headings: TEpiHeadings read FAllHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading;
    property    ValueLabels: TEpiValueLabelSets read FValueLabels;
    property    ValueLabelSet[Index: integer]: TEpiValueLabelSet read GetValueLabelSet;
    Property    Size: Integer read GetSize write SetSize;
    property    Verified[Index: integer]: boolean read GetVerified write SetVerified;
    property    Deleted[Index: integer]: boolean read GetDeleted write SetDeleted;
  public
    { "Publiclised" inherited properties }
    property    Name;
    property    Id;
  end;
