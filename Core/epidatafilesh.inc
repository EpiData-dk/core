{%MainUnit epidatafiles.pas}

type

  { TEpiDataFiles }

//  TEpiDataFilesChangeEventType = (
//      edfce
//    );

  TEpiDataFiles = class(TEpiCustomList)
  private
    function    GetDataFile(Index: integer): TEpiDataFile;
    function    GetSettings: TEpiSettings;
  protected
    Property    Settings: TEpiSettings read GetSettings;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    class function XMLName: string; override;
    function    ScrambleXml: boolean; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewDataFile: TEpiDataFile;
    Property    DataFile[Index: integer]: TEpiDataFile read GetDataFile; default;
  end;

  TEpiDataFile = class(TEpiCustomItem)
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
    procedure   LocalEventHook(Sender: TObject; EventGrp: TEpiEventGroup; EventType: Word; Data: pointer);
    function    GetSize: Integer;
    procedure   SetSize(const AValue: Integer);
    function    SaveRecords(Lvl: integer): string;
  public
    constructor Create(AOwner: TEpiCustomBase; const Size: integer = 0);
    destructor  Destroy; override;
    class function XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewSection: TEpiSection;
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
  public
    { "Publiclised" inherited properties }
    property    Name;
    property    Id;
  end;
