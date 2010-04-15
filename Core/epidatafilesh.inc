{%MainUnit epidatafiles.pas}

type

  { TEpiDataFiles }

  TEpiDataFiles = class(TEpiCustomList)
  private
    function    GetDataFile(Index: integer): TEpiDataFile;
    function    GetSettings: TEpiSettings;
  protected
    Property    Settings: TEpiSettings read GetSettings;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewDataFile: TEpiDataFile;
    Property    DataFile[Index: integer]: TEpiDataFile read GetDataFile; default;
  end;

  TEpiDataFile = class(TEpiCustomItem)
  private
    { Data containers }
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
  private
    { Properties }
  public
    constructor Create(AOwner: TEpiCustomBase; const Size: integer = 0);
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    property    Sections: TEpiSections read FSections;
    property    Section[Index: integer]: TEpiSection read GetSection;
    property    MainSection: TEpiSection read FMainSection;
{    property    Fields: TEpiFields read FFields;
    property    Field[Index: integer]: TEpiField read GetField;
    property    Headings: TEpiHeadings read FHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading; }
    property    ValueLabels: TEpiValueLabelSets read FValueLabels;
    property    ValueLabelSet[Index: integer]: TEpiValueLabelSet read GetValueLabelSet;
  end;
