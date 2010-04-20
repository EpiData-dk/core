{%MainUnit epidatafiles.pas}

type

  { TEpiSections }

  TEpiSections = class(TEpiCustomList)
  private
    function GetSection(Index: integer): TEpiSection;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    class function XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewSection: TEpiSection;
    property    Section[Index: integer]: TEpiSection read GetSection; default;
  end;

  { TEpiSection }

  TEpiSection = class(TEpiCustomControlItem)
  private
    FFields: TEpiFields;
    FGroups: TEpiGroups;
    FHeadings: TEpiHeadings;
    function GetField(Index: integer): TEpiField;
    function GetHeading(Index: integer): TEpiHeading;
  protected
    class function IdString: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    class function XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewHeading: TEpiHeading;
    property    Fields: TEpiFields read FFields;
    property    Field[Index: integer]: TEpiField read GetField;
    property    Headings: TEpiHeadings read FHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading;
    property    Groups: TEpiGroups read FGroups;
  public
    { "Publiclised" inherited properties }
    property    Name;
    property    Id;
    property    Top;
    property    Left;
  end;

