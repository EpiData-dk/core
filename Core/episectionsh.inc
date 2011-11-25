{%MainUnit epidatafiles.pas}

type

  TEpiSectionsChangeEventType = (
    esceWidth, esceHeight
  );

  { TEpiSections }

  TEpiSections = class(TEpiCustomControlItemList)
  private
    function GetSection(Index: integer): TEpiSection;
  protected
    function Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewSection: TEpiSection;
    function    GetSectionByName(Const AName: string): TEpiSection;
    property    Section[Index: integer]: TEpiSection read GetSection; default;
  end;

  { TEpiSection }

  TEpiSection = class(TEpiCustomControlItem)
  private
    FDataFile: TEpiDataFile;
    FFields: TEpiFields;
    FGroups: TEpiGroups;
    FHeadings: TEpiHeadings;
    FHeight: integer;
    FCaption: TEpiTranslatedTextWrapper;
    FWidth: integer;
    function    GetField(Index: integer): TEpiField;
    function    GetHeading(Index: integer): TEpiHeading;
    procedure   SetHeight(const AValue: integer);
    procedure   SetWidth(const AValue: integer);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    SaveAttributesToXml: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewHeading: TEpiHeading;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    Fields: TEpiFields read FFields;
    property    Field[Index: integer]: TEpiField read GetField;
    property    Headings: TEpiHeadings read FHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading;
    property    Groups: TEpiGroups read FGroups;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Width: integer read FWidth write SetWidth;
    property    Height: integer read FHeight write SetHeight;
  end;

