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
    function    ItemClass: TEpiCustomItemClass; override;
    function    NewSection: TEpiSection;
    function    GetSectionByName(Const AName: string): TEpiSection;
    function    GetUniqueItemName(AClass: TEpiCustomItemClass): string; override;
    property    Section[Index: integer]: TEpiSection read GetSection; default;
  end;

  { TEpiSection }

  TEpiSection = class(TEpiCustomControlItem)
  private
    FDataFile: TEpiDataFile;
    FFields: TEpiFields;
    FGroupRights: TEpiGroupRights;
    FHeadings: TEpiHeadings;
    FHeight: integer;
    FCaption: TEpiTranslatedTextWrapper;
    FWidth: integer;
    function    GetField(Index: integer): TEpiField;
    function    GetHeading(Index: integer): TEpiHeading;
    procedure   SetHeight(const AValue: integer);
    procedure   SetWidth(const AValue: integer);
  private
    FAllControls: TEpiCustomControlItemList;  // Stores a local list of all fields/headings in sorted order.
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewHeading: TEpiHeading;
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    Fields: TEpiFields read FFields;
    property    Field[Index: integer]: TEpiField read GetField;
    property    Headings: TEpiHeadings read FHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading;
    property    GroupRights: TEpiGroupRights read FGroupRights;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Width: integer read FWidth write SetWidth;
    property    Height: integer read FHeight write SetHeight;
    property    AllControls: TEpiCustomControlItemList read FAllControls;
  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

