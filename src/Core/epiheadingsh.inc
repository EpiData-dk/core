{%MainUnit epidatafiles.pas}

type

  TEpiHeadingChangeEvent = (ehceType);

  TEpiHeadingsEnumerator = class;

  { TEpiHeadings }

  TEpiHeadings = class(TEpiCustomControlItemList)
  protected
    function Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   ItemClass: TEpiCustomItemClass; override;
    function   NewHeading: TEpiHeading;
    function   GetHeadingByName(Const AName: string): TEpiHeading;
    function   GetEnumerator: TEpiHeadingsEnumerator;
    procedure  Assign(const AEpiCustomBase: TEpiCustomBase); override;
  end;

  { TEpiHeading }

  TEpiHeading = class(TEpiCustomControlItem)
  private
    FCaption:   TEpiTranslatedTextWrapper;
    FDataFile: TEpiDataFile;
    FHeadingType: TEpiHeadingType;
    function   GetSection: TEpiSection;
    procedure  SetHeadingType(AValue: TEpiHeadingType);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function  SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
  public
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Section:  TEpiSection read GetSection;
    property    HeadingType: TEpiHeadingType read FHeadingType write SetHeadingType;
  end;

  { TEpiHeadingsEnumerator }

  TEpiHeadingsEnumerator = class(TEpiCustomControlItemListEnumerator)
  protected
    function GetCurrent: TEpiHeading; override;
  public
    property Current: TEpiHeading read GetCurrent;
  end;


