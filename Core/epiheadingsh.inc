{%MainUnit epidatafiles.pas}

type

  TEpiHeadingChangeEvent = (ehceType);

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
    function    SaveAttributesToXml: string; override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
  public
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Section:  TEpiSection read GetSection;
    property    HeadingType: TEpiHeadingType read FHeadingType write SetHeadingType;
  end;

