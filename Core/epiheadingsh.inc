{%MainUnit epidatafiles.pas}

type

  { TEpiHeadings }

  TEpiHeadings = class(TEpiCustomNamedItemList)
  protected
    function NamePrefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    function   NewHeading: TEpiHeading;
    function   GetHeadingByName(Const AName: string): TEpiHeading;
  end;

  { TEpiHeading }

  TEpiHeading = class(TEpiCustomControlItem)
  private
    FCaption:   TEpiTranslatedText;
    FDataFile: TEpiDataFile;
    function   GetSection: TEpiSection;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    class function IdString: string; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
  public
    property    Caption: TEpiTranslatedText read FCaption;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Section:  TEpiSection read GetSection;
  end;

