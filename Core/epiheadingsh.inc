{%MainUnit epidatafiles.pas}

type

  { TEpiHeadings }

  TEpiHeadings = class(TEpiCustomControlItemList)
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
    FCaption:   TEpiTranslatedTextWrapper;
    FDataFile: TEpiDataFile;
    function   GetSection: TEpiSection;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    class function IdString: string; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
  public
    property    Caption: TEpiTranslatedTextWrapper read FCaption;
    property    DataFile: TEpiDataFile read FDataFile;
    property    Section:  TEpiSection read GetSection;
  end;

