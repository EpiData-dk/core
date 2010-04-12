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

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;
