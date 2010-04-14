{%MainUnit epidatafiles.pas}

type

  { TEpiFields }

  TEpiFields = class(TEpiCustomList)
  private
    function GetDataFile: TEpiDataFile;

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    DataFile: TEpiDataFile read GetDataFile;
  end;

  { TEpiField }

  TEpiField = class(TEpiCustomItem)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
  end;

