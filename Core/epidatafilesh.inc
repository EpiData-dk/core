{%MainUnit epidatafiles.pas}

type

  { TEpiDataFiles }

  TEpiDataFiles = class(TEpiCustomList)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

  TEpiDataFile = class(TEpiCustomItem)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;
