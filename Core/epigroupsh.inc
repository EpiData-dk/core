{%MainUnit epidatafiles.pas}

type

  { TEpiGroups }

  TEpiGroups = class(TEpiCustomList)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

  { TEpiGroup }

  TEpiGroup = class(TEpiCustomItem)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

