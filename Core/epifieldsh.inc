{%MainUnit epidatafiles.pas}

type

  { TEpiFields }

  TEpiFields = class(TEpiCustomList)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

  { TEpiField }

  TEpiField = class(TEpiCustomItem)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

