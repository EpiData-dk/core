{%MainUnit epidatafiles.pas}

type

  { TEpiHeadings }

  TEpiHeadings = class(TEpiCustomList)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

  { TEpiHeading }

  TEpiHeading = class(TEpiCustomItem)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

