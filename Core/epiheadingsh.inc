{%MainUnit epidatafiles.pas}

type

  { TEpiHeadings }

  TEpiHeadings = class(TEpiCustomList)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;

    function   NewHeading: TEpiHeading;
  end;

  { TEpiHeading }

  TEpiHeading = class(TEpiCustomItem)
  private

  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
  end;

