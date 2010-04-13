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
    { Data containers }
    FGroups: TEpiGroups;
    FFields: TEpiFields;
    FHeadings: TEpiHeadings;
    function GetField(Index: integer): TEpiField;
    function GetGroup(Index: integer): TEpiGroup;
    function GetHeading(Index: integer): TEpiHeading;
  private
    { Private housekeeping }
    FRecordStatus: TEpiField;
  private
    { Properties }
  public
    constructor Create(AOwner: TEpiCustomBase; const Size: integer = 0);
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Groups: TEpiGroups read FGroups;
    property    Group[Index: integer]: TEpiGroup read GetGroup;
    property    Fields: TEpiFields read FFields;
    property    Field[Index: integer]: TEpiField read GetField;
    property    Headings: TEpiHeadings read FHeadings;
    property    Heading[Index: integer]: TEpiHeading read GetHeading;
  end;
