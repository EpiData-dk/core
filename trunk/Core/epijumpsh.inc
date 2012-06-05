{%MainUnit epidatafiles.pas}

type

  { TEpiJumps }

  TEpiJumps = class(TEpiCustomList)
  private
    function GetJump(index: integer): TEpiJump;
    function GetJumpFromValue(const Value: string): TEpiJump;
  protected
    function Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewJump: TEpiJump;
    property    Jump[index: integer]: TEpiJump read GetJump; default;
    property    JumpFromValue[const Value: string]: TEpiJump read GetJumpFromValue;
  end;

  { TEpiJump }

  TEpiJump = class(TEpiCustomItem)
  private
    FJumpToField: TEpiField;
    FJumpType: TEpiJumpType;
    FResetType: TEpiJumpResetType;
    procedure   SetJumpToField(const AValue: TEpiField);
    procedure   SetJumpType(const AValue: TEpiJumpType);
    procedure   SetResetType(const AValue: TEpiJumpResetType);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    function    GetJumpValueAsString: string; virtual; abstract;
    function    WriteNameToXml: boolean; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    SaveAttributesToXml: string; override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    JumpToField: TEpiField read FJumpToField write SetJumpToField;
    property    JumpType: TEpiJumpType read FJumpType write SetJumpType;
    property    ResetType: TEpiJumpResetType read FResetType write SetResetType;
    property    JumpValueAsString: string read GetJumpValueAsString;
  end;

  { TEpiBoolJump }

  TEpiBoolJump = class(TEpiJump)
  private
    FJumpValue: EpiBool;
    procedure SetJumpValue(const AValue: EpiBool);
  protected
    function  GetJumpValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property  JumpValue: EpiBool read FJumpValue write SetJumpValue;
  end;

  { TEpiIntJump }

  TEpiIntJump = class(TEpiJump)
  private
    FJumpValue: EpiInteger;
    procedure SetJumpValue(const AValue: EpiInteger);
  protected
    function  GetJumpValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property  JumpValue: EpiInteger read FJumpValue write SetJumpValue;
  end;

  { TEpiFloatJump }

  TEpiFloatJump = class(TEpiJump)
  private
    FJumpValue: EpiFloat;
    procedure SetJumpValue(const AValue: EpiFloat);
  protected
    function  GetJumpValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property  JumpValue: EpiFloat read FJumpValue write SetJumpValue;
  end;

  { TEpiStringJump }

  TEpiStringJump = class(TEpiJump)
  private
    FJumpValue: EpiString;
    procedure SetJumpValue(const AValue: EpiString);
  protected
    function  GetJumpValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property  JumpValue: EpiString read FJumpValue write SetJumpValue;
  end;
