{%MainUnit epidatafiles.pas}

type

  { TEpiJumps }

  TEpiJumps = class(TEpiCustomList)
  private
    function GetJump(index: integer): TEpiJump;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewJump: TEpiJump;
    property    Jump[index: integer]: TEpiJump read GetJump; default;
  end;

  { TEpiJump }

  TEpiJump = class(TEpiCustomItem)
  private
    FJumpToField: TEpiField;
    FResetOnJump: boolean;
    procedure   SetJumpToField(const AValue: TEpiField);
    procedure   SetResetOnJump(const AValue: boolean);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    function    GetJumpValueAsString: string; virtual; abstract;
    function    GetResetValueAsString: string; virtual; abstract;
    class function IdString: string; override;
  public
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    SaveAttributesToXml: string; override;
    property    JumpToField: TEpiField read FJumpToField write SetJumpToField;
    property    ResetOnJump: boolean read FResetOnJump write SetResetOnJump;
    property    JumpValueAsString: string read GetJumpValueAsString;
    property    ResetValueAsString: string read GetResetValueAsString;
  end;

  { TEpiBoolJump }

  TEpiBoolJump = class(TEpiJump)
  private
    FJumpValue: EpiBool;
    FResetValue: EpiBool;
    procedure SetJumpValue(const AValue: EpiBool);
    procedure SetResetValue(const AValue: EpiBool);
  protected
    function  GetJumpValueAsString: string; override;
    function  GetResetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property  JumpValue: EpiBool read FJumpValue write SetJumpValue;
    property  ResetValue: EpiBool read FResetValue write SetResetValue;
  end;

  { TEpiIntJump }

  TEpiIntJump = class(TEpiJump)
  private
    FJumpValue: EpiInteger;
    FResetValue: EpiInteger;
    procedure SetJumpValue(const AValue: EpiInteger);
    procedure SetResetValue(const AValue: EpiInteger);
  protected
    function  GetJumpValueAsString: string; override;
    function  GetResetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property  JumpValue: EpiInteger read FJumpValue write SetJumpValue;
    property  ResetValue: EpiInteger read FResetValue write SetResetValue;
  end;

  { TEpiFloatJump }

  TEpiFloatJump = class(TEpiJump)
  private
    FJumpValue: EpiFloat;
    FResetValue: EpiFloat;
    procedure SetJumpValue(const AValue: EpiFloat);
    procedure SetResetValue(const AValue: EpiFloat);
  protected
    function  GetJumpValueAsString: string; override;
    function  GetResetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property  JumpValue: EpiFloat read FJumpValue write SetJumpValue;
    property  ResetValue: EpiFloat read FResetValue write SetResetValue;
  end;

  { TEpiStringJump }

  TEpiStringJump = class(TEpiJump)
  private
    FJumpValue: EpiString;
    FResetValue: EpiString;
    procedure SetJumpValue(const AValue: EpiString);
    procedure SetResetValue(const AValue: EpiString);
  protected
    function  GetJumpValueAsString: string; override;
    function  GetResetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property  JumpValue: EpiString read FJumpValue write SetJumpValue;
    property  ResetValue: EpiString read FResetValue write SetResetValue;
  end;
