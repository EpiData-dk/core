{%MainUnit epidatafile.pas}

type
  { TEpiTextLabels }

  TEpiTextLabels = class(TObject)
  private
    FOwned: Boolean;
    FOwner:     TEpiDataFile;
    FList:      TFPList;
    function    GetCount: Cardinal;
    function    GetTextLabel(Index: integer): TEpiTextLabel;
  public
    constructor Create(AOwner: TEpiDataFile); virtual;
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    procedure   Add(aTextLabel: TEpiTextLabel);
    procedure   Delete(aTextLabel: TEpiTextLabel);
    function    TextLabelById(Const aTextLabelId: string): TEpiTextLabel;
    function    TextLabelExists(Const aTextLabelId: string): boolean;
    function    IndexOf(Const aTextLabelId: string): integer;
    Property    TextLabel[Index: integer]: TEpiTextLabel read GetTextLabel; default;
    Property    Count: Cardinal read GetCount;
    Property    Owned: Boolean read FOwned write FOwned;
  end;

  { TEpiTextLabel }

  TEpiTextLabel = class(TObject)
  private
    FOwner:      TEpiTextLabels;
    FId:         string;
    FText:       string;
    FScreenProp: TEpiScreenProperty;
    FTextLeft: Integer;
    FTextTop: Integer;
    procedure SetId(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetTextLeft(const AValue: Integer);
    procedure SetTextTop(const AValue: Integer);
  public
    constructor Create(AOwner: TEpiTextLabels);
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    property    Id:         string read FId write SetId;
    property    Text:       string read FText write SetText;
    Property    TextTop:    Integer read FTextTop write SetTextTop;
    Property    TextLeft:   Integer read FTextLeft write SetTextLeft;
    property    ScreenProp: TEpiScreenProperty read FScreenProp write FScreenProp;
  private
    // Events (and control):
    FOnChange:     ^TEpiTextLabelChangeEvent;
    FOnChangeCount: Integer;
    FUpdateCount:   Integer;
    procedure DoChange(EventType: TEpiTextLabelChangeEventType; OldValue: Pointer);
  public
    // Events (and control):
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
    procedure UnRegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
  end;
