{%MainUnit epidatafiles.pas}

type

  { TEpiCalculation }

  TEpiCalcType = (ctTimeDiff, ctCombineDate, ctCombineString);
  TEpiCalculation = class(TEpiCustomBase)
  private
    FResultField: TEpiField;
    function GetOwnerField: TEpiField;
    procedure SetResultField(const AValue: TEpiField);
  protected
    function GetCalcType: TEpiCalcType; virtual; abstract;
    procedure RegisterFieldHook(NewField: TEpiField; OldField: TEpiField);
    procedure FieldHook(Const Sender: TEpiCustomBase;
      Const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer); virtual;
    property OwnerField: TEpiField read GetOwnerField;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function XMLName: string; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveToXml(Content: String; Lvl: integer): string; override;
    function SaveAttributesToXml: string; override;
    property CalcType: TEpiCalcType read GetCalcType;
    property ResultField: TEpiField read FResultField write SetResultField;
  end;

  { TEpiTimeCalc }
  TEpiTimeCalcType = (ctAsYear, ctAsMonths, ctAsWeeks, ctAsDays, ctAsDayFraction);
  TEpiTimeCalc = class(TEpiCalculation)
  private
    FEndDate: TEpiDateField;
    FEndTime: TEpiDateTimeField;
    FStartDate: TEpiDateField;
    FStartTime: TEpiDateTimeField;
    FTimeCalcType: TEpiTimeCalcType;
    procedure SetEndDate(const AValue: TEpiDateField);
    procedure SetEndTime(const AValue: TEpiDateTimeField);
    procedure SetStartDate(const AValue: TEpiDateField);
    procedure SetStartTime(const AValue: TEpiDateTimeField);
    procedure SetTimeCalcType(const AValue: TEpiTimeCalcType);
  protected
    function GetCalcType: TEpiCalcType; override;
    procedure FieldHook(Const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
      nil): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveToXml(Content: String; Lvl: integer): string; override;
    property StartDate: TEpiDateField read FStartDate write SetStartDate;
    property StartTime: TEpiDateTimeField read FStartTime write SetStartTime;
    property EndDate: TEpiDateField read FEndDate write SetEndDate;
    property EndTime: TEpiDateTimeField read FEndTime write SetEndTime;
    property TimeCalcType: TEpiTimeCalcType read FTimeCalcType write SetTimeCalcType;
  end;

  { TEpiCombineDateCalc }

  TEpiCombineDateCalc = class(TEpiCalculation)
  private
    FDay: TEpiIntField;
    FMonth: TEpiIntField;
    FYear: TEpiIntField;
    procedure SetDay(const AValue: TEpiIntField);
    procedure SetMonth(const AValue: TEpiIntField);
    procedure SetYear(const AValue: TEpiIntField);
  protected
    function GetCalcType: TEpiCalcType; override;
    procedure FieldHook(Const Sender: TEpiCustomBase;
      Const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveToXml(Content: String; Lvl: integer): string; override;
    property Day: TEpiIntField read FDay write SetDay;
    property Month: TEpiIntField read FMonth write SetMonth;
    property Year: TEpiIntField read FYear write SetYear;
  end;

  { TEpiCombineStringCalc }

  TEpiCombineStringCalc = class(TEpiCalculation)
  private
    FDelim1: string;
    FDelim2: string;
    FField1: TEpiField;
    FField2: TEpiField;
    FField3: TEpiField;
    procedure SetDelim1(const AValue: string);
    procedure SetDelim2(const AValue: string);
    procedure SetField1(const AValue: TEpiField);
    procedure SetField2(const AValue: TEpiField);
    procedure SetField3(const AValue: TEpiField);
  protected
    function GetCalcType: TEpiCalcType; override;
    procedure FieldHook(Const Sender: TEpiCustomBase;
      Const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveToXml(Content: String; Lvl: integer): string; override;
    property Field1: TEpiField read FField1 write SetField1;
    property Delim1: string read FDelim1 write SetDelim1;
    property Field2: TEpiField read FField2 write SetField2;
    property Delim2: string read FDelim2 write SetDelim2;
    property Field3: TEpiField read FField3 write SetField3;
  end;
