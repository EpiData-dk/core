{%MainUnit epidatafiles.pas}

type

  { TEpiComparison }
  TEpiComparisonType = (fcEq, fcNEq, fcLT, fcLEq, fcGEq, fcGT);
  TEpiComparison = class(TEpiCustomBase)
  private
    function GetOwnerField: TEpiField;
  private
    FCompareField: TEpiField;
    FCompareType: TEpiComparisonType;
    procedure   SetCompareField(const AValue: TEpiField);
    procedure   SetCompareType(const AValue: TEpiComparisonType);
    procedure   FieldHook(Const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer); virtual;
    property    OwnerField: TEpiField read GetOwnerField;
  protected
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
       ReferenceType: Byte; const ReferenceId: string); override;
    function  DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    SaveAttributesToXml: string; override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    CompareField: TEpiField read FCompareField write SetCompareField;
    property    CompareType: TEpiComparisonType read FCompareType write SetCompareType;
  end;

function ComparisonTypeToString(ct: TEpiComparisonType): string;
