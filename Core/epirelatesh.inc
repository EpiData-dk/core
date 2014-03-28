{%MainUnit epidatafiles.pas}

type

  { TEpiRelates }

  TEpiRelates = class(TEpiCustomList)
  private
    function GetRelate(Index: Integer): TEpiRelate;
    function GetRelateFromValue(Value: EpiString): TEpiRelate;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function XMLName: string; override;
    function ItemClass: TEpiCustomItemClass; override;
    function NewRelate: TEpiRelate;
    property Relate[Index: Integer]: TEpiRelate read GetRelate; default;
    property RelateFromValue[Value: EpiString]: TEpiRelate read GetRelateFromValue;
  end;

  { TEpiRelate }

  TEpiRelate = class(TEpiCustomItem)
  private
    FDetailRelation: TEpiCustomItem;
    FRelateValue: EpiString;
    procedure SetDetailRelation(AValue: TEpiCustomItem);
    procedure SetRelateValue(AValue: EpiString);
  protected
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
       ReferenceType: Byte; const ReferenceId: string); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    function WriteNameToXml: boolean; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function SaveAttributesToXml: string; override;
    function XMLName: string; override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property DetailRelation: TEpiCustomItem read FDetailRelation write SetDetailRelation;
    property RelateValue: EpiString read FRelateValue write SetRelateValue;
  end;

