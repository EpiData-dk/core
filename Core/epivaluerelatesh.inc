{%MainUnit epidatafiles.pas}

type

  { TEpiValueRelates }

  TEpiValueRelates = class(TEpiRelates)
  private
    function GetRelate(Index: Integer): TEpiValueRelate;
    function GetRelateFromValue(Value: EpiString): TEpiValueRelate;
  public
    function ItemClass: TEpiCustomItemClass; override;
    property Relate[Index: Integer]: TEpiValueRelate read GetRelate; default;
    property RelateFromValue[Value: EpiString]: TEpiValueRelate read GetRelateFromValue;
  end;

  TEpiValueRelate = class(TEpiRelate)
  private
    FRelateValue: EpiString;
    procedure SetRelateValue(AValue: EpiString);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
      override;
    property RelateValue: EpiString read FRelateValue write SetRelateValue;
  end;

