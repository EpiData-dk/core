{%MainUnit epidatafiles.pas}

type
  TLinkType = (
    // TEpiJump
    ltJump,
    // TEpiField
    ltValueLabelWrite,
    // TEpiCalculation
    // - Time diff...
    ltCalcResField, ltCalcStartDate, ltCalcEndDate, ltCalcStartTime, ltCalcEndTime,
    // - combine date...
    ltCalcDayField, ltCalcMonthField, ltCalcYearField,
    // - combine string...
    ltCalcField1, ltCalcField2, ltCalcField3,
    // TEpiComparison
    ltCompare
    );

  { TLinkMap }

  TLinkMap = class
  private
    FLinkTypes: TList;
    FLink: TStringList;
    function GetLinkType(const Index: integer): TLinkType;
    function GetObjects(const Index: integer): TObject;
    function GetRefObject(const Index: integer): String;
  public
    constructor Create;
    procedure AddLink(LinkType: TLinkType; Obj: TObject; RefObjId: string);
    function Count: integer;
    procedure Clear;
    property LinkTypes[Const Index: integer]: TLinkType read GetLinkType;
    property Objects[Const Index: integer]: TObject read GetObjects;
    property RefObjId[Const Index: integer]: String read GetRefObject;
  end;

var
  LinkMap: TLinkMap;

{ TLinkMap }

function TLinkMap.GetLinkType(const Index: integer): TLinkType;
begin
  result := TLinkType(PtrUInt(FLinkTypes.Items[Index]));
end;

function TLinkMap.GetObjects(const Index: integer): TObject;
begin
  result := FLink.Objects[index];
end;

function TLinkMap.GetRefObject(const Index: integer): String;
begin
  result := FLink[Index];
end;

constructor TLinkMap.Create;
begin
  FLinkTypes := TList.Create;
  FLink      := TStringList.Create;
end;

procedure TLinkMap.AddLink(LinkType: TLinkType; Obj: TObject; RefObjId: string);
begin
  FLinkTypes.Add(Pointer(PtrUInt(LinkType)));
  FLink.AddObject(RefObjId, Obj);
end;

function TLinkMap.Count: integer;
begin
  result := FLinkTypes.Count;
end;

procedure TLinkMap.Clear;
begin
  FLinkTypes.Clear;
  FLink.Clear;
end;


