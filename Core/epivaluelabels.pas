unit epivaluelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafilestypes, variants, Laz2_DOM;

type

  TEpiValueLabelChangeEvent = (evceValue, evceMissing);
  TEpiValueLabelSetChangeEvent = (evlsMaxValueLength);

  { TEpiCustomValueLabel }

  TEpiCustomValueLabel = class(TEpiCustomItem)
  private
    FIsMissingValue: boolean;
    FOrder: Integer;
    FLabel: TEpiTranslatedText;
    procedure SetIsMissingValue(AValue: boolean);
  protected
    function GetValueAsString: string; virtual; abstract;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    function WriteNameToXml: boolean; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property    Order: integer read FOrder write FOrder;
    property    TheLabel: TEpiTranslatedText read FLabel write FLabel;
    property    IsMissingValue: boolean read FIsMissingValue write SetIsMissingValue;
    property    ValueAsString: string read GetValueAsString;
  end;
  TEpiCustomValueLabelClass = class of TEpiCustomValueLabel;

  { TEpiIntValueLabel }

  TEpiIntValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiInteger;
    procedure SetValue(AValue: EpiInteger);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiInteger read FValue write SetValue;
  end;

  { TEpiDateValueLabel }

  TEpiDateValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiDate;
    procedure SetValue(AValue: EpiDate);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiDate read FValue write SetValue;
  end;

  { TEpiFloatValueLabel }

  TEpiFloatValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiFloat;
    procedure SetValue(AValue: EpiFloat);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiFloat read FValue write SetValue;
  end;

  { TEpiTimeValueLabel }

  TEpiTimeValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiTime;
    procedure SetValue(AValue: EpiTime);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiTime read FValue write SetValue;
  end;

  { TEpiStringValueLabel }

  TEpiStringValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiString;
    procedure SetValue(AValue: EpiString);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiString read FValue write SetValue;
  end;


  { TEpiValueLabelSetEnumerator }

  TEpiValueLabelSetEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiCustomValueLabel; override;
  public
    property Current: TEpiCustomValueLabel read GetCurrent;
  end;

  { TEpiValueLabelSet }

  TEpiValueLabelSetScope = (vlsInternal, vlsExternal);

  TEpiValueLabelSet = class(TEpiCustomList)
  { External Valuelabel Set Properties }
  private
    FExtFileName: string;
  public
    property    ExtFileName: string read FExtFileName write FExtFileName;
  private
    FLabelScope: TEpiValueLabelSetScope;
    FLabelType: TEpiFieldType;
    FWriteNameToXml: boolean;
    function    GetValueLabel(const AValue: variant): TEpiCustomValueLabel;
    function    GetValueLabelIndex(const AValue: variant): integer;
    function    GetIsMissingValue(const AValue: variant): boolean;
    function    GetIsMaxMissingValue(const AValue: variant): boolean;
    function    GetIs2ndMaxMissingValue(const AValue: variant): boolean;
    function    GetValueLabelString(const AValue: variant): string;
    function    GetValueLabelExists(const AValue: variant): boolean;
    function    GetValueLabels(const index: integer): TEpiCustomValueLabel;
    procedure   SetLabelType(const AValue: TEpiFieldType);
  private
    { House-keeping for MaxValueLength }
    FDirtyCache: boolean;
    FCachedLength: LongInt;
    procedure   DirtyCacheAndSendChangeEvent;
    procedure   AssignValues(Const Src: TEpiValueLabelSet);
  protected
    procedure   LoadOldInternalTag(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); virtual;
    function    WriteNameToXml: boolean; override;
    procedure   DoAssignList(const EpiCustomList: TEpiCustomList); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    ItemClass: TEpiCustomItemClass; override;
    function    GetEnumerator: TEpiValueLabelSetEnumerator;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    function    NewValueLabel: TEpiCustomValueLabel;
    procedure   InsertItem(const Index: integer; Item: TEpiCustomItem); override;
    function    DeleteItem(Index: integer): TEpiCustomItem; override;
    property    LabelScope: TEpiValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TEpiFieldType read FLabelType write SetLabelType;
    property    ValueLabels[Const index: integer]: TEpiCustomValueLabel read GetValueLabels; default;
    property    ValueLabel[Const AValue: variant]: TEpiCustomValueLabel read GetValueLabel;
    property    ValueLabelString[Const AValue: variant]: string read GetValueLabelString;
    property    ValueLabelExists[Const AValue: variant]: boolean read GetValueLabelExists;
    property    IsMissingValue[Const AValue: variant]: boolean read GetIsMissingValue;
    property    IsMaxMissingValue[Const AValue: variant]: boolean read GetIsMaxMissingValue;
    property    Is2ndMaxMissingValue[Const AValue: variant]: boolean read GetIs2ndMaxMissingValue;
  public
    { Aux. functions. }
    function    MissingCount: LongInt;
    function    MaxValueLength: LongInt;
  end;

  { TEpiValueLabelSetsEnumerator }

  TEpiValueLabelSetsEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiValueLabelSet; override;
  public
    property Current: TEpiValueLabelSet read GetCurrent;
  end;

  { TEpiValueLabelSets }

  TEpiValueLabelSets = class(TEpiCustomList)
  private
    function GetVLSet(Const Index: Integer; Const Scope: TEpiValueLabelSetScope): TEpiValueLabelSet;
    function GetExternalCount: Integer;
    function GetExternalSets(Index: integer): TEpiValueLabelSet;
    function GetInternalCount: Integer;
    function GetInternalSets(Index: integer): TEpiValueLabelSet;
    function    GetValueLabels(index: integer): TEpiValueLabelSet;
  protected
    function    Prefix: string; override;
    procedure   LoadExternalValueLabelSet(DocFileCache: TObject;
      Root: TDomNode);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    ValidateRename(ValueLabelSet: TEpiValueLabelSet; NewName: string): boolean;
    function    NewValueLabelSet(ALabelType: TEpiFieldType): TEpiValueLabelSet;
    function    GetValueLabelSetByName(Const AName: string): TEpiValueLabelSet;
    function    GetEnumerator: TEpiValueLabelSetsEnumerator;
    property    ValueLabels[index: integer]: TEpiValueLabelSet read GetValueLabels; default;
  public
    { Aux. functions }
    property    InternalCount: Integer read GetInternalCount;
    property    ExternalCount: Integer read GetExternalCount;
    property    InternalSets[Index: integer]: TEpiValueLabelSet read GetInternalSets;
    property    ExternalSets[Index: integer]: TEpiValueLabelSet read GetExternalSets;
  end;

implementation

uses
  strutils, math, LazUTF8, LazFileUtils, epidocument, epiopenfile,
  epiopenfile_cache, epiglobals, epiconvertutils;

{ TEpiValueLabelSetEnumerator }

function TEpiValueLabelSetEnumerator.GetCurrent: TEpiCustomValueLabel;
begin
  Result := TEpiCustomValueLabel(inherited GetCurrent);
end;

{ TEpiCustomValueLabel }

procedure TEpiCustomValueLabel.SetIsMissingValue(AValue: boolean);
var
  Val: Boolean;
begin
  if FIsMissingValue = AValue then Exit;
  Val := FIsMissingValue;
  FIsMissingValue := AValue;
  DoChange(eegValueLabel, Word(evceMissing), @Val);
end;

function TEpiCustomValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiCustomValueLabel(Result) do
  begin
    FIsMissingValue := Self.FIsMissingValue;
    FOrder          := Self.FOrder;
  end;
end;

function TEpiCustomValueLabel.WriteNameToXml: boolean;
begin
  Result := false;
end;

function TEpiCustomValueLabel.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if IsMissingValue then
    SaveDomAttr(Result, rsMissing, IsMissingValue);

  SaveDomAttr(Result, rsOrder, Order);
end;

constructor TEpiCustomValueLabel.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  IsMissingValue := false;
  FLabel := TEpiTranslatedText.Create(Self, rsLabel);
  RegisterClasses([FLabel]);
end;

destructor TEpiCustomValueLabel.Destroy;
begin
  FLabel.Free;
  inherited Destroy;
end;

function TEpiCustomValueLabel.XMLName: string;
begin
  Result := rsValueLabel;
end;

procedure TEpiCustomValueLabel.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMElement;
  Attr: TDOMAttr;
begin
  // Root = <ValueLabel>
  Node := TDOMElement(Root);

  Order    := LoadAttrInt(Root, rsOrder);
  if LoadAttr(Attr, Root, rsMissing, false) then
    IsMissingValue := LoadAttrBool(Root, rsMissing);
  TheLabel.LoadFromXml(Root, ReferenceMap);
end;

procedure TEpiCustomValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgVL: TEpiCustomValueLabel absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FIsMissingValue := OrgVL.FIsMissingValue;
  FOrder          := OrgVL.FOrder;
  EndUpdate;
end;

{ TEpiIntValueLabel }

procedure TEpiIntValueLabel.SetValue(AValue: EpiInteger);
var
  Val: EpiInteger;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiIntValueLabel.GetValueAsString: string;
begin
  Result := IntToStr(Value);
end;

function TEpiIntValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiIntValueLabel(Result).FValue := FValue;
end;

function TEpiIntValueLabel.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
  SaveDomAttr(Result, rsValue, Value);
end;

procedure TEpiIntValueLabel.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  Value := LoadAttrInt(Root, rsValue);
end;

procedure TEpiIntValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiIntValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiDateValueLabel }

procedure TEpiDateValueLabel.SetValue(AValue: EpiDate);
var
  Val: EpiDate;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiDateValueLabel.GetValueAsString: string;
begin
  case TEpiValueLabelSet(Owner).LabelType of
    ftDMYDate: result := FormatDateTime('DD/MM/YYYY', FValue);
    ftMDYDate: result := FormatDateTime('MM/DD/YYYY', FValue);
    ftYMDDate: result := FormatDateTime('YYYY/MM/DD', FValue);
  end;
end;

function TEpiDateValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiDateValueLabel(Result).FValue := FValue;
end;

function TEpiDateValueLabel.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
  SaveDomAttr(Result, rsValue, Value);
end;

procedure TEpiDateValueLabel.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  Value := LoadAttrInt(Root, rsValue);
end;

procedure TEpiDateValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiDateValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiFloatValueLabel }

procedure TEpiFloatValueLabel.SetValue(AValue: EpiFloat);
var
  Val: EpiFloat;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiFloatValueLabel.GetValueAsString: string;
begin
  Result := FloatToStr(Value);
end;

function TEpiFloatValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiFloatValueLabel(Result).FValue := FValue;
end;

function TEpiFloatValueLabel.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
  SaveDomAttr(Result, rsValue, Value);
end;

procedure TEpiFloatValueLabel.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  Value := LoadAttrFloat(Root, rsValue);
end;

procedure TEpiFloatValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiFloatValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiTimeValueLabel }

procedure TEpiTimeValueLabel.SetValue(AValue: EpiTime);
var
  Val: EpiTime;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiTimeValueLabel.GetValueAsString: string;
begin
  result := TimeToStr(Value);
end;

function TEpiTimeValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiTimeValueLabel(Result).FValue := FValue;
end;

function TEpiTimeValueLabel.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
  SaveDomAttr(Result, rsValue, Value);
end;

procedure TEpiTimeValueLabel.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  Value := LoadAttrDateTime(Root, rsValue);
end;

procedure TEpiTimeValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiTimeValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiStringValueLabel }

procedure TEpiStringValueLabel.SetValue(AValue: EpiString);
var
  Val: EpiString;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiStringValueLabel.GetValueAsString: string;
begin
  Result := Value;
end;

function TEpiStringValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiStringValueLabel(Result).FValue := FValue;
end;

function TEpiStringValueLabel.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
  SaveDomAttr(Result, rsValue, Value);
end;

procedure TEpiStringValueLabel.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  Value := LoadAttrString(Root, rsValue);
end;

procedure TEpiStringValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiStringValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiValueLabelSet }

function OrderCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiCustomValueLabel absolute Item1;
  VL2: TEpiCustomValueLabel absolute Item2;
begin
  Result := VL1.Order - VL2.Order;
end;

function FloatCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiFloatValueLabel absolute Item1;
  VL2: TEpiFloatValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := Math.Sign(VL1.Value - VL2.Value)
  else
    Result := OrderCompare(Item1, Item2);
end;

function IntCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiIntValueLabel absolute Item1;
  VL2: TEpiIntValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := VL1.Value - VL2.Value
  else
    Result := OrderCompare(Item1, Item2);
end;

function StringCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiStringValueLabel absolute Item1;
  VL2: TEpiStringValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := CompareStr(VL1.Value, VL2.Value)
  else
    Result := OrderCompare(Item1, Item2);
end;

procedure TEpiValueLabelSet.SetLabelType(const AValue: TEpiFieldType);
begin
  if AValue = FLabelType then exit;
  FLabelType := AValue;
end;

procedure TEpiValueLabelSet.DirtyCacheAndSendChangeEvent;
begin
  FDirtyCache := true;
  DoChange(eegValueLabelSet, Word(evlsMaxValueLength), @FCachedLength);
end;

procedure TEpiValueLabelSet.AssignValues(const Src: TEpiValueLabelSet);
begin
  FLabelScope := Src.FLabelScope;
  FLabelType  := Src.FLabelType;
  FName       := Src.FName;
  FExtFileName := Src.FExtFileName;
end;

function TEpiValueLabelSet.GetValueLabelString(const AValue: variant): string;
var
  i: Integer;
begin
  I := GetValueLabelIndex(AValue);
  if I = -1 then
    result := VarToStr(AValue)
  else
    result := ValueLabels[i].TheLabel.Text
end;

function TEpiValueLabelSet.GetValueLabelExists(const AValue: variant): boolean;
begin
  result := GetValueLabelIndex(AValue) <> -1;
end;

function TEpiValueLabelSet.GetValueLabels(const index: integer
  ): TEpiCustomValueLabel;
begin
  result := TEpiCustomValueLabel(Items[index]);
end;

function TEpiValueLabelSet.GetValueLabel(const AValue: variant): TEpiCustomValueLabel;
var
  I: LongInt;
begin
  result := nil;

  I := GetValueLabelIndex(AValue);
  if I <> -1 then
    Result := ValueLabels[I];
end;

function TEpiValueLabelSet.GetValueLabelIndex(const AValue: variant): integer;
var
  lInt: EpiInteger;
  lFlt: EpiFloat;
  lStr: EpiString;
  lDate: EpiDate;
  lTime: EpiTime;
  DummyStr: string;
begin
  case VarType(AValue) of
    varsmallint,
    varinteger,
    varint64,
    varword,
    varqword:
      begin
        lInt := Integer(AValue);
        lFlt := lInt;
        lStr := IntToStr(lInt);
        lDate := EpiDate(lInt);
        lTime := EpiTime(lInt);
      end;

    varsingle,
    vardouble:
      begin
        lFlt := EpiFloat(AValue);
        lInt := trunc(lFlt);
        lStr := FloatToStr(lFlt);
        lDate := EpiDate(lInt);
        lTime := EpiTime(lFlt);
      end;

//    vtBoolean       = 1;
//    vtChar          = 2;
    varstring,
    varustring:
      begin
        lStr := EpiString(AValue);
        TryStrToInt64(lStr, lInt);
        TryStrToFloat(lStr, lFlt);
        EpiStrToDateGuess(lStr, lDate, DummyStr);
        EpiStrToTimeGues(lStr, lTime, DummyStr);
      end;

{    vtPointer       = 5;
    vtPChar         = 6;
    vtObject        = 7;
    vtClass         = 8;
    vtWideChar      = 9;
    vtPWideChar     = 10;
        = 11;
    vtCurrency      = 12;
    vtVariant       = 13;
    vtInterface     = 14;
        = 15;            }
    else
      begin
        lstr := VarTypeAsText(VarType(AValue));
      end;
  end;

  case LabelType of
    ftString,
    ftUpperString:
      begin
        for result := 0 to Count - 1 do
          if AnsiSameStr(AValue, TEpiStringValueLabel(Items[result]).FValue) then
            exit;
      end;
    ftFloat:
      begin
        for result := 0 to Count -1 do
          if SameValue(AValue, TEpiFloatValueLabel(Items[result]).FValue) then
            exit;
      end;
{    ftTime:
      begin
        for result := 0 to Count -1 do
          if SameValue(AValue, TEpiFloatValueLabel(Items[result]).FValue) then
            exit;
      end;        }
    ftInteger:
      begin
        for result := 0 to Count - 1 do
          if AValue = TEpiIntValueLabel(Items[result]).FValue then
            exit;
      end;
    ftDMYDate, ftMDYDate, ftYMDDate:
      begin
        for result := 0 to Count - 1 do
          if AValue = TEpiDateValueLabel(Items[result]).FValue then
            exit;
      end;
  end;
  result := -1;
end;

function TEpiValueLabelSet.GetIsMissingValue(const AValue: variant): boolean;
var
  i: Integer;
begin
  I := GetValueLabelIndex(AValue);
  Result := (I <> -1) and
            ValueLabels[i].IsMissingValue;
end;

function TEpiValueLabelSet.GetIsMaxMissingValue(const AValue: variant): boolean;
var
  Runner: Integer;
  I: Integer;
begin
  I := GetValueLabelIndex(AValue);
  Result := (I <> -1) and
            ValueLabels[I].IsMissingValue;

  if (not Result) then Exit;

  // Assumes sorted by values.
  for Runner := Count -1 downto I + 1 do
    if ValueLabels[Runner].IsMissingValue then
      Exit(false);
end;

function TEpiValueLabelSet.GetIs2ndMaxMissingValue(const AValue: variant
  ): boolean;
var
  Counter: Integer;
  Runner: Integer;
  I: Integer;
begin
  I := GetValueLabelIndex(AValue);
  Result := (I <> -1) and
            ValueLabels[I].IsMissingValue;

  if (not Result) then Exit;

  Counter := 0;

  // Assumes sorted by values.
  for Runner := Count -1 downto I + 1 do
    if ValueLabels[Runner].IsMissingValue then
      Inc(Counter);

  Result := (Counter = 1);
end;

procedure TEpiValueLabelSet.LoadOldInternalTag(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  NValueLabel: TEpiCustomValueLabel;
begin
  // Root = <Internal>
  FLabelScope := vlsInternal;

  FWriteNameToXml := false;
  inherited LoadFromXml(Root, ReferenceMap);
  FWriteNameToXml := true;
end;

function TEpiValueLabelSet.WriteNameToXml: boolean;
begin
  Result := FWriteNameToXml;
end;

procedure TEpiValueLabelSet.DoAssignList(const EpiCustomList: TEpiCustomList);
var
  Item: TEpiCustomValueLabel;
  i: Integer;
begin
  BeginUpdate;
  if EpiCustomList.Count > 0 then
  begin
    for i := 0 to EpiCustomList.Count - 1 do
    begin
      Item := NewValueLabel;
      Item.Assign(EpiCustomList[i]);
    end;
  end;
  EndUpdate;
end;

function TEpiValueLabelSet.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiValueLabelSet(Result).AssignValues(Self);
end;

procedure TEpiValueLabelSet.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);
  if (EventGroup <> eegValueLabel) then Exit;

  if TEpiValueLabelChangeEvent(EventType) = evceValue then
    DirtyCacheAndSendChangeEvent;
end;

function TEpiValueLabelSet.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  if LabelScope = vlsInternal then
    Result := inherited SaveToDom(RootDoc)
  else
    begin
      Result := RootDoc.CreateElement(XMLName);
      SaveDomAttr(Result, rsId, Name);
      SaveDomAttr(Result, rsFilename, ExtFileName);
    end;

  if not Assigned(Result) then exit;

  SaveDomAttrEnum(Result, rsType, LabelType, TypeInfo(TEpiFieldType));
  SaveDomAttrEnum(Result, rsValueLabelScope, LabelScope, TypeInfo(TEpiValueLabelSetScope));
end;

constructor TEpiValueLabelSet.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FWriteNameToXml := true;
  FLabelScope := vlsInternal;
  FLabelType  := ftInteger;
  FName       := '';
  FDirtyCache := false;
end;

destructor TEpiValueLabelSet.Destroy;
begin
  inherited Destroy;
end;

function TEpiValueLabelSet.XMLName: string;
begin
  Result := rsValueLabelSet;
end;

function TEpiValueLabelSet.ItemClass: TEpiCustomItemClass;
begin
  case LabelType of
    ftDMYDate, ftMDYDate, ftYMDDate,
    ftInteger:
      Result := TEpiIntValueLabel;

    ftFloat,
    ftTime:
      Result := TEpiFloatValueLabel;

    ftString:
      Result := TEpiStringValueLabel;
  end;
end;

function TEpiValueLabelSet.GetEnumerator: TEpiValueLabelSetEnumerator;
begin
  result := TEpiValueLabelSetEnumerator.Create(Self);
end;

procedure TEpiValueLabelSet.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  Attr: TDOMAttr;
  Version: Integer;
begin
  Version := TEpiDocument(RootOwner).Version;
  // Root = <ValueLabel>

  // In version 1+2 the idea was that Internal and External valuelabels
  // was represented using a Sub-tag <Internal> or <External> below the
  // <ValueLabelSet> tag. This contruct makes reading individual ValueLabel's
  // more complex, because we have to circumvent the inherited structure of
  // loading items with CustomItemList
  if Version <= 2 then
  begin
    if LoadAttr(Attr, Root, rsId, True) then
      FName := LoadAttrString(Root, rsId);

    if LoadNode(Node, Root, rsInternal, false) then
      LoadOldInternalTag(Node, ReferenceMap);

    // Was never officially supported and used...
    {if LoadNode(Node, Root, rsExternal, false) then
      LoadExternal(Node);}
  end;

  // In version 3 the <Internal> tag has been removed and we can use a "normal"
  // inherited loading, because TEpiValueLabelSet is a CustomItemList.;
  if (Version >= 3) and
     (LabelScope = vlsInternal)
  then
    inherited LoadFromXml(Root, ReferenceMap);
end;

procedure TEpiValueLabelSet.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  AssignValues(TEpiValueLabelSet(AEpiCustomBase));
end;

function TEpiValueLabelSet.NewValueLabel: TEpiCustomValueLabel;
begin
  Result := TEpiCustomValueLabel(NewItem);
  result.Order := Count + 1;
end;

procedure TEpiValueLabelSet.InsertItem(const Index: integer;
  Item: TEpiCustomItem);
begin
  inherited InsertItem(Index, Item);
  DirtyCacheAndSendChangeEvent;
end;

function TEpiValueLabelSet.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := inherited DeleteItem(Index);
  DirtyCacheAndSendChangeEvent;
end;

function TEpiValueLabelSet.MissingCount: LongInt;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if ValueLabels[i].IsMissingValue then Inc(result);
end;

function TEpiValueLabelSet.MaxValueLength: LongInt;
var
  i: Integer;
begin
  if not FDirtyCache then
    Exit(FCachedLength);

  Result := 0;
  for i := 0 to Count -1 do
    Result := Max(Result, UTF8Length(ValueLabels[i].ValueAsString));
  FCachedLength := Result;
  FDirtyCache := false;
end;

{ TEpiValueLabelSetsEnumerator }

function TEpiValueLabelSetsEnumerator.GetCurrent: TEpiValueLabelSet;
begin
  Result := TEpiValueLabelSet(inherited GetCurrent);
end;

{ TEpiValueLabelSets }

function TEpiValueLabelSets.GetVLSet(const Index: Integer;
  const Scope: TEpiValueLabelSetScope): TEpiValueLabelSet;
var
  Runner: Integer;
  i: Integer;
begin
  Runner := -1;
  for i := 0 to Count - 1 do
  begin
    Result := ValueLabels[i];
    if Result.LabelScope = Scope then
      inc(Runner);
    if Runner = Index then
      break;
  end;

  if Runner < Index then
    Result := nil;
end;

function TEpiValueLabelSets.GetExternalCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if ValueLabels[i].LabelScope = vlsExternal then
      Inc(Result);
end;

function TEpiValueLabelSets.GetExternalSets(Index: integer): TEpiValueLabelSet;
begin
  Result := GetVLSet(Index, vlsExternal);
end;

function TEpiValueLabelSets.GetInternalCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if ValueLabels[i].LabelScope = vlsInternal then
      Inc(Result);
end;

function TEpiValueLabelSets.GetInternalSets(Index: integer): TEpiValueLabelSet;
begin
  Result := GetVLSet(Index, vlsInternal);
end;

function TEpiValueLabelSets.GetValueLabels(index: integer): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(Items[Index]);
end;

function TEpiValueLabelSets.Prefix: string;
begin
  Result := 'valuelabel_id_';
end;

procedure TEpiValueLabelSets.LoadExternalValueLabelSet(DocFileCache: TObject;
  Root: TDomNode);
var
  ExtType: TEpiFieldType;
  FileName: EpiString;
  RootDoc: TEpiDocument;
  DocFile: TEpiDocumentFile;
  VLSet: TEpiValueLabelSet;
  ExtID: EpiString;
  Res: TOpenEpiWarningResult;
  Acontinue: boolean;
begin
  // Root = <ValueLabelSet  scope="vlsExternal">

  ExtID    := LoadAttrString(Root, rsId);
  ExtType  := TEpiFieldType(LoadAttrEnum(Root, rsType, TypeInfo(TEpiFieldType)));
  FileName := LoadAttrString(Root, rsFilename);

  RootDoc := TEpiDocument(RootOwner);
  DocFile := TEpiDocumentFileCache(DocFileCache).OpenFile(FileName, true);

  if Not Assigned(DocFile) then
  begin
    if Assigned(RootDoc.OnLoadError) then
      RootDoc.OnLoadError(RootDoc, 0, @FileName ,AContinue);

    if not AContinue then
      raise EEpiExternalFileNoFound.Create('Value Label: External file "' + FileName + '" not found');

    Exit;
  end;

  VLSet := DocFile.Document.ValueLabelSets.GetValueLabelSetByName(ExtID);
  if Not Assigned(VLSet) then
  begin
    VLSet := TEpiValueLabelSet.Create(Self);
    VLSet.LabelType := ExtType;
    Res := DocFile.OnWarning(wtLockFile, '');
    exit;
  end;

  VLSet := TEpiValueLabelSet(VLSet.Clone(Self));
  VLSet.FLabelScope := vlsExternal;
  VLSet.ExtFileName := FileName;
  AddItem(VLSet);
end;

function TEpiValueLabelSets.ValidateRename(ValueLabelSet: TEpiValueLabelSet;
  NewName: string): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Count - 1 do
    if CompareStr(ValueLabels[i].Name, NewName) = 0 then
      exit;
  result := true;
end;

constructor TEpiValueLabelSets.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiValueLabelSets.Destroy;
begin
  inherited Destroy;
end;

function TEpiValueLabelSets.XMLName: string;
begin
  result := rsValueLabelSets;
end;

type

  { TVLDocFile }

  TVLDocFile = class(TEpiDocumentFile)
  protected
    function DefaultWarningResult(WarningType: TOpenEpiWarningType
      ): TOpenEpiWarningResult; override;
  public
    constructor Create; override;
  end;

function TVLDocFile.DefaultWarningResult(WarningType: TOpenEpiWarningType
  ): TOpenEpiWarningResult;
begin
  if WarningType = wtSysReadOnly then
    Result := wrYes
  else
    Result := inherited DefaultWarningResult(WarningType);
end;

constructor TVLDocFile.Create;
begin
  inherited Create;
end;

procedure TEpiValueLabelSets.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  NValueLabelSet: TEpiValueLabelSet;
  Attr: TDOMAttr;
  Scope: TEpiValueLabelSetScope;
  DocFileCache: TEpiDocumentFileCache;
  Doc: TEpiDocument;

begin
  // Root = <ValueLabelSets>
  Doc := TEpiDocument(RootOwner);

  DocFileCache := TEpiDocumentFileCache.Create;
  DocFileCache.OnPassword := Doc.OnPassword;
//  DocFileCache.OnError    := Doc.OnLoadError;
//  DocFileCache.OnProgress := Doc.OnProgress;
//  DocFileCache.OnWarning  := Doc.on;
  DocFileCache.DocumentFileClass := TVLDocFile;
  try
    Node := Root.FirstChild;
    while Assigned(Node) do
    begin
      while NodeIsWhiteSpace(Node) do
        Node := Node.NextSibling;
      if not Assigned(Node) then exit;

      CheckNode(Node, rsValueLabelSet);

      Scope := vlsInternal;
      if TEpiDocument(RootOwner).Version >= 3 then
        Scope := TEpiValueLabelSetScope(LoadAttrEnum(Node, rsValueLabelScope, TypeInfo(TEpiValueLabelSetScope)));

      if Scope = vlsInternal then
      begin;
        // Newer load the content of the content of the Security Valuelabel set, since in any future version
        // we may wish to extend the content. And all new content will be appended to existing hardcoded version.
        if (LoadAttrString(Node, rsId, '', false) = EpiSecurityLogValuelLabelSetName) then
          NValueLabelSet := TEpiValueLabelSet(GetItemByName(EpiSecurityLogValuelLabelSetName))
        else
          begin
          NValueLabelSet := NewValueLabelSet(TEpiFieldType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiFieldType))));
          NValueLabelSet.LoadFromXml(Node, ReferenceMap);
        end;
      end else begin
        LoadExternalValueLabelSet(DocFileCache, Node);
      end;

      Node := Node.NextSibling;
    end;
  finally
    DocFileCache.Free;
  end;
end;

function TEpiValueLabelSets.NewValueLabelSet(ALabelType: TEpiFieldType
  ): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(NewItem(TEpiValueLabelSet));
  result.LabelType := ALabelType;
  result.ItemOwner := true;
end;

function TEpiValueLabelSets.GetValueLabelSetByName(const AName: string
  ): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(GetItemByName(AName));
end;

function TEpiValueLabelSets.GetEnumerator: TEpiValueLabelSetsEnumerator;
begin
  result := TEpiValueLabelSetsEnumerator.Create(Self);
end;

end.

