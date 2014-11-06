unit epirelates;

{$mode objfpc}{$H+}

interface

uses
  Classes, epicustombase, Laz2_DOM;

type

  TEpiCustomRelate = class;
  TEpiRelate = class;

  { TEpiCustomRelates }

  TEpiCustomRelates = class(TEpiCustomList)
  private
    function GetRelate(Index: Integer): TEpiCustomRelate;
  protected
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
  public
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function NewRelate: TEpiCustomRelate; virtual;
  protected
    property Relate[Index: Integer]: TEpiCustomRelate read GetRelate; default;
  end;

  { TEpiRelates }

  TEpiRelates = class(TEpiCustomRelates)
  private
    function GetRelate(Index: Integer): TEpiRelate;
  public
    function ItemClass: TEpiCustomItemClass; override;
    function XMLName: string; override;
    function NewRelate: TEpiRelate; reintroduce;
    property Relate[Index: Integer]: TEpiRelate read GetRelate; default;
  end;

  { TEpiCustomRelate }

  TEpiCustomRelate = class(TEpiCustomItem)
  private
    FDetailRelation: TEpiCustomItem;
    procedure RelationHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure SetDetailRelation(AValue: TEpiCustomItem);
    procedure UpdateRelationHook(Const Old, New: TEpiCustomItem);
  protected
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
       ReferenceType: Byte; const ReferenceId: string); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    function WriteNameToXml: boolean; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
  protected
    property DetailRelation: TEpiCustomItem read FDetailRelation write SetDetailRelation;
  end;

  TEpiRelate = class(TEpiCustomRelate)
  public
    function XMLName: string; override;
    property DetailRelation;
  end;

implementation

uses
  epirelations, sysutils, epidocument;

{ TEpiRelates }

function TEpiRelates.GetRelate(Index: Integer): TEpiRelate;
begin
  result := TEpiRelate(inherited GetRelate(Index));
end;

function TEpiRelates.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiRelate;
end;

function TEpiRelates.XMLName: string;
begin
  Result := rsRelates;
end;

function TEpiRelates.NewRelate: TEpiRelate;
begin
  Result := TEpiRelate(inherited NewRelate);
end;

{ TEpiCustomRelates }

function TEpiCustomRelates.GetRelate(Index: Integer): TEpiCustomRelate;
begin
  result := TEpiCustomRelate(Items[Index]);
end;

procedure TEpiCustomRelates.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  // Not my child!
  if IndexOf(TEpiCustomItem(Initiator)) < 0  then exit;

  if (EventGroup <> eegCustomBase) then exit;
  if (EventType  <> Word(ecceReferenceDestroyed)) then exit;

  RemoveItem(TEpiCustomItem(Initiator));
  Initiator.Free;
end;

function TEpiCustomRelates.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
end;

constructor TEpiCustomRelates.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiCustomRelates.NewRelate: TEpiCustomRelate;
begin
  result := TEpiCustomRelate(NewItem());
end;

{ TEpiCustomRelate }

procedure TEpiCustomRelate.RelationHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator <> DetailRelation) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (EventType <> Word(ecceDestroy)) then exit;

  Data := DetailRelation;
  DetailRelation := nil;
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Data);
end;

procedure TEpiCustomRelate.SetDetailRelation(AValue: TEpiCustomItem);
var
  Val: TEpiCustomItem;
begin
  if Assigned(AValue) and
     (not AValue.InheritsFrom(TEpiCustomItem))
  then
    Raise Exception.Create('TEpiRelate.SetDetailRelation only acception object of type TEpiCustomItem!');

  if FDetailRelation = AValue then Exit;
  UpdateRelationHook(FDetailRelation, AValue);

  Val := FDetailRelation;
  FDetailRelation := AValue;
//  DoChange(eegCustomBase, erceDetailRelation, @Val);
end;

procedure TEpiCustomRelate.UpdateRelationHook(const Old, New: TEpiCustomItem
  );
begin
  if Assigned(Old) then
    Old.UnRegisterOnChangeHook(@RelationHook);

  if Assigned(New) then
    New.RegisterOnChangeHook(@RelationHook, true);
end;

procedure TEpiCustomRelate.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
var
  Obj: TEpiCustomBase;
  Doc: TEpiDocument;
begin
  if (EpiClassType = TEpiCustomRelate)
  then
    begin
      Obj := RootOwner;
      if not (Obj is TEpiDocument) then exit;
      Doc := TEpiDocument(Obj);

      case ReferenceType of

        0: // DetailRelation
          DetailRelation := TEpiCustomItem(Doc.Relations.GetItemByName(ReferenceId));
      end;

    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiCustomRelate.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  if Assigned(DetailRelation) then
    ReferenceMap.AddFixupReference(Result, TEpiCustomRelate, 0, DetailRelation.Name);
end;

function TEpiCustomRelate.WriteNameToXml: boolean;
begin
  Result := false;
end;

function TEpiCustomRelate.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsRelationRef, DetailRelation.Name);
end;

constructor TEpiCustomRelate.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiCustomRelate.Destroy;
begin
  UpdateRelationHook(DetailRelation, nil);
  inherited Destroy;
end;

procedure TEpiCustomRelate.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiCustomRelate, 0, LoadAttrString(Root, rsRelationRef));
end;

procedure TEpiCustomRelate.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
end;

{ TEpiRelate }

function TEpiRelate.XMLName: string;
begin
  Result := rsRelate;
end;

end.

