unit epicustomcontrolitem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, epicustomitem;

type
  { TEpiCustomControlItem }

  TEpiCustomControlItem = class(TEpiCustomItem)
  private
    FLeft: integer;
    FTop: integer;
  protected
    function   SaveAttributesToXml: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure  SetLeft(const AValue: Integer); virtual;
    procedure  SetTop(const AValue: Integer); virtual;
    procedure  Assign(const AEpiCustomBase: TEpiCustomBase); override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    property   Left: Integer read FLeft write SetLeft;
    property   Top: Integer read FTop write SetTop;
  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;
  TEpiCustomControlItemClass = class of TEpiCustomControlItem;


implementation

{ TEpiCustomControlItem }

function TEpiCustomControlItem.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttr(rsTop, Top) +
    SaveAttr(rsLeft, Left);
end;

procedure TEpiCustomControlItem.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  Top := LoadAttrInt(Root, rsTop);
  Left := LoadAttrInt(Root, rsLeft);
end;

procedure TEpiCustomControlItem.SetLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if Left = AValue then exit;
  Val := Left;
  FLeft := AValue;
  DoChange(eegCustomBase, Word(ecceSetLeft), @Val);
end;

procedure TEpiCustomControlItem.SetTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if Top = AValue then exit;
  Val := Top;
  FTop := AValue;
  DoChange(eegCustomBase, Word(ecceSetTop), @Val);
end;

procedure TEpiCustomControlItem.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgControlItem: TEpiCustomControlItem absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  if Top = 0 then
    Top := OrgControlItem.Top;
  if Left = 0 then
    Left := OrgControlItem.Left;
  EndUpdate;
end;

function TEpiCustomControlItem.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  Result.SetAttribute(rsTop, IntToStr(Top));
  Result.SetAttribute(rsLeft, IntToStr(Left));
end;

function TEpiCustomControlItem.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiCustomControlItem(Result) do
  begin
    FTop  := Self.FTop;
    FLeft := Self.FLeft;
  end;
end;

end.

