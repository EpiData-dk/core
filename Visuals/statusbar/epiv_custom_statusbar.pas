unit epiv_custom_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, contnrs, epiopenfile,
  epidatafiles, epicustombase;

type

  TEpiVCustomStatusbarUpdateCondition = (
    sucDefault,     // Regular update requested from program
    sucCustom,      // Specieal update performed by a descending class.
    sucDocFile,     // Updated docfile
    sucDataFile,    // Updated datafile
    sucSelection,   // Updated selection
    sucSave         // Project was saved
  );

  TEpiVCustomStatusBarItem = class;

  { TEpiVCustomStatusBar }

  TEpiVCustomStatusBar = class(TCustomPanel)
  private
    FItemList: TObjectList;
    FResizableItemsCount: Integer;
    FInterItemSpace: Integer;
    FDocFile: TEpiDocumentFile;
    FDatafile: TEpiDataFile;
    FSelection: TEpiCustomList;
    procedure   SetInterItemSpace(AValue: Integer);
    procedure   SetDocFile(AValue: TEpiDocumentFile);
    procedure   SetDatafile(AValue: TEpiDataFile);
    procedure   SetSelection(AValue: TEpiCustomList);
    procedure   DoUpdateItems(Condition: TEpiVCustomStatusbarUpdateCondition);
  protected
    procedure   DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure   AddItem(StatusBarItem: TEpiVCustomStatusBarItem); virtual;
    procedure   Clear; virtual;
    procedure   Resize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure   Update(Condition: TEpiVCustomStatusbarUpdateCondition = sucDefault);
    property    InterItemSpace: Integer read FInterItemSpace write SetInterItemSpace;
    property    DocFile: TEpiDocumentFile read FDocFile write SetDocFile;
    property    Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property    Selection: TEpiCustomList read FSelection write SetSelection;
  end;

  { TEpiVCustomStatusBarItem }

  TEpiVCustomStatusBarItem = class
  private
    FResizable: Boolean;
    FPanel: TCustomPanel;
    FStatusBar: TEpiVCustomStatusBar;
    FVisible: Boolean;
  protected
    procedure  Update(Condition: TEpiVCustomStatusbarUpdateCondition); virtual;
    procedure  SetVisible(AValue: Boolean); virtual;
    property   Panel: TCustomPanel read FPanel;
    property   Statusbar: TEpiVCustomStatusBar read FStatusBar;
  public
    class function Caption: string;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); virtual;
    function    GetPreferedWidth: Integer; virtual;
    property    Resizable: Boolean read FResizable write FResizable;
    property    Visible: Boolean read FVisible write SetVisible;
  end;
  TEpiVCustomStatusBarItemClass = class of TEpiVCustomStatusBarItem;


procedure EpiV_RegisterCustomStatusBarItem(Const CustomStatusBarItemClass: TEpiVCustomStatusBarItemClass);
function  EpiV_GetCustomStatusBarItems: TList;

implementation

uses
  Controls, Graphics;

var
  FCustomStatusBarItemList: TList = nil;

procedure EpiV_RegisterCustomStatusBarItem(
  const CustomStatusBarItemClass: TEpiVCustomStatusBarItemClass);
begin
  if (not Assigned(FCustomStatusBarItemList)) then
    FCustomStatusBarItemList := TList.Create;

  FCustomStatusBarItemList.Add(CustomStatusBarItemClass);
end;

function EpiV_GetCustomStatusBarItems: TList;
begin
  result := FCustomStatusBarItemList;
end;

{ TEpiVCustomStatusBar }

procedure TEpiVCustomStatusBar.SetInterItemSpace(AValue: Integer);
begin
  if FInterItemSpace = AValue then Exit;
  FInterItemSpace := AValue;

  Resize;
end;

procedure TEpiVCustomStatusBar.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;
  DoUpdateItems(sucDataFile);
end;

procedure TEpiVCustomStatusBar.DoUpdateItems(
  Condition: TEpiVCustomStatusbarUpdateCondition);
var
  I: Integer;
begin
  for I := 0 to FItemList.Count - 1 do
    TEpiVCustomStatusBarItem(FItemList[i]).Update(Condition);
end;

procedure TEpiVCustomStatusBar.SetSelection(AValue: TEpiCustomList);
begin
  if FSelection = AValue then Exit;
  FSelection := AValue;
  DoUpdateItems(sucSelection);
end;

procedure TEpiVCustomStatusBar.SetDocFile(AValue: TEpiDocumentFile);
begin
  if FDocFile = AValue then Exit;
  FDocFile := AValue;

  DoUpdateItems(sucDocFile);
end;

procedure TEpiVCustomStatusBar.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer
  );
begin
  AHeight := 30;
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
end;

constructor TEpiVCustomStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FItemList := TObjectList.create(false);
  FResizableItemsCount := 0;
  FInterItemSpace      := 2;

  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TEpiVCustomStatusBar.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  DoUpdateItems(Condition);
  Resize;
end;

procedure TEpiVCustomStatusBar.AddItem(StatusBarItem: TEpiVCustomStatusBarItem);
begin
  // Code to side-by-side setting the items.
  BeginAutoSizing;

  StatusBarItem.Panel.Parent := Self;

  if FItemList.Count = 0 then
    StatusBarItem.Panel.AnchorParallel(akLeft, 0, Self)
  else
    StatusBarItem.Panel.AnchorToNeighbour(akLeft, FInterItemSpace, TEpiVCustomStatusBarItem(FItemList.Last).Panel);

  StatusBarItem.Panel.AnchorParallel(akTop,    2, Self);
  StatusBarItem.Panel.AnchorParallel(akBottom, 2, Self);

  EndAutoSizing;

  if StatusBarItem.Resizable then
    Inc(FResizableItemsCount);

  FItemList.Add(StatusBarItem);
end;

procedure TEpiVCustomStatusBar.Clear;
var
  I: Integer;
begin
  BeginAutoSizing;

  for I := 0 to FItemList.Count - 1 do
    FItemList[i].Free;

  FItemList.Clear;
end;

procedure TEpiVCustomStatusBar.Resize;
var
  TotalFixedWidth, ResizableWidth, I, ItemCount: Integer;
begin
  inherited Resize;
  if AutoSizeDelayed then exit;

  TotalFixedWidth := 0;
  ItemCount := 0;
  for I := 0 to FItemList.Count - 1 do
    with TEpiVCustomStatusBarItem(FItemList[i]) do
    begin
      if (not Resizable) and (Visible) then
        Inc(TotalFixedWidth, GetPreferedWidth);
      if (Visible) then
        Inc(ItemCount);
    end;

  if FResizableItemsCount > 0 then
    ResizableWidth := (ClientWidth - TotalFixedWidth - (FInterItemSpace * (ItemCount - 1))) div FResizableItemsCount;

  for I := 0 to FItemList.Count - 1 do
    with TEpiVCustomStatusBarItem(FItemList[i]) do
      if Resizable then
        Panel.Width := ResizableWidth
      else if (Visible) then
        Panel.Width := GetPreferedWidth;
end;

{ TEpiVCustomStatusBarItem }

class function TEpiVCustomStatusBarItem.Caption: string;
begin
  result := 'Caption not overridden for: ' + ClassName;
end;

constructor TEpiVCustomStatusBarItem.Create(AStatusBar: TEpiVCustomStatusBar);
begin
  FPanel := TCustomPanel.Create(StatusBar);
  with FPanel do
    begin
      Color := clWhite;
      BevelInner := bvLowered;
      BevelOuter := bvNone;
    end;
  FStatusBar := AStatusBar;

  FResizable := false;
  FVisible   := true;
end;

function TEpiVCustomStatusBarItem.GetPreferedWidth: Integer;
begin
  Result := 10;
end;

procedure TEpiVCustomStatusBarItem.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;

  Panel.Visible := FVisible;
  Statusbar.Update(sucDefault);
end;

procedure TEpiVCustomStatusBarItem.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  //
end;

finalization
  FCustomStatusBarItemList.Free;

end.

