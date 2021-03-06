unit epiv_custom_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, contnrs, epiopenfile,
  epidatafiles, epicustombase, LMessages;

type

  TEpiVCustomStatusbarUpdateCondition = (
    sucDefault,     // Regular update requested from program
    sucDocFile,     // Updated docfile
    sucDataFile,    // Updated datafile
    sucSelection,   // Updated selection
    sucSave,        // Project was saved
    sucExample      // All items should display an example, not using the statusbar/datafile, etc...
  );

  TEpiVCustomStatusBarItem = class;

  { TEpiVCustomStatusBar }

  TEpiVCustomStatusBar = class(TCustomPanel)
  private
    FItemList: TStringList;
    FInterItemSpace: Integer;
    FDocFile: TEpiDocumentFile;
    FDatafile: TEpiDataFile;
    FSelection: TEpiCustomList;
    procedure   SetInterItemSpace(AValue: Integer);
    procedure   SetDocFile(AValue: TEpiDocumentFile);
    procedure   SetDatafile(AValue: TEpiDataFile);
    procedure   SetSelection(AValue: TEpiCustomList);
    procedure   DoUpdateItems(Condition: TEpiVCustomStatusbarUpdateCondition);
    function GetCount: Integer;
    function GetStatusbarItem(const Index: Integer): TEpiVCustomStatusBarItem;
  protected
    procedure   DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure   AddItem(AStatusBarItem: TEpiVCustomStatusBarItem); virtual;
    procedure   Clear; virtual;
    procedure   Resize; override;
    property    Count: Integer read GetCount;
    property    StatusbarItem[Const Index: Integer]: TEpiVCustomStatusBarItem read GetStatusbarItem;
    property    StatusbarItems: TStringList read FItemList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update(Condition: TEpiVCustomStatusbarUpdateCondition = sucDefault);
    procedure   IsShortCut(var Msg: TLMKey; var Handled: Boolean);
    property    InterItemSpace: Integer read FInterItemSpace write SetInterItemSpace;
    property    DocFile: TEpiDocumentFile read FDocFile write SetDocFile;
    property    Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property    Selection: TEpiCustomList read FSelection write SetSelection;
  end;

  { TEpiVCustomStatusBarItem }

  TEpiVCustomStatusBarItem = class
  private
    FResizable: Boolean;
    FPanel: TPanel;
    FStatusBar: TEpiVCustomStatusBar;
    FVisible: Boolean;
  protected
    procedure  IsShortCut(var Msg: TLMKey; var Handled: Boolean); virtual;
    procedure  SetVisible(AValue: Boolean); virtual;
    property   Statusbar: TEpiVCustomStatusBar read FStatusBar;
  public
    class function Caption: string; virtual;
    class function Name: string; virtual; abstract;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); virtual;
    destructor  Destroy; override;
    function    GetPreferedWidth: Integer; virtual;
    procedure   Update(Condition: TEpiVCustomStatusbarUpdateCondition); virtual;
    property    Resizable: Boolean read FResizable write FResizable;
    property    Panel: TPanel read FPanel;
    property    Visible: Boolean read FVisible write SetVisible;
  end;
  TEpiVCustomStatusBarItemClass = class of TEpiVCustomStatusBarItem;


procedure EpiV_RegisterCustomStatusBarItem(Const CustomStatusBarItemClass: TEpiVCustomStatusBarItemClass);
function  EpiV_GetCustomStatusBarItems: TStringList;

implementation

uses
  Controls, Graphics;

var
  FCustomStatusBarItemList: TStringList = nil;

procedure EpiV_RegisterCustomStatusBarItem(
  const CustomStatusBarItemClass: TEpiVCustomStatusBarItemClass);
begin
  if (not Assigned(FCustomStatusBarItemList)) then
    FCustomStatusBarItemList := TStringList.Create;

  if FCustomStatusBarItemList.IndexOf(CustomStatusBarItemClass.Name) >= 0 then
    Raise Exception.Create(
      'Duplicate CustomStatusbarItemClass names:' + LineEnding +
      ' Name:      ' + CustomStatusBarItemClass.Name + LineEnding +
      ' ClassName: ' + CustomStatusBarItemClass.ClassName
      );

  FCustomStatusBarItemList.AddObject(CustomStatusBarItemClass.Name, TObject(CustomStatusBarItemClass));
end;

function EpiV_GetCustomStatusBarItems: TStringList;
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
    TEpiVCustomStatusBarItem(FItemList.Objects[i]).Update(Condition);
end;

function TEpiVCustomStatusBar.GetCount: Integer;
begin
  result := FItemList.Count;
end;

function TEpiVCustomStatusBar.GetStatusbarItem(const Index: Integer
  ): TEpiVCustomStatusBarItem;
begin
  result := TEpiVCustomStatusBarItem(FItemList.Objects[Index]);
end;

procedure TEpiVCustomStatusBar.SetSelection(AValue: TEpiCustomList);
begin
//  if FSelection = AValue then Exit;
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

  FItemList := TStringList.Create;
  FInterItemSpace      := 2;

  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TEpiVCustomStatusBar.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TEpiVCustomStatusBar.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  DoUpdateItems(Condition);
  Resize;
end;

procedure TEpiVCustomStatusBar.IsShortCut(var Msg: TLMKey; var Handled: Boolean
  );
var
  I: Integer;
begin
  for I := 0 to FItemList.Count - 1 do
  begin
    TEpiVCustomStatusBarItem(FItemList.Objects[i]).IsShortCut(Msg, Handled);
    if Handled then Exit;
  end;
end;

procedure TEpiVCustomStatusBar.AddItem(AStatusBarItem: TEpiVCustomStatusBarItem
  );
begin
  // Code to side-by-side setting the items.
  DisableAutoSizing;

  AStatusBarItem.Panel.Parent := Self;

  if FItemList.Count = 0 then
    AStatusBarItem.Panel.AnchorParallel(akLeft, 0, Self)
  else
    AStatusBarItem.Panel.AnchorToNeighbour(akLeft, FInterItemSpace, TEpiVCustomStatusBarItem(FItemList.Objects[Count - 1]).Panel);

  AStatusBarItem.Panel.AnchorParallel(akTop,    2, Self);
  AStatusBarItem.Panel.AnchorParallel(akBottom, 2, Self);

  EnableAutoSizing;

  FItemList.AddObject(AStatusBarItem.Name, AStatusBarItem);
end;

procedure TEpiVCustomStatusBar.Clear;
var
  I: Integer;
begin
  DisableAutoSizing;

  for I := 0 to FItemList.Count - 1 do
    FItemList.Objects[i].Free;

  FItemList.Clear;

  EnableAutoSizing;
end;

procedure TEpiVCustomStatusBar.Resize;
var
  TotalFixedWidth, ResizableWidth, I, ItemCount, ResizableItemsCount: Integer;
begin
  inherited Resize;
  if AutoSizeDelayed then exit;

  TotalFixedWidth := 0;
  ItemCount := 0;
  ResizableItemsCount := 0;

  for I := 0 to FItemList.Count - 1 do
    with TEpiVCustomStatusBarItem(FItemList.Objects[i]) do
    begin
      if (not Resizable) and (Visible) then
        Inc(TotalFixedWidth, GetPreferedWidth);

      if (Visible) then
        begin
          Inc(ItemCount);
          if (Resizable) then
            Inc(ResizableItemsCount);
        end;
    end;

  if ResizableItemsCount > 0 then
    ResizableWidth := (ClientWidth - TotalFixedWidth - (FInterItemSpace * (ItemCount - 1))) div ResizableItemsCount;

  for I := 0 to FItemList.Count - 1 do
    with TEpiVCustomStatusBarItem(FItemList.Objects[i]) do
      if Resizable then
        Panel.SetBoundsKeepBase(Panel.Left, Panel.Top, ResizableWidth, Panel.Height)
//        Panel.Width := ResizableWidth
      else if (Visible) then
        Panel.SetBoundsKeepBase(Panel.Left, Panel.Top, GetPreferedWidth, Panel.Height);
//        Panel.Width := GetPreferedWidth;
end;

{ TEpiVCustomStatusBarItem }

class function TEpiVCustomStatusBarItem.Caption: string;
begin
  result := 'Caption not overridden for: ' + ClassName;
end;

constructor TEpiVCustomStatusBarItem.Create(AStatusBar: TEpiVCustomStatusBar);
begin
  FPanel := TPanel.Create(StatusBar);
  with FPanel do
    begin
      Color := clDefault;
      BevelInner := bvLowered;
      BevelOuter := bvNone;
    end;
  FStatusBar := AStatusBar;

  FResizable := false;
  FVisible   := true;
end;

destructor TEpiVCustomStatusBarItem.Destroy;
begin
  FPanel.Free;
  inherited Destroy;
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

procedure TEpiVCustomStatusBarItem.IsShortCut(var Msg: TLMKey;
  var Handled: Boolean);
begin
  Handled := false;
end;

procedure TEpiVCustomStatusBarItem.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  //
end;

finalization
  FCustomStatusBarItemList.Free;

end.

