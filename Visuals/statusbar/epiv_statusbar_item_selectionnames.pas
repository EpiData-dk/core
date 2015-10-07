unit epiv_statusbar_item_selectionnames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, epicustombase, StdCtrls;

type

  { TEpiVStatusBarItem_SelectionNames }

  TEpiVStatusBarItem_SelectionNames = class(TEpiVCustomStatusBarItem)
  private
    FLocalSelection: TEpiCustomList;
    FNamesLabel: TLabel;
    procedure AssignHooks;
    procedure DoUpdate;
    procedure ItemChangeHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  protected
    procedure Update(Condition: TEpiVCustomStatusbarUpdateCondition); override;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); override;
    destructor Destroy; override;
    function GetPreferedWidth: Integer; override;
  end;

implementation

uses
  Controls;

{ TEpiVStatusBarItem_SelectionNames }

procedure TEpiVStatusBarItem_SelectionNames.AssignHooks;
var
  Item: TEpiCustomItem;
begin
  for Item in FLocalSelection do
    Item.UnRegisterOnChangeHook(@ItemChangeHook);

  FLocalSelection.Clear;

  for Item in Statusbar.Selection do
  begin
    FLocalSelection.AddItem(Item);
    Item.RegisterOnChangeHook(@ItemChangeHook, true);
  end;
end;

procedure TEpiVStatusBarItem_SelectionNames.DoUpdate;
var
  Item: TEpiCustomItem;
  S: String;
begin
  S := '';
  for Item in Statusbar.Selection do
    S := S + Item.Name + ', ';

  Delete(S, Length(S) - 1, 2);

  FNamesLabel.Caption := S;
end;

procedure TEpiVStatusBarItem_SelectionNames.ItemChangeHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegCustomBase) then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy:
      Sender.UnRegisterOnChangeHook(@ItemChangeHook);
    ecceUpdate: ;
    ecceName:
      DoUpdate;
    ecceAddItem: ;
    ecceDelItem: ;
    ecceSetItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceText:
      DoUpdate;
    ecceReferenceDestroyed: ;
    ecceListMove: ;
  end;
end;

procedure TEpiVStatusBarItem_SelectionNames.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: ;
    sucDocFile: ;
    sucDataFile: ;
    sucSelection:
      begin
        AssignHooks;
        DoUpdate;
      end;
    sucSave: ;
  end;
end;

constructor TEpiVStatusBarItem_SelectionNames.Create(
  AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);
  FLocalSelection := TEpiCustomList.Create(nil);
  FLocalSelection.ItemOwner := false;

  Resizable := true;

  FNamesLabel := TLabel.Create(Panel);
  FNamesLabel.AnchorParallel(akLeft, 2, Panel);
  FNamesLabel.AnchorParallel(akRight, 2, Panel);
  FNamesLabel.AnchorVerticalCenterTo(Panel);
  FNamesLabel.Caption := '';
  FNamesLabel.Parent := Panel;
end;

destructor TEpiVStatusBarItem_SelectionNames.Destroy;
var
  Item: TEpiCustomItem;
begin
  for Item in FLocalSelection do
    Item.UnRegisterOnChangeHook(@ItemChangeHook);

  FLocalSelection.Free;
  inherited Destroy;
end;

function TEpiVStatusBarItem_SelectionNames.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FNamesLabel.Left + FNamesLabel.Width + 2;
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_SelectionNames);

end.

