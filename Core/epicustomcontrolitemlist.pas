unit epicustomcontrolitemlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epicustomitemlist, epicustomitem,
  epicustomcontrolitem;

type

  { TEpiCustomControlItemList }
  TEpiCustomControlItemListEnumerator = class;
  TEpiCustomControlItemList = class(TEpiCustomItemList)
  private
    procedure ChangeHook(Const Sender: TEpiCustomBase;
       Const Initiator: TEpiCustomBase;
       EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  protected
    procedure DoSort; override;
    procedure DoChange(const Initiator: TEpiCustomBase;
       EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
       overload;
  public
    procedure InsertItem(const Index: integer; Item: TEpiCustomItem); override;
    function  DeleteItem(Index: integer): TEpiCustomItem; override;
    function GetEnumerator: TEpiCustomControlItemListEnumerator;
  end;

  { TEpiCustomControlItemListEnumerator }

  TEpiCustomControlItemListEnumerator = class(TEpiCustomItemListEnumerator)
  protected
    function GetCurrent: TEpiCustomControlItem; override;
  public
    property Current: TEpiCustomControlItem read GetCurrent;
  end;


implementation

{ TEpiCustomControlItemList }

function SortControlItems(Item1, Item2: Pointer): Integer;
var
  CI1: TEpiCustomControlItem absolute Item1;
  CI2: TEpiCustomControlItem absolute Item2;
begin
  // The same pointers is also the same object!
  if CI1.Equals(CI2) then exit(0);

  result := CI1.Top - CI2.Top;
  if result = 0 then
    result := CI1.Left - CI2.Left;

  // If two ControlItems are placed on eachother - the "highest" is the biggest pointer value.
  if result = 0 then
    result := Item1 - Item2;
end;

procedure TEpiCustomControlItemList.ChangeHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  EpiInitiator: TEpiCustomControlItem absolute Initiator;
begin
  if (EventGroup <> eegCustomBase) then exit;
  if not (Initiator is TEpiCustomControlItem) then exit;
  if IndexOf(EpiInitiator) = -1 then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceSetTop:  Sort;
    ecceSetLeft: Sort;
  end;
end;

procedure TEpiCustomControlItemList.DoSort;
begin
  if not Assigned(OnSort) then
    OnSort := @SortControlItems;

  inherited DoSort;

  if OnSort = @SortControlItems then
    OnSort := nil;
end;

procedure TEpiCustomControlItemList.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);
  ChangeHook(Self, Initiator, EventGroup, EventType, Data);
end;

procedure TEpiCustomControlItemList.InsertItem(const Index: integer;
  Item: TEpiCustomItem);
begin
  inherited InsertItem(Index, Item);
  if not ItemOwner then
    Item.RegisterOnChangeHook(@ChangeHook, true);
end;

function TEpiCustomControlItemList.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := inherited DeleteItem(Index);
  Result.UnRegisterOnChangeHook(@ChangeHook);
end;

function TEpiCustomControlItemList.GetEnumerator: TEpiCustomControlItemListEnumerator;
begin
  result := TEpiCustomControlItemListEnumerator.Create(Self);
end;

{ TEpiCustomControlItemListEnumerator }

function TEpiCustomControlItemListEnumerator.GetCurrent: TEpiCustomControlItem;
begin
  result := TEpiCustomControlItem(inherited GetCurrent);
end;

end.

