unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type

  { TEpiCustomBase }
  TEpiEventGroup = (eegCustomBase);
  TEpiCustomChangeEventType = (eceUpdate);
  TEpiChangeEvent = procedure(Sender: TObject; EventGroup: Word; EventType: Word; Data: Pointer) of object;

  TEpiCustomBase = class
  private
    FOwner: TEpiCustomBase;
  protected
    constructor Create(AOwner: TEpiCustomBase); virtual;
    Function    Ins(Level: integer): string;
  public
    procedure  SaveToStream(St: TStream; Lvl: integer); virtual;
    procedure  LoadFromXml(Root: TDOMNode); virtual;
  private
    // OnChange-hook privates
    FOnChangeList: ^TEpiChangeEvent;
    FOnChangeListCount: Integer;
    FUpdateCount: Integer;
  protected
    procedure  DoChange(EventGroup: Word; EventType: Word; Data: Pointer);
  public
    // OnChange-hook methods
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate; virtual;
    procedure  RegisterOnChangeHook(Event: TEpiChangeEvent); virtual;
    procedure  UnRegisterOnChangeHook(Event: TEpiChangeEvent); virtual;
  end;

  { TEpiCustomItem }

  TEpiCustomItem = class(TEpiCustomBase)
  private

  protected
    constructor Create(AOwner: TEpiCustomBase); override;
  protected
    FId: string;
    FName: string;
    function GetId: string; virtual;
    function GetName: string;
    procedure SetId(const AValue: string); virtual;
    procedure SetName(const AValue: string);
  public
    property Id: string read GetId write SetId;
    property Name: string read GetName write SetName;
  end;

  { TEpiCustomList }

  TEpiCustomList = class(TEpiCustomBase)
  private
    FList: TFPList;
  protected
     constructor Create(AOwner: TEpiCustomBase); override;
  public

  end;

implementation

uses
  StrUtils;
{ TEpiCustomBase }

constructor TEpiCustomBase.Create(AOwner: TEpiCustomBase);
begin
  FOwner := AOwner;
end;

function TEpiCustomBase.Ins(Level: integer): string;
begin
  result := DupeString(' ', Level);
end;

procedure TEpiCustomBase.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  // Base template - should be overridden in descendants.
  S := Ins(LvL) + '<' + ClassName + '>Not Implemented Yet</' + ClassName + '>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiCustomBase.LoadFromXml(Root: TDOMNode);
begin
  // Do nothing - should be overridden in descendants.
end;

procedure TEpiCustomBase.DoChange(EventGroup: word; EventType: word; Data: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeListCount - 1 do
    FOnChangeList[i](Self, EventGroup, EventType, Data);
end;

procedure TEpiCustomBase.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiCustomBase.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(word(eegCustomBase), word(eceUpdate), nil);
end;

procedure TEpiCustomBase.RegisterOnChangeHook(Event: TEpiChangeEvent);
begin
  Inc(FOnChangeListCount);
  ReAllocMem(FOnChangeList, FOnChangeListCount * SizeOf(TEpiChangeEvent));
  FOnChangeList[FOnChangeListCount-1] := Event
end;

procedure TEpiCustomBase.UnRegisterOnChangeHook(Event: TEpiChangeEvent);
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeListCount -1 do
  begin
    if FOnChangeList[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeListCount then exit;

  dec(FOnChangeListCount);
  if FOnChangeListCount > Idx then
    System.Move(FOnChangeList[Idx+1],FOnChangeList[Idx],(FOnChangeListCount-Idx)*SizeOf(TEpiChangeEvent));
  ReAllocMem(FOnChangeList, FOnChangeListCount*SizeOf(TEpiChangeEvent));
end;

{ TEpiCustomList }

constructor TEpiCustomList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FList := TFPList.Create;
end;

{ TEpiCustomItem }

constructor TEpiCustomItem.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiCustomItem.GetId: string;
begin
  result := FId;
end;

function TEpiCustomItem.GetName: string;
begin
  result := FName;
end;

procedure TEpiCustomItem.SetId(const AValue: string);
begin
  if FId = AValue then exit;
  
end;

procedure TEpiCustomItem.SetName(const AValue: string);
begin
  if FName = AValue then exit;
  FName := AValue;
end;


end.

