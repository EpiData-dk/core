unit epitextlabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  { TEpiTextLabels }

  TEpiTextLabels = class(TObject)
  private
    FOwned: Boolean;
    FOwner:     TEpiDataFile;
    FList:      TFPList;
    FReportOnChange: Boolean;
    function    GetCount: Cardinal;
    function    GetTextLabel(Index: integer): TEpiTextLabel;
  public
    constructor Create(AOwner: TEpiDataFile); virtual;
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    procedure   Add(aTextLabel: TEpiTextLabel);
    procedure   Delete(aTextLabel: TEpiTextLabel);
    function    TextLabelById(Const aTextLabelId: string): TEpiTextLabel;
    function    TextLabelExists(Const aTextLabelId: string): boolean;
    function    IndexOf(Const aTextLabelId: string): integer;
    Property    TextLabel[Index: integer]: TEpiTextLabel read GetTextLabel; default;
    Property    Count: Cardinal read GetCount;
    Property    ReportOnChange: Boolean read FReportOnChange write FReportOnChange;
    Property    Owned: Boolean read FOwned write FOwned;
  end;

  { TEpiTextLabel }

  TEpiTextLabel = class(TObject)
  private
    FOwner:      TEpiTextLabels;
    FId:         string;
    FText:       string;
    FScreenProp: TEpiScreenProperty;
    FTextLeft: Integer;
    FTextTop: Integer;
    procedure SetId(const AValue: string);
    procedure SetText(const AValue: string);
    procedure SetTextLeft(const AValue: Integer);
    procedure SetTextTop(const AValue: Integer);
  public
    constructor Create(AOwner: TEpiTextLabels);
    destructor  Destroy; override;
    // TODO : Clone!
    // TODO : Reset!
    property    Id:         string read FId write SetId;
    property    Text:       string read FText write SetText;
    Property    TextTop:    Integer read FTextTop write SetTextTop;
    Property    TextLeft:   Integer read FTextLeft write SetTextLeft;
    property    ScreenProp: TEpiScreenProperty read FScreenProp write FScreenProp;
  private
    // Events (and control):
    FOnChange:     ^TEpiTextLabelChangeEvent;
    FOnChangeCount: Integer;
    FUpdateCount:   Integer;
    procedure DoChange(EventType: TEpiTextLabelChangeEventType; OldValue: Pointer);
  public
    // Events (and control):
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
    procedure UnRegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
  end;

implementation

{ TEpiTextLabels }

function TEpiTextLabels.GetCount: Cardinal;
begin
  result := FList.Count;
end;

function TEpiTextLabels.GetTextLabel(Index: integer): TEpiTextLabel;
begin
  result := TEpiTextLabel(FList[Index]);
end;

constructor TEpiTextLabels.Create(AOwner: TEpiDataFile);
begin
  FList := TFPList.Create;
  FOwner := AOwner;
  FReportOnChange := false;
end;

destructor TEpiTextLabels.Destroy;
var
  T: TEpiTextLabel;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      T := TEpiTextLabel(FList.Last);
      FreeAndNil(T);
    end;
    Flist.Delete(Flist.Count - 1);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEpiTextLabels.Add(aTextLabel: TEpiTextLabel);
begin
  FList.Add(aTextLabel);

  if Owned then
    aTextLabel.FOwner := Self;

  if ReportOnChange and Assigned(FOwner) then
    FOwner.DoChange(dceAddText, aTextLabel);
end;

procedure TEpiTextLabels.Delete(aTextLabel: TEpiTextLabel);
var
  Idx: LongInt;
begin
  Idx := IndexOf(aTextLabel.Id);
  FList.Delete(Idx);

  if Owned then
    aTextLabel.FOwner := nil;
  if ReportOnChange and Assigned(FOwner) then
    FOwner.DoChange(dceRemoveText, aTextLabel);
end;

function TEpiTextLabels.TextLabelById(const aTextLabelId: string): TEpiTextLabel;
var
  i: LongInt;
begin
  Result := nil;
  i := IndexOf(aTextLabelId);
  if i >= 0 then
    result := TEpiTextLabel(FList[i]);
end;

function TEpiTextLabels.TextLabelExists(const aTextLabelId: string): boolean;
begin
  result := Assigned(TextLabelById(aTextLabelId));
end;

function TEpiTextLabels.IndexOf(const aTextLabelId: string): integer;
begin
  for result := 0 to FList.Count - 1 do
    if CompareText(TEpiTextLabel(Flist[result]).Id, aTextLabelId) = 0 then
      exit;
  result := -1;
end;

{ TEpiTextLabel }

procedure TEpiTextLabel.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(tceId, @Val);
end;

procedure TEpiTextLabel.SetText(const AValue: string);
var
  Val: String;
begin
  if FText = AValue then exit;
  Val := FText;
  FText := AValue;
  DoChange(tceText, @Val);
end;

procedure TEpiTextLabel.SetTextLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if FTextLeft = AValue then exit;
  Val := FTextLeft;
  FTextLeft := AValue;
  DoChange(tceLeft, @Val);
end;

procedure TEpiTextLabel.SetTextTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if FTextTop = AValue then exit;
  Val := FTextTop;
  FTextTop := AValue;
  DoChange(tceTop, @Val);
end;

constructor TEpiTextLabel.Create(AOwner: TEpiTextLabels);
begin
  FOwner := AOwner;
end;

destructor TEpiTextLabel.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiTextLabel.DoChange(EventType: TEpiTextLabelChangeEventType;
  OldValue: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeCount - 1 do
    FOnChange[i](Self, EventType, OldValue);
end;

procedure TEpiTextLabel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiTextLabel.EndUpdate;
begin
  Dec(FUpdateCount);

  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(tceUpdate, nil);
end;

procedure TEpiTextLabel.RegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
begin
  Inc(FOnChangeCount);
  ReAllocMem(FOnChange, FOnChangeCount * SizeOf(TEpiTextLabelChangeEvent));
  FOnChange[FOnChangeCount-1] := Event
end;

procedure TEpiTextLabel.UnRegisterOnChangeHook(Event: TEpiTextLabelChangeEvent
  );
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeCount -1 do
  begin
    if FOnChange[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeCount then exit;

  dec(FOnChangeCount);
  if FOnChangeCount > Idx then
    System.Move(FOnChange[Idx+1], FOnChange[Idx], (FOnChangeCount-Idx)*SizeOf(TEpiFieldChangeEvent));
  ReAllocMem(FOnChange, FOnChangeCount*SizeOf(TEpiFieldChangeEvent));
end;

end.

