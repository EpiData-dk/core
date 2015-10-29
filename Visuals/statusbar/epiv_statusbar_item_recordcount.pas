unit epiv_statusbar_item_recordcount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, StdCtrls;

type

  { TEpiVStatusBarItem_RecordCount }

  TEpiVStatusBarItem_RecordCount = class(TEpiVCustomStatusBarItem)
  private
    FExampleValue: Integer;
    FLabel: TLabel;
    FRecordsLabel: TLabel;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DoUpdate(Example: Boolean = false);
  protected
    procedure Update(Condition: TEpiVCustomStatusbarUpdateCondition); override;
  public
    class function Caption: string; override;
    class function Name: string; override;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); override;
    function GetPreferedWidth: Integer; override;
  end;

implementation

uses
  Controls;

{ TEpiVStatusBarItem_RecordCount }

procedure TEpiVStatusBarItem_RecordCount.MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if WheelDelta > 0 then
    Inc(FExampleValue)
  else
    Dec(FExampleValue);

  if (FExampleValue < 0) then
    FExampleValue := 0;
  if (FExampleValue > 9999) then
    FExampleValue := 9999;

  DoUpdate(true);
end;

procedure TEpiVStatusBarItem_RecordCount.DoUpdate(Example: Boolean);
begin
  if Example then
    FRecordsLabel.Caption := IntToStr(FExampleValue)
  else
    if Assigned(Statusbar.Datafile) then
      IntToStr(Statusbar.Datafile.Size);
end;

procedure TEpiVStatusBarItem_RecordCount.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: DoUpdate;
    sucDocFile: ;
    sucDataFile: DoUpdate;
    sucSelection: ;
    sucSave: ;
    sucExample:
      begin
        FExampleValue := 42;
        Panel.OnMouseWheel := @MouseWheel;
        FLabel.OnMouseWheel := @MouseWheel;
        FRecordsLabel.OnMouseWheel := @MouseWheel;
        DoUpdate(true);
      end;
  end;
end;

class function TEpiVStatusBarItem_RecordCount.Caption: string;
begin
  Result := 'Total count of Records';
end;

class function TEpiVStatusBarItem_RecordCount.Name: string;
begin
  Result := 'RecourdCount';
end;

constructor TEpiVStatusBarItem_RecordCount.Create(
  AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

  FLabel := TLabel.Create(Panel);
  FLabel.AnchorParallel(akLeft, 2, Panel);
  FLabel.AnchorVerticalCenterTo(Panel);
  FLabel.Caption := 'Records: ';
  FLabel.Parent := Panel;

  FRecordsLabel := TLabel.Create(Panel);
  FRecordsLabel.AnchorToNeighbour(akLeft, 2, FLabel);
  FRecordsLabel.AnchorVerticalCenterTo(Panel);
  FRecordsLabel.Caption := '0';
  FRecordsLabel.Parent := Panel;
end;

function TEpiVStatusBarItem_RecordCount.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FRecordsLabel.Left + FRecordsLabel.Width + 2;
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_RecordCount);

end.

