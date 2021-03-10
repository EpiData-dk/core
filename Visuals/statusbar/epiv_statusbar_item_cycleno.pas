unit epiv_statusbar_item_cycleno;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, StdCtrls;

type

  { TEpiVStatusBarItem_CycleNo }

  TEpiVStatusBarItem_CycleNo = class(TEpiVCustomStatusBarItem)
  private
    FExampleValue: Integer;
    FLabel: TLabel;
    FCycleNoLabel: TLabel;
    procedure DoUpdate(Example: boolean = false);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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

{ TEpiVStatusBarItem_CycleNo }

procedure TEpiVStatusBarItem_CycleNo.DoUpdate(Example: boolean);
begin
  if Example then
    begin
      Visible := true;
      FCycleNoLabel.Caption := IntToSTr(FExampleValue)
    end
  else
    if Assigned(Statusbar.DocFile) and
       Assigned(Statusbar.DocFile.Document)
    then
      begin
        Visible := true;
        FCycleNoLabel.Caption := IntToStr(Statusbar.DocFile.Document.CycleNo)
      end
    else
      Visible := false;
end;

procedure TEpiVStatusBarItem_CycleNo.MouseWheel(Sender: TObject;
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

procedure TEpiVStatusBarItem_CycleNo.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: ;
    sucDocFile: DoUpdate;
    sucDataFile: ;
    sucSelection: ;
    sucSave: DoUpdate;
    sucExample:
      begin
        FExampleValue := 9;
        Panel.OnMouseWheel  := @MouseWheel;
        FLabel.OnMouseWheel := @MouseWheel;
        FCycleNoLabel.OnMouseWheel := @MouseWheel;
        DoUpdate(true);
      end;
  end;
end;

class function TEpiVStatusBarItem_CycleNo.Caption: string;
begin
  Result := 'Current Cycle Number';
end;

class function TEpiVStatusBarItem_CycleNo.Name: string;
begin
  result := 'CycleNo';
end;

constructor TEpiVStatusBarItem_CycleNo.Create(AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

//  Visible := false;

  FLabel := TLabel.Create(Panel);
  FLabel.AnchorParallel(akLeft, 2, Panel);
  FLabel.AnchorVerticalCenterTo(Panel);
  FLabel.Caption := 'Cycle No.: ';
  FLabel.Parent := Panel;

  FCycleNoLabel := TLabel.Create(Panel);
  FCycleNoLabel.AnchorToNeighbour(akLeft, 2, FLabel);
  FCycleNoLabel.AnchorVerticalCenterTo(Panel);
  FCycleNoLabel.Caption := '0';
  FCycleNoLabel.Parent := Panel;
end;

function TEpiVStatusBarItem_CycleNo.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FCycleNoLabel.Left + FCycleNoLabel.Width + 2;
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_CycleNo);

end.

