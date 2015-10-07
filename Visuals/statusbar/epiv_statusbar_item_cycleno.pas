unit epiv_statusbar_item_cycleno;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, StdCtrls;

type

  { TEpiVStatusBarItem_CycleNo }

  TEpiVStatusBarItem_CycleNo = class(TEpiVCustomStatusBarItem)
  private
    FLabel: TLabel;
    FCycleNoLabel: TLabel;
    procedure DoUpdate;
  protected
    procedure Update(Condition: TEpiVCustomStatusbarUpdateCondition); override;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); override;
    function GetPreferedWidth: Integer; override;
  end;

implementation

uses
  Controls;

{ TEpiVStatusBarItem_CycleNo }

procedure TEpiVStatusBarItem_CycleNo.DoUpdate;
begin
  FCycleNoLabel.Caption := IntToStr(Statusbar.DocFile.Document.CycleNo);
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
  end;
end;

constructor TEpiVStatusBarItem_CycleNo.Create(AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

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

