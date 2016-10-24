unit epiv_statusbar_item_currentuser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, epiopenfile, StdCtrls, epidocument,
  epicustombase;

type

  { TEpiVStatusBarItem_CurrentUser }

  TEpiVStatusBarItem_CurrentUser = class(TEpiVCustomStatusBarItem)
  private
    FDocument: TEpiDocument;
    FLabel: TLabel;
    FLoginLabel: TLabel;
    FExampleType: byte;
    procedure UpdateHooks;
    procedure DoUpdate;
    procedure BumpExampleType;
    procedure DoExample;
    procedure DocumentChangeEvent(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure MouseClick(Sender: TObject);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure Update(Condition: TEpiVCustomStatusbarUpdateCondition); override;
  public
    class function Caption: string; override;
    class function Name: string; override;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); override;
    destructor Destroy; override;
    function GetPreferedWidth: Integer; override;
  end;

implementation

uses
  Controls;

{ TEpiVStatusBarItem_CurrentUser }

procedure TEpiVStatusBarItem_CurrentUser.MouseClick(Sender: TObject);
begin
  BumpExampleType;
  DoExample;
end;

procedure TEpiVStatusBarItem_CurrentUser.MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  BumpExampleType;
  DoExample;
end;

procedure TEpiVStatusBarItem_CurrentUser.UpdateHooks;
begin
  if Assigned(FDocument) then
    FDocument.RegisterOnChangeHook(@DocumentChangeEvent, false);
end;

procedure TEpiVStatusBarItem_CurrentUser.DoUpdate;
var
  DocFile: TEpiDocumentFile;
begin
  DocFile := Statusbar.DocFile;

  if (Assigned(FDocument) and Assigned(DocFile)) then
  begin

    // Extended Access is used for project
    if Assigned(DocFile.AuthedUser) then
    begin
      Visible := true;
      FLabel.Caption := 'Login: ';
      FLoginLabel.Caption := DocFile.AuthedUser.Login;
    end else
    // Simple password is used for project
    if (FDocument.PassWord <> '') then
    begin
      Visible := true;
      FLabel.Caption := 'Encrypted';
      FLoginLabel.Visible := false;
    end else
    // No means of encryption is used for projectS
    begin
      Visible := false;
    end;

  end else
    Visible := false;
end;

procedure TEpiVStatusBarItem_CurrentUser.BumpExampleType;
begin
  Inc(FExampleType);
  if (FExampleType > 2) then
    FExampleType := 0;
end;

procedure TEpiVStatusBarItem_CurrentUser.DoExample;
begin
  Case FExampleType of
    0:
      begin
        FLabel.Visible      := True;
        FLoginLabel.Visible := True;
        FLabel.Caption      := 'Login: ';
        FLoginLabel.Caption := 'admin';
      end;
    1:
      begin
        FLabel.Visible      := True;
        FLoginLabel.Visible := false;
        FLabel.Caption      := 'Encrypted';
      end;
    2:
      begin
        FLabel.Visible      := false;
        FLoginLabel.Visible := false;
      end;
  end;
end;

procedure TEpiVStatusBarItem_CurrentUser.DocumentChangeEvent(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (Initiator <> FDocument) then exit;
  if (not (EventGroup in [eegCustomBase, eegDocument])) then exit;

  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy:
          begin
            FDocument.UnRegisterOnChangeHook(@DocumentChangeEvent);
            FDocument := nil;
            DoUpdate;
          end;
      end;

    eegDocument:
      case TEpiDocumentChangeEvent(EventType) of
        edcePassword: DoUpdate;
      end;
  end;
end;

procedure TEpiVStatusBarItem_CurrentUser.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: ;
    sucDocFile:
      begin
        if Assigned(Statusbar.DocFile) then
          FDocument := Statusbar.DocFile.Document
        else
          FDocument := nil;

        UpdateHooks;
        DoUpdate;
      end;
    sucDataFile: ;
    sucSelection: ;
    sucSave: ;
    sucExample:
      begin
        FExampleType := 0;
        Panel.OnMouseWheel := @MouseWheel;
        Panel.OnClick := @MouseClick;
        FLabel.OnMouseWheel := @MouseWheel;
        FLoginLabel.OnMouseWheel := @MouseWheel;
        DoExample;
      end;
  end;
end;

class function TEpiVStatusBarItem_CurrentUser.Caption: string;
begin
  Result := 'Encryption/Login Information';
end;

class function TEpiVStatusBarItem_CurrentUser.Name: string;
begin
  result := 'CurrentUser';
end;

constructor TEpiVStatusBarItem_CurrentUser.Create(
  AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

  FLabel := TLabel.Create(Panel);
  FLabel.AnchorParallel(akLeft, 2, Panel);
  FLabel.AnchorVerticalCenterTo(Panel);
  FLabel.Caption := '';
  FLabel.Parent := Panel;

  FLoginLabel := TLabel.Create(Panel);
  FLoginLabel.AnchorToNeighbour(akLeft, 2, FLabel);
  FLoginLabel.AnchorVerticalCenterTo(Panel);
  FLoginLabel.Caption := '';
  FLoginLabel.Parent := Panel;
end;

destructor TEpiVStatusBarItem_CurrentUser.Destroy;
begin
  inherited Destroy;
end;

function TEpiVStatusBarItem_CurrentUser.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  if FLoginLabel.Visible then
    Result := FLoginLabel.Left + FLoginLabel.Width + 2
  else
    Result := FLabel.Left + FLabel.Width + 2;
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_CurrentUser);

end.

