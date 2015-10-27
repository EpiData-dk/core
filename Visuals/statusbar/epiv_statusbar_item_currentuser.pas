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
    procedure UpdateHooks;
    procedure DoUpdate;
    procedure DocumentChangeEvent(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
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

procedure TEpiVStatusBarItem_CurrentUser.UpdateHooks;
begin
  FDocument.RegisterOnChangeHook(@DocumentChangeEvent, false);
end;

procedure TEpiVStatusBarItem_CurrentUser.DoUpdate;
var
  DocFile: TEpiDocumentFile;
begin
  DocFile := Statusbar.DocFile;

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
end;

procedure TEpiVStatusBarItem_CurrentUser.DocumentChangeEvent(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (Initiator <> FDocument) then exit;
  if (EventGroup <> eegDocument) then exit;

  case TEpiDocumentChangeEvent(EventType) of
    edcePassword: DoUpdate;
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
        FDocument := Statusbar.DocFile.Document;
        UpdateHooks;
        DoUpdate;
      end;
    sucDataFile: ;
    sucSelection: ;
    sucSave: ;
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

