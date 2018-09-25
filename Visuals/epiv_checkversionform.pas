unit epiv_checkversionform;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, CheckBoxThemed, Buttons;

type

  { TCheckVersionForm }

  TCheckVersionForm = class(TForm)
  private
    CurrentVersionCaption: TLabel;
    CurrentVersionInfo: TLabel;

    PublicVersionCaption: TLabel;
    PublicVersionInfo: TLabel;

    TestVersionCaption: TLabel;
    TestVersionInfo: TLabel;

    CloseBtn: TBitBtn;
    procedure DownloadLinkClick(Sender: TObject);
    procedure DownloadLinkMouseEnter(Sender: TObject);
    procedure DownloadLinkMouseLeave(Sender: TObject);
    procedure ShowForm(Sender: TObject);
  private
    FCheckBoxValue: Boolean;
    procedure CreateControls;
    procedure UpdateVersions;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Controls, Graphics, LCLIntf, ExtCtrls, epiversionutils, Dialogs;


const
  DefaultURL = 'http://epidata.dk';
  PublicDownloadURL = 'http://epidata.dk/download.php';
  TestDownloadURL = 'http://epidata.dk/testing.php';

{ TCheckVersionForm }

procedure TCheckVersionForm.DownloadLinkClick(Sender: TObject);
var
  URL: String;
begin
  URL := DefaultURL;

  if Sender = PublicVersionInfo then
    URL := PublicDownloadURL;

  if Sender = TestVersionInfo then
    URL := TestDownloadURL;

  OpenURL(URL);
end;

procedure TCheckVersionForm.DownloadLinkMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TCheckVersionForm.DownloadLinkMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TCheckVersionForm.ShowForm(Sender: TObject);
begin
  UpdateVersions;
  CloseBtn.SetFocus;
end;

procedure TCheckVersionForm.CreateControls;
var
  Image: TImage;
  WebLabel: TLabel;
begin
  WebLabel := TLabel.Create(Self);

  with WebLabel do
  begin
    Caption := 'www.epidata.dk';
    Font.Bold := true;
    Font.Underline := true;
    AnchorHorizontalCenterTo(Self);
    AnchorParallel(akTop, 10, Self);
    OnClick := @DownloadLinkClick;
    OnMouseEnter := @DownloadLinkMouseEnter;
    OnMouseLeave := @DownloadLinkMouseLeave;
    Parent := Self;
  end;

  Image := TImage.Create(Self);
  with Image do
  begin
    Width := 64;
    Height := 64;
    AnchorToNeighbour(akTop, 10, WebLabel);
    AnchorParallel(akLeft, 10, Self);
    Picture.Icon := Application.Icon;
    Parent := Self;
  end;

  CurrentVersionCaption := TLabel.Create(Self);
  with CurrentVersionCaption do
  begin
    Caption := 'Current Version:';
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akTop,   0, Image);
    AnchorToNeighbour(akLeft, 15, Image);
    Parent := Self
  end;

  CurrentVersionInfo := TLabel.Create(self);
  with CurrentVersionInfo do
  begin
    Caption := '0.0.0.0';
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(akLeft, 20, CurrentVersionCaption);
    AnchorParallel(akBottom, 0, CurrentVersionCaption);
    Parent := Self
  end;

  PublicVersionCaption := TLabel.Create(Self);
  with PublicVersionCaption do
  begin
    Caption := 'Public Version:';
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(akTop, 10, CurrentVersionCaption);
    AnchorParallel(akLeft, 0, CurrentVersionCaption);
    Parent := Self
  end;

  PublicVersionInfo := TLabel.Create(self);
  with PublicVersionInfo do
  begin
    Font.Color := clBlue;
    Font.Underline := true;
    Caption := '0.0.0.0';
    Hint := 'Go to download page: ' + PublicDownloadURL;
    ShowHint := true;
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akLeft, 0, CurrentVersionInfo);
    AnchorParallel(akBottom, 0, PublicVersionCaption);
    OnClick := @DownloadLinkClick;
    OnMouseEnter := @DownloadLinkMouseEnter;
    OnMouseLeave := @DownloadLinkMouseLeave;
    Parent := Self
  end;

  TestVersionCaption := TLabel.Create(Self);
  with TestVersionCaption do
  begin
    Caption := 'Test Version:';
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(akTop, 10, PublicVersionCaption);
    AnchorParallel(akLeft, 0, CurrentVersionCaption);
    Parent := Self
  end;

  TestVersionInfo := TLabel.Create(self);
  with TestVersionInfo do
  begin
    Font.Color := clBlue;
    Font.Underline := true;
    Caption := '0.0.0.0';
    Hint := 'Go to testing download page: ' + TestDownloadURL;
    ShowHint := true;
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akLeft, 0, CurrentVersionInfo);
    AnchorParallel(akBottom, 0, TestVersionCaption);
    OnClick := @DownloadLinkClick;
    OnMouseEnter := @DownloadLinkMouseEnter;
    OnMouseLeave := @DownloadLinkMouseLeave;
    Parent := Self
  end;

  CloseBtn := TBitBtn.Create(Self);
  with CloseBtn do
  begin
    Kind := bkClose;
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(aktop, 10, TestVersionCaption);
    AnchorHorizontalCenterTo(Self);
    Parent := Self;
  end;
end;

procedure TCheckVersionForm.UpdateVersions;
var
  Current: TEpiVersionInfo;
  Stable: TEpiVersionInfo;
  Test: TEpiVersionInfo;
  Response: string;
  CurrentScore: Integer;
  StableScore: Integer;
  TestScore: Integer;
  NewStable: Boolean;
  NewTest: Boolean;
begin
  Current := GetEpiVersion(HINSTANCE);
  if not CheckVersionOnline(ApplicationName, Stable, Test, Response) then
  begin
    ShowMessage(
      'ERROR: Could not find version information.' + LineEnding +
      'Response: ' + Response);
    exit;
  end;

  with Current do
    CurrentScore  := (VersionNo * 10000) + (MajorRev * 100) + (MinorRev);
  With Stable do
    StableScore := (VersionNo * 10000) + (MajorRev * 100) + (MinorRev);
  With Test do
    TestScore   := (VersionNo * 10000) + (MajorRev * 100) + (MinorRev);

  NewStable     := (StableScore - CurrentScore) > 0;
  NewTest       := (TestScore   - CurrentScore) > 0;

  with Current, CurrentVersionInfo do
  begin
    CurrentVersionInfo.Caption := Format('%d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);
    Font.Bold := (not NewStable) and (not NewTest);
  end;

  With Stable, PublicVersionInfo do
  begin
    Caption := Format('%d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);
    Font.Bold := NewStable;
    if NewStable then
      Hint := 'New stable release available at: ' + PublicDownloadURL;
  end;

  With Test, TestVersionInfo do
  begin
    Caption := Format('%d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);
    Font.Bold := NewTest;
    if NewTest then
      Hint := 'New test release available at: ' + TestDownloadURL;
  end;
end;


constructor TCheckVersionForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);

  Position := poMainFormCenter;

  CreateControls;
  AutoSize := true;

  BorderStyle := bsDialog;
  BorderSpacing.InnerBorder := 5;

  OnShow := @ShowForm;
end;

end.

