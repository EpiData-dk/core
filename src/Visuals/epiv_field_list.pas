unit epiv_field_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons;

type

  { TEpiVFieldList }

  TEpiVFieldList = class(TFrame)
    Panel1: TPanel;
    MoveUpBtn: TSpeedButton;
    MoveDownBtn: TSpeedButton;
  private
    procedure LoadGlyphs;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  epiv_datamodule;

{ TEpiVFieldList }

procedure TEpiVFieldList.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(35, MoveUpBtn.Glyph);
  DM.Icons16.GetBitmap(36, MoveDownBtn.Glyph);
end;

constructor TEpiVFieldList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LoadGlyphs;
end;

end.

