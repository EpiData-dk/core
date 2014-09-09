unit epiv_datamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls,
  epicustombase,
  epidatafiles,
  epidatafilestypes;

type

  { TDM }

  TDM = class(TDataModule)
    Icons16: TImageList;
  private
    { private declarations }
  public
    function GetImageIndex(Const Ft: TEpiFieldType): Integer;
    function GetImageIndex(Const ControlItem: TEpiCustomControlItem): Integer;
  end;

var
  DM: TDM;

implementation

{$R *.lfm}

{ TDM }

function TDM.GetImageIndex(const Ft: TEpiFieldType): Integer;
begin
  Result := -1;

  case Ft of
    ftBoolean:
      Result := 1;
    ftInteger:
      Result := 2;
    ftFloat:
      Result := 3;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate:
      Result := 4;
    ftTime:
      Result := 5;
    ftString,
    ftUpperString:
      Result := 6;
    ftAutoInc:
      Result := 7;
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      Result := 8;
    ftTimeAuto:
      Result := 9;
  end;
end;

function TDM.GetImageIndex(const ControlItem: TEpiCustomControlItem): Integer;
begin
  if ControlItem.InheritsFrom(TEpiSection) then
    Result := 10;

  if ControlItem.InheritsFrom(TEpiHeading) then
    Result := 11;

  if ControlItem.InheritsFrom(TEpiField) then
    Result := GetImageIndex(TEpiField(ControlItem).FieldType);
end;

end.

