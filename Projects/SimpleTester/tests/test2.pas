unit test2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, customtest, epidocument;

type

  { TTest2 }

  TTest2 = class(TCustomTest)
  private
    FDoc: TEpiDocument;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Run: boolean; override;
  end;

implementation

uses
  epidatafiles, epidatafilestypes, epivaluelabels, strutils;

{ TTest2 }

constructor TTest2.Create;
begin
  inherited Create;
end;

destructor TTest2.Destroy;
begin
  inherited Destroy;
end;

function TTest2.Run: boolean;
var
  VLSet: TEpiValueLabelSet;
  Df: TEpiDataFile;
  F: TEpiField;
  i: Integer;
begin
  result := true;

  try
    FDoc := TEpiDocument.Create('en');
    VLSet := Fdoc.DataFiles.ValueLabelSets.NewValueLabelSet(ftInteger);

    for i := 1 to 10 do
      with TEpiIntValueLabel(VLSet.NewValueLabel) do
      begin
        Value := i;
        TheLabel.Text := DupeString(IntToStr(i), 5);
        if (i mod 2) = 0 then IsMissingValue := true;
      end;

    Df := Fdoc.DataFiles.NewDataFile;
    F := Df.NewField(ftInteger);
    F.ValueLabelSet := VLSet;

    VLSet := Fdoc.DataFiles.ValueLabelSets.NewValueLabelSet(ftFloat);
    for i := 1 to 10 do
      with TEpiFloatValueLabel(VLSet.NewValueLabel) do
      begin
        Value := i / 10;
        TheLabel.Text := DupeString(IntToStr(i), 5);
        if (i mod 2) = 0 then IsMissingValue := true;
      end;

    Df := Fdoc.DataFiles.NewDataFile;
    F := Df.NewField(ftFloat);
    F.ValueLabelSet := VLSet;
  except
    result := false;
  end;
  FDoc.Free;
end;

initialization

begin
  RegisterTest(TTest2);
end;

end.

