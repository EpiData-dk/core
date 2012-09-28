unit ut_epivaluelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, epivaluelabels;

type

  { TUnitTest_EpiStudy }

  { TUnitTest_EpiValueLabels }

  TUnitTest_EpiValueLabels = class(TTestCase)
  private
    FCallBacks: integer;
    FEpiDoc: TEpiDocument;
    FValueLabelSets: TEpiValueLabelSets;
    FIntVL: TEpiValueLabelSet;
    FFloatVL: TEpiValueLabelSet;
    FStringVL: TEpiValueLabelSet;
    procedure ChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    { TEpiValueLabelSets }
//    procedure NewValueLabelSet;

    { TEpiValueLabelSet }
//    procedure NewValueLabel;
    procedure MissingCount;
    procedure MaxValueLength;
    procedure ValueLabelString;
    procedure ValueLabelExists;
    procedure IsMissingValue;

    { TEpiStringValueLabel }
    procedure StringValue;

    { TEpiFloatValueLabel }
    procedure FloatValue;

    { TEpiIntValueLabel }
    procedure IntValue;

    { TEpiCustomValueLabel }
    procedure ValueAsString;
    procedure Order;
    procedure TheLabel;
  end;


implementation


uses
  epidatafilestypes;

{ TUnitTest_EpiValueLabels }

procedure TUnitTest_EpiValueLabels.ChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TUnitTest_EpiValueLabels.SetUpOnce;
var
  VL: TEpiCustomValueLabel;
  i: Integer;
begin
  inherited SetUpOnce;
  FEpiDoc := TEpiDocument.Create('en');
  FValueLabelSets := FEpiDoc.ValueLabelSets;

  FIntVL    := FValueLabelSets.NewValueLabelSet(ftInteger);
  for i := 1 to 4 do
  begin
    with TEpiIntValueLabel(FIntVL.NewValueLabel) do
    begin
      Value := i;
      TheLabel.Text := 'Int-' + IntToStr(i);
      IsMissingValue := (i mod 3) = 0;
    end;
  end;

  FFloatVL  := FValueLabelSets.NewValueLabelSet(ftFloat);
  for i := 1 to 5 do
  begin
    with TEpiFloatValueLabel(FFloatVL.NewValueLabel) do
    begin
      Value := i * 0.1;
      TheLabel.Text := 'Float-' + IntToStr(i);
      IsMissingValue := (i mod 3) = 0;
    end;
  end;

  FStringVL := FValueLabelSets.NewValueLabelSet(ftString);
  for i := 1 to 6 do
  begin
    with TEpiStringValueLabel(FStringVL.NewValueLabel) do
    begin
      Value := Char(i + Ord('A') - 1);
      TheLabel.Text := 'String-' + IntToStr(i);
      IsMissingValue := (i mod 3) = 0;
    end;
  end;
end;

procedure TUnitTest_EpiValueLabels.TearDownOnce;
begin
  FEpiDoc.Free;
  inherited TearDownOnce;
end;

{procedure TUnitTest_EpiValueLabels.NewValueLabelSet;
begin
  FIntVL := FValueLabelSets.NewValueLabelSet(ftInteger);
end;

procedure TUnitTest_EpiValueLabels.NewValueLabel;
begin

end;}

procedure TUnitTest_EpiValueLabels.MissingCount;
begin
  CheckEquals(1, FIntVL.MissingCount, 'IntVL missing count');
  CheckEquals(1, FFloatVL.MissingCount, 'FloatVL missing count');
  CheckEquals(2, FStringVL.MissingCount, 'StringVL missing count');
end;

procedure TUnitTest_EpiValueLabels.MaxValueLength;
begin
  CheckEquals(1, FIntVL.MaxValueLength, 'IntVL Max Value Length');
  CheckEquals(3, FFloatVL.MaxValueLength, 'FloatVL Max Value Length');
  CheckEquals(1, FStringVL.MaxValueLength, 'StringVL Max Value Length');
end;

procedure TUnitTest_EpiValueLabels.ValueLabelString;
var
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    CheckEquals(IntToStr(i), FIntVL[i-1].ValueAsString);
    CheckEquals(FloatToStr(i * 0.1), FFloatVL[i-1].ValueAsString);
    CheckEquals(string(Char(i + Ord('A') - 1)), FStringVL[i-1].ValueAsString);
  end;
end;

procedure TUnitTest_EpiValueLabels.ValueLabelExists;
begin
  Check(FIntVL.ValueLabelExists[2], 'IntVL (Value=2) did not exist');
  CheckFalse(FIntVL.ValueLabelExists[5], 'IntVL (Value=5) did exist');

  Check(FFloatVL.ValueLabelExists[0.2], 'FloatVL (Value=0.2) did not exist');
  CheckFalse(FFloatVL.ValueLabelExists[0.6], 'FloatVL (Value=0.6) did exist');

  Check(FStringVL.ValueLabelExists['A'], 'StringVL (Value=A) did not exist');
  CheckFalse(FStringVL.ValueLabelExists['Z'], 'StringVL (Value=Z) did exist');
end;

procedure TUnitTest_EpiValueLabels.IsMissingValue;
begin

end;

procedure TUnitTest_EpiValueLabels.StringValue;
begin

end;

procedure TUnitTest_EpiValueLabels.FloatValue;
begin

end;

procedure TUnitTest_EpiValueLabels.IntValue;
begin

end;

procedure TUnitTest_EpiValueLabels.ValueAsString;
begin

end;

procedure TUnitTest_EpiValueLabels.Order;
begin

end;

procedure TUnitTest_EpiValueLabels.TheLabel;
begin

end;

initialization
  RegisterTest(TUnitTest_EpiValueLabels.Suite);

end.

