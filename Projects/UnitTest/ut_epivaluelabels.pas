unit ut_epivaluelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, epivaluelabels;

type

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
//    procedure Order;
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
  Check(FIntVL.IsMissingValue[3], 'IntVL (Value=2) was not missingvalue');
  CheckFalse(FIntVL.IsMissingValue[4], 'IntVL (Value=4) was missingvalue');

  Check(FFloatVL.IsMissingValue[0.3], 'FloatVL (Value=0.2) was not missingvalue');
  CheckFalse(FFloatVL.IsMissingValue[0.5], 'FloatVL (Value=0.6) was missingvalue');

  Check(FStringVL.IsMissingValue['C'], 'StringVL (Value=C) was not missingvalue');
  Check(FStringVL.IsMissingValue['F'], 'StringVL (Value=F) was not missingvalue');
  CheckFalse(FStringVL.IsMissingValue['A'], 'StringVL (Value=A) was missingvalue');
end;

procedure TUnitTest_EpiValueLabels.StringValue;
begin
  CheckEquals('A', TEpiStringValueLabel(FStringVL.ValueLabels[0]).Value);
  CheckEquals('B', TEpiStringValueLabel(FStringVL.ValueLabels[1]).Value);
  CheckEquals('C', TEpiStringValueLabel(FStringVL.ValueLabels[2]).Value);
end;

procedure TUnitTest_EpiValueLabels.FloatValue;
begin
  CheckEquals(0.1, TEpiFloatValueLabel(FFloatVL.ValueLabels[0]).Value);
  CheckEquals(0.2, TEpiFloatValueLabel(FFloatVL.ValueLabels[1]).Value);
  CheckEquals(0.3, TEpiFloatValueLabel(FFloatVL.ValueLabels[2]).Value);
end;

procedure TUnitTest_EpiValueLabels.IntValue;
begin
  CheckEquals(1, TEpiIntValueLabel(FIntVL.ValueLabels[0]).Value);
  CheckEquals(2, TEpiIntValueLabel(FIntVL.ValueLabels[1]).Value);
  CheckEquals(3, TEpiIntValueLabel(FIntVL.ValueLabels[2]).Value);
end;

procedure TUnitTest_EpiValueLabels.ValueAsString;
begin
  CheckEquals('1', FIntVL.ValueLabels[0].ValueAsString);
  CheckEquals(FloatToStr(0.1), FFloatVL.ValueLabels[0].ValueAsString);
  CheckEquals('A', FStringVL.ValueLabels[0].ValueAsString);
  CheckEquals('2', FIntVL.ValueLabels[1].ValueAsString);
  CheckEquals(FloatToStr(0.2), FFloatVL.ValueLabels[1].ValueAsString);
  CheckEquals('B', FStringVL.ValueLabels[1].ValueAsString);
  CheckEquals('3', FIntVL.ValueLabels[2].ValueAsString);
  CheckEquals(FloatToStr(0.3), FFloatVL.ValueLabels[2].ValueAsString);
  CheckEquals('C', FStringVL.ValueLabels[2].ValueAsString);
end;

{procedure TUnitTest_EpiValueLabels.Order;
begin
  // TODO : A test for Order?
end;}

procedure TUnitTest_EpiValueLabels.TheLabel;
var
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    CheckEquals('Int-' + IntToStr(i),    FIntVL.ValueLabels[i-1].TheLabel.Text);
    CheckEquals('Float-' + IntToStr(i),  FFloatVL.ValueLabels[i-1].TheLabel.Text);
    CheckEquals('String-' + IntToStr(i), FStringVL.ValueLabels[i-1].TheLabel.Text);
  end;
end;

initialization
  RegisterTest(TUnitTest_EpiValueLabels.Suite);

end.

