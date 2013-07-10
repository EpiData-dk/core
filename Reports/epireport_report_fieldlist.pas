unit epireport_report_fieldlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_base, epireport_types,
  epidatafiles;

type

  EEpiReportFieldList = class(EEpiReportBaseException);

  { TEpiReportFieldList }

  TEpiReportFieldList = class(TEpiReportBase)
  private
    FExtendedList: boolean;
    FFields: TEpiFields;
    FSortType: TEpiReportFieldListSortType;
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
       overload;
    procedure RunReport; override;
    property Fields: TEpiFields read FFields write FFields;
    property ExtendedList: boolean read FExtendedList write FExtendedList;
    property SortType: TEpiReportFieldListSortType read FSortType write FSortType;
  end;

implementation

uses
  epimiscutils, epidatafilestypes;

resourcestring
  SEpiReportFieldListNoFields = 'EpiReport: No fields assigned to field list.';

{ TEpiReportFieldList }

procedure TEpiReportFieldList.DoSanityCheck;
begin
  inherited DoSanityCheck;
end;

constructor TEpiReportFieldList.Create(ReportGenerator: TEpiReportGeneratorBase
  );
begin
  inherited Create(ReportGenerator);
  ExtendedList := false;
  SortType := stEntryFlow;
end;

function FieldNameSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result:= AnsiCompareStr(Field1.Name, Field2.Name);
end;

procedure TEpiReportFieldList.RunReport;
var
  FieldList: TEpiFields;
  ColCount: Integer;
  i: Integer;
  S: String;
begin
  inherited RunReport;

  FieldList := TEpiFields.Create(nil);
  FieldList.Sorted := true;
  for i := 0 to Fields.Count - 1 do FieldList.AddItem(Fields[i]);
  case SortType of
    stFieldName: FieldList.OnSort := @FieldNameSort;
    stEntryFlow: ;// Do nothing - Fields is alread sorted by flow (in Core v1.3)
  end;
  FieldList.Sort;

  if ExtendedList then
    DoTableHeader('Field extended view:', 13, FieldList.Count + 1)
  else
    DoTableHeader('Question list overview:', 4, FieldList.Count + 1);
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Type');
  DoTableCell(2, 0, 'Length');
  DoTableCell(3, 0, 'Question');
  if ExtendedList then
  begin
    DoTableCell(4, 0,  'ValueLabel');
    DoTableCell(5, 0,  'Def.Values');
    DoTableCell(6, 0,  'Visual');
    DoTableCell(7, 0,  'Entry Mode');
    DoTableCell(8, 0,  'Range');
    DoTableCell(9, 0,  'Compare');
    DoTableCell(10, 0, 'Jumps');
    DoTableCell(11, 0, 'Calculate');
    DoTableCell(12, 0, 'Notes');
//    DoTableCell(13, 0, 'Script');
  end;

  for i := 0 to FieldList.Count - 1 do
  with FieldList[i] do
  begin
    DoTableCell(0, i+1, Name);
    DoTableCell(1, i+1, EpiTypeNames[FieldType]);

    if FieldType in FloatFieldTypes then
      DoTableCell(2, i+1, Format('%d.%d', [Length - Decimals - 1, Decimals]), tcaRightAdjust)
    else
      DoTableCell(2, i+1, IntToStr(Length), tcaRightAdjust);

    DoTableCell(3, i+1, Question.Text);

    if not ExtendedList then continue;

    if Assigned(ValueLabelSet) then
      DoTableCell(4, i+1, ValueLabelSet.Name)
    else
      DoTableCell(4, i+1, '');

    if HasDefaultValue or RepeatValue then
      DoTableCell(5, i+1, 'x', tcaCenter);


    if ShowValueLabel or ForcePickList then
      DoTableCell(6, i+1, 'x', tcaCenter);

    if (EntryMode in [emMustEnter, emNoEnter]) or
       ConfirmEntry
    then
      DoTableCell(7, i+1, 'x', tcaCenter);

    if Assigned(Ranges) then
      DoTableCell(8, i+1, 'x', tcaCenter);

    if Assigned(Comparison) then
      DoTableCell(9, i+1, 'x', tcaCenter);

    if Assigned(Jumps) then
      DoTableCell(10, i+1, 'x', tcaCenter);

    if Assigned(Calculation) then
      DoTableCell(11, i+1, 'x', tcaCenter);

    if Notes.Text <> '' then
      DoTableCell(12, i+1, 'x', tcaCenter);

    // Script
{    if Assigned(Ranges) then
      DoTableCell(13, i+1, 'x');}
  end;
  S := '';
  if ExtendedList then
    S :=
      'An x in a column indicate one or more of these specifications:' + LineEnding +
      'Def.Values:   Repeat, Default Value' + LineEnding +
      'Visual:       Show Picklist, show Category text from Value Label' + LineEnding +
      'Entry-Mode :  Mustenter, Noenter, confirm' + LineEnding +
      'Range:        Range is specified' + LineEnding +
      'Compare:      Compare with another field' + LineEnding +
      'Jumps:        Jump specified' + LineEnding +
      'Calculate:    Calculation, copy from category text to other field' + LineEnding +
      'Notes:        Note text defined' + LineEnding +
      'Scrip:        Script defined';
  DoTableFooter(S);
end;

end.

