unit epireport_fieldlist_extended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_types, epireport_htmlgenerator,
  epidocument, epidatafiles;

{type

  { TEpiReportExtendedFieldList }

  TEpiReportExtendedFieldList = class(TEpiReportBase)
  private
    FEpiDataFiles: TEpiDataFiles;
    FSortType:  TEpiReportFieldListSortType;
    procedure   PrintDatafile(Const DataFile: TEpiDataFile);
  public
    constructor Create(const AEpiDocument: TEpiDocument;
      Const ASortType: TEpiReportFieldListSortType = stCreation);
    procedure   RunReport; override;
    property    EpiDataFiles: TEpiDataFiles read FEpiDataFiles;
    property    SortType: TEpiReportFieldListSortType read FSortType write FSortType;
  end;

  { TEpiReportExtendedFieldListHtml }

  TEpiReportExtendedFieldListHtml = class(TEpiReportExtendedFieldList)
  private
    FHtmlGenerator: TEpiReportHTMLGenerator;
    FCompleteHtml: Boolean;
  protected
    function GetReportText: string; override;
  public
    constructor Create(const AEpiDocument: TEpiDocument;
      Const ASortType: TEpiReportFieldListSortType = stCreation;
      Const CompleteHtml: boolean = false);
    destructor Destroy; override;
    procedure RunReport; override;
    property HtmlGenerator: TEpiReportHTMLGenerator read FHtmlGenerator;
  end;
                   }
implementation
            {
uses
  epimiscutils, epidatafilestypes;

{ TEpiReportExtendedFieldList }

function FieldNameSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result:= AnsiCompareStr(Field1.Name, Field2.Name);
end;

function EntryFlowSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
  MainSection: TEpiSection;

  function SectionTop(Const F: TEpiField): integer;
  begin
    result := F.Section.Top{ + F.Top};
  end;
  function SectionLeft(Const F: TEpiField): integer;
  begin
    result := F.Section.Left{ + F.Left};
  end;

begin
  if (Field1.Section = Field2.Section) then
  begin
    Result := (Field1.Top - Field2.Top);
    if Result = 0 then
      Result := (Field1.Left - Field2.Left);
    Exit;
  end;

  MainSection := Field1.DataFile.MainSection;

  // Cross section comparison.
  if (Field1.Section <> MainSection) and (Field2.Section <> MainSection) then
  begin
    result := SectionTop(Field1) - SectionTop(Field2);
    if result = 0 then
      Result := SectionLeft(Field1) - SectionLeft(Field2);
    Exit;
  end;

  // Main <-> Section comparison
  if (Field1.Section = MainSection) then
  begin
    result := Field1.Top - SectionTop(Field2);
    if Result = 0 then
      result := Field1.Left - SectionLeft(Field2);
    exit;
  end;

  // Section <-> Main comparison
  if (Field2.Section = MainSection) then
  begin
    result := SectionTop(Field1) - Field2.Top;
    if Result = 0 then
      result := SectionLeft(Field1) - Field2.Left;
  end;
end;

procedure TEpiReportExtendedFieldList.PrintDatafile(const DataFile: TEpiDataFile
  );
var
  FieldList: TList;
  ExtendedList: TList;
  i: Integer;
  j: Integer;
  S: String;
  k: Integer;
begin
  FieldList := TList.Create;
  for i := 0 to DataFile.Fields.Count - 1 do FieldList.Add(DataFile.Field[i]);
  case SortType of
    stCreation:  ; // Do nothing...
    stFieldName: FieldList.Sort(@FieldNameSort);
    stEntryFlow: FieldList.Sort(@EntryFlowSort);
  end;

  DoHeading(DataFile.Caption.Text);
  DoTableHeader('Overview:', 14, DataFile.Fields.Count + 1);
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Type');
  DoTableCell(2, 0, 'ValueLabel');
  DoTableCell(3, 0, 'Question');
  DoTableCell(4, 0, 'Length');
  DoTableCell(5, 0, 'Range');
  DoTableCell(6, 0, 'Entry Mode');
  DoTableCell(7, 0, 'Confirm');
  DoTableCell(8, 0, 'Repeat');
  DoTableCell(9, 0, 'Default Value');
  DoTableCell(10, 0, 'Show valuelabel');
  DoTableCell(11, 0, 'Show picklist');
  DoTableCell(12, 0, 'Write valuelabel');
  DoTableCell(13, 0, 'Extended');

  ExtendedList := TList.Create;
  for i := 0 to FieldList.Count - 1 do
  with TEpiField(FieldList[i]) do
    begin
    DoTableCell(0, i+1, Name);
    DoTableCell(1, i+1, EpiTypeNames[FieldType]);

    if Assigned(ValueLabelSet) then
      DoTableCell(2, i+1, ValueLabelSet.Name)
    else
      DoTableCell(2, i+1, '');
    DoTableCell(3, i+1, Question.Text);
    if FieldType in FloatFieldTypes then
      DoTableCell(4, i+1, Format('%d.%d', [Length - Decimals - 1, Decimals]))
    else
      DoTableCell(4, i+1, IntToStr(Length));

    if Assigned(Ranges) then
      DoTableCell(5, i+1, Ranges.RangesToText)
    else
      DoTableCell(5, i+1, '');

    Case EntryMode of
      emDefault:   DoTableCell(6, i+1, '');
      emMustEnter: DoTableCell(6, i+1, 'Must Enter');
      emNoEnter:   DoTableCell(6, i+1, 'No Enter');
    end;
    DoTableCell(7, i+1, BoolToStr(ConfirmEntry, 'x', ''));
    DoTableCell(8, i+1, BoolToStr(RepeatValue, 'x', ''));
    DoTableCell(9, i+1, DefaultValueAsString);
    DoTableCell(10, i+1, BoolToStr(ShowValueLabel, 'x', ''));
    DoTableCell(11, i+1, BoolToStr(ForcePickList, 'x', ''));
    if Assigned(ValueLabelWriteField) then
      DoTableCell(12, i+1, ValueLabelWriteField.Name)
    else
      DoTableCell(12, i+1, '');

    if Assigned(Comparison) or
       Assigned(Jumps) or
       Assigned(Calculation) or
       (Notes.Text <> '') then
    begin
      DoTableCell(13, i+1, 'x');
      ExtendedList.Add(TEpiField(FieldList[i]));
    end else
      DoTableCell(13, i+1, '');
  end;
  DoTableFooter('');

  for i := 0 to ExtendedList.Count - 1 do
  with TEpiField(ExtendedList[i]) do
  begin
    j := 0;
    if Assigned(Comparison)  then inc(j);
    if Assigned(Jumps)       then inc(j);
    if Assigned(Calculation) then inc(j);
    if (Notes.Text <> '')    then inc(j);

    DoTableHeader(Name, 2, j + 1);
    DoTableCell(0, 0, 'Extension:');
    DoTableCell(1, 0, 'Value:');

    j := 1;
    if Assigned(Comparison)  then
    begin
      DoTableCell(0, j, 'Comparison');
      S := Name;
      case Comparison.CompareType of
        fcLT:  S += '<';
        fcLEq: S += '<=';
        fcGEq: S += '>=';
        fcGT:  S += '>';
      end;
      S += Comparison.CompareField.Name;
      DoTableCell(1, j, S);
      Inc(j);
    end;

    if Assigned(Jumps)       then
    begin
      DoTableCell(0, j, 'Jumps');
      S := '';
      for k := 0 to Jumps.Count -1 do
      with Jumps[k] do
      begin
        S += JumpValueAsString + ' > ';
        case JumpType of
          jtSaveRecord:    S += 'Save Record';
          jtExitSection:   S += 'Exit Section';
          jtSkipNextField: S += 'Skip Next Field';
          jtToField:       S += JumpToField.Name;
        end;
        S += LineEnding;
      end;
      DoTableCell(1, j, S);
      Inc(j);
    end;

    if Assigned(Calculation) then
    begin
      DoTableCell(0, j, 'Calculation');

      case Calculation.CalcType of
        ctTimeDiff:
          with TEpiTimeCalc(Calculation) do
          begin
            S := 'Time Difference:' + LineEnding +
                 ResultField.Name + '=(';
            if Assigned(StartDate) then S += StartDate.Name + '+';
            if Assigned(StartTime) then S += StartTime.Name;
            S += ')-(';
            if Assigned(EndDate) then S += EndDate.Name + '+';
            if Assigned(EndTime) then S += EndTime.Name;
            S += ')';
          end;
        ctCombineDate:
          with TEpiCombineDateCalc(Calculation) do
          begin
            S := 'Create Date:' + LineEnding +
                 ResultField.Name + '=DMY(';
            S += Day.Name + ',';
            S += Month.Name + ',';
            S += Year.Name + ')';
          end;
        ctCombineString:
          with TEpiCombineStringCalc(Calculation) do
          begin
            S := 'Combine String:' + LineEnding +
                 ResultField.Name + '=';
            if Assigned(Field1) then S += Field1.Name + ' + ';
            S += Delim1 + ' + ';
            if Assigned(Field2) then S += Field2.Name + ' + ';
            S += Delim2 + ' + ';
            if Assigned(Field3) then S += Field3.Name;
          end;
      end;

      DoTableCell(1, j, S);
      Inc(j);
    end;

    if (Notes.Text <> '') then
    begin
      DoTableCell(0, j, 'Notes');
      DoTableCell(1, j, Notes.Text);
      Inc(j);
    end;
    DoTableFooter('');
  end;
end;

constructor TEpiReportExtendedFieldList.Create(
  const AEpiDocument: TEpiDocument; const ASortType: TEpiReportFieldListSortType
  );
begin
  inherited Create(AEpiDocument);
  FEpiDataFiles := AEpiDocument.DataFiles;
  FSortType := ASortType;
end;

procedure TEpiReportExtendedFieldList.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  for i := 0 to EpiDataFiles.Count - 1 do
    PrintDatafile(EpiDataFiles[i]);
end;

{ TEpiReportExtendedFieldListHtml }

function TEpiReportExtendedFieldListHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportExtendedFieldListHtml.Create(
  const AEpiDocument: TEpiDocument;
  const ASortType: TEpiReportFieldListSortType; const CompleteHtml: boolean);
begin
  inherited Create(AEpiDocument, ASortType);
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
  FCompleteHtml := CompleteHtml;
end;

destructor TEpiReportExtendedFieldListHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportExtendedFieldListHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml('List of fields (extended)');

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;
             }
end.

