unit epireport_fieldlist_simple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_datafilesreport_base, epireport_types,
  epidatafiles, epireport_generator_base;

type

  { TEpiReportSimpleFieldList }
  TEpiReportSimpleFieldList = class(TEpiReportDataFilesBase)
  private
    FSortType:  TEpiReportFieldListSortType;
  protected
    procedure   PrintDatafile(Const DataFile: TEpiDataFile); override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase);
       override;
    procedure   RunReport; override;
    property    SortType: TEpiReportFieldListSortType read FSortType write FSortType;
  end;


implementation

uses
  epimiscutils;

{ TEpiReportSimpleFieldList }

function FieldNameSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result:= AnsiCompareStr(Field1.Name, Field2.Name);
end;

procedure TEpiReportSimpleFieldList.PrintDatafile(const DataFile: TEpiDataFile);
var
  i: Integer;
  FieldList: TEpiFields;
begin
  DoTableHeader(DataFile.Caption.Text, 4, DataFile.Fields.Count + 1);
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Type');
  DoTableCell(2, 0, 'ValueLabel');
  DoTableCell(3, 0, 'Question');


  FieldList := TEpiFields.Create(nil);
  FieldList.Sorted := true;
  for i := 0 to DataFile.Fields.Count - 1 do FieldList.AddItem(DataFile.Field[i]);
  case SortType of
    stFieldName: FieldList.OnSort := @FieldNameSort;
    stEntryFlow: ; // Do nothing - list is already sorted (Core v1.3)
  end;
  FieldList.Sort;

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
  end;
  DoTableFooter('');
end;

constructor TEpiReportSimpleFieldList.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FSortType := stEntryFlow;
end;

procedure TEpiReportSimpleFieldList.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  for i := 0 to EpiDataFiles.Count - 1 do
    PrintDatafile(EpiDataFiles[i]);
end;

end.

