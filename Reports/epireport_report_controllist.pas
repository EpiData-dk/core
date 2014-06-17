unit epireport_report_controllist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_base, epireport_types,
  epicustombase;

type

  EEpiReportControlList = class(EEpiReportBaseException);

  { TEpiReportControlList }

  TEpiReportControlList = class(TEpiReportBase)
  private
    FControlItems: TEpiCustomControlItemList;
    FControlItemsAreSubItemized: boolean;
    FExtendedList: boolean;
    FTableFooter: string;
    FTableHeader: string;
    function ItemTypeName(Const Item: TEpiCustomItem): string;
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
       overload;
    procedure RunReport; override;
    property ControlItems: TEpiCustomControlItemList read FControlItems write FControlItems;
    // If true, then the layout of items are hierachial:
    //  - if an item in ControlItems contains subcontrols, these are NOT present in the ControlItems list.
    // If false, the list is flat:
    //  - all subcontrols of an Item in ControlItems, are ALSO present in the ControItems list.
    property ControlItemsAreSubItemized: boolean read FControlItemsAreSubItemized write FControlItemsAreSubItemized;
    // Add information about Missingvalues + Valuelabelset to report.
    property ExtendedList: boolean read FExtendedList write FExtendedList;
    // Header to be printed in ouput table
    property TableHeader: string read FTableHeader write FTableHeader;
    property TableFooter: string read FTableFooter write FTableFooter;
  end;

implementation

uses
  epidatafiles, epidatafilestypes, epidatafileutils, epimiscutils;

{ TEpiReportControlList }

function TEpiReportControlList.ItemTypeName(const Item: TEpiCustomItem): string;
begin
  Result := 'Unknown!';

  if Item is TEpiField then
    Result := EpiTypeNames[TEpiField(Item).FieldType];

  if Item is TEpiSection then
    Result := 'Section';

  if Item is TEpiHeading then
    Result := 'Heading';
end;

procedure TEpiReportControlList.DoSanityCheck;
begin
  inherited DoSanityCheck;
end;

constructor TEpiReportControlList.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FTableHeader := 'List Overview';
  FTableFooter := '';
  FControlItemsAreSubItemized := false;
  FExtendedList := false;
end;

procedure TEpiReportControlList.RunReport;
var
  CCount: Integer;
  RCount: Integer;
  CurCol: Integer;
  i: Integer;
  Item: TEpiCustomItem;
  Runner: Integer;
  j: Integer;
  S: TEpiSection;
  FDataFile: TEpiDataFile;
  FCurSection: TEpiSection;


  procedure AddItemToTable(Const Item: TEpiCustomItem; Const Runner: Integer);
  var
    F: TEpiField;
    S: String;
    i: integer;
  begin
    CurCol := 0;

    S := '';
    if ((Item is TEpiField) and (TEpiField(Item).Section <> TEpiField(Item).DataFile.MainSection)) or
       ((Item is TEpiHeading) and (TEpiHeading(Item).Section <> TEpiHeading(Item).DataFile.MainSection))
    then
      S := '> ';

    DoTableCell(PostInc(CurCol), Runner, S + Item.Name,      tcaLeftAdjust);
    DoTableCell(PostInc(CurCol), Runner, ItemTypeName(Item), tcaLeftAdjust);

    if Item is TEpiField then
      DoTableCell(PostInc(CurCol), Runner, IntToStr(TEpiField(Item).Length), tcaRightAdjust)
    else
      Inc(CurCol);

    if ExtendedList and
       (Item is TEpiField) and
       (Assigned(TEpiField(Item).ValueLabelSet))
    then
    begin
      F := TEpiField(Item);
      S := '';
      for i := 0 to F.ValueLabelSet.Count - 1 do
        if F.ValueLabelSet[i].IsMissingValue then
          S += F.ValueLabelSet[i].ValueAsString + ' ';

      Delete(S, Length(S), 1);
      DoTableCell(PostInc(CurCol), Runner, S, tcaLeftAdjust);
      DoTableCell(PostInc(CurCol), Runner, F.ValueLabelSet.Name, tcaLeftAdjust);
    end else
      if ExtendedList then
        Inc(CurCol, 2);

    if Item is TEpiField then
      DoTableCell(PostInc(CurCol), Runner, TepiField(Item).Question.Text, tcaLeftAdjust);
    if Item is TEpiHeading then
      DoTableCell(PostInc(CurCol), Runner, TEpiHeading(Item).Caption.Text, tcaLeftAdjust);
    if Item is TEpiSection then
      DoTableCell(PostInc(CurCol), Runner, TEpiSection(Item).Caption.Text, tcaLeftAdjust);
  end;


begin
  inherited RunReport;

  CCount := 4;
  if ExtendedList then
    CCount := 6;

  if FControlItems.Count > 0 then
  begin
    if FControlItems[0] is TEpiHeading then
      FDataFile := TEpiHeading(FControlItems[0]).DataFile;
    if FControlItems[0] is TEpiField then
      FDataFile := TEpiField(FControlItems[0]).DataFile;
    if FControlItems[0] is TEpiSection then
      FDataFile := TEpiSection(FControlItems[0]).DataFile;
  end;

  RCount := FControlItems.Count + 1;
  for i := 0 to FControlItems.Count - 1 do
  begin
    Item :=  FControlItems[i];
    if Item = FDataFile.MainSection then
    begin
      Dec(RCount);
      continue;
    end;

    if Item is TEpiSection then
    begin
      if (I > 0) then   // Do NOT make a space if first item is a section!
        Inc(RCount);

      if ControlItemsAreSubItemized then
      begin
        Inc(RCount, TEpiSection(Item).AllControls.Count);
        // TODO : Code for section->"item
      end;
    end;

    if not ControlItemsAreSubItemized then
    begin
      if (i > 0) and                                               // Do not access item with negative index.
         (ITem.Owner.Owner = FDataFile.MainSection) and            // If current item is located on main secion
         (FControlItems[i-1] <> FDataFile.MainSection) and         // AND previous item IS NOT the main section
         (FControlItems[i-1].Owner.Owner <> FDataFile.MainSection) // AND previous item IS NOT locatred on main section
      then
        Inc(RCount);                                               // THEN add a space... :=)
    end;
  end;

  DoTableHeader(TableHeader, CCount, RCount);

  CurCol := 0;
  DoTableCell(PostInc(CurCol), 0, 'Name', tcaLeftAdjust);
  DoTableCell(PostInc(CurCol), 0, 'Type', tcaLeftAdjust);
  DoTableCell(PostInc(CurCol), 0, 'Length', tcaLeftAdjust);
  if ExtendedList then
  begin
    DoTableCell(PostInc(CurCol), 0, 'Missing Value(s)', tcaLeftAdjust);
    DoTableCell(PostInc(CurCol), 0, 'Value Label', tcaLeftAdjust);
  end;
  DoTableCell(PostInc(CurCol), 0, 'Question / Caption', tcaLeftAdjust);


  Runner := 1;
  for i := 0 to FControlItems.Count - 1 do
  begin
    Item := FControlItems[i];

    // Do not include main
    if (Item = FDataFile.MainSection)
    then
      Continue;

    // Make a space before a section
    if (Item is TEpiSection) and
       (i > 0)
    then
      Inc(Runner);

    if not ControlItemsAreSubItemized then
    begin
      if (i > 0) and                                               // Do not access item with negative index.
         (ITem.Owner.Owner = FDataFile.MainSection) and            // If current item is located on main secion
         (FControlItems[i-1] <> FDataFile.MainSection) and         // AND previous item IS NOT the main section
         (FControlItems[i-1].Owner.Owner <> FDataFile.MainSection) // AND previous item IS NOT locatred on main section
      then
        Inc(Runner);                                               // THEN add a space... :=)
    end;

    AddItemToTable(Item, PostInc(Runner));

    if (Item is TEpiSection)
    then
    begin
      if ControlItemsAreSubItemized then
        for j := 0 to TEpiSection(Item).AllControls.Count - 1 do
          AddItemToTable(TEpiSection(Item).AllControls[j], PostInc(Runner));
    end;

  end;
  DoTableFooter(TableFooter);
end;

end.

