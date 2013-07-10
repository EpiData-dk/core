unit epireport_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TEpiReportFieldListSortType = (stFieldName, stEntryFlow);

  TEpiReportGeneratorTableHeaderOptions =
    (
      thoRowHeader, thoColHeader                              // Fist row/col is considered a header (Generator should emphasize these)
    );
  TEpiReportGeneratorTableHeaderOptionSet = set of TEpiReportGeneratorTableHeaderOptions;

  TEpiReportGeneratorTableCellAdjustment =
    (
      tcaAutoAdjust, tcaLeftAdjust, tcaCenter, tcaRightAdjust   // Text Adjustment of cell  (Auto: leftadjust text, rightadjust numbers)
    );

implementation

end.

