unit epireport_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TEpiReportFieldListSortType = (stFieldName, stEntryFlow);

  TEpiReportGeneratorTableHeaderOptions =
    (
      thoRowHeader, // Fist row is considered a header (Generator should emphasize these)
      thoColHeader  // Fist col is considered a header (Generator should emphasize these)
    );
  TEpiReportGeneratorTableHeaderOptionSet = set of TEpiReportGeneratorTableHeaderOptions;

  TEpiReportGeneratorTableCellAdjustment =
    ( // Text Adjustment of cell:
      tcaAutoAdjust,   // leftadjust text, rightadjust numbers
      tcaLeftAdjust,   // Force left adjust
      tcaCenter,       // Center content
      tcaRightAdjust   // Force right adjust
    );

  TEpiReportGeneratorTableCellOptions =
    (
      // Borders are not excluse, but if two cell next to eachother (top-bottom, left-right)
      // have set the same border (eg. topcell has bottom border AND bottomcell has top border)
      // only a single border should be drawn!
      tcoTopBorder,     // Adds border to top of cell
      tcoBottomBorder,  // Adds border to bottom of cell
      tcoLeftBorder,    // Adds border to left of cell
      tcoRightBorder    // Adds border to right of cell
    );
  TEpiReportGeneratorTableCellOptionSet = Set of TEpiReportGeneratorTableCellOptions;


implementation

end.

