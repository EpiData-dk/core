unit epidatafilestypes;

{$mode objfpc}{$H+}

interface

uses
  variants;

type
  EpiInteger   = Integer;
  EpiFloat     = Extended;
  EpiDate      = Integer;
  EpiTime      = TDateTime;
  EpiDateTime  = TDateTime;
  EpiString    = string;
  EpiBool      = Byte;
  EpiVariant   = Variant;

  TEpiFieldType = (
    // Bool
    ftBoolean,
    // Numbers
    ftInteger, ftAutoInc, ftFloat,
    // Date/time
    ftDMYDate, ftMDYDate, ftYMDDate,
    ftDMYToday, ftMDYToday, ftYMDToday,
    ftTime, ftTimeNow,
    // Strings
    ftString, ftUpperString
  );

  TEpiFieldTypes  = Set of TEpiFieldType;
  TEpiRecordState = (rsNormal, rsVerified, rsDeleted);

implementation

end.

