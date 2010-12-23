unit epidatafilestypes;

{$mode objfpc}{$H+}

interface

uses
  variants;

type
  EpiInteger   = Int64;
  EpiFloat     = Extended;
  EpiDate      = Integer;
  EpiTime      = TDateTime;
  EpiDateTime  = TDateTime;
  EpiString    = string;
  EpiBool      = Byte;
  EpiVariant   = Variant;

  TEpiFieldType = (
    // Bool  (0)
    ftBoolean = 0,

    // Numbers  (1, 2, 3)
    ftInteger, ftAutoInc, ftFloat,

    // Date (4, 5, 6)
    ftDMYDate, ftMDYDate, ftYMDDate,
    // (7, 8, 9)
    ftDMYToday, ftMDYToday, ftYMDToday,

    // Time (10, 11)
    ftTime, ftTimeNow,

    // Strings (12, 13)
    ftString, ftUpperString
  );
  TEpiFieldTypes  = Set of TEpiFieldType;

const
  BoolFieldTypes    = [ftBoolean];

  IntFieldTypes     = [ftInteger, ftAutoInc];

  FloatFieldTypes   = [ftFloat];

  DateFieldTypes    = [ftDMYDate,  ftMDYDate,  ftYMDDate,
                       ftDMYToday, ftMDYToday, ftYMDToday];

  TimeFieldTypes    = [ftTime, ftTimeNow];

  StringFieldTypes  = [ftString, ftUpperString];

  AutoFieldTypes    = [ftAutoInc,
                       ftDMYToday, ftMDYToday, ftYMDToday,
                       ftTimeNow];

  RangeFieldTypes   = (IntFieldTypes + FloatFieldTypes + DateFieldTypes) - AutoFieldTypes;

type
  TEpiRecordState   = (rsNormal, rsVerified, rsDeleted);

implementation

end.

