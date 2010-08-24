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
    // Bool  (0)
    ftBoolean = 0,

    // Numbers  (1, 2, 3)
    ftInteger, ftAutoInc, ftFloat,

    // Date/time (4, 5, 6)
    ftDMYDate, ftMDYDate, ftYMDDate,
    // (7, 8, 9)
    ftDMYToday, ftMDYToday, ftYMDToday,
    // (10, 11)
    ftTime, ftTimeNow,

    // Strings (12, 13)
    ftString, ftUpperString
  );
  TEpiFieldTypes  = Set of TEpiFieldType;

const
  BoolFieldTypes:
    TEpiFieldTypes = [ftBoolean];

  IntFieldTypes:
    TEpiFieldTypes = [ftInteger, ftAutoInc];

  FloatFieldTypes:
    TEpiFieldTypes = [ftFloat];

  DateFieldTypes:
    TEpiFieldTypes = [ftDMYDate,  ftMDYDate,  ftYMDDate,
                   ftDMYToday, ftMDYToday, ftYMDToday];

  TimeFieldTypes:
    TEpiFieldTypes = [ftTime, ftTimeNow];

  StringFieldTypes:
    TEpiFieldTypes = [ftString, ftUpperString];

  AutoFieldTypes:
    TEpiFieldTypes = [ftAutoInc,
                   ftDMYToday, ftMDYToday, ftYMDToday,
                   ftTimeNow];

type
  TEpiRecordState = (rsNormal, rsVerified, rsDeleted);

implementation

end.

