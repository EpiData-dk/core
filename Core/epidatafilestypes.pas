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

type
  TEpiRecordState = (rsNormal, rsVerified, rsDeleted);

implementation

end.

