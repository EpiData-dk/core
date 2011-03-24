unit epidatafilestypes;

{$mode objfpc}{$H+}

interface

uses
  variants;


  //===================
  // FIELD TYPE RELATED
  //===================
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
  // "Native sets"
  BoolFieldTypes    = [ftBoolean];

  IntFieldTypes     = [ftInteger, ftAutoInc];

  FloatFieldTypes   = [ftFloat];

  DateFieldTypes    = [ftDMYDate,  ftMDYDate,  ftYMDDate,
                       ftDMYToday, ftMDYToday, ftYMDToday];

  TimeFieldTypes    = [ftTime, ftTimeNow];

  StringFieldTypes  = [ftString, ftUpperString];

  // Complete set
  AllFieldTypes     = BoolFieldTypes + IntFieldTypes + FloatFieldTypes +
                      DateFieldTypes + TimeFieldTypes + StringFieldTypes;

  // Special auto type set.
  AutoFieldTypes    = [ftAutoInc,
                       ftDMYToday, ftMDYToday, ftYMDToday,
                       ftTimeNow];

  // Composed Sets.
  AutoUpdateFieldTypes   = AutoFieldTypes - [ftAutoInc];
  RangeFieldTypes        = (IntFieldTypes + FloatFieldTypes + DateFieldTypes + TimeFieldTypes) - AutoFieldTypes;
  ValueLabelFieldTypes   = (BoolFieldTypes + IntFieldTypes + FloatFieldTypes + StringFieldTypes) - AutoFieldTypes;
  JumpsFieldTypes        = (BoolFieldTypes + IntFieldTypes + FloatFieldTypes + StringFieldTypes) - AutoFieldTypes;
  EntryModeFieldTypes    = AllFieldTypes - AutoFieldTypes;
  ConfirmEntryFieldTypes = AllFieldTypes - AutoFieldTypes;
  RepeatValueFieldTypes  = AllFieldTypes - AutoFieldTypes;
  NotesFieldTypes        = AllFieldTypes - AutoFieldTypes;
  DefaultValueFieldTypes = AllFieldTypes - AutoFieldTypes;

  //================
  // RECORD RELATED
  //================
type
  TEpiRecordState   = (rsNormal, rsVerified, rsDeleted);


  //================
  // RECORD RELATED
  //================
type
  TEpiEntryMode     = (emDefault, emMustEnter, emNoEnter);

  //================
  // JUMPS RELATED
  //================
type
  TEpiJumpType = (jtSaveRecord, jtExitSection, jtSkipNextField, jtToField);
  TEpiJumpResetType = (jrLeaveAsIs, jrSystemMissing, jrMaxMissing, jr2ndMissing);


implementation

end.

