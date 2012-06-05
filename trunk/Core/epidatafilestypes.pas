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
    ftBoolean,

    // Numbers  (1, 2, 3)
    ftInteger, ftAutoInc, ftFloat,

    // Date (4, 5, 6)
    ftDMYDate, ftMDYDate, ftYMDDate,
    // (7, 8, 9)
    ftDMYAuto, ftMDYAuto, ftYMDAuto,

    // Time (10, 11)
    ftTime, ftTimeAuto,

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
                       ftDMYAuto, ftMDYAuto, ftYMDAuto];

  TimeFieldTypes    = [ftTime, ftTimeAuto];

  StringFieldTypes  = [ftString, ftUpperString];

  // Complete set
  AllFieldTypes     = BoolFieldTypes + IntFieldTypes + FloatFieldTypes +
                      DateFieldTypes + TimeFieldTypes + StringFieldTypes;

  // Special auto type set.
  AutoFieldTypes    = [ftAutoInc,
                       ftDMYAuto, ftMDYAuto, ftYMDAuto,
                       ftTimeAuto];

  // Composed Sets.
  AutoUpdateFieldTypes   = AutoFieldTypes - [ftAutoInc];
  RangeFieldTypes        = (IntFieldTypes + FloatFieldTypes + DateFieldTypes + TimeFieldTypes) - AutoFieldTypes;
  ValueLabelFieldTypes   = (BoolFieldTypes + IntFieldTypes + FloatFieldTypes + StringFieldTypes) - AutoFieldTypes;
  JumpsFieldTypes        = (BoolFieldTypes + IntFieldTypes + FloatFieldTypes + StringFieldTypes) - AutoFieldTypes;
  CompareFieldTypes      = (IntFieldTypes + FloatFieldTypes + DateFieldTypes + TimeFieldTypes) - AutoFieldTypes;
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


function NativeFieldTypeSetFromFieldType(Ft: TEpiFieldType): TEpiFieldTypes;

implementation

function NativeFieldTypeSetFromFieldType(Ft: TEpiFieldType): TEpiFieldTypes;
begin
  case Ft of
    ftBoolean:  result := BoolFieldTypes;
    ftInteger,
    ftAutoInc:  result := IntFieldTypes;
    ftFloat:    result := FloatFieldTypes;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate,
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto: result := DateFieldTypes;
    ftTime,
    ftTimeAuto:  result := TimeFieldTypes;
    ftString,
    ftUpperString: result := StringFieldTypes;
  end;
end;

end.

