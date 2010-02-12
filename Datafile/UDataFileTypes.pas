unit UDataFileTypes;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Variants;

type
  EpiInteger   = Integer;
  EpiFloat     = Extended;
  EpiDate      = Integer;
  EpiTime      = Double;
  EpiDateTime  = Double;
  EpiString    = string;
  EpiBool      = Byte;
  EpiVariant   = Variant;

  TFieldNaming = (fnFirstWord, fnAuto); 

  //              0          1         2       3            4
  TFieldType   = (ftInteger, ftString, ftDate, ftUpperAlfa, ftCheckBox,
  //              5          6         7       8            9
                  ftBoolean, ftFloat, ftPhoneNum, ftTime, ftLocalNum,
  //              10       11          12       13      14
                  ftToday, ftEuroDate, ftIDNUM, ftRes4, ftRes5,
  //              15          16           17         18       19
                  ftQuestion, ftEuroToday, ftSoundex, ftCrypt, ftYMDDate,
  //              20
                  ftYMDToday);

  TFieldTypes  = Set of TFieldType;

  TRecordState = (rsNormal, rsVerified, rsDeleted);

  TRequestPasswordType  = (rpOpen, rpCreate);
  TRequestPasswordEvent = procedure(Sender: TObject; RequestType: TRequestPasswordType; var Password: string) of object;

  TTranslateEvent       = function(LangCode: Integer; const Text: widestring): widestring of object;

  TProgressResult       = (prNormal, prCancel);
  TProgressEvent        = function(Sender: TObject; Percent: Cardinal; Msg: string): TProgressResult of object;

  TEpiDataFileOption    = (eoIgnoreChecks, eoIgnoreRelates, eoIgnoreIndex);
  TEpiDataFileOptions   = set of TEpiDataFileOption;

  TDataFileType         = (dftEpiDataXml = 1, dftEpiDataRec, dftStata, dftOds, dftXls, dftText, dftDBase, dftQES, dftNone);

  TEntryType            = (entAny, entMust, entNone);

  TByteOrder            = (boLittleEndian, boBigEndian);

  TEpiFieldChangeEventType =
    (fceUpdate, fceName, fceLength, fceDecimals, fceFLeft, fceFTop, fceFColTxt, fceFColHl, fceFColBg,
     fceVarLabel, fceVLeft, fceVTop, fceVColTxt, fceVColBg);
  TEpiFieldChangeEvent  = procedure(Sender: TObject; EventType: TEpiFieldChangeEventType; OldValue: Pointer) of object;
  TEpiFieldChangeDataEvent = procedure(Sender: TObject; Index: Integer; OldValue: Pointer) of object;

  TEpiTextLabelChangeEventType =
    (tceUpdate, tceId, tceText, tceLeft, tceTop, tceColTxt, tceColHl, tceColBg);
  TEpiTextLabelChangeEvent  = procedure(Sender: TObject; EventType: TEpiTextLabelChangeEventType; OldValue: Pointer) of object;

  TEpiDataFileChangeEventType =
    (dceUpdate, dceName, dceLabel, dceStudy, dceAddField, dceRemoveField, dceFieldOrder, dceAddText, dceRemoveText, dceAddScreenProp, dceRemoveScreenProp);
  TEpiDataFileChangeEvent = procedure(Sender: TObject; EventType: TEpiDataFileChangeEventType; Data: Pointer) of object;

  TEpiDialogFilterPair = record
    FilterName: string;
    FilterExt:  string;
  end;

implementation

end.
