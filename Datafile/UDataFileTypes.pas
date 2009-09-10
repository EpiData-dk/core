unit UDataFileTypes;

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

  TFieldType   = (ftInteger, ftString, ftDate, ftUpperAlfa, ftCheckBox,
                  ftBoolean, ftFloat, ftPhoneNum, ftTime, ftLocalNum,
                  ftToday, ftEuroDate, ftIDNUM, ftRes4, ftRes5,
                  ftQuestion, ftEuroToday, ftSoundex, ftCrypt, ftYMDDate,
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

  TDataFileType         = (dftEpiData, dftStata, dftCSV, dftDBase);

  TByteOrder            = (boLittleEndian, boBigEndian);
  TStringMode           = (smUTF8, smAnsi);

implementation

end.
