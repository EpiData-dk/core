unit UDataFileTypes;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

type
  TFieldNaming = (fnFirstWord, fnAuto); 
  TFieldType   = (ftInteger, ftAlfa, ftDate, ftUpperAlfa, ftCheckBox,
                  ftBoolean, ftFloat, ftPhoneNum, ftTime, ftLocalNum,
                  ftToday, ftEuroDate, ftIDNUM, ftRes4, ftRes5,
                  ftQuestion, ftEuroToday, ftSoundex, ftCrypt, ftYMDDate,
                  ftYMDToday);

  TFieldTypes  = Set of TFieldType;

  TRecordState = (rsNormal, rsVerified, rsDeleted);

  TRequestPasswordType  = (rpOpen, rpCreate);
  TRequestPasswordEvent = procedure(Sender: TObject; RequestType: TRequestPasswordType; var Password: String) of object;

  TTranslateEvent       = function(LangCode: Integer; const Text: widestring): widestring of object;

  TProgressResult       = (prNormal, prCancel);
  TProgressEvent        = function(Sender: TObject; Percent: Cardinal; Msg: String): TProgressResult of object;

  TEpiDataFileOption    = (eoInMemory, eoIgnoreChecks, eoIgnoreRelates, eoIgnoreIndex);
  TEpiDataFileOptions   = set of TEpiDataFileOption;

  TDataFileType         = (dftEpiData, dftStata, dftCSV);

  TByteOrder            = (boLittleEndian, boBigEndian);

implementation

end.
