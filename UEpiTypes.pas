unit UEpiTypes;

interface

type
  TFieldTypes = (ftInteger,ftAlfa,ftDate,ftUpperAlfa,ftCheckBox,
                ftBoolean,ftFloat,ftPhoneNum,ftTime,ftLocalNum,
                ftToday,ftEuroDate,ftIDNUM,ftRes4,ftRes5,
                ftQuestion,ftEuroToday,ftSoundex,ftCrypt,ftYMDDate,ftYMDToday);
  TMissingValues=Array[0..2] of string; //Holds missing values for fields
  TScopes=(scLocal,scGlobal,scCumulative);  //used with DEFINEd variables in checkfiles
  TMissingActions=(maIgnoreMissing,maRejectMissing);

implementation

end.
