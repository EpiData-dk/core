unit UEpiTypes;

interface
  TFelttyper = (ftInteger,ftAlfa,ftDate,ftUpperAlfa,ftCheckBox,
                ftBoolean,ftFloat,ftPhoneNum,ftTime,ftLocalNum,
                ftToday,ftEuroDate,ftIDNUM,ftRes4,ftRes5,
                ftQuestion,ftEuroToday,ftSoundex,ftCrypt,ftYMDDate,ftYMDToday);
  TMissingValues:    Array[0..2] of string; //Holds missing values for fields
  TScopes=(scLocal,scGlobal,scCumulative);  //used with DEFINEd variables in checkfiles
  PLabelRec=^TLabelRec;
  TLabelRec=Record
      Value:String[30];
      Text:String[80];
      Next:Pointer;
    END;

implementation

end.
