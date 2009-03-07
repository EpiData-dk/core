unit UEpiDataConstants;

interface

uses
  graphics, UEpiTypes;

CONST
  NEWRECORD=-1;
  NumChars:        Set of CHAR=['0'..'9'];
  AlfaNumChars:    Set of CHAR=['0'..'9','A'..'Z','a'..'z'];
  AlfaChars:       Set of CHAR=['A'..'Z','a'..'z'];
  IntegerChars:    Set of CHAR=['0'..'9','-','+'];
  FloatChars:      Set of CHAR=['0'..'9', '.', ',', '-', '+'];
  DateChars:       Set of CHAR=['0'..'9','/'];
  BooleanChars:    Set of CHAR=['y','Y','n','N','1','0'];
  BooleanYesChars: Set of CHAR=['y','Y','1'];
  BooleanNoChars:  Set of CHAR=['n','N','0'];
  DaysInMonth:     ARRAY[1..12] OF BYTE = (31,29,31,30,31,30,31,31,30,31,30,31);
  TerminatorChars: Set of CHAR=['!','?','^'];
  EOLchars: array[0..2] of char = '!'#13#10;
  NewLine:Byte=13;
  LineFeed:Byte=10;
  CRLF=#13#10;
  MaxRecLineLength=78;
  FieldNameLen:Byte=10;
  MaxIndices=3;
  ColorNames:ARRAY[0..17] OF string = ('AQUA','BLACK','BLUE','DKGRAY','FUCHSIA','GRAY',
                        'GREEN','LIME','LTGRAY','MAROON','NAVY','OLIVE','PURPLE','RED','SILVER','TEAL','WHITE','YELLOW');
  ColorValues:ARRAY[0..17] OF TColor = (clAqua, clBlack, clBlue, clDkGray, clFuchsia,
                        clGray, clGreen, clLime, clLtGray, clMaroon, clNavy, clOlive,
                        clPurple, clRed, clSilver, clTeal, clWhite, clYellow);
  TextColors:array[0..15] of TColor = (clBlack,clNavy,clGreen,clTeal,
                        clMaroon,clPurple,clOlive,clSilver,clGray,
                        clBlue,clLime,clAqua,clRed,clFuchsia,clYellow,clWhite);
  BgColors:array[0..7] of TColor = (clBlack,clNavy,clGreen,clTeal,clMaroon,clPurple,clOlive,clSilver);
  SupportedFieldTypes: SET OF TFieldtypes = [ftInteger,ftAlfa,ftDate,ftUpperAlfa,
                          ftBoolean,ftFloat,ftToday,ftEuroDate,ftIDNUM,ftQuestion,
                          ftEuroToday,ftSoundex,ftCrypt,ftYMDDate,ftYMDToday];
  DateFieldTypes: SET OF TFieldtypes = [ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday];

  {$I Datafile\errorcodes.inc}




implementation

end.
