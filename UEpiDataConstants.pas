unit UEpiDataConstants;

interface

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
  NewLine:Byte=13;
  LineFeed:Byte=10;
  MaxRecLineLength=78;
  FieldNameLen:Byte=10;




implementation

end.
