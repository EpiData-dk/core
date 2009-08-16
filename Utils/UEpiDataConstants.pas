unit UEpiDataConstants;

{$mode objfpc}{$H+}

interface

uses
  UDataFileTypes, Graphics, math;
  
type
  TCharSet = Set of Char;

CONST
  CoreVersion = 2;

  NewRecord = -1;
  NumChars:        TCharSet = ['0'..'9'];
  AlfaNumChars:    TCharSet = ['0'..'9','A'..'Z','a'..'z'];
  AlfaChars:       TCharSet = ['A'..'Z','a'..'z'];
  IntegerChars:    TCharSet = ['0'..'9','-','+'];
  FloatChars:      TCharSet = ['0'..'9', '.', ',', '-', '+'];
  DateChars:       TCharSet = ['0'..'9','/'];
  BooleanChars:    TCharSet = ['y','Y','n','N','1','0'];
  BooleanYesChars: TCharSet = ['y','Y','1','t','T'];
  BooleanNoChars:  TCharSet = ['n','N','0','f','F'];
  DateSeparators:  TCharSet = ['-', '/', ':', '.'];

  EOLchars:        array[0..2] of char = '!'#13#10;
  DaysInMonth:     ARRAY[1..12] OF BYTE = (31,29,31,30,31,30,31,31,30,31,30,31);
{  TerminatorChars: Set of CHAR=['!','?','^'];

  NewLine:Byte=13;
  LineFeed:Byte=10;
  CRLF=#13#10;
}

  MaxRecLineLength=78;    
  MaxFieldNameLen: Byte = 10;
  MaxIndices=3;
  ChkColorNames:   ARRAY[0..17] OF string =
                    ('AQUA', 'BLACK', 'BLUE', 'DKGRAY', 'FUCHSIA', 'GRAY',
                     'GREEN', 'LIME', 'LTGRAY', 'MAROON', 'NAVY', 'OLIVE',
                     'PURPLE', 'RED', 'SILVER', 'TEAL', 'WHITE', 'YELLOW');
  ChkColorTypes:  ARRAY[0..17] OF TColor =
                    (clAqua, clBlack, clBlue, clDkGray, clFuchsia, clGray,
                     clGreen, clLime, clLtGray, clMaroon, clNavy, clOlive,
                     clPurple, clRed, clSilver, clTeal, clWhite, clYellow);
                     
  FieldTypeNames: Array[0..20] of widestring =
                   ('Numeric', 'Text', 'Date (mdy)', 'Uppercase text', 'Checkbox',
                    'Boolean', 'Numeric', 'Phonenumber', 'Time', 'Local phonenumber',
                    'Today (mdy)', 'Date (dmy)', 'ID-number', '', '', 'Question',
                    'Today (dmy)', 'Soundex', 'Encryptfield', 'Date (ymd)',
                    'Today (ymd)');

  SupportedFieldTypes:
    TFieldTypes = [ftInteger, ftString, ftDate, ftUpperAlfa, ftBoolean, ftFloat,
                   ftToday, ftEuroDate, ftIDNUM, ftQuestion, ftEuroToday,
                   ftSoundex, ftCrypt, ftYMDDate, ftYMDToday];

  DateFieldTypes:
    TFieldTypes = [ftDate, ftToday, ftEuroDate, ftEuroToday,
                   ftYMDDate,ftYMDToday];

  IntFieldTypes:
    TFieldTypes = [ftInteger, ftIDNUM];

  FloatFieldTypes:
    TFieldTypes = [ftFloat];

  BoolFieldTypes:
    TFieldTypes = [ftBoolean];

  StringFieldTypes:
    TFieldTypes = [ftString, ftUpperAlfa, ftSoundex, ftCrypt];


  {$I ErrorCodes.inc}

implementation

end.
