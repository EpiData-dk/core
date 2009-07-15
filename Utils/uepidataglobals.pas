unit UEpiDataGlobals;

{$mode objfpc}{$H+}

interface

uses
  UDataFileTypes, Graphics;
  
type
  TCharSet = Set of Char;

CONST
  CoreVersion = 1;

  NewRecord = -1;
  NumChars:        TCharSet = ['0'..'9'];
  AlfaChars:       TCharSet = ['A'..'Z','a'..'z'];
  SignChars:       TCharSet = ['+', '-'];
  CommaChars:      TCharSet = ['.', ','];
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
                     
  FieldTypeNames: Array[0..20] of string =
                   ('Numeric', 'Text', 'Date (mdy)', 'Uppercase text', 'Checkbox',
                    'Boolean', 'Numeric', 'Phonenumber', 'Time', 'Local phonenumber',
                    'Today (mdy)', 'Date (dmy)', 'ID-number', '', '', 'Question',
                    'Today (dmy)', 'Soundex', 'Encryptfield', 'Date (ymd)',
                    'Today (ymd)');

 (* CommonWords: packed array[0..13] of String[10]=
      ('OF',      'AND',    'IF NO',    'IF YES',   'WHO',
       'WHERE',   'WHAT',   'DID',      'WHEN',     'HOW MANY',
       'THE',     'TO',     'IF',       'ARE');            *)

  SupportedFieldTypes:
    TFieldTypes = [ftInteger, ftAlfa, ftDate, ftUpperAlfa, ftBoolean, ftFloat,
                   ftToday, ftEuroDate, ftIDNUM, ftQuestion, ftEuroToday,
                   ftSoundex, ftCrypt, ftYMDDate, ftYMDToday];

  DateFieldTypes:
    TFieldTypes = [ftDate, ftToday, ftEuroDate, ftEuroToday,
                   ftYMDDate,ftYMDToday];

var
  // CommonWords: List of common english word that is used to strip
  //              string when creating new variables.
  CommonWords: array of string;
  BooleanYesChars: TCharSet = ['y','Y','1','t','T'];
  BooleanNoChars:  TCharSet = ['n','N','0','f','F'];
  BooleanChars:    TCharSet;
  AlfaNumChars:    TCharSet;
  DateChars:       TCharSet;
  IntegerChars:    TCharSet;
  FloatChars:      TCharSet;

  {$I ErrorCodes.inc}

implementation

var
  Initialized: boolean = false;

procedure InitGlobals;
begin
  if Initialized then exit;

  // Setting initial common english words.
  SetLength(CommonWords, 14);
  CommonWords[0] := 'OF';
  CommonWords[1] := 'AND';
  CommonWords[2] := 'IF NO';
  CommonWords[3] := 'IF YES';
  CommonWords[4] := 'WHO';
  CommonWords[5] := 'WHERE';
  CommonWords[6] := 'WHAT';
  CommonWords[7] := 'DID';
  CommonWords[8] := 'WHEN';
  CommonWords[9] := 'HOW MANY';
  CommonWords[10] := 'THE';
  CommonWords[11] := 'TO';
  CommonWords[12] := 'IF';
  CommonWords[13] := 'ARE';

  // Common global sets.
  BooleanChars := BooleanYesChars + BooleanNoChars;
  AlfaNumChars := AlfaChars + NumChars;
  DateChars    := NumChars + DateSeparators;
  IntegerChars := NumChars + SignChars;
  FloatChars   := IntegerChars + CommaChars;
  //

  Initialized := true;
end;

initialization
  InitGlobals;

end.
