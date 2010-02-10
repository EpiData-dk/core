unit UEpiDataGlobals;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  UDataFileTypes, sysutils;
  
type
  TCharSet = Set of Char;

CONST
  CoreVersion = 2;

  NewRecord = -1;
  NumChars:        TCharSet = ['0'..'9'];
  AlfaChars:       TCharSet = ['A'..'Z','a'..'z'];
  SignChars:       TCharSet = ['+', '-'];
  CommaChars:      TCharSet = ['.', ','];
  DateSeparators:  TCharSet = ['-', '/', ':', '.'];

  EOLchars:        array[0..2] of char = '!'#13#10;
  DaysInMonth:     ARRAY[1..12] OF BYTE = (31,29,31,30,31,30,31,31,30,31,30,31);

  NumGuessLines         = 3000;
  MaxRecLineLength      = 78;
  MaxFieldNameLen       = 10;
  MaxIndices            = 3;
  MaxIntegerLength      = 5;
  MaxDefinedMissingValues = 2;

  EpiDialogFilterCollection: TEpiDialogFilterPair = (
    FilterName: 'Supported files';
    FilterExt:  '';
  );

  EpiDialogFilterXML: TEpiDialogFilterPair = (
    FilterName: 'EpiData XML Data file (*.recxml)';
    FilterExt:  '*.recxml';
  );

  EpiDialogFilterREC: TEpiDialogFilterPair = (
    FilterName: 'EpiData data file (*.rec)';
    FilterExt:  '*.rec';
  );

  EpiDialogFilterText: TEpiDialogFilterPair = (
    FilterName: 'Text file (*.txt,*.csv)';
    FilterExt:  '*.txt;*.csv';
  );

  EpiDialogFilterODS: TEpiDialogFilterPair = (
    FilterName: 'Open Document Spreadsheet (*.ods)';
    FilterExt:  '*.ods';
  );

  EpiDialogFilterXLS: TEpiDialogFilterPair = (
    FilterName: 'Excel Spreadsheet (*.xls)';
    FilterExt:  '*.xls';
  );

  EpiDialogFilterDTA: TEpiDialogFilterPair = (
    FilterName: 'Stata file (*.dta)';
    FilterExt:  '*.dta';
  );

  EpiDialogFilterDBF: TEpiDialogFilterPair = (
    FilterName: 'dBase file (*.dbf)';
    FilterExt:  '*.dbf';
  );

  EpiDialogFilterQES: TEpiDialogFilterPair = (
    FilterName: 'QES file (*.qes)';
    FilterExt:  '*.qes';
  );

  EpiDialogFilterAll: TEpiDialogFilterPair = (
    FilterName: 'Show All (*.*)';
    FilterExt:  '*.*';
  );

  // Colours - codes and names.
  // - base colour (white, use if no other good alternative exists).
  EpiColourBase      = $FFFFFF;    // White
  // - Field colours.
  EpiColourFieldBg   = $FFFFFF;    // White
  EpiColourFieldTxt  = $000000;    // Black
  EpiColourFieldHl   = $00FFFF;    // Aqua/Cyan
  // - Label colours.
  EpiColourLabelTxt  = $000000;    // Black
  EpiColourLabelBg   = $FFFFFF;    // White
  // - Form colours.
  EpiColourBackGrnd  = $FFFFFF;    // White

  ChkColorNames:   ARRAY[0..17] OF string =
                    ('AQUA', 'BLACK', 'BLUE', 'DKGRAY', 'FUCHSIA', 'GRAY',
                     'GREEN', 'LIME', 'LTGRAY', 'MAROON', 'NAVY', 'OLIVE',
                     'PURPLE', 'RED', 'SILVER', 'TEAL', 'WHITE', 'YELLOW');

  ChkColorTypes:  ARRAY[0..17] OF Integer = (
                 // clAqua,   clBlack, clBlue,   clDkGray, clFuchsia, clGray,
                    $00FFFF,  $000000, $0000FF,  $A9A9A9,  $FF00FF,   $808080,
                 // clGreen,  clLime,  clLtGray, clMaroon, clNavy,    clOlive,
                    $008000,  $00FF00, $D3D3D3,  $800000,  $000080,   $808000,
                 // clPurple, clRed,   clSilver, clTeal,   clWhite,   clYellow
                    $800080,  $FF0000, $C0C0C0,  $008080,  $FFFFFF,   $FFFF00);

  ChkColourEpiInfoTypes: Array[0..7] of Integer =
                    // For now just all white...
                    ($FFFFFF, $FFFFFF, $FFFFFF, $FFFFFF,
                     $FFFFFF, $FFFFFF, $FFFFFF, $FFFFFF);

  FieldTypeXmlNames: Array[TFieldType] of string =
                   ('int', 'string', 'mdydate', 'uppercase', 'checkbox',
                    'bool', 'float', 'phone', 'time', 'localphone',
                    'mdytoday', 'dmydate', 'idnum', 'res1', 'res2', 'label',
                    'dmytoday', 'soundex', 'crypt', 'ymddate',
                    'ymdtoday');

  FieldTypeNames: Array[TFieldType] of string =
                   ('Integer', 'Text', 'Date (mdy)', 'Uppercase text', 'Checkbox',
                    'Boolean', 'Float', 'Phonenumber', 'Time', 'Local phonenumber',
                    'Today (mdy)', 'Date (dmy)', 'ID-number', '', '', 'Label',
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
  EpiInternalFormatSettings: TFormatSettings;
  EpiExternalFormatSettings: TFormatSettings;
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

  // Format setting.
  EpiInternalFormatSettings.ThousandSeparator := ',';
  EpiInternalFormatSettings.DecimalSeparator  := '.';
  EpiInternalFormatSettings.DateSeparator     := '/';
  EpiInternalFormatSettings.TimeSeparator     := ':';
  EpiExternalFormatSettings := EpiInternalFormatSettings;

  Initialized := true;
end;

initialization
  InitGlobals;

end.
