unit epieximtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epimiscutils;

type
  TEpiFieldNamingCase = (fncUpper, fncLower, fncAsIs);

  TEpiControlItemPosition = procedure (Const Sender: TObject;
    Const ControlItem: TEpiCustomControlItem;
    var Top, Left: Integer) of object;

const
  EpiFieldNamingCaseToString: array[TEpiFieldNamingCase] of string =
  (
    'Uppercase',
    'Lowercase',
    'Leave as is'
  );

type
  TEpiEncoding =
    (eeUTF8,             // Unicode (UTF-8)
     eeCP1250,           // Centel Europe (CP1250)
     eeCP1251,           // Cyrillic (CP1251)
     eeCP1252,           // Latin 1(CP1252)
     eeCP1253,           // Greek (CP1253)
     eeCP1254,           // Turkish (CP1254)
     eeCP1255,           // Hebrew (CP1255)
     eeCP1256,           // Arabic (CP1256)
     eeCP1257,           // Baltic (CP1257)
     eeCP1258,           // Vietnamese (CP1258)
     eeCP874,            // Thai (CP874)
     eeKOI8,             // Russian (KOI8)
     eeCP936,            // Chinese simple (CP936, GB2312)
     eeCP950,            // Chinese complex
     eeCP949,            // korean
     eeCP932,            // japanes
     eeGuess,            // Used for guess encoding during text read.
     eeSystemDefault     // Used for export/import using current system setting.
    );

const
  EpiEncodingToString: array[TEpiEncoding] of string =
    ('UTF8',             // Unicode (UTF-8)
     'CP1250',           // Centel Europe (CP1250)
     'CP1251',           // Cyrillic (CP1251)
     'CP1252',           // Latin 1(CP1252)
     'CP1253',           // Greek (CP1253)
     'CP1254',           // Turkish (CP1254)
     'CP1255',           // Hebrew (CP1255)
     'CP1256',           // Arabic (CP1256)
     'CP1257',           // Baltic (CP1257)
     'CP1258',           // Vietnamese (CP1258)
     'CP874',            // Thai (CP874)
     'KOI8',             // Russian (KOI8)
     'CP936',            // Chinese simple (CP936', GB2312)
     'CP950',            // Chinese complex
     'CP949',            // korean
     'CP932',            // japanes
     'Guess',            // Used for guess encoding during text read.
     'System'            // Used for export/import using current system setting.
     );

type

//  Stata Version 4    = $69;
//  Stata Version 6    = $6C; // dta_108
//  Stata Version 7    = $6E; // dta_110
//  Stata Version 8+9  = $71; // dta_113
//  Stata Version 10   = $72; // dta_114
//  Stata Version 12   = $73; // dta_115
//  Stata Version 13   = $75; // dta_117    // First Stata format with XML-Like structure.
//  Stata Version 14   = $76; // dta_118    // First Stata format to SUPPORT UTF-8! Yay!

  TEpiStataVersion =
    (dta4 = $69,
     dta6 = 108,
     dta7 = 110,
     dta8 = 113,
     dta10 = 114,
     dta12 = 115,
     dta13 = 117,
     dta14 = 118
    );

  TEpiStataEndian =
    (eseLittleEndian, eseBigEndian);

const
  // 21916 days after 30 dec. 1899 = 1/1/1960 Stata base date.
  StataBaseDate = 21916;
  StataBaseDateTime = TDateTime(StataBaseDate);

  StataByteConst   = #251;
  StataIntConst    = #252;
  StataLongConst   = #253;
  StataFloatConst  = #254;
  StataDoubleConst = #255;

  StataStrLsConstXML  = 32768;
  StataDoubleConstXML = 65526;
  StataFloatConstXML  = 65527;
  StataLongConstXML   = 65528;
  StataIntConstXML    = 65529;
  StataByteConstXML   = 65530;


  // Stata Value Ranges
  StataMinByte =        -127;
  StataMaxByte =         100;
  StataMinInt  =      -32767;
  StataMaxInt  =       32740;
  StataMinLong = -2147483647;
  StataMaxLong =  2147483620;


function EpiStataVersionToString(Const StataVersion: TEpiStataVersion): string;

// Extracts filename extension and returns best guess af to what the filetype is. If no
// known filetype is found result is false and FileType is undefined.
function FilenameToFileType(const Filename: UTF8String; out FileType: TEpiDialogFilter): boolean;


implementation

uses
  LazUTF8;

function EpiStataVersionToString(const StataVersion: TEpiStataVersion): string;
begin
  case StataVersion of
    dta4:  result := 'Stata 4';
    dta6:  result := 'Stata 5, 6';
    dta7:  result := 'Stata 7';
    dta8:  result := 'Stata 8, 9';
    dta10: result := 'Stata 10, 11';
    dta12: result := 'Stata 12';
    dta13: result := 'Stata 13';
    dta14: result := 'Stata 14';
  else
    result := 'Stata version is missing string representation in: epieximtypes.pas';
  end;
end;

function FilenameToFileType(const Filename: UTF8String; out FileType: TEpiDialogFilter): boolean;
var
  Ext: RawByteString;
begin
  Result := true;
  Ext := ExtractFileExt(UTF8LowerString(Filename));

  // dfEPX, dfEPZ, dfREC, dfText, dfODS, dfXLS, dfDTA, dfDBF, dfSPSS, dfSAS, dfDDI, dfCollection, dfAll;
  case Ext of
    '.rec':  FileType := dfREC;
    '.dta':  FileType := dfDTA;
    '.txt',
    '.csv':  FileType := dfText;
    '.epx':  FileType := dfEPX;
    '.epz':  FileType := dfEPZ;
  else
    result := false;
  end;
end;

end.

