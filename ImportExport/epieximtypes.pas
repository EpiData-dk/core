unit epieximtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TEpiFieldNamingCase = (fncUpper, fncLower, fncAsIs);


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

//  Stata Version 4    = $69;
//  Stata Version 6    = $6C; // dta_108
//  Stata Version 7    = $6E; // dta_110
//  Stata Version 8+9  = $71; // dta_113
//  Stata Version 10   = $72; // dta_114
//  Stata Version 12   = $73; // dta_115
  TEpiStataVersion =
    (dta4 = $69,
     dta6 = 108,
     dta7 = 110,
     dta8 = 113,
     dta10 = 114,
     dta12 = 115
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

  // 21916 days after 30 dec. 1899 = 1/1/1960 Stata base date.
  StataBaseDate = 21916;
  StataBaseDateTime = TDateTime(StataBaseDate);

  StataByteConst   = #251;
  StataIntConst    = #252;
  StataLongConst   = #253;
  StataFloatConst  = #254;
  StataDoubleConst = #255;

implementation

end.

