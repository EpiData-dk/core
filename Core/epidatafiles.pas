unit epidatafiles;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, episettings, epidatafilestypes,
  epivaluelabels, epirights, epiranges, epirelates;

type
  TEpiSaveModeEnum = (esmDatafile, esmField);

var
  EpiSaveMode: TEpiSaveModeEnum = esmDatafile;

type
  // Forward prototype declaration...

  // DataFile
  TEpiDataFiles = class;
  TEpiDataFile = class;

  // Groups
  TEpiSections = class;
  TEpiSection = class;

  // Fields
  TEpiFields = class;
  TEpiField = class;

  // Heading
  TEpiHeadings = class;
  TEpiHeading = class;

  // Jumps
  TEpiJumps = class;
  TEpiJump = class;

  // Calculations
  TEpiCalculation = class;

  // Comparison
  TEpiComparison = class;

  // Relations
  TEpiValueRelates = class;
  TEpiValueRelate = class;

  PEpiString = ^EpiString;

  TEpiFieldDataEventRecord = record
    Index: Integer;                       // The index at which the datachange happend
    case FieldType: TEpiFieldType of      // The senders Fieldtype (also accessible through the change event, but convinient)
      ftBoolean:                          // Old Value as per field type
        (BoolValue: EpiBool);
      ftInteger,
      ftAutoInc:
        (IntValue: EpiInteger);
      ftFloat:
        (FloatValue: EpiFloat);
      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        (DateValue: EpiDate);
      ftTime,
      ftTimeAuto:
        (TimeValue: EpiTime);
      ftString,
      ftUpperString:
        (StringValue: PEpiString);
  end;
  PEpiFieldDataEventRecord = ^TEpiFieldDataEventRecord;


{$I epidatafilesh.inc}
{$I episectionsh.inc}
{$I epifieldsh.inc}
{$I epiheadingsh.inc}
{$I epijumpsh.inc}
{$I epicalculationsh.inc}
{$I epicomparisonh.inc}
{$I epivaluerelatesh.inc}

implementation

uses
  epidocument, math, epiconvertutils, epidatafileutils,
  DCPrijndael, DCPsha256, contnrs, typinfo, LazUTF8, epidatafilerelations,
  strutils, epistringutils, epiadmin, epiglobals, episecuritylog;

{$I epidatafiles.inc}
{$I episections.inc}
{$I epifields.inc}
{$I epiheadings.inc}
{$I epijumps.inc}
{$I epicalculations.inc}
{$I epicomparison.inc}
{$I epivaluerelates.inc}

end.

