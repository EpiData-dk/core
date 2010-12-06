unit epidatafiles;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, epicustombase, episettings, epidatafilestypes,
  epivaluelabels, epiadmin, epiranges;

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

{$I epidatafilesh.inc}
{$I episectionsh.inc}
{$I epifieldsh.inc}
{$I epiheadingsh.inc}

implementation

uses
  epidocument, math, epistringutils;

{$I epidatafiles.inc}
{$I episections.inc}
{$I epifields.inc}
{$I epiheadings.inc}

end.

