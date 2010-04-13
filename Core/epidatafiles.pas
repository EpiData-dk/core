unit epidatafiles;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, epicustombase, episettings;

type
  // Forward prototype declaration...

  // DataFile
  TEpiDataFiles = class;
  TEpiDataFile = class;

  // Groups
  TEpiGroups = class;
  TEpiGroup = class;

  // Fields
  TEpiFields = class;
  TEpiField = class;

  // Heading
  TEpiHeadings = class;
  TEpiHeading = class;

{$I epidatafilesh.inc}
{$I epigroupsh.inc}
{$I epifieldsh.inc}
{$I epiheadingsh.inc}

implementation

uses
  epidocument, strutils;

{$I epidatafiles.inc}
{$I epigroups.inc}
{$I epifields.inc}
{$I epiheadings.inc}

end.

