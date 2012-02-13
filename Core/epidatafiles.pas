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

  // Jumps
  TEpiJumps = class;
  TEpiJump = class;

  // Calculations
  TEpiCalculation = class;

  // Comparison
  TEpiComparison = class;

{$I epidatafilesh.inc}
{$I episectionsh.inc}
{$I epifieldsh.inc}
{$I epiheadingsh.inc}
{$I epijumpsh.inc}
{$I epicalculationsh.inc}
{$I epicomparisonh.inc}

implementation

uses
  epidocument, math, epistringutils, epiconvertutils,
  DCPrijndael, DCPsha256;

{$I epidatafiles_helper.inc}
{$I epidatafiles.inc}
{$I episections.inc}
{$I epifields.inc}
{$I epiheadings.inc}
{$I epijumps.inc}
{$I epicalculations.inc}
{$I epicomparison.inc}

initialization
begin
  LinkMap := TLinkMap.Create;
end;

finalization
begin
  LinkMap.Free;
end;

end.

