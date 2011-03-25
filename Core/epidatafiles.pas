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

{$I epidatafilesh.inc}
{$I episectionsh.inc}
{$I epifieldsh.inc}
{$I epiheadingsh.inc}
{$I epijumpsh.inc}
{$I epicalculationsh.inc}

implementation

uses
  epidocument, math, epistringutils, epiconvertutils;

var
  JumpList: TStringList;
  ValueLabelWriteList: TStringList;

{$I epidatafiles.inc}
{$I episections.inc}
{$I epifields.inc}
{$I epiheadings.inc}
{$I epijumps.inc}
{$I epicalculations.inc}

initialization
begin
  JumpList := TStringList.Create;
  ValueLabelWriteList := TStringList.Create;
end;

finalization
begin
  JumpList.Free;
  ValueLabelWriteList.Free;
end;

end.

