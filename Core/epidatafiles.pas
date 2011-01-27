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

{$I epidatafilesh.inc}
{$I episectionsh.inc}
{$I epifieldsh.inc}
{$I epiheadingsh.inc}
{$I epijumpsh.inc}

implementation

uses
  epidocument, math, epistringutils;

var
  JumpList: TStringList;

{$I epidatafiles.inc}
{$I episections.inc}
{$I epifields.inc}
{$I epiheadings.inc}
{$I epijumps.inc}

initialization
begin
  JumpList := TStringList.Create;
end;

finalization
begin
  JumpList.Free;
end;

end.

