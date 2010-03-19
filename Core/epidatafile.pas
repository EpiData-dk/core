unit epidatafile; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicheckfilecmds, epicheckfiletypes,
  epidatatypes, epivaluelabels, episcreenproperties, DCPrijndael,
  DOM, math;

type
  TEpiDataFileProperties = class;
  TEpiDataFile = class;
  TEpiField = class;
  TEpiFields = class;
  TEpiFieldProperties = class;
  TEpiTextLabel = class;
  TEpiTextLabels = class;

{$I epidatafileh.inc}
{$I epifieldh.inc}
{$I epitextlabelh.inc}


implementation

uses
  XMLRead, epilog, epidataglobals, epiutils, epiimportexport, strutils,
  epistringutils, DCPbase64, epicheckfileio, FileUtil, epidateutils,
  DCPsha1;

{$I epidatafile.inc}
{$I epifield.inc}
{$I epitextlabel.inc}

end.

