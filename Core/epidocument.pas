unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, epiadmin, epistudy, epidatafile;

type

  TEpiDocument = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
    Property   Admin: TEpiAdmin;
    Property   Study: TEpiStudy;
    Property   DataFiles: TEpiDataFiles;
  end;

implementation

uses
  epistringutils, EpiBase64, epiutils, FileUtil,
  StrUtils, epicheckfileio, Math, epidateutils, XMLRead,
  epiimportexport;




end.
