unit epireport_datafilesreport_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidatafiles;

const
  SEpiReportDataFilesBaseMissing = 'Datafiles not assigned';

type
  EEpiReportDataFilesBaseException = class(Exception);

  { TEpiReportDataFilesBase }

  TEpiReportDataFilesBase = class(TEpiReportBase)
  private
    FEpiDataFiles: TEpiDataFiles;
  protected
    procedure   DoSanityCheck; override;
    procedure   PrintDataFile(Const DataFile: TEpiDataFile); virtual; abstract;
  public
    property    EpiDataFiles: TEpiDataFiles read FEpiDataFiles write FEpiDataFiles;
  end;


implementation

{ TEpiReportDataFilesBase }

procedure TEpiReportDataFilesBase.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FEpiDataFiles) then
    DoError(EEpiReportDataFilesBaseException, SEpiReportDataFilesBaseMissing);
end;

end.

