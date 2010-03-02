unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, epiadmin, epistudy, epidatafile, epirelate, XMLRead;

type

  { TEpiDocument }

  TEpiDocument = class(TObject)
  private
    FAdmin: TEpiAdmin;
    FDataFiles: TEpiDataFiles;
    FRelates: TEpiRelates;
    FStudy: TEpiStudy;
    procedure SetAdmin(const AValue: TEpiAdmin);
    procedure SetDataFiles(const AValue: TEpiDataFiles);
    procedure SetRelates(const AValue: TEpiRelates);
    procedure SetStudy(const AValue: TEpiStudy);

  public
    constructor Create;
    destructor Destroy; override;
    Property   Admin: TEpiAdmin read FAdmin write SetAdmin;
    Property   Study: TEpiStudy read FStudy write SetStudy;
    Property   DataFiles: TEpiDataFiles read FDataFiles write SetDataFiles;
    Property   Relates: TEpiRelates read FRelates write SetRelates;
  end;

implementation

{ TEpiDocument }

procedure TEpiDocument.SetAdmin(const AValue: TEpiAdmin);
begin
  if FAdmin = AValue then exit;
  FAdmin := AValue;
end;

procedure TEpiDocument.SetDataFiles(const AValue: TEpiDataFiles);
begin
  if FDataFiles = AValue then exit;
  FDataFiles := AValue;
end;

procedure TEpiDocument.SetRelates(const AValue: TEpiRelates);
begin
  if FRelates = AValue then exit;
  FRelates := AValue;
end;

procedure TEpiDocument.SetStudy(const AValue: TEpiStudy);
begin
  if FStudy = AValue then exit;
  FStudy := AValue;
end;

constructor TEpiDocument.Create;
begin

end;

destructor TEpiDocument.Destroy;
begin
  inherited Destroy;
end;

end.
