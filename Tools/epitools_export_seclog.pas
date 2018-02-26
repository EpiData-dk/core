unit epitools_export_seclog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument;

type

  { TEpiTool_ExportSecurityLog }

  TEpiTool_ExportSecurityLog = class
  private
  public
    constructor Create; virtual;
    function ExportSecLog(Document: TEpiDocument; ExportAfterNoDays: Integer; DeleteLog: boolean; ExportFilename: UTF8String): boolean; virtual;
  end;

implementation

{ TEpiTool_ExportSecurityLog }

constructor TEpiTool_ExportSecurityLog.Create;
begin
  //
end;

function TEpiTool_ExportSecurityLog.ExportSecLog(Document: TEpiDocument;
  ExportAfterNoDays: Integer; DeleteLog: boolean; ExportFilename: UTF8String
  ): boolean;
begin
  result := false;

  if (not Assigned(Document)) then exit;
  if (Document.Admin.Initialized) then exit;


end;

end.

