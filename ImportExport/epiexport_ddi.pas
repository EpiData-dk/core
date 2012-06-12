unit epiexport_ddi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings, DOM;

type

  { TEpiDDIExport }

  TEpiDDIExport = class

  public
    constructor Create;
    destructor  Destroy; override;
    function    ExportDDI(Const Settings: TEpiDDIExportSetting): boolean;
  end;

implementation

uses
  XMLWrite;

{ TEpiDDIExport }

constructor TEpiDDIExport.Create;
begin

end;

destructor TEpiDDIExport.Destroy;
begin
  inherited Destroy;
end;

function TEpiDDIExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
var
  XMLDoc: TXMLDocument;
  Root: TDOMElement;
begin
  XMLDoc := TXMLDocument.Create;
  Root   := XMLDoc.CreateElement('DDIInstance');
  XMLDoc.AppendChild(Root);
  Root   := XMLDoc.DocumentElement;

  {
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="ddi:instance:3_1"
  xsi:schemaLocation="ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd"
  id="2fbc5af9-939d-4b95-999a-2920c38c9dc3"
  version="1.0.0"
  versionDate="2012-05-30T13:27:58.364+02:00"
  agency="dk.dda"}

  with Root do
  begin
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    SetAttribute('xmlns', 'ddi:instance:3_1');
    SetAttribute('xsi:schemaLocation', 'ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd');
    // Generate ID!
    // GUIDToString()

    // Version NO!
    SetAttribute('version', '1.0.0');
    SetAttribute('versionDate', FormatDateTime('YYYY/MM/DD"T"HH:MM:SS"."ZZZ', Now));
    SetAttribute('agency', 'dk.dda');
  end;

  WriteXML(XMLDoc, Settings.ExportFileName)
end;

end.

