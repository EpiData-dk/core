unit epiexport_ddi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings, DOM;

type

  { TEpiDDIExport }

  TEpiDDIExport = class
  private
    EpiDoc:     TEpiDocument;
    XMLDoc:     TXMLDocument;
    DDIInstance: TDOMElement;
    StudyUnit:    TDOMElement;

    procedure     AddNameSpace(Elem: TDOMElement; NameSpace: string);
    procedure     AddID(Elem: TDOMElement; Prefix: string);

  public
    constructor Create;
    destructor  Destroy; override;
    function    ExportDDI(Const Settings: TEpiDDIExportSetting): boolean;
  end;

implementation

uses
  XMLWrite;

{ TEpiDDIExport }

procedure TEpiDDIExport.AddNameSpace(Elem: TDOMElement; NameSpace: string);
begin
  Elem.SetAttribute('xmlns', 'ddi:' + NameSpace + ':3_1');
end;

procedure TEpiDDIExport.AddID(Elem: TDOMElement; Prefix: string);
begin
//  TGuid;
end;

constructor TEpiDDIExport.Create;
begin

end;

destructor TEpiDDIExport.Destroy;
begin
  inherited Destroy;
end;

function TEpiDDIExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
begin
  XMLDoc := TXMLDocument.Create;
  DDIInstance := XMLDoc.CreateElement('DDIInstance');
  XMLDoc.AppendChild(DDIInstrance);
  DDIInstance := XMLDoc.DocumentElement;

  {
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="ddi:instance:3_1"
  xsi:schemaLocation="ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd"
  id="2fbc5af9-939d-4b95-999a-2920c38c9dc3"
  version="1.0.0"
  versionDate="2012-05-30T13:27:58.364+02:00"
  agency="dk.dda"}

  AddNameSpace(DDIInstance, 'instance');
  with DDIInstance do
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

