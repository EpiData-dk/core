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
    DDIStudyUnit:    TDOMElement;

    procedure     AddNameSpace(Elem: TDOMElement; NameSpace: string);
    procedure     AddID(Elem: TDOMElement; Prefix: string);

    procedure     ExportEpiStudy;
    procedure     ExportEpiValueLabels;
    procedure     ExportEpiDataFiles;

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
var
  GUID: TGUID;
  S: String;
begin
  CreateGUID(GUID);
  S := GUIDToString(GUID);
  S := Prefix + Copy(S, 2, Length(S) - 2);
  Elem.SetAttribute('id', S);
end;

procedure TEpiDDIExport.ExportEpiStudy;
begin
  with EpiDoc.Study do
  begin
{    Property   AbstractText: TEpiTranslatedTextWrapper read FAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    property   Agency: string read FAgency write SetAgency;
    property   Citations: TEpiTranslatedTextWrapper read FCitations;
    Property   Created: TDateTime read FCreated;
    property   Funding: TEpiTranslatedTextWrapper read FFunding;
    property   GeographicalCoverage: TEpiTranslatedTextWrapper read FGeographicalCoverage;
    property   Identifier: string read FIdentifier write SetIdentifier;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    property   Notes: string read FNotes write SetNotes;
    property   Publisher: TEpiTranslatedTextWrapper read FPublisher;
    property   Purpose: TEpiTranslatedTextWrapper read FPurpose;
    property   Population: TEpiTranslatedTextWrapper read FPopulation;
    property   Rights: TEpiTranslatedTextWrapper read FRights;
    property   TimeCoverage: TEpiTranslatedTextWrapper read FTimeCoverage;
    Property   Title: TEpiTranslatedTextWrapper read FTitle;
    Property   Keywords: string read FKeywords write SetKeywords;
    property   Version: string read FVersion write SetVersion;       }

    // Abstract requires only DDIStudyUnit.


  end;
end;

procedure TEpiDDIExport.ExportEpiValueLabels;
begin

end;

procedure TEpiDDIExport.ExportEpiDataFiles;
begin

end;

constructor TEpiDDIExport.Create;
begin

end;

destructor TEpiDDIExport.Destroy;
begin
  inherited Destroy;
end;

function TEpiDDIExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
var
  Elem: TDOMElement;
begin
  XMLDoc := TXMLDocument.Create;
  DDIInstance := XMLDoc.CreateElement('DDIInstance');
  XMLDoc.AppendChild(DDIInstance);
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
  AddID(DDIInstance, 'inst-');
  with DDIInstance do
  begin
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    SetAttribute('xmlns', 'ddi:instance:3_1');
    SetAttribute('xsi:schemaLocation', 'ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd');
    // Version NO!
    SetAttribute('version', '1.0.0');
    SetAttribute('versionDate', FormatDateTime('YYYY/MM/DD"T"HH:MM:SS"."ZZZ', Now));
    SetAttribute('agency', 'dk.dda');
  end;

  DDIStudyUnit := XMLDoc.CreateElement('StudyUnit');
  DDIInstance.AppendChild(Elem);

  WriteXML(XMLDoc, Settings.ExportFileName)
end;

end.

