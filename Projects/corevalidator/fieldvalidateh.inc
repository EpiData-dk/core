{%mainunit validationunit.pas}

  // XML comparing.
  function XmlToRec(OrgField, NewField: TEpiField): TExitResult;
  function XmlToDta(OrgField, NewField: TEpiField): TExitResult;
  function XmlToOds(OrgField, NewField: TEpiField): TExitResult;
  function XmlToXls(OrgField, NewField: TEpiField): TExitResult;
  function XmlToTxt(OrgField, NewField: TEpiField): TExitResult;
  function XmlToDbf(OrgField, NewField: TEpiField): TExitResult;

  // Rec comparing.
  function RecToXml(OrgField, NewField: TEpiField): TExitResult;
  function RecToDta(OrgField, NewField: TEpiField): TExitResult;
  function RecToOds(OrgField, NewField: TEpiField): TExitResult;
  function RecToXls(OrgField, NewField: TEpiField): TExitResult;
  function RecToTxt(OrgField, NewField: TEpiField): TExitResult;
  function RecToDbf(OrgField, NewField: TEpiField): TExitResult;

  // Stata comparing.
  function DtaToXml(OrgField, NewField: TEpiField): TExitResult;
  function DtaToRec(OrgField, NewField: TEpiField): TExitResult;
  function DtaToOds(OrgField, NewField: TEpiField): TExitResult;
  function DtaToXls(OrgField, NewField: TEpiField): TExitResult;
  function DtaToTxt(OrgField, NewField: TEpiField): TExitResult;
  function DtaToDbf(OrgField, NewField: TEpiField): TExitResult;

  // OpenOffice comparing.
  function OdsToXml(OrgField, NewField: TEpiField): TExitResult;
  function OdsToRec(OrgField, NewField: TEpiField): TExitResult;
  function OdsToDta(OrgField, NewField: TEpiField): TExitResult;
  function OdsToXls(OrgField, NewField: TEpiField): TExitResult;
  function OdsToTxt(OrgField, NewField: TEpiField): TExitResult;
  function OdsToDbf(OrgField, NewField: TEpiField): TExitResult;

  // Excel comparing.
  function XlsToXml(OrgField, NewField: TEpiField): TExitResult;
  function XlsToRec(OrgField, NewField: TEpiField): TExitResult;
  function XlsToDta(OrgField, NewField: TEpiField): TExitResult;
  function XlsToOds(OrgField, NewField: TEpiField): TExitResult;
  function XlsToTxt(OrgField, NewField: TEpiField): TExitResult;
  function XlsToDbf(OrgField, NewField: TEpiField): TExitResult;

  // Text
  function TxtToXml(OrgField, NewField: TEpiField): TExitResult;
  function TxtToRec(OrgField, NewField: TEpiField): TExitResult;
  function TxtToDta(OrgField, NewField: TEpiField): TExitResult;
  function TxtToOds(OrgField, NewField: TEpiField): TExitResult;
  function TxtToXls(OrgField, NewField: TEpiField): TExitResult;
  function TxtToDbf(OrgField, NewField: TEpiField): TExitResult;

  // DBase
  function DbfToXml(OrgField, NewField: TEpiField): TExitResult;
  function DbfToRec(OrgField, NewField: TEpiField): TExitResult;
  function DbfToDta(OrgField, NewField: TEpiField): TExitResult;
  function DbfToOds(OrgField, NewField: TEpiField): TExitResult;
  function DbfToXls(OrgField, NewField: TEpiField): TExitResult;
  function DbfToTxt(OrgField, NewField: TEpiField): TExitResult;

