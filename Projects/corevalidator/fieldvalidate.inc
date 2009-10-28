{%mainunit validationunit.pas}

{
  Case OrgField.FieldType of
    ftInteger, ftIDNUM:
      ;
    ftDate, ftToday, ftEuroDate, ftEuroToday, ftYMDDate, ftYMDToday:
      ;
    ftFloat:
      ;
    ftBoolean:
      ;
    ftString, ftUpperAlfa, ftSoundex, ftCrypt:
      ;
    ftQuestion:
      Result := erOk;
  end;
}

function Max(A, B: TExitResult): TExitResult; overload;
begin
  if A < B then
    result := B
  else
    result := A;
end;

function ValidateFieldType(AField: TEpiField; CorrectFieldType: TFieldType; FailResult: TExitResult): TExitResult;
begin
  result := Validate(AField.FieldType = CorrectFieldType, FailResult);
  if Result > erOk then
    Reporter.ReportEvent(rtError, 'Field "%s" does not have correct type: Was %d, Expected: %d', [AField.FieldType, CorrectFieldType]);
end;

function XmlToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function XmlToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger, ftIDNUM:
      begin
        if OrgField.FieldLength >= 10 then
          result := ValidateFieldType(NewField, ftFloat, erAbortField)
        else
          Result := ValidateFieldType(NewField, ftInteger, erAbortField);
      end;
    ftDate, ftToday, ftEuroDate, ftEuroToday, ftYMDDate, ftYMDToday:
      result := ValidateFieldType(NewField, ftDate, erAbortField);
    ftFloat:
      begin
        if OrgField.FieldDecimals = 0 then
          Result := ValidateFieldType(NewField, ftInteger, erAbortField)
        else
          Result := ValidateFieldType(NewField, ftFloat, erAbortField);
      end;
    ftBoolean:
      result := ValidateFieldType(NewField, ftInteger, erAbortField);
    ftString, ftUpperAlfa, ftSoundex, ftCrypt:
      Result := ValidateFieldType(NewField, ftString, erAbortField);
    ftQuestion:
      Result := erOk;
  end;
end;

function XmlToOds(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger, ftIDNUM:
      Result := ValidateFieldType(NewField, ftInteger, erAbortField);
    ftDate, ftToday:
      Result := ValidateFieldType(NewField, ftDate, erAbortField);
    ftEuroDate, ftEuroToday:
      Result := ValidateFieldType(NewField, ftEuroDate, erAbortField);
    ftYMDDate, ftYMDToday:
      Result := ValidateFieldType(NewField, ftYMDDate, erAbortField);
    ftFloat:
      begin
        if OrgField.FieldDecimals = 0 then
          Result := ValidateFieldType(NewField, ftInteger, erAbortField)
        else
          Result := ValidateFieldType(NewField, ftFloat, erAbortField);
      end;
    ftString, ftUpperAlfa, ftSoundex, ftCrypt, ftBoolean:
      result := ValidateFieldType(NewField, ftString, erAbortField);
    ftQuestion:
      Result := erOk;
  end;
end;

function XmlToXls(OrgField, NewField: TEpiField): TExitResult;
begin
  result := XmlToOds(OrgField, NewField);
end;

function XmlToTxt(OrgField, NewField: TEpiField): TExitResult;
begin
  result := XmlToOds(OrgField, NewField);
end;

function XmlToDbf(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger, ftIDNUM, ftFloat,
    ftString, ftUpperAlfa, ftSoundex, ftCrypt:
      result := XmlToOds(OrgField, NewField);
    ftDate, ftToday, ftEuroDate, ftEuroToday, ftYMDDate, ftYMDToday:
      result := XmlToDta(OrgField, NewField);
    ftBoolean:
      result := ValidateFieldType(NewField, ftBoolean, erAbortField);
    ftQuestion:
      Result := erOk;
  end;
end;

function RecToXml(OrgField, NewField: TEpiField): TExitResult;
begin
  result := XmlToRec(OrgField, NewField);
end;

function RecToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  Result := XmlToDta(OrgField, NewField);
end;

function RecToOds(OrgField, NewField: TEpiField): TExitResult;
begin
  Result := XmlToOds(OrgField, NewField);
end;

function RecToXls(OrgField, NewField: TEpiField): TExitResult;
begin
  result := XmlToXls(OrgField, NewField);
end;

function RecToTxt(OrgField, NewField: TEpiField): TExitResult;
begin
  Result := XmlToTxt(OrgField, NewField);
end;

function RecToDbf(OrgField, NewField: TEpiField): TExitResult;
begin
  Result := XmlToDbf(OrgField, NewField);
end;

function DtaToXml(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DtaToRec(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DtaToOds(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DtaToXls(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DtaToTxt(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DtaToDbf(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function OdsToXml(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function OdsToRec(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function OdsToDta(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function OdsToXls(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function OdsToTxt(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function OdsToDbf(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XlsToXml(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XlsToRec(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XlsToDta(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XlsToOds(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XlsToTxt(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XlsToDbf(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function TxtToXml(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function TxtToRec(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function TxtToDta(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function TxtToOds(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function TxtToXls(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function TxtToDbf(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DbfToXml(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DbfToRec(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DbfToDta(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DbfToOds(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DbfToXls(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function DbfToTxt(OrgField, NewField: TEpiField): TExitResult;
begin

end;

