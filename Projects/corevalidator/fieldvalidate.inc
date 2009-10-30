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
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
}

function Max(A, B: TExitResult): TExitResult; overload;
begin
  if A < B then
    result := B
  else
    result := A;
end;

function Min(A, B: TExitResult): TExitResult; overload;
begin
  if A > B then
    result := B
  else
    result := A;
end;

function ValidateFieldType(AField: TEpiField; CorrectFieldType: TFieldType; FailResult: TExitResult): TExitResult;
begin
  result := Validate(AField.FieldType = CorrectFieldType, FailResult);
  if Result > erOk then
    Reporter.ReportEvent(rtError, 'Export Field "%s" does not have correct type: Was %d, Expected: %d', [AField.FieldName, AField.FieldType, CorrectFieldType]);
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
      result := ValidateFieldType(NewField, ftEuroDate, erAbortField);
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
  if (NewField.FieldType = ftInteger) and
     (NewField.FieldLength = 0) then
  begin
    // The field read from the file contained no data. As a result the field is
    // automatically set to Integer with 0 length. Hence any comparison with the
    // original field is pointless.
    Result := erOk;
    Exit;
  end;
  Case OrgField.FieldType of
    ftInteger, ftIDNUM:
      Result := ValidateFieldType(NewField, ftInteger, erAbortField);
    ftDate, ftToday, ftEuroDate, ftEuroToday, ftYMDDate, ftYMDToday:
      begin
        Result := Validate(NewField.FieldType = ftDate, erAbortField);
        Result := Min(Result, Validate(NewField.FieldType = ftEuroDate, erAbortField));
        Result := Min(Result, Validate(NewField.FieldType = ftYMDDate, erAbortField));
        if Result > erOk then
          ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
      end;
    ftFloat:
      begin
        // Testing on NewField.FieldDecimals because exporting a float field
        // with 1+ decimals may contain only int values, which is imported
        // as an Int field!
        if NewField.FieldDecimals = 0 then
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
      result := ValidateFieldType(NewField, ftDate, erAbortField);
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
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function DtaToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := DtaToXml(OrgField, NewField);
end;

function DtaToOds(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function DtaToXls(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := XmlToXls(NewField, OrgField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function DtaToTxt(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := XmlToTxt(NewField, OrgField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function DtaToDbf(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := XmlToDbf(NewField, OrgField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function OdsToXml(OrgField, NewField: TEpiField): TExitResult;
begin
  result := DtaToXml(OrgField, NewField);
end;

function OdsToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := DtaToRec(OrgField, NewField);
end;

function OdsToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := XmlToDta(OrgField, NewField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function OdsToXls(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function OdsToTxt(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := XmlToTxt(OrgField, NewField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function OdsToDbf(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger,ftDate, ftEuroDate, ftYMDDate,ftFloat, ftString:
      result := XmlToDbf(OrgField, NewField);
  else
    // Should always fail!
    Result := ValidateFieldType(NewField, ftCheckBox, erAbortField);
  end;
end;

function XlsToXml(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToXml(OrgField, NewField);
end;

function XlsToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToRec(OrgField, NewField);
end;

function XlsToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToDta(OrgField, NewField);
end;

function XlsToOds(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function XlsToTxt(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToTxt(OrgField, NewField);
end;

function XlsToDbf(OrgField, NewField: TEpiField): TExitResult;
begin
  Result := OdsToDbf(OrgField, NewField);
end;

function TxtToXml(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToXml(OrgField, NewField);
end;

function TxtToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToRec(OrgField, NewField);
end;

function TxtToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToDta(OrgField, NewField);
end;

function TxtToOds(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToTxt(OrgField, NewField);
end;

function TxtToXls(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToXls(OrgField, NewField);
end;

function TxtToDbf(OrgField, NewField: TEpiField): TExitResult;
begin
  result := OdsToDbf(OrgField, NewField);
end;

function DbfToXml(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function DbfToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function DbfToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function DbfToOds(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function DbfToXls(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

function DbfToTxt(OrgField, NewField: TEpiField): TExitResult;
begin
  result := ValidateFieldType(NewField, OrgField.FieldType, erAbortField);
end;

