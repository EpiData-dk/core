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
  end;
}

function Max(A, B: TExitResult): TExitResult; overload;
begin
  if A < B then
    result := B
  else
    result := A;
end;

function XmlToRec(OrgField, NewField: TEpiField): TExitResult;
begin
  result := Validate(OrgField.FieldLength = NewField.FieldLength, erAbortField);
  if Result >= erAbortField then Exit;

  result := Validate(OrgField.FieldDecimals = NewField.FieldDecimals, erAbortField);
  if Result >= erAbortField then Exit;
end;

function XmlToDta(OrgField, NewField: TEpiField): TExitResult;
begin
  Case OrgField.FieldType of
    ftInteger, ftIDNUM:
      begin
        if OrgField.FieldLength <= 2 then
          result := Validate(NewField.FieldLength = 3, erAbortField)
        else if OrgField.FieldLength <= 5 then
          result := Validate(NewField.FieldLength = 5, erAbortField)
        else if OrgField.FieldLength <= 9 then
          result := Validate(NewField.FieldLength = 10, erAbortField)
        else  // Exports as float field.
          result := Validate(NewField.FieldLength = 18, erAbortField);
      end;
    ftDate, ftToday, ftEuroDate, ftEuroToday, ftYMDDate, ftYMDToday:
      result := Validate(OrgField.FieldLength = NewField.FieldLength, erAbortField);
    ftFloat:
      begin
        result := Validate(NewField.FieldLength = 18, erAbortField);
        result := Max(result, Validate(NewField.FieldDecimals = 4, erAbortField));
      end;
    ftBoolean:
      result := Validate(NewField.FieldLength = 3);
    ftString, ftUpperAlfa, ftSoundex, ftCrypt:
      ;
  end;
end;

function XmlToOds(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XmlToXls(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XmlToTxt(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function XmlToDbf(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function RecToXml(OrgField, NewField: TEpiField): TExitResult;
begin
  result := Validate(OrgField.FieldName = NewField.FieldName, erAbortField);
  if Result >= erAbortField then Exit;

  result := Validate(OrgField.FieldLength = NewField.FieldLength, erAbortField);
  if Result >= erAbortField then Exit;

  result := Validate(OrgField.FieldDecimals = NewField.FieldDecimals, erAbortField);
  if Result >= erAbortField then Exit;
end;

function RecToDta(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function RecToOds(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function RecToXls(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function RecToTxt(OrgField, NewField: TEpiField): TExitResult;
begin

end;

function RecToDbf(OrgField, NewField: TEpiField): TExitResult;
begin

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

