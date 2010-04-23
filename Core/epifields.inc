
{%MainUnit epidatafiles.pas}

const
  NA_INT       = MaxInt;
  NA_FLOAT     = MaxFloat;
  NA_DATE      = NA_INT;
  NA_TIME      = MaxDouble;
  NA_DATETIME  = NA_TIME;
  NA_STRING    = '.';
  NA_BOOL      = $7F;

  Field_Growth_Factor = 1.20;

{ TEpiFields }

function TEpiFields.GetDataFile: TEpiDataFile;
begin
  result := TEpiDataFile(Owner);
end;

function TEpiFields.GetField(index: integer): TEpiField;
begin
  result := TEpiField(Items[index]);
end;

constructor TEpiFields.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiFields.Destroy;
begin
  inherited Destroy;
end;

function TEpiFields.XMLName: string;
begin
  Result := rsFields;
end;

procedure TEpiFields.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NField: TEpiField;
begin
  // Root = <Fields>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsField);

    NField := NewField(TEpiFieldType(LoadNodeInt(Node, rsType)));
    NField.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiFields.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := TEpiField.CreateField(Self, FieldType);
  Result.Id := 'field_id_' + IntToStr(Count);
  AddItem(Result);
end;

{ TEpiField }

procedure TEpiField.SetDecimals(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if Decimals = AValue then exit;
  Val := Decimals;
  FDecimals := AValue;
  DoChange(eegDataFiles, Word(efceSetDecimal), @Val);
end;

procedure TEpiField.SetLength(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if Length = AValue then exit;
  Val := Length;
  FLength := AValue;
  DoChange(eegDataFiles, Word(efceSetLength), @Val);
end;

procedure TEpiField.CheckIndex(const index: integer);
begin
  if (Index < 0) or (Index > Size - 1) then
    Raise Exception.CreateFmt('Index out of bounds: %d', [Index]);
end;

function TEpiField.GetSize: Integer;
begin
  result := FSize;
end;

procedure TEpiField.Grow;
begin
  Capacity := Trunc(Capacity * Field_Growth_Factor);
end;

procedure TEpiField.SetSize(const AValue: Integer);
begin
  if AValue = Size then exit;
  if AValue > Capacity then
    Capacity := AValue;
  FSize := AValue;
end;

constructor TEpiField.Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner);
  FFieldType := AFieldType;
  FQuestion := TEpiHeading.Create(Self);

  RegisterClasses([FQuestion]);
end;

class function TEpiField.IdString: string;
begin
  Result := 'field_id_';
end;

class function TEpiField.CreateField(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType): TEpiField;
begin
  case AFieldType of
    ftInteger, ftAutoInc:
      Result := TEpiIntField.Create(AOwner, AFieldType);

    ftDMYDate,  ftMDYDate,  ftYMDDate,
    ftDMYToday, ftMDYToday, ftYMDToday:
      Result := TEpiDateField.Create(AOwner, AFieldType);

    ftTime, ftTimeNow:
      result := TEpiDateTimeField.Create(AOwner, AFieldType);

    ftFloat:
      Result := TEpiFloatField.Create(AOwner, AFieldType);

    ftBoolean:
      Result := TEpiBoolField.Create(AOwner, AFieldType);

    ftString, ftUpperString:
      Result := TEpiStringField.Create(AOwner, AFieldType);
  else
    result := nil;
  end;
end;

destructor TEpiField.Destroy;
begin
  inherited Destroy;
end;

function TEpiField.XMLName: string;
begin
  Result := rsField;
end;

function TEpiField.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsType, Integer(FieldType)) +
    SaveNode(Lvl + 1, rsLength, Length) +
    SaveNode(Lvl + 1, rsDecimals, Decimals);
  if Assigned(ValueLabelSet) then
    Content += SaveNode(Lvl + 1, rsValueLabelId, ValueLabelSet.Id);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiField.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <Field>
  inherited LoadFromXml(Root);
  Length   := LoadNodeInt(Root, rsLength);
  Decimals := LoadNodeInt(Root, rsDecimals);
  if LoadNode(Node, Root, rsValueLabelId, false) then
    ValueLabelSet := TEpiValueLabelSet(DataFile.ValueLabels.GetItemById(LoadNodeString(Root, rsValueLabelId)));
end;

{ TEpiIntField }

function TEpiIntField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsInteger[index] > 0 then
    result := 1
  else
    result := 0;
end;

function TEpiIntField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsInteger(const index: Integer): EpiInteger;
begin
  CheckIndex(Index);
  result := FData[Index];
end;

function TEpiIntField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := IntToStr(AsInteger[Index]);
end;

function TEpiIntField.GetAsTime(const index: Integer): EpiTime;
begin
  Result := AsDateTime[index];
end;

function TEpiIntField.GetAsValue(const index: Integer): EpiVariant;
begin
  Result := AsInteger[Index];
end;

function TEpiIntField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiIntField.GetIsMissing(const index: Integer): boolean;
begin
  Result := AsInteger[Index] = DefaultMissing;
end;

procedure TEpiIntField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := AValue;
end;

procedure TEpiIntField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := AValue;
end;

procedure TEpiIntField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := Trunc(AValue);
end;

procedure TEpiIntField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
  // TODO : TEpiIntField - Rounding function.
    AsInteger[Index] := trunc(AValue);
end;

procedure TEpiIntField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  CheckIndex(Index);
  FData[Index] := AValue;
end;

procedure TEpiIntField.SetAsString(const index: Integer; const AValue: EpiString
  );
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[index] := StrToIntDef(AValue, DefaultMissing);
end;

procedure TEpiIntField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := Trunc(AValue);
end;

procedure TEpiIntField.SetAsValue(const index: Integer; const AValue: EpiVariant
  );
begin
  AsInteger[index] := AValue;
end;

procedure TEpiIntField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  FCapacity := AValue;
end;

procedure TEpiIntField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsInteger[index] := DefaultMissing;
end;

class function TEpiIntField.CheckMissing(AValue: EpiInteger): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiIntField.DefaultMissing: EpiInteger;
begin
  result := NA_INT;
end;

function TEpiIntField.Compare(i, j: integer): integer;
begin
  result := AsInteger[i] - AsInteger[j];
end;

procedure TEpiIntField.Exchange(i, j: integer);
var
  TmpInt: EpiInteger;
begin
  TmpInt := AsInteger[i];
  AsInteger[i] := AsInteger[j];
  AsInteger[j] := TmpInt;
end;

{ TEpiFloatField }

function TEpiFloatField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsFloat[index] > 0 then
    result := 1
  else
    result := 0;
end;

function TEpiFloatField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiFloatField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsFloat[index];
end;

function TEpiFloatField.GetAsFloat(const index: Integer): EpiFloat;
begin
  CheckIndex(Index);
  Result := FData[index];
end;

function TEpiFloatField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else   // TODO : Rounding function!!!
    result := Trunc(AsFloat[index]);
end;

function TEpiFloatField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := FloatToStr(AsFloat[Index]);
end;

function TEpiFloatField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsFloat[index];
end;

function TEpiFloatField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsFloat[index];
end;

function TEpiFloatField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiFloatField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsFloat[index] = DefaultMissing;
end;

procedure TEpiFloatField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsDate(const index: Integer; const AValue: EpiDate
  );
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  CheckIndex(Index);
  FData[index] := AValue;
end;

procedure TEpiFloatField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := StrToFloatDef(AValue, DefaultMissing);
end;

procedure TEpiFloatField.SetAsTime(const index: Integer; const AValue: EpiTime
  );
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsFloat[index] := AValue;
end;

procedure TEpiFloatField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsFloat[index] := DefaultMissing;
end;

procedure TEpiFloatField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiFloatField.CheckMissing(AValue: EpiFloat): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiFloatField.DefaultMissing: EpiFloat;
begin
  result := NA_FLOAT;
end;

function TEpiFloatField.Compare(i, j: integer): integer;
begin
  Result := Sign(AsFloat[i] - AsFloat[j]);
end;

procedure TEpiFloatField.Exchange(i, j: integer);
var
  TmpFlt: EpiFloat;
begin
  TmpFlt := AsFloat[i];
  AsFloat[i] := AsFloat[j];
  AsFloat[j] := TmpFlt;
end;

{ TEpiBoolField }

function TEpiBoolField.GetAsBoolean(const index: Integer): EpiBool;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiBoolField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else     // TODO: translation of boolean characters.
    result := BoolToStr(AsBoolean[index] = 0, 'Y', 'N')
end;

function TEpiBoolField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsBoolean[index];
end;

function TEpiBoolField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiBoolField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsBoolean[index] = DefaultMissing;
end;

procedure TEpiBoolField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  CheckIndex(Index);
  FData[index] := AValue;
end;

procedure TEpiBoolField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if System.Length(AValue) > 0 then
  begin
    // TODO : Better recognition of boolean strings.
    if AValue[1] in ['Y'] then
      AsBoolean[Index] := 1
    else
      AsBoolean[Index] := 0;
  end else
    AsBoolean[Index] := 0;
end;

procedure TEpiBoolField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsBoolean[index] := AValue;
end;

procedure TEpiBoolField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsBoolean[index] := DefaultMissing;
end;

procedure TEpiBoolField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiBoolField.CheckMissing(AValue: EpiBool): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiBoolField.DefaultMissing: EpiBool;
begin
  result := NA_BOOL;
end;

function TEpiBoolField.Compare(i, j: integer): integer;
begin
  Result := Sign(AsBoolean[i]) - Sign(AsBoolean[j]);
end;

procedure TEpiBoolField.Exchange(i, j: integer);
var
  TmpBool: EpiBool;
begin
  TmpBool := AsBoolean[i];
  AsBoolean[i] := AsBoolean[j];
  AsBoolean[j] := TmpBool;
end;

{ TEpiStringField }

function TEpiStringField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if Trim(AsString[index]) <> '' then
    result := 1
  else
    result := 0;
end;

function TEpiStringField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else        // TODO : String To Date conversion.
    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else        // TODO : String To Date conversion.
    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := StrToFloatDef(AsString[index], TEpiFloatField.DefaultMissing);
end;

function TEpiStringField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := StrToIntDef(AsString[index], TEpiIntField.DefaultMissing);
end;

function TEpiStringField.GetAsString(const index: Integer): EpiString;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiStringField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else        // TODO : String To Time conversion.
    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsString[index];
end;

function TEpiStringField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiStringField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsString[index] = DefaultMissing;
end;

procedure TEpiStringField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue >= 1 then
    AsString[Index] := 'Y' // TODO : Tranlation for boolean characters.
  else
    AsString[Index] := 'N' // TODO : Tranlation for boolean characters.
end;

procedure TEpiStringField.SetAsDate(const index: Integer; const AValue: EpiDate
  );
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                 // TODO : Date to String conversion.
    AsString[index] := DateToStr(TDateTime(AValue));
end;

procedure TEpiStringField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                 // TODO : Date to String conversion.
    AsString[index] := DateToStr(AValue);
end;

procedure TEpiStringField.SetAsFloat(const index: Integer;
  const AValue: EpiFloat);
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsString[index] := FloatToStr(AValue);
end;

procedure TEpiStringField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsString[index] := IntToStr(AValue);
end;

procedure TEpiStringField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  CheckIndex(Index);
  FData[index] := AValue;
end;

procedure TEpiStringField.SetAsTime(const index: Integer; const AValue: EpiTime
  );
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                 // TODO : Date to String conversion.
    AsString[index] := DateToStr(AValue);
end;

procedure TEpiStringField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsString[index] := AValue;
end;

procedure TEpiStringField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsString[index] := DefaultMissing;
end;

procedure TEpiStringField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiStringField.CheckMissing(AValue: EpiString): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiStringField.DefaultMissing: EpiString;
begin
  result := NA_STRING;
end;

function TEpiStringField.Compare(i, j: integer): integer;
begin
  Result := CompareStr(AsString[i], AsString[j]);
end;

procedure TEpiStringField.Exchange(i, j: integer);
var
  TmpStr: string;
begin
  TmpStr := AsString[i];
  AsString[i] := AsString[j];
  AsString[j] := TmpStr;
end;

{ TEpiDateField }

function TEpiDateField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsDate[index] >= 1 then
    result := 1
  else
    result := 0;
end;

function TEpiDateField.GetAsDate(const index: Integer): EpiDate;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiDateField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsDate[Index];
end;

function TEpiDateField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsDate[index];
end;

function TEpiDateField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := AsDate[index];
end;

function TEpiDateField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := DateToStr(AsDateTime[index]);
end;

function TEpiDateField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsDate[Index];
end;

function TEpiDateField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsDate[index];
end;

function TEpiDateField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiDateField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsDate[index] = DefaultMissing;
end;

class function TEpiDateField.CheckMissing(AValue: EpiDate): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateField.DefaultMissing: EpiDate;
begin
  result := NA_DATE;
end;

function TEpiDateField.Compare(i, j: integer): integer;
begin
  Result := AsDate[i] - AsDate[j];
end;

procedure TEpiDateField.Exchange(i, j: integer);
var
  TmpDate: EpiDate;
begin
  TmpDate := AsDate[i];
  AsDate[i] := AsDate[j];
  AsDate[j] := TmpDate;
end;

procedure TEpiDateField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDate[index] := AValue;
end;

procedure TEpiDateField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  CheckIndex(Index);
  FData[index] := AValue;
end;

procedure TEpiDateField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDate[index] := AValue;
end;

procedure TEpiDateField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                   // TODO : Date conversion routine.
    AsDate[index] := Trunc(StrToDateDef(AValue, DefaultMissing));
end;

procedure TEpiDateField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsDate[index] := AValue;
end;

procedure TEpiDateField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsDate[index] := DefaultMissing;
end;

procedure TEpiDateField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  FCapacity := AValue;
end;

{ TEpiDateTimeField }

function TEpiDateTimeField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else
    result := Sign(AsDateTime[index]);
end;

function TEpiDateTimeField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else        // TODO : Rounding function
    result := Trunc(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiDateTimeField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsDateTime[Index];
end;

function TEpiDateTimeField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := Trunc(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := DateTimeToStr(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := DefaultMissing
  else
    result := frac(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsValue(const index: Integer): EpiVariant;
begin
  Result := AsDateTime[index];
end;

function TEpiDateTimeField.GetCapacity: Integer;
begin
  Result := System.Length(FData);
end;

function TEpiDateTimeField.GetIsMissing(const index: Integer): boolean;
begin
  Result := AsDateTime[index] = NA_DATETIME;
end;

procedure TEpiDateTimeField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsDate(const index: Integer;
  const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  CheckIndex(index);
  FData[Index] := AValue;
end;

procedure TEpiDateTimeField.SetAsFloat(const index: Integer;
  const AValue: EpiFloat);
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := StrToDateDef(AValue, DefaultMissing);
end;

procedure TEpiDateTimeField.SetAsTime(const index: Integer;
  const AValue: EpiTime);
begin
  AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsDateTime[index] := DefaultMissing;
end;

procedure TEpiDateTimeField.SetCapacity(AValue: Integer);
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  FCapacity := AValue;
end;

class function TEpiDateTimeField.CheckMissing(AValue: EpiDateTime): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateTimeField.DefaultMissing: EpiDateTime;
begin
  result := NA_DATETIME;
end;

function TEpiDateTimeField.Compare(i, j: integer): integer;
begin
  Result := Sign(AsDateTime[i] - AsDateTime[j]);
end;

procedure TEpiDateTimeField.Exchange(i, j: integer);
var
  TmpDateTime: EpiDateTime;
begin
  TmpDateTime := AsDateTime[i];
  AsDateTime[i] := AsDateTime[j];
  AsDateTime[j] := TmpDateTime;
end;

