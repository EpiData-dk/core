
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

constructor TEpiFields.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiFields.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiFields.SaveToStream(St: TStream; Lvl: integer);
begin
  SaveList(St, Lvl, Self, rsFields);
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

    NField := DataFile.NewField(TEpiFieldType(LoadNodeInt(Node, rsType)));
    NField.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiFields.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := nil;
  Result.Id := 'field_id_' + IntToStr(Count);
  AddItem(Result);
end;

{ TEpiField }

procedure TEpiField.SetFieldDecimals(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if FFieldDecimals = AValue then exit;
  Val := FFieldDecimals;
  FFieldDecimals := AValue;
  DoChange(eegDataFiles, Word(efceSetDecimal), @Val);
end;

procedure TEpiField.SetFieldLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if FFieldLeft = AValue then exit;
  Val := FFieldLeft;
  FFieldLeft := AValue;
  DoChange(eegDataFiles, Word(efceSetLeft), @Val);
end;

procedure TEpiField.SetFieldLength(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if FFieldLength = AValue then exit;
  Val := FFieldLength;
  FFieldLength := AValue;
  DoChange(eegDataFiles, Word(efceSetLength), @Val);
end;

procedure TEpiField.SetFieldTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if FFieldTop = AValue then exit;
  Val := FFieldTop;
  FFieldTop := AValue;
  DoChange(eegDataFiles, Word(efceSetTop), @Val);
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

constructor TEpiField.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiField.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiField.SaveToStream(St: TStream; Lvl: integer);
begin
  inherited SaveToStream(St, Lvl);
end;

procedure TEpiField.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
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
  result := Length(FData);
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
  SetLength(FData, AValue);
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
  result := Length(FData);
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
  SetLength(FData, AValue);
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
  result := Length(FData);
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
  else if Length(AValue) > 0 then
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
  SetLength(FData, AValue);
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
  result := Length(FData);
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
  SetLength(FData, AValue);
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

class function TEpiDateField.CheckMissing(AValue: EpiDate): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateField.DefaultMissing: EpiDate;
begin
  result := NA_DATE;
end;


{ TEpiDateTimeField }

class function TEpiDateTimeField.CheckMissing(AValue: EpiDateTime): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateTimeField.DefaultMissing: EpiDateTime;
begin
  result := NA_DATETIME;
end;

