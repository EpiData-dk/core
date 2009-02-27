unit UeFields;

interface

uses
  SysUtils,Graphics,classes, UEpiTypes, UValueLabels;


TYPE
  TeField=Class(TObject)
  private
    FEpiDataFile:     TObject;        //TEpiDataFile that owns the field
    FName:            String;         //Fieldname
    FFieldText:       String;         //Entry made in the field (= text property)
    FData:            Array of string;
    FScope:           TScopes;        //Scope (Local, global, comulative), used only in DEFINEd variables
    FMissingValues:   TMissingValues; //legal missing values
    FVariableLabel:   String;         //Variable label
    FFieldtype:       TFieldtypes;    //Field type
    FLength:          Byte;           //Length of data in field
    FCryptEntryLength:Byte;           //Entrylength of encrypt fields (Flength is coded length)   //&&
    FLengthInFile:    Byte;           //Length of field in file
    FNumDecimals:     Byte;           //Number of decimals in numeric fields
    FQuestion:        String;     //The field's question (to the left of field)
    FOriginalQuest:   String;     //Question as it is saved in REC-file
    FLastField:        Boolean;      //True if field is last field in dataform
    FStartPos:        Integer;       //Start position in datafile record of field
    FFieldChar:       Char;
    FFieldColor:      Integer;
    FQuestColor:      Integer;
    FFieldNo:         Integer;
    {Entryfield coordinates}
    FQuestTop:        Integer;      //Question pixel-based top-position   *
    FQuestLeft:       Integer;      //Question pixel-based left-position   *
    FFieldTop:        Integer;      //Entry field's pixel-based top         *
    FFieldLeft:       Integer;      //Entry field's pixel-based left         *
    FFieldWidth:      Integer;      //Entry field's width in pixels          *
    FFieldX,FFieldY:  Integer;      //Field's coordinates in characters
    FQuestX,FQuestY:  Integer;      //Question's coordinates in characters
    {Check related properties}
    FMustEnter:       Boolean;      //True if MUSTENTER is set
    FRepeat:          Boolean;      //True if REPEAT is set
    FMin:             String;       //Mininum value (set by RANGE)
    FMax:             String;       //Maximum value (set by RANGE)
    FLegal:           String;       //Legal values (incl. RANGE values)
    FRangeDefined:    Boolean;      //True if FLegal includes a Range definition
//    FCommentLegalRec: PLabelRec;    //Pointer to comment legal record (value label)
    FShowLegalPickList: Boolean;    //True if Comment Legal Show (forces picklist to be shown)
    FPickListNoSelect: Boolean;     //If True then no item is automatically selected in LegalPickList
    FJumps:           String;       //Jumps definitions
    FJumpResetChar:   Char;         //Fill char when JUMPS RESET "-" is used
    {autosearch properties}
    FAutosearch:      Boolean;      //True if field has autosearch
    FAutoFields:      String;       //CSV-string of fields to autosearch
    FAutoList:        Boolean;      //True if Autosearch has LIST parameter set
    {other properties}
    FNoEnter:         Boolean;
    FEntryField:       TObject;      //Pointer to TEntryField on Dataform
    FFieldComments:   String;       //Comments in checkfile in fieldblock - used only in checkfilemode
    FFieldN:           Integer;      //'Free' property for different uses
    FIndex:           Byte;         //Key number = index number
    FIsTypeStatusBar: Boolean;      //Indicates if field has TYPE STATUSBAR
    FTypeComments:    Boolean;      //Fields has TYPE COMMENT
    FTypeString:      Boolean;      //Indicates that field has a TYPE "lkjlkj" command
    FTypeCommentField: Integer;     //Used with TYPE COMMENT fieldname - holds number of field to receive the comment
    FTypeCommentFieldStr: string;   //Used with TYPE COMMENT fieldname - holds name of field to receive the comment
    FTypeField:       TObject;       //Label on dataform with TYPE-text - var tidligere TLabel
    FTypeColor:       TColor;       //Color of TYPE-label
    FConfirm:         Boolean;      //If true then confirm with ENTER before field is left (overrides df^.Confirm)
    FAfterCmds:        TList;        //Commands run After Entry
    FBeforeCmds:       TList;        //Commands run Before Entry
    FOldReadOnly:      Boolean;      //Used to save the ReadOnly status before setting ReadOnly=True during Relate-operations
    FTopOfScreen:     Boolean;      //True=Move field to top of screen when entered ("New Page")
    FTopOfScreenLines: Byte;        //Number of lines to move topofscreen field down
    FHasGlobalMissing: Boolean;   //Set if MISSINGVALUE var1-var2 9 8 7 type command is run previously
    FDefaultValue:     string;      //A default value of the field defined by DEFAULTVALUE x
    FHasGlobalDefaultValue: Boolean;   //A default value define by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FFieldFormat:     string;       //Used by analysis: defines the formatting of the data, e.g. %d for integers
    FValuelabel:      TValueLabelSet;

    Function  GetFieldName:String;
    Function  GetAsString:String;
    Procedure SetAsString(Value:String);
    Function  GetAsLabel:String;
    Function  GetHasValueLabels:Boolean;
    Function  GetMissingValues(Index: Integer):string;
    Procedure SetMissingValues(Index: Integer; Value:string);
    Function  GetFieldtypeName:string;
    function  Lang(langcode: Integer;  const langtext: string): String;
  public
    constructor Create;
    destructor  Destroy; override;
    Procedure   ResetCheckProperties;
    Procedure   ResetField;
    Function    HasCheckProperties:Boolean;
    Function    HasSpecialChecks:Boolean;
    Function    Value2Label(Value: string):string;
    Procedure   Clone(dest: TeField; clonevalue:boolean=false);

    Property    AsString:String read GetAsString write SetAsString;
    Property    AsLabel:String read GetAsLabel;
    Property    FieldName:String read GetFieldName write FName;
    Property    Scope:TScopes read FScope write FScope;
    Property    HasValueLabels:Boolean read GetHasValueLabels;
    Property    MissingValues[Index: integer]:string read GetMissingValues write SetMissingValues;
    Property    Fieldtype: TFieldTypes read FFieldtype write FFieldtype;
    Property    FieldtypeName:string read getFieldtypeName;
    Property    VariableLabel:string read FVariableLabel write FVariableLabel;
    Property    Length:byte read FLength write FLength;
    Property    CryptEntryLength:Byte read FCryptEntryLength write FCryptEntryLength;
    Property    LengthInFile:Byte read FLengthInFile write FLengthInFile;
    Property    NumDecimals:Byte read FNumDecimals write FNumDecimals;
    Property    Question:String read FQuestion write FQuestion;
    Property    OriginalQuest:String read FOriginalQuest write FOriginalQuest;
    Property    LastField:Boolean read FLastField write FLastField;
    Property    StartPos:Integer read FStartPos write FStartPos;
    Property    FieldChar:Char read FFieldChar write FFieldChar;
    Property    FieldColor:Integer read FFieldColor write FFieldColor;
    Property    QuestColor:Integer read FQuestColor write FQuestColor;
    Property    FieldNo:Integer read FFieldNo write FFieldNo;
    {Entryfield coordinates}
    Property    QuestTop:Integer read FQuestTop write FQuestTop;
    Property    QuestLeft:Integer read FQuestLeft write FQuestLeft;
    Property    FieldTop:Integer read FFieldTop write FFieldTop;
    Property    FieldLeft:Integer read FFieldLeft write FFieldLeft;
    Property    FieldWidth:Integer read FFieldWidth write FFieldWidth;
    Property    FieldX:Integer read FFieldX write FFieldX;
    Property    FieldY:Integer read FFieldY write FFieldY;
    Property    QuestX:Integer read FQuestX write FQuestX;
    Property    QuestY:Integer read FQuestY write FQuestY;
    {Check related properties}
    Property    MustEnter:Boolean read FMustEnter write FMustEnter;
    Property    doRepeat:Boolean read FRepeat write FRepeat;      //True if REPEAT is set
    Property    Min:String read FMin write FMin;
    Property    Max:String read FMax write FMin;
    Property    Legal:String read FLegal write FLegal;
    Property    RangeDefined:Boolean read FRangeDefined write FRangeDefined;
    //Property    CommentLegalRec: PLabelRec;   //TODO: skal ændres til typer i UValueLabel
    Property    ShowLegalPickList:Boolean read FShowLegalPickList write FShowLegalPickList;
    Property    PickListNoSelect:Boolean read FPickListNoSelect write FPickListNoSelect;
    Property    Jumps:String read FJumps write FJumps;
    Property    JumpResetChar:Char read FJumpResetChar write FJumpResetChar;
    {autosearch properties}
    Property    Autosearch:Boolean read FAutosearch write FAutosearch;
    Property    AutoFields:String read FAutoFields write FAutoFields;
    Property    AutoList:Boolean read FAutoList write FAutoList;
    {other properties}
    Property    NoEnter:Boolean read FNoEnter write FNoEnter;
    Property    EntryField:TObject read FEntryField write FEntryField;
    Property    FieldComments:String read FFieldComments write FFieldComments;
    Property    FieldN:Integer read FFieldN write FFieldN;
    Property    Index:Byte read FIndex write FIndex;
    Property    IsTypeStatusBar:Boolean read FIsTypeStatusBar write FIsTypeStatusBar;
    Property    TypeComments:Boolean read FTypeComments write FTypeComments;
    Property    TypeString:Boolean read FTypeString write FTypeString;
    Property    TypeCommentField:Integer read FTypeCommentField write FTypeCommentField;
    Property    TypeCommentFieldStr:string read FTypeCommentFieldStr write FTypeCommentFieldStr;
    Property    TypeField:TObject read FTypeField write FTypeField;
    Property    TypeColor:TColor read FTypeColor write FTypeColor;
    Property    Confirm:Boolean read FConfirm write FConfirm;
    Property    AfterCmds:TList read FAfterCmds write FAfterCmds;
    Property    BeforeCmds:TList read FBeforeCmds write FBeforeCmds;
    Property    OldReadOnly:Boolean read FOldReadOnly write FOldReadOnly;
    Property    TopOfScreen:Boolean read FTopOfScreen write FTopOfScreen;
    Property    TopOfScreenLines:Byte read FTopOfScreenLines write FTopOfScreenLines;
    Property    HasGlobalMissing:Boolean read FHasGlobalMissing write FHasGlobalMissing;
    Property    DefaultValue:string read FDefaultValue write FDefaultValue;
    Property    HasGlobalDefaultValue:Boolean read FHasGlobalDefaultValue write FHasGlobalDefaultValue;
    Property    FieldFormat:string read FFieldFormat write FFieldFormat;
    Property    Valuelabel:TValueLabelSet read FValueLabel write FValueLabel;
  END;

  TeFields = class(TObject)
    private
      FEpiDataFile:     TObject;      //TEpiDataFile that owns the fieldlist
      FList: TList;
      function  GetCount:integer;
      function  GetField(index:integer):TeField;
    public
      constructor create;
      destructor destroy; override;
      procedure  Add(field: TeField);
      function   FieldByIndex(index:integer):TeField;
      function   FieldByName(name:string):TeField;
      property   count:integer read GetCount;
      property   items[index:integer]:TeField read getField; default;
  end;



implementation

uses
  UEpiDataFile;

// TeFields ****************************************

constructor TeFields.create;
begin
  FList:=TList.create;
end;

destructor TeFields.destroy;
var
  n:integer;
begin
  if FList.Count>0 then
    for n:=0 to FList.Count-1 do
      TeField(FList[n]).Free;
  FList.Free;
end;

function TeFields.GetCount:integer;
begin
  result:=FList.count;
end;

procedure TeFields.Add(field: TeField);
begin
  field.FEpiDataFile:=FEpiDataFile;
  FList.Add(field);
end;

function  TeFields.GetField(index:integer):TeField;
begin
  if index>=FList.Count then result:=NIL
  else result:=TeField(FList[index]);
end;

function TeFields.FieldByIndex(index:integer):TeField;
begin
  if index>=FList.Count then result:=NIL
  else result:=TeField(FList[index]);
end;

function TeFields.FieldByName(name:string):TeField;
var
  n:integer;
begin
  result:=NIL;
  if FList.Count=0 then exit;
  n:=0;
  name:=AnsiLowerCase(name);
  while (n<FList.count) and (result=NIL) do
    begin
      if AnsiLowerCase(TeField(FList[n]).FieldName)=name then result:=TeField(FList[n]);
      inc(n);
    end;
end;

// TeField **************************************************************

Procedure TeField.ResetCheckProperties;
BEGIN
  FMustEnter:=False;
  FRepeat:=False;
  FMin:='';
  FMax:='';
  FLegal:='';
  FRangeDefined:=False;
//  FCommentLegalRec:=NIL;
  FShowLegalPickList:=False;
  FPickListNoSelect:=False;
  FFieldComments:='';
  //FValueLabel:='';  //TODO ordnes når Torsten har valuelabels på plads
  FJumps:='';
  FJumpResetChar:=#0;
  FNoEnter:=False;
  FIndex:=0;
  FIsTypeStatusBar:=False;
  FTypeColor:=0;
  FTypeComments:=False;
  FTypeString:=False;
  FTypeCommentField:=-1;
  FTypeCommentFieldStr:='';
  FConfirm:=False;
  FTopOfScreen:=False;
  FTopOfScreenLines:=0;
  FTypeField:=NIL;
  //HUSK HER: Kald aftercmds.free - commandlist skal kunne frigive sine egne kommandoer
  AfterCmds:=NIL;
  BeforeCmds:=NIL;
  //*************************************************'
  FMissingValues[0]:='';
  FMissingValues[1]:='';
  FMissingValues[2]:='';
  FAutosearch:=False;
  FAutoFields:='';
  FAutoList:=False;
END;

Procedure TeField.ResetField;
BEGIN
  ResetCheckProperties;
  FName:='';
  FVariableLabel:='';
  FFieldtype:=ftInteger;
  FLength:=0;
  FCryptEntryLength:=0;
  FNumDecimals:=0;
  FQuestion:='';
  FOriginalQuest:='';
  LastField:=False;
  FFieldText:='';
  FStartPos:=0;
  EntryField:=NIL;
  FFieldComments:='';
  FieldN:=0;
  FTypeField:=NIL;
  OldReadOnly:=False;
  FHasGlobalMissing:=False;
  FFieldFormat:='';
END;

Constructor TeField.Create;
BEGIN
  inherited Create;
  ResetField;
END;

Destructor TeField.Destroy;
BEGIN
  Resetfield;
  inherited Destroy;
END;

Function TeField.HasCheckProperties:Boolean;
BEGIN
  IF (FMin<>'') OR (FMax<>'') OR (FLegal<>'') OR (FJumps<>'')
  //OR (trim(FValueLabel)<>'') OR (FMustEnter=True) OR (FRepeat=True)   //TODO når valuelabels er på plads
  OR (FFieldComments<>'') OR (AfterCmds<>NIL) OR (BeforeCmds<>NIL)
  OR (FNoEnter=True) OR (FIsTypeStatusBar=True) OR (FTypeComments)
  OR (FIndex>0) OR (FConfirm) OR (FTopOfScreen) OR (FAutosearch)
  OR (FMissingValues[0]<>'') OR (FMissingValues[1]<>'') OR (FMissingValues[2]<>'')
  THEN Result:=True ELSE Result:=False;
END;

Function TeField.HasSpecialChecks:Boolean;
BEGIN
  IF (FFieldComments<>'') OR (AfterCmds<>NIL) OR (BeforeCmds<>NIL)
  OR (FNoEnter=True) OR (FIsTypeStatusBar=True) OR (FTypeComments)
  OR (FIndex>0) OR (FConfirm)
  THEN Result:=True ELSE Result:=False;
END;

Function TeField.GetAsString:String;
BEGIN
  Result:=trim(FFieldText);
END;

Procedure TeField.SetAsString(Value:String);
BEGIN
  if FFieldtype=ftCrypt then FFieldText:=copy(Value,1,FCryptEntryLength) else FFieldText:=Copy(Value,1,FLength);
END;

Function TeField.GetAsLabel:String;
BEGIN
  //TODO: afventer valuelabelstruktur fra Torsten
  //IF FCommentLegalRec<>NIL
  //THEN result:=trim(GetCommentLegalText(FFieldText,FCommentLegalRec))
  //ELSE result:=trim(FFieldText);
END;

Function TeField.Value2Label(Value: string):string;
BEGIN
  //TODO: afventer valuelabelstruktur fra Torsten
  //IF FCommentLegalRec<>NIL
  //THEN result:=trim(GetCommentLegalText(Value,FCommentLegalRec))
  //ELSE result:='';
END;

Function TeField.GetHasValueLabels:Boolean;
BEGIN
//  Result:=(FCommentLegalRec<>NIL);
END;


Function TeField.GetFieldname:String;
BEGIN
  Result:=trim(FName);
END;

Function TeField.GetMissingValues(Index: Integer):string;
BEGIN
  Result:=FMissingValues[Index];
END;

Procedure TeField.SetMissingValues(Index: Integer; Value:string);
BEGIN
  FMissingValues[Index]:=Value;
END;

Procedure TeField.Clone(dest: TeField; clonevalue:boolean=false);
begin
  if (not assigned(dest)) then raise Exception.Create('Destination field is not assigned');
  //TODO: mangler håndtering af FData
  dest.FName:=FName;
  dest.FVariableLabel:=FVariableLabel;
  dest.Fieldtype:=FFieldtype;
  dest.FLength:=FLength;
  dest.FCryptEntryLength:=FCryptEntryLength;
  dest.FLengthInFile:=FLengthInFile;
  dest.FNumDecimals:=FNumDecimals;
  dest.FQuestion:=FQuestion;
  dest.FOriginalQuest:=FOriginalQuest;
  dest.LastField:=LastField;
  if (clonevalue) then dest.FFieldText:=FFieldText;
  dest.FStartPos:=FStartPos;
  dest.EntryField:=EntryField;
  dest.FFieldComments:=FFieldComments;
  dest.FieldN:=FieldN;
  dest.FTypeField:=FTypeField;
  dest.OldReadOnly:=OldReadOnly;
  dest.FHasGlobalMissing:=FHasGlobalMissing;
  dest.FMustEnter:=FMustEnter;
  dest.FRepeat:=FRepeat;
  dest.FMin:=FMin;
  dest.FMax:=FMax;
  dest.FLegal:=FLegal;
  dest.FRangeDefined:=FRangeDefined;

//  dest.FCommentLegalRec:=NIL;      //Mangler implementering

  dest.FShowLegalPickList:=FShowLegalPickList;
  dest.FPickListNoSelect:=FPickListNoSelect;
  dest.FFieldComments:=FFieldComments;
  //dest.FValueLabel:='';   //Mangler implementering;   //TODO når valuelabels er på plads
  dest.FJumps:=FJumps;
  dest.FJumpResetChar:=FJumpResetChar;
  dest.FNoEnter:=FNoEnter;
  dest.FIndex:=FIndex;
  dest.FIsTypeStatusBar:=FIsTypeStatusBar;
  dest.FTypeColor:=FTypeColor;
  dest.FTypeComments:=FTypeComments;
  dest.FTypeString:=FTypeString;
  dest.FTypeCommentField:=FTypeCommentField;
  dest.FTypeCommentFieldStr:=FTypeCommentFieldStr;
  dest.FConfirm:=FConfirm;
  dest.FTopOfScreen:=FTopOfScreen;
  dest.FTopOfScreenLines:=FTopOfScreenLines;
  dest.FTypeField:=FTypeField;

  dest.AfterCmds:=NIL;  //Mangler implementering
  dest.BeforeCmds:=NIL; //Mangler implementering

  dest.FMissingValues[0]:=FMissingValues[0];
  dest.FMissingValues[1]:=FMissingValues[1];
  dest.FMissingValues[2]:=FMissingValues[2];
  dest.FAutosearch:=FAutosearch;
  dest.FAutoFields:=FAutoFields;
  dest.FAutoList:=FAutoList;


  dest.FStartPos:=FStartPos;
  dest.FFieldChar:=FFieldChar;
  dest.FFieldColor:=FFieldColor;
  dest.FQuestColor:=FQuestColor;
  dest.FFieldNo:=FFieldNo;
  dest.FQuestTop:=FQuestTop;
  dest.FQuestLeft:=FQuestLeft;
  dest.FFieldTop:= FFieldTop;
  dest.FFieldLeft:=FFieldLeft;
  dest.FFieldWidth:=FFieldWidth;
  dest.FFieldX:= FFieldX;
  dest.FFieldY:= FFieldY;
  dest.FQuestX:=FQuestX;
  dest.FQuestY:=FQuestY;
  dest.FDefaultValue:=FDefaultValue;
  dest.FHasGlobalDefaultValue:=FHasGlobalDefaultValue;
end;

function TeField.Lang(langcode: Integer;  const langtext: string): String;
begin
  result:=langtext;
  if (not assigned(FEpiDataFile)) then exit;
  if (not assigned(TEpiDataFile(FEpiDataFile).onTranslate)) then exit;
  result:=TEpiDataFile(FEpiDataFile).OnTranslate(langcode,langtext);
end;

function TeField.GetFieldtypeName:string;
var
  n:integer;
begin
  n:=ORD(FFieldtype);
  case n of
    0: result:=lang(50100,'Numeric');
    1: result:=lang(50101,'Text');
    2: result:=lang(50102,'Date (mdy)');
    3: result:=lang(50103,'Uppercase text');
    4: result:='Checkbox';
    5: result:=lang(50105,'Boolean');
    6: result:=lang(50100,'Numeric');
    7: result:='Phonenumber';
    8: result:='Time';
    9: result:='Local phonenumber';
    10: result:=lang(50110,'Today (mdy)');
    11: result:=lang(50111,'Date (dmy)');
    12: result:=lang(50112,'ID-number');
    15: result:=lang(50115,'Question');
    16: result:=lang(50116,'Today (dmy)');
    17: result:=lang(50117,'Soundex');
    18: result:=lang(50118,'Encryptfield');
    19: result:=lang(50119,'Date (ymd)');
    20: result:=lang(50120,'Today (ymd)');
  else
    result:='Unknown type';
  end;
end;

// ********************** TeField END***************************



end.
