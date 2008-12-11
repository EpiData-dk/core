unit UeFields;

interface
  TScopes=(scLocal,scGlobal,scCumulative);

  TeFields = class(TList)
    private
    public
  end;

  TeField=Class(TObject)
  private
    FEpiDataFile:     TObject;      //TEpiDataFile that owns the field
    FName:            String;       //Fieldname
    FFieldText:       String;       //Entry made in the field (= text property)
    FScope:           TScopes;      //Scope (Local, global, comulative), used only in DEFINEd variables
    FMissingValues:   TMissingValues;    //legal missing values
    FVariableLabel:   String[80];   //Variable label
    Felttype:         TFelttyper;   //Field type
    FLength:          Byte;         //Length of data in field
    FCryptEntryLength:Byte;         //Entrylength of encrypt fields (Flength is coded length)   //&&
    FLengthInFile:    Byte;         //Length of field in file
    FNumDecimals:     Byte;         //Number of decimals in numeric fields
    FQuestion:        String[80];   //The field's question (to the left of field)
    FOriginalQuest:   String[80];   //Question as it is saved in REC-file
    LastField:        Boolean;      //True if field is last field in dataform
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
    FCommentLegalRec: PLabelRec;    //Pointer to comment legal record (value label)
    FShowLegalPickList: Boolean;    //True if Comment Legal Show (forces picklist to be shown)
    FPickListNoSelect: Boolean;     //If True then no item is automatically selected in LegalPickList
    FValueLabel:      String[40];   //Name of value label = Comment legal label
    FJumps:           String;       //Jumps definitions
    FJumpResetChar:   Char;         //Fill char when JUMPS RESET "-" is used
    {autosearch properties}
    FAutosearch:      Boolean;      //True if field has autosearch
    FAutoFields:      String;       //CSV-string of fields to autosearch
    FAutoList:        Boolean;      //True if Autosearch has LIST parameter set
    {other properties}
    FNoEnter:         Boolean;
    EntryField:       Pointer;      //Pointer to TEntryField on Dataform
    FFieldComments:   String;       //Comments in checkfile in fieldblock - used only in checkfilemode
    FieldN:           Integer;      //'Free' property for different uses
    FIndex:           Byte;         //Key number = index number
    FIsTypeStatusBar: Boolean;      //Indicates if field has TYPE STATUSBAR
    FTypeComments:    Boolean;      //Fields has TYPE COMMENT
    FTypeString:      Boolean;      //Indicates that field has a TYPE "lkjlkj" command
    FTypeCommentField: Integer;     //Used with TYPE COMMENT fieldname - holds number of field to receive the comment
    FTypeCommentFieldStr: string;   //Used with TYPE COMMENT fieldname - holds name of field to receive the comment
    FTypeField:       TLabel;       //Label on dataform with TYPE-text
    FTypeColor:       TColor;       //Color of TYPE-label
    FConfirm:         Boolean;      //If true then confirm with ENTER before field is left (overrides df^.Confirm)
    AfterCmds:        TList;        //Commands run After Entry
    BeforeCmds:       TList;        //Commands run Before Entry
    OldReadOnly:      Boolean;      //Used to save the ReadOnly status before setting ReadOnly=True during Relate-operations
    FTopOfScreen:     Boolean;      //True=Move field to top of screen when entered ("New Page")
    FTopOfScreenLines: Byte;        //Number of lines to move topofscreen field down
    FHasGlobalMissing: Boolean;   //Set if MISSINGVALUE var1-var2 9 8 7 type command is run previously
    FDefaultValue:     string;      //A default value of the field defined by DEFAULTVALUE x
    FHasGlobalDefaultValue: Boolean;   //A default value define by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FFieldFormat:     string;       //Used by analysis: defines the formatting of the data, e.g. %d for integers

    Function  GetFieldName:String;
    Function  GetAsString:String;
    Procedure SetAsString(Value:String);
    Function  GetAsLabel:String;
    Function  GetHasValueLabels:Boolean;
    Function  GetMissingValues(Index: Integer):Str15;
    Procedure SetMissingValues(Index: Integer; Value:str15);
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
    Property    MissingValues[Index: integer]:str15 read GetMissingValues write SetMissingValues;
  END;

implementation

// TeField **************************************************************

Procedure TeField.ResetCheckProperties;
BEGIN
  FMustEnter:=False;
  FRepeat:=False;
  FMin:='';
  FMax:='';
  FLegal:='';
  FRangeDefined:=False;
  FCommentLegalRec:=NIL;
  FShowLegalPickList:=False;
  FPickListNoSelect:=False;
  FFieldComments:='';
  FValueLabel:='';
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
  Felttype:=ftInteger;
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
  OR (trim(FValueLabel)<>'') OR (FMustEnter=True) OR (FRepeat=True)
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
  if Felttype=ftCrypt then FFieldText:=copy(Value,1,FCryptEntryLength) else FFieldText:=Copy(Value,1,FLength);
END;

Function TeField.GetAsLabel:String;
BEGIN
  IF FCommentLegalRec<>NIL
  THEN result:=trim(GetCommentLegalText(FFieldText,FCommentLegalRec))
  ELSE result:=trim(FFieldText);
END;

Function TeField.Value2Label(Value: string):string;
BEGIN
  IF FCommentLegalRec<>NIL
  THEN result:=trim(GetCommentLegalText(Value,FCommentLegalRec))
  ELSE result:='';
END;

Function TeField.GetHasValueLabels:Boolean;
BEGIN
  Result:=(FCommentLegalRec<>NIL);
END;


Function TeField.GetFieldname:String;
BEGIN
  Result:=trim(FName);
END;

Function TeField.GetMissingValues(Index: Integer):Str15;
BEGIN
  Result:=FMissingValues[Index];
END;

Procedure TeField.SetMissingValues(Index: Integer; Value:str15);
BEGIN
  FMissingValues[Index]:=Value;
END;

Procedure TeField.Clone(dest: TeField; clonevalue:boolean=false);
begin
  if (not assigned(dest)) then raise Exception.Create('Destination field is not assigned');
  dest.FName:=FName;
  dest.FVariableLabel:=FVariableLabel;
  dest.Felttype:=Felttype;
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

  dest.FCommentLegalRec:=NIL;      //Mangler implementering

  dest.FShowLegalPickList:=FShowLegalPickList;
  dest.FPickListNoSelect:=FPickListNoSelect;
  dest.FFieldComments:=FFieldComments;
  dest.FValueLabel:='';   //Mangler implementering;
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

// ********************** TeField END***************************



end.
