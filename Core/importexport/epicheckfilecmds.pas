unit epicheckfilecmds;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, epivaluelabels, epidatatypes, epicheckfiletypes;

type

  TChkCommands = class;

  { TChkCommand }

  TChkCommand = Class(TObject)
  private
  protected
    function    GetCommandType: TChkCmdType; virtual; abstract;
  public
    Constructor Create(); virtual;
    Destructor  Destroy(); override;
    procedure   Clone(var Dst: TChkCommand); virtual;
    property    CmdType: TChkCmdType read GetCommandType;
  end;

  { TChkIf }

  TChkIf = class(TChkCommand)
  private
    FIfCmds:    TChkCommands;
    FElseCmds:  TChkCommands;
    FExpr:      string;
    FShowExpr:  string;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure   Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    Property    Expr: string read FExpr write FExpr;
    Property    ShowExpr:  string read FShowExpr write FShowExpr;
    Property    IfCmds: TChkCommands read FIfCmds write FIfCmds;
    Property    ElseCmds: TChkCommands read FElseCmds write FElseCmds;
  end;

  { TChkHelp }

  TChkHelp = class(TChkCommand)
  private
    FText:    string;
    FKeys:      string;
    FMsgType:   byte;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure   Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    Property    Text: string read FText write FText;
    Property    Keys: string read FKeys write FKeys;
    Property    MsgType: byte read FMsgType write FMsgType;
  end;

  { TChkFieldReferer }

  TChkFieldReferer = class(TChkCommand)
  private
    FCmdType:    TChkCmdType;
    FVarNumber:  Integer;
    FVarName:    string;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create(aCmdType: TChkCmdType);
    destructor  Destroy; override;
    Property    VarNumber: Integer read FVarNumber write FVarNumber;
    Property    VarName: string read FVarName write FVarName;
  end;

  { TChkComLegal }

  TChkComLegal = class(TChkCommand)
  private
    FValueLabel:      TValueLabelSet;
    FValueLabelIsFieldRef: Boolean;
    FValueLabelUse:   string;
    FShowList:        Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   ValueLabel: TValueLabelSet read FValueLabel write FValueLabel;
    Property   ValueLabelIsFieldRef: Boolean read FValueLabelIsFieldRef write FValueLabelIsFieldRef;
    Property   ValueLabelUse: string read FValueLabelUse write FValueLabelUse;
    Property   ShowList: Boolean read FShowList write FShowList;
  end;

  { TChkTypeStr }

  TChkTypeStr = class(TChkCommand)
  private
    FVarNumber: Integer;
    FText:      string;
    FColor:     Integer;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   VarNumber: Integer read FVarNumber write FVarNumber;
    Property   Text: string read FText write FText;
    Property   Color: Integer read FColor write FColor;
  end;

  { TChkRelate }

  TChkRelate = class(TChkCommand)
  private
    FRelField:   string;
    FRelFileNo:  Integer;
    FRelFileStr: string;
    FOne2One:    Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    Property    RelField: string read FRelField write FRelField;
    Property    RelFileNo: Integer read FRelFileNo write FRelFileNo;
    Property    RelFileStr:string read FRelFileStr write FRelFileStr;
    Property    One2One: Boolean read FOne2One write FOne2One;
  end;

  { TChkLet }

  TChkLet = class(TChkCommand)
  private
    FVarName:      string;
    FVarNumber:    Integer;
    FVarIsField:   Boolean;
    FCodedWithLET: Boolean;
    FLetExpr:      string;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   VarName: string read FVarName write FVarName;
    Property   VarNumber: Integer read FVarNumber write FVarNumber;
    Property   VarIsField: Boolean read FVarIsField write FVarIsField;
    Property   CodedWithLET: Boolean read FCodedWithLET write FCodedWithLET;
    Property   LetExpr: string read FLetExpr write FLetExpr;
  end;

  { TChkComment }

  TChkComment = class(TChkCommand)
  private
    FComment: string;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property   Comment: string read FComment write FComment;
  end;

  { TChkDefine }

  TChkDefine = class(TChkCommand)
  private
    FFieldName:   string;
    FFieldType:   TFieldType;
    FLength:      Integer;
    FNumDecimals: Byte;
    FScope:       TFieldScope;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property   FieldName: string read FFieldName write FFieldName;
    property   FieldType: TFieldType read FFieldType write FFieldType;
    property   Length: Integer read FLength write FLength;
    property   NumDecimals: Byte read FNumDecimals write FNumDecimals;
    property   Scope: TFieldScope read FScope write FScope;
  end;

  { TChkWriteNote }

  TChkWriteNote = class(TChkCommand)
  private
    FNote:      string;
    FShowNotes: Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property   Note: string read FNote write FNote;
    property   ShowNotes: Boolean read FShowNotes write FShowNotes;
  end;

  { TChkClpBrd }

  TChkClpBrd = class(TChkCommand)
  private
    FText: string;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   Text: string read FText write FText;
  end;

  { TChkBackup }

  TChkBackup = class(TChkCommand)
  private
    FDestLib:  string;
    FZip:      Boolean;
    FEncrypt:  Boolean;
    FFilename: string;
    FPass:     string;
    FAddDate:  Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   DestLib: string read FDestLib write FDestLib;
    Property   Zip: Boolean read FZip write FZip;
    Property   Encrypt: Boolean read FEncrypt write FEncrypt;
    Property   Filename: string read FFilename write FFilename;
    Property   Password: string read FPass write FPass;
    Property   AddDate: Boolean read FAddDate write FAddDate;
  end;

  { TChkBeep }

  TChkBeep = class(TChkCommand)
  private
    FBeepType: TBeepTypes;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property   BeepType: TBeepTypes read FBeepType write FBeepType;
  end;

  { TChkLoadLib }

  TChkLoadLib = class(TChkCommand)
  private
    FLibName: string;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   LibName: string read FLibName write FLibName;
  end;

  { TChkExec }

  TChkExec = class(TChkCommand)
  private
    FCmdLine: string;
    FParams:  string;
    FHide:    Boolean;
    FWait:    Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   CmdLine: string read FCmdLine write FCmdLine;
    Property   Params: string read FParams write FParams;
    Property   Hide: Boolean read FHide write FHide;
    Property   Wait: Boolean read FWait write FWait;
  end;

  { TChkLeaveField }

  TChkLeaveField = class(TChkCommand)
  private
    FLeaveStyle:  TLeaveStyles;
    FIsLastField: Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   LeaveStyle: TLeaveStyles read FLeaveStyle write FLeaveStyle;
    Property   IsLastField: Boolean read FIsLastField write FIsLastField;
  end;

  { TChkColor }

  TChkColor = class(TChkCommand)
  private
    FColorCmd:    Byte;      //1=color question, 2=color data,  3=color background, 4=color fieldname
    FTxtColor:    Byte;
    FBgColor:     Byte;
    FIsEpiInfoNo: Boolean;
    FFieldNo:     Byte;
  protected
    function    GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   ColorCmd: Byte read FColorCmd write FColorCmd;
    Property   TxtColor: Byte read FTxtColor write FTxtColor;
    Property   BgColor: Byte read FBgColor write FBgColor;
    Property   IsEpiInfoNo: Boolean read FIsEpiInfoNo write FIsEpiInfoNo;
    Property   FieldNo: Byte read FFieldNo write FFieldNo;
  end;

  { TChkOther }

  TChkOther = class(TChkCommand)
  private
    FCmdType: TChkCmdType;
  protected
    function  GetCommandType(): TChkCmdType; override;
    procedure Clone(var Dst: TChkCommand); override;
  public
    constructor Create(aCmdType: TChkCmdType);
    destructor Destroy; override;
  end;

  { TChkCommands }

  TChkCommands = class(TObject)
  private
    FList:     TList;
    function   GetCount: integer;
    function   GetItem(Index: integer): TChkCommand;
  public
    constructor Create();
    destructor  Destroy(); override;
    procedure   AddCommand(aCmd: TChkCommand);
    procedure   Clone(var Dest: TChkCommands);
    property    Count: integer read GetCount;
    property    Items[index:integer]: TChkCommand read GetItem; default;
  end;

implementation

uses
  SysUtils;

 { TChkCommands }

constructor TChkCommands.Create();
begin
  inherited;
  FList := TList.Create;
end;

destructor TChkCommands.Destroy();
var
  TmpCmd: TChkCommand;
begin
  While FList.Count > 0 do
  BEGIN
    TmpCmd := TChkCommand(FList.Last);
    FreeAndNil(TmpCmd);
    FList.Delete(FList.Count - 1);
  END;
  FreeAndNil(FList);
  inherited;
end;

procedure TChkCommands.AddCommand(aCmd: TChkCommand);
begin
  FList.Add(aCmd);
end;

function TChkCommands.GetCount:integer;
begin
  result := FList.count;
end;

function TChkCommands.GetItem(index:integer): TChkCommand;
begin
  result := NIL;
  if (index >= 0) and (index < FList.Count) then
    result := TChkCommand(FList.Items[index])
end;

procedure TChkCommands.Clone(var Dest: TChkCommands);
var
  n: integer;
  TmpCmd: TChkCommand;
begin
  if (not assigned(dest)) then
    dest := TChkCommands.create;
  for n := 0 TO FList.Count - 1 do
  begin
    TmpCmd := nil;
    Items[N].Clone(TmpCmd);
    Dest.AddCommand(TmpCmd);
  end;
end;

{ TChkCommand }

constructor TChkCommand.Create();
begin

end;

destructor TChkCommand.Destroy();
begin
  inherited Destroy();
end;

procedure TChkCommand.Clone(var Dst: TChkCommand);
begin
  if not Assigned(Dst) then
    Dst := TChkCommand(ClassType.Create());
end;

{ TChkIf }

function TChkIf.GetCommandType(): TChkCmdType;
begin
  Result := cmdIF;
end;

procedure TChkIf.Clone(var Dst: TChkCommand);
begin
  Inherited Clone(Dst);
  IfCmds.Clone(TChkIf(Dst).FIfCmds);
  ElseCmds.Clone(TChkIf(Dst).FElseCmds);
  TChkIf(Dst).FExpr := FExpr;
  TChkIf(Dst).FShowExpr := FShowExpr;
end;

constructor TChkIf.Create;
begin
  inherited;
  FIfCmds := TChkCommands.Create;
  FElseCmds := TChkCommands.Create;
  FExpr := '';
  FShowExpr := '';
end;

destructor TChkIf.Destroy;
begin
  FreeAndNil(FIfCmds);
  FreeAndNil(FElseCmds);
  inherited;
end;

{ TChkHelp }

function TChkHelp.GetCommandType(): TChkCmdType;
begin
  Result := cmdHelp;
end;

procedure TChkHelp.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkHelp(Dst).FKeys := FKeys;
  TChkHelp(Dst).FMsgType := FMsgType;
  TChkHelp(Dst).FText := FText;
end;

constructor TChkHelp.Create;
begin
  inherited Create;
  FKeys := '';
  FMsgType := 3; // mtConfirmation;
  FText := '';
end;

destructor TChkHelp.Destroy;
begin
  inherited Destroy;
end;

{ TChkFieldReferer }

function TChkFieldReferer.GetCommandType(): TChkCmdType;
begin
  Result := FCmdType;
end;

procedure TChkFieldReferer.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkFieldReferer(Dst).FCmdType := FCmdType;
  TChkFieldReferer(Dst).FVarName := FVarName;
  TChkFieldReferer(Dst).FVarNumber := FVarNumber;
end;

constructor TChkFieldReferer.Create(aCmdType: TChkCmdType);
begin
  inherited Create;
  FCmdType := aCmdType;
  FVarName := '';
  FVarNumber := -1;
end;

destructor TChkFieldReferer.Destroy;
begin
  inherited Destroy;
end;

{ TChkComLegal }

function TChkComLegal.GetCommandType(): TChkCmdType;
begin
  Result := cmdComLegal;
end;

procedure TChkComLegal.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkComLegal(Dst).FShowList := FShowList;
  TChkComLegal(Dst).FValueLabel := FValueLabel;
  TChkComLegal(Dst).FValueLabelIsFieldRef := FValueLabelIsFieldRef;
  TChkComLegal(Dst).FValueLabelUse := FValueLabelUse;
end;

constructor TChkComLegal.Create;
begin
  inherited Create;
  FShowList := False;
  FValueLabel := nil;
  FValueLabelIsFieldRef := false;
  FValueLabelUse := '';
end;

destructor TChkComLegal.Destroy;
begin
  inherited Destroy;
end;

{ TChkTypeStr }

function TChkTypeStr.GetCommandType(): TChkCmdType;
begin
  Result := cmdTypeString;
end;

procedure TChkTypeStr.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkTypeStr(Dst).FVarNumber := FVarNumber;
  TChkTypeStr(Dst).FText := FText;
  TChkTypeStr(Dst).FColor := FColor;
end;

constructor TChkTypeStr.Create;
begin
  inherited Create;
  FVarNumber := 0;
  FText := '';
  FColor := 0;
end;

destructor TChkTypeStr.Destroy;
begin
  inherited Destroy;
end;

{ TChkRelate }

function TChkRelate.GetCommandType(): TChkCmdType;
begin
  Result := cmdRelate;
end;

procedure TChkRelate.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkRelate(Dst).FOne2One := FOne2One;
  TChkRelate(Dst).FRelField := FRelField;
  TChkRelate(Dst).FRelFileNo := FRelFileNo;
  TChkRelate(Dst).FRelFileStr := FRelFileStr;
end;

constructor TChkRelate.Create;
begin
  inherited Create;
  FOne2One := false;
  FRelField := '';
  FRelFileNo := 0;
  FRelFileStr := '';
end;

destructor TChkRelate.Destroy;
begin
  inherited Destroy;
end;

{ TChkLet }

function TChkLet.GetCommandType(): TChkCmdType;
begin
  Result := cmdLet;
end;

procedure TChkLet.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkLet(Dst).FVarNumber := FVarNumber;
  TChkLet(Dst).FVarName := FVarName;
  TChkLet(Dst).FCodedWithLET := FCodedWithLET;
  TChkLet(Dst).FLetExpr := FLetExpr;
  TChkLet(Dst).FVarIsField := FVarIsField;
end;

constructor TChkLet.Create;
begin
  inherited Create;
  FVarNumber := 0;
  FVarName := '';
  FCodedWithLET := false;
  FLetExpr := '';
  FVarIsField := false;
end;

destructor TChkLet.Destroy;
begin
  inherited Destroy;
end;

{ TChkComment }

function TChkComment.GetCommandType(): TChkCmdType;
begin
  Result := cmdComment;
end;

procedure TChkComment.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkComment(Dst).FComment := FComment;
end;

constructor TChkComment.Create;
begin
  inherited Create;
  FComment := '';
end;

destructor TChkComment.Destroy;
begin
  inherited Destroy;
end;

{ TChkDefine }

function TChkDefine.GetCommandType(): TChkCmdType;
begin
  Result := cmdDefine;
end;

procedure TChkDefine.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkDefine(Dst).FFieldName := FFieldName;
  TChkDefine(Dst).FFieldType := FFieldType;
  TChkDefine(Dst).FLength := FLength;
  TChkDefine(Dst).FNumDecimals := FNumDecimals;
  TChkDefine(Dst).FScope := FScope;
end;

constructor TChkDefine.Create;
begin
  inherited Create;
  FFieldName := '';
  FFieldType := ftString;
  FLength := 0;
  FNumDecimals := 0;
  FScope := scNone;
end;

destructor TChkDefine.Destroy;
begin
  inherited Destroy;
end;

{ TChkWriteNote }

function TChkWriteNote.GetCommandType(): TChkCmdType;
begin
  Result := cmdWriteNote;
end;

procedure TChkWriteNote.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkWriteNote(Dst).FNote := FNote;
  TChkWriteNote(Dst).FShowNotes := FShowNotes;
end;

constructor TChkWriteNote.Create;
begin
  inherited Create;
  FNote := '';
  FShowNotes := false;
end;

destructor TChkWriteNote.Destroy;
begin
  inherited Destroy;
end;

{ TChkClpBrd }

function TChkClpBrd.GetCommandType(): TChkCmdType;
begin
  Result := cmdCopyToClipboard;
end;

procedure TChkClpBrd.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkClpBrd(Dst).FText := FText;
end;

constructor TChkClpBrd.Create;
begin
  inherited Create;
  FText := '';
end;

destructor TChkClpBrd.Destroy;
begin
  inherited Destroy;
end;

{ TChkBackup }

function TChkBackup.GetCommandType(): TChkCmdType;
begin
  Result := cmdBackup;
end;

procedure TChkBackup.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkBackup(Dst).FDestLib :=FDestLib;
  TChkBackup(Dst).FZip := FZip;
  TChkBackup(Dst).FEncrypt := FEncrypt;
  TChkBackup(Dst).FFilename := FFilename;
  TChkBackup(Dst).FPass := FPass;
  TChkBackup(Dst).FAddDate := FAddDate;
end;

constructor TChkBackup.Create;
begin
  inherited Create;
  FDestLib := '';
  FZip := false;
  FEncrypt := false;
  FFilename := '';
  FPass := '';
  FAddDate := false;
end;

destructor TChkBackup.Destroy;
begin
  inherited Destroy;
end;

{ TChkBeep }

function TChkBeep.GetCommandType(): TChkCmdType;
begin
  Result := cmdBeep;
end;

procedure TChkBeep.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkBeep(Dst).FBeepType := FBeepType;
end;

constructor TChkBeep.Create;
begin
  inherited Create;
  FBeepType := btStandard;
end;

destructor TChkBeep.Destroy;
begin
  inherited Destroy;
end;

{ TChkLoadLib }

function TChkLoadLib.GetCommandType(): TChkCmdType;
begin
  Result := cmdLoad;
end;

procedure TChkLoadLib.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkLoadLib(Dst).FLibName := FLibName;
end;

constructor TChkLoadLib.Create;
begin
  inherited Create;
  FLibName := '';
end;

destructor TChkLoadLib.Destroy;
begin
  inherited Destroy;
end;

{ TChkExec }

function TChkExec.GetCommandType(): TChkCmdType;
begin
  Result := cmdExecute;
end;

procedure TChkExec.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkExec(Dst).FCmdLine := FCmdLine;
  TChkExec(Dst).FParams := FParams;
  TChkExec(Dst).FHide := FHide;
  TChkExec(Dst).FWait := FWait;
end;

constructor TChkExec.Create;
begin
  inherited Create;
  FCmdLine := '';
  FParams := '';
  FHide := false;
  FWait := false;
end;

destructor TChkExec.Destroy;
begin
  inherited Destroy;
end;

{ TChkLeaveField }

function TChkLeaveField.GetCommandType(): TChkCmdType;
begin
  Result := cmdLeaveField;
end;

procedure TChkLeaveField.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkLeaveField(Dst).FIsLastField := FIsLastField;
  TChkLeaveField(Dst).FLeaveStyle := FLeaveStyle;
end;

constructor TChkLeaveField.Create;
begin
  inherited Create;
  FIsLastField := false;
  FLeaveStyle := lsNone;
end;

destructor TChkLeaveField.Destroy;
begin
  inherited Destroy;
end;

{ TChkColor }

function TChkColor.GetCommandType(): TChkCmdType;
begin
  Result := cmdColor;
end;

procedure TChkColor.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkColor(Dst).FColorCmd := FColorCmd;
  TChkColor(Dst).FTxtColor := FTxtColor;
  TChkColor(Dst).FBgColor := FBgColor;
  TChkColor(Dst).FIsEpiInfoNo := FIsEpiInfoNo;
  TChkColor(Dst).FFieldNo := FFieldNo;
end;

constructor TChkColor.Create;
begin
  inherited Create;
  FColorCmd := 0;
  FTxtColor := 0;
  FBgColor := 0;
  FIsEpiInfoNo := false;
  FFieldNo := 0;
end;

destructor TChkColor.Destroy;
begin
  inherited Destroy;
end;


{ TChkOther }

function TChkOther.GetCommandType(): TChkCmdType;
begin
  Result := FCmdType;
end;

procedure TChkOther.Clone(var Dst: TChkCommand);
begin
  inherited Clone(Dst);
  TChkOther(Dst).FCmdType := FCmdType;
end;

constructor TChkOther.Create(aCmdType: TChkCmdType);
begin
  FCmdType := aCmdType;
end;

destructor TChkOther.Destroy;
begin
  inherited Destroy;
end;

end.
