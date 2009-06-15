unit UCheckFileCmds;

interface

uses
  // TODO -o Torsten : Get rid of Dialogs and Graphic. It compiles most of the LCL into the binary.
  Classes, UValueLabels, UDataFileTypes, UCheckFileTypes, Dialogs, Graphics;

type

  TChkCommands = class;

  { TChkCommand }

  TChkCommand = Class(TObject)
  protected
    function    GetCommandType(): TChkCmdType; virtual; abstract;
  public
    Constructor Create(); virtual;
    Destructor  Destroy(); override;
    property    CmdType: TChkCmdType read GetCommandType;
  end;

  { TChkIf }

  TChkIf = class(TChkCommand)
  private
    FIfCmds:    TChkCommands;
    FElseCmds:  TChkCommands;
    FExpr:      UTF8String;
    FShowExpr:  UTF8String;
  protected
    function    GetCommandType(): TChkCmdType; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    Property    Expr: UTF8String read FExpr write FExpr;
    Property    ShowExpr:  UTF8String read FShowExpr write FShowExpr;
    Property    IfCmds: TChkCommands read FIfCmds write FIfCmds;
    Property    ElseCmds: TChkCommands read FElseCmds write FElseCmds;
  end;

  { TChkHelp }

  TChkHelp = class(TChkCommand)
  private
    FString:    UTF8String;
    FKeys:      UTF8String;
    FMsgType:   TMsgDlgType;
  protected
    function    GetCommandType(): TChkCmdType; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    Property    aString: UTF8String read FString write FString;
    Property    Keys: UTF8String read FKeys write FKeys;
    Property    MsgType: TMsgDlgType read FMsgType write FMsgType;
  end;

  { TChkFieldReferer }

  TChkFieldReferer = class(TChkCommand)
  private
    FCmdType:    TChkCmdType;
    FVarNumber:  Integer;
    FVarName:    UTF8String;
  protected
    function    GetCommandType(): TChkCmdType; override;
  public
    constructor Create(aCmdType: TChkCmdType);
    destructor  Destroy; override;
    Property    VarNumber: Integer read FVarNumber write FVarNumber;
    Property    VarName: UTF8String read FVarName write FVarName;
  end;

  { TChkComLegal }

  TChkComLegal = class(TChkCommand)
  private
    FVarNumber:       Integer;
    FValueLabelName:  UTF8String;
    FValueLabel:      TValueLabelSet;
    FValueLabelIsFieldRef: Boolean;
    FValueLabelUse:   UTF8String;
    FShowList:        Boolean;
  protected
    function    GetCommandType(): TChkCmdType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   VarNumber: Integer read FVarNumber write FVarNumber;
    Property   ValueLabelName: UTF8String read FValueLabelName write FValueLabelName;
    Property   ValueLabel: TValueLabelSet read FValueLabel write FValueLabel;
    Property   ValueLabelIsFieldRef: Boolean read FValueLabelIsFieldRef write FValueLabelIsFieldRef;
    Property   ValueLabelUse: UTF8String read FValueLabelUse write FValueLabelUse;
    Property   ShowList: Boolean read FShowList write FShowList;
  end;

  { TChkTypeStr }

  TChkTypeStr = class(TChkCommand)
  private
    FVarNumber: Integer;
    FText:      UTF8String;
    FColor:     TColor;
  protected
    function    GetCommandType(): TChkCmdType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property   VarNumber: Integer read FVarNumber write FVarNumber;
    Property   Text: UTF8String read FText write FText;
    Property   Color: TColor read FColor write FColor;
  end;
  
  PCmd = ^TCmd;
  TCmd = record
    Next: PCmd;
    case Command: TChkCmdType of
      cmdIF:
         (IfExpr:          string;
          IfShowExpr:      string;
          IfCmds:          TChkCommands;
          ElseCmds:        TChkCommands);
      cmdHelp:
         (HelpString:      string;
          HelpType:        TMsgDlgType;
          HelpKeys:        string);
      cmdHide, cmdUnHide,
      cmdClear, cmdGoto:
         (HideVarNumber:   Integer;
          HideVarName:     string);
      cmdComLegal:
         (clVarNumber:     Integer;
          ValueLabelName:  string;
          ValueLabel:      TValueLabelSet;
          ValueLabelIsFieldRef: Boolean;
          ValueLabelUse:   string;
          ShowList:        Boolean);
      cmdTypeString:
         (tsVarNumber:     Integer;
          TypeText:        string;
          Typecolor:       TColor);
      cmdRelate:
         (RelField:        string;
          RelFileNo:       Integer;
          RelFileStr:      string;
          One2One:         Boolean);
      cmdLet:
         (VarName:         string;
          VarNumber:       Integer;
          VarIsField:      Boolean;
          CodedWithLET:    Boolean;
          LetExpr:         string);
      cmdComment:
         (Comment:         string);
      cmdDefine:
         (FName:           string;
          FeltType:        TFieldType;
          FLength:         Integer;
          FNumDecimals:    Byte;
          FScope:          TFieldScope);
      cmdWriteNote:
         (FNote:           string;
          ShowNotes:       Boolean);
      cmdCopyToClipboard:
         (CopyStr:         string);
      cmdBackup:
         (DestLib:         string;
          zipit:           Boolean;
          encryptit:       Boolean;
          filename:        string;
          pw:              string;
          dateit:          Boolean);
      cmdBeep:
         (BeepType:        TBeepTypes);
      cmdLoad:
         (DLLName:         string);
      cmdExecute:
         (ExecCmdLine:     string;
          ExecParams:      string;
          ExecHide:        Boolean;
          ExecWait:        Boolean);
      cmdLeaveField:
         (cLeaveStyle:     TLeaveStyles;
          IsLastField:     Boolean);
      cmdColor:
         (ColorCmd:        Byte;      //1=color question, 2=color data,  3=color background, 4=color fieldname
          TxtColor:        TColor;
          BgColor:         TColor;
          IsEpiInfoNo:     Boolean;
          CFieldno:        Byte);
    end;

  TChkCommands = class(TObject)
  private
    FList: TList;
    function GetCount:integer;
    function GetItem(index:integer):PCmd;
    Procedure DisposeCommandList(aList: TList);
  public
    constructor Create();
    destructor  Destroy(); override;
    procedure   AddCommand(aCmd: PCmd);
    procedure   Clone(var Dest: TChkCommands);
    function    NewCmd: PCmd;
    property    Count: integer read GetCount;
    property    items[index:integer]:PCmd read GetItem; default;
    property    List:TList read FList;
  end;

implementation

uses
  SysUtils;

 { TChkCommands }

constructor TChkCommands.Create();
begin
  inherited;
  FList:=TList.Create;
end;

destructor TChkCommands.Destroy();
begin
  DisposeCommandList(FList);
  inherited;
end;

Procedure TChkCommands.DisposeCommandList(aList: TList);
VAR
  n:Integer;
  tmpCmdRec: PCmd;
BEGIN
  FOR n:=0 TO AList.Count-1 DO
    BEGIN
      TmpCmdRec := PCmd(AList.Items[n]);
      CASE tmpCmdRec^.Command OF
        cmdIF:
          BEGIN
            IF tmpCmdRec^.IfCmds <> NIL THEN
              DisposeCommandList(tmpCmdRec^.IfCmds.List);
            IF tmpCmdRec^.ElseCmds <> NIL THEN
               DisposeCommandList(tmpCmdRec^.ElseCmds.List);
          END;
      END;  //case
      Dispose(PCmd(Alist.Items[n]));
    END;  //for
  FreeAndNil(Alist);
END;

procedure TChkCommands.AddCommand(aCmd: PCmd);
begin
  FList.Add(aCmd);
end;

function TChkCommands.GetCount:integer;
begin
  result := FList.count;
end;

function TChkCommands.GetItem(index:integer):PCmd;
begin
  result := NIL;
  if (index >= 0) and (index < FList.Count) then
    result := PCmd(FList.Items[index])
end;

procedure TChkCommands.Clone(var Dest: TChkCommands);
var
  n: integer;
  aCmd, new: PCmd;
begin
  if (not assigned(dest)) then
    dest := TChkCommands.create;
  for n := 0 TO FList.Count-1 do
  begin
    aCmd := PCmd(FList.Items[n]);
    new := NewCmd;
    new^ := aCmd^;
    new^.IfCmds :=NIL;
    new^.ElseCmds :=NIL;
    if aCmd^.IfCmds <> NIL then
    begin
      new^.IfCmds := TChkCommands.create;
      aCmd^.IfCmds.Clone(new^.IfCmds);
    end;
    if aCmd^.ElseCmds <> NIL then
    begin
      new^.ElseCmds := TChkCommands.create;
      aCmd^.ElseCmds.Clone(new^.ElseCmds);
    end;
    dest.AddCommand(new);
  end;
end;

function TChkCommands.NewCmd: PCmd;
begin
  new(result);
  result^.IfCmds := NIL;
  result^.ElseCmds := NIL;
end;

{ TChkCommand }

constructor TChkCommand.Create();
begin

end;

destructor TChkCommand.Destroy();
begin
  inherited Destroy();
end;

{ TChkIf }

function TChkIf.GetCommandType(): TChkCmdType;
begin
  Result := cmdIF;
end;

constructor TChkIf.Create;
begin
  inherited;

end;

destructor TChkIf.Destroy;
begin

  inherited;
end;

{ TChkHelp }

function TChkHelp.GetCommandType(): TChkCmdType;
begin
  Result := cmdHelp;
end;

constructor TChkHelp.Create;
begin
  inherited Create;
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

constructor TChkFieldReferer.Create(aCmdType: TChkCmdType);
begin
  FCmdType := aCmdType;
  inherited Create;
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

constructor TChkComLegal.Create;
begin
  inherited Create;
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

constructor TChkTypeStr.Create;
begin
  inherited Create;
end;

destructor TChkTypeStr.Destroy;
begin
  inherited Destroy;
end;

end.
