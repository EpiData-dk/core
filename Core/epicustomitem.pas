unit epicustomitem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, contnrs, epicustombase;

type

  { TEpiCustomItem }

  TEpiCustomItem = class(TEpiCustomBase)
  protected
    FName: string;
    function    GetName: string; virtual;
    procedure   SetName(const AValue: string); virtual;
    function    SaveAttributesToXml: string; override;
    function    DoValidateRename(Const NewName: string): boolean; virtual;
    function    WriteNameToXml: boolean; virtual;
  protected
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    function    ValidateRename(Const NewName: string; RenameOnSuccess: boolean): boolean; virtual;
    property    Name: string read GetName write SetName;
  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;

  { CustomData }
  // CustomData is a custom property that can be used freely to store some data
  // along with the object. User added content is NEVER used by the internals of
  // Core.
  // In addition data will not be copied/assigned/freed etc., hence it is
  // entirely up to the user to keep track of it's use throught a program.
  private
    FCustomData: TFPObjectHashTable;
  public
    procedure AddCustomData(const Key: string; Const Obj: TObject);
    function  FindCustomData(const Key: string): TObject;
    function  RemoveCustomData(Const Key: string): TObject;
  end;
  TEpiCustomItemClass = class of TEpiCustomItem;

implementation

uses
  epistringutils;

{ TEpiCustomItem }

function TEpiCustomItem.GetName: string;
begin
  result := FName;
end;

procedure TEpiCustomItem.SetName(const AValue: string);
var
  Val: String;
begin
  if FName = AValue then exit;

  // Validate identifier
  if not DoValidateRename(AValue) then Exit;

  Val := FName;
  FName := AValue;
  DoChange(eegCustomBase, Word(ecceName), @Val);
end;

function TEpiCustomItem.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml;
  if WriteNameToXml then
    // We use name in program but present it as "id=...." in the XML.
    Result += SaveAttr(rsId, Name);
end;

function TEpiCustomItem.DoValidateRename(const NewName: string): boolean;
begin
  result := ValidateIdentifierUTF8(NewName);
//TODO  if (Owner is TEpiCustomList) then
//    result := result and (TEpiCustomList(Owner).ValidateRename(NewName, false));
end;

function TEpiCustomItem.WriteNameToXml: boolean;
begin
  result := true;
end;

function TEpiCustomItem.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if WriteNameToXml then
    SaveDomAttr(Result, rsId, Name);
end;

destructor TEpiCustomItem.Destroy;
begin
  FName := '';
  if Assigned(FCustomData) then
    FreeAndNil(FCustomData);
  inherited Destroy;
end;

procedure TEpiCustomItem.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, rsId, false) then
    FName := LoadAttrString(Root, rsId)
  else if WriteNameToXml then
    // This class was supposed to write an ID -> hence it also expects one! Error!
    RaiseErrorAttr(Root, rsId);
end;

procedure TEpiCustomItem.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  Name := TEpiCustomItem(AEpiCustomBase).Name;
  EndUpdate;
end;

function TEpiCustomItem.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  if NewName = Name then exit(true);
  result := DoValidateRename(NewName);
end;

function TEpiCustomItem.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiCustomItem(Result).FName := FName;
end;

procedure TEpiCustomItem.AddCustomData(const Key: string; const Obj: TObject);
begin
  if not Assigned(FCustomData) then
    FCustomData := TFPObjectHashTable.Create(false);

  try
    FCustomData.Items[Key] := Obj;
  except
    raise Exception.Create('TEpiCustomItem: Duplicate CustomData - key=' + Key);
  end;
end;

function TEpiCustomItem.FindCustomData(const Key: string): TObject;
begin
  result := nil;
  if Assigned(FCustomData) then
    result := FCustomData.Items[Key];
end;

function TEpiCustomItem.RemoveCustomData(const Key: string): TObject;
begin
  Result := FindCustomData(Key);
  if not Assigned(Result) then exit;

  FCustomData.Delete(Key);

  if FCustomData.Count = 0 then
    FreeAndNil(FCustomData);
end;

end.

