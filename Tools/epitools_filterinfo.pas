unit epitools_filterinfo;

{$mode objfpc}{$H+}

interface

{
  This unit is a meant as a tool where eg. a TEpiDataFile is sent, and then
  all existing fields are applied a list of other fields which could be
  filtering information the given field.

  This filter information is NOT dynamic, hence the tool needs to be (re-)run
  before each use.
  AND the content in the custom data, MUST be freed by the "user" of the filter.
}

uses
  Classes, SysUtils, epidatafiles;


type

  { TEpiToolFilterInfo }

  TEpiToolFilterInfo = class
  private
    FField: TEpiField;
    FValue: TEpiJump;
  public
    property Field: TEpiField read FField write FField;
    property Filter: TEpiJump read FValue write FValue;
  end;


const
  EPITOOL_FILTER_CUSTDATA = 'EPITOOL_FILTER_CUSTDATA';

procedure EpiTool_UpdateFilterInformation(Const DF: TEpiDataFile);
procedure EpiTool_RemoveFilterInformation(Const DF: TEpiDataFile);

implementation

uses
  epidatafilestypes;

const
  EPITOOL_FILTER_INTERNAL_DATA = 'EPITOOL_FILTER_INTERNAL_DATA';

procedure ApplyListToField(Const F: TEpiField; Const List: TList);
var
  CData: TList;
begin
  if List.Count = 0 then exit;

  CData := TList(F.FindCustomData(EPITOOL_FILTER_CUSTDATA));

  if not Assigned(CData) then
  begin
    CData := TList.Create;
    F.AddCustomData(EPITOOL_FILTER_CUSTDATA, CData);
  end;

  CData.AddList(List);
end;

function NewFilterInfoToList(Const F: TEpiField; Const Jump: TEpiJump;
  Const List: TList): TEpiToolFilterInfo;
begin
  Result := TEpiToolFilterInfo.Create;
  Result.Field := F;
  Result.Filter := Jump;
  List.Add(Result);
end;

procedure AddFilterInfoToInternalData(Const F: TEpiField;
  Const FI: TEpiToolFilterInfo);
var
  CData: TList;
begin
  CData := TList(F.FindCustomData(EPITOOL_FILTER_INTERNAL_DATA));
  if not Assigned(CData) then
  begin
    CData := TList.Create;
    F.AddCustomData(EPITOOL_FILTER_INTERNAL_DATA, CData);
  end;
  CData.Add(FI);
end;

procedure EpiTool_UpdateFilterInformation(const DF: TEpiDataFile);
var
  CurrentFilter: TList;
  Fields: TEpiFields;
  F: TEpiField;
  i: Integer;
  j: Integer;
  Jmp: TEpiJump;
  Idx: Integer;
  FI: TEpiToolFilterInfo;
  L: TList;

begin
  Fields := DF.Fields;
  CurrentFilter := TList.Create;


  for i := 0 to Fields.Count - 1 do
  begin
    F := Fields[i];

    // Is this Field the destination for a jump, hence should we remove
    // filter information from the current list of active filters.
    // Otherwise F will falsely display Fields in L as being a filter of F.
    L := TList(F.RemoveCustomData(EPITOOL_FILTER_INTERNAL_DATA));
    if Assigned(L) then
      for j := 0 to L.Count - 1 do
        CurrentFilter.Remove(L.Items[j]);

    // Current active list of filters given flow.
    ApplyListToField(F, CurrentFilter);

    // Now check the current field if we should add additional filter
    // information to the current list of filter.
    // This is done if the field has any jumps assigned to it.
    if not Assigned(F.Jumps) then continue;

    for j := 0 to F.Jumps.Count - 1 do
    begin
      Jmp := F.Jumps[j];

      case Jmp.JumpType of
        jtSaveRecord:
          NewFilterInfoToList(F, Jmp, CurrentFilter);
        jtExitSection:
          begin
            FI := NewFilterInfoToList(F, Jmp, CurrentFilter);

            if F.Section <> Df.MainSection then
            begin
              Idx := Fields.IndexOf(F) + 1;
              while (Idx < Fields.Count) and
                    (Fields[Idx].Section = F.Section) do
                Inc(Idx);

              if (Idx < Fields.Count) then
                AddFilterInfoToInternalData(Fields[Idx], FI);
            end;
          end;
        jtSkipNextField:
          begin
            FI := NewFilterInfoToList(F, Jmp, CurrentFilter);

            Idx := Fields.IndexOf(F);
            Inc(Idx, 2);
            while (Fields[Idx].EntryMode = emNoEnter) do
              Inc(Idx);

            AddFilterInfoToInternalData(Fields[Idx], FI);
          end;
        jtToField:
          begin
            // We do not support jumps going backwards in flow.
            if Fields.IndexOf(Jmp.JumpToField) < Fields.IndexOf(F) then continue;

            FI := NewFilterInfoToList(F, Jmp, CurrentFilter);
            AddFilterInfoToInternalData(Jmp.JumpToField, FI);
          end;
      end;
    end;
  end;
  CurrentFilter.Free;
end;

procedure EpiTool_RemoveFilterInformation(const DF: TEpiDataFile);
var
  F: TEpiField;
  L: TList;
  FI: TEpiToolFilterInfo;
  i: Integer;
  FIList: TList;
begin
  FIList := TList.Create;

  for i := 0 to DF.Fields.Count - 1 do
  begin
    F := DF.Field[i];

    L := TList(F.RemoveCustomData(EPITOOL_FILTER_CUSTDATA));
    if Assigned(L) then
      FIList.Assign(L, laOr);

    L.Free;
  end;
  For i := 0 to FIList.Count - 1 do
    TObject(FIList[i]).Free;
  FIList.Clear;
  FIList.Free;
end;

end.

