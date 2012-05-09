unit epiintegritycheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Epi...
  epicustombase, epidatafiles;

type

  { TEpiIntegrityChecker }

  TEpiIntegrityChecker = class
   private
     //
   public
     // Checks the datafile for index integrity.
     //   out FailedRecords: Array of int returned with record no. to record where more than one exists.
     //   StopOnFail: if TRUE then integrity check stops on first found doublicate record.
     //   KeyFields: if assigned, this list is used as index fields instead of Datafiles key fields.
     //   result: true if NO doublicate records found, otherwise false.
     function IndexIntegrity(const DataFile: TEpiDataFile; out
       FailedRecords: TBoundArray; out FailValues: TBoundArray;
       StopOnFail: boolean = false; KeyFields: TEpiFields = nil): boolean;
  end;

implementation

uses
  contnrs, LCLIntf, fgl;

type
  TIntegrityList = specialize TFPGMap<Integer, Integer>;  // Maps RecNo -> FailValue

{ TEpiIntegrityChecker }

function CompareInt(Const Item1, Item2: Integer): Integer;
begin
  result := Item1 - Item2;
end;

function TEpiIntegrityChecker.IndexIntegrity(const DataFile: TEpiDataFile; out
  FailedRecords: TBoundArray; out FailValues: TBoundArray; StopOnFail: boolean;
  KeyFields: TEpiFields): boolean;
var
  S: String;
  j: Integer;
  i: Integer;
  HashMap: TFPDataHashTable;
  Failed: Boolean;
  CollisionRecList: TIntegrityList;

  procedure AddFailedRecord(RecNo: Integer; CollisionRecNo: Integer = -1);
  begin
    if CollisionRecNo = -1 then
      CollisionRecList.Add(RecNo, 2)  // Failed because of missing value.
    else begin
      CollisionRecList.Add(RecNo, 1); // Failed dues to dublicate.
      CollisionRecList.Add(CollisionRecNo, 1);  // Also add collision record - if already present previous entry is ignored.
    end;
  end;

begin
  {
    Finding index errors - IDEA:
    ------------------------------------

    for each record:
      1: Hash (K1, ..., Kn) -> A
      2: CollisionDetect ( A )    // Collides with B
      3:  No  -> Insert (A, #recno); continue on 1...
          Yes -> LookUp (#recno(B))
      4: if (K1,A = K1,B) and ... (Kn,A = Kn,B) then
           Fail!
         else
           New Hash Alg.
  }
  if not Assigned(KeyFields) then
    KeyFields := DataFile.KeyFields;

  if KeyFields.Count = 0 then exit(true);

  CollisionRecList := TIntegrityList.Create ;
  CollisionRecList.OnKeyCompare := @CompareInt;
  CollisionRecList.Sorted := true;
  CollisionRecList.Duplicates := dupIgnore;
  HashMap := TFPDataHashTable.CreateWith(DataFile.Size, @RSHash);

  for i := 0 to DataFile.Size - 1 do
  begin
    {$IFDEF EPI_INTEGRITY_DEBUG}
    // Since this is (most likely) run through debugger during
    // development AND raising exceptions wiht debugger attached
    // is EXTREMELY slow - we skip if more than 100 dublicates are
    // found.
    // Speed is not a problem when compiling for release and with
    // no debugger.
    if (CollisionRecList.Count >= 100) and (DataFile.Size > 1000) then
      break;
    {$ENDIF}
    Failed := false;

    // Concat K1, ..., Kn
    S := '';
    for j := 0 to KeyFields.Count - 1 do
      if KeyFields[j].IsMissing[i] then
      begin
        AddFailedRecord(i);
        Failed := true;
      end else
        S += KeyFields[j].AsString[i];

    if Failed and StopOnFail then break;
    if Failed then continue;

    // 1: Hash (K1...) -> A
    try
      HashMap.Add(S, Pointer(i));
    except
      // 2: Collision Detect
      on E: EDuplicate do
        begin
          // 3 + 4 : Since .Add(..) already checks key!
          AddFailedRecord(i, PtrUInt(THTDataNode(HashMap.Find(S)).Data));
          if StopOnFail then break;
        end;
    end;
    // 3: No -> ... (already added in HashMap.Add).
  end;

  SetLength(FailedRecords, CollisionRecList.Count);
  SetLength(FailValues, CollisionRecList.Count);
  for i := 0 to CollisionRecList.Count - 1 do
  begin
    FailedRecords[i] := CollisionRecList.Keys[i];
    FailValues[i]    := CollisionRecList.Data[i];
  end;

  Result := (CollisionRecList.Count = 0);
  CollisionRecList.Free;
  HashMap.Free;
end;

end.

