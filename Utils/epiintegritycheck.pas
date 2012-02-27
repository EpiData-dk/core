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
     function IndexIntegrity(Const DataFile: TEpiDataFile;
       out FailedRecords: TBoundArray;
       StopOnFail: boolean = false;
       KeyFields: TEpiFields = nil): boolean;
  end;

implementation

uses
  contnrs, LCLIntf;

{ TEpiIntegrityChecker }

function TEpiIntegrityChecker.IndexIntegrity(const DataFile: TEpiDataFile; out
  FailedRecords: TBoundArray; StopOnFail: boolean; KeyFields: TEpiFields
  ): boolean;
var
  S: String;
  j: Integer;
  i: Integer;
  HashMap: TFPDataHashTable;
  L: Integer;
  T1: QWord;
  T2: QWord;
  T3: Integer;
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

  HashMap := TFPDataHashTable.CreateWith(DataFile.Size, @RSHash);
  L := 0;

//  T1 := GetTickCount64;
  for i := 0 to DataFile.Size - 1 do
  begin
    {$IFDEF EPI_INTEGRITY_DEBUG}
    // Since this is (most likely) run through debugger during
    // development AND raising exceptions wiht debugger attacher
    // is EXTREMELY slow - we skip if more than 100 dublicates are
    // found.
    // Speed is not a problem when compiling for release and with
    // no debugger.
    if (L >= 100) and (DataFile.Size > 1000) then
      break;
    {$ENDIF}

    // Concat K1, ..., Kn
    S := '';
    for j := 0 to KeyFields.Count - 1 do
      S += KeyFields[j].AsString[i];

    // 1: Hash (K1...) -> A
    try
      HashMap.Add(S, Pointer(i));
    except
      // 2: Collision Detect
      on E: EDuplicate do
        begin
          // 3 + 4 : Since .Add(..) already checks key!
          Inc(L);
          SetLength(FailedRecords, L);
          FailedRecords[L-1] := i;

          if StopOnFail then break;
        end;
    end;
    // 3: No -> ... (already added in HashMap.Add).
  end;
{  T2 := GetTickCount64;
  T3 := t2 - t1;
  WriteLn('Tick Count: ', T3);
  WriteLn('L: ', L);      }

  Result := (L = 0);
end;

end.

