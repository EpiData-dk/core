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
     function IndexIntegrity(Const DataFile: TEpiDataFile;
       out FailedRecords: TBoundArray; StopOnFail: boolean = false): boolean;
  end;

implementation

uses
  contnrs, LCLIntf;

{ TEpiIntegrityChecker }

function TEpiIntegrityChecker.IndexIntegrity(const DataFile: TEpiDataFile; out
  FailedRecords: TBoundArray; StopOnFail: boolean): boolean;
var
  KeyFields: TEpiFields;
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
  KeyFields := DataFile.KeyFields;
  if KeyFields.Count = 0 then exit(true);

  HashMap := TFPDataHashTable.CreateWith(DataFile.Size, @RSHash);
  L := 0;

//  T1 := GetTickCount64;
  for i := 0 to DataFile.Size - 1 do
  begin
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

