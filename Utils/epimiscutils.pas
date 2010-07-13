unit epimiscutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

const
  EpiTypeCastArray: array[TEpiFieldType, TEpiFieldType] of boolean =
      // Cast To Types
//                ftBoolean, ftInteger, ftAutoInc, ftFloat, ftDMYDate, ftMDYDate, ftYMDDate, ftDMYToday, ftMDYToday, ftYMDToday, ftTime, ftTimeNow, ftString, ftUpperString
{C} {ftBoolean} ((true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{a} {ftInteger}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{s} {ftAutoInc}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{t} {ftFloat}    (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftDMYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{F} {ftMDYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{r} {ftYMDDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{o} {ftDMYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{m} {ftMDYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
    {ftYMDToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
    {ftTime}     (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftTimeNow}  (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftString}   (true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,     true),
  {ftUpperString}(true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,     true));


  // File dialog filter functions.
  function GetEpiDialogFilter(ShowXML, ShowREC, ShowText, ShowODS, ShowXLS,
    ShowDTA, ShowDBF, ShowQES, ShowCollection, ShowAll: boolean): string;

implementation

type
  TEpiDialogFilterPair = record
    FilterName: string;
    FilterExt:  string;
  end;

const
  EpiDialogFilterCollection: TEpiDialogFilterPair = (
    FilterName: 'Supported files';
    FilterExt:  '';
  );

  EpiDialogFilterXML: TEpiDialogFilterPair = (
    FilterName: 'EpiData XML Data file (*.epx)';
    FilterExt:  '*.epx';
  );

  EpiDialogFilterREC: TEpiDialogFilterPair = (
    FilterName: 'EpiData data file (*.rec)';
    FilterExt:  '*.rec';
  );

  EpiDialogFilterText: TEpiDialogFilterPair = (
    FilterName: 'Text file (*.txt,*.csv)';
    FilterExt:  '*.txt;*.csv';
  );

  EpiDialogFilterODS: TEpiDialogFilterPair = (
    FilterName: 'Open Document Spreadsheet (*.ods)';
    FilterExt:  '*.ods';
  );

  EpiDialogFilterXLS: TEpiDialogFilterPair = (
    FilterName: 'Excel Spreadsheet (*.xls)';
    FilterExt:  '*.xls';
  );

  EpiDialogFilterDTA: TEpiDialogFilterPair = (
    FilterName: 'Stata file (*.dta)';
    FilterExt:  '*.dta';
  );

  EpiDialogFilterDBF: TEpiDialogFilterPair = (
    FilterName: 'dBase file (*.dbf)';
    FilterExt:  '*.dbf';
  );

  EpiDialogFilterQES: TEpiDialogFilterPair = (
    FilterName: 'QES file (*.qes)';
    FilterExt:  '*.qes';
  );

  EpiDialogFilterAll: TEpiDialogFilterPair = (
    FilterName: 'Show All (*.*)';
    FilterExt:  '*.*';
  );


function GetEpiDialogFilter(ShowXML, ShowREC, ShowText, ShowODS, ShowXLS,
  ShowDTA, ShowDBF, ShowQES, ShowCollection, ShowAll: boolean): string;
var
  CollectedExt: string;

  function AddFilter(Filter: TEpiDialogFilterPair): string;
  begin
    result := Filter.FilterName + '|' + Filter.FilterExt + '|';
    CollectedExt += Filter.FilterExt + ';';
  end;

begin
  Result := '';
  CollectedExt := '';
  if ShowXML then
    Result += AddFilter(EpiDialogFilterXML);
  if ShowREC then
    Result += AddFilter(EpiDialogFilterREC);
  if ShowText then
    Result += AddFilter(EpiDialogFilterText);
  if ShowODS then
    Result += AddFilter(EpiDialogFilterODS);
  if ShowXLS then
    Result += AddFilter(EpiDialogFilterXLS);
  if ShowDTA then
    Result += AddFilter(EpiDialogFilterDTA);
  if ShowDBF then
    Result += AddFilter(EpiDialogFilterDBF);
  if ShowQES then
    Result += AddFilter(EpiDialogFilterQES);

  if ShowCollection then
    Result := EpiDialogFilterCollection.FilterName + '|' +
              CollectedExt + '|' + Result;

  if ShowAll then
    Result += AddFilter(EpiDialogFilterAll);
end;

end.

