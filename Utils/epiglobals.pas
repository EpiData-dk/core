unit epiglobals;

{$mode objfpc}{$H+}

interface

const

  // Remember to update: function IsReservedEpiFieldName(const Name: string): boolean;
  //  in epimiscutils.pas if adding a new reserved fieldname.
  // Index strings
  EpiIndexIntegrityFieldName = '_IndexFail';
  EpiIndexIntegrityValueLabelSetName = '_label_indexFail';
  // Double Entry strings
  EpiDoubleEntryFieldName = '_DoubleEntry';
  EpiDoubleEntryValueLabelSetName = '_label_DoubleEntry';

implementation

end.

