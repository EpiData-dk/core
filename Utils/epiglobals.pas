unit epiglobals;

{$mode objfpc}{$H+}

interface

uses
  epistringutils;

const
  IntegerChars:    TCharSet=['0'..'9','-','+'];
  FloatChars:      TCharSet=['0'..'9', '.', ',', '-', '+'];
  DateChars:       TCharSet=['0'..'9','/', '-', '.'];
  TimeChars:       TCharSet=['0'..'9',':','.'];
  BooleanChars:    TCharSet=['y','Y','n','N','1','0'];

  BooleanYesChars: TCharSet=['Y','y','1'];
  BooleanNoChars:  TCharSet=['N','n','0'];

const

  // Remember to update: function IsReservedEpiFieldName(const Name: string): boolean;
  //  in epimiscutils.pas if adding a new reserved fieldname.
  // Index strings
  EpiIndexIntegrityFieldName = '_IndexFail';
  EpiIndexIntegrityValueLabelSetName = '_label_indexFail';

  // Double Entry strings
  EpiDoubleEntryFieldName = '_DoubleEntry';
  EpiDoubleEntryValueLabelSetName = '_label_DoubleEntry';

  // Special Top-level Administrators Group Name
  EpiAdminGroupName = 'admins_group';

  // Failed Login Constants
  EpiAdminLoginAttemps = 3;
  EpiAdminLoginInterval = 5 * 60;   // 5 minuts = 300 seconds.

implementation

end.

