const
  // Common constants in the XML tree.
  // - Generic
  rsType = 'Type';
  rsName = 'Name';
  // - CustomItem
  rsId   = 'id';
  // - CustomControlItem
  rsTop  = 'Top';
  rsLeft = 'Left';

  // XML Tree structure.
  rsEpiData         = 'EpiData';
   // Settings
   rsSettings        = 'Settings';
    rsVersion         = 'Version';
    rsScrambled       = 'Scrambled';
    rsDateSep         = 'DateSeparator';
    rsTimeSep         = 'TimeSeparator';
    rsDecSep          = 'DecimalSeparator';
    rsMissingStr      = 'MissingString';

   rsProjectSettings = 'ProjectSettings';
    rsTimedBackupInterval='TimedBackupInterval';
    rsBackupOnShutdown= 'BackupOnShutdown';
    rsShowFieldNames  = 'ShowFieldNames';
    rsShowFieldBorders= 'ShowFieldBorders';

   rsAdmin           = 'Admin';
    // User
    rsUsers           = 'Users';
     rsUser            = 'User';
      rsLogin           = 'Login';
      rsPassword        = 'Password';
      rsMasterPassword  = 'MasterPassword';
      rsLastLogin       = 'LastLogin';
      rsExpireDate      = 'ExpireDate';
      rsGroupId         = 'GroupId';

      // Group
    rsGroups          = 'Groups';
     rsGroup           = 'Group';
      rsRights          = 'Rights';

   rsStudy           = 'Study';
    rsProtocol        = 'Protocol';
    rsAbstract        = 'Abstract';
    rsAuthor          = 'Author';
    rsOwner           = 'Owner';
    rsCreated         = 'Created';
    rsCreator         = 'Creator';
    rsDescription     = 'Description';
    rsLanguage        = 'Language';
    rsModified        = 'Modified';
    rsProvenance      = 'Provenance';
    rsReferences      = 'References';
    rsRightsholder    = 'Rightsholder';
    rsSource          = 'Source';
    rsSpatial         = 'Spatial';
    rsSubject         = 'Subject';
    rsTemporal        = 'Temporal';
    rsTitle           = 'Title';

   rsValueLabelSets  = 'ValueLabelSets';
    rsValueLabelSet   = 'ValueLabelSet';
     rsInternal        = 'Internal';
      rsValueLabel      = 'ValueLabel';
       rsLabel           = 'Label';
     rsExternal        = 'External';
      rsFile            = 'File';
      rsDataFileId      = 'DataFileId';
      rsValueField      = 'ValueField';
      rsLabelField      = 'LabelField';

   rsDataFiles       = 'DataFiles';
    rsDataFile        = 'DataFile';
     rsScreenInfo      = 'ScreenInfo';
     rsSections        = 'Sections';
      rsSection         = 'Section';
       rsWidth           = 'Width';
       rsHeight          = 'Height';
       rsGroupIdList     = 'GroupIdList';
       rsHeadings        = 'Headings';
        rsHeading         = 'Heading';
         rsCaption         = 'Caption';
       rsFields          = 'Fields';
        rsField           = 'Field';
         rsLength          = 'Length';
         rsDecimals        = 'Decimals';
         rsValueLabelId    = 'ValueLabelId';
         rsRanges           = 'Ranges';
          rsRange            = 'Range';
           rsStart            = 'start';
           rsEnd              = 'end';

     rsRecords         = 'Records';
      rsRecord          = 'Record';

   rsRelations       = 'Relations';
    rsPrimaryKeys     = 'PrimaryKeys';
     rsPrimaryKey      = 'PrimaryKey';
      rsFieldIdList     = 'FieldIdList';
    rsRelates         = 'Relates';
     rsRelate          = 'Relate';
      rsFieldId         = 'FieldId';
      rsValue           = 'Value';
      rsDestDataFileId  = 'DestDataFileId';

