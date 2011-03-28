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
    rsAutoIncStart    = 'AutoIncStart';
    rsTimedBackupInterval='TimedBackupInterval';
    rsBackupOnShutdown= 'BackupOnShutdown';
    rsShowFieldNames  = 'ShowFieldNames';
    rsShowFieldBorders= 'ShowFieldBorders';

   rsAdmin           = 'Admin';
    // User
    rsUsers           = 'Users';
     rsUser            = 'User';
      rsFirstName       = 'FirstName';
      rsLastName        = 'LastName';
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
    rsAbstract        = 'Abstract';
    rsAuthor          = 'Author';
    rsCitations       = 'Citations';
    rsCreated         = 'Created';
    rsFunding         = 'Funding';
    rsGeoCoverage     = 'GeographicalCoverage';
    rsIdentifier      = 'Identifier';
    rsLanguage        = 'Language';
    rsModified        = 'Modified';
    rsOtherLanguages  = 'OtherLanguages';
    rsPublisher       = 'Publisher';
    rsPurpose         = 'Purpose';
    rsTimeCoverage    = 'TimeCoverage';
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
         rsQuestion        = 'Question';
         rsNotes           = 'Notes';
         rsValueLabelId    = 'ValueLabelId';
         rsValueLabelWriteRef = 'ValueLabelWriteRef';
         rsRanges           = 'Ranges';
          rsRange            = 'Range';
           rsStart            = 'start';
           rsEnd              = 'end';
         rsJumps            = 'Jumps';
          rsJump             = 'Jump';
           rsJumpOn           = 'on';
           rsJumpType         = 'type';
           rsJumpTo           = 'to';
           rsJumpReset        = 'reset';
         rsEntryMode        = 'EntryMode';
         rsConfirmEntry     = 'ConfirmEntry';
         rsShowValueLabel   = 'ShowValueLabel';
         rsRepeatValue      = 'Repeat';
         rsDefaultValue     = 'DefaultValue';
         rsAutoUpdateMode   = 'AutoUpdateMode';
         rsCalculation      = 'Calculation';
          rsResultField      = 'ResultField';
          rsTimeCalcType     = 'TimeCalcType';
          rsStartDate        = 'StartDate';
          rsStartTime        = 'StartTime';
          rsEndDate          = 'EndDate';
          rsEndTime          = 'EndTime';
          rsDayField         = 'DayField';
          rsMonthField       = 'MonthField';
          rsYearField        = 'YearField';
          rsField1           = 'Field1';
          rsField2           = 'Field2';
          rsField3           = 'Field3';
          rsDelim1           = 'Delimiter1';
          rsDelim2           = 'Delimiter2';

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

