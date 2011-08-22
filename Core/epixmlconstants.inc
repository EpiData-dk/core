const
  // Common constants in the XML tree.
  // - Generic
  rsType = 'type';
  rsName = 'name';
  rsCaption     = 'Caption';
  rsText        = 'Text';
  rsVersionAttr = 'version';
  rsCreatedAttr = 'created';

  // - CustomItem
  rsId   = 'id';
  // - CustomControlItem
  rsTop  = 'top';
  rsLeft = 'left';

  // XML Tree structure.
  rsEpiData         = 'EpiData';
   // Settings
   rsSettings        = 'Settings';
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
    rsVersion         = 'Version';

   rsValueLabelSets  = 'ValueLabelSets';
    rsValueLabelSet   = 'ValueLabelSet';
     rsInternal        = 'Internal';
      rsValueLabel      = 'ValueLabel';
       rsLabel           = 'Label';
       rsValue           = 'value';
       rsOrder           = 'order';
       rsMissing         = 'missing';
     rsExternal        = 'External';
      rsFile            = 'File';
      rsDataFileId      = 'DataFileId';
      rsValueField      = 'ValueField';
      rsLabelField      = 'LabelField';

   rsDataFiles       = 'DataFiles';
    rsDataFile        = 'DataFile';
     rsSections        = 'Sections';
      rsSection         = 'Section';
       rsWidth           = 'width';
       rsHeight          = 'height';
       rsHeadings        = 'Headings';
        rsHeading         = 'Heading';
       rsFields          = 'Fields';
        rsField           = 'Field';
         rsLength          = 'length';
         rsDecimals        = 'decimals';
         rsEntryMode       = 'entrymode';
         rsConfirmEntry    = 'confirmentry';
         rsValueLabelId    = 'valuelabelref';
         rsValueLabelWriteRef = 'valuelabelwriteref';
         rsShowValueLabel  = 'showvaluelabel';
         rsForcePickList   = 'forcepicklist';
         rsRepeatValue     = 'repeat';
         rsDefaultValue    = 'defaultvalue';
         rsAutoUpdateMode  = 'autoupdatemode';
         rsQuestion        = 'Question';
         rsNotes           = 'Notes';
         rsRanges           = 'Ranges';
          rsRange            = 'Range';
           rsStart            = 'start';
           rsEnd              = 'end';
         rsJumps           = 'Jumps';
          rsJump            = 'Jump';
           rsJumpOn          = 'on';
           rsJumpType        = 'type';
           rsJumpTo          = 'to';
           rsJumpReset       = 'reset';
         rsCalculation     = 'Calculation';
          rsResultField     = 'ResultField';
          rsTimeCalcType    = 'TimeCalcType';
          rsStartDate       = 'StartDate';
          rsStartTime       = 'StartTime';
          rsEndDate         = 'EndDate';
          rsEndTime         = 'EndTime';
          rsDayField        = 'DayField';
          rsMonthField      = 'MonthField';
          rsYearField       = 'YearField';
          rsField1          = 'Field1';
          rsField2          = 'Field2';
          rsField3          = 'Field3';
          rsDelim1          = 'Delimiter1';
          rsDelim2          = 'Delimiter2';
         rsCompare         = 'Compare';
          rsCompareTo       = 'to';
          rsCompareType     = 'type';

     rsRecords         = 'Records';
      rsRecord          = 'Record';
       rsRecordStatus    = 'st';

   rsRelations       = 'Relations';
    rsPrimaryKeys     = 'PrimaryKeys';
     rsPrimaryKey      = 'PrimaryKey';
      rsFieldIdList     = 'FieldIdList';
    rsRelates         = 'Relates';
     rsRelate          = 'Relate';
      rsFieldId         = 'FieldId';
      rsRelateValue     = 'RelateValue';
      rsDestDataFileId  = 'DestDataFileId';

