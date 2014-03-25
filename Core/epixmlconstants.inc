const
  // Common constants in the XML tree.
  // - Generic
  rsType = 'type';
  rsName = 'name';
  rsCaption     = 'Caption';
  rsText        = 'Text';
  rsVersionAttr = 'version';
  rsCreatedAttr = 'created';
  rsModifiedAttr = 'modified';

  // - CustomItem
  rsId   = 'id';
  // - CustomControlItem
  rsTop  = 'top';
  rsLeft = 'left';

  // XML Tree structure.
  rsEpiData         = 'EpiData';
   rsCycle          = 'cycle';
   // Settings
   rsSettings        = 'Settings';
//    rsScrambled       = 'Scrambled';
    rsDateSep         = 'dateSeparator';
    rsTimeSep         = 'timeSeparator';
    rsDecSep          = 'decimalSeparator';
    rsMissingStr      = 'missingString';

   rsProjectSettings = 'ProjectSettings';
    rsAutoIncStart    = 'autoIncStart';
    rsTimedBackupInterval='timedBackupInterval';
    rsBackupOnShutdown= 'backupOnShutdown';
    rsShowFieldNames  = 'showFieldNames';
    rsShowFieldBorders= 'showFieldBorders';

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

   rsStudy           = 'StudyInfo';
    rsAbstract        = 'Abstract';
    rsAgency          = 'Agency';
    rsAuthor          = 'Author';
    rsCitations       = 'Citations';
    rsCreated         = 'Created';
    rsFunding         = 'Funding';
    rsGeoCoverage     = 'GeographicalCoverage';
    rsIdentifier      = 'Identifier';
    rsKeywords        = 'Keywords';
    rsLanguage        = 'Language';
    rsModified        = 'Modified';
    rsOtherLanguages  = 'OtherLanguages';
    rsPublisher       = 'Publisher';
    rsPurpose         = 'Purpose';
    rsPopulation      = 'Population';
    rsTimeCoverage    = 'TimeCoverage';
    rsTitle           = 'Title';
    rsVersion         = 'Version';
    rsDataColectionStart = 'DataCollectionStart';
    rsDataColectionEnd   = 'DataCollectionEnd';
    rsDesign             = 'StudyDesign';
    rsUnitOfObservation  = 'UnitOfObservation';

   rsValueLabelSets  = 'ValueLabelSets';
    rsValueLabelSet   = 'ValueLabelSet';
     rsValueLabel      = 'ValueLabel';
      rsValueLabelScope = 'scope';                   // Version 3+ property
      rsInternal        = 'Internal';                // version 1+2 property, deleted in version 3
       rsLabel           = 'Label';
       rsValue           = 'value';
       rsOrder           = 'order';
       rsMissing         = 'missing';

     rsFile            = 'File';



{     // version 1+2 property, but never used
      rsExternal        = 'External';
      rsFile            = 'File';
      rsDataFileId      = 'DataFileId';
      rsValueField      = 'ValueField';
      rsLabelField      = 'LabelField';}

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
         rsEntryMode       = 'entryMode';
         rsConfirmEntry    = 'confirmEntry';
         rsValueLabelId    = 'valueLabelRef';
         rsValueLabelWriteRef = 'valueLabelWriteFieldRef';
         rsShowValueLabel  = 'showValueLabel';
         rsShowValueLabelNotes  = 'showValueLabelNotes';
         rsForcePickList   = 'forcePickList';
         rsRepeatValue     = 'repeat';
         rsDefaultValue    = 'defaultValue';
         rsAutoUpdateMode  = 'autoUpdateMode';
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
           rsJumpTo          = 'toFieldRef';
           rsJumpReset       = 'reset';
         rsCalculation     = 'Calculation';
          rsResultField     = 'ResultFieldRef';
          rsTimeCalcType    = 'TimeCalcTypeRef';
          rsStartDate       = 'StartDateRef';
          rsStartTime       = 'StartTimeRef';
          rsEndDate         = 'EndDateRef';
          rsEndTime         = 'EndTimeRef';
          rsDayField        = 'DayFieldRef';
          rsMonthField      = 'MonthFieldRef';
          rsYearField       = 'YearFieldRef';
          rsField1          = 'Field1Ref';
          rsField2          = 'Field2Ref';
          rsField3          = 'Field3Ref';
          rsDelim1          = 'Delimiter1';
          rsDelim2          = 'Delimiter2';
         rsCompare         = 'Compare';
          rsCompareTo       = 'toFieldRef';
         rsRelates         = 'Relates';
          rsRelate          = 'Relate';
           rsRelationRef  = 'relationRef';
           rsRelateValue     = 'relateValue';

     rsRecords         = 'Records';
      rsRecord          = 'Record';
       rsRecordStatus    = 'status';

   rsRelations       = 'Relations';
    rsRelation        = 'Relation';
     rsDataFileRef     = 'dataFileRef';
     rsMaxRecordCount  = 'maxRecordCount';
