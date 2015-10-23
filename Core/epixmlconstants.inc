const
  // Common constants in the XML tree.
  // - Generic
  rsType = 'type';
  rsName = 'name';
  rsCaption     = 'Caption';
  rsText        = 'Text';
  rsVersionAttr = 'version';
  rsBranchAttr  = 'branch';   // only used in development versions
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
    rsCapitalPassword = 'Password' deprecated;
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
      rsFullName        = 'FullName';
      rsPassword        = 'password';
      rsMasterPassword  = 'masterPassword';
      rsLastLogin       = 'lastLogin';
      rsExpireDate      = 'expireDate';
      rsGroupRefs       = 'GroupRefs';

    // Group
    rsGroups          = 'Groups';
     rsGroup           = 'Group';
      rsManageRights    = 'manageRights';

    rsGroupRights     = 'GroupRights';
     rsGroupRight      = 'GroupRight';
      rsEntryRights     = 'entryRights';
      rsGroupRef        = 'groupRef';

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

     rsFilename            = 'filename';

   rsDataFiles       = 'DataFiles';
    rsDataFile        = 'DataFile';
      rsStatusbarContentString = 'StatusbarContentString';
     rsAfterRecordState = 'afterRecordState';
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
         rsZeroFilled      = 'zerofilled';
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
        rsRelationRef     = 'relationRef';

      rsValueRelates    = 'ValueRelates';
       rsValueRelate     = 'ValueRelate';
        rsRelateValue     = 'relateValue';

     rsRecords         = 'Records';
      rsEncryptedSalt   = 'encryptedSalt';
      rsRecord          = 'Record';
       rsRecordStatus    = 'status';

   rsRelations       = 'Relations';
    rsRelation        = 'Relation';
     rsDataFileRef     = 'dataFileRef';
     rsMaxRecordCount  = 'maxRecordCount';
