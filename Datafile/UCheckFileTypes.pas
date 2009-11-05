unit UCheckFileTypes;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  UEpiDataGlobals;

type

  TBeepTypes = (btWarning,btConfirmation,btStandard);
  TFieldScope = (scNone, scLocal, scGlobal, scCumulative);
  TLeaveStyles = (lsEnter,lsBrowse,lsJumpFirst,lsJumpLast,lsChangeRec,lsNone);
  TMissingValues = Array[0..MaxDefinedMissingValues] of string;   //Holds missing values for fields

  TChkCmdType = (cmdIF, cmdHelp, cmdHide, cmdUnhide, cmdClear, cmdGoTo, cmdComLegal,
                 cmdExit, cmdDefine, cmdAutosave, cmdConfirm, cmdTypeString,
                 cmdRelate, cmdIgnoreMissing, cmdWriteNote, cmdBackup, cmdBeep,
                 cmdLoad, cmdExecute, cmdColor, cmdMissingAll, cmdQuit,
                 cmdCopyToClipboard, cmdShowLastRecord, cmdDefaultAll,
                 //NB! Insert new codes BEFORE cmdLet
                 cmdLet, cmdComment, cmdLeaveField);

  TTypeType  = (ttNone, ttComment, ttField);

  TMissingAction = (maIgnoreMissing, maRejectMissing);

CONST
  ChkCmdNames: Array[TChkCmdType] of string =
                ('IF', 'HELP', 'HIDE', 'UNHIDE', 'CLEAR', 'GOTO', 'COMMENT',
                'EXIT', 'DEFINE', 'AUTOSAVE', 'CONFIRM', 'TYPE',
                'RELATE', 'IGNOREMISSING', 'WRITENOTE', 'BACKUP', 'BEEP',
                'LOAD', 'EXECUTE', 'COLOR', 'MISSINGVALUE', 'QUIT',
                'COPYTOCLIPBOARD', 'SHOWLASTRECORD','DEFAULTVALUE',
                'LET','dummy','leavefield');

implementation

end.
