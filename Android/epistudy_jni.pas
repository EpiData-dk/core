unit EpiStudy_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiStudyJNI = 'Java_dk_epidata_androidclient_core_EpiStudy_';

function EpiStudy_Create(Env: PJNIEnv; This: jobject; AOwner: jint): jint; cdecl;
procedure EpiStudy_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiStudy_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
function EpiStudy_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
procedure EpiStudy_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;
function EpiStudy_GetAbstractText(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetAuthor(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiStudy_SetAuthor(Env: PJNIEnv; This: jobject; Self: jint; Author: jstring); cdecl;
function EpiStudy_GetAgency(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiStudy_SetAgency(Env: PJNIEnv; This: jobject; Self: jint; Agency: jstring); cdecl;
function EpiStudy_GetCitations(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetCreated(Env: PJNIEnv; This: jobject; Self: jint): jdouble; cdecl;
function EpiStudy_GetFunding(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetGeographicalCoverage(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetIdentifier(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiStudy_SetIdentifier(Env: PJNIEnv; This: jobject; Self: jint; Identifier: jstring); cdecl;
function EpiStudy_GetModifiedDate(Env: PJNIEnv; This: jobject; Self: jint): jdouble; cdecl;
procedure EpiStudy_SetModifiedDate(Env: PJNIEnv; This: jobject; Self: jint; ModifiedDate: jdouble); cdecl;
function EpiStudy_GetNotes(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiStudy_SetNotes(Env: PJNIEnv; This: jobject; Self: jint; Notes: jstring); cdecl;
function EpiStudy_GetPublisher(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetPurpose(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetPopulation(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetRights(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetTitle(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetKeywords(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiStudy_SetKeywords(Env: PJNIEnv; This: jobject; Self: jint; Keywords: jstring); cdecl;
function EpiStudy_GetVersion(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiStudy_SetVersion(Env: PJNIEnv; This: jobject; Self: jint; Version: jstring); cdecl;
function EpiStudy_GetDataCollectionStart(Env: PJNIEnv; This: jobject; Self: jint): jdouble; cdecl;
procedure EpiStudy_SetDataCollectionStart(Env: PJNIEnv; This: jobject; Self: jint; DataCollectionStart: jdouble); cdecl;
function EpiStudy_GetDataCollectionEnd(Env: PJNIEnv; This: jobject; Self: jint): jdouble; cdecl;
procedure EpiStudy_SetDataCollectionEnd(Env: PJNIEnv; This: jobject; Self: jint; DataCollectionEnd: jdouble); cdecl;
function EpiStudy_GetDesign(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiStudy_GetUnitOfObservation(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;

implementation

uses
  epistudy, Laz2_DOM, epicustombase;

function EpiStudy_Create(Env: PJNIEnv; This: jobject; AOwner: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy.Create(TEpiCustomBase(AOwner)));
end;

procedure EpiStudy_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiStudy(Self).Destroy();
end;

function EpiStudy_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).XMLName());
end;

function EpiStudy_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint;
  Content: jstring; Lvl: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).SaveToXml(GetJNIString(Env, Content),Lvl));
end;

procedure EpiStudy_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint;
  Root: jint); cdecl;
begin
  TEpiStudy(Self).LoadFromXml(TDOMNode(Root));
end;

function EpiStudy_GetAbstractText(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiStudy(Self).AbstractText);
end;
function EpiStudy_GetAuthor(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).Author);
end;
procedure EpiStudy_SetAuthor(Env: PJNIEnv; This: jobject; Self: jint; Author: jstring); cdecl;
begin
  TEpiStudy(Self).Author := GetJNIString(Env, Author);
end;
function EpiStudy_GetAgency(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).Agency);
end;
procedure EpiStudy_SetAgency(Env: PJNIEnv; This: jobject; Self: jint; Agency: jstring); cdecl;
begin
  TEpiStudy(Self).Agency := GetJNIString(Env, Agency);
end;
function EpiStudy_GetCitations(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Citations);
end;
function EpiStudy_GetCreated(Env: PJNIEnv; This: jobject; Self: jint): jdouble;
  cdecl;
begin
  result := jdouble(TEpiStudy(Self).Created);
end;
function EpiStudy_GetFunding(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Funding);
end;
function EpiStudy_GetGeographicalCoverage(Env: PJNIEnv; This: jobject;
  Self: jint): jint; cdecl;
begin
  result := jint(TEpiStudy(Self).GeographicalCoverage);
end;
function EpiStudy_GetIdentifier(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).Identifier);
end;
procedure EpiStudy_SetIdentifier(Env: PJNIEnv; This: jobject; Self: jint; Identifier: jstring); cdecl;
begin
  TEpiStudy(Self).Identifier := GetJNIString(Env, Identifier);
end;
function EpiStudy_GetModifiedDate(Env: PJNIEnv; This: jobject; Self: jint
  ): jdouble; cdecl;
begin
  result := jdouble(TEpiStudy(Self).ModifiedDate);
end;
procedure EpiStudy_SetModifiedDate(Env: PJNIEnv; This: jobject; Self: jint;
  ModifiedDate: jdouble); cdecl;
begin
  TEpiStudy(Self).ModifiedDate := TDateTime(ModifiedDate);
end;
function EpiStudy_GetNotes(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).Notes);
end;
procedure EpiStudy_SetNotes(Env: PJNIEnv; This: jobject; Self: jint; Notes: jstring); cdecl;
begin
  TEpiStudy(Self).Notes := GetJNIString(Env, Notes);
end;
function EpiStudy_GetPublisher(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Publisher);
end;
function EpiStudy_GetPurpose(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Purpose);
end;
function EpiStudy_GetPopulation(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Population);
end;
function EpiStudy_GetRights(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Rights);
end;
function EpiStudy_GetTitle(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Title);
end;
function EpiStudy_GetKeywords(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).Keywords);
end;
procedure EpiStudy_SetKeywords(Env: PJNIEnv; This: jobject; Self: jint; Keywords: jstring); cdecl;
begin
  TEpiStudy(Self).Keywords := GetJNIString(Env, Keywords);
end;
function EpiStudy_GetVersion(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiStudy(Self).Version);
end;
procedure EpiStudy_SetVersion(Env: PJNIEnv; This: jobject; Self: jint; Version: jstring); cdecl;
begin
  TEpiStudy(Self).Version := GetJNIString(Env, Version);
end;
function EpiStudy_GetDataCollectionStart(Env: PJNIEnv; This: jobject; Self: jint
  ): jdouble; cdecl;
begin
  result := jdouble(TEpiStudy(Self).DataCollectionStart);
end;
procedure EpiStudy_SetDataCollectionStart(Env: PJNIEnv; This: jobject;
  Self: jint; DataCollectionStart: jdouble); cdecl;
begin
  TEpiStudy(Self).DataCollectionStart := TDateTime(DataCollectionStart);
end;
function EpiStudy_GetDataCollectionEnd(Env: PJNIEnv; This: jobject; Self: jint
  ): jdouble; cdecl;
begin
  result := jdouble(TEpiStudy(Self).DataCollectionEnd);
end;
procedure EpiStudy_SetDataCollectionEnd(Env: PJNIEnv; This: jobject;
  Self: jint; DataCollectionEnd: jdouble); cdecl;
begin
  TEpiStudy(Self).DataCollectionEnd := TDateTime(DataCollectionEnd);
end;
function EpiStudy_GetDesign(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiStudy(Self).Design);
end;
function EpiStudy_GetUnitOfObservation(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiStudy(Self).UnitOfObservation);
end;
end.

