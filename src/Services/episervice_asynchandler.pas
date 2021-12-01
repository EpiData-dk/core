unit episervice_asynchandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidocument;

type

  TEpiSecviceAsyncThreadId = (esatMain, esatOther);
  TEpiSecviceAsyncThreadIds = set of TEpiSecviceAsyncThreadId;

  { TEpiSevice_AsyncHandler }

  TEpiSevice_AsyncHandler = class
  private
    FSyncEntry: Pointer;
    procedure InternalSyncronize();
  private
    FMethodList: TList;
    FDocuments: TList;
    procedure InternalChangeHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
//    procedure SetDocument(AValue: TEpiDocument);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterAsyncHandler(Event: TEpiChangeEvent; EventGroup: TEpiEventGroup; EventType: Word; ThreadIds: TEpiSecviceAsyncThreadIds = [esatMain, esatOther]);
    procedure UnRegisterAsyncHandler(Event: TEpiChangeEvent; EventGroup: TEpiEventGroup; EventType: Word);
    procedure AddDocument(Const ADocument: TEpiDocument);
    procedure RemoveDocument(Const ADocument: TEpiDocument);
//    property Document: TEpiDocument read FDocument write SetDocument;
  end;

var
  EpiAsyncHandlerGlobal: TEpiSevice_AsyncHandler;

implementation

type
    TAsyncEventEntry = record
      Event: TEpiChangeEvent;
      EventGroup: TEpiEventGroup;
      EventType: Word;
      EventThreadIDs: TEpiSecviceAsyncThreadIds;
      EventSender: TEpiCustomBase;
      EventInitiator: TEpiCustomBase;
      EventData: Pointer;
    end;
    PAsyncEventEntry = ^TAsyncEventEntry;

{ TEpiSevice_AsyncHandler }

procedure TEpiSevice_AsyncHandler.InternalSyncronize;
begin
  with PAsyncEventEntry(FSyncEntry)^ do
  begin
    Event(EventSender, EventInitiator, EventGroup, EventType, EventData);
  end;
end;

procedure TEpiSevice_AsyncHandler.InternalChangeHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Entry: PAsyncEventEntry;
  lThreadID: TEpiSecviceAsyncThreadId;
begin
  if (GetCurrentThreadId = MainThreadID) then
    lThreadID := esatMain
  else
    lThreadID := esatOther;

  // First pass on events
  for Entry in FMethodList do
  begin
    if (Entry^.EventGroup = EventGroup) and
       (Entry^.EventType  = EventType) and
       (lThreadID in Entry^.EventThreadIDs)
    then
      begin
        Entry^.EventSender := Sender;
        Entry^.EventInitiator := Initiator;
        Entry^.EventData := Data;
        FSyncEntry := Entry;

        TThread.Synchronize(nil, @InternalSyncronize);
      end;
  end;

  // Now check if this is a document and if it is being destroyed
  if (Initiator is TEpiDocument) and
     (EventGroup = eegCustomBase) and
     (EventType = Word(ecceDestroy))
  then
    begin
      // This document is destroying itself.
      // Remove it from the document list.
      RemoveDocument(TEpiDocument(Initiator));
    end;
end;

constructor TEpiSevice_AsyncHandler.Create;
begin
  FMethodList := TList.Create;
  FDocuments  := TList.Create;
end;

destructor TEpiSevice_AsyncHandler.Destroy;
var
  i: Integer;
begin
  for i := FDocuments.Count - 1 downto 0 do
    RemoveDocument(TEpiDocument(FDocuments[i]));

  FDocuments.Free;

  FMethodList.Clear;
  FMethodList.Free;
  inherited Destroy;
end;

procedure TEpiSevice_AsyncHandler.RegisterAsyncHandler(Event: TEpiChangeEvent;
  EventGroup: TEpiEventGroup; EventType: Word;
  ThreadIds: TEpiSecviceAsyncThreadIds);
var
  Entry: PAsyncEventEntry;
begin
  Entry := PAsyncEventEntry(New(PAsyncEventEntry));
  Entry^.Event := Event;
  Entry^.EventGroup := EventGroup;
  Entry^.EventType := EventType;
  Entry^.EventThreadIDs := ThreadIds;

  FMethodList.Add(Entry);
end;

procedure TEpiSevice_AsyncHandler.UnRegisterAsyncHandler(
  Event: TEpiChangeEvent; EventGroup: TEpiEventGroup; EventType: Word);
var
  Entry: PAsyncEventEntry;
  i: Integer;
begin
  for i := FMethodList.Count - 1 downto 0 do
  begin
    Entry := PAsyncEventEntry(FMethodList[i]);

    if (Entry^.Event = Event) and
       (Entry^.EventGroup = EventGroup) and
       (Entry^.EventType  = EventType)
    then
      FMethodList.Delete(i);
  end;
end;

procedure TEpiSevice_AsyncHandler.AddDocument(const ADocument: TEpiDocument);
begin
  if FDocuments.IndexOf(ADocument) >= 0 then exit;

  ADocument.RegisterOnChangeHook(@InternalChangeHook, true);
  FDocuments.Add(ADocument);
end;

procedure TEpiSevice_AsyncHandler.RemoveDocument(const ADocument: TEpiDocument);
var
  Idx: Integer;
begin
  Idx := FDocuments.IndexOf(ADocument);

  // If document not added then exit (may do an error?)
  if (Idx < 0) then Exit;
  ADocument.UnRegisterOnChangeHook(@InternalChangeHook);
  FDocuments.Delete(Idx);
end;

initialization
  EpiAsyncHandlerGlobal := TEpiSevice_AsyncHandler.Create;

finalization
  EpiAsyncHandlerGlobal.Free;

end.

