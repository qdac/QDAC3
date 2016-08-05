unit SyncMultipeReadersOneWriter;

// #################################################
// #### Simple implementation of the multiple readers one exclusive writer
// #### pattern.
// #################################################

interface

uses Windows, SysUtils, SyncObjs;

{ .$Define CheckRecursiveWriteLock }
{ .$Define CheckCorrectReleaseAcquire }

// #################################################
// #### base idea (with some changes)
// from: http://firebird3.0.sourcearchive.com/documentation/3.0.0~svnplus-p53030.ds3-1/rwlock_8h_source.html
type
  TMultipleReaderOneWriter = class(TObject)
  private
    fMem: Pointer; // used to ensure 16byte alignement for the variables

    fLock: PInteger; // this is the actual lock
    // -50000 - writer is active
    // 0 - noone owns the lock
    // positive value - number concurrent readers
    fBlockedReaders: PInteger;
    fBlockedWriters: PInteger;

{$IFDEF CheckCorrectReleaseAcquire}
    fNumWriteLock: PInteger;
    fNumReadLock: PInteger;
{$ENDIF}
    fWriterEvt: TSimpleEvent;
    fReadersEvt: TSimpleEvent;

{$IFDEF CheckRecursiveWriteLock}
    fWriteLockThrdID: Cardinal;
    procedure CheckRecursiveWriteLock;
{$ENDIF}
  protected
    procedure UnblockWaiting; inline;
    function InternalTryAcquireReadLock: boolean; inline;
    function InternalTryAcquireWriteLock: boolean; inline;
  public
    procedure AcquireReadLock;
    procedure AcquireWriteLock;

    procedure ReleaseReadLock;
    procedure ReleaseWriteLock;

    function TryAcquireReadLock: boolean; overload;
    function TryAcquireWriteLock(const WaitTime: integer = 5000): boolean; overload;

    // for debug purposes

    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  LOCK_WRITER_OFFSET = 50000;

  { TMultipleReaderOneWriter }

procedure TMultipleReaderOneWriter.AcquireReadLock;
var
  waitRes: TWaitResult;
begin
  if not InternalTryAcquireReadLock then
  begin
    InterlockedIncrement(fBlockedReaders^);

    while not InternalTryAcquireReadLock do
    begin
      waitRes := fReadersEvt.WaitFor(500);
      if (waitRes <> wrSignaled) and (waitRes <> wrTimeout) then
        raise Exception.Create('Error - could not acquire read lock');
    end;

    InterlockedDecrement(fBlockedReaders^);
  end;

{$IFDEF CheckCorrectReleaseAcquire}
  InterlockedIncrement(fNumReadLock^);
  assert(fNumWriteLock^ = 0, 'Error num Write Locks is not zero (' +
    IntToStr(fNumWriteLock^) + ')');
{$ENDIF}
end;

procedure TMultipleReaderOneWriter.AcquireWriteLock;
begin
{$IFDEF CheckRecursiveWriteLock}
  CheckRecursiveWriteLock;
{$ENDIF}
  if not InternalTryAcquireWriteLock then
  begin
    InterlockedIncrement(fBlockedWriters^);
    while not InternalTryAcquireWriteLock do
    begin
      if not(fWriterEvt.WaitFor(5000) in [wrSignaled, wrTimeout]) then
        raise Exception.Create('Error acquiring write lock');
    end;

    InterlockedDecrement(fBlockedWriters^);
  end;

{$IFDEF CheckCorrectReleaseAcquire}
  InterlockedIncrement(fNumWriteLock^);
  assert(fNumReadLock^ = 0, 'Error num Read Locks is not zero (' + IntToStr(fNumReadLock^));
{$ENDIF}
end;

{$IFDEF CheckRecursiveWriteLock}

procedure TMultipleReaderOneWriter.CheckRecursiveWriteLock;
begin
  if GetCurrentThreadId = fWriteLockThrdID then
    raise Exception.Create('Error recursive write locks are not allowed.');
end;
{$ENDIF}

constructor TMultipleReaderOneWriter.Create;
begin
  inherited Create;

  fMem := AllocMem(6 * 16);

  fLock := PInteger(Cardinal(fMem) + 16 - Cardinal(fMem) and $F);
  fBlockedReaders := PInteger(PByte(fLock) + 16);
  fBlockedWriters := PInteger(PByte(fBlockedReaders) + 16);

  fReadersEvt := TSimpleEvent.Create(nil, False, False, '');
  fWriterEvt := TSimpleEvent.Create(nil, False, False, '');

{$IFDEF CheckCorrectReleaseAcquire}
  fNumWriteLock := PInteger(PByte(fBlockedWriters) + 16);
  fNumReadLock := PInteger(PByte(fNumWriteLock) + 16);
{$ENDIF}
  assert((Cardinal(fLock) and $4) = 0, 'Error lock needs to be aligned');
  assert((Cardinal(fBlockedReaders) and $4) = 0, 'Error blocked readers needs to be aligned');
  assert((Cardinal(fBlockedWriters) and $4) = 0, 'Error blocked writers needs to be aligned');
end;

destructor TMultipleReaderOneWriter.Destroy;
begin
  FreeMem(fMem);
  fReadersEvt.Free;
  fWriterEvt.Free;

  inherited;
end;

procedure TMultipleReaderOneWriter.ReleaseReadLock;
begin
{$IFDEF CheckCorrectReleaseAcquire}
  InterlockedDecrement(fNumReadLock^);
  assert(fNumWriteLock^ = 0, 'Error num Write Locks is not zero (' +
    IntToStr(fNumWriteLock^) + ')');
{$ENDIF}
  if InterlockedDecrement(fLock^) = 0 then
    UnblockWaiting;
end;

procedure TMultipleReaderOneWriter.ReleaseWriteLock;
begin
{$IFDEF CheckRecursiveWriteLock}
  fWriteLockThrdID := 0;
{$ENDIF}
{$IFDEF CheckCorrectReleaseAcquire}
  InterlockedDecrement(fNumWriteLock^);
  assert(fNumReadLock^ = 0, 'Error num Read Locks is not zero (' + IntToStr(fNumReadLock^) + ')');
{$ENDIF}
  if InterlockedExchangeAdd(fLock, LOCK_WRITER_OFFSET) = -LOCK_WRITER_OFFSET then
    UnblockWaiting;
end;

function TMultipleReaderOneWriter.TryAcquireReadLock: boolean;
var
  cnt: integer;
  waitRes: TWaitResult;
begin
  cnt := 0;

  Result := InternalTryAcquireReadLock;

  // try 5 times -> if nothing happens then we might be in analyze mode
  // thus fail to aquire the lock
  if not Result then
  begin
    InterlockedIncrement(fBlockedReaders^);

    while not Result and (cnt < 5) do
    begin
      waitRes := fReadersEvt.WaitFor(100);
      if (waitRes <> wrSignaled) and (waitRes <> wrTimeout) then
        exit;

      inc(cnt);
      Result := InternalTryAcquireReadLock;
    end;

    InterlockedDecrement(fBlockedReaders^);
  end;

{$IFDEF CheckCorrectReleaseAcquire}
  if Result then
  begin
    InterlockedIncrement(fNumReadLock^);
    assert(fNumWriteLock^ = 0, 'Error num Write Locks is not zero (' +
      IntToStr(fNumWriteLock^) + ')');
  end;
{$ENDIF}
end;

function TMultipleReaderOneWriter.TryAcquireWriteLock(const WaitTime: integer = 5000): boolean;
var
  cnt: integer;
  numIter: integer;
begin
{$IFDEF CheckRecursiveWriteLock}
  CheckRecursiveWriteLock;
{$ENDIF}
  // wait for all readers to be released.
  // but only for 5 seconds (default)!
  Result := InternalTryAcquireWriteLock;
  if not Result then
  begin
    cnt := 0;
    numIter := WaitTime div 100;

    InterlockedIncrement(fBlockedWriters^);

    while (cnt < numIter) and not Result do
    begin
      if not(fWriterEvt.WaitFor(100) in [wrSignaled, wrTimeout]) then
        exit;
      Result := InternalTryAcquireWriteLock;
      inc(cnt);
    end;

    InterlockedDecrement(fBlockedWriters^);
  end;

{$IFDEF CheckCorrectReleaseAcquire}
  if Result then
  begin
    InterlockedIncrement(fNumWriteLock^);
    assert(fNumReadLock^ = 0, 'Error num Read Locks is not zero (' + IntToStr(fNumReadLock^) + ')');
  end;
{$ENDIF}
end;

procedure TMultipleReaderOneWriter.UnblockWaiting;
begin
  if InterlockedExchangeAdd(fBlockedWriters, 0) <> 0 then
    fWriterEvt.SetEvent
  else if fBlockedReaders^ <> 0 then
  begin
    if InterlockedExchangeAdd(fBlockedReaders, 0) <> 0 then
      fReadersEvt.SetEvent;
  end;
end;

function TMultipleReaderOneWriter.InternalTryAcquireReadLock: boolean;
begin
  if InterlockedExchangeAdd(fLock, 0) < 0 then
  begin
    Result := False;
    exit;
  end;
  if InterlockedIncrement(fLock^) > 0 then
  begin
    Result := True;
    exit;
  end;
  // we stepped on writer's toes. Fix the mistake
  if InterlockedDecrement(fLock^) = 0 then
    UnblockWaiting;

  Result := False;
end;

function TMultipleReaderOneWriter.InternalTryAcquireWriteLock: boolean;
begin
  if InterlockedExchangeAdd(fLock, 0) <> 0 then
  begin
    Result := False;
    exit;
  end;

  if InterlockedExchangeAdd(fLock, -LOCK_WRITER_OFFSET) = 0 then
  begin
    Result := True;

{$IFDEF CheckRecursiveWriteLock}
    fWriteLockThrdID := GetCurrentThreadId;
{$ENDIF}
    exit;
  end;

  // We stepped on somebody's toes. Fix our mistake
  if InterlockedExchangeAdd(fLock, LOCK_WRITER_OFFSET) = -LOCK_WRITER_OFFSET then
    UnblockWaiting;
  Result := False;
end;

end.
