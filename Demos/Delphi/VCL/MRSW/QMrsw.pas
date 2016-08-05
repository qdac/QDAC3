unit QMrsw;

interface

uses classes, syncobjs, qstring, windows;

type
  TQMRSW = class(TObject)
  protected
    FFlags: Integer;
    FReadCount: Integer; // 正在读取的数量
    FWriteCount: Integer; // 正在等待写入的数量
    FReadEvent, FWriteEvent: TEvent; // 读写事件通知，用于下一个读写请求处理
    FWritingThread: TThreadId; // 正在写入的线程编码
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
    function TryRead(AWaitTime: Cardinal): Boolean;
    function TryWrite(AWaitTime: Cardinal): Boolean;
  end;

implementation

const
  MRSW_READING = $01;
  MRSW_WRITING = $02;
  MRSW_ENDING  = $04;
  { TQMRSW }

procedure TQMRSW.BeginRead;
begin
TryRead(INFINITE);
end;

procedure TQMRSW.BeginWrite;
begin
TryWrite(INFINITE);
end;

constructor TQMRSW.Create;
begin
inherited;
FReadEvent := TEvent.Create(nil, true, false, '');
FWriteEvent := TEvent.Create(nil, false, false, '');
end;

destructor TQMRSW.Destroy;
begin
FreeObject(FReadEvent);
FreeObject(FWriteEvent);
inherited;
end;

procedure TQMRSW.EndRead;
var
  AFlags: Integer;
begin
if AtomicDecrement(FReadCount) = 0 then
  begin
  AtomicCmpExchange(FFlags, FFlags and (not MRSW_READING), MRSW_READING);
  FReadEvent.ResetEvent;
  // 没有读的了，那么就检查有没有需要写的，有则触发写事件
  if (FFlags = 0) and (FWriteCount > 0) then
    FWriteEvent.SetEvent;
  end;
end;

procedure TQMRSW.EndWrite;
var
  AThreadId: TThreadId;
begin
AThreadId := GetCurrentThreadId;
assert(FWritingThread = AThreadId, 'Only Writing Thread can EndWrite');
AtomicCmpExchange(FWritingThread, 0, AThreadId);
if AtomicCmpExchange(FFlags, FFlags and (not MRSW_WRITING), MRSW_WRITING) = MRSW_WRITING
then
  begin
  if AtomicDecrement(FWriteCount) = 0 then
    begin
    // 都写完了，那检查有没有读的
    if ((FFlags and MRSW_WRITING) = 0) and (FReadCount > 0) then
      FReadEvent.SetEvent;
    end
  else // 触发下一次写
    FWriteEvent.SetEvent;
  end;
end;

function TQMRSW.TryRead(AWaitTime: Cardinal): Boolean;
var
  AWaited: Cardinal;
  AFlags: Integer;
begin
Result := false;
AtomicIncrement(FReadCount);
AWaited := 0;
repeat
  AFlags := FFlags;
  if (AFlags and MRSW_WRITING) <> 0 then // 正在写，则等待下次
    begin
    FReadEvent.ResetEvent;
    if FReadEvent.WaitFor(100) = wrSignaled then
      Continue;
    Inc(AWaited, 100);
    end
  else
    begin
    if (AtomicCmpExchange(FFlags, MRSW_READING, AFlags) and MRSW_WRITING) = 0
    then
      begin
      // 未处于写的状态
      Result := true;
      Break;
      end;
    end;
until (AWaited >= AWaitTime);
if not Result then
  AtomicDecrement(FReadCount);
end;

function TQMRSW.TryWrite(AWaitTime: Cardinal): Boolean;
var
  AFlags: Integer;
begin
Result := false;
AtomicIncrement(FWriteCount);
repeat
  AFlags := AtomicCmpExchange(FFlags, MRSW_WRITING, 0);
  if (AFlags = 0) then
    begin
    Result := true;
    FWritingThread := GetCurrentThreadId;
    Break;
    end
  else
    begin
    if FWriteEvent.WaitFor(AWaitTime) = wrSignaled then
      Continue
    else
      Break;
    end;
until false;

end;

end.
