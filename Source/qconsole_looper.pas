unit qconsole_looper;

interface

uses classes, sysutils, syncobjs,qstring{$IFDEF POSIX}, Posix.UniStd, Posix.Signal{$ENDIF};

type
  { TQConsoleLooper 用来控制台循环，保证程序能够在主线程中执行一些后台线程需要前台处理的过程，
    请注意如果你调用 ReadLn 一类的IO阻塞操作会阻止主线程去执行 TThread.Queue/Synchronize/ForceQueue
    等请求执行的异步函数，在其执行完成后会恢复执行。同理除非不得不做，不要在投递的函数做阻塞操作。
  }

  TPosixSignalNotify = procedure(Sender: TObject; ASignalNum: Integer; var AHandled: Boolean) of object;

  TQConsoleLooper = class
  private
    class var FCurrent: TQConsoleLooper;
    class
    function GetCurrent: TQConsoleLooper; static;
  protected
    FEvent: TEvent;
    FOnTerminate: TNotifyEvent;
    FPidFile: String;
    FOnSignal: TPosixSignalNotify;
    FTerminated, FTerminateOnException, FIsDaemon: Boolean;
    FSignals: TIntegerSet;
    procedure DoWakeMainThread(AObject: TObject);
    procedure DoSignalEvent(ASignalNum: Integer);
    class procedure DoSignal(SigNum: Integer); cdecl; static;
  public
    constructor Create(ATerminateOnException: Boolean); overload;
    destructor Destroy; override;
    class destructor Destroy;
    procedure Run;
    procedure Terminate;
    procedure HandleSignal(ASignalNum: Integer);
    procedure UnhandleSignal(ASignalNum: Integer);
    property Terminated: Boolean read FTerminated;
    property TerminateOnException: Boolean read FTerminateOnException write FTerminateOnException;
    property IsDaemon: Boolean read FIsDaemon write FIsDaemon;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    property OnSignal: TPosixSignalNotify read FOnSignal write FOnSignal;
    property PidFile:String read FPidFile write FPidFile;
    class property Current: TQConsoleLooper read GetCurrent;
  end;

implementation

{ TQConsoleLooper }

constructor TQConsoleLooper.Create(ATerminateOnException: Boolean);
begin
  inherited Create;
  FTerminateOnException := ATerminateOnException;
  FEvent := TEvent.Create(nil, false, false, '');
  WakeMainThread := DoWakeMainThread;
{$IFDEF POSIX}
  Signal(SIGQUIT, DoSignal);
  Signal(SIGABORT, DoSignal);
{$ENDIF}
end;

destructor TQConsoleLooper.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

class function TQConsoleLooper.GetCurrent: TQConsoleLooper;
begin
  if not Assigned(FCurrent) then
    FCurrent := TQConsoleLooper.Create(false);
  Result := FCurrent;
end;

procedure TQConsoleLooper.HandleSignal(ASignalNum: Integer);
begin
{$IFDEF POSIX}
  Signal(ASignalNum, DoSignal);
{$ENDIF}
end;

procedure TQConsoleLooper.Run;
  {$IFDEF POSIX}
  function DaemonCheck:Boolean;
  var
    AProcessId: Integer;
  begin
    Result:=false;
    if IsDaemon then // 如果是 Linux 守护进程，则尝试 fork 一个子进程，然后退出
    begin
      AProcessId:=fork();
      if AProcessId<>0 then
        begin
        Result:=True;
        Terminate;
        if Length(PidFile)>0 then
          SaveTextU(IntToStr(PidFile));
        end;
    end;
  end;
  {$ENDIF}
begin
  {$IFDEF POSIX}
  if DaemonCheck then
    Exit;
  {$ENDIF}
  repeat
    try
      CheckSynchronize;
      if not FTerminated then
        FEvent.WaitFor(INFINITE);
    except
      on E: Exception do
      begin
        if not(E is EAbort) then
        begin
          WriteLn(E.ClassName + ':' + E.Message);
          if FTerminateOnException then
            FTerminated := True;
        end;
      end;
    end;
  until FTerminated;
end;

procedure TQConsoleLooper.Terminate;
begin
  FTerminated := True;
  FEvent.SetEvent;
end;

procedure TQConsoleLooper.UnhandleSignal(ASignalNum: Integer);
begin
{$IFDEF POSIX}
  Signal(ASignalNum, SIG_DFL);
{$ENDIF}
end;

class destructor TQConsoleLooper.Destroy;
begin
  if Assigned(FCurrent) then
    FreeAndNil(FCurrent);
end;

class procedure TQConsoleLooper.DoSignal(SigNum: Integer);
begin
  Current.DoSignalEvent(SigNum);
end;

procedure TQConsoleLooper.DoSignalEvent(ASignalNum: Integer);
{$IFDEF POSIX}
var
  AHandled: Boolean;
{$ENDIF}
begin
  // 仅Posix系支持
{$IFDEF POSIX}
  AHandled := false;
  if Assigned(OnSignal) then
    OnSignal(Self, ASignalNum, AHandled);
  if not AHandled then
  begin
    if (ASignalNum in [SIGQUIT, SIGABRT]) or TerminateOnException then
      Terminate;
  end;
{$ENDIF}
end;

procedure TQConsoleLooper.DoWakeMainThread(AObject: TObject);
begin
  FEvent.SetEvent;
end;

end.
