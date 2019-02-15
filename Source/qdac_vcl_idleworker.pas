unit qdac_vcl_idleworker;
{ 如果启用 HOOK_NO_ALERTABLE_APIS 编译条件，要求搜索路径必需包含 Delphi Detours
  的相关单元，URL:https://github.com/MahdiSafsafi/delphi-detours-library
  如果不启用该编译条件，则 Application.ProcessMessages / Sleep/WaitForSingleObject/
  WaitForMultipleObjects 函数不会被挂接，由此在相关函数时不会触发空闲时要执行函数
  默认禁用
}
{.$DEFINE HOOK_NO_ALERTABLE_APIS }
{$IFDEF HOOK_NO_ALERTABLE_APIS}
{$DEFINE HOOK_SLEEP}
{$DEFINE HOOK_WAITOBJECT}
{$DEFINE HOOK_APPLICATION_PROCESSMESSAGES}
{$ENDIF}

interface

uses classes, sysutils, forms;

{ VCL Only }
type
  PQIdleJob = ^TQIdleJob;
{$IFDEF UNICODE}
  TQIdleJobProcA = reference to procedure(AParam: Pointer);
{$ENDIF}
  TQIdleJobProc = procedure(AParam: Pointer) of object;
  TQIdleJobProcG = procedure(AParam: Pointer);

  TQIdleJob = record
    BeforeExecute: TMethod;
    WorkProc: TMethod;
    AfterExecute: TMethod;
    Param: Pointer;
    Next: PQIdleJob;
  end;

  TQIdleWorker = class
  private
    class var FCurrent: TQIdleWorker;
    class function GetCurrent: TQIdleWorker; static;
  protected
    FFirst, FLast: PQIdleJob;
    FLastAppIdle: TIdleEvent;
    FQueueUserApcNeeded: Boolean;
    FRunInMainThread: Boolean;
    function FreeJob(AJob: PQIdleJob): PQIdleJob;
    procedure InvokeJobProc(const AProc: TMethod; AParam: Pointer); inline;
    procedure Execute;
    class procedure UserAPC(AParam: Pointer); stdcall; static;
    procedure Push(AItem: PQIdleJob);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure DoQueueUserApc;
  public
    procedure Queue(AProc: TQIdleJobProc; AParam: Pointer = nil;
      ADoneCallback: TQIdleJobProc = nil;
      APrepareCallback: TQIdleJobProc = nil); overload;
{$IFDEF UNICODE}
    procedure Queue(AProc: TQIdleJobProcA; AParam: Pointer = nil;
      ADoneCallback: TQIdleJobProcA = nil;
      APrepareCallback: TQIdleJobProcA = nil); overload;
{$ENDIF}
    procedure Queue(AProc: TQIdleJobProcG; AParam: Pointer = nil;
      ADoneCallback: TQIdleJobProcG = nil;
      APrepareCallback: TQIdleJobProcG = nil); overload;
    constructor Create;
    destructor Destroy; override;
    class destructor Destroy;
    class property Current: TQIdleWorker read GetCurrent;
  end;

implementation

uses windows, messages{$IFDEF HOOK_NO_ALERTABLE_APIS}, DDetours{$ENDIF};

var
  OldSleep, OldWaitForSingleObject, OldWaitForMultipleObjects: Pointer;

procedure NewSleep(dwMilliseconds: DWORD); stdcall;
var
  T: Cardinal;
begin
  T := GetTickCount;
  repeat
    SleepEx(dwMilliseconds, True);
  until GetTickCount - T >= dwMilliseconds;
end;

function NewWaitForSingleObject(hHandle: THandle; dwMilliseconds: DWORD)
  : DWORD; stdcall;
begin
  Result := WaitForSingleObjectEx(hHandle, dwMilliseconds, True);
end;

function NewWaitForMultipleObjects(nCount: DWORD; lpHandles: PWOHandleArray;
  bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall;
begin
  Result := WaitForMultipleObjectsEx(nCount, lpHandles, bWaitAll,
    dwMilliseconds, True);
end;

{ TQIdleWorker }

constructor TQIdleWorker.Create;
begin
  inherited;
  FLastAppIdle := Application.OnIdle;
  FRunInMainThread := GetCurrentThreadId = MainThreadId;
  if FRunInMainThread then
  begin
    Application.OnIdle := DoIdle;
    FQueueUserApcNeeded := True;
  end;
end;

destructor TQIdleWorker.Destroy;
begin
  while Assigned(FFirst) do
    FFirst := FreeJob(FFirst);
  inherited;
end;

class destructor TQIdleWorker.Destroy;
begin
  if Assigned(FCurrent) then
    FreeAndNil(FCurrent);
end;

procedure TQIdleWorker.DoIdle(Sender: TObject; var Done: Boolean);
begin
  Execute;
  Done := True;
end;

procedure TQIdleWorker.DoQueueUserApc;
begin
  QueueUserApc(@UserAPC, TThread.Current.Handle, IntPtr(Self));
  FQueueUserApcNeeded := False;
end;

procedure TQIdleWorker.Execute;
var
  AMsg: MSG;
  AItem:PQIdleJob;
begin
  while Assigned(FFirst) do
  begin
    try
      AItem:=FFirst;
      FFirst:=AItem.Next;
      if Assigned(AItem.BeforeExecute.Code) then
        InvokeJobProc(AItem.BeforeExecute, AItem.Param);
      InvokeJobProc(AItem.WorkProc, AItem.Param);
    finally
      if not Assigned(FFirst) then
        FLast := nil;
    end;
    if FRunInMainThread and PeekMessage(AMsg, 0, 0, 0, PM_NOREMOVE) then
      Break;
  end;
  FQueueUserApcNeeded := True;
end;

function TQIdleWorker.FreeJob(AJob: PQIdleJob): PQIdleJob;
begin
  Result := nil;
  if Assigned(AJob) then
  begin
    Result := AJob.Next;
{$IFDEF UNICODE}
    if (AJob.AfterExecute.Data = Pointer(-1)) and
      Assigned(AJob.AfterExecute.Code) then
    begin
      InvokeJobProc(AJob.AfterExecute, AJob.Param);
      TQIdleJobProcA(AJob.AfterExecute.Code) := nil;
    end;
    if (AJob.BeforeExecute.Data = Pointer(-1)) and
      Assigned(AJob.BeforeExecute.Code) then
      TQIdleJobProcA(AJob.BeforeExecute.Code) := nil;
    if (AJob.WorkProc.Data = Pointer(-1)) and Assigned(AJob.WorkProc.Code) then
      TQIdleJobProcA(AJob.WorkProc.Code) := nil;
{$ENDIF}
    Dispose(AJob);
  end;
end;

class function TQIdleWorker.GetCurrent: TQIdleWorker;
begin
  if not Assigned(FCurrent) then
    FCurrent := TQIdleWorker.Create;
  Result := FCurrent;
end;

procedure TQIdleWorker.InvokeJobProc(const AProc: TMethod; AParam: Pointer);
begin
  if IntPtr(AProc.Data) = 0 then
    TQIdleJobProcG(AProc.Code)(AParam)
{$IFDEF UNICODE}
  else if IntPtr(AProc.Data) = -1 then
    TQIdleJobProcA(AProc.Code)(AParam)
{$ENDIF}
  else
    TQIdleJobProc(AProc)(AParam);
end;

procedure TQIdleWorker.Push(AItem: PQIdleJob);
begin
  if not Assigned(FLast) then
    FFirst := AItem
  else
    FLast.Next := AItem;
  FLast := AItem;
  if FQueueUserApcNeeded then
    DoQueueUserApc;
end;

procedure TQIdleWorker.Queue(AProc: TQIdleJobProc; AParam: Pointer;
  ADoneCallback, APrepareCallback: TQIdleJobProc);
var
  AItem: PQIdleJob;
begin
  New(AItem);
  AItem.BeforeExecute := TMethod(APrepareCallback);
  AItem.WorkProc := TMethod(AProc);
  AItem.AfterExecute := TMethod(ADoneCallback);
  AItem.Param := AParam;
  AItem.Next := nil;
  Push(AItem);
end;
{$IFDEF UNICODE}

procedure TQIdleWorker.Queue(AProc: TQIdleJobProcA; AParam: Pointer;
  ADoneCallback, APrepareCallback: TQIdleJobProcA);
var
  AItem: PQIdleJob;
begin
  New(AItem);
  FillChar(AItem^, SizeOf(TQIdleJob), 0);
  AItem.BeforeExecute.Data := Pointer(-1);
  TQIdleJobProcA(AItem.BeforeExecute.Code) := APrepareCallback;
  AItem.WorkProc.Data := Pointer(-1);
  TQIdleJobProcA(AItem.WorkProc.Code) := AProc;
  AItem.AfterExecute.Data := Pointer(-1);
  TQIdleJobProcA(AItem.AfterExecute.Code) := ADoneCallback;
  AItem.Param := AParam;
  AItem.Next := nil;
  Push(AItem);
end;
{$ENDIF}

procedure TQIdleWorker.Queue(AProc: TQIdleJobProcG; AParam: Pointer;
  ADoneCallback, APrepareCallback: TQIdleJobProcG);
var
  AItem: PQIdleJob;
begin
  New(AItem);
  AItem.BeforeExecute.Data := nil;
  TQIdleJobProcG(AItem.BeforeExecute.Code) := APrepareCallback;
  AItem.WorkProc.Data := nil;
  TQIdleJobProcG(AItem.WorkProc.Code) := AProc;
  AItem.AfterExecute.Data := nil;
  TQIdleJobProcG(AItem.AfterExecute.Code) := ADoneCallback;
  AItem.Param := AParam;
  AItem.Next := nil;
  Push(AItem);
end;

class procedure TQIdleWorker.UserAPC(AParam: Pointer);
begin
  with TQIdleWorker(AParam) do
  begin
    Execute;
    if Assigned(FFirst) then
      DoQueueUserApc;
  end;
end;

type
  TSimpleProc = procedure of object;

procedure HookApis;
var
  AOld, ANew: TSimpleProc;
begin
{$IFDEF HOOK_NO_ALERTABLE_APIS}
{$IFDEF HOOK_APPLICATION_PROCESSMESSAGES}
  AOld := Application.ProcessMessages;
  ANew := Application.HandleMessage;
  InterceptCreate(TMethod(AOld).Code, TMethod(ANew).Code);
{$ENDIF}
{$IFDEF HOOK_SLEEP}
  OldSleep := InterceptCreate(@Sleep, @NewSleep);
{$ENDIF}
{$IFDEF HOOK_WAITOBJECT}
  OldWaitForSingleObject := InterceptCreate(@WaitForSingleObject,
    @NewWaitForSingleObject);
  OldWaitForMultipleObjects := InterceptCreate(@WaitForMultipleObjects,
    @NewWaitForMultipleObjects);
{$ENDIF}
{$ENDIF}
end;

procedure UnhookApis;
begin
{$IFDEF HOOK_NO_ALERTABLE_APIS}
{$IFDEF HOOK_WAITOBJECT}
  InterceptRemove(OldSleep);
{$ENDIF}
{$IFDEF HOOK_WAITOBJECT}
  InterceptRemove(OldWaitForSingleObject);
  InterceptRemove(OldWaitForMultipleObjects);
{$ENDIF}
{$ENDIF}
end;

initialization

HookApis;

finalization

UnhookApis;

end.
