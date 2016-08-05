unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, qstring, qrbtree, qtimetypes, qworker,
  SyncObjs, ExtCtrls, dateutils, ExtActns;

type
  TQSystemTimes = record
    IdleTime, UserTime, KernelTime: UInt64;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Label2: TLabel;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Label3: TLabel;
    Button16: TButton;
    Button17: TButton;
    Label4: TLabel;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Button37: TButton;
    Button38: TButton;
    Button39: TButton;
    lblPlanStatic: TLabel;
    btnHandleTest: TButton;
    Button40: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure Button35Click(Sender: TObject);
    procedure Button36Click(Sender: TObject);
    procedure Button37Click(Sender: TObject);
    procedure Button38Click(Sender: TObject);
    procedure Button39Click(Sender: TObject);
    procedure btnHandleTestClick(Sender: TObject);
  private
    { Private declarations }
    FSignalId: Integer;
    FMulticastSignal: Integer;
    FRuns: Integer;
    FSignalWaitHandle: IntPtr;
    FTestHandle: IntPtr;
    FLastTimes: TQSystemTimes;
    FMaxCpuUsage: Double;
    procedure DoJobProc(AJob: PQJob);
    procedure DoPostJobDone(AJob: PQJob);
    procedure DoMainThreadWork(AJob: PQJob);
    procedure DoPostJobMsg(var AMsg: TMessage); message WM_APP;
    procedure SignalWaitProc(AJob: PQJob);
    procedure DoSignalJobMsg(var AMsg: TMessage); message WM_APP + 1;
    procedure DoTimerProc(AJob: PQJob);
    procedure DoTimerJobMsg(var AMsg: TMessage); message WM_APP + 2;
    procedure DoLongtimeWork(AJob: PQJob);
    procedure DoLongworkDone(AJob: PQJob);
    procedure DoAtTimeJob1(AJob: PQJob);
    procedure DoAtTimeJob2(AJob: PQJob);
    procedure DoDelayJob(AJob: PQJob);
    procedure DoCancelJob(AJob: PQJob);
    procedure DoNullJob(AJob: PQJob);
    procedure DoCOMJob(AJob: PQJob);
    procedure DoRandDelay(AJob: PQJob);
    procedure DoMsgPackJob(AJob: PQJob);
    procedure DoFirstJobStep(AJob: PQJob);
    procedure DoSecondJobStep(AJob: PQJob);
    procedure SelfTerminateJob(AJob: PQJob);
    procedure DoMulticastSingal1(AJob: PQJob);
    procedure DoMulticastSingal2(AJob: PQJob);
    procedure DoTimeoutGroupJob(AJob: PQJob);
    procedure DoGroupTimeout(ASender: TObject);
    procedure DoGroupTimeoutDone(AJob: PQJob);
    procedure DoLoopJob(AJob: PQJob);
    procedure DoThreadSyncJob(AJob: PQJob);
    procedure DoThreadSyncMsg;
    procedure PostTestJobs(ACount: Integer);
    procedure DoWorkerPostJob(AJob: PQJob);
    procedure DoForJobProc(AMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
    procedure DoRepeatJob(AJob: PQJob);
    procedure ShowRepeatDone(AJob: PQJob);
    procedure DoCreateComplex(var AData: Pointer);
    procedure DoFreeComplexRecord(AData: Pointer);
    procedure DoComplexJob(AJob: PQJob);
    procedure DoStringDataJob(AJob: PQJob);
    procedure DoExceptionJob(AJob: PQJob);
    procedure DoRunInMainThreadJob(AJob: PQJob);
    procedure RunInMainO(AData: Pointer);
    procedure DoPlanJob(AJob: PQJob);
    procedure DoCustomPlanJob(AJob:PQJob);
    function GetCpuUsage: Double;
  public
    { Public declarations }
  end;

  TAutoFreeTestObject = class
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

  PAutoFreeRecord = ^TAutoFreeRecord;

  TAutoFreeRecord = record
    Id: Integer;
  end;

  PComplexRecord = ^TComplexRecord;

  TComplexRecord = record
    Name: String;
    Age: Integer;
    Temp: array of Single;
  end;

var
  Form1: TForm1;

implementation

uses comobj, qmsgpack, qmapsymbols;
{$R *.dfm}

function GetThreadStacks(AThread: TThread): QStringW;
begin
  Result := StackOfThread(AThread);
end;

procedure TForm1.SelfTerminateJob(AJob: PQJob);
begin
  Label4.Caption := '自结束作业已运行 ' + IntToStr(AJob.Runs) + '次';
  if AJob.Runs = 3 then
  begin
    AJob.IsTerminated := True;
    Label4.Caption := '自结束作业已结束.';
  end;
end;

procedure TForm1.ShowRepeatDone(AJob: PQJob);
begin
  ShowMessage('重复投寄作业完成。');
end;

procedure TForm1.SignalWaitProc(AJob: PQJob);
begin
  PostMessage(Handle, WM_APP + 1, AJob.Runs, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  ACpuUsage: Double;
begin
  Workers.Signal(FSignalId);
  ACpuUsage := GetCpuUsage;
  if ACpuUsage > FMaxCpuUsage then
    FMaxCpuUsage := ACpuUsage;
  Caption := Format('QWorker示例(%d 核心,%d 工作者,%d 忙 ,CPU:%0.2f%%/%0.2f%%:Max)',
    [GetCpuCount, Workers.Workers, Workers.BusyWorkers, ACpuUsage,
    FMaxCpuUsage]);
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  ATime: TDateTime;
begin
  ATime := Now;
  ATime := IncSecond(ATime, 10);
  Workers.At(DoAtTimeJob2, ATime, qworker.Q1Hour, nil, True);
  ShowMessage('这个任务将在' + FormatDateTime('hh:nn:ss.zzz', ATime) +
    '时第一次启动，以后每隔1小时定时启动一次。');
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  Workers.Post(DoCancelJob, Pointer(1));
  // 直接取消简单作业队列中的作业，正常情况下是没来的及执行
  Workers.Clear(DoCancelJob, Pointer(1));
  Workers.Post(DoCancelJob, Pointer(2));
  // 作业已经进行了，取消操作会等待作业完成
  Sleep(100);
  Workers.Clear(DoCancelJob, Pointer(2));
  // 重复作业
  Workers.Post(DoCancelJob, 1000, Pointer(3));
  // 直接取消重复作业队列中的作业
  Workers.Clear(DoCancelJob, Pointer(3));
  // 重复作业
  Workers.Post(DoCancelJob, 1000, Pointer(4));
  Sleep(200);
  // 直接取消重复作业队列中的作业
  Workers.Clear(DoCancelJob, Pointer(4));
  // 信号作业队列
  Workers.Wait(DoCancelJob, FSignalId);
  Workers.Clear(DoCancelJob, nil);

end;

procedure TForm1.Button12Click(Sender: TObject);
var
  AData: PAutoFreeRecord;
begin
  Workers.Post(DoNullJob, TAutoFreeTestObject.Create, false, jdfFreeAsObject);
  New(AData);
  Workers.Delay(DoNullJob, 1000, AData, false, jdfFreeAsSimpleRecord);
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  Workers.Post(DoCOMJob, nil);
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  Workers.Signal('MySignal.Start');
  Workers.Signal('MySignal.Start');
  Workers.Post(DoNullJob, nil);
  Workers.Clear('MySignal.Start');
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  Workers.Delay(DoRandDelay, Q1Second, nil);
end;

procedure DoGlobalJob(AJob: PQJob);
begin
  ShowMessage('全局函数作业已调用。');
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  Workers.Post(DoGlobalJob, nil, True);
end;

procedure TForm1.Button17Click(Sender: TObject);
begin
  Workers.Post(SelfTerminateJob, 10000, nil, True);
end;

procedure TForm1.Button18Click(Sender: TObject);
var
  AId: Integer;
  T: Cardinal;
begin
  AId := Workers.RegisterSignal('Signal.SelfKill');
  Workers.Wait(SelfTerminateJob, AId, True);
  Workers.Signal(AId);
  T := GetTickCount;
  while GetTickCount - T < 500 do
    Application.ProcessMessages;
  Workers.Signal(AId);
  T := GetTickCount;
  while GetTickCount - T < 500 do
    Application.ProcessMessages;
  Workers.Signal(AId);
  T := GetTickCount;
  while GetTickCount - T < 500 do
    Application.ProcessMessages;
  Workers.Signal(AId);
end;

procedure TForm1.Button19Click(Sender: TObject);
var
  AGroup: TQJobGroup;
  AMsg: String;
begin
  AGroup := TQJobGroup.Create(True);
  if AGroup.WaitFor() <> wrSignaled then
    AMsg := 'WaitFor空作业列表失败';
  AGroup.Prepare;
  AGroup.Add(DoExceptionJob, nil, false);
  AGroup.Add(DoLongtimeWork, nil, false);
  AGroup.Add(DoNullJob, nil, false);
  AGroup.Add(DoNullJob, nil, false);
  AGroup.Run;
  if AGroup.MsgWaitFor() <> wrSignaled then
    AMsg := 'WaitFor多个作业失败';
  FreeObject(AGroup);
  if Length(AMsg) > 0 then
    ShowMessage(AMsg)
  else
    ShowMessage('分组作业执行成功完成。');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Timer1Timer(Sender);
end;

procedure TForm1.Button20Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  Workers.Post(
    procedure(AJob: PQJob)
    begin
      ShowMessage('匿名函数作业过程已经被调用。');
    end, nil, True);
{$ELSE}
  ShowMessage('当前Delphi版本过低，匿名函数不受支持。');
{$ENDIF}
end;

procedure TForm1.Button21Click(Sender: TObject);
var
  AMsgPack: TQMsgPack;
  // 当前你也可以使用XML或者JSON格式传递参数，效率上基本没啥差别
begin
  // 写法1
  Workers.Post(DoMsgPackJob, TQMsgPack.Create.Add('Name', '参数样式1')
    .Parent.Add('Id', 100).Parent, True, jdfFreeAsObject
  // 设置为jdfFreeAsObject让系统自动将Data成员当做对象释放
    );
  // 写法2
  AMsgPack := TQMsgPack.Create;
  AMsgPack.Add('Name', '参数样式2');
  AMsgPack.Add('Id', 101);
  Workers.Post(DoMsgPackJob, AMsgPack, True, jdfFreeAsObject)
end;

procedure TForm1.Button22Click(Sender: TObject);
begin
  Workers.Post(DoFirstJobStep, nil, false);
end;

procedure TForm1.Button23Click(Sender: TObject);
var
  AGroup: TQJobGroup;
begin
  AGroup := TQJobGroup.Create(True);
  AGroup.Prepare;
  AGroup.Add(DoFirstJobStep, Pointer(1), false);
  AGroup.Add(DoSecondJobStep, nil, True);
  AGroup.Run;
  // 因为作业2在主线程中运行，所以不能简单的WaitFor一直等待，否则会锁死
  while AGroup.WaitFor(10) <> wrSignaled do
    Application.ProcessMessages;
  FreeObject(AGroup);
end;

procedure TForm1.Button24Click(Sender: TObject);
var
  AParams: TQMsgPack;
begin
  AParams := TQMsgPack.Create;
  AParams.Add('TimeStamp').AsDateTime := Now;
  AParams.Add('Sender', 'Button24');
  Workers.Signal(FMulticastSignal, AParams, jdfFreeAsObject);
end;

procedure TForm1.Button25Click(Sender: TObject);
var
  AGroup: TQJobGroup;
  I: Integer;
  ACount: PInteger;
begin
  AGroup := TQJobGroup.Create(True);
  New(ACount);
  AGroup.Tag := ACount;
  ACount^ := 0;
  AGroup.Prepare;
  for I := 0 to 4 do
    AGroup.Add(DoTimeoutGroupJob, ACount);
  AGroup.Run(100);
  AGroup.AfterDone := DoGroupTimeout;
end;

procedure TForm1.Button26Click(Sender: TObject);
begin
  Workers.Post(DoLoopJob, nil, false);
  Sleep(100);
  Workers.Clear(DoLoopJob, nil);
end;

procedure TForm1.Button27Click(Sender: TObject);
begin
  Workers.Post(DoThreadSyncJob, nil);
end;

procedure TForm1.Button28Click(Sender: TObject);
var
  AGroup: TQJobGroup;
begin
  AGroup := TQJobGroup.Create;
  AGroup.Add(DoThreadSyncJob, nil);
  if AGroup.MsgWaitFor() = wrSignaled then
    ShowMessage('后台线程处理完成.')
  else
    ShowMessage('后台线程处理未正常完成.');
  FreeObject(AGroup);
end;

procedure TForm1.Button29Click(Sender: TObject);
begin
  Workers.Clear(DoTimerProc, INVALID_JOB_DATA);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Workers.Post(DoPostJobDone, nil);
end;

procedure DoFreeJobDataC1(ASender: TQWorkers; AFreeType: TQJobDataFreeType;
const AData: Pointer);
begin
  Dispose(PComplexRecord(AData));
end;

procedure TForm1.Button30Click(Sender: TObject);
var
  p: PComplexRecord;
begin
  New(p);
  p.Name := 'QDAC';
  p.Age := 2;
  SetLength(p.Temp, 2);
  p.Temp[0] := 36.5;
  p.Temp[1] := 37;
  Workers.OnCustomFreeData := DoFreeJobDataC1;
  Workers.Post(DoNullJob, p, false, jdfFreeAsC1);
end;

procedure DoGlobalJob1(AJob: PQJob);
begin
  AtomicIncrement(Form1.FRuns);
end;

procedure TForm1.Button31Click(Sender: TObject);
var
  T: Cardinal;
begin
  FRuns := 0;
  T := GetTickCount;
{$IFDEF UNICODE}
  TQForJobs.For(0, 999999,
    procedure(AMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt)
    begin
      AtomicIncrement(FRuns);
      // 如果要中断循环，调用AMrg.Break函数,注意，由于是并发处理，所以最终的执行次数很可能
      // 会大于AIndex的值
      // if AIndex>50000 then
      // AMgr.BreakIt;
    end, false, nil);
{$ELSE}
  TQForJobs.For(0, 999999, DoForJobProc, false, nil);
{$ENDIF}
  T := GetTickCount - T;
  ShowMessage(IntToStr(T) + 'ms,Runs=' + IntToStr(FRuns));
end;

procedure TForm1.Button32Click(Sender: TObject);
var
  AStatus: TQWorkerStatus;
  I: Integer;
  ASeconds: Int64;
  S: String;
begin
  AStatus := Workers.EnumWorkerStatus;
  for I := Low(AStatus) to High(AStatus) do
  begin
    S := S + '【工作者 ' + IntToStr(I) + '(ID=' + IntToStr(AStatus[I].ThreadId) +
      ')】'#13#10 + ' 已处理:' + IntToStr(AStatus[I].Processed) + ' 状态:';
    if AStatus[I].IsIdle then
    begin
      ASeconds := (GetTimeStamp - AStatus[I].LastActive) div Q1Second;
      if ASeconds > 0 then
        S := S + '空闲,末次工作时间:' + RollupTime(ASeconds) + '前)'#13#10#13#10
      else
        S := S + '空闲,末次工作时间:0秒前)'#13#10#13#10;
    end
    else
    begin
      S := S + '忙碌(作业:' + AStatus[I].ActiveJob + ')'#13#10;
      if Length(AStatus[I].Stacks) > 0 then
        S := S + ' 堆栈:'#13#10 + AStatus[I].Stacks + #13#10#13#10
      else
        S := S + #13#10;
    end;
  end;
  ShowMessage(S);
end;

procedure TForm1.Button33Click(Sender: TObject);
begin
  ShowMessage(EnumWaitChains);
end;

procedure TForm1.Button34Click(Sender: TObject);
begin
  Workers.Post(DoRepeatJob, nil);
end;

procedure TForm1.Button35Click(Sender: TObject);
var
  AData: PComplexRecord;
begin
  New(AData);
  // AData将在DoFreeComplexRecord中自动释放
  AData.Name := 'Do you hat QDAC?';
  Workers.Post(DoComplexJob, TQJobExtData.Create(AData, DoFreeComplexRecord),
    True, jdfFreeAsObject);
  Workers.Post(DoComplexJob, TQJobExtData.Create(DoCreateComplex,
    DoFreeComplexRecord), True, jdfFreeAsObject);
  Workers.Post(DoStringDataJob, TQJobExtData.Create('Hello,world of QDAC'),
    True, jdfFreeAsObject);
{$IFDEF UNICODE}
  // 使用匿名函数释放
  New(AData);
  AData.Name := 'Do you like QDAC?';
  Workers.Post(DoComplexJob, TQJobExtData.Create(AData,
    procedure(AData: Pointer)
    begin
      Dispose(PComplexRecord(AData));
    end), True, jdfFreeAsObject);
  // 使用匿名函数初始化和释放
  Workers.Post(DoComplexJob, TQJobExtData.Create(
    procedure(var AData: Pointer)
    var
      S: PComplexRecord;
    begin
      New(S);
      S.Name := 'Hello,I am too simple!';
      S.Age := 11;
      SetLength(S.Temp, 2);
      AData := S;
    end,
    procedure(AData: Pointer)
    begin
      Dispose(PComplexRecord(AData));
    end), True, jdfFreeAsObject);
{$ENDIF}
end;

procedure TForm1.Button36Click(Sender: TObject);
var
  AStates: TQJobStateArray;
  I: Integer;
  ATime: Int64;
  ALoc: TQSymbolLocation;
  ABuilder: TQStringCatHelperW;
  AForm: TForm;
  AMemo: TMemo;
begin
  AForm := TForm.Create(Self);
  AMemo := TMemo.Create(AForm);
  AMemo.Parent := AForm;
  AMemo.Align := alClient;
  AForm.Position := poScreenCenter;
  ATime := GetTimeStamp;
  AStates := Workers.EnumJobStates;
  ABuilder := TQStringCatHelperW.Create;
  ABuilder.Cat('共发现 ').Cat(Length(AStates)).Cat(' 项作业'#13#10);
  for I := 0 to High(AStates) do
  begin
    if ABuilder.Position > 1000 then
    begin
      ABuilder.Cat('...(后续省略)');
      Break;
    end;
    ABuilder.Cat('作业 #').Cat(I + 1);
    if (AStates[I].Flags and JOB_ANONPROC) = 0 then
    begin
      if LocateSymbol(AStates[I].Proc.Code, ALoc) then
        ABuilder.Cat(' - ').Cat(ALoc.FunctionName)
      else
        ABuilder.Cat(TObject(AStates[I].Proc.Data)
          .MethodName(AStates[I].Proc.Code));
    end
    else
      ABuilder.Cat(' - 匿名函数');
    if AStates[I].IsRunning then
      ABuilder.Cat(' 运行中:'#13#10)
    else
      ABuilder.Cat(' 计划中:'#13#10);
    case AStates[I].Handle and $03 of
      0:
        begin
          ABuilder.Cat(' 简单作业'#13#10);
          Continue;
        end;
      1:
        ABuilder.Cat(' 重复作业(距下次执行时间: ' + FormatFloat('0.#',
          (AStates[I].NextTime - ATime) / 10) + 'ms)'#13#10);
      2:
        ABuilder.Cat(' 信号作业'#13#10);
      3:
        begin
          ABuilder.Cat(' 计划任务'#13#10);
          ABuilder.Cat('   下次执行时间: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss',
            AStates[I].Plan.NextTime) + #13#10);
          ABuilder.Cat('   掩码:').Cat(AStates[I].Plan.AsString).Cat(#13#10);
        end;
    end;
    ABuilder.Cat(' 已运行:').Cat(AStates[I].Runs).Cat(' 次'#13#10).Cat(' 任务提交时间: ')
      .Cat(RollupTime((ATime - AStates[I].PushTime) div 10000)).Cat(' 前'#13#10);
    if AStates[I].PopTime <> 0 then
    begin
      ABuilder.Cat(' 末次执行时间: ');
      if ATime - AStates[I].PopTime > 10000 then
        ABuilder.Cat(RollupTime((ATime - AStates[I].PopTime) div 10000))
          .Cat(' 前'#13#10)
      else
        ABuilder.Cat(FloatToStr((ATime - AStates[I].PopTime) / 10))
          .Cat('ms 前'#13#10);
    end;
    ABuilder.Cat(' 平均每次用时:').Cat(FormatFloat('0.#', AStates[I].AvgTime / 10))
      .Cat('ms'#13#10).Cat(' 总计用时:')
      .Cat(FormatFloat('0.#', AStates[I].TotalTime / 10)).Cat('ms'#13#10)
      .Cat(' 最大用时:').Cat(FormatFloat('0.#', AStates[I].MaxTime / 10))
      .Cat('ms'#13#10).Cat(' 最小用时:')
      .Cat(FormatFloat('0.#', AStates[I].MinTime / 10)).Cat('ms'#13#10);
    ABuilder.Cat(' 标志位:');
    if (AStates[I].Flags and JOB_RUN_ONCE) <> 0 then
      ABuilder.Cat('单次,');
    if (AStates[I].Flags and JOB_IN_MAINTHREAD) <> 0 then
      ABuilder.Cat('主线程,');
    if (AStates[I].Flags and JOB_GROUPED) <> 0 then
      ABuilder.Cat('已分组,');
    ABuilder.Cat(SLineBreak);
  end;
  ClearJobStates(AStates);
  AMemo.Lines.Text := ABuilder.Value;
  AForm.ShowModal;
  FreeObject(AForm);
  FreeObject(ABuilder);
end;

procedure TForm1.Button37Click(Sender: TObject);
var
  AState: TQJobState;
  S: String;
  ATime: Int64;
  ALoc: TQSymbolLocation;
begin
  ATime := GetTimeStamp;
  if Workers.PeekJobState(FSignalWaitHandle, AState) then
  begin
    S := '作业 ';
    if (AState.Flags and JOB_ANONPROC) = 0 then
    begin
      if LocateSymbol(AState.Proc.Code, ALoc) then
        S := S + ' - ' + ALoc.FunctionName
      else
        S := S + TObject(AState.Proc.Data).MethodName(AState.Proc.Code);
    end
    else
      S := S + ' - 匿名函数';
    if AState.IsRunning then
      S := S + ' 运行中:'#13#10
    else
      S := S + ' 计划中:'#13#10;
    case AState.Handle and $03 of
      0:
        begin
          S := S + ' 简单作业'#13#10;
          ShowMessage(S);
          Exit;
        end;
      1:
        S := S + ' 重复作业(距下次执行时间: ' + FormatFloat('0.#',
          (AState.NextTime - ATime) / 10) + 'ms)'#13#10;
      2:
        S := S + ' 信号作业'#13#10;
    end;
    S := S + ' 已运行:' + IntToStr(AState.Runs) + ' 次'#13#10 + ' 任务提交时间:' +
      RollupTime((ATime - AState.PushTime) div 10000) + ' 前'#13#10;
    if AState.PopTime <> 0 then
      S := S + ' 末次执行时间:' + RollupTime((ATime - AState.PopTime) div 10000) +
        ' 前' + #13#10;
    S := S + ' 平均每次用时:' + FormatFloat('0.#', AState.AvgTime / 10) + 'ms'#13#10 +
      ' 总计用时:' + FormatFloat('0.#', AState.TotalTime / 10) + 'ms'#13#10 +
      ' 最大用时:' + FormatFloat('0.#', AState.MaxTime / 10) + 'ms'#13#10 + ' 最小用时:'
      + FormatFloat('0.#', AState.MinTime / 10) + 'ms'#13#10;
    S := S + ' 标志位:';
    if (AState.Flags and JOB_RUN_ONCE) <> 0 then
      S := S + '单次,';
    if (AState.Flags and JOB_IN_MAINTHREAD) <> 0 then
      S := S + '主线程,';
    if (AState.Flags and JOB_GROUPED) <> 0 then
      S := S + '已分组,';
    if S[Length(S)] = ',' then
      SetLength(S, Length(S) - 1);
    ShowMessage(S);
    ClearJobState(AState);
  end
  else
    ShowMessage('未找到请求的句柄对应的作业，作业可能已经完成。');
end;

procedure RunInMainG(AData: Pointer);
begin
  ShowMessage('您好，主线程全局函数版本(当前线程ID=' + IntToStr(GetCurrentThreadId) + ',主线程ID='
    + IntToStr(MainThreadId) + ').');
end;

procedure TForm1.RunInMainO(AData: Pointer);
begin
  ShowMessage('您好，主线程对象成员函数版本(当前线程ID=' + IntToStr(GetCurrentThreadId) +
    ',主线程ID=' + IntToStr(MainThreadId) + ').');
end;

procedure TForm1.DoRunInMainThreadJob(AJob: PQJob);
begin
{$IFDEF UNICODE}
  RunInMainThread(
    procedure
    begin
      ShowMessage('您好，主线程匿名函数版本(当前线程ID=' + IntToStr(GetCurrentThreadId) +
        ',主线程ID=' + IntToStr(MainThreadId) + ').');
    end);
{$ENDIF}
  RunInMainThread(RunInMainG, nil);
  RunInMainThread(RunInMainO, nil);
end;

procedure TForm1.Button38Click(Sender: TObject);
begin
  Workers.Post(DoRunInMainThreadJob, nil);
end;

procedure TForm1.Button39Click(Sender: TObject);
var
  AFormat: String;
  AMask: TQPlanMask;
begin
  AMask := TQPlanMask.Create;
  if InputQuery('计划任务设置', '请输入Linux Cron 格式的计划设置(秒 分 时 日 月 周 年)', AFormat) then
  begin
    AMask.AsString := AFormat;
    Workers.Plan(DoCustomPlanJob, AMask, nil, True);
    ShowMessage('你输入的任务设置转换为后的结果为：' + AMask.AsString + #13#10 +
      '系统已经添加了您需要的计划任务，耐心等待任务执行。');
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage(IntToStr(GetTimeStamp));
end;

procedure TForm1.PostTestJobs(ACount: Integer);
var
  J: Integer;
begin
  J := 0;
  while J < ACount do
  begin
    assert(Workers.Post(DoJobProc, nil) <> 0, 'Post failure');
    Inc(J);
  end;
end;

procedure TForm1.DoWorkerPostJob(AJob: PQJob);
begin
  PostTestJobs(NativeInt(AJob.Data));
end;

procedure TForm1.btnHandleTestClick(Sender: TObject);
var
  AHandle: IntPtr;
  T, APlanStart: Cardinal;
  ADone: Boolean;
begin
  btnHandleTest.Enabled := false;
  btnHandleTest.Caption := '测试简单作业';
  ADone := false;
  // Simple Jobs
  AHandle := Workers.Post(
    procedure(AJob: PQJob)
    begin
      if AHandle <> AJob.Handle then
        ShowMessage('Error')
      else
        ShowMessage('Simple:' + IntToStr(AJob.Handle));
      ADone := True;
    end, nil, True);
  while not ADone do
    Application.ProcessMessages;
  btnHandleTest.Caption := '测试重复作业';
  // Repeat Jobs
  ADone := false;
  AHandle := Workers.Post(
    procedure(AJob: PQJob)
    begin
      Workers.ClearSingleJob(AJob.Handle, false);
      if AJob.Handle <> AHandle then
        ShowMessage('Error')
      else
        ShowMessage('Repeat:' + IntToStr(AJob.Handle));
      ADone := True;
    end, 100, nil, True);
  while not ADone do
    Application.ProcessMessages;
  btnHandleTest.Caption := '测试信号作业';
  // Signal Job
  ADone := false;
  AHandle := Workers.Wait(
    procedure(AJob: PQJob)
    begin
      Workers.ClearSingleJob(AJob.Handle, false);
      if AHandle <> AJob.Handle then
        ShowMessage('Error')
      else
        ShowMessage('Signal:' + IntToStr(AJob.Handle));
      ADone := True;
    end, FSignalId, True);
  while not ADone do
    Application.ProcessMessages;
  // Plan Job
  btnHandleTest.Caption := '计划作业-60秒';
  ADone := false;
  AHandle := Workers.Plan(
    procedure(AJob: PQJob)
    begin
      Workers.ClearSingleJob(AJob.Handle, false);
      if AHandle <> AJob.Handle then
        ShowMessage('Error')
      else
        ShowMessage('Plan:' + IntToStr(AJob.Handle));
      ADone := True;
    end, '*', nil, True);
  T := GetTickCount;
  APlanStart := T;
  while not(ADone or Application.Terminated) do
  begin
    if GetTickCount - T > 1000 then
    begin
      if GetTickCount - APlanStart < 60000 then
        btnHandleTest.Caption := '计划作业-' +
          IntToStr(60 - (GetTickCount - APlanStart) div 1000) + '秒'
      else
        btnHandleTest.Caption := '测试计划作业';
      T := GetTickCount;
    end;
    Application.ProcessMessages;
  end;
  btnHandleTest.Caption := '句柄测试';
  btnHandleTest.Enabled := True;
end;

procedure TForm1.Button4Click(Sender: TObject);
const
  ACount: Integer = 1000000;
var
  ANeedRuns, ACpuNum: Int64;
  S: String;
  I: Integer;
  T1: Int64;
  procedure WaitDone;
  begin
    while (FRuns < ANeedRuns) do
    begin
{$IFDEF MSWINDOWS}
      SwitchToThread;
{$ELSE}
      TThread.Yield;
{$ENDIF}
      Application.ProcessMessages;
    end;
  end;

  procedure RunWithPoster(APostWorkers: Integer);
  var
    I, ASubCount, ASubTotal: Integer;
  begin
    T1 := GetTimeStamp;
    FRuns := 0;
    ASubCount := (ACount div APostWorkers);
    ASubTotal := 0;
    for I := 0 to APostWorkers - 1 do
    begin
      if I = APostWorkers - 1 then
        ASubCount := ACount - ASubTotal
      else
        Inc(ASubTotal, ASubCount);
      Workers.Post(DoWorkerPostJob, Pointer(ASubCount), false);
    end;
    WaitDone;
    T1 := (GetTimeStamp - T1);
    S := S + IntToStr(APostWorkers) + ' 个线程投寄者，用时:' + IntToStr(T1 div 10) +
      'ms,速度:' + IntToStr(Int64(FRuns) * 10000 div T1) + '次/秒'#13#10;
  end;

begin
  Button4.Enabled := false;
  ANeedRuns := ACount;
  // 多线程投寄，多线程处理测试
  ACpuNum := GetCpuCount;
  T1 := GetTimeStamp;
  FRuns := 0;
  Workers.DisableWorkers;
  PostTestJobs(ACount);
  Workers.EnableWorkers;
  WaitDone;
  T1 := (GetTimeStamp - T1);
  S := '主线程投寄(批量模式)，用时:' + IntToStr(T1 div 10) + 'ms,速度:' +
    IntToStr(Int64(FRuns) * 10000 div T1) + '次/秒'#13#10;
  T1 := GetTimeStamp;
  FRuns := 0;
  PostTestJobs(ACount);
  WaitDone;
  T1 := (GetTimeStamp - T1);
  S := S + '主线程投寄，用时:' + IntToStr(T1 div 10) + 'ms,速度:' +
    IntToStr(Int64(FRuns) * 10000 div T1) + '次/秒'#13#10;
  for I := 1 to ACpuNum * 2 do
  begin
    RunWithPoster(I);
  end;
  ShowMessage(S);
  Button4.Enabled := True;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Workers.Post(DoMainThreadWork, nil, True);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Workers.Signal('MySignal.Start');
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  if Workers.LongtimeJob(DoLongtimeWork, nil) = 0 then
    ShowMessage('长时间作业投寄失败');
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  ShowMessage('这个任务将在5秒后第一次启动，以后每隔1小时定时启动一次。');
  Workers.At(DoAtTimeJob1, 5 * qworker.Q1Second, qworker.Q1Hour, nil, True)
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  Workers.Delay(DoDelayJob, 5 * qworker.Q1Second, nil, True)
end;

procedure TForm1.DoAtTimeJob1(AJob: PQJob);
begin
  ShowMessage('定时5秒后执行的任务已经执行了' + IntToStr(AJob.Runs + 1) + '次，1小时后执行下一次');
end;

procedure TForm1.DoAtTimeJob2(AJob: PQJob);
begin
  ShowMessage('定时任务已在' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + '开始第'
    + IntToStr(AJob.Runs + 1) + '次执行，1小时后执行下一次'#13#10 + '入队时间:' +
    IntToStr(AJob.PushTime) + #13#10 + '出队时间:' + IntToStr(AJob.PopTime));
end;

procedure TForm1.DoCancelJob(AJob: PQJob);
begin
  OutputDebugStringW(PWideChar(QStringW('DoCancelJob(') + IntToHex(IntPtr(AJob),
    8) + ')-' + IntToStr(Integer(AJob.Data)) + ' Started'));
  Sleep(5000);
  OutputDebugStringW(PWideChar(QStringW('DoCancelJob(') + IntToHex(IntPtr(AJob),
    8) + ')-' + IntToStr(Integer(AJob.Data)) + ' Finished'));
end;

procedure TForm1.DoCOMJob(AJob: PQJob);
var
  ADispatch: IDispatch;
begin
  AJob.Worker.ComNeeded();
  try
    ADispatch := CreateOleObject('ADODB.Recordset');
  except
  end;
end;

procedure TForm1.DoComplexJob(AJob: PQJob);
var
  AData: PComplexRecord;
begin
  AData := AJob.ExtData.Origin;
  ShowMessage(AData.Name);
end;

procedure TForm1.DoCreateComplex(var AData: Pointer);
var
  S: PComplexRecord;
begin
  New(S);
  S.Name := 'Hello,I am from QDAC Complex Demo';
  S.Age := 3;
  SetLength(S.Temp, 2);
  AData := S;
end;

procedure TForm1.DoCustomPlanJob(AJob: PQJob);
var
  APlan: PQJob;
begin
  APlan := AJob.PlanJob;
  lblPlanStatic.Caption := '计划任务已执行' + IntToStr(AJob.Runs + 1) + '次'#13#10 +
    '内容:' + APlan.ExtData.AsPlan.Plan.Content;
end;

procedure TForm1.DoDelayJob(AJob: PQJob);
begin
  ShowMessage('延迟的任务已经执行完成了。'#13#10 + '入队时间:' + IntToStr(AJob.PushTime) + #13#10
    + '出队时间:' + IntToStr(AJob.PopTime));
end;

procedure TForm1.DoExceptionJob(AJob: PQJob);
begin
  raise Exception.Create('只是一个测试作业异常');
end;

procedure TForm1.DoFirstJobStep(AJob: PQJob);
var
  AUrl: TDownloadURL;
begin
  AUrl := TDownloadURL.Create(nil);
  AUrl.URL :=
    'http://api.map.baidu.com/geocoder/v2/?address=长春市同光路&output=json&ak=E4805d16520de693a3fe707cdc962045&callback=showLocation';
  AUrl.Filename := ExtractFilePath(Application.ExeName) + 'baidu.html';
  if AUrl.Execute and (not Assigned(AJob.Data)) then
  begin
    Workers.Post(DoSecondJobStep, nil, True, jdfFreeAsSimpleRecord);
  end;
  AUrl.Free;
end;

procedure TForm1.DoForJobProc(AMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
begin
  AtomicIncrement(FRuns);
end;

procedure TForm1.DoFreeComplexRecord(AData: Pointer);
begin
  Dispose(PComplexRecord(AData));
end;

procedure TForm1.DoGroupTimeout(ASender: TObject);
begin
  Workers.Post(DoGroupTimeoutDone, ASender, True, jdfFreeAsObject)
end;

procedure TForm1.DoGroupTimeoutDone(AJob: PQJob);
var
  AGroup: TQJobGroup;
begin
  AGroup := AJob.Data;
  ShowMessage('分组超时作业实际完成' + IntToStr(PInteger(AGroup.Tag)^) + '次(计划10次)');
  Dispose(AGroup.Tag);
end;

procedure TForm1.DoJobProc(AJob: PQJob);
begin
  AtomicIncrement(FRuns);
end;

procedure TForm1.DoLongtimeWork(AJob: PQJob);
begin
  OutputDebugString('长时间作业开始.');
  while not AJob.IsTerminated do
  begin
    Sleep(1000);
    if AJob.ElapsedTime > 50000 then // 5s后结束任务，注意计时单位为0.1ms
      AJob.IsTerminated := True;
  end;
  if not Workers.Terminating then
    // 如果未结束，则触发一个通知能前台，这样方便前台做一些进一步处理
    Workers.Signal('Longwork.Done');
  OutputDebugString('长时间作业结束.');
end;

procedure TForm1.DoLongworkDone(AJob: PQJob);
begin
  ShowMessage('长时间作业已经完成');
end;

procedure TForm1.DoLoopJob(AJob: PQJob);
begin
  while not AJob.IsTerminated do
  begin
    Sleep(50);
  end;
end;

procedure TForm1.DoMainThreadWork(AJob: PQJob);
begin
  ShowMessage('这是在主线程中触发的异步作业。');
end;

procedure TForm1.DoMsgPackJob(AJob: PQJob);
begin
  ShowMessage(TQMsgPack(AJob.Data).AsString);
end;

procedure TForm1.DoMulticastSingal1(AJob: PQJob);
var
  AParams: TQMsgPack;
begin
  AParams := AJob.Data;
  ShowMessage('本提示来自DoMulticastSignal1,参数：'#13#10 + AParams.AsString);
end;

procedure TForm1.DoMulticastSingal2(AJob: PQJob);
var
  AParams: TQMsgPack;
begin
  AParams := AJob.Data;
  ShowMessage('本提示来自DoMulticastSignal2,参数：'#13#10 + AParams.AsString);
end;

procedure TForm1.DoNullJob(AJob: PQJob);
begin
  OutputDebugString('Null Job Executed');
end;

procedure TForm1.DoPlanJob(AJob: PQJob);
var
  APlan: PQJob;
begin
  APlan := AJob.PlanJob;
  lblPlanStatic.Caption := '计划任务已执行' + IntToStr(AJob.Runs + 1) + '次'#13#10 +
    '内容:' + APlan.ExtData.AsPlan.Plan.Content;
end;

procedure TForm1.DoPostJobDone(AJob: PQJob);
begin
  PostMessage(Handle, WM_APP, AJob.PopTime - AJob.PushTime, 0);
end;

procedure TForm1.DoPostJobMsg(var AMsg: TMessage);
begin
  ShowMessage(Format('作业投寄到执行用时 %g ms', [AMsg.WParam / 10]));
end;

procedure TForm1.DoRandDelay(AJob: PQJob);
begin
  Label3.Caption := '随机作业末次延迟 ' + IntToStr((AJob.PopTime - AJob.PushTime)
    div 10) + 'ms';
  Workers.Delay(AJob.WorkerProc.Proc, qworker.Q1Second +
    random(qworker.Q1Second), AJob.Data, True);
end;

procedure TForm1.DoRepeatJob(AJob: PQJob);
begin
  if Integer(AJob.Data) = 3 then
    Workers.Post(ShowRepeatDone, nil, True)
  else
  begin
    // 使用AJob的WorkerProc成员来投寄
{$IFDEF UNICODE}
    if AJob.IsAnonWorkerProc then // 匿名的话，使用第一种方式，否则用第二种即可
      Workers.Delay(TQJobProcA(AJob.WorkerProc.ProcA), random(30000),
        Pointer(Integer(AJob.Data) + 1))
    else
{$ENDIF}
      Workers.Delay(AJob.WorkerProc.Proc, random(30000),
        Pointer(Integer(AJob.Data) + 1));
    // 更直接的是直接使用Workers.Delay(DoRepeatJob,...)，因为调用自己，自己调用自
    // 己嘛，自己怎么能不知道自己姓啥呢
  end;
end;

procedure TForm1.DoSecondJobStep(AJob: PQJob);
var
  AFileName: String;
begin
  AFileName := ExtractFilePath(Application.ExeName) + 'baidu.html';
  MessageBoxW(Handle, PQCharW(LoadTextW(AFileName)), '结果', MB_OK);
  DeleteFile(AFileName);
end;

procedure TForm1.DoSignalJobMsg(var AMsg: TMessage);
begin
  Label2.Caption := Format('信号MySignal.Start已触发 %d次', [AMsg.WParam]);
end;

procedure TForm1.DoStringDataJob(AJob: PQJob);
begin
  ShowMessage(AJob.ExtData.AsString);
end;

procedure TForm1.DoThreadSyncJob(AJob: PQJob);
begin
  AJob.Synchronize(DoThreadSyncMsg);
end;

procedure TForm1.DoThreadSyncMsg;
begin
  ShowMessage('作业中调用线程的同步方法完成');
end;

procedure TForm1.DoTimeoutGroupJob(AJob: PQJob);
begin
  Sleep(50);
  AtomicIncrement(PInteger(AJob.Data)^);
end;

procedure TForm1.DoTimerJobMsg(var AMsg: TMessage);
begin
  Label1.Caption := '定时任务已执行' + IntToStr(AMsg.WParam) + '次';
end;

procedure TForm1.DoTimerProc(AJob: PQJob);
begin
  PostMessage(Handle, WM_APP + 2, AJob.Runs, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutDown := True;
  // 注册一个信号触发函数，以便在触发时执行
  FSignalId := Workers.RegisterSignal('MySignal.Start');
  FMulticastSignal := Workers.RegisterSignal('Multicase.Start');
  Workers.Wait(DoMulticastSingal1, FMulticastSignal);
  Workers.Wait(DoMulticastSingal2, FMulticastSignal);
  FSignalWaitHandle := Workers.Wait(SignalWaitProc, FSignalId);
  /// // / 使用名称来触发的信号
  Workers.Wait(DoLongworkDone, Workers.RegisterSignal('Longwork.Done'), True);
  // 注册一个定时执行任务信号，每0.1秒触发一次
  Workers.Post(DoTimerProc, 1000, nil);
  Workers.Plan(DoPlanJob, '0 * * * * * "1 Minute Plan Job"', nil, True);
  Workers.Plan(DoPlanJob, '0 0/2 * * * * "2 Minute Plan Job"', nil, True);
  Timer1Timer(Sender);
  GetThreadStackInfo := GetThreadStacks;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  Workers.Clear(Self);
end;

function TForm1.GetCpuUsage: Double;
var
  Usage, Idle: UInt64;
  CreateTime, ExitTime, IdleTime, UserTime, KernelTime: TFileTime;
  CurTimes: TQSystemTimes;
  function FileTimeToI64(const ATime: TFileTime): Int64;
  begin
    Result := (Int64(ATime.dwHighDateTime) shl 32) + ATime.dwLowDateTime;
  end;

begin
  Result := 0;
  if GetProcessTimes(GetCurrentProcess, CreateTime, ExitTime, KernelTime,
    UserTime) then
  begin
    CurTimes.UserTime := FileTimeToI64(UserTime);
    CurTimes.KernelTime := FileTimeToI64(KernelTime);
    CurTimes.IdleTime := GetTimeStamp;
    Usage := (CurTimes.UserTime - FLastTimes.UserTime) +
      (CurTimes.KernelTime - FLastTimes.KernelTime);
    if FLastTimes.IdleTime <> 0 then
    begin
      Idle := CurTimes.IdleTime - FLastTimes.IdleTime;
      if Idle > 0 then
        Result := Usage / Idle / GetCpuCount / 10;
    end;
    FLastTimes := CurTimes;
  end;

end;

{ TAutoFreeTestObject }

constructor TAutoFreeTestObject.Create;
begin
  OutputDebugString('TAutoFreeTestObject.Create');
end;

destructor TAutoFreeTestObject.Destroy;
begin
  OutputDebugString('TAutoFreeTestObject.Free');
  inherited;
end;

end.
