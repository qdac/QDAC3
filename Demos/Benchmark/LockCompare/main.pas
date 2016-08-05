unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,GpLockFreeQueue, StdCtrls,syncobjs,
  ExtCtrls, TeEngine, TeeProcs,
  Chart, ComCtrls, Series,JwaWinBase,JwaWinNT
  {$IF RTLVersion>18.5}
  //sWZ_SysCoreUnit Can't compile under 2007
  ,sWZ_SysCoreUnit
  {$IFEND}
  ;
{$I 'qdac.inc'}
type
  PSListHead=^TSListHead;

  TSListHead=record
    Next:PSListHead;
    Data:Pointer;
  end;
  //一个基于位锁的简单锁定对象，使用原子函数置位
  TQSimpleLock=class
  private
    FFlags:Integer;
  public
    constructor Create;
    procedure Enter;inline;
    procedure Leave;inline;
  end;
  TQSimpleQueue=class
  protected
    FFirst:PSListHead;
    FCount:Integer;
    FLocker:TQSimpleLock;
    procedure Clear;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Push(AData:Pointer);
    function Pop:Pointer;
  end;

  TQCSQueue=class
  protected
    FFirst:PSListHead;
    FCount:Integer;
    FLocker:SyncObjs.TCriticalSection;
    procedure Clear;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Push(AData:Pointer);
    function Pop:Pointer;
  end;

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCount:Integer;
    FThreadCount:Integer;
    FSimpleQueue:TQSimpleQueue;
    FGpQueue:TGpLockFreeQueue;
    FCsQueue:TQCSQueue;
    FSListHead:SLIST_HEADER;
    {$IF RTLVersion>18.5}
    FSwzQueue: TSwzSingleListWithLock;
    {$IFEND}
    procedure DoLockTest0;
    procedure DoLockTest1;
    procedure DoLockTest2;
    procedure DoLockTest3;
    procedure DoLockTest4;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  function AtomicAnd(var Dest:Integer;const AMask:Integer): Integer;inline;
  function AtomicOr(var Dest:Integer;const AMask:Integer):Integer;inline;
  {$IF RTLVersion<26}
  function AtomicIncrement(var Dest:Integer;const ADelta:Integer=1):Integer;inline;
  function AtomicCmpExchange(var Target: Integer; Value: Integer; Comparand: Integer): Integer;inline;
  {$IFEND}
implementation
const
  LoopCount:Integer=5000000;
var
  LastKernelTime,LastUserTime,LastCpuUsageCheckTime:Int64;
{$R *.dfm}
type
  TProcedureObject=procedure of object;
  TTestThread=class(TThread)
  protected
    FProc:TProcedureObject;
    procedure Execute;override;
  public
    constructor Create(AProc:TProcedureObject);overload;
  end;
{$IF RTLVersion<26}
function AtomicIncrement(var Dest:Integer;const ADelta:Integer=1):Integer;inline;
var
  J:Integer;
begin
if ADelta=1 then
  Result:=InterlockedIncrement(Dest)
else if ADelta=-1 then
  Result:=InterlockedDecrement(Dest)
else
  begin
  repeat
    Result:=Dest;
    J:=Result+ADelta;
  until InterlockedCompareExchange(Dest,J,Result)=Result;
  end;
end;

function AtomicCmpExchange(var Target: Integer; Value: Integer; Comparand: Integer): Integer;inline;
begin
Result:=InterlockedCompareExchange(Target,Value,Comparand);
end;
{$IFEND}
//位与，返回原值
function AtomicAnd(var Dest:Integer;const AMask:Integer): Integer;inline;
var
  i:Integer;
begin
repeat
  Result:=Dest;
  i:=Result and AMask;
until AtomicCmpExchange(Dest,i,Result)=Result;
end;
//位或，返回原值
function AtomicOr(var Dest:Integer;const AMask:Integer):Integer;inline;
var
  i:Integer;
begin
repeat
  Result:=Dest;
  i:=Result or AMask;
until {$IF RTLVersion<26}InterlockedCompareExchange{$ELSE}AtomicCmpExchange{$IFEND}(Dest,i,Result)=Result;
end;

constructor TQSimpleLock.Create;
begin
inherited;
FFlags:=0;
end;

procedure TQSimpleLock.Enter;
begin
while (AtomicOr(FFlags,$01) and $01)<>0 do
  begin
  {$IFDEF UNICODE}
  TThread.Yield;
  {$ELSE}
  SwitchToThread;
  {$ENDIF}
  end;
end;

procedure TQSimpleLock.Leave;
begin
AtomicAnd(FFlags,Integer($FFFFFFFE));
end;
function GetCpuUsage:Integer;
var
  AExitTime,AKernelTime,AUserTime,ACreateTime,AUsed,AIdle,ADelta:Int64;
begin
Result:=0;
if GetProcessTimes(GetCurrentProcess(),LPFILETIME(@ACreateTime)^,LPFILETIME(@AExitTime)^,
  LPFILETIME(@AKernelTime)^,LPFILETIME(@AUserTime)^) then
  begin
	if LastKernelTime <> 0 then
		begin
		AUsed:=(AKernelTime+AUserTime-LastKernelTime-LastUserTime) div 10000;
		ADelta:=GetTickCount-LastCpuUsageCheckTime;
		if ADelta > 0 then
			Result:=AUsed*100 div ADelta;
    end;
	LastKernelTime:=AKernelTime;
	LastUserTime:=AUserTime;
	end;
LastCpuUsageCheckTime:=GetTickCount;
end;

function GetProcessTime:Cardinal;
var
  {$IF RTLVersion>=26}
  st:TThread.TSystemTimes;
  {$ELSE}
  Idle, User, Kernel: TFileTime;
  {$IFEND}
begin
{$IF RTLVersion>=26}
TThread.GetSystemTimes(st);
Result:=(st.UserTime+st.KernelTime) div 100000;
{$ELSE}
if GetSystemTimes(@Idle, @Kernel, @User) then
  begin
  Result:=((UInt64(User.dwHighDateTime) shl 32 or User.dwLowDateTime)+
        (UInt64(Kernel.dwHighDateTime) shl 32 or Kernel.dwLowDateTime)) div 100000;
  end;
{$IFEND}
end;
procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  T1,T2,T3:Cardinal;
  J: Integer;
begin
Button1.Enabled:=False;
FSimpleQueue:=TQSimpleQueue.Create;
FGpQueue:=TGpLockFreeQueue.Create;
FCsQueue:=TQCSQueue.Create;
{$IF RTLVersion>18.5}
FSwzQueue := TSwzSingleListWithLock.Create;
{$IFEND}
InitializeSListhead(@FSListHead);
Memo1.Clear;
Series1.Clear;
Series2.Clear;
Series3.Clear;
for J := 2 to 20 do
  begin
  Memo1.Lines.Add('线程数:'+IntToStr(J));
  Application.ProcessMessages;
  GetCpuUsage;
  FCount:=0;
  T1:=GetTickCount;
  FThreadCount:=J;
  for I := 0 to J-1 do
    TTestThread.Create(DoLockTest1).Resume;
  while FThreadCount>0 do
    SwitchToThread;
  T1:=GetTickCount-T1;
  Sleep(50);
  Series1.AddXY(J,T1);
  Memo1.Lines.Add('SimpleLock 测试完成，用时'+IntToStr(T1)+'ms,CPU平均占用率:'+IntToStr(GetCpuUsage)+'%');
  Application.ProcessMessages;
  {$IF RTLVersion>18.5}
  GetCpuUsage;
  FCount:=0;
  FThreadCount:=J;
  T2:=GetTickCount;
  for I := 0 to J-1 do
    TTestThread.Create(DoLockTest0).Resume;
  while FThreadCount>0 do
    SwitchToThread;
  T2:=GetTickCount-T2;
  Sleep(50);
  Memo1.Lines.Add('SwzSingleList 测试完成，用时'+IntToStr(T2)+'ms,CPU平均占用率:'+IntToStr(GetCpuUsage)+'%');
  Series5.AddXY(J,T2);
  {$IFEND}
  GetCpuUsage;
  FCount:=0;
  FThreadCount:=J;
  T2:=GetTickCount;
  for I := 0 to J-1 do
    TTestThread.Create(DoLockTest2).Resume;
  while FThreadCount>0 do
    SwitchToThread;
  T2:=GetTickCount-T2;
  Sleep(50);
  Memo1.Lines.Add('LockFree 测试完成，用时'+IntToStr(T2)+'ms,CPU平均占用率:'+IntToStr(GetCpuUsage)+'%');
  Series2.AddXY(J,T2);
  Application.ProcessMessages;
  GetCpuUsage;
  FCount:=0;
  FThreadCount:=J;
  T3:=GetTickCount;
  for I := 0 to J-1 do
    TTestThread.Create(DoLockTest3).Resume;
  while FThreadCount>0 do
    SwitchToThread;
  T3:=GetTickCount-T3;
  Sleep(50);
  Memo1.Lines.Add('CriticalSection 测试完成，用时'+IntToStr(T3)+'ms,CPU平均占用率:'+IntToStr(GetCpuUsage)+'%');
  Series3.AddXY(J,T3);
  Application.ProcessMessages;
  GetCpuUsage;
  FCount:=0;
  FThreadCount:=J;
  T1:=GetTickCount;
  for I := 0 to J-1 do
    TTestThread.Create(DoLockTest4).Resume;
  while FThreadCount>0 do
    SwitchToThread;
  T1:=GetTickCount-T1;
  Memo1.Lines.Add('WindowsSList 测试完成，用时'+IntToStr(T1)+'ms,CPU平均占用率:'+IntToStr(GetCpuUsage)+'%');
  Application.ProcessMessages;
  Series4.AddXY(J,T1);
  Sleep(50);
  end;
FSimpleQueue.Free;
FGpQueue.Free;
FCsQueue.Free;
{$IF RTLVersion>18.5}
FSwzQueue.Free;
{$IFEND}
Button1.Enabled:=True;
ShowMessage('测试完成');
end;

procedure TForm1.DoLockTest0;
var
  I:Integer;
begin
{$IF RTLVersion>18.5}
repeat
    for I:=0 to 49 do
      FSwzQueue.Push(nil);
    for I:=0 to 49 do
      FSwzQueue.Pop;
  until AtomicIncrement(FCount,50)>=LoopCount;
{$IFEND}
AtomicIncrement(FThreadCount,-1);
end;

procedure TForm1.DoLockTest1;
var
  I:Integer;
begin
repeat
  for I:=0 to 49 do
    FSimpleQueue.Push(nil);
  for I:=0 to 49 do
    FSimpleQueue.Pop;
until AtomicIncrement(FCount,50)>=LoopCount;
AtomicIncrement(FThreadCount,-1);
end;

procedure TForm1.DoLockTest2;
var
  AValue:Int64;
  I:Integer;
begin
repeat
  for I:=0 to 49 do
    FGpQueue.Enqueue(0);
  for I:=0 to 49 do
    FGpQueue.Dequeue(AValue);
until AtomicIncrement(FCount,50)>=LoopCount;
AtomicIncrement(FThreadCount,-1);
end;

procedure TForm1.DoLockTest3;
var
  I:Integer;
begin
repeat
  for I:=0 to 49 do
    FCsQueue.Push(nil);
  for I:=0 to 49 do
    FCsQueue.Pop;
until AtomicIncrement(FCount,50)>=LoopCount;
AtomicIncrement(FThreadCount,-1);
end;

procedure TForm1.DoLockTest4;
var
  AEntry:PSLIST_ENTRY;
  I:Integer;
begin
repeat
  for I:=0 to 49 do
    begin
    New(AEntry);
    InterlockedPushEntrySList(@FSListHead,AEntry);
    end;
  for I:=0 to 49 do
    begin
    AEntry:=InterlockedPopEntrySList(@FSListHead);
    Dispose(AEntry);
    end;
until AtomicIncrement(FCount,50)>=LoopCount;
AtomicIncrement(FThreadCount,-1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
LastKernelTime:=0;
LastUserTime:=0;
LastCpuUsageCheckTime:=0;
end;

{ TQSimpleQueue }

procedure TQSimpleQueue.Clear;
var
  ANext:PSListHead;
begin
  inherited;
FLocker.Enter;
try
  while FFirst<>nil do
    begin
    ANext:=FFirst.Next;
    Dispose(FFirst);
    FFirst:=ANext;
    end;
finally
  FLocker.Leave;
end;
end;

constructor TQSimpleQueue.Create;
begin
  inherited;
FFirst:=nil;
FCount:=0;
FLocker:=TQSimpleLock.Create;
end;

destructor TQSimpleQueue.Destroy;
begin
Clear;
FreeAndNil(FLocker);
  inherited;
end;

function TQSimpleQueue.Pop: Pointer;
var
  AFirst:PSListHead;
begin
AFirst:=nil;
Result:=nil;
FLocker.Enter;
if FFirst<>nil then
  begin
  AFirst:=FFirst;
  Result:=FFirst.Data;
  FFirst:=AFirst.Next;
  end;
FLocker.Leave;
if AFirst<>nil then
  Dispose(AFirst);
end;

procedure TQSimpleQueue.Push(AData: Pointer);
var
  ANew:PSListHead;
begin
New(ANew);
ANew.Data:=AData;
FLocker.Enter;
ANew.Next:=FFirst;
FFirst:=ANew;
Inc(FCount);
FLocker.Leave;
end;

{ TQCSQueue }

procedure TQCSQueue.Clear;
var
  ANext:PSListHead;
begin
  inherited;
FLocker.Enter;
try
  while FFirst<>nil do
    begin
    ANext:=FFirst.Next;
    Dispose(FFirst);
    FFirst:=ANext;
    end;
finally
  FLocker.Leave;
end;
end;

constructor TQCSQueue.Create;
begin
inherited;
FLocker:=SyncObjs.TCriticalSection.Create;
end;

destructor TQCSQueue.Destroy;
begin
Clear;
FreeAndNil(FLocker);
  inherited;
end;

function TQCSQueue.Pop: Pointer;
var
  AFirst:PSListHead;
begin
AFirst:=nil;
Result:=nil;
FLocker.Enter;
if FFirst<>nil then
  begin
  AFirst:=FFirst;
  Result:=FFirst.Data;
  FFirst:=AFirst.Next;
  end;
FLocker.Leave;
if AFirst<>nil then
  Dispose(AFirst);
end;

procedure TQCSQueue.Push(AData: Pointer);
var
  ANew:PSListHead;
begin
New(ANew);
ANew.Data:=AData;
FLocker.Enter;
ANew.Next:=FFirst;
FFirst:=ANew;
Inc(FCount);
FLocker.Leave;
end;

{ TTestThread }

constructor TTestThread.Create(AProc: TProcedureObject);
begin
FProc:=AProc;
inherited Create(true);
FreeOnTerminate:=True;
end;

procedure TTestThread.Execute;
begin
FProc;
end;

end.
