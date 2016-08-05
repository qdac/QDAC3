unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Engine,
  FMXTee.Series, FMXTee.Procs, FMXTee.Chart, FMX.StdCtrls,sWZ_SysCoreUnit,syncobjs,
  Diagnostics, FMX.ListView.Types, FMX.ListView, FMX.Layouts, FMX.Memo;

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
    Button1: TButton;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Memo1: TMemo;
    Button2: TButton;
    Chart: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ChartClick(Sender: TObject);
  private
    { Private declarations }
    FCount:Integer;
    FSimpleQueue:TQSimpleQueue;
    FCsQueue:TQCSQueue;
    FSwzQueue: TSwzSingleListWithLock;
    procedure DoLockTest0;
    procedure DoLockTest1;
    procedure DoLockTest2;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  function AtomicAnd(var Dest:Integer;const AMask:Integer): Integer;inline;
  function AtomicOr(var Dest:Integer;const AMask:Integer):Integer;inline;
implementation
const
  LoopCount:Integer=1000000;//100万次
{$R *.fmx}
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
until AtomicCmpExchange(Dest,i,Result)=Result;
end;

constructor TQSimpleLock.Create;
begin
inherited;
FFlags:=0;
end;

procedure TQSimpleLock.Enter;
begin
while (AtomicOr(FFlags,$01) and $01)<>0 do
  TThread.Yield;
end;

procedure TQSimpleLock.Leave;
begin
AtomicAnd(FFlags,Integer($FFFFFFFE));
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
FLocker.Enter;
if FFirst<>nil then
  begin
  AFirst:=FFirst;
  Result:=FFirst.Data;
  FFirst:=AFirst.Next;
  end
else
  Result:=nil;
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
procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  T1,T2,T3:Cardinal;
  J: Integer;
  st:TThread.TSystemTimes;
  AWatch:TStopWatch;
begin
Button1.Enabled:=False;
FSimpleQueue:=TQSimpleQueue.Create;
FCsQueue:=TQCSQueue.Create;
FSwzQueue := TSwzSingleListWithLock.Create;
Series1.Clear;
Series1.LinePen.Width:=2;
Series1.LinePen.Color:=TAlphaColorRec.Red;
Series1.Legend.Color:=Series1.LinePen.Color;
Series2.Clear;
Series2.LinePen.Width:=2;
Series2.LinePen.Color:=TAlphaColorRec.Green;
Series2.Legend.Color:=Series2.LinePen.Color;
Series3.Clear;
Series3.LinePen.Width:=2;
Series3.LinePen.Color:=TAlphaColorRec.Blue;
Series3.Legend.Color:=Series3.LinePen.Color;
AWatch:=TStopWatch.Create;
for J := 2 to 20 do
  begin
  Memo1.Lines.Add('线程数:'+IntToStr(J));
  Application.ProcessMessages;
  TThread.GetSystemTimes(st);
  AWatch.Reset;
  AWatch.Start;
  FCount:=0;
  for I := 0 to J-1 do
    TThread.CreateAnonymousThread(DoLockTest1).Resume;
  while FCount<LoopCount do
    TThread.Yield;
  AWatch.Stop;
  T1:=AWatch.ElapsedMilliseconds;
  Sleep(50);
  Series1.AddXY(J,T1);
  Memo1.Lines.Add('SimpleLock 测试完成，用时'+IntToStr(T1)+'ms,CPU平均占用率:'+IntToStr(TThread.GetCPUUsage(st))+'%');
  Application.ProcessMessages;
  TThread.GetSystemTimes(st);
  AWatch.Reset;
  AWatch.Start;
  FCount:=0;
  for I := 0 to J-1 do
    TThread.CreateAnonymousThread(DoLockTest0).Resume;
  while FCount<LoopCount do
    TThread.Yield;
  AWatch.Stop;
  T2:=AWatch.ElapsedMilliseconds;
  Sleep(50);
  Memo1.Lines.Add('SwzSingleList 测试完成，用时'+IntToStr(T2)+'ms,CPU平均占用率:'+IntToStr(TThread.GetCPUUsage(st))+'%');
  Series3.AddXY(J,T2);
  TThread.GetSystemTimes(st);
  AWatch.Reset;
  AWatch.Start;
  FCount:=0;
  for I := 0 to J-1 do
    TThread.CreateAnonymousThread(DoLockTest2).Resume;
  while FCount<LoopCount do
    TThread.Yield;
  AWatch.Stop;
  T2:=AWatch.ElapsedMilliseconds;
  Sleep(50);
  Memo1.Lines.Add('CriticalSection 测试完成，用时'+IntToStr(T2)+'ms,CPU平均占用率:'+IntToStr(TThread.GetCPUUsage(st))+'%');
  Series2.AddXY(J,T2);
  Application.ProcessMessages;
  Sleep(50);
  end;
FSimpleQueue.Free;
FCsQueue.Free;
FSwzQueue.Free;
Button1.Enabled:=True;
ShowMessage('测试完成');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
Memo1.Visible:=True;
Chart1.Visible:=False;
end;

procedure TForm1.ChartClick(Sender: TObject);
begin
Memo1.Visible:=False;
Chart1.Visible:=True;
end;

procedure TForm1.DoLockTest0;
begin
repeat
    FSwzQueue.Push(nil);
    FSwzQueue.Pop;
  until AtomicIncrement(FCount)>=LoopCount;
end;

procedure TForm1.DoLockTest1;
begin
repeat
  FSimpleQueue.Push(nil);
  FSimpleQueue.Pop;
until AtomicIncrement(FCount)>=LoopCount;
end;

procedure TForm1.DoLockTest2;
begin
repeat
  FCsQueue.Push(nil);
  FCsQueue.Pop;
until AtomicIncrement(FCount)>=LoopCount;
end;

end.
