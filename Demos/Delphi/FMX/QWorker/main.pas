unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, qworker, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Timer1: TTimer;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FSignalId: Integer;
    procedure DoExtJob(AJob:PQJob);
  public
    { Public declarations }
    procedure DoJobProc(AJob: PQJob);
    procedure DoPostJobDone(AJob: PQJob);
    procedure SignalWaitProc(AJob: PQJob);
    procedure DoTimerProc(AJob: PQJob);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
Workers.Post(DoPostJobDone, nil, true); //
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
Workers.Signal(FSignalId);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
Workers.Signal('MySignal.Start');
end;

procedure TForm1.Button4Click(Sender: TObject);
const
  ACount: Integer = 10000;
var
  I, ARuns: Integer;
  T1: Int64;
  ANeedRuns: Int64;
begin
ARuns := 0;
ANeedRuns := ACount;
T1 := GetTimeStamp;
for I := 0 to ACount - 1 do
  begin
  assert(Workers.Post(DoJobProc, @ARuns) <> 0, 'Post failure');
  end;
while (ARuns < ANeedRuns) do
  TThread.Yield;
T1 := GetTimeStamp - T1;
if T1 > 0 then
  ShowMessage('Time Used=' + IntToStr(T1) + ',Runs=' + IntToStr(ARuns) +
    ',Speed=' + IntToStr(Int64(ARuns) * 10000 div T1))
else
  ShowMessage('Time Used=' + IntToStr(T1) + ',Runs=' + IntToStr(ARuns));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
Workers.Post(DoExtJob, TQJobExtData.Create('Hello,world'), true,
  jdfFreeAsObject);
end;

procedure TForm1.DoExtJob(AJob: PQJob);
begin
ShowMessage(AJob.ExtData.AsString);
end;

procedure TForm1.DoJobProc(AJob: PQJob);
var
  I: Integer;
begin
I := 0;
while I < 1000000 do
  Inc(I);
AtomicIncrement(PInteger(AJob.Data)^);
end;

procedure TForm1.DoPostJobDone(AJob: PQJob);
begin
ShowMessage(Format('作业投寄到执行用时 %g ms', [(AJob.PopTime - AJob.PushTime) / 10]));
end;

procedure TForm1.DoTimerProc(AJob: PQJob);
begin
Label1.Text := IntToStr(AJob.PopTime)+': 定时任务已执行' + IntToStr(AJob.Runs) + '次';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
ReportMemoryLeaksOnShutDown := true;
// 注册一个信号触发函数，以便在触发时执行
FSignalId := Workers.RegisterSignal('MySignal.Start');
Workers.Wait(SignalWaitProc, FSignalId, true);
// 注册一个定时执行任务信号，每0.1秒触发一次
Workers.Post(DoTimerProc, 1000, nil, true);
Caption := 'QWorkerDemo(FMX,' + IntToStr(GetCpuCount) + ' CPU)';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
Workers.Clear(Self);
end;

procedure TForm1.SignalWaitProc(AJob: PQJob);
begin
Label2.Text := Format('信号MySignal.Start已触发 %d次', [AJob.Runs]);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
Button2Click(Sender);
end;

end.
