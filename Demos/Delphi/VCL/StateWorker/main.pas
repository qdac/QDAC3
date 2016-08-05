unit main;

interface

{ 本示例应群友厦门小叶的请求制作，演示了以下功能：
  1、使用信号来进行状态控制；
  2、在随机延迟作业中再次延迟自身作业；
  3、清理作业
}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QWorker, QString;

type
  TWorkState = (wsStopped, wsPaused, wsRunning);

  TfrmMain = class(TForm)
    btnStart: TButton;
    btnPause: TButton;
    btnStop: TButton;
    lblStatus: TLabel;
    Button1: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FState: TWorkState;
    FPauseSignal: Integer;
    FStartSignal: Integer;
    FStopSignal: Integer;
    procedure SetState(const Value: TWorkState);
  public
    { Public declarations }
    property StartSignal: Integer read FStartSignal;
    property PauseSignal: Integer read FPauseSignal;
    property StopSignal: Integer read FStopSignal;
    property State: TWorkState read FState write SetState;
  end;

var
  frmMain: TfrmMain;

implementation

uses workpanel;
{$R *.dfm}

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
Workers.Signal(PauseSignal);
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
Workers.Signal(StartSignal);
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
Workers.Signal(StopSignal);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
Workers.&For(1,10,procedure(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt)
  begin
  OutputDebugString(PChar(IntToStr(AIndex)));
  end);
Workers.Post(procedure (AJob:PQJob)
  begin
  OutputDebugString(PChar(AJob.ExtData.AsString));
  end,
  10000,
  TQJobExtData.Create('Hello,world'),false,jdfFreeAsObject
  );
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
FStartSignal := Workers.RegisterSignal('Work.Start');
FPauseSignal := Workers.RegisterSignal('Work.Pause');
FStopSignal := Workers.RegisterSignal('Work.Stop');
frmWork := TfrmWork.Create(Application);
ReportMemoryLeaksOnShutdown:=True;
end;

procedure TfrmMain.SetState(const Value: TWorkState);
begin
if FState <> Value then
  begin
  FState := Value;
  btnStart.Enabled := (FState <> wsRunning);
  btnPause.Enabled := (FState = wsRunning);
  btnStop.Enabled := (FState <> wsStopped);
  case FState of
    wsStopped:
      begin
      lblStatus.Caption := '状态:已停止';
      btnStart.Caption := '开始';
      end;
    wsPaused:
      begin
      lblStatus.Caption := '状态:已暂停';
      btnStart.Caption := '恢复';
      end;
    wsRunning:
      begin
      lblStatus.Caption := '状态:运行中';
      btnStart.Caption := '开始';
      end;
  end;
  end;
end;

end.
