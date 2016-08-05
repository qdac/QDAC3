unit workpanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QWorker;

type
  TfrmWork = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FRuns: Integer;
    procedure DoStartWork(AJob: PQJob);
    procedure DoStopWork(AJob: PQJob);
    procedure DoPauseWork(AJob: PQJob);
    procedure DoDelayTimer(AJob: PQJob);
  public
    { Public declarations }
  end;

var
  frmWork: TfrmWork;

implementation

uses main;
{$R *.dfm}

procedure TfrmWork.DoDelayTimer(AJob: PQJob);
var
  ANextDelay: Int64;
begin
ANextDelay := random(1000);
Inc(FRuns);
Label1.Caption := '运行次数:' + IntToStr(FRuns) + '次，末次延迟:' +
  IntToStr((AJob.PopTime - AJob.PushTime) div 10) + 'ms,下一延迟:' +
  IntToStr(ANextDelay) + 'ms';
if (frmMain.State = wsRunning) or (not AJob.IsTerminated) then
  Workers.Delay(DoDelayTimer, ANextDelay * Q1MillSecond, nil,True);
end;

procedure TfrmWork.DoPauseWork(AJob: PQJob);
begin
Workers.Clear(DoDelayTimer, nil);
frmMain.State := wsPaused;
end;

procedure TfrmWork.DoStartWork(AJob: PQJob);
begin
Workers.Delay(DoDelayTimer, random(1000), nil,True);
if frmMain.State <> wsPaused then
  begin
  SetBounds(Screen.WorkAreaRect.Right - Width, Screen.WorkAreaTop,
    Width, Height);
  Show;
  end;
frmMain.State := wsRunning;
end;

procedure TfrmWork.DoStopWork(AJob: PQJob);
begin
Workers.Clear(DoDelayTimer, nil);
FRuns := 0;
frmMain.State := wsStopped;
Hide;
end;

procedure TfrmWork.FormCreate(Sender: TObject);
begin
Workers.Wait(DoStartWork, frmMain.StartSignal, True);
Workers.Wait(DoPauseWork, frmMain.PauseSignal, True);
Workers.Wait(DoStopWork, frmMain.StopSignal, True);
end;

procedure TfrmWork.FormDestroy(Sender: TObject);
begin
Workers.Clear(Self);
end;

end.
