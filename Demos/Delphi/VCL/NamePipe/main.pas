unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QString,QSimplePool, QNamePipe,
  QWorker;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Button4: TButton;
    Edit2: TEdit;
    Memo1: TMemo;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FServer: TQNamedPipeServer;
    FClient: TQNamedPipeClient;
    procedure DoLog(AJob: PQJob);
    procedure DoRecvFromServer(ASender: TQNamedPipe; const AData: PByte;const ASize:Integer);
    procedure DoEnterAlertMode(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
FServer.Active := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
FClient.Active := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
if FServer.ClientCount > 0 then
  FServer.Clients[0].Post(Edit1.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
FClient.Post(Edit2.Text);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  APool: TQSimplePool;
  p1, p2: PByte;
begin
APool := TQSimplePool.Create(100, 50);
p1 := APool.Pop;
FillChar(p1^, 50, 'A');
APool.Push(p1);
p2 := APool.Pop;
assert(p1 = p2);
APool.Push(p2);
APool.Free;
end;

procedure TForm1.DoEnterAlertMode(Sender: TObject; var Done: Boolean);
begin
SleepEx(0,True);
Done:=True;
end;

procedure TForm1.DoLog(AJob: PQJob);
begin
Memo1.Lines.Add(AJob.ExtData.AsString);
end;

procedure TForm1.DoRecvFromServer(ASender: TQNamedPipe; const AData: PByte;const ASize:Integer);
var
  S: String;
begin
S := Format('来自 %s 的进程 %d 的会话 %d:'#13#10'%s',
  [TQNamedPipeClient(ASender).ClientComputerName, TQNamedPipeClient(ASender)
  .ClientProcessId, TQNamedPipeClient(ASender).ClientSessionId,
  StrDupX(PQCharW(AData), ASize shr 1)]);
Workers.Post(DoLog, TQJobExtData.Create(S), True, jdfFreeAsObject);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FServer := TQNamedPipeServer.Create('\\.\pipe\QNamedPipeTest');
FClient := TQNamedPipeClient.Create('\\.\pipe\QNamedPipeTest');
FClient.AfterRecvData := DoRecvFromServer;
Application.OnIdle:=DoEnterAlertMode;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
FServer.Free;
FClient.Free;
end;

end.
