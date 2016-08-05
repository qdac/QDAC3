unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.SyncObjs, QString,
  qsocket_sharemem,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses QWorker;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AServer, AClient: TQShareMemConnection;
  AGroup: TQJobGroup;
begin
  // 建立一个共享内存的客户/服务对
  AServer := TQShareMemConnection.Create;
  AClient := TQShareMemConnection.Create;
  // 设定本地地址，连接到对方的本地地址
  AServer.ConnectTo(AClient.LocalAddr);
  AClient.ConnectTo(AServer.LocalAddr);
  // 作业组在两个线程中执行，一个负责写入，一个负责读取，完成数据交换
  AGroup := TQJobGroup.Create(False);
  AGroup.Prepare;
  // 服务器端作业写入数据
  AGroup.Add(
    procedure(AJob: PQJob)
    var
      AStream: TMemoryStream;
      AConnection: TQShareMemConnection;
    begin
      AConnection := AJob.Data;
      AStream := TMemoryStream.Create;
      AStream.LoadFromFile('C:\Windows\comsetup.log');
      AConnection.WriteStream.WriteData(AStream.Size);
      AConnection.WriteStream.CopyFrom(AStream, 0);
      FreeAndNil(AStream);
    end, AServer);
  // 客户端作业读取数据
  AGroup.Add(
    procedure(AJob: PQJob)
    var
      AStream: TMemoryStream;
      ASize: Int64;
      AConnection: TQShareMemConnection;
    begin
      Sleep(0);
      AConnection := AJob.Data;
      AStream := TMemoryStream.Create;
      if AConnection.WaitForData(10000) = wrSignaled then
      begin
        if AConnection.ReadStream.ReadData(ASize) = SizeOf(ASize) then
        begin
          while AStream.Size < ASize do
          begin
            if AConnection.WaitForData(1000) = wrSignaled then
              AStream.CopyFrom(AConnection.ReadStream, 0)
            else//如果等待1秒还没有新数据进入，则退出
              break;
          end;
        end;
      end;
      AStream.Position := 0;
      RunInMainThread(
        procedure
        begin
          Memo1.Text := LoadTextW(AStream);
        end);
      FreeAndNil(AStream);
    end, AClient);
  AGroup.Run();
  AGroup.MsgWaitFor();
  FreeAndNil(AClient);
  FreeAndNil(AServer);
end;

end.
