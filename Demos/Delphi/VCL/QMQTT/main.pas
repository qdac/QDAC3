unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  QMqttClient, QJson, QLog, qdac_ssl_ics;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    leServerHost: TLabeledEdit;
    leServerPort: TLabeledEdit;
    leUserName: TLabeledEdit;
    lePassword: TLabeledEdit;
    Button1: TButton;
    Panel5: TPanel;
    btnSubscribe: TButton;
    Panel6: TPanel;
    Panel7: TPanel;
    btnPublish: TButton;
    Splitter1: TSplitter;
    Label1: TLabel;
    edtSubscribeTopic: TEdit;
    Label2: TLabel;
    edtPublishTopic: TEdit;
    Label3: TLabel;
    edtMessage: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel8: TPanel;
    Label4: TLabel;
    cbxQoSLevel: TComboBox;
    Panel9: TPanel;
    Label5: TLabel;
    cbxRecvQoSLevel: TComboBox;
    tmSend: TTimer;
    chkAutoSend: TCheckBox;
    chkAutoClearLog: TCheckBox;
    Label6: TLabel;
    edtClientId: TEdit;
    tmStatics: TTimer;
    pnlStatus: TPanel;
    chkSSL: TCheckBox;
    cbxVersion: TComboBox;
    btnUnsubscribe: TButton;
    Panel10: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure btnSubscribeClick(Sender: TObject);
    procedure btnPublishClick(Sender: TObject);
    procedure tmSendTimer(Sender: TObject);
    procedure chkAutoSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmStaticsTimer(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure chkSSLClick(Sender: TObject);
    procedure btnUnsubscribeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FClient: TQMQTTMessageClient;
    procedure DoClientConnecting(ASender: TQMQTTMessageClient);
    procedure DoClientConnected(ASender: TQMQTTMessageClient);
    procedure DoClientDisconnected(ASender: TQMQTTMessageClient);
    procedure DoClientError(ASender: TQMQTTMessageClient; const AErrorCode: Integer; const AErrorMsg: String);
    procedure DoSubscribeDone(ASender: TQMQTTMessageClient; const AResults: TQMQTTSubscribeResults);
    procedure DoBeforePublish(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoAfterPublished(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoRecvTopic(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoStdTopicTest(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoPatternTopicTest(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoRegexTopicTest(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoMultiDispatch1(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoMultiDispatch2(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoBeforeUnsubscribe(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
    procedure DoAfterUnsubscribe(ASender: TQMQTTMessageClient; const ATopic: String);
    procedure SaveSettings;
    procedure LoadSettings;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring;
{$R *.dfm}

procedure TForm1.btnPublishClick(Sender: TObject);
begin
  if Assigned(FClient) then
    FClient.Publish(edtPublishTopic.Text, edtMessage.Text, TQMQTTQoSLevel(cbxQoSLevel.ItemIndex));
end;

procedure TForm1.btnSubscribeClick(Sender: TObject);
var
  ATopics: TArray<String>;
  S: String;
  p: PChar;
  C: Integer;
begin
  if Assigned(FClient) then
  begin
    SetLength(ATopics, 4);
    S := edtSubscribeTopic.Text;
    p := PChar(S);
    C := 0;
    while p^ <> #0 do
    begin
      ATopics[C] := DecodeTokenW(p, ',', #0, true);
      Inc(C);
    end;
    SetLength(ATopics, C);
    FClient.Subscribe(ATopics, TQMQTTQoSLevel(cbxRecvQoSLevel.ItemIndex));
  end;
end;

procedure TForm1.btnUnsubscribeClick(Sender: TObject);
var
  ATopics: TArray<String>;
  S: String;
  p: PChar;
  C: Integer;
begin
  if Assigned(FClient) then
  begin
    SetLength(ATopics, 4);
    S := edtSubscribeTopic.Text;
    p := PChar(S);
    C := 0;
    while p^ <> #0 do
    begin
      ATopics[C] := DecodeTokenW(p, ',', #0, true);
      Inc(C);
    end;
    SetLength(ATopics, C);
    FClient.Unsubscribe(ATopics);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Button1.Caption = '连接' then
  begin
    if not Assigned(FClient) then
      FClient := TQMQTTMessageClient.Create(Self);
    FClient.Stop;
    // ClientId 如果不指定，则会自动生成一个随机的ClientId，建议指定
    FClient.ClientId := edtClientId.Text;
    FClient.ServerHost := leServerHost.Text;
    FClient.ServerPort := StrToIntDef(leServerPort.Text, 1883);
    FClient.UserName := leUserName.Text;
    FClient.Password := lePassword.Text;
    FClient.UseSSL := chkSSL.Checked;
    // if FClient.UseSSL then
    // FClient.SSLManager.LoadCAFiles('root.pem');
    if cbxVersion.ItemIndex = 1 then
    begin
      FClient.ProtocolVersion := pv5_0;
      FClient.ConnectProps.AsInt[MP_NEED_PROBLEM_INFO] := 1;
    end
    else
      FClient.ProtocolVersion := pv3_1_1;
    FClient.PeekInterval := 15;
    // 5.0 test
    //
    // 如果不关心中间的连接过程，这些事件不需要管
    FClient.BeforeConnect := DoClientConnecting;
    FClient.AfterConnected := DoClientConnected;
    FClient.AfterDisconnected := DoClientDisconnected;
    FClient.AfterSubscribed := DoSubscribeDone;
    FClient.BeforePublish := DoBeforePublish;
    FClient.AfterPublished := DoAfterPublished;
    FClient.BeforeUnsubscribe := DoBeforeUnsubscribe;
    FClient.AfterUnsubscribed := DoAfterUnsubscribe;
    FClient.OnError := DoClientError;
    FClient.OnRecvTopic := DoRecvTopic;
    // 在这里添加自己的订阅，由于这个是要发送到服务器端的，所以不支持正则，但可以使用主题过滤表达式匹配
    // 请参考：http://itindex.net/detail/58722-mqtt-topic-%E9%80%9A%E9%85%8D%E7%AC%A6
    FClient.Subscribe(['/Topic/Dispatch'], qlMax1);
    // 主题的派发处理，支持完全匹配（默认）、主题过滤表达式和正则表达式，当收到相应的主题时会调用相应的处理函数
    FClient.RegisterDispatch('/Topic/Dispatch', DoStdTopicTest);
    FClient.RegisterDispatch('/+/Dispatch', DoPatternTopicTest, mtPattern);
    FClient.RegisterDispatch('/Topic\d', DoRegexTopicTest, mtRegex);
    FClient.RegisterDispatch('/Topic1', DoMultiDispatch1, mtFull);
    FClient.RegisterDispatch('/Topic1', DoMultiDispatch2, mtFull);
    // 启动后台工作
    FClient.Start;
  end
  else
  begin
    FClient.Stop;
    Button1.Caption := '连接';
  end;
end;

procedure TForm1.chkAutoSendClick(Sender: TObject);
begin
  tmSend.Enabled := chkAutoSend.Checked;
end;

procedure TForm1.chkSSLClick(Sender: TObject);
begin
  if chkSSL.Checked then
    leServerPort.Text := '8883'
  else
    leServerPort.Text := '1883';
end;

procedure TForm1.DoAfterPublished(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo2.Lines.Add('发布 ' + ATopic + ' ID=' + IntToStr(AReq.TopicId) + ',大小:' + IntToStr(AReq.Size) + ' 完成');
end;

procedure TForm1.DoAfterUnsubscribe(ASender: TQMQTTMessageClient; const ATopic: String);
begin
  Memo1.Lines.Add('订阅 ' + ATopic + ' 已取消');
end;

procedure TForm1.DoBeforePublish(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo2.Lines.Add('正在发布 ' + ATopic + ' ID=' + IntToStr(AReq.TopicId) + ',大小:' + IntToStr(AReq.Size) + ' ...');
end;

procedure TForm1.DoBeforeUnsubscribe(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('正在取消订阅 ' + ATopic + '...');
end;

procedure TForm1.DoClientConnected(ASender: TQMQTTMessageClient);
begin
  Memo1.Lines.Add(ASender.ServerHost + ':' + IntToStr(ASender.ServerPort) + ' 连接成功.');
  Button1.Caption := '断开';
end;

procedure TForm1.DoClientConnecting(ASender: TQMQTTMessageClient);
begin
  Memo1.Lines.Add('正在连接 ' + ASender.ServerHost + ':' + IntToStr(ASender.ServerPort));
end;

procedure TForm1.DoClientDisconnected(ASender: TQMQTTMessageClient);
begin
  Memo1.Lines.Add('连接 ' + ASender.ServerHost + ':' + IntToStr(ASender.ServerPort) + '已断开');
end;

procedure TForm1.DoClientError(ASender: TQMQTTMessageClient; const AErrorCode: Integer; const AErrorMsg: String);
begin
  Memo1.Lines.Add('错误:' + AErrorMsg + ',代码:' + IntToStr(AErrorCode));
end;

procedure TForm1.DoMultiDispatch1(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('Dispatch1 接收到Topic:' + ATopic + SLineBreak + '  ID:' + IntToStr(AReq.TopicId) + SLineBreak + '  内容(' +
    RollupSize(AReq.TopicContentSize) + '):' + AReq.TopicText);
end;

procedure TForm1.DoMultiDispatch2(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('Dispatch2 接收到Topic:' + ATopic + SLineBreak + '  ID:' + IntToStr(AReq.TopicId) + SLineBreak + '  内容(' +
    RollupSize(AReq.TopicContentSize) + '):' + AReq.TopicText);
end;

procedure TForm1.DoPatternTopicTest(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('通过主题词模式匹配派发:' + ATopic + SLineBreak + '  ID:' + IntToStr(AReq.TopicId) + SLineBreak + '  内容(' +
    RollupSize(AReq.TopicContentSize) + '):' + AReq.TopicText);
end;

procedure TForm1.DoRecvTopic(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('接收到Topic:' + ATopic + SLineBreak + '  ID:' + IntToStr(AReq.TopicId) + SLineBreak + '  内容(' +
    RollupSize(AReq.TopicContentSize) + '):' + AReq.TopicText);
end;

procedure TForm1.DoRegexTopicTest(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('通过主题词正则匹配派发:' + ATopic + SLineBreak + '  ID:' + IntToStr(AReq.TopicId) + SLineBreak + '  内容(' +
    RollupSize(AReq.TopicContentSize) + '):' + AReq.TopicText);
end;

procedure TForm1.DoStdTopicTest(ASender: TQMQTTMessageClient; const ATopic: String; const AReq: PQMQTTMessage);
begin
  Memo1.Lines.Add('通过主题词派发:' + ATopic + SLineBreak + '  ID:' + IntToStr(AReq.TopicId) + SLineBreak + '  内容(' +
    RollupSize(AReq.TopicContentSize) + '):' + AReq.TopicText);
end;

procedure TForm1.DoSubscribeDone(ASender: TQMQTTMessageClient; const AResults: TQMQTTSubscribeResults);
var
  I: Integer;
begin
  for I := 0 to High(AResults) do
  begin
    if AResults[I].Success then
      Memo1.Lines.Add(AResults[I].Topic + ' -> QoS ' + IntToStr(Ord(AResults[I].Qos)) + ' 订阅完成')
    else
      Memo1.Lines.Add(AResults[I].Topic + ' -> QoS ' + IntToStr(Ord(AResults[I].Qos)) + ' 订阅失败');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetDefaultLogFile('', 2097152, false);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FClient) then
    FClient.AfterDisconnected := nil;
end;

procedure TForm1.LoadSettings;
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    if FileExists('mqtt.config') then
      AJson.LoadFromFile('mqtt.config');
    leServerHost.Text := AJson.ValueByName('host', '');
    leServerPort.Text := AJson.ValueByName('port', '1883');
    leUserName.Text := AJson.ValueByName('user', '');
    lePassword.Text := AJson.ValueByName('pass', '');
  finally
    FreeAndNil(AJson);
  end;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  if Assigned(FClient) then
    FClient.Publish('/Topic1', StringReplicateW('0', 16848), qlMax1);
end;

procedure TForm1.SaveSettings;
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    AJson.Add('host').AsString := leServerHost.Text;
    AJson.Add('port').AsString := leServerPort.Text;
    AJson.Add('user').AsString := leUserName.Text;
    AJson.Add('pass').AsString := lePassword.Text;
    AJson.SaveToFile('mqtt.config');
  finally
    FreeAndNil(AJson);
  end;

end;

procedure TForm1.tmSendTimer(Sender: TObject);
begin
  btnPublishClick(Sender);
  if chkAutoClearLog.Checked then
  begin
    if tmSend.Tag = 0 then
      tmSend.Tag := GetTickCount
    else if GetTickCount - Cardinal(tmSend.Tag) > 15000 then
    begin
      tmSend.Tag := GetTickCount;
      Memo1.Text := '';
      Memo2.Text := '';
    end;
  end;
end;

procedure TForm1.tmStaticsTimer(Sender: TObject);
begin
  if Assigned(FClient) then
    pnlStatus.Caption := '收到主题:' + IntToStr(FClient.RecvTopics) + ' 发布主题:' + IntToStr(FClient.SentTopics);
end;

end.
