unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPServer, ExtCtrls, VirtualTrees, qstring, Buttons, StdCtrls,
  IdGlobal, IdSocketHandle;

type
  PQSyslog = ^TQSyslog;

  TQSyslog = record
    Msg: String;
    Host: String;
    Port: Word;
  end;

  PQParsedLog = ^TQParsedLog;

  TQParsedLog = record
    Level: Integer;
    LogTime: TDateTime;
    HostName: String;
    ThreadId: Integer;
    Text: String;
  end;

  TQLogHost = class
  private
    function GetItems(AIndex: Integer): PQParsedLog;
    function GetCount: Integer;
  protected
    FHost: QStringW;
    FCaches: TList;
    FLogFile: TFileStream;
    FStartPos: Int64; // 索引
  public
    constructor Create(AHost: QStringW); overload;
    destructor Destroy; override;
    procedure Add(S: QStringW);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: PQParsedLog read GetItems;
  end;

  TForm1 = class(TForm)
    vstHosts: TVirtualStringTree;
    Splitter1: TSplitter;
    usLogServer: TIdUDPServer;
    Panel1: TPanel;
    pnlLogDetail: TPanel;
    vstLogs: TVirtualStringTree;
    Splitter2: TSplitter;
    mmLogText: TMemo;
    pnlDetailTitle: TPanel;
    sbSize: TSpeedButton;
    sbClear: TSpeedButton;
    chkAutoScroll: TCheckBox;
    Panel2: TPanel;
    OpenDialog1: TOpenDialog;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure sbSizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlLogDetailResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstHostsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText:
{$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
    procedure vstLogsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText:
{$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
    procedure vstHostsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vstLogsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure sbClearClick(Sender: TObject);
{$IFDEF UNICODE}
    procedure usLogServerUDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure vstLogsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstLogsBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vstHostsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
{$ELSE}
    procedure usLogServerUDPRead(Sender: TObject; AData: TBytes;
      ABinding: TIdSocketHandle);
{$ENDIF}
  private
    FOriginDetailHeight: Integer;
    FList: TStringList;
    { Private declarations }
  protected
    procedure WMLogRecv(var AMsg: TMessage); message WM_APP;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dateutils;
{$R *.dfm}

type
  PLocateValue = ^TLocateValue;

  TLocateValue = record
    X: Single;
    Y: Single;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pnlLogDetail.Constraints.MinHeight := pnlDetailTitle.Height;
  try
    usLogServer.Active := True;
    FList := TStringList.Create;
  except
    Application.MessageBox('514端口已经被占用，请退出其它程序后重试。', '错误',
      MB_OK or MB_ICONSTOP);
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  while FList.Count > 0 do
  begin
    FList.Objects[0].Free;
    FList.Delete(0);
  end;
  FList.Free;
end;

procedure TForm1.pnlLogDetailResize(Sender: TObject);
begin
  if pnlLogDetail.ClientHeight = pnlDetailTitle.Height then
  begin
    sbSize.Caption := '2';
    FOriginDetailHeight := 150;
  end
  else
    sbSize.Caption := '0';
end;

procedure TForm1.sbClearClick(Sender: TObject);
begin
  if vstHosts.FocusedNode <> nil then
  begin
    TQLogHost(FList.Objects[vstHosts.FocusedNode.Index]).Clear;
    vstLogs.RootNodeCount := 0;
  end;
end;

procedure TForm1.sbSizeClick(Sender: TObject);
begin
  if sbSize.Caption = '0' then // 要求最小化
  begin
    sbSize.Caption := '2';
    FOriginDetailHeight := pnlLogDetail.ClientHeight;
    pnlLogDetail.ClientHeight := pnlDetailTitle.Height;
  end
  else
  begin
    sbSize.Caption := '0';
    pnlLogDetail.ClientHeight := FOriginDetailHeight;
  end;
end;

procedure TForm1.usLogServerUDPRead
{$IFDEF UNICODE}(AThread: TIdUDPListenerThread; const AData: TIdBytes;
  ABinding: TIdSocketHandle);
{$ELSE}
  (Sender: TObject; AData: TBytes; ABinding: TIdSocketHandle);
{$ENDIF}

var
  AMsg: PQSyslog;
  AEncode: TTextEncoding;
  AByBom: Boolean;
begin
  AEncode := DetectTextEncoding(@AData[0], Length(AData), AByBom);
  New(AMsg);
  if AEncode = teAnsi then
    AMsg.Msg := qstring.AnsiDecode(@AData[0], Length(AData))
  else if AEncode = teUtf8 then
    AMsg.Msg := qstring.Utf8Decode(@AData[0], Length(AData))
  else if AEncode = teUnicode16LE then
    AMsg.Msg := StrDupX(PWideChar(@AData[0]), Length(AData) shr 1)
  else
  begin
    ExchangeByteOrder(@AData[0], Length(AData));
    AMsg.Msg := StrDupX(@AData[0], Length(AData) shr 1);
  end;
  AMsg.Host := ABinding.PeerIP;
  AMsg.Port := ABinding.PeerPort;
  PostMessage(Handle, WM_APP, WPARAM(AMsg), 0);
end;

procedure TForm1.vstHostsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AHelper: TQLogHost;
begin
  if Node <> nil then
  begin
    AHelper := FList.Objects[Node.Index] as TQLogHost;
    vstLogs.RootNodeCount := AHelper.Count;
  end;
end;

procedure TForm1.vstHostsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText:
{$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
begin
  CellText := FList[Node.Index];
end;

procedure TForm1.vstHostsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AHelper: TQLogHost;
  AItem: PQParsedLog;
begin
  if Assigned(vstHosts.FocusedNode) then
  begin
    AHelper := FList.Objects[vstHosts.FocusedNode.Index] as TQLogHost;
    case Key of
      VK_DELETE:
        begin
          FList.Delete(vstHosts.FocusedNode.Index);
          vstHosts.DeleteNode(vstHosts.FocusedNode);
          FreeAndNil(AHelper);
          vstLogs.RootNodeCount := 0;
          mmLogText.Clear;
        end;
    end;
  end;
end;

procedure TForm1.vstLogsBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  AHelper: TQLogHost;
  AItem: PQParsedLog;
begin
  if Assigned(vstHosts.FocusedNode) and Assigned(Node) and
    (not(vsSelected in Node.States)) then
  begin
    AHelper := FList.Objects[vstHosts.FocusedNode.Index] as TQLogHost;
    AItem := AHelper.Items[Node.Index];
    case AItem.Level of
      0, 2, 3:
        ItemColor := clBlack;
      1, 4, 5:
        ItemColor := clMaroon;
    end;
  end;
end;

procedure TForm1.vstLogsDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  AHelper: TQLogHost;
  AItem: PQParsedLog;
begin
  if Assigned(vstHosts.FocusedNode) and Assigned(Node) and
    (not(vsSelected in Node.States)) then
  begin
    AHelper := FList.Objects[vstHosts.FocusedNode.Index] as TQLogHost;
    AItem := AHelper.Items[Node.Index];
    case AItem.Level of
      0, 2, 3:
        TargetCanvas.Font.Color := clRed;
      1, 4, 5:
        TargetCanvas.Font.Color := clYellow;
    end;
  end;
end;

procedure TForm1.vstLogsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Node <> nil then
    mmLogText.Text := vstLogs.Text[Node, 5]
  else
    mmLogText.Text := '';
end;

procedure TForm1.vstLogsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText:
{$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
var
  AHelper: TQLogHost;
  AItem: PQParsedLog;
const
  LogLevels: array [0 .. 7] of String = ('紧急', '警报', '严重', '错误', '警告', '注意',
    '提示', '调试');
begin
  if Assigned(vstHosts.FocusedNode) and Assigned(Node) then
  begin
    AHelper := FList.Objects[vstHosts.FocusedNode.Index] as TQLogHost;
    AItem := AHelper.Items[Node.Index];
    case Column of
      0: // SeqNo
        CellText := IntToStr(Node.Index);
      1: // Level
        begin
          if AItem.Level in [0 .. 7] then
            CellText := LogLevels[AItem.Level]
          else
            CellText := IntToStr(AItem.Level);
        end;
      2: // DateTime
        CellText := FormatDateTime('mm-dd hh:nn:ss', AItem.LogTime);
      3: // HostName
        CellText := AItem.HostName;
      4: // ThreadId
        CellText := IntToStr(AItem.ThreadId);
      5: // Message
        CellText := AItem.Text
    else
      CellText := '';
    end;
  end
  else
    CellText := '';
end;

procedure TForm1.WMLogRecv(var AMsg: TMessage);
var
  ALog: PQSyslog;
  AIndex: Integer;
  AHelper: TQLogHost;
begin
  ALog := PQSyslog(AMsg.WPARAM);
  if EndWithW(ALog.Msg, '~Who~Is~QLog~SysD~Server~', False) then
  begin
    // 接收到QLog的查询日志服务器消息
    usLogServer.Send(ALog.Host, ALog.Port, '~I~am~QLog~SysD~Server~');
  end;
  ALog.Host := ALog.Host + ':' + IntToStr(ALog.Port);
  AIndex := FList.IndexOf(ALog.Host);
  if AIndex = -1 then
  begin
    AHelper := TQLogHost.Create(ALog.Host);
    FList.AddObject(ALog.Host, AHelper);
    vstHosts.RootNodeCount := FList.Count;
    if FList.Count = 1 then
    begin
      vstHosts.FocusedNode := vstHosts.GetFirst();
      AIndex := 0;
    end;
  end
  else
    AHelper := FList.Objects[AIndex] as TQLogHost;
  AHelper.Add(ALog.Msg);
  if Assigned(vstHosts.FocusedNode) and
    (Integer(vstHosts.FocusedNode.Index) = AIndex) then
  begin
    vstLogs.RootNodeCount := AHelper.Count;
    if chkAutoScroll.Checked then
      vstLogs.FocusedNode := vstLogs.GetLast();
  end;
  Dispose(ALog);
end;

{ TQLogHost }

procedure TQLogHost.Add(S: QStringW);
var
  AItem: PQParsedLog;
  V: Int64;
  p, ps: PQCharW;
const
  SpaceChars: PWideChar = ' '#9#10#13;
  procedure ParseSyslogTime;
  var
    I, Y, M, D, H, N, S: Integer;
  const
    MonthNames: array [0 .. 11] of QStringW = ('Jan', 'Feb', 'Mar', 'Apr',
      'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    Comma: PWideChar = ',';
    Digits: PWideChar = '0123456789';
  begin
    SkipSpaceW(p);
    Y := YearOf(Now);
    M := 0;
    for I := 0 to 11 do
    begin
      if StartWithW(p, PQCharW(MonthNames[I]), True) then
      begin
        M := I + 1;
        Inc(p, Length(MonthNames[I]));
        SkipSpaceW(p);
        Break;
      end;
    end;
    ParseInt(p, V);
    D := V;
    SkipSpaceW(p);
    ParseInt(p, V);
    H := V;
    Inc(p);
    ParseInt(p, V);
    N := V;
    Inc(p);
    ParseInt(p, V);
    S := V;
    try
      AItem.LogTime := EncodeDateTime(Y, M, D, H, N, S, 0);
    except
      AItem.LogTime := Now;
    end;
    SkipSpaceW(p);
  end;

  procedure SkipReservedLevels;
  const
    ReservedLevels: array [0 .. 7] of QStringW = ('[EMG]', '[ALERT]', '[FATAL]',
      '[ERROR]', '[WARN]', '[HINT]', '[MSG]', '[DEBUG]');
  var
    I: Integer;
  begin
    for I := 0 to 7 do
    begin
      if StartWithW(p, PQCharW(ReservedLevels[I]), True) then
      begin
        Inc(p, Length(ReservedLevels[I]));
        Break;
      end;
    end;
  end;

begin
  New(AItem);
  FCaches.Add(AItem);
  p := PQCharW(S);
  if p^ = '<' then
  begin
    // <Level>
    Inc(p);
    ParseInt(p, V);
    if p^ = '>' then
      AItem.Level := (V and $07);
    Inc(p);
  end;
  // 解析时间
  ParseSyslogTime;
  // 解析主机名
  ps := p;
  SkipUntilW(p, SpaceChars);
  AItem.HostName := StrDupX(ps, p - ps);
  SkipSpaceW(p);
  // 解析线程编码
  if p^ = '[' then
  begin
    Inc(p);
    ParseInt(p, V);
    AItem.ThreadId := V;
    if p^ = ']' then
      Inc(p);
    // 解析日志级别
  end;
  // 跳过为了使用其它查看器而加入的[Level]文本
  SkipReservedLevels;
  AItem.Text := StrDupW(p);
end;

procedure TQLogHost.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(Items[I]);
  FCaches.Clear;
end;

constructor TQLogHost.Create(AHost: QStringW);
begin
  inherited Create;
  FHost := AHost;
  FCaches := TList.Create;
end;

destructor TQLogHost.Destroy;
begin
  Clear;
  FCaches.Free;
  inherited;
end;

function TQLogHost.GetCount: Integer;
begin
  Result := FCaches.Count;
end;

function TQLogHost.GetItems(AIndex: Integer): PQParsedLog;
begin
  Result := FCaches[AIndex];
end;

end.
