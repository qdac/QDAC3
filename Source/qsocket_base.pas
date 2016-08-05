unit qsocket_base;

interface

uses classes, sysutils, qstring, qworker, syncobjs;

type
  TQBaseConnection = class;

  TQConnectionNotifyEvent = procedure(ASender: TQBaseConnection) of object;
  TQConnectionCallback = procedure(ASender: TQBaseConnection; AParam: IntPtr)
    of object;

  TQBaseConnection = class
  protected
    FLocalAddr, FRemoteAddr: QStringW;
    FTag: IntPtr;
    FConnected: Boolean;
    FReadStream, FWriteStream: TStream;
    FErrorCode: Cardinal;
    FErrorMsg: String;
    function GetDataReady: Boolean; virtual;
    procedure SetConnected(const Value: Boolean);
    procedure SetLocalAddr(const Value: QStringW);
    procedure SetRemoteAddr(const Value: QStringW);
    function GetReadStream: TStream; virtual;
    function GetWriteStream: TStream; virtual;
    procedure HandleNeeded; virtual;
    function InternalConnect: Boolean; virtual;
    procedure InternalDisconnect; virtual;
    procedure SetLastError(ACode: Cardinal; const AMsg: String); overload;
    procedure SetLastError(const AMsg: String); overload;
    procedure DoConnected;
    procedure DoDisconnected;
    procedure AddrChanged; virtual;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure ConnectTo(const ARemoteAddr: QStringW);
    function TryConnectTo(const ARemoteAddr: QStringW): Boolean; virtual;
    procedure Disconnect; virtual;
    function WaitForData(ATimeout: Cardinal): TWaitResult; overload; virtual;
    procedure WaitForData(ACallback: TQConnectionCallback; AParam: IntPtr);
      overload; virtual;
    property Connected: Boolean read FConnected write SetConnected;
    property LocalAddr: QStringW read FLocalAddr write SetLocalAddr;
    property RemoteAddr: QStringW read FRemoteAddr write SetRemoteAddr;
    property DataReady: Boolean read GetDataReady;
    property ReadStream: TStream read GetReadStream;
    property WriteStream: TStream read GetWriteStream;
    property Tag: IntPtr read FTag write FTag;
    property LastErrorCode: Cardinal read FErrorCode;
    property LastErrorMsg: String read FErrorMsg;
  end;

implementation

resourcestring
  SCantConnectToRemote = '无法连接到远程地址:%s。';
  SCantCreateHandle = '无法创建连接句柄。';
  SAsynWaitNotSupport = '连接不支持异步等待数据。';
  { TQBaseConnection }

procedure TQBaseConnection.DoConnected;
begin
  FConnected := True;
end;

procedure TQBaseConnection.DoDisconnected;
begin

end;

procedure TQBaseConnection.AddrChanged;
begin

end;

procedure TQBaseConnection.ConnectTo(const ARemoteAddr: QStringW);
begin
  if not TryConnectTo(ARemoteAddr) then
    raise QException.CreateFmt(SCantConnectToRemote, [ARemoteAddr])
  else
    FRemoteAddr := ARemoteAddr;
end;

constructor TQBaseConnection.Create;
begin
  inherited;
end;

destructor TQBaseConnection.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TQBaseConnection.Disconnect;
begin
  InternalDisconnect;
end;

function TQBaseConnection.GetDataReady: Boolean;
begin
  Result := False;
end;

function TQBaseConnection.GetReadStream: TStream;
begin
  HandleNeeded;
  Result := FReadStream;
end;

function TQBaseConnection.GetWriteStream: TStream;
begin
  HandleNeeded;
  Result := FWriteStream;
end;

procedure TQBaseConnection.HandleNeeded;
begin
  raise Exception.Create(SCantCreateHandle);
end;

function TQBaseConnection.InternalConnect: Boolean;
begin
  Result := False;
end;

procedure TQBaseConnection.InternalDisconnect;
begin

end;

procedure TQBaseConnection.SetConnected(const Value: Boolean);
begin
  if FConnected <> Value then
  begin
    if Value then
      ConnectTo(RemoteAddr)
    else
      Disconnect;
  end;
end;

procedure TQBaseConnection.SetLastError(ACode: Cardinal; const AMsg: String);
begin
  FErrorCode := ACode;
  FErrorMsg := AMsg;
end;

procedure TQBaseConnection.SetLastError(const AMsg: String);
begin
  SetLastError(Cardinal(-1), AMsg);
end;

procedure TQBaseConnection.SetLocalAddr(const Value: QStringW);
begin
  if FLocalAddr <> Value then
  begin
    FLocalAddr := Value;
    AddrChanged;
  end;
end;

procedure TQBaseConnection.SetRemoteAddr(const Value: QStringW);
begin
  if FRemoteAddr <> Value then
  begin
    if Connected then
      Disconnect;
    FRemoteAddr := Value;
    AddrChanged;
  end;
end;

function TQBaseConnection.TryConnectTo(const ARemoteAddr: QStringW): Boolean;
begin
  if Connected then
    Disconnect;
  FRemoteAddr := ARemoteAddr;
  Result := InternalConnect;
end;

function TQBaseConnection.WaitForData(ATimeout: Cardinal): TWaitResult;
begin
  Result := wrTimeout;
end;

procedure TQBaseConnection.WaitForData(ACallback: TQConnectionCallback;
  AParam: IntPtr);
begin
  raise Exception.Create(SAsynWaitNotSupport);
end;

end.
