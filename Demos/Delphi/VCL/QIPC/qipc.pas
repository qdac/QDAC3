unit qipc;

interface
uses classes,sysutils,windows,qstring,qrbtree,System.Generics.Collections;
type
  TQConnection=class;
  TQTransport=class;
  TQSession=class;
  TQDispatch=class;
  //IP V4端点，端口号和地址
  TQIPV4EndPoint=record
    Port:Word;
    case Integer of
      0:(Addr:Longint);
      1:(B1,B2,B3,B4:Byte);
  end;
  //IP V6端点，暂未定义
  TQIPV6EndPoint=record

  end;
  //本机进程间通讯端点，使用进程ID作为地址
  TQIPCEndPoint=record
    ProcessId:Cardinal;//进程ID
  end;

  TQEndPoint=record
  case Integer of
    0:(IPV4:TQIPV4EndPoint);
    1:(IPV6:TQIPV6EndPoint);
    2:(IPC:TQIPCEndPoint);
  end;

  TQIOEvent=procedure (ASender:TQConnection;const ARemote:TQEndPoint;const ABuf:TBytes) of object;

  TQConnection=class
  protected
    FOwner: TQTransport;
    FHandle:THandle;
    FLocal,FRemote:TQEndPoint;
    FOnRecv: TQIOEvent;
    FAfterSent: TQIOEvent;
    FBeforeSend: TQIOEvent;
    FSessions:TQHashTable;
    procedure HandleNeeded;virtual;
    function CreateHandle:THandle;virtual;
    procedure DestroyHandle;virtual;
  public
    constructor Create(AOwner:TQTransport);overload;virtual;
    destructor Destroy;override;
    function Connect:Boolean;
    procedure Disconnect;
    function Send(const Buffer;ASize:Cardinal):Cardinal;overload;virtual;
    function Send(const ABuf:TBytes):Boolean;overload;virtual;
    function Recv:TBytes;overload;
    function Recv(var ABuf:PByte;ASize:Cardinal):Cardinal;overload;virtual;
    procedure Post(const Buffer;ASize:Cardinal);
    property OnRecv:TQIOEvent read FOnRecv write FOnRecv;
    property BeforeSend:TQIOEvent read FBeforeSend write FBeforeSend;
    property AfterSent:TQIOEvent read FAfterSent write FAfterSent;
    property Remote:TQEndPoint read FRemote write FRemote;
    property Local:TQEndPoint read FLocal write FLocal;
    property Transport:TQTransport read FOwner;
  end;

  TQSession=class
  private
    function GetTransport: TQTransport;inline;
  protected
    FOwner:TQConnection;
    FStream:TStream;
  public
    constructor Create(AOwner:TQConnection);overload;virtual;
    destructor Destroy;override;
    property Stream:TStream read FStream;
    property Transport:TQTransport read GetTransport;
    property Connection:TQConnection read FOwner;
  end;

  TQTransport=class
  protected
    FHandle:THandle;
    FConnections:TQHashTable;
    FDispatch:TQDispatch;
    function GetCount: Integer;
  public
    constructor Create;overload;
    destructor Destroy;override;
    function Listen(AEndPoint:TQEndPoint):Boolean;
    function EnumConnections(AList:TList):Cardinal;
    property Dispatch:TQDispatch read FDispatch write FDispatch;
    property Count:Integer read GetCount;
  end;

  TQSessionDispatchEvent=procedure (ASender:TQDispatch;ASession:TQSession;var AHandled:Boolean;ATag:Pointer);

  TQDispatch=class
  private
    function GetCount: Integer;
  protected
    FTransport:TQTransport;
    FHandlers:TQHashTable;
  public
    constructor Create(AOwner:TQTransport);overload;virtual;
    destructor Destroy;override;
    function Add(AHandler:TQSessionDispatchEvent;ATag:Pointer):Boolean;
    procedure Remove(AHandler:TQSessionDispatchEvent;ATag:Pointer);
    procedure Clear(AObject:Pointer);overload;
    procedure Clear;overload;
    property Count:Integer read GetCount;
  end;
implementation


{ TQTransport }

constructor TQTransport.Create;
begin

end;

destructor TQTransport.Destroy;
begin

  inherited;
end;

function TQTransport.GetCount: Integer;
begin

end;

{ TQConnection }

function TQConnection.Connect: Boolean;
begin

end;

constructor TQConnection.Create(AOwner: TQTransport);
begin

end;

function TQConnection.CreateHandle: THandle;
begin

end;

destructor TQConnection.Destroy;
begin

  inherited;
end;

procedure TQConnection.DestroyHandle;
begin

end;

procedure TQConnection.Disconnect;
begin

end;

procedure TQConnection.HandleNeeded;
begin

end;

procedure TQConnection.Post(const Buffer; ASize: Cardinal);
begin

end;

function TQConnection.Recv(var ABuf: PByte; ASize: Cardinal): Cardinal;
begin

end;

function TQConnection.Recv: TBytes;
begin

end;

function TQConnection.Send(const Buffer; ASize: Cardinal): Cardinal;
begin

end;

function TQConnection.Send(const ABuf: TBytes): Boolean;
begin

end;

{ TQSession }

constructor TQSession.Create(AOwner: TQConnection);
begin

end;

destructor TQSession.Destroy;
begin

  inherited;
end;

function TQSession.GetTransport: TQTransport;
begin
Result:=Connection.Transport;
end;

{ TQDispatch }

function TQDispatch.Add(AHandler: TQSessionDispatchEvent;
  ATag: Pointer): Boolean;
begin

end;

procedure TQDispatch.Clear;
begin

end;

constructor TQDispatch.Create(AOwner: TQTransport);
begin

end;

destructor TQDispatch.Destroy;
begin

  inherited;
end;

function TQDispatch.GetCount: Integer;
begin

end;

procedure TQDispatch.Remove(AHandler: TQSessionDispatchEvent; ATag: Pointer);
begin

end;

end.
