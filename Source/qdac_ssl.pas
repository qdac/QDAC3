unit qdac_ssl;

interface

uses classes, sysutils, qdac_openssl;

type
  TQSSLItem = class;

  TQSSLManager = class
  private
    class function GetCurrent: TQSSLManager; static;
  protected
    FContext: PSSL_CTX;
    FPassword: String;
    FCAFile: String;
    FCARootFile: String;
    FPrivateKeyFile: String;
    FInitialized: Boolean;
    class var FCurrent: TQSSLManager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadCAFiles(const ACAFile: String = '');
    procedure LoadKey(const APrivateKeyFile: String = '');
    function NewItem: TQSSLItem;
    property Password: String read FPassword write FPassword;
    property PrivateKeyFile: String read FPrivateKeyFile write FPrivateKeyFile;
    property CAFile: String read FCAFile write FCAFile;
    property CARootFile: String read FCARootFile write FCARootFile;
    property Initialized: Boolean read FInitialized;
    class property Current: TQSSLManager read GetCurrent;
  end;

  TQSSLMode = (EnablePartialWrite, AcceptMovingWriteBuffer, AutoReply, NoAutoChain);
  TQSSLModes = set of TQSSLMode;

  TQSSLItem = class
  private
    FSSL: PSSL;
    FHandle: THandle;
    FOwner: TQSSLManager;
    FModes: TQSSLModes;
    function GetCipherBits: Integer;
    function GetCipherName: String;
    function GetCurentCipher: Pointer;
    function GetPeerCertificate: PX509;
    function GetVerified: Boolean;
    procedure SetModes(const Value: TQSSLModes);
    function GetSSLErrrCode: Integer;
    function GetLastSSLErrorMsg: String;
  public
    constructor Create(AOwner: TQSSLManager); overload;
    destructor Destroy; override;
    function Bind(AHandle: THandle): Boolean;
    function Accept: Boolean;
    function Connect: Boolean;
    function Shutdown: Boolean;
    function Read(var ABuf; ACount: Integer): Integer;
    function Peek(var ABuf; ACount: Integer): Integer;
    function Write(const ABuf; ACount: Integer): Integer;
    function Pending: Integer;
    property PeerCertificate: PX509 read GetPeerCertificate;
    property CurrentCipher: Pointer read GetCurentCipher;
    property CipherName: String read GetCipherName;
    property CipherBits: Integer read GetCipherBits;
    property Verified: Boolean read GetVerified;
    property Modes: TQSSLModes read FModes write SetModes;
    property LastError: Integer read GetSSLErrrCode;
    property LastErrorMsg: String read GetLastSSLErrorMsg;
  end;

implementation

uses qstring;

resourcestring
  SInitFailed = '初始化加密库失败，无法加载 %s/%s/%s 之一，请检查其是否存在。';
  SCreateContextError = '初始化 SSL 上下文失败。';
  SLoadCAError = '加载 CA 证书 %s 失败，请检查证书是否有效。';
  { TQSSLManager }

constructor TQSSLManager.Create;

begin
  inherited;
  FInitialized := InitSSLInterface;
  if FInitialized then
  begin
    // 服务器方法
    SslLibraryInit;
    SslLoadErrorStrings;
    OPENSSLaddallalgorithms();
    SslLoadErrorStrings;
    // 创建SSL上下文
    FContext := SslCtxNew(SslMethodV23);
    if FContext = nil then
      raise Exception.Create(SCreateContextError);
  end;
end;

destructor TQSSLManager.Destroy;
begin
  if Assigned(FContext) then
  begin
    try
      begin
        SSLFree(FContext);
      end;
    finally
      FContext := nil;
    end;
  end;
  inherited;
end;

class function TQSSLManager.GetCurrent: TQSSLManager;
begin
  if not Assigned(FCurrent) then
  begin
    Result := TQSSLManager.Create;
    if AtomicCmpExchange(Pointer(FCurrent), Pointer(Result), nil) <> nil then
      FreeAndNil(Result);
  end;
  Result := FCurrent;
end;

procedure TQSSLManager.LoadCAFiles(const ACAFile: String);
var
  AStatus: Integer;
  ACipherList: StringA;
begin
  if Length(ACAFile) > 0 then
    FCARootFile := ACAFile;
  ACipherList := 'DEFAULT';
  SslCtxSetCipherList(FContext, ACipherList);
  // 设置证书文件口令
  SslCtxSetDefaultPasswdCbUserdata(FContext, PCharA(StringA(FPassword)));
  // 加载可信任的 CA 证书
  if Length(CAFile) > 0 then
  begin
    AStatus := SslCtxLoadVerifyLocations(FContext, StringA(CAFile), './');
    if AStatus <= 0 then
      raise Exception.CreateFmt(SLoadCAError, [CAFile]);
  end;
  // 加载自己的证书
  if Length(CARootFile) > 0 then
  begin
    AStatus := SslCtxUseCertificateFile(FContext, StringA(CARootFile), SSL_FILETYPE_PEM);
    if AStatus <= 0 then
      raise Exception.CreateFmt(SLoadCAError, [CAFile]);
  end;
end;

procedure TQSSLManager.LoadKey(const APrivateKeyFile: String);
var
  AStatus: Integer;
begin
  PrivateKeyFile := APrivateKeyFile;
  // 加载自己的私钥
  if Length(PrivateKeyFile) > 0 then
  begin
    AStatus := SslCtxUsePrivateKeyFile(FContext, StringA(PrivateKeyFile), SSL_FILETYPE_PEM);
    if AStatus <= 0 then
      raise Exception.CreateFmt(SLoadCAError, [CAFile]);
    // 判定私钥是否正确
    if SslCtxCheckPrivateKeyFile(FContext) = 0 then
      raise Exception.CreateFmt(SLoadCAError, [CAFile]);
  end;
end;

function TQSSLManager.NewItem: TQSSLItem;
begin
  Result := TQSSLItem.Create(Self);
end;

{ TQSSLItem }

function TQSSLItem.Accept: Boolean;
begin
  Result := SSLAccept(FSSL) > 0;
end;

function TQSSLItem.Bind(AHandle: THandle): Boolean;
begin
  if FHandle <> AHandle then
  begin
    FHandle := AHandle;
    if FHandle <> 0 then
      Result := SslSetFd(FSSL, AHandle) > 0;
  end
  else
    Result := True;
end;

function TQSSLItem.Connect: Boolean;
var
  ACode: Integer;
begin
  ACode := SSLConnect(FSSL);
  if ACode = -1 then
  begin
    while LastError in [SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE] do
    begin
      Sleep(10);
    end;
  end;
  Result := ACode > 0;
end;

constructor TQSSLItem.Create(AOwner: TQSSLManager);
begin
  inherited Create;
  FOwner := AOwner;
  FSSL := SslNew(AOwner.FContext);
end;

destructor TQSSLItem.Destroy;
begin
  SslShutdown(FSSL);
  SSLFree(FSSL);
  inherited;
end;

function TQSSLItem.GetCipherBits: Integer;
begin
  if SSLCipherGetBits(FSSL, Result) <= 0 then
    Result := -1;
end;

function TQSSLItem.GetCipherName: String;
begin
  Result := SSLCipherGetName(FSSL);
end;

function TQSSLItem.GetCurentCipher: Pointer;
begin
  Result := SSLGetCurrentCipher(FSSL);
end;

function TQSSLItem.GetLastSSLErrorMsg: String;
begin
  Result := qstring.Utf8Decode(PQCharA(ERR_error_string(LastError, nil)));
end;

function TQSSLItem.GetPeerCertificate: PX509;
begin
  Result := SslGetPeerCertificate(FSSL);
end;

function TQSSLItem.GetSSLErrrCode: Integer;
begin
  if SSLGetError(FSSL, Result) <= 0 then
    Result := SSL_ERROR_SSL;
end;

function TQSSLItem.GetVerified: Boolean;
begin
  Result := SSLGetVerifyResult(FSSL) > 0;
end;

function TQSSLItem.Peek(var ABuf; ACount: Integer): Integer;
begin
  Result := SSLPeek(FSSL, @ABuf, ACount);
end;

function TQSSLItem.Pending: Integer;
begin
  Result := SSLPending(FSSL);
end;

function TQSSLItem.Read(var ABuf; ACount: Integer): Integer;
begin
  Result := SSLRead(FSSL, @ABuf, ACount);
end;

procedure TQSSLItem.SetModes(const Value: TQSSLModes);
var
  AValue: Integer;
  I: TQSSLMode;
begin
  AValue := 0;
  for I := Low(TQSSLMode) to High(TQSSLMode) do
  begin
    if I in Value then
      AValue := AValue or (1 shl Ord(I));
  end;
  SSLSetMode(FSSL, AValue);
end;

function TQSSLItem.Shutdown: Boolean;
begin
  Result := SslShutdown(FSSL) > 0;
end;

function TQSSLItem.Write(const ABuf; ACount: Integer): Integer;
begin
  Result := SSLWrite(FSSL, @ABuf, ACount);
end;

end.
