unit qdac_ssl_ics;

interface

uses Classes, Sysutils, qdac_ssl, OverbyteIcsWSocket, OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, QString, QWorker{$IFDEF MSWINDOWS},
  WinSock{$ELSE} , Posix.Base, Posix.Stdio, Posix.Pthread, Posix.UniStd, IOUtils,
  Posix.NetDB, Posix.SysSocket, Posix.Fcntl, Posix.StrOpts, Posix.Errno,
  Posix.NetinetIn, Posix.arpainet, Posix.SysSelect, Posix.Systime{$ENDIF};

implementation

type
  TQICSSSLFactory = class;

  TQSSLContextHacker = class(TSSLContext)
  public
    property Context: PSSL_CTX read FSSLCtx;
  end;

  TQICSBaseFile = class(TInterfacedObject)
  private
    FFactory: TQICSSSLFactory;
    FFileName: String;
    FData: TBytes;
    FPassword: String;
    FFormat: TQSSLFileFormat;
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);
    function GetData: TBytes;
    procedure SetData(const AData: TBytes);
    procedure DecodeData; virtual; abstract;
    function GetPassword: String;
    procedure SetPassword(const AValue: String);
    function GetFormat: TQSSLFileFormat;
    procedure SetFormat(const AValue: TQSSLFileFormat);
  public
    constructor Create(AContext: TQICSSSLFactory);
    procedure LoadFromFile(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    function Exists: Boolean; virtual;
    property FileName: String read FFileName write SetFileName;
    property Data: TBytes read FData write SetData;
    property Password: String read FPassword write FPassword;
    property Format: TQSSLFileFormat read FFormat write FFormat;
  end;

  TQICSCA = class(TQICSBaseFile, IQSSLCA)
  protected
    FX509: TX509Base;
    procedure DecodeData; override;
  public
    destructor Destroy; override;
    procedure SaveToStream(AStream: TStream); override;
    function Exists: Boolean; override;
  end;

  TQICSKey = class(TQICSBaseFile, IQSSLKey)
  protected
    FKey: PEVP_PKEY;
    procedure DecodeData; override;
    class function PasswordCallBack(Buf: PAnsiChar; Num, RWFlag: Integer; UserData: Pointer): Integer; cdecl; static;
  public
    destructor Destroy; override;
    procedure SaveToStream(AStream: TStream); override;
    function Exists: Boolean; override;
  end;

  TQICSItem = class(TInterfacedObject, IQSSLItem)
  private
    FServerName: String;
    class procedure InfoCallBack(const ssl: PSSL; Where, Ret: Integer); cdecl; static;
  protected
    FFactory: TQICSSSLFactory;
    FHandle: THandle;
    FSSL: PSSL;
    FSslBio, FNBIO, FIBIO: PBIO;
    FPeerCA: IQSSLCA;
    FHandShakeCount: Integer;
    FSsl_In_CB, FSslInRenegotiation, FHandshakeEventDone, FHandshakeDone: Boolean;
    function GetHandle: THandle;
    procedure SetHandle(AHandle: THandle);
    function Accept: Boolean;
    function Connect: Boolean;
    function Read(var ABuf; ACount: Integer): Integer;
    function Write(const ABuf; ACount: Integer): Integer;
    function GetPrivateKey: IQSSLKey;
    function GetPrivateCA: IQSSLCA;
    function GetPeerCA: IQSSLCA;
    function GetRootCA: IQSSLCA;
    function GetCipherBits: Integer;
    function GetCipherName: String;
    function GetFactory: IQSSLFactory;
    procedure Clear;
    procedure CloseDelayed;
    procedure DoCloseSocket;
    procedure Shutdown;
  public
    constructor Create(AContext: TQICSSSLFactory); overload;
    destructor Destroy; override;
    property Handle: THandle read GetHandle write SetHandle;
    property PrivateKey: IQSSLKey read GetPrivateKey;
    property PrivateCA: IQSSLCA read GetPrivateCA;
    property RootCA: IQSSLCA read GetRootCA;
    property CipherBits: Integer read GetCipherBits;
    property CipherName: String read GetCipherName;
    property ServerName: String read FServerName write FServerName;
  end;

  TQICSSSLFactory = class(TInterfacedObject, IQSSLFactory)
  private
    class var FCurrent: TQICSSSLFactory;
  protected
    FContext: TQSSLContextHacker;
    FRootCA, FPrivateCA: IQSSLCA;
    FPrivateKey: IQSSLKey;
    function NewItem: IQSSLItem;
    function GetRootCA: IQSSLCA;
    function GetPrivateCA: IQSSLCA;
    function GetPrivateKey: IQSSLKey;
    function GetRootCAPath: String;
    procedure SetRootCAPath(const Value: String);
    function GetName: String;
    function GetLastError: Integer;
    function GetLastErrorMsg: String;
    function GetVerifyPeer: Boolean;
    procedure SetVerifyPeer(const AValue: Boolean);
    procedure NeedInitialized;
    function GetSSLContext: PSSL_CTX;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function NewFactory: IQSSLFactory;
    procedure Assign(ASource: TQICSSSLFactory);
    property RootCA: IQSSLCA read GetRootCA;
    property PrivateKey: IQSSLKey read GetPrivateKey;
    property CAPath: String read GetRootCAPath write SetRootCAPath;
    property SSLContext: PSSL_CTX read GetSSLContext;
    class property Current: TQICSSSLFactory read FCurrent;
  end;
  { TQICSCA }

procedure TQICSCA.DecodeData;
var
  AExt: String;
  ABio: PBIO;
begin
  {
    .PEM, .CER, .CRT - Base64 encoded DER - LoadFromPemFile/SaveToPemFile
    .DER, .CER, .CRT - binary DER - LoadFromPEMFile/SaveToDERFile
    .P7B, .P7R, .SPC - PKCS#7 - LoadFromP7BFile/SaveToP7BFile
    .PFX, .P12 - PKCS#12
  }
  FFactory.NeedInitialized;
  ABio := nil;
  if not Assigned(FX509) then
    FX509 := TX509Base.Create(nil);
  try
    if Format = ffUnknown then
    begin
      AExt := LowerCase(ExtractFileExt(FFileName));
      if Length(AExt) > 0 then
      begin
        if (AExt = '.pfx') or (AExt = '.p12') then
          FFormat := ffP12
        else if (AExt = '.p7b') or (AExt = '.p7r') or (AExt = '.p7s') or (AExt = '.spc') then
          FFormat := ffP7B
        else if (AExt = '.pem') or (AExt = '.der') or (AExt = '.cer') or (AExt = '.crt') then
          FFormat := ffPEM;
      end;
    end
    else
    begin
      case FFormat of
        ffPEM, ffCER64, ffCRT64, ffDER, ffCER, ffCRT:
          begin
            if Length(FData) < 16 then
              raise ESSLError.Create('Bad or empty SSL data');
            ABio := f_BIO_new_mem_buf(@FData[0], Length(FData));
            FX509.ReadFromBio(ABio, false, Password);
          end;
        ffP7B, ffP7R, ffSPC:
          begin
            // Todo:将来再支持
          end;
        ffPFX, ffP12:
          begin
            // Todo:将来再支持
          end;
      end;
    end;
  finally
    if not FX509.IsCertLoaded then
      FreeAndNil(FX509);
    if Assigned(ABio) then
      f_bio_free(ABio);
  end;
end;

destructor TQICSCA.Destroy;
begin
  if Assigned(FX509) then
    FreeAndNil(FX509);
  inherited;
end;

function TQICSCA.Exists: Boolean;
begin
  Result := (Length(FData) > 0) and Assigned(FX509) and FX509.IsCertLoaded;
end;

procedure TQICSCA.SaveToStream(AStream: TStream);
begin
  // todo:将来再支持
end;

{ TQICSSSLFactory }

procedure TQICSSSLFactory.Assign(ASource: TQICSSSLFactory);
begin
  // FRootCA.Assign(ASource,FRootCA);
  // Todo:
end;

constructor TQICSSSLFactory.Create;
begin
  inherited;
  FContext := TQSSLContextHacker.Create(nil);
  FContext.SslVerifyPeer := false;
  FContext.SslVerifyFlags := [];
  FRootCA := TQICSCA.Create(Self);
  FPrivateCA := TQICSCA.Create(Self);
  FPrivateKey := TQICSKey.Create(Self);
  if not Assigned(FCurrent) then
    AtomicCmpExchange(Pointer(FCurrent), Pointer(Self), nil);
end;

destructor TQICSSSLFactory.Destroy;
begin
  FRootCA := nil;
  FPrivateCA := nil;
  FPrivateKey := nil;
  FContext.DeInitContext;
  FreeAndNil(FContext);
  inherited;
end;

function TQICSSSLFactory.GetLastError: Integer;
begin
  Result := FContext.LastSslError;
end;

function TQICSSSLFactory.GetLastErrorMsg: String;
begin
  Result := FContext.LastSslErrMsg;
end;

function TQICSSSLFactory.GetName: String;
begin
  Result := 'OpenSSL(ICS)';
end;

function TQICSSSLFactory.GetPrivateCA: IQSSLCA;
begin
  Result := FPrivateCA;
end;

function TQICSSSLFactory.GetPrivateKey: IQSSLKey;
begin
  Result := FPrivateKey;
end;

function TQICSSSLFactory.GetRootCA: IQSSLCA;
begin
  Result := FRootCA;
end;

function TQICSSSLFactory.GetRootCAPath: String;
begin
  Result := FContext.SslCAPath;
end;

function TQICSSSLFactory.GetSSLContext: PSSL_CTX;
begin
  Result := FContext.Context;
end;

function TQICSSSLFactory.GetVerifyPeer: Boolean;
begin
  Result := FContext.SslVerifyPeer;
end;

procedure TQICSSSLFactory.NeedInitialized;
begin
  if not FContext.IsCtxInitialized then
    FContext.InitContext;
end;

function TQICSSSLFactory.NewFactory: IQSSLFactory;
var
  ATemp: TQICSSSLFactory;
begin
  ATemp := TQICSSSLFactory.Create;
  Result := ATemp;
  ATemp.Assign(Self);
end;

function TQICSSSLFactory.NewItem: IQSSLItem;
begin
  if Assigned(f_ssl_read) then
    Result := TQICSItem.Create(Self)
  else
    Result:=nil;
end;

procedure TQICSSSLFactory.SetRootCAPath(const Value: String);
begin
  FContext.SslCAPath := Value;
end;

procedure TQICSSSLFactory.SetVerifyPeer(const AValue: Boolean);
begin
  FContext.SslVerifyPeer := AValue;
end;

{ TQICSBaseFile }

constructor TQICSBaseFile.Create(AContext: TQICSSSLFactory);
begin
  inherited Create;
  FFactory := AContext;
end;

function TQICSBaseFile.Exists: Boolean;
begin
  Result := Length(FData) > 0;
end;

function TQICSBaseFile.GetData: TBytes;
begin
  Result := FData;
end;

function TQICSBaseFile.GetFileName: String;
begin
  Result := FFileName;
end;

function TQICSBaseFile.GetFormat: TQSSLFileFormat;
begin
  Result := FFormat;
end;

function TQICSBaseFile.GetPassword: String;
begin
  Result := FPassword;
end;

procedure TQICSBaseFile.LoadFromFile(const AFileName: String);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    FFileName := AFileName;
    LoadFromStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQICSBaseFile.LoadFromStream(AStream: TStream);
var
  ASize: IntPtr;
begin
  ASize := AStream.Size - AStream.Position;
  SetLength(FData, ASize);
  AStream.ReadBuffer(FData[0], ASize);
  DecodeData;
end;

procedure TQICSBaseFile.SaveToFile(const AFileName: String);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQICSBaseFile.SetData(const AData: TBytes);
begin
  FData := Copy(AData, 0, Length(AData));
  DecodeData;
end;

procedure TQICSBaseFile.SetFileName(const AFileName: String);
begin
  if FFileName <> AFileName then
  begin
    SetLength(FData, 0);
    if Length(AFileName) > 0 then
      LoadFromFile(AFileName)
    else
      SetLength(FFileName, 0);
  end;
end;

procedure TQICSBaseFile.SetFormat(const AValue: TQSSLFileFormat);
begin
  if FFormat <> AValue then
  begin
    FFormat := AValue;
  end;
end;

procedure TQICSBaseFile.SetPassword(const AValue: String);
begin
  FPassword := AValue;
end;

{ TQICSKey }
class function TQICSKey.PasswordCallBack(Buf: PAnsiChar; Num: Integer; RWFlag: Integer; UserData: Pointer): Integer;
var
  Obj: TQICSKey;
  SslPassPhraseA: AnsiString;
begin
{$IFNDEF NO_SSL_MT}
  LockPwdCB.Enter;
  try
{$ENDIF}
    Obj := TQICSKey(UserData);
    if (Num < (Length(Obj.Password) + 1)) or (Length(Obj.Password) = 0) then
      Result := 0
    else
    begin
      SslPassPhraseA := Obj.FFactory.FContext.PasswordConvert(Obj.Password);
      Move(Pointer(SslPassPhraseA)^, Buf^, Length(SslPassPhraseA) + 1);
      Result := Length(SslPassPhraseA);
    end;
{$IFNDEF NO_SSL_MT}
  finally
    LockPwdCB.Leave;
  end;
{$ENDIF}
end;

procedure TQICSKey.DecodeData;
var
  Bio: PBIO;
  Pkey: PEVP_PKEY;
begin
  FFactory.NeedInitialized;
  if Length(FData) = 0 then
    Exit;
  Pkey := nil;
  Bio := f_BIO_new_mem_buf(@FData[0], Length(FData));
  if Assigned(Bio) then
  begin
    try
      Pkey := f_PEM_read_bio_PrivateKey(Bio, nil, PasswordCallBack, Self);
      if Assigned(Pkey) then
      begin
        begin
          if (f_SSL_CTX_use_PrivateKey(FFactory.SSLContext, Pkey) > 0) then
          begin
            FKey := Pkey;
            Pkey := nil;
          end;
        end;
      end;
    finally
      if Assigned(Pkey) then
        f_EVP_PKEY_free(Pkey);
      if Assigned(Bio) then
        f_bio_free(Bio);
    end;
  end;
end;

destructor TQICSKey.Destroy;
begin
  if Assigned(FKey) then
  begin
    f_EVP_PKEY_free(FKey);
    FKey := nil;
  end;
  inherited;
end;

function TQICSKey.Exists: Boolean;
begin
  Result := (Length(FData) > 0) and Assigned(FKey);
end;

procedure TQICSKey.SaveToStream(AStream: TStream);
begin
  // Todo:将来支持
end;

{ TQICSItem }

function TQICSItem.Accept: Boolean;
begin
  Result := f_ssl_accept(FSSL) > 0;
end;

procedure TQICSItem.Clear;
begin
  if Assigned(FSslBio) then
    f_bio_free(FSslBio);
  if Assigned(FNBIO) then
    f_bio_free(FNBIO);
  if Assigned(FIBIO) then
    f_bio_free(FIBIO);
  if Assigned(FSSL) then
  begin
    Shutdown;
    f_ssl_free(FSSL);
  end;
end;

procedure TQICSItem.CloseDelayed;
begin
  Workers.Post(
    procedure(AJob: PQJob)
    begin
      DoCloseSocket;
    end, nil);
end;

procedure TQICSItem.DoCloseSocket;
begin
{$IFDEF MSWINDOWS}
  closesocket(FHandle);
{$ELSE}
  __close(FHandle);
{$ENDIF}
end;

function TQICSItem.Connect: Boolean;
var
  ACode: Integer;
begin
  ACode := f_ssl_connect(FSSL);
  if ACode = -1 then
  begin
    repeat
      Sleep(10);
      ACode := FFactory.GetLastError;
    until not(ACode in [SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE]);
  end;
  Result := ACode > 0;
end;

constructor TQICSItem.Create(AContext: TQICSSSLFactory);
begin
  inherited Create;
  FFactory := AContext;
end;

destructor TQICSItem.Destroy;
begin
  Clear;
  inherited;
end;

function TQICSItem.GetCipherBits: Integer;
begin
  Result := 0;
end;

function TQICSItem.GetCipherName: String;
begin
  Result := '';
end;

function TQICSItem.GetFactory: IQSSLFactory;
begin
  Result := FFactory;
end;

function TQICSItem.GetHandle: THandle;
begin
  Result := FHandle;
end;

function TQICSItem.GetPeerCA: IQSSLCA;
begin
  Result := FPeerCA;
end;

function TQICSItem.GetPrivateCA: IQSSLCA;
begin
  Result := FFactory.GetPrivateCA;
end;

function TQICSItem.GetPrivateKey: IQSSLKey;
begin
  Result := FFactory.GetPrivateKey;
end;

function TQICSItem.GetRootCA: IQSSLCA;
begin
  Result := FFactory.GetRootCA;
end;

function TQICSItem.Read(var ABuf; ACount: Integer): Integer;
begin
  if Assigned(f_ssl_read) then
    Result := f_ssl_read(FSSL, @ABuf, ACount)
  else
    Result:=0;
end;

class procedure TQICSItem.InfoCallBack(const ssl: PSSL; Where: Integer; Ret: Integer);
var
  Obj: TQICSItem;
  function IsSslRenegotiationDisallowed: Boolean;
  begin
    Result := ICS_SSL_NO_RENEGOTIATION or (f_Ics_SSL_get_options(Obj.FSSL) and SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION = 0);
  end;
  procedure LogState;
  var
    W, Err: Integer;
    Str: String;
  begin
    W := Where and (not SSL_ST_MASK);
    if (W and SSL_ST_CONNECT) <> 0 then
      Str := 'SSL_connect: '
    else if (W and SSL_ST_ACCEPT) <> 0 then
      Str := 'SSL_accept: '
    else if (Where and SSL_CB_HANDSHAKE_START) <> 0 then
      Str := 'SSL_handshake_start: ' { V8.53 }
    else if (Where and SSL_CB_HANDSHAKE_DONE) <> 0 then
      Str := 'SSL_handshake_done: ' { V8.53 }
    else if (Where and SSL_CB_ALERT) <> 0 then
      Str := 'SSL_alert: ' { V8.55 }
    else
      Str := 'undefined: ';

    if ((Where and SSL_CB_LOOP) <> 0) then
    begin
      DebugOut(Str + String(f_SSL_state_string_long(ssl)));
    end
    else if (Where and SSL_CB_ALERT) <> 0 then
    begin
      if (Where and SSL_CB_READ) <> 0 then
        Str := 'read '
      else
        Str := 'write ';

      DebugOut('SSL3 alert ' + Str + String(f_SSL_alert_type_string_long(Ret)) + ' ' +
        String(f_SSL_alert_desc_string_long(Ret)));
    end
    else if (Where and SSL_CB_EXIT) <> 0 then
    begin
      if Ret = 0 then
      begin
        DebugOut('failed in ' + String(f_SSL_state_string_long(ssl)));
      end
      else if Ret < 0 then
      begin
        Err := f_ssl_get_error(ssl, Ret);
        if NOT((Err = SSL_ERROR_WANT_READ) or { V8.14 only want real errors }
          (Err = SSL_ERROR_WANT_WRITE)) then
        begin
          DebugOut(Str + 'error ' + IntToStr(Err) + { V8.14 actual error }
            ' in ' + String(f_SSL_state_string_long(ssl)));
        end;
      end;
    end
    else
      DebugOut(Str + 'where=' + IntToHex(Where, 8) + ', state=' + String(f_SSL_state_string_long(ssl)));
  end;

begin
  TQSSLManager.Lock;
  try
    LogState;
    Obj := TQICSItem(f_SSL_get_ex_data(ssl, 0));
    if not Assigned(Obj) then
      raise ESSLError.Create('ICB> Extended data not assigned fatal error!');
    Obj.FSsl_In_CB := TRUE;
    try
      if (Where and SSL_CB_HANDSHAKE_START) <> 0 then
      begin
        Inc(Obj.FHandShakeCount);
        if (f_SSL_version(Obj.FSSL) < TLS1_3_VERSION) then
        begin
          if (Obj.FHandShakeCount > 1) and IsSslRenegotiationDisallowed then
            Obj.CloseDelayed;
          if Obj.FHandShakeCount > 1 then
            Obj.FSslInRenegotiation := TRUE;
        end;
      end
      else if ((Where and SSL_CB_HANDSHAKE_DONE) > 0) and (NOT Obj.FHandshakeEventDone) then
      begin
        Obj.FHandshakeDone := TRUE;
      end
    finally
      Obj.FSsl_In_CB := false;
      if Obj.FHandle = THandle(-1) then
        Obj.CloseDelayed;
    end;
  finally
    TQSSLManager.Unlock;
  end;
end;

procedure TQICSItem.SetHandle(AHandle: THandle);
var
  VerifyParam: PX509_VERIFY_PARAM;
  Count: Integer;
  Dummy: Byte;
  ReadBytes: LongWord;
begin
  if FHandle <> AHandle then
  begin
    Clear;
    FHandle := AHandle;
    if AHandle <> THandle(-1) then
    begin
      FFactory.NeedInitialized;
      FSSL := f_ssl_new(FFactory.SSLContext);
      if not Assigned(FSSL) then
        raise ESSLError.Create('Create new ssl object failed');
      f_ERR_clear_error;
      f_ssl_set_fd(FSSL, AHandle);
      f_SSL_set_ex_data(FSSL, 0, Self);
      f_SSL_set_info_callback(FSSL, InfoCallBack);
      if FFactory.FPrivateCA.Exists then
        f_SSL_CTX_use_certificate(FFactory.SSLContext, (FFactory.PrivateKey as TQICSCA).FX509.X509);
    end;
  end;
end;

procedure TQICSItem.Shutdown;
begin
  f_ssl_shutdown(FSSL);
  f_SSL_set_info_callback(FSSL, nil);
end;

function TQICSItem.Write(const ABuf; ACount: Integer): Integer;
begin
  if Assigned(f_ssl_write) then
    Result := f_ssl_write(FSSL, @ABuf, ACount)
  else
    Result:=0;
end;

initialization

TQSSLManager.Current.RegisterFactory(TQICSSSLFactory.Create);

finalization

TQSSLManager.Current.UnregisterFactory(TQICSSSLFactory.Current);

end.
