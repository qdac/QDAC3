unit qdac_ssl;

interface

uses classes, sysutils, syncobjs;

type
  IQSSLFactory = interface;

  IQSSLX509 = interface
    ['{99B324F9-D274-4ADC-9AC0-C1E58E1B000D}']
  end;

  PQSSLStack = Pointer;

  IQSSLContext = interface
    ['{6135E5C8-895A-4BAD-93CD-4B37FA8ACD38}']
  end;

  TQSSLFileFormat = (ffUnknown, ffPEM, ffCER64, ffCRT64, ffDER, ffCER, ffCRT, ffP7B, ffP7R, ffSPC, ffPFX, ffP12);

  IQSSLCA = interface
    ['{42D82B35-F2E6-4CEF-98C9-C01AE26B12D1}']
    procedure LoadFromFile(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    function GetFileName: String;
    procedure SetFileName(const AFileName: String);
    function GetData: TBytes;
    procedure SetData(const AData: TBytes);
    function GetPassword: String;
    procedure SetPassword(const AValue: String);
    function GetFormat: TQSSLFileFormat;
    procedure SetFormat(const AValue: TQSSLFileFormat);
    function Exists: Boolean;
    property FileName: String read GetFileName write SetFileName;
    property Data: TBytes read GetData write SetData;
    property Password: String read GetPassword write SetPassword;
    property Format: TQSSLFileFormat read GetFormat write SetFormat;
  end;

  IQSSLKey = interface(IQSSLCA)
    ['{8ABDDEE5-1480-43D7-98D4-B4ECD9B0B10E}']
  end;

  IQSSLItem = interface
    ['{F05A2E31-99AF-400D-BAE7-C33A5C66654C}']
    function GetHandle: THandle;
    procedure SetHandle(AHandle: THandle);
    function Accept: Boolean;
    function Connect: Boolean;
    procedure Shutdown;
    function Read(var ABuf; ACount: Integer): Integer;
    function Write(const ABuf; ACount: Integer): Integer;
    function GetPrivateKey: IQSSLKey;
    function GetPrivateCA: IQSSLCA;
    function GetRootCA: IQSSLCA;
    function GetCipherBits: Integer;
    function GetCipherName: String;
    function GetFactory: IQSSLFactory;
    property Handle: THandle read GetHandle write SetHandle;
    property PrivateKey: IQSSLKey read GetPrivateKey;
    property PrivateCA: IQSSLCA read GetPrivateCA;
    property RootCA: IQSSLCA read GetRootCA;
    property CipherBits: Integer read GetCipherBits;
    property CipherName: String read GetCipherName;
  end;

  IQSSLFactory = interface
    ['{F58D9C76-4C0A-4104-9B15-2D54C61AA0E7}']
    function NewItem: IQSSLItem;
    function NewFactory: IQSSLFactory;
    function GetRootCA: IQSSLCA;
    function GetPrivateCA: IQSSLCA;
    function GetPrivateKey: IQSSLKey;
    function GetName: String;
    function GetLastError: Integer;
    function GetLastErrorMsg: String;
    function GetVerifyPeer: Boolean;
    procedure SetVerifyPeer(const AValue: Boolean);
    function GetRootCAPath: String;
    property VerifyPeer: Boolean read GetVerifyPeer write SetVerifyPeer;
    property LastError: Integer read GetLastError;
    property LastErrorMsg: String read GetLastErrorMsg;
    property Name: String read GetName;
    property RootCA: IQSSLCA read GetRootCA;
    property PrivateCA: IQSSLCA read GetPrivateCA;
    property PrivateKey: IQSSLKey read GetPrivateKey;
  end;

  ESSLError = class(Exception)

  end;

  TQSSLManager = class sealed
  private
    class function GetCurrent: TQSSLManager; static;
    class function GetActiveFactory: IQSSLFactory; static;
  protected
  class var
    FCurrent: TQSSLManager;
    FLocker: TCriticalSection;

  var
    FFactories: array of IQSSLFactory;
    FActiveIndex: Integer;
    function InternalGetActiveFactory: IQSSLFactory;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFactory(const AItem: IQSSLFactory);
    procedure UnregisterFactory(const AItem: IQSSLFactory);
    class procedure Lock;
    class procedure Unlock;
    class property ActiveFactory: IQSSLFactory read GetActiveFactory;
    class property Current: TQSSLManager read GetCurrent;
  end;

implementation

{ TQSSLManager }

constructor TQSSLManager.Create;
begin
  inherited;
end;

destructor TQSSLManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FFactories) do
    FFactories[I] := nil;
  inherited;
end;

class function TQSSLManager.GetActiveFactory: IQSSLFactory;
begin
  Result := Current.InternalGetActiveFactory;
end;

class function TQSSLManager.GetCurrent: TQSSLManager;
begin
  if not Assigned(FCurrent) then
  begin
    FCurrent := TQSSLManager.Create;
    TQSSLManager.FLocker := TCriticalSection.Create;
  end;
  Result := FCurrent;
end;

function TQSSLManager.InternalGetActiveFactory: IQSSLFactory;
begin
  if Length(FFactories) > 0 then
  begin
    if (FActiveIndex >= 0) and (FActiveIndex < Length(FFactories)) then
      Result := FFactories[FActiveIndex]
    else
    begin
      Result := FFactories[0];
      FActiveIndex := 0;
    end;
  end
  else
    Result := nil;
end;

class procedure TQSSLManager.Lock;
begin
  FLocker.Enter;
end;

procedure TQSSLManager.RegisterFactory(const AItem: IQSSLFactory);
begin
  if Assigned(AItem) then
  begin
    SetLength(FFactories, Length(FFactories) + 1);
    FFactories[High(FFactories)] := AItem;
  end;
end;

class procedure TQSSLManager.Unlock;
begin
  FLocker.Leave;
end;

procedure TQSSLManager.UnregisterFactory(const AItem: IQSSLFactory);
var
  I: Integer;
begin
  for I := Low(FFactories) to High(FFactories) do
  begin
    if FFactories[I] = AItem then
    begin
      Delete(FFactories, I, 1);
      Break;
    end;
  end;
end;

initialization

finalization

if Assigned(TQSSLManager.FCurrent) then
  begin
  FreeAndNil(TQSSLManager.FCurrent);
  FreeAndNil(TQSSLManager.FLocker);
  end;
end.
