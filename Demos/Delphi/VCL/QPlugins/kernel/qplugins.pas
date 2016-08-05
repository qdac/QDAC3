unit qplugins;

interface

uses classes, sysutils, types, qstring, qvalue, qtimetypes, math;

const
  LOG_EMERGENCY: BYTE = 0;
  LOG_ALERT = 1;
  LOG_FATAL = 2;
  LOG_ERROR = 3;
  LOG_WARN = 4;
  LOG_HINT = 5;
  LOG_MESSAGE = 6;
  LOG_DEBUG = 7;

type
  // 版本信息
  TQShortVersion = packed record
    case Integer of
      0:
        (Major, Minor, Release, Build: BYTE);
      1:
        (Value: Integer);
  end;

  TQPluginVersion = packed record
    Version: TQShortVersion;
    Company: array [0 .. 63] of WideChar;
    Name: array [0 .. 63] of WideChar;
    Description: array [0 .. 255] of WideChar;
    FileName: array [0 .. 255] of WideChar;
  end;

  // 插件版本信息
  IQVersion = interface
    ['{4AD82500-4148-45D1-B0F8-F6B6FB8B7F1C}']
    function GetVersion(var AVerInfo: TQPluginVersion): Boolean; stdcall;
  end;

  // Stream
  IQStream = interface
    ['{BCFD2F69-CCB8-4E0B-9FE9-A7D58797D1B8}']
    function Read(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Write(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Seek(AOffset: Int64; AFrom: BYTE): Int64; stdcall;
    procedure SetSize(ANewSize: UInt64); stdcall;
    function CopyFrom(AStream: IQStream; ACount: Int64): Int64; stdcall;
  end;

  // 参数规格化，用于使用不同语言之间交互
  TQParamType = (ptUnknown,
    // Integer Types
    ptInt8, ptUInt8, ptInt16, ptUInt16, ptInt32, ptUInt32, ptInt64, ptUInt64,
    ptFloat4, ptFloat8, // Float types
    ptDateTime, ptInterval, // DateTime types
    ptAnsiString, ptUtf8String, ptUnicodeString, // String types
    ptBoolean, // Boolean
    ptGuid, // Guid
    ptBytes, // Binary
    ptPointer, // untyped pointer
    ptInterface, // Interface
    ptStream, // 流
    ptArray // Array
    );
  IQParams = interface;

  IQParam = interface
    ['{8641FD44-1BC3-4F04-B730-B5406CDA17E3}']
    function GetName: PWideChar; stdcall;
    function GetAsInteger: Integer; stdcall;
    procedure SetAsInteger(const AValue: Integer); stdcall;
    function GetAsInt64: Int64; stdcall;
    procedure SetAsInt64(const AValue: Int64); stdcall;
    function GetAsBoolean: Boolean; stdcall;
    procedure SetAsBoolean(const AValue: Boolean); stdcall;
    function GetAsSingle: Single; stdcall;
    procedure SetAsSingle(const AValue: Single); stdcall;
    function GetAsFloat: Double; stdcall;
    procedure SetAsFloat(const AValue: Double); stdcall;
    function GetAsString(ABuf: PWideChar; ABufLen: Cardinal): Cardinal stdcall;
    procedure SetAsString(const AValue: PWideChar); stdcall;
    function GetAsGuid: TGuid; stdcall;
    procedure SetAsGuid(const Value: TGuid); stdcall;
    function GetAsBytes(ABuf: PByte; ABufLen: Cardinal): Cardinal; stdcall;
    procedure SetAsBytes(ABuf: PByte; ABufLen: Cardinal); stdcall;
    function GetAsInterface: IInterface; stdcall;
    procedure SetAsInterface(AIntf: IInterface); stdcall;
    function GetIsNull: Boolean; stdcall;
    procedure SetNull; stdcall;
    function GetAsArray: IQParams; stdcall;
    function GetAsStream: IQStream; stdcall;
    function GetAsPointer: Pointer; stdcall;
    procedure SetAsPointer(AValue: Pointer); stdcall;
    function GetParent: IQParams; stdcall;
    function GetSize: Integer; stdcall;
    function GetType: TQParamType; stdcall;
    procedure SetType(const AType: TQParamType); stdcall;
    procedure SetSize(ASize: Integer); stdcall;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsGuid: TGuid read GetAsGuid write SetAsGuid;
    property IsNull: Boolean read GetIsNull;
    property AsArray: IQParams read GetAsArray;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property Parent: IQParams read GetParent;
    property ParamType: TQParamType read GetType;
    property Size: Integer read GetSize;
  end;

  IQParams = interface
    ['{B5746B65-7586-4DED-AE20-D4FF9B6ECD9E}']
    function GetItems(AIndex: Integer): IQParam; stdcall;
    function GetCount: Integer; stdcall;
    function ByName(const AName: PWideChar): IQParam; stdcall;
    function ByPath(APath: PWideChar): IQParam; stdcall;
    function Add(const AName: PWideChar; AParamType: TQParamType)
      : IQParam; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Clear; stdcall;
    procedure SaveToStream(AStream: IQStream); stdcall;
    procedure LoadFromStream(AStream: IQStream); stdcall;
    procedure SaveToFile(const AFileName: PWideChar); stdcall;
    procedure LoadFromFile(const AFileName: PWideChar); stdcall;
    property Items[AIndex: Integer]: IQParam read GetItems; default;
    property Count: Integer read GetCount;
  end;

  IQService = interface
    ['{0DA5CBAC-6AB0-49FA-B845-FDF493D9E639}']
    function NewInstance: IQService; stdcall;
    function Execute(AParams: IQParams; AResult: IQParams): Boolean; stdcall;
    function Suspended(AParams: IQParams): Boolean; stdcall;
    function Resume(AParams: IQParams): Boolean; stdcall;
    function GetParent: IQService; stdcall;
    function GetName: PWideChar; stdcall;
    function GetAttrs: IQParams; stdcall;
    function GetLastErrorMsg: PWideChar; stdcall;
    function GetLastErrorCode: Cardinal; stdcall;
    function GetId: TGuid; stdcall;
    property Parent: IQService read GetParent;
    property Name: PWideChar read GetName;
    property Attrs: IQParams read GetAttrs;
    property LastError: Cardinal read GetLastErrorCode;
    property LastErrorMsg: PWideChar read GetLastErrorMsg;
  end;

  IQServices = interface
    ['{7325DF17-BC83-4163-BB72-0AE0208352ED}']
    function GetItems(AIndex: Integer): IQService; stdcall;
    function GetCount: Integer; stdcall;
    function ByPath(APath: PWideChar): IQService; stdcall;
    function ById(const AId: TGuid): IQService; stdcall;
    function Add(AItem: IQService): Integer; stdcall;
    function IndexOf(AItem: IQService): Integer; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Remove(AItem: IQService); stdcall;
    procedure Clear; stdcall;
    procedure Lock; stdcall;
    procedure Unlock; stdcall;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IQService read GetItems; default;
  end;

  IQNotify = interface
    ['{00C7F80F-44BF-4E60-AA58-5992B2B71754}']
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean); stdcall;
  end;

  IQNotifyManager = interface(IQServices)
    ['{037DCCD1-6877-4917-A315-120CD3E403F4}']
    procedure Subscribe(ANotifyId: Cardinal; AHandler: IQNotify); stdcall;
    procedure Unsubscribe(ANotifyId: Cardinal; AHandler: IQNotify); stdcall;
    function IdByName(const AName: PWideChar): Cardinal; stdcall;
    function NameOfId(const AId: Cardinal): PWideChar; stdcall;
    procedure Send(AId: Cardinal; AParams: IQParams); stdcall;
    procedure Post(AId: Cardinal; AParams: IQParams); stdcall;
    function GetCount: Integer; stdcall;
    function GetId(const AIndex: Integer): Cardinal; stdcall;
    function GetName(const AIndex: Integer): PWideChar; stdcall;
    property Count: Integer read GetCount;
    property Id[const AIndex: Integer]: Cardinal read GetId;
    property Name[const AIndex: Integer]: PWideChar read GetName;
  end;

  IQTextRouter = interface(IQService)
    ['{F3834278-4D2F-46D5-AA72-6EF016CE7F3A}']
    function GetSource: PWideChar; stdcall;
    function GetTarget: PWideChar; stdcall;
    procedure SetRule(const ASource, ATarget: PWideChar); stdcall;
  end;

  IQIdRouter = interface
    ['{C2390553-ABE3-489A-8713-CB28A938C000}']
    function GetSource: TGuid; stdcall;
    function GetTarget: TGuid; stdcall;
    procedure SetRule(const ASource, ATarget: TGuid); stdcall;
  end;

  IQLoader = interface(IQService)
    ['{3F576A14-D251-47C4-AB6E-0F89B849B71F}']
  end;

  IQLog = interface(IQService)
    procedure Post(ALevel: BYTE; AMsg: PWideChar); stdcall;
    procedure Flush; stdcall;
  end;

  IQPluginsManager = interface(IQServices)
    ['{BDE6247B-87AD-4105-BDC9-1EA345A9E4B0}']
    function GetLoaders: IQServices; stdcall;
    function GetRouters: IQServices; stdcall;
    function GetServices: IQServices; stdcall;
    function Require(APath: PWideChar): IQService; stdcall;
    procedure Start; stdcall;
    function Stop: Boolean; stdcall;
    function Replace(ANewManager: IQPluginsManager): Boolean; stdcall;
    property Services: IQServices read GetServices;
    property Router: IQServices read GetRouters;
    property Loaders: IQServices read GetLoaders;
  end;

  // 下面是Delphi对上述接口的实现

  // 重新实现 TInterfacedObject，以便子类能够重载 QueryInterface 方法
  TQInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
  public
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGuid; out Obj): HRESULT;
      virtual; stdcall;
  end;

  // Delphi 的流接口与 IQStream 之间的相互转换

  TQStream = class(TStream)
  protected
    FStream: IQStream;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AStream: IQStream); overload;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream); overload;
    procedure LoadFromStream(AStream: IQStream); overload;
    procedure LoadFromFile(const AFileName: QStringW);
    procedure SaveToStream(AStream: TStream); overload;
    procedure SaveToStream(AStream: IQStream); overload;
    procedure SaveToFile(const AFileName: QStringW);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TQStreamHelper = class(TQInterfacedObject, IQStream)
  protected
    FStream: TStream;
    FOwnStream: Boolean;
    function Read(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Write(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Seek(AOffset: Int64; AFrom: BYTE): Int64; stdcall;
    procedure SetSize(ANewSize: UInt64); stdcall;
    function CopyFrom(AStream: IQStream; ACount: Int64): Int64; stdcall;
  public
    constructor Create(AStream: TStream; AOwnStream: Boolean);
    destructor Destroy; override;
  end;

  // 参数接口的实现
  TQParams = class;

  TQParam = class(TQInterfacedObject, IQParam)
  protected
    FName: QStringW;
    FType: TQParamType;
    FValue: TQValue;
    FOwner: TQParams;
    function GetName: PWideChar; stdcall;
    function GetAsInteger: Integer; stdcall;
    procedure SetAsInteger(const AValue: Integer); stdcall;
    function GetAsInt64: Int64; stdcall;
    procedure SetAsInt64(const AValue: Int64); stdcall;
    function GetAsBoolean: Boolean; stdcall;
    procedure SetAsBoolean(const AValue: Boolean); stdcall;
    function GetAsSingle: Single; stdcall;
    procedure SetAsSingle(const AValue: Single); stdcall;
    function GetAsFloat: Double; stdcall;
    procedure SetAsFloat(const AValue: Double); stdcall;
    function GetAsString(ABuf: PWideChar; ABufLen: Cardinal): Cardinal; stdcall;
    procedure SetAsString(const AValue: PWideChar); stdcall;
    function GetAsGuid: TGuid; stdcall;
    procedure SetAsGuid(const Value: TGuid); stdcall;
    function GetAsBytes(ABuf: PByte; ABufLen: Cardinal): Cardinal; stdcall;
    procedure SetAsBytes(ABuf: PByte; ABufLen: Cardinal); stdcall;
    function GetAsInterface: IInterface; stdcall;
    procedure SetAsInterface(AIntf: IInterface); stdcall;
    function GetIsNull: Boolean; stdcall;
    procedure SetNull; stdcall;
    function GetAsArray: IQParams; stdcall;
    function GetAsStream: IQStream; stdcall;
    function GetAsPointer: Pointer; stdcall;
    procedure SetAsPointer(AValue: Pointer); stdcall;
    function GetParent: IQParams; stdcall;
    function GetSize: Integer; stdcall;
    function GetType: TQParamType; stdcall;
    procedure SetType(const AType: TQParamType); stdcall;
    procedure SetSize(ASize: Integer); stdcall;
    function GetAsUString: QStringW;
    procedure SetAsUString(const Value: QStringW);
    function GetAsDBytes: TBytes;
    procedure SetAsDBytes(const Value: TBytes);
  public
    constructor Create(AOwner: TQParams); overload;
    destructor Destroy; override;
    property Name: QStringW read FName write FName;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsString: QStringW read GetAsUString write SetAsUString;
    property AsGuid: TGuid read GetAsGuid write SetAsGuid;
    property AsBytes: TBytes read GetAsDBytes write SetAsDBytes;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property IsNull: Boolean read GetIsNull;
    property AsArray: IQParams read GetAsArray;
    property AsStream: IQStream read GetAsStream;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property Parent: IQParams read GetParent;
    property Size: Integer read GetSize write SetSize;
    property DataType: TQParamType read FType write SetType;
  end;

  TQParams = class(TQInterfacedObject, IQParams)
  protected
    function GetItems(AIndex: Integer): IQParam; stdcall;
    function GetCount: Integer; stdcall;
    function ByName(const AName: PWideChar): IQParam; stdcall;
    function ByPath(APath: PWideChar): IQParam; stdcall;
    function Add(const AName: PWideChar; AParamType: TQParamType)
      : IQParam; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Clear; stdcall;
    procedure SaveToStream(AStream: IQStream); stdcall;
    procedure LoadFromStream(AStream: IQStream); stdcall;
    procedure SaveToFile(const AFileName: PWideChar); stdcall;
    procedure LoadFromFile(const AFileName: PWideChar); stdcall;
  end;

function QStream(AStream: TStream; AStreamOwner: Boolean): IQStream; overload;
function QStream(AStream: IQStream): TQStream; overload;

const
  ParamTypeNames: array [TQParamType] of QStringW = ('Unknown',
    // Integer Types
    'Shortint', 'Byte', 'Smallint', 'Word', 'Integer', 'DWord', 'Int64',
    'UInt64', 'Single', 'Double', // Float types
    'DateTime', 'Interval', // DateTime types
    'AnsiString', 'Utf8String', 'UnicodeString', // String types
    'Bool', // Boolean
    'Guid', // Guid
    'Binary', // Binary
    'Pointer', // untyped pointer
    'Interface', // Interface
    'Stream', // 流
    'Array' // Array
    );

implementation

resourcestring
  SCantConvert = '无法转换类型 %s 的值到 %s 。';

  { TQInterfacedObject }

function TQInterfacedObject.QueryInterface(

  const IID: TGuid; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TQInterfacedObject._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TQInterfacedObject._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    FreeAndNil(Self);
end;

{ TQStream }

constructor TQStream.Create(AStream: IQStream);
begin
  inherited Create;
  FStream := AStream;
end;

destructor TQStream.Destroy;
begin
  FStream := nil;
  inherited;
end;

procedure TQStream.LoadFromFile(

  const AFileName: QStringW);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQStream.LoadFromStream(AStream: IQStream);
begin
  FStream.CopyFrom(AStream, AStream.Seek(0, soFromEnd) - AStream.Seek(0,
    soFromCurrent));
end;

procedure TQStream.LoadFromStream(AStream: TStream);
begin
  CopyFrom(AStream, AStream.Size - AStream.Position);
end;

function TQStream.Read(

  var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(@Buffer, Count);
end;

procedure TQStream.SaveToFile(

  const AFileName: QStringW);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQStream.SaveToStream(AStream: IQStream);
begin
  AStream.CopyFrom(FStream, Size - Position);
end;

procedure TQStream.SaveToStream(AStream: TStream);
begin
  AStream.CopyFrom(Self, Size - Position);
end;

function TQStream.Seek(

  const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, BYTE(Origin));
end;

procedure TQStream.SetSize(

  const NewSize: Int64);
begin
  FStream.SetSize(NewSize);
end;

function TQStream.Write(

  const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(@Buffer, Count);
end;

{ TQStreamHelper }

function TQStreamHelper.CopyFrom(AStream: IQStream; ACount: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N, W: Integer;
  Buffer: TBytes;
begin
  if ACount <= 0 then
  begin
    ACount := AStream.Seek(0, soFromEnd);
    AStream.Seek(0, soFromBeginning);
  end;
  Result := ACount;
  if ACount > MaxBufSize then
    BufSize := MaxBufSize
  else
    BufSize := ACount;
  SetLength(Buffer, BufSize);
  try
    while ACount <> 0 do
    begin
      if ACount > BufSize then
        N := BufSize
      else
        N := ACount;
      N := AStream.Read(@Buffer[0], N);
      W := 0;
      while N > W do
        Inc(W, Write(@Buffer[W], N - W));
      Dec(ACount, N);
    end;
  finally
    SetLength(Buffer, 0);
  end;
end;

constructor TQStreamHelper.Create(AStream: TStream; AOwnStream: Boolean);
begin
  inherited Create;
  FStream := AStream;
  FOwnStream := AOwnStream;
end;

destructor TQStreamHelper.Destroy;
begin
  if FOwnStream then
    FreeAndNil(FStream);
  inherited;
end;

function TQStreamHelper.Read(pv: Pointer; cb: Cardinal): Cardinal;
begin
  Result := FStream.Read(pv^, cb);
end;

function TQStreamHelper.Seek(AOffset: Int64; AFrom: BYTE): Int64;
begin
  Result := FStream.Seek(AOffset, TSeekOrigin(AFrom));
end;

procedure TQStreamHelper.SetSize(ANewSize: UInt64);
begin
  FStream.Size := ANewSize;
end;

function TQStreamHelper.Write(pv: Pointer; cb: Cardinal): Cardinal;
begin
  Result := FStream.Write(pv^, cb);
end;

function QStream(AStream: TStream; AStreamOwner: Boolean): IQStream;
begin
  Result := TQStreamHelper.Create(AStream, AStreamOwner);
end;

function QStream(AStream: IQStream): TQStream;
begin
  Result := TQStream.Create(AStream);
end;

{ TQParam }

constructor TQParam.Create(AOwner: TQParams);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TQParam.Destroy;
begin
  SetNull;
  inherited;
end;

function TQParam.GetAsArray: IQParams;
begin
  if FType = ptArray then
    Result := TQParams(FValue.Value.AsPointer)
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptArray]]);
end;

function TQParam.GetAsBoolean: Boolean;
begin
  case FType of
    ptBoolean:
      Result := FValue.Value.AsBoolean;
    ptInt8, ptUInt8, ptInt16, ptUInt16, ptInt32:
      Result := FValue.Value.AsInteger <> 0;
    ptUInt32, ptInt64, ptUInt64:
      Result := FValue.Value.AsInt64 <> 0;
    ptFloat4:
      Result := not IsZero(FValue.Value.AsSingle);
    ptFloat8:
      Result := not IsZero(FValue.Value.AsFloat);
    ptDateTime:
      Result := not IsZero(FValue.Value.AsFloat);
    ptInterval:
      Result := not FValue.Value.AsInterval.IsZero;
    ptAnsiString, ptUtf8String, ptUnicodeString:
      begin
        if not TryStrToBool(FValue.Value.AsString^, Result) then
          raise QException.CreateFmt(SCantConvert,
            [ParamTypeNames[FType], ParamTypeNames[ptBoolean]]);
      end;
    ptPointer, ptInterface:
      Result := FValue.Value.AsPointer <> nil
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptBoolean]]);
  end;
end;

function TQParam.GetAsBytes(ABuf: PByte; ABufLen: Cardinal): Cardinal;
const
  FixedSizes: array [TQParamType] of Integer = (-1,
    // Integer Types
    1, 1, 2, 2, 4, 4, 8, 8, 4, 8, // Float types
    8, 8, // DateTime types
    -1, -1, -1, // String types
    1, // Boolean
    -1, // Guid
    -1, // Binary
    -1, // untyped pointer
    -1, // Interface
    -1, // 流
    -1 // Array
    );
  procedure AsAnsiString;
  var
    S: QStringA;
  begin
    S := qstring.AnsiEncode(FValue.Value.AsString^);
    if (ABufLen = 0) or (ABuf = nil) then
      Result := S.Length
    else
    begin
      if ABufLen > S.Length then
        Result := S.Length
      else
        Result := ABufLen;
      Move(PQCharA(S)^, ABuf^, Result);
    end;
  end;

  procedure AsUtf8String;
  var
    S: QStringA;
  begin
    S := qstring.Utf8Encode(FValue.Value.AsString^);
    if (ABufLen = 0) or (ABuf = nil) then
      Result := S.Length
    else
    begin
      if ABufLen > S.Length then
        Result := S.Length
      else
        Result := ABufLen;
      Move(PQCharA(S)^, ABuf^, Result);
    end;
  end;

  procedure AsUnicodeString;
  begin
    if (ABufLen = 0) or (ABuf = nil) then
      Result := Length(FValue.Value.AsString^) shl 1
    else
    begin
      if ABufLen > (Length(FValue.Value.AsString^) shl 1) then
        Result := Length(FValue.Value.AsString^) shl 1
      else
        Result := ABufLen;
      Move(PQCharW(FValue.Value.AsString^)^, ABuf^, Result);
    end;
  end;
  procedure AsGuid;
  begin
    if (ABufLen = 0) or (ABuf = nil) then
      Result := SizeOf(TGuid)
    else
    begin
      if ABufLen > SizeOf(TGuid) then
        Result := SizeOf(TGuid)
      else
        Result := ABufLen;
      Move(FValue.Value.AsGuid^, ABuf^, Result);
    end;
  end;

  procedure AsBytes;
  begin
    if (ABufLen = 0) or (ABuf = nil) then
      Result := FValue.Value.AsStream.Size
    else
    begin
      if ABufLen > FValue.Value.AsStream.Size then
        Result := FValue.Value.AsStream.Size
      else
        Result := ABufLen;
      Move(FValue.Value.AsStream.Memory^, ABuf^, Result);
    end;
  end;

begin
  if FixedSizes[FType] <> -1 then
  begin
    if (ABufLen = 0) or (ABuf = nil) then
      Result := FixedSizes[FType]
    else
    begin
      if ABufLen >= FixedSizes[FType] then
        Result := FixedSizes[FType]
      else
        Result := ABufLen;
      Move(FValue.Value, ABuf^, Result);
    end;
  end
  else
  begin
    case FType of
      ptAnsiString:
        AsAnsiString;
      ptUtf8String:
        AsUtf8String;
      ptUnicodeString:
        AsUnicodeString;
      ptGuid:
        AsGuid;
      ptBytes, ptStream:
        AsBytes
    else
      raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
        ParamTypeNames[ptBytes]]);
    end;
  end;
end;

function TQParam.GetAsDBytes: TBytes;
begin
  case FType of
    ptInt8, ptUInt8, ptBoolean:
      begin
        SetLength(Result, 1);
        Result[0] := FValue.Value.AsShort;
      end;
    ptInt16, ptUInt16:
      begin
        SetLength(Result, 2);
        PSmallint(@Result[0])^ := FValue.Value.AsSmallint;
      end;
    ptInt32, ptUInt32, ptFloat4:
      begin
        SetLength(Result, 4);
        PInteger(@Result[0])^ := FValue.Value.AsInteger;
      end;
    ptInt64, ptUInt64, ptFloat8, ptDateTime, ptInterval:
      begin
        SetLength(Result, 8);
        PInt64(@Result[0])^ := FValue.Value.AsInt64;
      end;
    ptAnsiString:
      Result := qstring.AnsiEncode(FValue.Value.AsString^);
    ptUtf8String:
      Result := qstring.Utf8Encode(FValue.Value.AsString^);
    ptUnicodeString:
      begin
        SetLength(Result, Length(FValue.Value.AsString^) shl 1);
        if Length(Result) > 0 then
          Move(PWideChar(FValue.Value.AsString^)^, Result[0], Length(Result));
      end;
    ptGuid:
      begin
        SetLength(Result, SizeOf(TGuid));
        PGuid(@Result[0])^ := FValue.Value.AsGuid^;
      end;
    ptBytes, ptStream:
      begin
        SetLength(Result, FValue.Value.AsStream.Size);
        if Length(Result) > 0 then
          Move(FValue.Value.AsStream.Memory^, Result[0], Length(Result));
      end
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptBytes]]);
  end;
end;

function TQParam.GetAsFloat: Double;
begin
  case FType of
    ptInt8:
      Result := FValue.Value.AsShort;
    ptUInt8:
      Result := FValue.Value.AsByte;
    ptInt16:
      Result := FValue.Value.AsSmallint;
    ptUInt16:
      Result := FValue.Value.AsWord;
    ptInt32:
      Result := FValue.Value.AsInteger;
    ptUInt32:
      Result := FValue.Value.AsInt64;
    ptInt64:
      Result := FValue.Value.AsInt64;
    ptUInt64:
      Result := UInt64(FValue.Value.AsInt64);
    ptFloat4:
      Result := FValue.Value.AsSingle;
    ptFloat8, ptDateTime:
      Result := FValue.Value.AsFloat;
    ptAnsiString, ptUtf8String, ptUnicodeString:
      if not TryStrToFloat(FValue.Value.AsString^, Result) then
        raise QException.CreateFmt(SCantConvert,
          [ParamTypeNames[FType], ParamTypeNames[ptFloat8]]);
    ptBoolean:
      Result := Integer(FValue.Value.AsBoolean)
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptFloat8]]);
  end;
end;

function TQParam.GetAsGuid: TGuid;
begin
  if FType = ptGuid then
    Result := FValue.Value.AsGuid^
  else if FType in [ptAnsiString, ptUtf8String, ptUnicodeString] then
  begin
    if not TryStrToGuid(FValue.Value.AsString^, Result) then
      raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
        ParamTypeNames[ptGuid]]);
  end
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptGuid]]);
end;

function TQParam.GetAsInt64: Int64;
begin
  case FType of
    ptInt8:
      Result := FValue.Value.AsShort;
    ptUInt8:
      Result := FValue.Value.AsByte;
    ptInt16:
      Result := FValue.Value.AsSmallint;
    ptUInt16:
      Result := FValue.Value.AsWord;
    ptInt32:
      Result := FValue.Value.AsInteger;
    ptUInt32:
      Result := Cardinal(FValue.Value.AsInteger);
    ptInt64, ptUInt64:
      Result := FValue.Value.AsInt64;
    ptFloat4:
      Result := Trunc(FValue.Value.AsSingle);
    ptFloat8, ptDateTime:
      Result := Trunc(FValue.Value.AsFloat);
    ptInterval:
      Result := FValue.Value.AsInt64;
    ptAnsiString, ptUtf8String, ptUnicodeString:
      begin
        if not TryStrToInt64(FValue.Value.AsString^, Result) then
          raise QException.CreateFmt(SCantConvert,
            [ParamTypeNames[FType], ParamTypeNames[ptInt64]]);
      end;
    ptBoolean:
      Result := Integer(FValue.Value.AsBoolean)
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptInt64]]);
  end;
end;

function TQParam.GetAsInteger: Integer;
begin
  Result := AsInt64;
end;

function TQParam.GetAsInterface: IInterface;
begin
  if FType = ptInterface then
    Result := FValue.Value.AsPointer
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptInterface]]);
end;

function TQParam.GetAsPointer: Pointer;
begin
  if FType = ptPointer then
    Result := FValue.Value.AsPointer
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptInterface]]);
end;

function TQParam.GetAsSingle: Single;
begin
  Result := AsFloat;
end;

function TQParam.GetAsStream: IQStream;
begin
  if FType = ptStream then
    Result := QStream(FValue.Value.AsStream, False) // ????流应该归谁管？怎么管？
  else
    raise QException.CreateFmt(SCantConvert, [ParamTypeNames[FType],
      ParamTypeNames[ptInt64]]);
end;

function TQParam.GetAsString(ABuf: PWideChar; ABufLen: Cardinal): Cardinal;
begin

end;

function TQParam.GetAsUString: QStringW;
begin

end;

function TQParam.GetIsNull: Boolean;
begin

end;

function TQParam.GetName: PWideChar;
begin

end;

function TQParam.GetParent: IQParams;
begin

end;

function TQParam.GetSize: Integer;
begin

end;

function TQParam.GetType: TQParamType;
begin

end;

procedure TQParam.SetAsBoolean(

  const AValue: Boolean);
begin

end;

procedure TQParam.SetAsBytes(ABuf: PByte; ABufLen: Cardinal);
begin

end;

procedure TQParam.SetAsDBytes(const Value: TBytes);
begin

end;

procedure TQParam.SetAsFloat(

  const AValue: Double);
begin

end;

procedure TQParam.SetAsGuid(

  const Value: TGuid);
begin

end;

procedure TQParam.SetAsInt64(

  const AValue: Int64);
begin

end;

procedure TQParam.SetAsInteger(

  const AValue: Integer);
begin

end;

procedure TQParam.SetAsInterface(AIntf: IInterface);
begin

end;

procedure TQParam.SetAsPointer(AValue: Pointer);
begin

end;

procedure TQParam.SetAsSingle(

  const AValue: Single);
begin

end;

procedure TQParam.SetAsString(

  const AValue: PWideChar);
begin

end;

procedure TQParam.SetAsUString(const Value: QStringW);
begin

end;

procedure TQParam.SetNull;
begin

end;

procedure TQParam.SetSize(ASize: Integer);
begin

end;

procedure TQParam.SetType(const AType: TQParamType);
begin

end;

end.
