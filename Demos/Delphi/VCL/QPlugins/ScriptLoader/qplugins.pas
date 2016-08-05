unit qplugins;

interface

uses classes, sysutils, types, syncobjs, qstring, qtimetypes, qvalue,
  qpluginsintf, qrbtree,
  qjson{$IFDEF UNICODE}, system.Rtti,
  system.Generics.collections{$ENDIF};

const
  LA_STARTING = $00;
  LA_CANSTOP = $01;
  LA_STOPING = $02;
  QPID_LOADERS: TGuid = '{48A35E1E-A2C8-4D3A-93FB-00E9659E8C23}';
  QPID_ROUTERS: TGuid = '{1463C870-1BC9-48EA-8E2A-0E4F45FECF78}';
  QPID_SERVICES: TGuid = '{63BE03F1-4778-4D98-9445-2C8285B6395E}';

type
  { QPlugins 工作过程
    1.宿主
    1.1 宿主创建 IQPluginsManager 的全局实例，该实例在主程序和动态链接库之间共享；
    1.2 通过引用实现了 IQPluginsLoader 的不同单元引入默认的加载器；
    1.3 调用 IQPluginsLoader.Execute(plrLoading,...) 函数来完成其它加载器和插件的注册工作；
    2.插件
    每个插件可以提供0到多个服务，QPlugins 工作在服务级别.
    2.1 加载器(Loader)
    如何生成具体的服务实例并注册到 IQPluginsManager 是加载器的任务。加载器本身也是一个服务，
    但它由宿主程序调用它的 Execute 方法来实现，这是明确约定的。
    加载器做为特殊服务，我们为其约定特殊的扩展名为 ".ldr"，程序启动时，扫描目录下的 *.ldr，
    并调用其 DLLEntry，由其自行完成自身的注册，然后等待宿主调用其 Execute 方法来注册扫描插件。

    2.2 普通服务
    一个服务可以由一到多个插件提供不同的版本，不同的版本之间通过不同的ID进行区分。插件需要
    某项服务时，向 IQPluginsManager 来查询，并返回对应的实例接口。
    3.路由
    多个插件提供相同的服务时，我们可以通过 IQPluginsRouter 接口来做路由控制。当我们向
    IQPluginManager 查询某一服务时，它会通过路由表来查找特定的实例。
  }

  // 下面是一些核心对象的默认实现
  TQParam = class;
  TQParams = class;

{$IFDEF UNICODE}
  TQParamList = TList<TQParam>;
{$ELSE}
  TQParamList = TList;
{$ENDIF}

  TQInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
  public
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGuid; out Obj): HRESULT;
      virtual; stdcall;
  end;

  TQParam = class(TQInterfacedObject, IQParam)
  private
  protected
    FName: QStringW;
    FDataType: TQParamType;
    FValue: TQValue;
    FParent: TQParams;
    FParamType: TQParamType;
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
    function GetAsString(ABuf: PWideChar; ABufLen: Integer): Integer; stdcall;
    procedure SetAsString(const AValue: PWideChar); stdcall;
    function GetAsBytes(ABuf: PByte; ABufLen: Integer): Integer; stdcall;
    procedure SetAsBytes(ABuf: PByte; ABufLen: Integer); stdcall;
    function GetAsInterface: IInterface; stdcall;
    procedure SetAsInterface(AIntf: IInterface); stdcall;
    function GetAsArray: IQParams; stdcall;
    function GetParent: IQParams; stdcall;
    function GetValueBytes: TBytes;
    procedure SetValueBytes(const Value: TBytes);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    function GetAsGuid: TGuid; stdcall;
    procedure SetAsGuid(const Value: TGuid); stdcall;
    function GetValueString: QStringW;
    procedure SetValueString(const Value: QStringW);
    function GetIsNull: Boolean; stdcall;
    procedure SetNull; stdcall;
    function GetSize: Integer; stdcall;
    procedure SetSize(ASize: Integer); stdcall;
    function GetAsStream: IQStream; stdcall;
    procedure SetParamType(const Value: TQParamType);
    function GetAsParams: TQParams;
    function GetType: TQParamType; stdcall;
    function GetAsPointer: Pointer; stdcall;
    procedure SetAsPointer(AValue: Pointer); stdcall;
    function GetAsValue: TValue;
    procedure SetAsValue(const Value: TValue);
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Name: QStringW read FName write FName;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: QStringW read GetValueString write SetValueString;
    property AsBytes: TBytes read GetValueBytes write SetValueBytes;
    property AsGuid: TGuid read GetAsGuid write SetAsGuid;
    property AsArray: IQParams read GetAsArray;
    property AsParams: TQParams read GetAsParams;
    property AsStream: IQStream read GetAsStream;
    property Parent: IQParams read GetParent;
    property DataType: TQParamType read FParamType write SetParamType;
{$IFDEF UNICODE}
    property AsValue: TValue read GetAsValue write SetAsValue;
{$ENDIF}
  end;

  TQParams = class(TQInterfacedObject, IQParams)
  protected
    FItems: TQParamList;
    function GetItems(AIndex: Integer): IQParam; stdcall;
    function GetChildItems(AIndex: Integer): TQParam;
    function GetCount: Integer; stdcall;
    function GetSize: Integer; stdcall;
    procedure InternalEncode(ABuilder: TQStringCatHelperW; AParams: TQParams);
    procedure InternalCopy(ASourceList, ADestList: IQParams);
    function GetAsJson: QStringW;
  public
    constructor Create;
    destructor Destroy; override;
    // Warn:注意CopyTo/CopyFrom前必需自行管理引用计数，否则可能会被意外释放
    procedure CopyTo(ATarget: IQParams);
    procedure CopyFrom(ASource: IQParams);
    function ParamByName(const AName: PWideChar): IQParam; stdcall;
    function ParamByPath(APath: PWideChar): IQParam; stdcall;
    function Add(AType: TQParamType): TQParam; overload;
    function Add(const AName: PWideChar; AParamType: TQParamType): IQParam;
      overload; stdcall;
    function Add(const AName: QStringW; AValue: Boolean): TQParam; overload;
    function Add(const AName: QStringW; AValue: Int64): TQParam; overload;
    function Add(const AName: QStringW; AValue: Double): TQParam; overload;
    function AddDateTime(const AName: QStringW; AValue: TDateTime): TQParam;
    function Add(const AName, AValue: QStringW): TQParam; overload;
    function Add(const AName: QStringW; const AValue: TGuid): TQParam; overload;
    function Add(const AName: QStringW; const AValue: TBytes): TQParam;
      overload;
    function AddArray(const AName: QStringW): TQParams; overload;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Clear; stdcall;
    function AsRecord(AlignSize: Byte): TBytes;
    procedure SaveToStream(AStream: IQStream); overload; stdcall;
    procedure SaveToStream(AStream: TStream); overload;
    procedure LoadFromStream(AStream: IQStream); overload; stdcall;
    procedure LoadFromStream(AStream: TStream); overload;
    procedure SaveToFile(const AFileName: PWideChar); stdcall;
    procedure LoadFromFile(const AFileName: PWideChar); stdcall;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TQParam read GetChildItems; default;
    property AsJson: QStringW read GetAsJson;
  end;

  TQStream = class(TStream)
  protected
    FStream: IQStream;
  public
    constructor Create(AStream: IQStream); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TQPluginServiceItem = record
    Id: TGuid;
    Name: QStringW;
    NameHash: Cardinal;
    Handler: IQPluginService;
  end;

  PQPluginServiceItem = ^TQPluginServiceItem;
{$IFDEF UNICODE}
  TQServiceList = TList<PQPluginServiceItem>;
{$ELSE}
  TQServiceList = TList;
{$ENDIF}

  TQPluginService = class(TQInterfacedObject, IQPluginService)
  protected
    FId: TGuid;
    FName: QStringW;
    FAttrs: TQParams;
    FParent: IQPluginServices;
    FLastError: Integer;
    FLastErrorMsg: QStringW;
    FLoader: IQPluginsLoader;
    FTag: IntPtr;
    function Execute(const AParams: IQParams; AResult: IQParams): Boolean;
      virtual; stdcall;
    function GetParent: IQPluginServices; stdcall;
    procedure SetParent(AParent: IQPluginServices); stdcall;
    function GetName: PWideChar; stdcall;
    function GetAttributes: IQParams; stdcall;
    function SaveStates(AParams: IQParams): Boolean; virtual; stdcall;
    function LoadStates(AParams: IQParams): Boolean; virtual; stdcall;
    function Terminate(AWait: Boolean): Boolean; virtual; stdcall;
    function GetLoader: IQPluginsLoader; stdcall;
    function GetLastError: Integer; stdcall;
    function GetLastErrorMsg: PWideChar; stdcall;
    function NewInstance: IQPluginService; virtual; stdcall;
    function GetId: TGuid; stdcall;
    procedure SetLastError(ACode: Integer; const S: QStringW);
  public
    constructor Create(const AId: TGuid; const AName: QStringW);
      overload; virtual;
    constructor Create(const AName: QStringW); overload;
    destructor Destroy; override;
    property Id: TGuid read FId;
    property Parent: IQPluginServices read FParent;
    property Name: QStringW read FName;
    property Attrs: IQParams read GetAttributes;
    property LastError: Integer read FLastError;
    property LastErrorMsg: QStringW read FLastErrorMsg;
    property Loader: IQPluginsLoader read GetLoader;
    property Tag: IntPtr read FTag write FTag;
  end;

  TQPluginServices = class(TQPluginService, IQPluginServices)
  protected
    FItems: TQServiceList;
    FLocker: TCriticalSection;
    function GetItems(AIndex: Integer): IQPluginService; stdcall;
    function GetCount: Integer; stdcall;
    function Execute(const AParams: IQParams; AResult: IQParams): Boolean;
      override; stdcall;
    function IndexById(const AId: TGuid): Integer;
    function IndexOf(AItem: IQPluginService): Integer; virtual; stdcall;
    constructor Create; overload;
  public
    constructor Create(const AId: TGuid; const AName: QStringW); override;
    destructor Destroy; override;
    function ByPath(APath: PWideChar): IQPluginService; virtual; stdcall;
    function ById(const AId: TGuid): IQPluginService; virtual; stdcall;
    function Add(AItem: IQPluginService): Integer; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Remove(AItem: IQPluginService); stdcall;
    procedure Clear; stdcall;
    procedure Lock; stdcall;
    procedure Unlock; stdcall;
    function SaveStates(AParams: IQParams): Boolean; override; stdcall;
    function LoadStates(AParams: IQParams): Boolean; override; stdcall;
    property Items[AIndex: Integer]: IQPluginService read GetItems; default;
    property Count: Integer read GetCount;
  end;
{$IFDEF UNICODE}

  TQPluginRouteRules = TList<PQPluginRouteRule>;
{$ELSE}
  TQPluginRouteRules = TList;
{$ENDIF}

  TQPluginsSimpleRouter = class(TQPluginService, IQPluginsRouter)
  protected
    FLocker: TCriticalSection;
    FHashList: TQHashTable;
    FItems: TQPluginRouteRules;
    function GetCount: Integer; stdcall;
    function GetItems(AIndex: Integer): PQPluginRouteRule; stdcall;
    function NewRule(AByPath: Boolean): PQPluginRouteRule;
  public
    constructor Create(const AId: TGuid; const AName: QStringW); override;
    destructor Destroy; override;
    procedure AddPath(const ASourcePath, ATargetPath: PWideChar); stdcall;
    function AddMap(const ASourcePath, ATargetPath: PWideChar)
      : Boolean; stdcall;
    function AddId(const ASourceId, ATargetId: TGuid): Boolean; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Clear; stdcall;
    function ByPath(APath: PWideChar): IQPluginService; stdcall;
    function ById(const AId: TGuid): IQPluginService; stdcall;
  end;

  // 插件管理中心，管理所有的插件及其提供的服务
function PluginsManager: IQPluginsManager;
function QStream(AStream: TStream; AStreamOwner: Boolean): IQStream;
function FirstChildService(AService: IQPluginService): IQPluginService;

implementation

uses {$IFDEF MSWINDOWS}windows{$ELSE}Posix.SysSocket{$ENDIF};

resourcestring
  SUnsupportNow = '当前不支持此功能。';

var
  _PluginsManager: IQPluginsManager = nil;

type
  TQStreamHelper = class(TQInterfacedObject, IQStream)
  protected
    FStream: TStream;
    FOwnStream: Boolean;
    function Read(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Write(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Seek(AOffset: Int64; AFrom: Byte): Int64; stdcall;
    procedure SetSize(ANewSize: UInt64); stdcall;
    function CopyFrom(AStream: IQStream; ACount: Int64): Int64; stdcall;
  public
    constructor Create(AStream: TStream; AOwnStream: Boolean);
    destructor Destroy; override;
  end;

{$IFDEF MSWINDOWS}

function OpenManager: IQPluginsManager;
var
  AName: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF};
  AHandle: THandle;
  P: Pointer;
begin
  AName := 'Local\QPlugins_Manager_PID_' + IntToStr(GetCurrentProcessId);
  AHandle := OpenFileMappingW(FILE_MAP_READ, False, PWideChar(AName));
  if AHandle = 0 then
  begin
    Result := nil
  end
  else
  begin
    P := MapViewOfFile(AHandle, FILE_MAP_READ, 0, 0, 0);
    Result := IUnknown(PPointer(P)^) as IQPluginsManager;
    UnmapViewOfFile(P);
    { Close the file mapping }
    CloseHandle(AHandle);
  end;
end;
{$ENDIF}

function PluginsManager: IQPluginsManager;
begin
  if not Assigned(_PluginsManager) then
    _PluginsManager := OpenManager;
  Result := _PluginsManager;
end;

function FirstChildService(AService: IQPluginService): IQPluginService;
var
  AList: IQPluginServices;
begin
  if Assigned(AService) then
  begin
    if (AService.QueryInterface(IQPluginServices, AList) = S_OK) then
    begin
      if AList.Count > 0 then
        Result := FirstChildService(AList[0]).NewInstance
      else
        Result := nil;
    end
    else
      Result := AService.NewInstance;
  end;
end;

{ TQPluginServices }

function TQPluginServices.Add(AItem: IQPluginService): Integer;
var
  AData: PQPluginServiceItem;
begin
  Lock;
  try
    Result := IndexById(AItem.Id);
    if (Result = -1) or (Items[Result] <> AItem) then
    begin
      New(AData);
      AData.Id := AItem.Id;
      AData.Name := AItem.Name;
      AData.Handler := AItem;
      AData.NameHash := HashOf(PWideChar(AData.Name), Length(AData.Name) shl 1);
      Result := FItems.Add(AData);
      AItem.SetParent(Self);
    end;
  finally
    Unlock;
  end;
end;

function TQPluginServices.ById(const AId: TGuid): IQPluginService;
var
  I: Integer;
  AItem: PQPluginServiceItem;
  AServices: IQPluginServices;
  AParent: TQPluginServices;
  AIntf: IInterface;
begin
  Lock;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      AItem := PQPluginServiceItem(FItems[I]);
      if AItem.Id = AId then
      begin
        Result := PQPluginServiceItem(FItems[I]).Handler;
        Exit;
      end
      else if AItem.Handler.QueryInterface(AId, AIntf) = S_OK then
      begin
        Result := PQPluginServiceItem(FItems[I]).Handler;
        Exit;
      end
      else if AItem.Handler.QueryInterface(IQPluginServices, AServices) = S_OK
      then
      begin
        Result := AServices.ById(AId);
        if Assigned(Result) then
          Exit;
      end;
    end;
    Result := nil;
  finally
    Unlock;
  end;
end;

function TQPluginServices.ByPath(APath: PWideChar): IQPluginService;
var
  AName: QStringW;
  I: Integer;
  AItem: PQPluginServiceItem;
  AServices: IQPluginServices;
  AParent: TQPluginServices;
begin
  Result := nil;
  if APath <> nil then
  begin
    Lock;
    try
      AParent := Self;
      while (APath^ <> #0) and Assigned(AParent) do
      begin
        AName := DecodeTokenW(APath, '/', #0, True, True);
        for I := 0 to AParent.FItems.Count - 1 do
        begin
          AItem := AParent.FItems[I];
          if StrCmpW(PWideChar(AName), PWideChar(AItem.Name), True) = 0 then
          begin
            if APath^ <> #0 then
            begin
              if AItem.Handler.QueryInterface(IQPluginServices, AServices) = S_OK
              then
                AParent := AItem.Handler as TQPluginServices
              else
                AParent := nil;
            end
            else
              Result := AItem.Handler;
            Break;
          end
        end;
      end;
      if APath^ <> #0 then
        Result := nil;
    finally
      Unlock;
    end;
  end;
end;

procedure TQPluginServices.Clear;
var
  I: Integer;
  AItem: PQPluginServiceItem;
begin
  Lock;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      AItem := FItems[I];
      AItem.Handler := nil;
      Dispose(AItem);
    end;
    FItems.Clear;
  finally
    Unlock;
  end;
end;

constructor TQPluginServices.Create;
begin
  inherited Create;
  FItems := TQServiceList.Create;
  FLocker := TCriticalSection.Create;
end;

constructor TQPluginServices.Create(const AId: TGuid; const AName: QStringW);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FItems := TQServiceList.Create;
  FLocker := TCriticalSection.Create;
end;

procedure TQPluginServices.Delete(AIndex: Integer);
var
  AItem: PQPluginServiceItem;
begin
  Lock;
  try
    AItem := FItems[AIndex];
    AItem.Handler := nil;
    FItems.Delete(AIndex);
    Dispose(AItem);
  finally
    Unlock;
  end;
end;

destructor TQPluginServices.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FLocker);
  if Assigned(FAttrs) then
    FAttrs._Release;
  inherited;
end;

function TQPluginServices.Execute(const AParams: IQParams;
  AResult: IQParams): Boolean;
begin
  // Do nothing
  Result := True;
end;

function TQPluginServices.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQPluginServices.GetItems(AIndex: Integer): IQPluginService;
begin
  Result := PQPluginServiceItem(FItems[AIndex]).Handler;
end;

function TQPluginServices.IndexById(const AId: TGuid): Integer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    if PQPluginServiceItem(FItems[I]).Id = AId then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TQPluginServices.IndexOf(AItem: IQPluginService): Integer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    if PQPluginServiceItem(FItems[I]).Handler = AItem then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TQPluginServices.LoadStates(AParams: IQParams): Boolean;
var
  I: Integer;
  ASubParams: IQParams;
begin
  Lock;
  try
    for I := 0 to Count - 1 do
    begin
      // ASubParams:=AParams.Add(Items[I].GetName,ptArray).GetAsArray;

    end;
    Result := True;
  finally
    Unlock;
  end;
end;

procedure TQPluginServices.Lock;
begin
  FLocker.Enter;
end;

procedure TQPluginServices.Remove(AItem: IQPluginService);
var
  I: Integer;
  AData: PQPluginServiceItem;
begin
  Lock;
  try
    for I := 0 to FItems.Count - 1 do
    begin
      if Items[I] = AItem then
      begin
        AData := FItems[I];
        AData.Handler := nil;
        FItems.Delete(I);
        Dispose(AData);
        Break;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TQPluginServices.SaveStates(AParams: IQParams): Boolean;
var
  I: Integer;
  AParam: IQParam;
  AObjParams: IQParams;
  AService: PQPluginServiceItem;
begin
  Lock;
  try
    for I := 0 to Count - 1 do
    begin
      AService := FItems[I];
      AParam := AParams.Add(PWideChar(AService.Name), ptArray);
      AObjParams := AParam.GetAsArray;
      AObjParams.Add('Id', ptGuid).SetAsGuid(AService.Id);
      AService.Handler.SaveStates(AParam.GetAsArray);
    end;
    if Assigned(FAttrs) then
      FAttrs.CopyTo(AParams.Add('Attrs', ptArray).GetAsArray);
    Result := True;
  finally
    Unlock;
  end;
end;

procedure TQPluginServices.Unlock;
begin
  FLocker.Leave;
end;

{ TQParam }

constructor TQParam.Create;
begin
  inherited;
  FDataType := ptUnknown;
end;

destructor TQParam.Destroy;
begin
  SetNull;
  inherited;
end;

function TQParam.GetAsArray: IQParams;
begin
  if FDataType = ptArray then
    Result := IQParams(FValue.Value.AsPointer)
  else
    Result := nil;
end;

function TQParam.GetAsBoolean: Boolean;
begin
  Result := FValue.AsBoolean;
end;

function TQParam.GetAsBytes(ABuf: PByte; ABufLen: Integer): Integer;
var
  ABytes: TBytes;
begin
  ABytes := FValue.AsBytes;
  if ABufLen > 0 then
  begin
    if ABufLen > Length(ABytes) then
      Result := Length(ABytes)
    else
      Result := ABufLen;
    Move(ABytes[0], ABuf^, Result);
  end
  else
    Result := Length(ABytes);
end;

function TQParam.GetAsDateTime: TDateTime;
begin
  Result := FValue.AsDateTime;
end;

function TQParam.GetAsFloat: Double;
begin
  Result := FValue.AsFloat;
end;

function TQParam.GetAsGuid: TGuid;
begin
  Result := FValue.AsGuid;
end;

function TQParam.GetAsInt64: Int64;
begin
  Result := FValue.AsInt64;
end;

function TQParam.GetAsInteger: Integer;
begin
  Result := FValue.AsInteger;
end;

function TQParam.GetAsInterface: IInterface;
begin
  Result := IInterface(FValue.Value.AsPointer)
end;

function TQParam.GetAsParams: TQParams;
begin
  if FParamType = ptArray then
    Result := IInterface(FValue.Value.AsPointer) as TQParams
  else
    Result := nil;
end;

function TQParam.GetAsPointer: Pointer;
begin
  if FParamType in [ptPointer, ptInterface, ptStream, ptArray] then
    Result := FValue.Value.AsPointer
  else
    Result := nil;
end;

function TQParam.GetAsSingle: Single;
begin
  Result := FValue.AsSingle;
end;

function TQParam.GetAsStream: IQStream;
begin
  if FParamType = ptStream then
    Result := QStream(FValue.Value.AsStream, False)
  else
    Result := nil;
end;

function TQParam.GetAsString(ABuf: PWideChar; ABufLen: Integer): Integer;
var
  S: QStringW;
begin
  S := FValue.AsString;
  if ABufLen > 0 then
  begin
    if ABufLen > Length(S) then
      Result := Length(S)
    else
      Result := ABufLen;
    Move(PQCharW(S)^, ABuf^, Result shl 1);
  end
  else
    Result := Length(S);
end;

function TQParam.GetAsValue: TValue;
begin
  
end;

function TQParam.GetIsNull: Boolean;
begin
  Result := FValue.IsNull;
end;

function TQParam.GetName: PWideChar;
begin
  Result := PWideChar(FName);
end;

function TQParam.GetParent: IQParams;
begin
  Result := FParent;
end;

function TQParam.GetSize: Integer;
begin
  case FParamType of
    ptInt8, ptUInt8, ptBoolean:
      Result := 1;
    ptInt16, ptUInt16:
      Result := 2;
    ptInt32, ptUInt32, ptFloat4:
      Result := 4;
    ptInt64, ptUInt64, ptFloat8, ptDateTime, ptInterval:
      Result := 8;
    ptAnsiString:
      Result := qstring.AnsiEncode(AsString).Length;
    ptUtf8String:
      Result := qstring.Utf8Encode(AsString).Length;
    ptUnicodeString:
      Result := Length(AsString);
    ptBytes:
      Result := FValue.Size;
    ptGuid:
      Result := 16;
    ptArray:
      Result := TQParams(FValue.Value.AsPointer).GetSize
  else
    Result := 0;
  end;
end;

function TQParam.GetType: TQParamType;
begin
  Result := FParamType;
end;

function TQParam.GetValueBytes: TBytes;
begin
  Result := FValue.AsBytes;
end;

function TQParam.GetValueString: QStringW;
begin
  Result := FValue.AsString;
end;

procedure TQParam.SetAsBoolean(const AValue: Boolean);
begin
  FValue.AsBoolean := AValue;
end;

procedure TQParam.SetAsBytes(ABuf: PByte; ABufLen: Integer);
var
  ATemp: TBytes;
begin
  SetLength(ATemp, ABufLen);
  Move(ABuf^, ATemp[0], ABufLen);
  FValue.AsBytes := ATemp;
end;

procedure TQParam.SetAsDateTime(const Value: TDateTime);
begin
  FValue.AsDateTime := Value;
end;

procedure TQParam.SetAsFloat(const AValue: Double);
begin
  FValue.AsFloat := AValue;
end;

procedure TQParam.SetAsGuid(const Value: TGuid);
begin
  FValue.AsGuid := Value;
end;

procedure TQParam.SetAsInt64(const AValue: Int64);
begin
  FValue.AsInt64 := AValue;
end;

procedure TQParam.SetAsInteger(const AValue: Integer);
begin
  FValue.AsInteger := AValue;
end;

procedure TQParam.SetAsInterface(AIntf: IInterface);
begin
  IInterface(FValue.Value.AsPointer) := AIntf;
end;

procedure TQParam.SetAsPointer(AValue: Pointer);
begin
  if FParamType = ptPointer then
    FValue.Value.AsPointer := AValue
  else
    raise Exception.Create('参数类型与请求的值不匹配');
end;

procedure TQParam.SetAsSingle(const AValue: Single);
begin
  FValue.AsSingle := AValue;
end;

procedure TQParam.SetAsString(const AValue: PWideChar);
begin
  FValue.AsString := AValue;
end;

procedure TQParam.SetAsValue(const Value: TValue);
begin

end;

procedure TQParam.SetNull;
begin
  if FDataType = ptArray then
    IInterface(FValue.Value.AsPointer) := nil
  else
    FValue.Reset;
end;

procedure TQParam.SetParamType(const Value: TQParamType);
begin
  if FParamType <> Value then
  begin
    FParamType := Value;
    case Value of
      ptUnknown:
        FValue.Reset;
      ptInt8 .. ptInt32:
        FValue.TypeNeeded(vdtInteger);
      ptUInt32, ptInt64, ptUInt64:
        FValue.TypeNeeded(vdtInt64);
      ptFloat4:
        FValue.TypeNeeded(vdtSingle);
      ptFloat8:
        FValue.TypeNeeded(vdtFloat);
      ptDateTime:
        FValue.TypeNeeded(vdtDateTime);
      ptInterval:
        FValue.TypeNeeded(vdtInterval);
      ptAnsiString .. ptUnicodeString:
        FValue.TypeNeeded(vdtString);
      ptBoolean:
        FValue.TypeNeeded(vdtBoolean);
      ptBytes:
        FValue.TypeNeeded(vdtStream);
      ptGuid:
        FValue.TypeNeeded(vdtGuid);
      ptInterface:
        FValue.Reset;
      ptArray:
        begin
          FValue.Reset;
          IInterface(FValue.Value.AsPointer) := TQParams.Create;
        end;
    end;
  end;
end;

procedure TQParam.SetSize(ASize: Integer);
begin
  raise Exception.Create(SUnsupportNow);
end;

procedure TQParam.SetValueBytes(const Value: TBytes);
begin
  FValue.AsBytes := Value;
end;

procedure TQParam.SetValueString(const Value: QStringW);
begin
  FValue.AsString := Value;
end;

{ TQParams }

function TQParams.Add(const AName: PWideChar; AParamType: TQParamType): IQParam;
begin
  Result := Add(AParamType);
  (Result as TQParam).FName := AName;
end;

function TQParams.Add(const AName: QStringW; AValue: Double): TQParam;
begin
  Result := Add(PWideChar(AName), ptFloat8) as TQParam;
  Result.AsFloat := AValue;
end;

function TQParams.Add(const AName: QStringW; AValue: Int64): TQParam;
begin
  Result := Add(PWideChar(AName), ptInt64) as TQParam;
  Result.AsInt64 := AValue;
end;

function TQParams.Add(const AName: QStringW; AValue: Boolean): TQParam;
begin
  Result := Add(PWideChar(AName), ptBoolean) as TQParam;
  Result.AsBoolean := AValue;
end;

function TQParams.Add(const AName: QStringW; const AValue: TBytes): TQParam;
begin
  Result := Add(PWideChar(AName), ptBytes) as TQParam;
  Result.AsBytes := AValue;
end;

function TQParams.Add(AType: TQParamType): TQParam;
begin
  Result := TQParam.Create;
  Result.DataType := AType;
  Result._AddRef;
  FItems.Add(Result);
end;

function TQParams.Add(const AName: QStringW; const AValue: TGuid): TQParam;
begin
  Result := Add(PWideChar(AName), ptGuid) as TQParam;
  Result.AsGuid := AValue;
end;

function TQParams.Add(const AName, AValue: QStringW): TQParam;
begin
  Result := Add(PWideChar(AName), ptUnicodeString) as TQParam;
  Result.AsString := AValue;
end;

function TQParams.AddArray(const AName: QStringW): TQParams;
var
  AParam: TQParam;
begin
  AParam := Add(PWideChar(AName), ptArray) as TQParam;
  Result := AParam.AsParams;
end;

function TQParams.AddDateTime(const AName: QStringW; AValue: TDateTime)
  : TQParam;
begin
  Result := Add(PWideChar(AName), ptArray) as TQParam;
  Result.AsDateTime := AValue;
end;

function TQParams.AsRecord(AlignSize: Byte): TBytes;
begin
  SetLength(Result, 0);
end;

procedure TQParams.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TQParam(FItems[I])._Release;
  FItems.Clear;
end;

procedure TQParams.CopyFrom(ASource: IQParams);
begin
  InternalCopy(ASource, Self);
end;

procedure TQParams.CopyTo(ATarget: IQParams);
begin
  Clear;
  InternalCopy(Self, ATarget);
end;

constructor TQParams.Create;
begin
  inherited Create;
  FItems := TQParamList.Create;
end;

procedure TQParams.Delete(AIndex: Integer);
begin
  TQParam(FItems[AIndex])._Release;
  FItems.Delete(AIndex);
end;

destructor TQParams.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
end;

function TQParams.GetAsJson: QStringW;
var
  ABuilder: TQStringCatHelperW;
begin
  ABuilder := TQStringCatHelperW.Create;
  try
    InternalEncode(ABuilder, Self);
    Result := ABuilder.Value;
  finally
    FreeAndNil(ABuilder);
  end;
end;

function TQParams.GetChildItems(AIndex: Integer): TQParam;
begin
  Result := FItems[AIndex];
end;

function TQParams.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQParams.GetItems(AIndex: Integer): IQParam;
begin
  Result := FItems[AIndex];
end;

function TQParams.GetSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    Inc(Result, Items[I].GetSize);
end;

procedure TQParams.InternalCopy(ASourceList, ADestList: IQParams);
var
  I, C: Integer;
  AParam: IQParam;
  ATarget: IQParam;
  procedure CopyString;
  var
    L: Integer;
    S: QStringW;
  begin
    L := 0;
    L := AParam.GetAsString(nil, L);
    SetLength(S, L);
    AParam.GetAsString(PWideChar(S), L);
    ATarget.SetAsString(PWideChar(S));
  end;
  procedure CopyBytes;
  var
    L: Integer;
    ATemp: TBytes;
  begin
    L := AParam.GetAsBytes(nil, 0);
    if L > 0 then
    begin
      SetLength(ATemp, L);
      AParam.GetAsBytes(@ATemp[0], L);
      ATarget.SetAsBytes(@ATemp[0], L);
    end;
  end;

begin
  Clear;
  C := ASourceList.GetCount;
  for I := 0 to C - 1 do
  begin
    AParam := ASourceList.GetItems(I);
    ATarget := Add(AParam.GetName, AParam.GetType) as TQParam;
    case AParam.GetType of
      ptInt8, ptUInt8, ptInt16, ptUInt16, ptInt32:
        ATarget.SetAsInteger(AParam.GetAsInteger);
      ptUInt32, ptInt64, ptUInt64:
        ATarget.SetAsInt64(AParam.GetAsInt64);
      ptFloat4:
        ATarget.SetAsSingle(AParam.GetAsSingle);
      ptFloat8, ptDateTime:
        ATarget.SetAsFloat(AParam.GetAsFloat); // Float types
      ptInterval:
        raise Exception.Create(SUnsupportNow);
      ptAnsiString, ptUtf8String, ptUnicodeString:
        CopyString;
      ptBoolean:
        ATarget.SetAsBoolean(AParam.GetAsBoolean);
      ptGuid:
        ATarget.SetAsGuid(AParam.GetAsGuid); // Guid
      ptBytes:
        CopyBytes;
      ptArray:
        InternalCopy(AParam.GetAsArray, ATarget.GetAsArray);
    end;
  end;
end;

procedure TQParams.InternalEncode(ABuilder: TQStringCatHelperW;
  AParams: TQParams);
var
  AIsArray: Boolean;
  I: Integer;
  function EncodeBytes(const ABytes: TBytes): QStringW;
  begin
    OnQJsonEncodeBytes(ABytes, Result);
  end;
  procedure EncodeParams(AParams: TQParams);
  var
    I: Integer;
    AItem: TQParam;
  begin
    if AParams.Count > 0 then
    begin
      for I := 0 to AParams.Count - 1 do
      begin
        AItem := AParams[I];
        if not AIsArray then
          ABuilder.Cat('"', 1).Cat(TQJson.JsonEscape(AItem.Name, False))
            .Cat('":', 2);
        if AItem.FParamType in [ptAnsiString, ptUtf8String, ptUnicodeString,
          ptGuid, ptInterval] then
          ABuilder.Cat('"', 1).Cat(TQJson.JsonEscape(AItem.AsString, False))
            .Cat('"', 1)
        else if AItem.FParamType = ptBytes then
          ABuilder.Cat('"', 1).Cat(EncodeBytes(AItem.AsBytes)).Cat('"', 1)
        else if AItem.FParamType = ptDateTime then
          ABuilder.Cat('"', 1).Cat(TQJson.EncodeDateTime(AItem.AsDateTime))
            .Cat('"', 1)
        else if AItem.FParamType = ptArray then
          InternalEncode(ABuilder, AItem.AsParams)
        else
          ABuilder.Cat(AItem.AsString);
        ABuilder.Cat(',', 1);
      end;
      ABuilder.Back(1);
    end;
  end;

begin
  AIsArray := True;
  for I := 0 to AParams.Count - 1 do
  begin
    if Length(AParams[I].Name) <> 0 then
    begin
      AIsArray := False;
      Break;
    end;
  end;
  if AIsArray then
  begin
    ABuilder.Cat('[', 1);
    EncodeParams(AParams);
    ABuilder.Cat(']', 1);
  end
  else
  begin
    ABuilder.Cat('{', 1);
    EncodeParams(AParams);
    ABuilder.Cat('}', 1);
  end;
end;

procedure TQParams.LoadFromFile(const AFileName: PWideChar);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(QStream(AStream, False));
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQParams.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(QStream(AStream, False));
end;

procedure TQParams.LoadFromStream(AStream: IQStream);
var
  ATemp: TQStream;
  AJson: TQJson;
  procedure JsonToParams(AParent: TQJson; AParams: TQParams);
  var
    I: Integer;
    JT: TQJson;

  begin
    for I := 0 to AParent.Count - 1 do
    begin
      JT := AParent[I];
      case JT.DataType of
        jdtNull:
          AParams.Add(PWideChar(JT.Name), ptUnknown).SetNull;
        jdtString:
          AParams.Add(JT.Name, JT.AsString);
        jdtInteger:
          AParams.Add(JT.Name, JT.AsInt64);
        jdtFloat:
          AParams.Add(JT.Name, JT.AsFloat);
        jdtBoolean:
          AParams.Add(JT.Name, JT.AsBoolean);
        jdtArray, jdtObject:
          JsonToParams(JT, AParams.AddArray(JT.Name));
      end;
    end;
  end;

begin
  Clear;
  AJson := TQJson.Create;
  ATemp := TQStream.Create(AStream);
  try
    AJson.LoadFromStream(ATemp);
    JsonToParams(AJson, Self);
  finally
    FreeAndNil(ATemp);
    FreeAndNil(AJson);
  end;
end;

function TQParams.ParamByName(const AName: PWideChar): IQParam;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    if StrCmpW(PWideChar(TQParam(FItems[I]).Name), AName, True) = 0 then
    begin
      Result := FItems[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TQParams.ParamByPath(APath: PWideChar): IQParam;
var
  AName: QStringW;
  AParent: IQParams;
begin
  if APath <> nil then
  begin
    AParent := Self;
    repeat
      AName := DecodeTokenW(APath, '/', #0, True, True);
      Result := AParent.ParamByName(PWideChar(AName));
      AParent := Result.GetAsArray;
    until (APath^ = #0) or (Result = nil);
    if APath^ <> #0 then
      Result := nil;
  end
  else
    Result := nil;
end;

procedure TQParams.SaveToFile(const AFileName: PWideChar);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(QStream(AStream, False));
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TQParams.SaveToStream(AStream: TStream);
begin
  SaveToStream(QStream(AStream, False));
end;

procedure TQParams.SaveToStream(AStream: IQStream);
var
  AService: IQPluginService;
  AResult: TQParams;
  procedure DefaultHandler;
  var
    S: QStringA;
    L, W: Integer;
    P: PQCharA;
  begin
    S := qstring.Utf8Encode(AsJson);
    L := S.Length;
    P := PQCharA(S);
    while L > 0 do
    begin
      W := AStream.Write(P, L);
      Dec(L, W);
      Inc(P, W);
    end;
  end;

begin
  AService := PluginsManager.RequireService('Services/Params/ToStream');
  if Assigned(AService) then
  begin
    AResult := TQParams.Create;
    try
      AResult.Add('Result', ptStream);
      if AService.Execute(Self, AResult) then // 尝试写入到流中
      begin
        AStream.CopyFrom(AResult[0].GetAsStream, 0);
        Delete(Count - 1);
      end
      else
      begin
        Delete(Count - 1);
        DefaultHandler;
      end;
    finally
      FreeAndNil(AResult);
    end;
  end
  else
    DefaultHandler;
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

function TQStreamHelper.Seek(AOffset: Int64; AFrom: Byte): Int64;
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

function TQStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(@Buffer, Count);
end;

function TQStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Byte(Origin));
end;

function TQStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(@Buffer, Count);
end;

{ TQPluginService }

constructor TQPluginService.Create(const AId: TGuid; const AName: QStringW);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FAttrs := TQParams.Create;
end;

constructor TQPluginService.Create(const AName: QStringW);
begin
  Create(NewId, AName);
end;

destructor TQPluginService.Destroy;
begin
  FAttrs := nil;
  inherited;
end;

function TQPluginService.Execute(const AParams: IQParams;
  AResult: IQParams): Boolean;
begin
  Result := True;
end;

function TQPluginService.GetAttributes: IQParams;
begin
  Result := FAttrs;
end;

function TQPluginService.GetId: TGuid;
begin
  Result := FId;
end;

function TQPluginService.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TQPluginService.GetLastErrorMsg: PWideChar;
begin
  Result := PWideChar(FLastError);
end;

function TQPluginService.GetLoader: IQPluginsLoader;
begin
  Result := FLoader;
end;

function TQPluginService.GetName: PWideChar;
begin
  Result := PWideChar(FName);
end;

function TQPluginService.GetParent: IQPluginServices;
begin
  Result := FParent;
end;

function TQPluginService.LoadStates(AParams: IQParams): Boolean;
begin
  Result := True;
end;

function TQPluginService.NewInstance: IQPluginService;
begin
  Result := Self;
end;

function TQPluginService.SaveStates(AParams: IQParams): Boolean;
begin
  Result := True;
end;

procedure TQPluginService.SetLastError(ACode: Integer; const S: QStringW);
begin
  FLastError := ACode;
  FLastErrorMsg := S;
end;

procedure TQPluginService.SetParent(AParent: IQPluginServices);
begin
  FParent := AParent;
end;

function TQPluginService.Terminate(AWait: Boolean): Boolean;
begin
  Result := True;
end;

{ TQInterfacedObject }

function TQInterfacedObject.QueryInterface(const IID: TGuid; out Obj): HRESULT;
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
    FreeObject(Self);
end;

{ TQPluginsSimpleRouter }

function TQPluginsSimpleRouter.AddId(const ASourceId, ATargetId: TGuid)
  : Boolean;
var
  AItem: PQPluginRouteRule;
begin
  NewRule(False);
  AItem.SourceId := ASourceId;
  AItem.TargetId := ATargetId;
  FLocker.Enter;
  try
    FItems.Add(AItem);
    FHashList.Add(AItem, HashOf(AItem, SizeOf(TQPluginRouteRule)));
  finally
    FLocker.Leave;
  end;
end;

function TQPluginsSimpleRouter.AddMap(const ASourcePath,
  ATargetPath: PWideChar): Boolean;
begin

end;

procedure TQPluginsSimpleRouter.AddPath(const ASourcePath,
  ATargetPath: PWideChar);
var
  AItem: PQPluginRouteRule;
  L1, L2: Integer;
  AHash: Cardinal;
  function CopyRule(S: PWideChar): PWideChar;
  var
    L: Integer;
  begin
    L := (StrLen(S) shl 1) + 2;
    GetMem(Result, L);
    Move(S^, Result^, L);
  end;

begin
  NewRule(False);
  L1 := StrLen(ASourcePath);
  L2 := StrLen(ATargetPath);
  GetMem(AItem.SourcePath, (L1 + L2 + 2) shl 1);
  AItem.TargetPath := AItem.SourcePath;
  Inc(AItem.TargetPath, L1 + 1);
  Move(ASourcePath^, AItem.SourcePath^, (L1 + 1) shl 1);
  Move(ATargetPath^, AItem.TargetPath^, (L2 + 1) shl 1);
  AHash := HashOf(AItem.SourcePath, (L1 + L2 + 2) shl 1);
  FLocker.Enter;
  try
    FItems.Add(AItem);
    FHashList.Add(AItem, AHash);
  finally
    FLocker.Leave;
  end;
end;

function TQPluginsSimpleRouter.ById(const AId: TGuid): IQPluginService;
begin

end;

function TQPluginsSimpleRouter.ByPath(APath: PWideChar): IQPluginService;
begin

end;

procedure TQPluginsSimpleRouter.Clear;
begin

end;

constructor TQPluginsSimpleRouter.Create(const AId: TGuid;
  const AName: QStringW);
begin
  inherited;
  FHashList := TQHashTable.Create;
  FItems := TQPluginRouteRules.Create;
end;

procedure TQPluginsSimpleRouter.Delete(AIndex: Integer);
begin

end;

destructor TQPluginsSimpleRouter.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FHashList);
  inherited;
end;

function TQPluginsSimpleRouter.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQPluginsSimpleRouter.GetItems(AIndex: Integer): PQPluginRouteRule;
begin
  Result := FItems[AIndex];
end;

function TQPluginsSimpleRouter.NewRule(AByPath: Boolean): PQPluginRouteRule;
begin
  New(Result);
  Result.ByPath := AByPath;
end;

initialization

finalization

_PluginsManager := nil;

end.
