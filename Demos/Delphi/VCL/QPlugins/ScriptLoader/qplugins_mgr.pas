unit qplugins_mgr;

interface

uses classes, types, sysutils, syncobjs, qstring, qtimetypes, qvalue, qjson,
  qpluginsintf, qplugins,qplugins_base{$IFDEF UNICODE},
  system.Generics.collections{$ENDIF};

type
  TQPluginsManager = class;
  TQPluginServcieNotifyEvent = procedure(ASender: TQPluginsManager;
    AErrorObject: IQPluginService);

  TQPluginsManager = class(TQPluginServices, IQPluginsManager, IQNotify)
  protected
    FLoaders, FRouters, FServices: TQPluginServices;
    FOnError: TQPluginServcieNotifyEvent;
    FStarted: Boolean;
    FRefCount: Integer;
    FStartedId, FStoppingId, FRouteChangedId: Cardinal;
    FNotifyManager: IQNotifyManager;
    function GetServices: IQPluginServices; stdcall;
    function GetLoaders: IQPluginServices; stdcall;
    function GetRouters: IQPluginServices; stdcall;
    function InternalFindService(APath: PWideChar): IQPluginService;
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean);stdcall;
    procedure RegisterNotifies(AFireStarted: Boolean);
    procedure UnregisterNotifies;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Start; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
      override; stdcall;
    function Stop: Boolean; stdcall;
    function ByPath(APath: PWideChar): IQPluginService; override; stdcall;
    function ById(const AId: TGUID): IQPluginService; override; stdcall;
    function RequireService(APath: PWideChar): IQPluginService; stdcall;
    function Replace(ANewManager: IQPluginsManager): Boolean; stdcall;
    property Loaders: IQPluginServices read GetLoaders;
    property Routers: IQPluginServices read GetRouters;
    property Services: IQPluginServices read GetServices;
    property OnError: TQPluginServcieNotifyEvent read FOnError write FOnError;
    property Started: Boolean read FStarted;
  end;

implementation

{$IFDEF MSWINDOWS}

uses windows;

var
  MapHandle: THandle;
{$ENDIF}

resourcestring
  SUnsupportNow = '当前不支持此功能。';

procedure RegisterManager;
var
  AName: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF};
  AMgr: IQPluginsManager;
{$IFDEF MSWINDOWS}
  P: Pointer;
{$ENDIF}
begin
  if PluginsManager = nil then
  begin
    AName := 'Local\QPlugins_Manager_PID_' + IntToStr(GetCurrentProcessId);
    AMgr := TQPluginsManager.Create as IQPluginsManager;
{$IFDEF MSWINDOWS}
    MapHandle := CreateFileMappingW(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      0, SizeOf(Pointer), PWideChar(AName));
    if MapHandle = 0 then
      RaiseLastOSError(GetLastError);
    P := MapViewOfFile(MapHandle, FILE_MAP_WRITE, 0, 0, 0);
    IQPluginServices(PPointer(P)^) := AMgr;
    UnmapViewOfFile(P);
{$ENDIF}
  end;
end;

procedure UnregisterManager;
begin
  if MapHandle <> 0 then
  begin
    PluginsManager._Release; // 减少引用计数
    CloseHandle(MapHandle);
    MapHandle := 0;
  end;
end;

{ TQPluginsManager }

function TQPluginsManager.ById(const AId: TGUID): IQPluginService;
var
  I: Integer;
begin
  Lock;
  try
    Result := nil;
    if FRouters.Count > 0 then
    begin
      for I := 0 to FRouters.Count - 1 do
      begin
        Result := (FRouters[I] as IQPluginsRouter).ById(AId);
        if Result <> nil then
          Break;
      end;
    end;
    if not Assigned(Result) then
      Result := inherited ById(AId);
  finally
    Unlock;
  end;
end;

function TQPluginsManager.ByPath(APath: PWideChar): IQPluginService;
begin
  Lock;
  try
    Result := InternalFindService(APath);
  finally
    Unlock;
  end;
end;

constructor TQPluginsManager.Create;
begin
  inherited Create(GUID_NULL, 'Manager');
  FLoaders := TQPluginServices.Create(QPID_LOADERS, 'Loaders');
  Add(FLoaders);
  FRouters := TQPluginServices.Create(QPID_ROUTERS, 'Routers');
  Add(FRouters);
  FServices := TQPluginServices.Create(QPID_SERVICES, 'Services');
  Add(FServices);
end;

destructor TQPluginsManager.Destroy;
begin
  Clear;
  inherited;
end;

function TQPluginsManager.QueryInterface(const IID: TGUID; out Obj): HRESULT;
var
  AService: IQPluginService;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> 0 then // 如果默认实现没有，那按ID查
  begin
    AService := ById(IID);
    if AService = nil then
      Result := E_NOINTERFACE
    else
    begin
      AService.NewInstance.QueryInterface(IID, Obj);
      Result := S_OK;
    end;
  end;
end;

function TQPluginsManager.GetLoaders: IQPluginServices;
begin
  Result := FLoaders;
end;

function TQPluginsManager.GetRouters: IQPluginServices;
begin
  Result := FRouters;
end;

function TQPluginsManager.GetServices: IQPluginServices;
begin
  Result := FServices;
end;

function TQPluginsManager.InternalFindService(APath: PWideChar)
  : IQPluginService;
var
  I: Integer;
begin
  Result := nil;
  if FRouters.Count > 0 then
  begin
    for I := 0 to FRouters.Count - 1 do
    begin
      Result := (FRouters[I] as IQPluginsRouter).ByPath(APath);
      if Result <> nil then
        Break;
    end;
  end;
  if not Assigned(Result) then
    Result := inherited ByPath(APath);
end;

procedure TQPluginsManager.Notify(const AId: Cardinal; AParams: IQParams;
  var AFireNext: Boolean);
var
  ASender: IQParam;
  ANotifyMgr: IQNotifyManager;
begin
  if AId = FRouteChangedId then
  begin
    if AParams.Count > 0 then
    begin
      ASender := AParams[0];
      if ASender.GetType = ptInterface then
      begin
        if Supports(ASender.AsInterface, IQNotifyManager, ANotifyMgr) then
        begin
          UnregisterNotifies;
          RegisterNotifies(False);
        end;
      end;
    end;
  end;
end;

procedure TQPluginsManager.RegisterNotifies(AFireStarted: Boolean);
begin
  if Supports(Self, IQNotifyManager, FNotifyManager) then
  begin
    FStartedId := FNotifyManager.IdByName('QPlugins.Started');
    FStoppingId := FNotifyManager.IdByName('QPlugins.Stopping');
    FRouteChangedId := FNotifyManager.IdByName('QPlugins.Router.Changed');
    FNotifyManager.Subscribe(FRouteChangedId, Self);
    if AFireStarted then
      FNotifyManager.Post(FStartedId, TQParams.Create);
  end;
end;

function TQPluginsManager.Replace(ANewManager: IQPluginsManager): Boolean;
begin
  Result := False;
end;

function TQPluginsManager.RequireService(APath: PWideChar): IQPluginService;
begin
  Lock;
  try
    Result := InternalFindService(APath);
  finally
    Unlock;
  end;
end;

procedure TQPluginsManager.Start;
var
  I: Integer;
  AParams,AResult: IQParams;
  ALoader: IQPluginService;
begin
  AParams := TQParams.Create;
  AResult:=TQParams.Create;
  AParams.Add('State', ptInt32).SetAsInteger(LA_STARTING);
  AParams.Add('Result', ptInt32).SetAsInteger(0);
  for I := 0 to FLoaders.Count - 1 do
  begin
    ALoader := FLoaders[I];
    if not ALoader.Execute(AParams,AResult) then
    begin
      if Assigned(FOnError) then
        FOnError(Self, ALoader);
    end;
  end;
  FStarted := True;
  RegisterNotifies(True);
end;

function TQPluginsManager.Stop: Boolean;
var
  I: Integer;
  AParams,AResult: IQParams;
  ALoader: IQPluginService;
begin
  AParams := TQParams.Create;
  AParams.Add('State', ptInt32).SetAsInteger(LA_CANSTOP);
  Result := True;
  for I := 0 to FLoaders.Count - 1 do
  begin
    ALoader := FLoaders[I];
    if not ALoader.Execute(AParams,AResult) then
    begin
      Result := False;
      Exit;
    end;
  end;
  if Result then
  begin
    AParams.GetItems(0).SetAsInteger(LA_STOPING);
    if Assigned(FNotifyManager) then
      FNotifyManager.Send(FStoppingId, AParams);
    for I := 0 to FLoaders.Count - 1 do
    begin
      ALoader := FLoaders[I];
      if not ALoader.Execute(AParams,AResult) then
      begin
        Result := False;
        Exit;
      end;
    end;
    FStarted := False;
    UnregisterNotifies;
  end;
end;

procedure TQPluginsManager.UnregisterNotifies;
begin
  FNotifyManager.Unsubscribe(FStartedId, Self);
  FNotifyManager.Unsubscribe(FStoppingId, Self);
  FNotifyManager.Unsubscribe(FRouteChangedId, Self);
  FNotifyManager := nil;
end;

initialization

RegisterManager;

finalization

UnregisterManager;

end.
