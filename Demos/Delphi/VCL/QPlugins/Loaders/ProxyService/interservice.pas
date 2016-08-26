unit interservice;

interface

uses classes, sysutils, qplugins, qparams, qworker, windows, messages, syncobjs,
  Generics.collections, qsimplepool;

type
  // RPC 进程对象
  TQRPCProcess = class
  protected
    FPath: String; // 服务提供进程路径，使用URI方式来定位，本地为：file:///xxxx
    FMaxProcess: Integer; // 最大允许启动的服务端进程数量
  public
    constructor Create(APath: String; AMaxNum: Integer = 1); virtual;
    property Path: String read FPath;
    property MaxProcess: Integer read FMaxProcess;
  end;

  // 本进程的监听进程对象，负责派发接收到的结果
  TQRPCLocalProcess = class(TQRPCProcess)
  protected
    procedure Dispatch(ASource: TQRPCProcess; AParams: IQParams; ATag: Pointer);
      virtual; abstract;
    procedure HandleResult(ASource: TQRPCProcess; AResult: IQParams;
      ATag: Pointer); virtual; abstract;
    procedure Notify(ASource: TQRPCProcess; AId: Cardinal; AParams: IQParams);
      virtual; abstract;
  end;

  TQPendingCallback = procedure(ASerivce: IQService; AParams, AResult: IQParams;
    ATag: Pointer) of object;

  TQPendingRPC = class
  private
    FParams: IQParams;
    FResult: IQParams;
    FTag: Pointer;
    FService: IQService;
    FEvent: TEvent;
    FCallback: TQPendingCallback;
  public
    constructor Create(AService: IQService; AParams, AResult: IQParams;
      ATag: Pointer); overload;
    destructor Destroy; override;
    function WaitFor(ATimeout: Cardinal): TWaitResult;
    property Params: IQParams read FParams;
    property Result: IQParams read FResult;
    property Tag: Pointer read FTag;
    property Callback: TQPendingCallback read FCallback write FCallback;
  end;

  // 远程进程的信息
  TQRPCRemoteProcess = class(TQRPCProcess)
  protected
    function InternalSend(ACall: TQPendingRPC): Boolean; virtual; abstract;
  end;
{$IFDEF UNICODE}

  TQProcessList = TList<TQRPCProcess>;
{$ELSE}
  TQProcessList = TList;
{$ENDIF}
  TQProxyService = class;

  TQRPCProcesses = class(TCriticalSection)
  private
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQRPCProcess;
  protected
    FLocal: TQRPCLocalProcess;
    FItems: TQProcessList;
    FEvents: TQSimplePool;
    function InternalPost(ACall: TQPendingRPC): Boolean; virtual;
    procedure DoNewEvent(ASender: TQSimplePool; var AData: Pointer);
    procedure DoFreeEvent(ASender: TQSimplePool; AData: Pointer);
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Clear;
    // 投寄一个不关心返回结果的请求
    function Post(AService: IQService; AParams, AResult: IQParams;
      ATag: Pointer): Boolean; overload;
    function Post(AService: IQService; AParams, AResult: IQParams;
      ATag: Pointer): TQPendingRPC;
    function Post(AService: IQService; AParams, AResult: IQParams;
      ATag: Pointer; ACallback: TQPendingCallback): Boolean;
    function Send(AService: IQService; AParams, AResult: IQParams): Boolean;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TQRPCProcess read GetItems; default;
    property LocalProcess: TQRPCLocalProcess read FLocal write FLocal;
  end;

  TQProxyService = class(TQService)
  protected
    FProcess: TQRPCRemoteProcess;
  public
    function Execute(AParams: IQParams; AResult: IQParams): Boolean;
      override; stdcall;
    property Process: TQRPCRemoteProcess read FProcess; // 服务所隶属的进程
  end;

implementation

var
  RemoteProcesses: TQRPCProcesses;
  { TQRPCProcesses }

procedure TQRPCProcesses.Clear;
var
  I: Integer;
begin
  Enter;
  try
    for I := 0 to FItems.Count - 1 do
      FreeAndNil(FItems[I]);
    FItems.Clear;
  finally
    Leave;
  end;
end;

constructor TQRPCProcesses.Create;
begin
  inherited Create;
  FItems := TQProcessList.Create;
  FEvents := TQSimplePool.Create(100, SizeOf(Pointer));
  FEvents.OnNewItem := DoNewEvent;
  FEvents.OnFree := DoFreeEvent;
end;

destructor TQRPCProcesses.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FEvents);
  inherited;
end;

procedure TQRPCProcesses.DoFreeEvent(ASender: TQSimplePool; AData: Pointer);
begin
  FreeAndNil(AData);
end;

procedure TQRPCProcesses.DoNewEvent(ASender: TQSimplePool; var AData: Pointer);
begin
  AData := TEvent.Create(nil, false, false, '');
end;

function TQRPCProcesses.GetCount: Integer;
begin
  Result := FItems.Create;
end;

function TQRPCProcesses.GetItems(AIndex: Integer): TQRPCProcess;
begin
  Result := FItems[AIndex];
end;

function TQRPCProcesses.InternalPost(ACall: TQPendingRPC): Boolean;
begin
  Result := (ACall.FService as TQProxyService).FProcess.InternalSend(ACall);
end;

function TQRPCProcesses.Post(AService: IQService; AParams, AResult: IQParams;
  ATag: Pointer): Boolean;
var
  ACall: TQPendingRPC;
begin
  ACall := TQPendingRPC.Create(AService, AParams, AResult, ATag);
  try
    Result := InternalPost(ACall);
    if not Result then
      FreeAndNil(ACall);
  except
    FreeAndNil(ACall);
  end;
end;

function TQRPCProcesses.Post(AService: IQService; AParams, AResult: IQParams;
  ATag: Pointer): TQPendingRPC;
begin
  Result := TQPendingRPC.Create(AService, AParams, AResult, ATag);
  try
    if not InternalPost(Result) then
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
end;

function TQRPCProcesses.Post(AService: IQService; AParams, AResult: IQParams;
  ATag: Pointer; ACallback: TQPendingCallback): Boolean;
var
  ACall: TQPendingRPC;
begin
  ACall := TQPendingRPC.Create(AService, AParams, AResult, ATag);
  try
    ACall.Callback := ACallback;
    Result := InternalPost(ACall);
    if not Result then
      FreeAndNil(ACall);
  except
    FreeAndNil(ACall);
  end;
end;

function TQRPCProcesses.Send(AService: IQService;
  AParams, AResult: IQParams): Boolean;
var
  ACall: TQPendingRPC;
begin
  ACall := TQPendingRPC.Create(AService, AParams, AResult, nil);
  try
    Result := InternalPost(ACall);
    if Result then
    begin
      if ACall.FEvent.WaitFor = wrSignaled then
        Result := True;
    end;
  finally
    FreeAndNil(ACall);
  end;
end;

{ TQProxyService }

function TQProxyService.Execute(AParams, AResult: IQParams): Boolean;
begin
  Result := RemoteProcesses.Send(Self, AParams, AResult);
end;

{ TQPendingRPC }

constructor TQPendingRPC.Create(AService: IQService; AParams, AResult: IQParams;
  ATag: Pointer);
begin
  inherited Create;
  FService := AService;
  FParams := AParams;
  FResult := AResult;
  FTag := ATag;
  FEvent := RemoteProcesses.FEvents.Pop;
end;

destructor TQPendingRPC.Destroy;
begin
  RemoteProcesses.FEvents.Push(FEvent);
  inherited;
end;

function TQPendingRPC.WaitFor(ATimeout: Cardinal): TWaitResult;
begin
  Result := FEvent.WaitFor(ATimeout);
end;

initialization

RemoteProcesses := TQRPCProcesses.Create();

end.
