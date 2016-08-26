unit serviceproxy;

interface

uses classes, sysutils, qplugins, qparams, qworker, windows, messages, syncobjs,
  Generics.collections;

type
  TQServiceCallBack = procedure(AParams, AResult: IQParams) of object;
  TQServiceCallBackA = reference to procedure(AParams, AResult: IQParams);
  TQServiceCallBackG = procedure(AParams, AResult: IQParams);
  PQServicePendingCall = ^TQServicePendingCall;

  TQServicePendingCall = record
    Id: Integer;
    Params, Result: IQParams;
    Callback: TMethod;
    Prior: PQServicePendingCall;
  end;

  TQServiceProcess = class(TCriticalSection)
  private
    FHost: String; // 服务进程所在的主机名
    FPath: String; // 服务提供程序路径
    FPId: Cardinal; // 进程ID
    FHandle: THandle;
    FLastCallId: Integer;
    FKnownServices: IQServices; // 已知注册的服务列表
    FLastPending: PQServicePendingCall;
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Handle: THandle read FHandle;
  end;

  TQProxyService = class(TQService)
  protected
    FProcess: TQServiceProcess;
  public
    function Execute(AParams: IQParams; AResult: IQParams): Boolean;
      override; stdcall;
  end;

implementation

uses tlhelp32, qstring, qsimplepool;

const
  SInterProcessNotify = 'QNotify.InterProcess';
  SRPCAtom = 'QPlugins.Rpc.Atom';
  SServiceMissed = 'Unknown service with id %s';
  SServiceFailure = 'Execute service %s failure.';
  RPC_CALL = 0;
  RPC_RESULT = 1;
  RPC_NOTIFY = 2;
  RPC_SUBSCRIBE = 3;

  QERROR_SUCCESS = 0; // 执行成功
  QERROR_SERVICE_MISSED = 1; // 服务不存在
  QERROR_SERVICE_FAIL = 2; // 执行服务失败

type
  TQRPCCallHeader = packed record
    Action: Byte; // 类型:RPC_CALL
    Service: TGuid; // 请求执行的服务ID
    Data: array [0 .. 0] of Byte; // 额外的数据内容
  end;

  PQRPCCallHeader = ^TQRPCCallHeader;

  TQRPCResultHeader = packed record
    Action: Byte; // 类型：RPC_RESULT
    ErrorCode: Cardinal; // 错误代码
    Offset: Cardinal; // 参数偏移
    ErrorMsg: array [0 .. 0] of WideChar;
  end;

  PQRPCResultHeader = ^TQRPCResultHeader;

  TQRPCNotifyHeader = packed record
    Action: Byte; // 类型：RPC_NOTIFY
    NameLen: WORD; // 名称长度
    Name: array [0 .. 0] of WideChar; // 名称
  end;

  PQRPCNotifyHeader = ^TQRPCNotifyHeader;

  TQRPCSubscribeHeader = packed record
    Action: Byte; // 类型：RPC_SUBSCRIBE
    NameLen: WORD; // 要订阅的通知名称长度
    Name: array [0 .. 0] of WideChar; // 通知名称
  end;

  TRPCCall = class
  protected
    FFrom: HWND;
    FTag: Pointer;
    FServiceId: TGuid;
    FParams: IQParams;
  public
    constructor Create(AFrom: HWND; const AHeader: PQRPCCallHeader;
      ATotalLen: Integer);
  end;

  PRPCCall = ^TRPCCall;
  TQRPCExchange = class;
  TQRPCDispatchEvent = procedure(ASender: TQRPCExchange; ASource: THandle;
    AData: PByte; ASize: Cardinal; ATag: Pointer) of object;

  TQRPCExchange = class
  protected
    FLocalWnd: HWND;
    FHostWnd: HWND;
    FOnDispatch: TQRPCDispatchEvent;
    procedure WndProc(var AMsg: TMessage);
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Send(ATarget: HWND; AData: Pointer; ASize: Cardinal;
      ATag: Pointer);
    property OnDispatch: TQRPCDispatchEvent read FOnDispatch write FOnDispatch;
  end;
{$IFDEF UNICODE}

  TQProcessList = TList<TQServiceProcess>;
{$ELSE}
  TQProcessList = TList;
{$ENDIF}

  TQRPCPending = record
    Params: IQParams;
    Result: IQParams;
    Source: IQService;
    Event: TEvent;
  end;

  PQRPCPending = ^TQRPCPending;

  // TQProxySource 用于标明服务的调用来源
  TQServiceProcesses = class(TCriticalSection)
  protected
    // 服务进程列表
    FItems: TList;
    FEventPool: TQSimplePool;
    procedure Dispatch(ASender: TQRPCExchange; ASource: HWND; AData: PByte;
      ASize: Cardinal; ATag: Pointer);
    procedure DoRPCCall(AJob: PQJob);
    procedure SendRPCResult(ATarget: HWND; const ATag: Pointer;
      AErrorCode: WORD; AErrorMsg: QStringW; AParams: IQParams);
    procedure DoNewEvent(ASender: TQSimplePool; var AData: Pointer);
    procedure DoFreeEvent(ASender: TQSimplePool; AData: Pointer);
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Clear;
    function PostCall(ASource: IQService; AParams, AResult: IQParams): THandle;
  end;

var
  RpcExchange: TQRPCExchange;
  { TQProxyService }

function TQProxyService.Execute(AParams, AResult: IQParams): Boolean;
begin
  AParams.Add('_ServiceId', ptGuid).AsGuid := Id;
  Result := FProcess.SendCall(AParams, AResult);
end;

{ TQServiceProcess }

constructor TQServiceProcess.Create;
begin
  FLocker := TCriticalSection.Create;
end;

destructor TQServiceProcess.Destroy;
begin
  FreeAndNil(FLocker);
  inherited;
end;

procedure TQServiceProcess.InternalPostCall(AParams, AResult: IQParams;
  ACallback: TMethod);
var
  AData: PQServicePendingCall;
begin
  New(AData);
  AData.Id := AtomicIncrement(FLastCallId);
  AData.Params := AParams;
  AData.Result := AResult;
  AData.Callback := ACallback;
  InternalSend(AData);
end;

procedure TQServiceProcess.InternalSend(ACallback: PQServicePendingCall);
var
  AStream: IQStream;
begin
  AStream := QStream(TMemoryStream.Create, True);
  ACallback.Params.SaveToStream(AStream);
  FLocker.Enter;
  try
    ACallback.Prior := FLastPending;
    FLastPending := ACallback;
  finally
    FLocker.Leave;
  end;
  // 发送请求
end;

procedure TQServiceProcess.PostCall(AParams, AResult: IQParams;
  ACallback: TQServiceCallBack);
var
  ACallData: PQServicePendingCall;
begin
  New(ACallData);
  ACallData.Id := AtomicIncrement(FLastCallId);
  ACallData.Params := AParams;
  ACallData.Result := AResult;
  ACallData.Callback := ACallback;
  InternalSend(ACallData);
end;

procedure TQServiceProcess.PostCall(AParams, AResult: IQParams;
  ACallback: TQServiceCallBackA);
begin

end;

procedure TQServiceProcess.PostCall(AParams, AResult: IQParams;
  ACallback: TQServiceCallBackG);
begin

end;

function TQServiceProcess.SendCall(AParams, AResult: IQParams): Boolean;
begin

end;

{ TRPCCall }

constructor TRPCCall.Create(AFrom: HWND; const AHeader: PQRPCHeader;
  ATotalLen: Integer);
var
  AParams: TQParams;
  AStream: TMemoryStream;
begin
  FFrom := AFrom;
  FTagId := AHeader.TagId;
  FServiceId := AHeader.Service;
  if ATotalLen > SizeOf(TQRPCHeader) then
  begin
    AParams := TQParams.Create;
    AStream := TMemoryStream.Create;
    try
      AStream.Size := ATotalLen - SizeOf(TQRPCHeader);
      Move(AHeader.Data[0], AStream.Memory^, AStream.Size);
      AParams.LoadFromStream(AStream);
      FParams := AParams;
      AParams := nil;
    finally
      FreeAndNil(AStream);
      if Assigned(AParams) then
        FreeAndNil(AParams);
    end;
  end
  else
    FParams := nil;
end;

function DoEnumExchangeWnd(AWnd: HWND; AParam: LPARAM): Boolean; stdcall;
var
  AId: Cardinal;
  pParam: PCardinal;
begin
  Result := True;
  pParam := PCardinal(AParam);
  GetWindowThreadProcessId(AWnd, AId);
  if AId = pParam^ then
  begin
    Inc(pParam);
    if GetProp(AWnd, SRPCAtom) = THandle(-1) then
    begin
      pParam^ := AWnd;
      Result := False;
    end;
  end;
end;

function GetProcessExchangeWnd(AProcessId: Cardinal): HWND;
var
  AParam: array [0 .. 1] of Cardinal;
begin
  AParam[0] := AProcessId;
  AParam[1] := 0;
  EnumWindows(@DoEnumExchangeWnd, LPARAM(@AParam[0]));
  Result := AParam[1];
end;

// 查找宿主程序的数据交换窗口句柄
function FindHostRpcWnd: HWND;
var
  APID: Cardinal;
  function GetParentProcessId: Cardinal;
  var
    hSnapshot: THandle;
    AEntry: TProcessEntry32;
  begin
    Result := 0;
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnapshot <> INVALID_HANDLE_VALUE then
    begin
      try
        AEntry.dwSize := SizeOf(TProcessEntry32);
        if Process32First(hSnapshot, AEntry) then
        begin
          repeat
            if AEntry.th32ProcessID = GetCurrentProcessId then
            begin
              Result := AEntry.th32ParentProcessID;
              break;
            end;
          until not Process32Next(hSnapshot, AEntry);
        end;
      finally
        CloseHandle(hSnapshot);
      end;
    end;
  end;

begin
  APID := GetParentProcessId;
  if APID <> 0 then
  begin
    Result := GetProcessExchangeWnd(APID);
    // PostLog(llDebug, '父进程ID:%d,数据交换窗口句柄:%d', [APID, Result]);
  end
  else
  begin
    Result := 0;
    // PostLog(llError, '未找到父进程，数据交换窗口未知');
  end;
end;

{ TQRPCExchange }

constructor TQRPCExchange.Create;
begin
  inherited;
  FLocalWnd := AllocateHWND(WndProc);
  FHostWnd := FindHostRpcWnd;
end;

destructor TQRPCExchange.Destroy;
begin
  DeallocateHwnd(FLocalWnd);
  inherited;
end;

procedure TQRPCExchange.Send(ATarget: HWND; AData: Pointer; ASize: Cardinal;
  ATag: Pointer);
var
  ACopyData: TCopyDataStruct;
  ABytes: TBytes;
begin
  ACopyData.dwData := ULONG_PTR(ATag);
  ACopyData.cbData := ASize;
  if ASize > 0 then
  begin
    ACopyData.lpData := AData;
    SendMessage(ATarget, WM_COPYDATA, FLocalWnd, LPARAM(@ACopyData));
  end;
end;

procedure TQRPCExchange.WndProc(var AMsg: TMessage);
  procedure DispatchData;
  var
    ACopyMsg: TWMCopyData absolute AMsg;
  begin
    if Assigned(OnDispatch) then
      OnDispatch(Self, ACopyMsg.From, ACopyMsg.CopyDataStruct.lpData,
        ACopyMsg.CopyDataStruct.cbData, ACopyMsg.CopyDataStruct.dwData);
  end;

begin
  if AMsg.Msg = WM_COPYDATA then
  begin
  end
  else
    AMsg.Result := DefWindowProc(FLocalWnd, AMsg.Msg, AMsg.WParam, AMsg.LPARAM);
end;

{ TQServiceProcesses }

procedure TQServiceProcesses.Clear;
var
  I: Integer;
begin
  Enter;
  try
    for I := 0 to FItems.Count - 1 do
      FreeObject(FItems[I]);
    FItems.Clear;
  finally
    Leave;
  end;
end;

constructor TQServiceProcesses.Create;
begin
  inherited Create;
  FItems := TQProcessList.Create;
  FEventPool := TQSimplePool.Create(100, SizeOf(TEvent));
  FEventPool.OnNewItem := DoNewEvent;
  FEventPool.OnFree := DoFreeEvent;
end;

destructor TQServiceProcesses.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TQServiceProcesses.Dispatch(ASender: TQRPCExchange; ASource: HWND;
  AData: PByte; ASize: Cardinal; ATag: Pointer);
  procedure HandleResult;
  begin
    // 处理调用的返回结果
  end;

  procedure HandleNotify;
  begin
    // 处理通知
  end;

  procedure HandleSubscribe;
  begin
    // 处理订阅
  end;

begin
  case AData^ of
    RPC_CALL:
      begin
        if Workers.Post(DoRPCCall, TRPCCall.Create(ASource, AData, ASize), True,
          jdfFreeAsObject) = 0 then
          SendRPCResult(ASource, ATag, QERROR_SERVICE_FAIL, nil);
      end;
    RPC_RESULT:
      HandleResult;
    RPC_NOTIFY:
      HandleNotify;
    RPC_SUBSCRIBE:
      HandleSubscribe;
  end;
end;

procedure TQServiceProcesses.DoFreeEvent(ASender: TQSimplePool; AData: Pointer);
begin
  FreeObject(AData);
end;

procedure TQServiceProcesses.DoNewEvent(ASender: TQSimplePool;
  var AData: Pointer);
begin
  AData := TEvent.Create(nil, False, False, '');
end;

procedure TQServiceProcesses.DoRPCCall(AJob: PQJob);
var
  ACall: TRPCCall;
  AService: IQService;

  procedure ExecuteService;
  var
    AHandled: Boolean;
    AResult: IQParams;
  begin
    AResult := TQParams.Create;
    try
      AHandled := AService.Execute(ACall.FParams, AResult);
    except
      on E: Exception do
      begin
        AHandled := False;
        SendRPCResult(ACall.FFrom, ACall.FTag, QERROR_SERVICE_FAIL,
          E.Message, nil);
        Exit;
      end;
    end;
    if AHandled then
      SendRPCResult(ACall.FFrom, ACall.FTag, '', QERROR_SUCCESS, AResult)
    else
      SendRPCResult(ACall.FFrom, ACall.FTag, QERROR_SERVICE_FAIL,
        Format(SServiceFailure, [AService.Name]), AResult);
  end;

begin
  ACall := AJob.Data;
  AService := PluginsManager.ById(ACall.FServiceId);
  if Assigned(AService) then
    ExecuteService
  else
    SendRPCResult(ACall.FFrom, ACall.FTag, QERROR_SERVICE_MISSED,
      Format(SServiceMissed, [GuidToString(ACall.FServiceId)]), nil);
end;

function TQServiceProcesses.PostCall(ASource: TQProcessProxy;
  AParams, AResult: IQParams): THandle;
var
  APending: PQRPCPending;
  ACall: TQRPCCallHeader;
  AStream: TMemoryStream;
begin
  New(APending);
  APending.Params := AParams;
  APending.Result := AResult;
  APending.Source := ASource;
  APending.Event := FEventPool.Pop;
  ACall.Action := RPC_CALL;
  ACall.Service := ASource.GetId;
  AStream := TMemoryStream.Create;
  try
    AStream.WriteBuffer(ACall, SizeOf(ACall));
    if AParams is TQParams then
      (AParams as TQParams).SaveToStream(AStream)
    else
      AParams.SaveToStream(QStream(AStream, False));
    RpcExchange.Send(ASource.Handle, AStream.Memory, AStream.Size, APending);
  finally
    FreeAndNil(AStream);
  end;
  Result := APending;
end;

procedure TQServiceProcesses.SendRPCResult(ATarget: HWND; const ATag: Pointer;
  AErrorCode: WORD; AErrorMsg: QStringW; AParams: IQParams);
var
  AStream: TMemoryStream;
  AHeader: TQRPCResultHeader;
  L: Integer;
begin
  AStream := TMemoryStream.Create;
  try
    AHeader.Action := RPC_RESULT;
    AHeader.ErrorCode := AErrorCode;
    L := Length(AErrorMsg) shl 1;
    if L > 0 then
      AHeader.Offset := SizeOf(AHeader) + L + 2
    else
      AHeader.Offset := SizeOf(AHeader);
    AStream.WriteBuffer(AHeader, SizeOf(AHeader));
    if L > 0 then
      AStream.WriteBuffer(PWideChar(AErrorMsg)^, L + 2);
    if Assigned(AParams) then
    begin
      if AParams is TQParams then
        (AParams as TQParams).SaveToStream(AStream)
      else
        AParams.SaveToStream(QStream(AStream, False));
    end;
    RpcExchange.Send(ATarget, AStream.Memory, AStream.Size, ATag);
  finally
    FreeAndNil(AStream);
  end;
end;

initialization

RpcExchange := TQRPCExchange.Create;

finalization

FreeAndNil(RpcExchange);

end.
