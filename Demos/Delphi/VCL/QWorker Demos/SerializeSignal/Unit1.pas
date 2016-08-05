unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.syncobjs,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, qstring, QWorker;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FIds: array [0 .. 2] of Integer;
    procedure DoSignal1(AJob: PQJob);
    procedure DoSignal2(AJob: PQJob);
    procedure DoSignal3(AJob: PQJob);
  public
    { Public declarations }
  end;

  PQSignalQueueItem = ^TQSignalQueueItem;

  TQSignalQueueItem = record
    Id: Integer;
    Data: Pointer;
    FreeType: TQJobDataFreeType;
    Timeout: Cardinal;
    WaitEvent: Pointer;
    Next: PQSignalQueueItem;
  end;

  TQSignalQueue = class
  private
  protected
    FFirst, FLast: PQSignalQueueItem;
    FCount: Integer;
    FMaxItems: Cardinal;
    FLocker: TCriticalSection;
    FNotify: TEvent;
    FThread: TThread;
    procedure Clear;
    function Pop: PQSignalQueueItem;
    function Wait: TWaitResult;
    function InternalPush(AId: Integer; AData: Pointer;
      AFreeType: TQJobDataFreeType; ATimeout: Cardinal;
      AWaiter: TEvent): Boolean;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Post(AId: Integer; AData: Pointer; AFreeType: TQJobDataFreeType)
      : Boolean; overload;
    function Post(AName: QStringW; AData: Pointer; AFreeType: TQJobDataFreeType)
      : Boolean; overload;
    function Send(AId: Integer; AData: Pointer; AFreeType: TQJobDataFreeType;
      ATimeout: Cardinal = INFINITE): TWaitResult; overload;
    function Send(AName: QStringW; AData: Pointer; AFreeType: TQJobDataFreeType;
      ATimeout: Cardinal = INFINITE): TWaitResult; overload;
    property MaxItems: Cardinal read FMaxItems write FMaxItems;
    property Count: Integer read FCount;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TQSignalQueueExecutor = class(TThread)
  protected
    FQueue: TQSignalQueue;
    procedure Execute; override;
  end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Workers.SignalQueue.Post('Signal1', nil);
    Workers.SignalQueue.Post('Signal2', nil);
    Workers.SignalQueue.Post('Signal3', nil);
  end
  else
  begin
    Workers.Signal('Signal1', nil);
    Workers.Signal('Signal2', nil);
    Workers.Signal('Signal3', nil);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Workers.SignalQueue.Post(FIds[0], nil);
    Workers.SignalQueue.Post(FIds[1], nil);
    Workers.SignalQueue.Post(FIds[2], nil);
  end
  else
  begin
    Workers.Signal(FIds[0], nil);
    Workers.Signal(FIds[1], nil);
    Workers.Signal(FIds[2], nil);
  end;
end;

procedure TForm1.DoSignal1(AJob: PQJob);
begin
  Memo1.Lines.Add('Signal1 fired');
end;

procedure TForm1.DoSignal2(AJob: PQJob);
begin
  Memo1.Lines.Add('Signal2 fired');
end;

procedure TForm1.DoSignal3(AJob: PQJob);
begin
  Memo1.Lines.Add('Signal3 fired');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIds[0] := Workers.RegisterSignal('Signal1');
  FIds[1] := Workers.RegisterSignal('Signal2');
  FIds[2] := Workers.RegisterSignal('Signal3');
  Workers.Wait(DoSignal1, FIds[0], true);
  Workers.Wait(DoSignal2, FIds[1], true);
  Workers.Wait(DoSignal3, FIds[2], true);
  ReportMemoryLeaksOnShutdown := true;
end;

{ TQSignalQueue }

procedure TQSignalQueue.Clear;
var
  ANext: PQSignalQueueItem;
begin
  FLocker.Enter;
  try
    while Assigned(FFirst) do
    begin
      if Assigned(FFirst.WaitEvent) then
        TEvent(FFirst.WaitEvent).SetEvent;
      ANext := FFirst.Next;
      FreeAndNil(FFirst);
      FFirst := ANext;
    end;
    FLast := nil;
  finally
    FLocker.Leave;
  end;
end;

constructor TQSignalQueue.Create;
begin
  inherited Create;
  FMaxItems := 4096;
  FLocker := TCriticalSection.Create;
  FThread := TQSignalQueueExecutor.Create(true);
  TQSignalQueueExecutor(FThread).FQueue := Self;
  FThread.Suspended := false;
  FThread.FreeOnTerminate := true;
end;

destructor TQSignalQueue.Destroy;
begin
  Clear;
  FThread.Terminate;
  FNotify.SetEvent;
  FreeAndNil(FLocker);
  inherited;
end;

function TQSignalQueue.InternalPush(AId: Integer; AData: Pointer;
  AFreeType: TQJobDataFreeType; ATimeout: Cardinal; AWaiter: TEvent): Boolean;
var
  AItem: PQSignalQueueItem;
begin
  Result := Count < MaxItems;
  if Result then
  begin
    New(AItem);
    AItem.Id := AId;
    AItem.FreeType := AFreeType;
    case AFreeType of
      jdfFreeAsObject:
        TObject(AItem.Data) := AData;
      jdfFreeAsInterface:
        IInterface(AItem.Data) := IInterface(AData);
    end;
    AItem.Timeout := ATimeout;
    AItem.Next := nil;
    TEvent(AItem.WaitEvent) := AWaiter;
    FLocker.Enter;
    if Assigned(FLast) then
      FLast.Next := AItem
    else
      FFirst := AItem;
    FLast := AItem;
    FLocker.Leave;
    FNotify.SetEvent;
  end;
end;

function TQSignalQueue.Pop: PQSignalQueueItem;
begin
  FLocker.Enter;
  Result := FFirst;
  if Assigned(FFirst) then
    FFirst := FFirst.Next;
  if not Assigned(FFirst) then
    FLast := nil;
  FLocker.Leave;
end;

function TQSignalQueue.Post(AId: Integer; AData: Pointer;
  AFreeType: TQJobDataFreeType): Boolean;
begin
  Result := InternalPush(AId, AData, AFreeType, 0, nil);
end;

function TQSignalQueue.Post(AName: QStringW; AData: Pointer;
  AFreeType: TQJobDataFreeType): Boolean;
begin
  Result := InternalPush(Workers.RegisterSignal(AName), AData,
    AFreeType, 0, nil);
end;

function TQSignalQueue.Send(AId: Integer; AData: Pointer;
  AFreeType: TQJobDataFreeType; ATimeout: Cardinal): TWaitResult;
var
  AEvent: TEvent;
begin
  AEvent := TEvent.Create(nil, false, false, '');
  if InternalPush(AId, AData, AFreeType, ATimeout, AEvent) then
    Result := AEvent.WaitFor(ATimeout)
  else
    Result := wrError;
  FreeAndNil(AEvent);
end;

function TQSignalQueue.Send(AName: QStringW; AData: Pointer;
  AFreeType: TQJobDataFreeType; ATimeout: Cardinal): TWaitResult;
begin
  Result := Send(Workers.RegisterSignal(AName), AData, AFreeType, ATimeout);
end;

function TQSignalQueue.Wait: TWaitResult;
begin
  Result := FNotify.WaitFor;
end;

{ TQSignalQueueExecutor }

procedure TQSignalQueueExecutor.Execute;
var
  AItem: PQSignalQueueItem;
begin
  while (not Terminated) do
  begin
    if FQueue.Wait = wrSignaled then
    begin
      repeat
        AItem := FQueue.Pop;
        if Assigned(AItem) then
        begin
          if Workers.Signal(AItem.Id, AItem.Data, AItem.FreeType, AItem.Timeout)
            = wrSignaled then
          begin
            if Assigned(AItem.WaitEvent) then
              TEvent(AItem.WaitEvent).SetEvent;
          end;
          Dispose(AItem);
        end
        else
          break;
      until Terminated;
    end;
  end;
end;

end.
