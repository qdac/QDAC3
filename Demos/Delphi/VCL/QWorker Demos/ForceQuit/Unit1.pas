unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QWorker, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoFrozen(AJob: PQJob);
    procedure DoFrozenWithLocalData(AJob: PQJob);
    procedure DoJobFrozen(AJob: PQJob);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  // 此示例用于帮助用户管理 ForceQuit 时内部分配的局部需要释放的实例，以尽量减少内存泄露

  TQDataFreeEvent = procedure(AInstance: Pointer; AReleaseCount: Integer)
    of object;
  PQJobLocalItem = ^TQJobLocalItem;

  TQJobLocalItem = record
    Instance: Pointer;
    ReleaseCount: Integer;
    OnFree: TQDataFreeEvent;
    Prior: PQJobLocalItem;
  end;

  TQJobLocalData = class
  private
    FLast: PQJobLocalItem;
    FTag: Pointer;
    procedure DoFreeObject(AInstance: Pointer; AReleaseCount: Integer);
    procedure DoFreeInterface(AInstance: Pointer; AReleaseCount: Integer);
    procedure InternalAdd(AInstance: Pointer; AOnFree: TQDataFreeEvent;
      AReleaseCount: Integer = MaxInt);
  public
    constructor Create(ATag: Pointer); overload;
    destructor Destroy; override;
    procedure Add(AObject: TObject); overload;
    procedure Add(AInterface: IInterface;
      AReleaseCount: Integer = MaxInt); overload;
    procedure Add(AData: Pointer; AOnFree: TQDataFreeEvent); overload;
    property Tag: Pointer read FTag;
  end;

  TSimpleObject = class
  public
    destructor Destroy; override;
  end;

  TSimpleInterface = class(TInterfacedObject)
  public
    destructor Destroy; override;
  end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I, ADelay: Integer;
begin
  for I := 0 to 19 do
  begin
    ADelay := random(20);
    Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' 作业 ' + IntToStr(I) +
      ' 在 ' + IntToStr(ADelay + 10) + ' 秒后将被强制退出');
    Workers.Delay(DoFrozen, ADelay * Q1Second, Pointer(I));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I, ADelay: Integer;
begin
  for I := 0 to 19 do
  begin
    ADelay := random(20);
    Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' 作业 ' + IntToStr(I) +
      ' 在 ' + IntToStr(ADelay + 10) + ' 秒后将被强制退出');
    Workers.Delay(DoFrozenWithLocalData, ADelay * Q1Second, TQJobLocalData.Create(Pointer(I)
      ), false, jdfFreeAsObject);
  end;
end;

procedure TForm1.DoFrozen(AJob: PQJob);
begin
  Sleep(MaxInt);
end;

procedure TForm1.DoFrozenWithLocalData(AJob: PQJob);
var
  AObj: TObject;
  AIntf: IInterface;
begin
  with TQJobLocalData(AJob.Data) do
  begin
    AObj := TSimpleObject.Create;
    Add(AObj);
    AIntf := TSimpleInterface.Create;
    Add(AIntf);
  end;
  // 进入僵尺状态，超时会释放AObj和AIntf实例
  Sleep(MaxInt);
end;

procedure TForm1.DoJobFrozen(AJob: PQJob);
begin
  RunInMainThread(
    procedure
    begin
      if not AJob.InMainThread then
      begin
        if IntPtr(AJob.Data)<20 then
          Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' 作业 ' +
            IntToStr(IntPtr(AJob.Data)) + ' 超时无响应，强制退出。')
        else
          Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' 作业 ' +
            IntToStr(IntPtr(TQJobLocalData(AJob.Data).Tag)) + ' 超时无响应，强制退出。');
        AJob.Worker.ForceQuit;
      end
      else
      begin
        // 如果主线程锁死，那么如果是处于alert模式，可以通过 QueueUserApc 来模拟复活，但不是啥可取的办法，尽量避免
      end;
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Workers.MaxWorkers := 10;
  Workers.JobFrozenTime := 10;
  Workers.OnJobFrozen := DoJobFrozen;
end;

{ TQJobLocalData }

procedure TQJobLocalData.Add(AObject: TObject);
begin
  if Assigned(AObject) then
    InternalAdd(AObject, DoFreeObject, MaxInt);
end;

procedure TQJobLocalData.Add(AInterface: IInterface; AReleaseCount: Integer);
begin
  if Assigned(AInterface) then
  begin
    AInterface._AddRef;
    InternalAdd(Pointer(AInterface), DoFreeInterface, AReleaseCount);
  end;
end;

procedure TQJobLocalData.Add(AData: Pointer; AOnFree: TQDataFreeEvent);
begin
  InternalAdd(AData, AOnFree);
end;

constructor TQJobLocalData.Create(ATag: Pointer);
begin
  inherited Create;
  FTag := ATag;
end;

destructor TQJobLocalData.Destroy;
var
  AItem: PQJobLocalItem;
  ATemp: TQJobLocalItem;
begin
  while Assigned(FLast) do
  begin
    try
      AItem := FLast;
      FLast := AItem.Prior;
      ATemp := AItem^;
      Dispose(AItem);
      if Assigned(ATemp.OnFree) then
        AItem.OnFree(ATemp.Instance, ATemp.ReleaseCount);
    except
    end;
  end;
  inherited;
end;

procedure TQJobLocalData.DoFreeInterface(AInstance: Pointer;
AReleaseCount: Integer);
begin
  with IInterface(AInstance) do
  begin
    // 这里需要用户根据实例情况自行处理，程序这里设计是强制保证接口被释放掉
    if _Release <> 0 then
    begin
      while (_Release <> 0) and (AReleaseCount > 0) do
        Dec(AReleaseCount);
    end;
  end;
end;

procedure TQJobLocalData.DoFreeObject(AInstance: Pointer;
AReleaseCount: Integer);
begin
  // 对于AUTO_REFCOUNT平台，这块应考虑调用_ObjRelease 和接口一样处理，保证对象释放
  FreeAndNil(AInstance);
end;

procedure TQJobLocalData.InternalAdd(AInstance: Pointer;
AOnFree: TQDataFreeEvent; AReleaseCount: Integer);
var
  AItem: PQJobLocalItem;
begin
  New(AItem);
  AItem.Instance := AInstance;
  AItem.OnFree := AOnFree;
  AItem.Prior := FLast;
  AItem.ReleaseCount := AReleaseCount;
  FLast := AItem;
end;

{ TSimpleObject }

destructor TSimpleObject.Destroy;
begin
  RunInMainThread(
    procedure
    begin
      TForm1(Application.MainForm).Memo1.Lines.Add('TSimpleObject 对象释放');
    end);
  inherited;
end;

{ TSimpleInterface }

destructor TSimpleInterface.Destroy;
begin
  RunInMainThread(
    procedure
    begin
      TForm1(Application.MainForm).Memo1.Lines.Add('TSimpleInterface 对象释放');
    end);
  inherited;
end;

end.
