unit hostmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, qstring, qplugins,
  qplugins_base, qplugins_qworker;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

type
  TSimpleJob = class(TInterfacedObject, IQJobCallback)
  protected
    FForm: TForm3;
    FRunTimes: Integer;
    procedure DoJob(AParams: IQParams); stdcall;
    procedure AfterDone; stdcall;
    procedure BeforeCancel; stdcall;
  public
    constructor Create(AForm: TForm3); overload;
    procedure BeforeDestruction; override;
  end;
{$IFDEF UNICODE}

  TQJobCallbackA = reference to procedure(AParams: IQParams);

  TSimpleJobA = class(TInterfacedObject, IQJobCallback)
  protected
    FCallback: TQJobCallbackA;
    procedure DoJob(AParams: IQParams); stdcall;
    procedure AfterDone; stdcall;
    procedure BeforeCancel; stdcall;
  public
    constructor Create(ACallback: TQJobCallbackA);
  end;
{$ENDIF}

procedure TForm3.Button1Click(Sender: TObject);
var
  AParams: IQParams;
begin
  AParams := PluginsManager.NewParams;
  AParams.Add('Text', ptUnicodeString).AsString :=
    PluginsManager.NewString('Hello,world');
  (PluginsManager as IQWorkers).Post(TSimpleJob.Create(Self), AParams, true);
end;

{ TSimpleJob }

procedure TSimpleJob.AfterDone;
begin
  DebugOut('作业执行完成');
end;

procedure TSimpleJob.BeforeCancel;
begin
  DebugOut('作业已经取消');
end;

procedure TSimpleJob.BeforeDestruction;
begin
  inherited;
  if FRunTimes > 0 then
    AfterDone
  else
    BeforeCancel;
end;

constructor TSimpleJob.Create(AForm: TForm3);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TSimpleJob.DoJob(AParams: IQParams);
var
  I: Integer;
begin
  FForm.Memo1.Lines.BeginUpdate;
  try
    for I := 0 to AParams.Count - 1 do
      FForm.Memo1.Lines.Add(Format('%s - %s', [AParams[I].Name,
        AParams[I].AsString.Value]));
  finally
    FForm.Memo1.Lines.EndUpdate;
    Inc(FRunTimes);
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  AParams: IQParams;
begin
  // 这个示例演示了转化为Delphi的匿名函数方式调用
  AParams := PluginsManager.NewParams;
  AParams.Add('Text', ptUnicodeString).AsString :=
    PluginsManager.NewString('Hello,world');
  (PluginsManager as IQWorkers)
    .Post(TSimpleJobA.Create(
    procedure(AParams: IQParams)
    var I: Integer;
    begin
      Memo1.Lines.BeginUpdate;
      try
        for I := 0 to AParams.Count - 1 do
        Memo1.Lines.Add(Format('%s - %s',[AParams[I].Name, AParams[I].AsString.Value]));
    finally
      Memo1.Lines.EndUpdate;
    end;
  end), AParams, true);
end;

{ TSimpleJobA }

procedure TSimpleJobA.AfterDone;
begin

end;

procedure TSimpleJobA.BeforeCancel;
begin

end;

constructor TSimpleJobA.Create(ACallback: TQJobCallbackA);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TSimpleJobA.DoJob(AParams: IQParams);
begin
  FCallback(AParams);
end;

end.
