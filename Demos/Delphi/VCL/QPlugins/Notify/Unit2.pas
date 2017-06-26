unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.StdCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Gauges,
  QPlugins,QPlugins_base,QPlugins_params;

type
  TForm2 = class(TForm, IQNotify)
    Gauge1: TGauge;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    FNotifyId: Integer;
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean); stdcall;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}
{ TForm2 }

procedure TForm2.CheckBox1Click(Sender: TObject);
var
  AMgr: IQNotifyManager;
begin
  AMgr := PluginsManager as IQNotifyManager;
  // 注册通知，事件的名称不需要特意注册，IdByName在名称不存在时，会自动注册并生成一个新的ID返回
  // 多个不同的 Id 就多次订阅即可
  FNotifyId := AMgr.IdByName('Tracker.Changed');

  if CheckBox1.Checked  then
     AMgr.Subscribe(FNotifyId, Self)
  else
    AMgr.unSubscribe(FNotifyId, Self);

end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
var
  AMgr: IQNotifyManager;
begin
  AMgr := PluginsManager as IQNotifyManager;
  // 注册通知，事件的名称不需要特意注册，IdByName在名称不存在时，会自动注册并生成一个新的ID返回
  // 多个不同的 Id 就多次订阅即可
  FNotifyId := AMgr.IdByName('Tracker.Changed');
  AMgr.unSubscribe(FNotifyId, Self);
  action := CaFree;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  AMgr: IQNotifyManager;
begin
  AMgr := PluginsManager as IQNotifyManager;
  // 注册通知，事件的名称不需要特意注册，IdByName在名称不存在时，会自动注册并生成一个新的ID返回
  // 多个不同的 Id 就多次订阅即可
  FNotifyId := AMgr.IdByName('Tracker.Changed');
  AMgr.Subscribe(FNotifyId, Self);
end;

procedure TForm2.Notify(const AId: Cardinal; AParams: IQParams;
  var AFireNext: Boolean);
begin


  if AId = FNotifyId then // 多个通知关联到同一个对象时，通过AId进行进行区分
  begin
    if not Visible then
      Show;
    Gauge1.Progress := AParams[0].AsInteger;
  end;
end;

end.
