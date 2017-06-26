unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  QString,
  QPlugins,QPlugins_base,
  QPlugins_vcl_formsvc,
  QPlugins_params;

type
  TForm3 = class(TForm ,IQNotify)
    CheckBox1: TCheckBox;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
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
  Form3: TForm3;

implementation

{$R *.dfm}



procedure TForm3.CheckBox1Click(Sender: TObject);
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

procedure TForm3.FormCreate(Sender: TObject);
var
  AMgr: IQNotifyManager;
begin

  Label2.Caption := '实例创建时间：' + FormatDateTime('hh:nn:ss', Now);

  AMgr := PluginsManager as IQNotifyManager;
  // 注册通知，事件的名称不需要特意注册，IdByName在名称不存在时，会自动注册并生成一个新的ID返回
  // 多个不同的 Id 就多次订阅即可
  FNotifyId := AMgr.IdByName('Tracker.Changed');
  AMgr.Subscribe(FNotifyId, Self);
end;


procedure TForm3.FormDestroy(Sender: TObject);
var
  AMgr: IQNotifyManager;
begin
  AMgr := PluginsManager as IQNotifyManager;
  // 注册通知，事件的名称不需要特意注册，IdByName在名称不存在时，会自动注册并生成一个新的ID返回
  // 多个不同的 Id 就多次订阅即可
  FNotifyId := AMgr.IdByName('Tracker.Changed');
  AMgr.unSubscribe(FNotifyId, Self);

end;

procedure TForm3.Notify(const AId: Cardinal; AParams: IQParams;
  var AFireNext: Boolean);
begin

  if AId = FNotifyId then // 多个通知关联到同一个对象时，通过AId进行进行区分
  begin
    if not Visible then
      Show;
    ProgressBar1.Position  := AParams[0].AsInteger;
  end;
end;

initialization

RegisterServices('/Services/Docks/Forms',
  [TQVCLFormService.Create('DLL_Shared', TForm3.Create(nil),nil)]);
RegisterServices('/Services/Docks/Forms',
  [TQVCLFormService.Create('DLL_MutiInstance', TForm3)]);

finalization

UnregisterServices('/Services/Docks/Forms', ['DLL_Shared', 'DLL_MutiInstance']);

end.
