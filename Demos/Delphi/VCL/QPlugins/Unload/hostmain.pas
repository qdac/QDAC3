unit hostmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, qplugins,
  qplugins_base,qplugins_params, qplugins_loader_lib, Vcl.ComCtrls, qplugins_formsvc,
  qplugins_vcl_formsvc;

{ 要安全的移除一项服务，则使用该服务的模块必需实现IQNotify接口，并对NID_PLUGIN_UNLOADING
  通知的响应，并在通知里移除掉到服务的引用，以便插件能够被安全的释放。

  如果需要在某个插件注册后，引用某项服务，可以响应NID_PLUGIN_LOADED通知，在其中查询新的
  服务实例并记录它
}
type
  TForm1 = class(TForm, IQNotify)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mmLogs: TMemo;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FHoldService: IQService;
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean); stdcall;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AFileName: String;
  ALoader: IQLoader;
begin
  AFileName := ExtractFilePath(Application.ExeName) + 'plugins.dll';
  if FileExists(AFileName) then
  begin
    ALoader := PluginsManager.ByPath('/Loaders/Loader_DLL') as IQLoader;
    if Assigned(ALoader) then
      ALoader.LoadServices(PWideChar(AFileName));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AModule: HMODULE;
  ALoader: IQLoader;
  function ServiceModule: HMODULE;
  begin
    if Assigned(FHoldService) then
      Result := FHoldService.GetOwnerInstance
    else
      Result := 0;
  end;

begin
  AModule := ServiceModule;
  if AModule <> 0 then
  begin
    ALoader := PluginsManager.ByPath('/Loaders/Loader_DLL') as IQLoader;
    if Assigned(ALoader) then
      ALoader.UnloadServices(AModule);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AFormService: IQFormService;
begin
  if GetService('Services/Forms/DynamicLoadForm', IQFormService, AFormService)
  then
  begin
    AFormService.DockTo(TabSheet2.Handle, faContent);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with PluginsManager as IQNotifyManager do
  begin
    Subscribe(NID_PLUGIN_LOADING, Self);
    Subscribe(NID_PLUGIN_UNLOADING, Self);
    Subscribe(NID_PLUGIN_LOADED, Self);
  end;
  PluginsManager.Loaders.Add
    (TQDLLLoader.Create(ExtractFilePath(Application.ExeName), '.dll'));
  PluginsManager.Start;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  with PluginsManager as IQNotifyManager do
  begin
    Unsubscribe(NID_PLUGIN_LOADING, Self);
    Unsubscribe(NID_PLUGIN_UNLOADING, Self);
    Unsubscribe(NID_PLUGIN_LOADED, Self);
  end;
end;

procedure TForm1.Notify(const AId: Cardinal; AParams: IQParams;
  var AFireNext: Boolean);
var
  AParam: IQParam;
begin
  if Assigned(AParams) then
  begin
    case AId of
      NID_PLUGIN_LOADING:
        begin
          AParam := AParams.ByName('File');
          mmLogs.Lines.Add('正在加载插件 ' + ParamAsString(AParam) + ' ...');
        end;
      NID_PLUGIN_LOADED:
        begin
          FHoldService := PluginsManager.ByPath('Services/HoldService');
          if Assigned(FHoldService) then
            mmLogs.Lines.Add('HoldService 已经成功加载');
        end;
      NID_PLUGIN_UNLOADING:
        begin
          AParam := AParams.ByName('Instance');
          if Assigned(AParam) and
            (FHoldService.GetOwnerInstance = AParam.AsInt64) then
          begin
            FHoldService := nil;
            AParam := AParams.ByName('File');
            mmLogs.Lines.Add('正在卸载插件' + ParamAsString(AParam) + '，移除关联服务 ...');
          end;
        end;
    end;

  end;
end;

end.
