unit hostmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qplugins,qplugins_base, qplugins_formsvc,
  qplugins_vcl_formsvc, qplugins_loader_lib, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AService: IQFormService;
begin
  if Supports(PluginsManager.ByPath('Services/Docks/Forms/VSTForm'),
    IQFormService, AService) then
  begin
    AService.DockTo(Panel1.Handle, Panel1.ClientRect);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// 注意：由于 DLL 中的服务使用的 VirtualStringTree 创建了后台线程
//如果不先注销后台服务的话，应用程序退出时，会由于 Delphi 自身实现的问题
//造成程序无法退出。在这里先移除注册，然后再正常关闭就好了
  UnregisterServices('Services/Docks/Forms', ['VSTForm']);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PluginsManager.Loaders.Add
    (TQDLLLoader.Create(ExtractFilePath(Application.ExeName), '.DLL'));
  PluginsManager.Start;
end;

end.
