unit hostmain;

interface

{
  VCL DLL或主程序需要引用 QPlugins.VCL 单元，该单元实现了替代的消息处理和派发
}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, QPlugins, QPlugins_loader_lib, QPlugins_Vcl_Messages;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PageControl1Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IDockableControl = interface
    ['{D0A4BDFA-CB29-4725-9158-C199B9C373C9}']
    procedure DockTo(AHandle: HWND); stdcall;
    procedure HostSizeChanged; stdcall;
  end;

  TDockHostPage = class(TTabSheet)
  private
    procedure SetDockedControl(const Value: IDockableControl);
  protected
    FDockedControl: IDockableControl;
  public
    property DockedControl: IDockableControl read FDockedControl
      write SetDockedControl;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  ACtrl: IDockableControl;
  ATabSheet: TDockHostPage;
begin
  ACtrl := PluginsManager.ByPath('Services/Docks/Frame') as IDockableControl;
  if Assigned(ACtrl) then
  begin
    ATabSheet := TDockHostPage.Create(PageControl1);
    ATabSheet.PageControl := PageControl1;
    ATabSheet.Caption := '第 ' + IntToStr(PageControl1.PageCount) + ' 页';
    ATabSheet.DockedControl := ACtrl;
    PageControl1.ActivePage := ATabSheet;
  end
  else
    Application.MessageBox('Services/Docks/Frame 服务未注册，请编译DLL先。', '服务未注册',
      MB_OK or MB_ICONSTOP);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  APath: String;
begin
  ReportMemoryLeaksOnShutdown := True;
  APath := ExtractFilePath(Application.ExeName);
  // 注册默认的 DLL 加载器，扩展名可以根据实际的情况随意修改，多个扩展名之间用逗号或分号分隔
  PluginsManager.Loaders.Add(TQDLLLoader.Create(APath, '.dll'));
  // 启动插件注册，如果要显示加载进度，可以注册IQNotify响应函数响应进度通知
  PluginsManager.Start;
end;

procedure TForm1.PageControl1Resize(Sender: TObject);
var
  I: Integer;
  APage: TDockHostPage;
begin
  for I := 0 to PageControl1.PageCount - 1 do
  begin
    APage := PageControl1.Pages[I] as TDockHostPage;
    if APage.DockedControl <> nil then
      APage.DockedControl.HostSizeChanged;
  end;
end;

{ TDockHostPage }

procedure TDockHostPage.SetDockedControl(const Value: IDockableControl);
begin
  if FDockedControl <> Value then
  begin
    FDockedControl := Value;
    if Assigned(Value) then
      Value.DockTo(Handle);
  end;
end;

end.
