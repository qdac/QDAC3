unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvComponentBase,
  JvInterpreter, JvInterpreterFm, Vcl.Menus;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    mmLogs: TMemo;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    MainMenu1: TMainMenu;
    Services1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DoClickMenu(ASender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qpluginsintf, qplugins,qplugins_base, qplugins_mgr, directsvc;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if PluginsManager = nil then
    Caption := '插件管理器未找到，不支持插件功能。'
  else
    Caption := '插件管理器已创建';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AParams: TQParams;
  AStream: TMemoryStream;
begin
  AParams := TQParams.Create;
  AParams.Add('Id', 100);
  AParams.Add('Name', 'Jone smith');
  with AParams.AddArray('Array') do
  begin
    Add(ptFloat8).AsFloat := 50.6;
  end;
  ShowMessage(AParams.AsJson);
  AStream := TMemoryStream.Create;
  AParams.SaveToStream(AStream);
  AStream.Position := 0;
  AParams.LoadFromStream(AStream);
  ShowMessage(AParams.AsJson);
  FreeAndNil(AParams);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
begin
  PluginsManager.Start;
  for I := 0 to PluginsManager.GetServices.GetCount - 1 do
  begin
    mmLogs.Lines.Add('服务 ' + PluginsManager.GetServices.GetItems(I).GetName
      + ' 已注册');
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  PluginsManager.Stop;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  AService: IQPluginService;
  AParams,AResult: IQParams;
begin
  AService := PluginsManager.ByPath('Services/计算1+2的值');
  if AService <> nil then
  begin
    AParams := TQParams.Create;
    AParams.Add('X', ptInt32).AsInteger := 10;
    AParams.Add('Y', ptInt32).AsInteger := 20;
    if AService.Execute(AParams,AResult) then
    begin
      mmLogs.Lines.Add('执行服务 ' + AService.GetName + ' 成功');
      mmLogs.Lines.Add((AParams[0] as TQParam).AsString + '+' +
        (AParams[1] as TQParam).AsString + '=' + (AResult[0] as TQParam)
        .AsString);
    end
    else
      mmLogs.Lines.Add('执行服务 ' + AService.GetName + ' 失败' +
        AService.GetLastErrorMsg);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  AService: IMyService;
begin
  AService := PluginsManager.ByPath('Services/MyService') as IMyService;
  if Assigned(AService) then
  begin
    mmLogs.Lines.Add('MyService 服务已经找到');
    mmLogs.Lines.Add('MyService.Sum(1+2)=' + IntToStr(AService.Sum(1, 2)));
    mmLogs.Lines.Add('MyService.Max(1,2)=' + IntToStr(AService.Max(1, 2)));
    mmLogs.Lines.Add('MyService.Min(1,2)=' + IntToStr(AService.Min(1, 2)));
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  AService: IMyService;
  APluginSvc: IQPluginService;
begin
  AService := PluginsManager as IMyService;
  mmLogs.Lines.Add('MyService.Sum(10+20)=' + IntToStr(AService.Sum(10, 20)));
  AService := nil;
  if Supports(PluginsManager, IMyService, AService) then
    mmLogs.Lines.Add('MyService.Max(30,10)=' + IntToStr(AService.Max(30, 10)));
end;

procedure TForm1.DoClickMenu(ASender: TObject);
var
  AService:IQPluginService;
  AParams,AResult:IQParams;
begin
AParams:=TQParams.Create;
AResult:=TQParams.Create;
AService:=IQPluginService(Pointer(TMenuItem(ASender).Tag));
AParams.Add('Sender',ptPointer).AsPointer:=ASender;
AService.Execute(AParams,AResult);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ARoot:IQPluginServices;
  procedure CreateMenus(AParentMenu: TMenuItem; AParentList: IQPluginServices);
  var
    AItem: IQPluginService;
    AMenu: TMenuItem;
    AChildList:IQPluginServices;
    I: Integer;
  begin
    if Assigned(AParentList) then
    begin
      for I := 0 to AParentList.Count - 1 do
      begin
        AItem := AParentList[I];
        AMenu := TMenuItem.Create(Self);
        AMenu.Caption := AItem.Name;
        AMenu.Tag := IntPtr(AItem);
        AParentMenu.Add(AMenu);
        if AItem.QueryInterface(IQPluginServices,AChildList)<>S_OK then
          AMenu.OnClick := DoClickMenu
        else
          CreateMenus(AMenu, AChildList);
      end;
    end;
  end;

begin
  MainMenu1.Items.Clear;
  if Supports(PluginsManager.RequireService('Services/Menu'),IQPluginServices,ARoot) then
    CreateMenus(MainMenu1.Items,ARoot);
end;

end.
