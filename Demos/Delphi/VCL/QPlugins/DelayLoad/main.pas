unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring, qjson, qplugins, qplugins_base,qplugins_loader_lib, qplugins_params,
  qplugins_router_delayload;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AService: IQService;
  AResult: IQParams;
begin
  AService := PluginsManager.ByPath('/Services/Math/Sum');
  if AService <> nil then
  begin
    AResult := TQParams.Create;
    Memo1.Lines.Add('调用服务 /Services/Math/Sum 计算 100+200=');
    if AService.Execute(NewParams([100, 200]), AResult) then
      Memo1.Lines.Add(IntToStr(AResult[0].AsInteger))
    else
      Memo1.Lines.Add('执行失败:' + AService.LastErrorMsg);
  end
  else
    Memo1.Lines.Add('执行失败:服务未找到');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AJson: TQJson;
  ADelayRouter: TQDelayRouter;
  APath: String;
begin
  APath := ExtractFilePath(Application.ExeName);
  AJson := TQJson.Create;
  AJson.DataType := jdtArray;
  with AJson.Add() do
  begin
    Add('Id').AsString := GUIDToString(NewId);
    Add('Path').AsString := '/Services/Math/Sum';
    Add('Loader').AsString := 'Loader_DLL';
    Add('Module').AsString := 'delaydll.dll';
  end;
  AJson.SaveToFile(APath + 'delayload.config');
  FreeAndNil(AJson);
  PluginsManager.Loaders.Add(TQDLLLoader.Create(APath, '.dll'));
  ADelayRouter := TQDelayRouter.Create;
  ADelayRouter.ConfigFile := ExtractFilePath(Application.ExeName) +
    'delayload.config';
  PluginsManager.Routers.Add(ADelayRouter);
  // 不要调用 PluginsManager.Start 以避免初始化delaydll.dll
  ReportMemoryLeaksOnShutdown := True;
end;

end.
