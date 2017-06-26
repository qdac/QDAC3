unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, qstring,
  QPlugins, qplugins_base,QPlugins_params, QPlugins_loader_script;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  function ServiceSource(AService: IQService): QStringW;
  begin
    if Assigned(AService.loader) then
    begin
      SetLength(Result, MAX_PATH);
      SetLength(Result, AService.loader.GetServiceSource(AService,
        PWideChar(Result), MAX_PATH));
    end
    else
      Result := Application.ExeName;
  end;
  procedure DisplayVersion(AVer: IQVersion; AIndent: String);
  var
    AInfo: TQVersion;
  begin
    if AVer.GetVersion(AInfo) then
    begin
      Memo1.Lines.Add(AIndent + '版本信息:');
      Memo1.Lines.Add(AIndent + '  名称:' + AInfo.Name);
      Memo1.Lines.Add(AIndent + '  版本号:' + Format('%d.%d.%d.%d',
        [Integer(AInfo.Version.Major), Integer(AInfo.Version.Minor),
        Integer(AInfo.Version.Release), Integer(AInfo.Version.Build)]));
      Memo1.Lines.Add(AIndent + '  公司:' + AInfo.Company);
      Memo1.Lines.Add(AIndent + '  描述:' + AInfo.Description);
      Memo1.Lines.Add(AIndent + '  文件名:' + AInfo.FileName);
    end;
  end;
  procedure EnumChildServices(AParent: IQServices; AIndent: String = '');
  var
    I: Integer;
    AService: IQService;
    AChilds: IQServices;
    AVer: IQVersion;
  begin
    Memo1.Lines.Add(AIndent + AParent.Name + ' <- ' +
      ServiceSource(AParent as IQService));
    AIndent := AIndent + '  ';
    for I := 0 to AParent.Count - 1 do
    begin
      AService := AParent[I];
      if Supports(AService, IQServices, AChilds) then
        EnumChildServices(AChilds, AIndent)
      else
      begin
        Memo1.Lines.Add(AIndent + AService.Name + ' <- ' +
          ServiceSource(AService));
        if Supports(AService, IQVersion, AVer) then
          DisplayVersion(AVer, AIndent + '  ');
      end;
    end;
  end;

begin
  EnumChildServices(PluginsManager.Loaders);
  EnumChildServices(PluginsManager.Routers);
  EnumChildServices(PluginsManager.Services);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AParams, AResult: IQParams;
  AService: IQService;
begin
  AService := PluginsManager.ByPath('Services/计算1+2的值');
  if Assigned(AService) then
  begin
    AParams := TQParams.Create;
    AParams.Add('X', ptInt32).AsInteger := 100;
    AParams.Add('Y', ptInt32).AsInteger := 200;
    AResult := TQParams.Create;
    if AService.Execute(AParams, AResult) then
    begin
      if AResult.Count > 0 then
        Memo1.Lines.Add('100+200=' + IntToStr(AResult[0].AsInteger))
      else
        Memo1.Lines.Add('发生了什么？你改源码了吗？');
    end
    else
      Memo1.Lines.Add('发生错误：' + AService.LastErrorMsg);
  end
  else
    ShowMessage('服务脚本没找着，请将 SumPlugin.pas 复制程序执行目录。');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  S: QStringW;
  AFileName: String;
begin
  // 如果不存在脚本，则创建它
  AFileName := ExtractFilePath(Application.ExeName) + 'SumPlugin.pas';
  if not FileExists(AFileName) then
  begin
    S := 'unit SumPlugins;'#13#10 +
      'function Sum1Vs2(X,Y:Integer):Integer;'#13#10 + 'begin'#13#10 +
      'Result:=X+Y;'#13#10 + 'end;'#13#10 + 'procedure RegisterServices;'#13#10
      + 'begin'#13#10 +
      'RegisterService(''{334E47E0-E0FB-4101-9936-2EA638721C6E}'',''Services'',''计算1+2的值'',''Sum1Vs2'');'#13#10
      + 'end;'#13#10 + 'end.';
    SaveTextU(AFileName, S);
  end;
  PluginsManager.Loaders.Add(TQPascalScriptLoader.Create
    (ExtractFilePath(Application.ExeName), '.PAS'));
  PluginsManager.Start;
end;

end.
