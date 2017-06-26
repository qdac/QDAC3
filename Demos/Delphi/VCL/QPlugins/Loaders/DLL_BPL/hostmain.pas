unit hostmain;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, QPlugins, DB, AdoInt,
  ADODB,
  ExtCtrls, Grids, DBGrids, StdCtrls, QString, QSimplePool,qplugins_base, QPlugins_params,
  QPlugins_loader_lib, QPlugins_Vcl_Messages;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    ADODataSet1: TADODataSet;
    DataSource1: TDataSource;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Splitter1: TSplitter;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IDBService = interface
    ['{D0B7605F-6E95-4F0E-81AD-E282BBC2A1E1}']
    function OpenDataStream(AStream: IQStream; ACmdText: PWideChar): Boolean;
    function ExecuteCmd(ACmdText: PWideChar): Boolean;
  end;

  ISumService = interface
    ['{8501374C-793F-48FF-ACE7-898E32EBA174}']
    function Sum(X, Y: Integer): Integer; stdcall;
  end;

  ICppSumService = interface
    ['{F98325B7-05EB-4268-83A3-3E260D3B2A99}']
    function Add(X, Y: Integer): Integer; stdcall;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure AdoSaveToStream(pvDataSet: TCustomADODataSet; pvStream: TStream);
begin
  OLEVariant(pvDataSet.Recordset).Save(TStreamAdapter.Create(pvStream)
    as IUnknown, adPersistADTG); // adPersistXML
end;

procedure AdoLoadFromStream(pvDataSet: TCustomADODataSet; pvStream: TStream);
var
  AR: _Recordset;
begin
  AR := _Recordset(CoRecordset.Create);
  pvStream.Position := 0;
  AR.Open(TStreamAdapter.Create(pvStream) as IUnknown, EmptyParam,
    adOpenUnspecified, adLockUnspecified, -1);
  pvDataSet.Recordset := AdoInt._Recordset(AR);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AService: IDBService;
  ASQL: QStringW;
  AStream: TMemoryStream;
begin
  if Supports(PluginsManager, IDBService, AService) then
  begin
    AStream := TMemoryStream.Create;
    try
      ASQL := 'select * from sysobjects';
      if AService.OpenDataStream(QStream(AStream, False), PWideChar(ASQL)) then
      begin
        AStream.Position := 0;
        AStream.SaveToFile('C:\temp\sysobjs.adtg');
        AdoLoadFromStream(ADODataSet1, AStream);
      end
      else
        ShowMessage((AService as IQService).LastErrorMsg);
    finally
      FreeObject(AStream);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AService: IDBService;
  ASQL: QStringW;
begin
  if Supports(PluginsManager, IDBService, AService) then
  begin
    ASQL := 'select * from syscolumns';
    if AService.ExecuteCmd(PWideChar(ASQL)) then
      ShowMessage('执行成功完成。')
    else
      ShowMessage((AService as IQService).LastErrorMsg);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
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
    Memo1.Lines.Add(AIndent + AParent.Name + ' <- 来自 ' +
      ServiceSource(AParent as IQService));
    AIndent := AIndent + '  ';
    for I := 0 to AParent.Count - 1 do
    begin
      AService := AParent[I];
      if Supports(AService, IQServices, AChilds) then
        EnumChildServices(AChilds, AIndent)
      else
      begin
        Memo1.Lines.Add(AIndent + AService.Name + ' <- 来自 ' +
          ServiceSource(AService)); // 这里服务来源无法出错，原因暂时未知
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

procedure TForm1.Button4Click(Sender: TObject);
var
  AService: ISumService;
begin
  AService := PluginsManager.ByPath('Services/Sum') as ISumService;
  if Assigned(AService) then
    Memo1.Lines.Add('100+300=' + IntToStr(AService.Sum(100, 300)));
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  AService: IQService;
  AResult: IQParams;
  AFileName: QStringW;
  ALoader: IQLoader;
begin
  AService := PluginsManager.ByPath('/Services/Math/Sum');
  if not Assigned(AService) then
  begin
    AFileName := ExtractFilePath(Application.ExeName) + 'dynload\dyndll.dll';
    ALoader := PluginsManager.Loaders[0] as IQLoader;
    if ALoader.LoadServices(PWideChar(AFileName)) = 0 then
    begin
      ShowMessage(ALoader.LastErrorMsg);
      Exit;
    end;
    AService := PluginsManager.ByPath('/Services/Math/Sum');
  end;
  if AService <> nil then
  begin
    SetLength(AFileName, MAX_PATH);
    AService.loader.GetServiceSource(AService, PQCharW(AFileName), MAX_PATH);
    ShowMessage(AFileName);
    AResult := TQParams.Create;
    Memo1.Lines.Add('调用服务 /Services/Math/Sum 计算 100+200=');
    if AService.Execute(Newparams([100, 200]), AResult) then
      Memo1.Lines.Add(IntToStr(AResult[0].AsInteger))
    else
      Memo1.Lines.Add('执行失败:' + AService.LastErrorMsg);
  end
  else
    Memo1.Lines.Add('执行失败:服务未找到');
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  AService: IQService;
  AResult: IQParams;
  AFileName: QStringW;
  AStream: TFileStream;
  ALoader: IQLoader;
begin
  AFileName := ExtractFilePath(Application.ExeName) + 'dynload\dyndll.dll';
  if FileExists(AFileName) then
  begin
    AStream := TFileStream.Create(AFileName, fmOpenRead);
    ALoader := PluginsManager.ByPath('Loaders/Loader_DLL') as IQLoader;
    ALoader.LoadServices(QStream(AStream, True));
    AService := PluginsManager.ByPath('/Services/Math/Sum');
    if AService <> nil then
    begin
      AResult := TQParams.Create;
      Memo1.Lines.Add('调用服务 /Services/Math/Sum 计算 100+200=');
      if AService.Execute(Newparams([100, 200]), AResult) then
        Memo1.Lines.Add(IntToStr(AResult[0].AsInteger))
      else
        Memo1.Lines.Add('执行失败:' + AService.LastErrorMsg);
    end
    else
      Memo1.Lines.Add('执行失败:服务未找到');
  end
  else
    ShowMessage('服务文件未找到，请确认dyndll.dll已经编译并放到正确的目录下。');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  APath: String;
begin
  ReportMemoryLeaksOnShutdown := True;
  APath := ExtractFilePath(Application.ExeName);
  PluginsManager.Loaders.Add(TQDLLLoader.Create(APath, '.dll'));
  PluginsManager.Loaders.Add(TQBPLLoader.Create(APath, '.bpl'));
  PluginsManager.Start;
end;

end.
