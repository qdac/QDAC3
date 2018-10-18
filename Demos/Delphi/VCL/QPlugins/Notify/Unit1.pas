unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  QString,
  QPlugins,
  QPlugins_base,
  qplugins_loader_lib,
  QPlugins_params,
  qplugins_Vcl_formsvc,
  qplugins_Vcl_Messages,
  qplugins_formsvc,

  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FChangeNotifyId: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  FL:TInterfaceList;   //保存窗体插件实例指针


implementation

uses
  unit2;
{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
begin
    TForm2.Create(application);
end;

//建立DLL插件窗体
procedure TForm1.Button2Click(Sender: TObject);
var
  AFormService: IQFormService;
begin
  if Supports(PluginsManager.ByPath('/Services/Docks/Forms/DLL_MutiInstance'),
    IQFormService, AFormService) then
  begin
    FL.Add(AFormService);
    //ShowMessage(ServiceSource(AFormService as IQService));
    AFormService.Show;

  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AParams:TQParams;
begin
AParams:=TQParams.Create;
AParams.Add('Id',100);
AParams.Add('Name','高原');
AParams.Add('IsFemale',false);
AParams.Add('Scale',75.6);
AParams.Add('Desc','这是测试'#13#10'高原有很多种');
AParams.Add('Array',NewParams([100,'QDAC',3.45]));
ShowMessage((AParams as IQParams).AsString.Value);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FL.Free ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  APath: String;
begin
  ReportMemoryLeaksOnShutdown := True;
  APath := ExtractFilePath(Application.ExeName);
  PluginsManager.Loaders.Add(TQDLLLoader.Create(APath, '.dll'));
  PluginsManager.Start;
  FChangeNotifyId := (PluginsManager as IQNotifyManager)
    .IdByName('Tracker.Changed');
  FL := TInterfaceList.Create ;

end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  (PluginsManager as IQNotifyManager).Send(FChangeNotifyId,
    NewParams([TrackBar1.Position]));
end;

end.
