unit AboutFrm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  ShellAPI,
  qplugins, qplugins_base,qplugins_params, menusvc;

type
  TfrmAbout = class(TForm)
    bvl1: TBevel;
    lblHomePage: TLabel;
    pnlBottom: TPanel;
    lblVersion: TLabel;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lblHomePageClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    procedure GetFileVersion;
  public
    { Public declarations }
  end;

  type
   TFileVersionInfo = packed record
    FixedInfo: TVSFixedFileInfo; {版本信息}
    CompanyName:String; {公司名称}
    FileDescription:String; {说明}
    FileVersion:String; {文件版本}
    InternalName:String; {内部名称}
    LegalCopyright:String; {版权}
    LegalTrademarks:String; {合法商标}
    OriginalFilename:String; {源文件名}
    ProductName:String; {产品名称}
    ProductVersion:String; {产品版本}
    Comments:String; {备注}
    LocalPort:String;
  end;



implementation



{$R *.dfm}


procedure TfrmAbout.btn1Click(Sender: TObject);
begin
  Close();
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Caption := '关于 ' + Application.Title;
  GetFileVersion();
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  //检测更新
//  if FileExists(ExtractFilePath(Application.ExeName) + APPFILE_Update_exe) then
//  begin
//    ShellExecute(Application.Handle, PChar('open'),
//      PChar(ExtractFilePath(Application.ExeName) + APPFILE_Update_exe),
//      PChar('/s'), nil, SW_SHOWNORMAL);
//  end;
end;

procedure TfrmAbout.GetFileVersion;
var
  FileVersionInfo: TFileVersionInfo;
begin
//  if GetFileVerInfo(Application.ExeName, FileVersionInfo) then
//  begin
//    lblVersion.Caption := '当前版本: ' +
//      IntToStr(HIWORD(FileVersionInfo.FixedInfo.dwFileVersionMS)) + '.' +
//      IntToStr(LOWORD(FileVersionInfo.FixedInfo.dwFileVersionMS));
//  end;
end;

procedure TfrmAbout.lblHomePageClick(Sender: TObject);
begin
  ShellExecute(Handle, PChar('open'),
    PChar('http://xxxxx.com'), nil, nil, SW_SHOWMAXIMIZED);
end;


type
  TShowFormAction = class(TQInterfacedObject, IQNotify)
  protected
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean); stdcall;
  end;
  { TShowFormAction }

procedure TShowFormAction.Notify(const AId: Cardinal; AParams: IQParams;
  var AFireNext: Boolean);
var
  F: TfrmAbout;
  I: Integer;
  AName: String;
begin
  if Assigned(AParams) and (ParamAsString(AParams.ByName('Name')) = 'About') then
  begin
    F := TfrmAbout.Create(Application);
    F.ShowModal;
    F.Free;
  end;
end;

var
  AFormAction: IQNotify;

procedure DoMenuServiceReady2(const AService: IQService); stdcall;
var
  F: TfrmAbout;
begin
  with AService as IQMenuService do
  begin
    AFormAction := TShowFormAction.Create;
    with RegisterMenu('/Help/About', AFormAction) do
    begin
      Caption := '关于(&S)';
      //F := TfrmAbout.Create(nil);
      //SetImage(TBitmap(F.img1.Picture.Graphic).Handle);
       Params := NewParams([]);
      Params.Add('Name', ptUnicodeString).AsString := NewString('About');
      //F.Free;
    end;

  end;
end;

initialization

  AFormAction := nil;
  PluginsManager.WaitService(IQMenuService, DoMenuServiceReady2);
  //DoMenuServiceReady2;
finalization

if Assigned(AFormAction) then
begin
  with PluginsManager as IQMenuService do
    UnregisterMenu('/Help/About', AFormAction);
  AFormAction := nil;
end;

end.
