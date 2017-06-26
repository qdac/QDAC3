unit hostmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QPlugins,QPlugins_base, QPlugins_Params,
  QPlugins_Loader_Lib;

type
  TForm2 = class(TForm)
    edtPluginsFile: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure EnumFileServices(const AFileName: String);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  AExt: String;
  ALoader: IQLoader;
begin
  if OpenDialog1.Execute then
  begin
    AExt := UpperCase(ExtractFileExt(OpenDialog1.FileName));
    if AExt = '.DLL' then
    begin
      ALoader := PluginsManager.ByPath('/Loaders/Loader_DLL') as IQLoader;
      if not Assigned(ALoader) then
      begin
        ALoader := TQDLLLoader.Create('', '.dll');
        PluginsManager.Loaders.Add(ALoader);
      end;
    end
    else // BPL
    begin
      ALoader := PluginsManager.ByPath('/Loaders/Loader_BPL') as IQLoader;
      if not Assigned(ALoader) then
      begin
        ALoader := TQBPLLoader.Create('', '.bpl');
        PluginsManager.Loaders.Add(ALoader);
      end;
    end;
    ALoader.LoadServices(PWideChar(OpenDialog1.FileName));
    edtPluginsFile.Text:=OpenDialog1.FileName;
    EnumFileServices(OpenDialog1.FileName);
  end;
end;

procedure TForm2.EnumFileServices(const AFileName: String);
var
  AInst: HINST;
  procedure EnumServices(AService: IQService);
  var
    AList: IQServices;
    I: Integer;
  begin
    if Supports(AService, IQServices, AList) then
    begin
      for I := 0 to AList.Count - 1 do
        EnumServices(AList.Items[I]);
    end
    else
    begin
      if AService.GetOwnerInstance = AInst then
        Memo1.Lines.Add(ServicePath(AService));
    end;
  end;

begin
  AInst := GetModuleHandle(PChar(AFileName));
  Memo1.Lines.Add(AFileName + ' 注册的服务(基址:' + IntToHex(AInst, 8) + ')');
  EnumServices(PluginsManager as IQService);
end;

end.
