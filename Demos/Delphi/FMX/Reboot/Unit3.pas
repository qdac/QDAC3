unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    Label3: TLabel;
    Button1: TButton;
    CalloutPanel1: TCalloutPanel;
    Image1: TImage;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.Button1Click(Sender: TObject);
begin
Application.Terminate;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
Label3.Text:='重启或关闭设备需要Root授权！'#13#10'未获取到相关授权可能因为：'#13#10+
'1、设备未Root，请使用相关工具软件进行Root。'#13#10+
'2、您未为本程序提供相应的权限，请重新提供权限。';
end;

end.
