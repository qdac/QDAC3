unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, qplugins, qplugins_params;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  IQMyServiceExt = interface
    ['{46DD594D-0A3D-49E6-BE86-61E653EB3589}']
    procedure SayHello;
  end;

  TQMyServiceExt = class(TQInterfacedObject, IQMyServiceExt)
  private
    procedure SayHello;
  public
    constructor Create; override;
  end;

var
  Form1: TForm1;

implementation

uses qstring;
{$R *.dfm}
{ TQMyServiceExt }

constructor TQMyServiceExt.Create;
begin
  inherited;
end;

procedure TQMyServiceExt.SayHello;
begin
  ShowMessage('Hello,QDAC.QPlugins.Extension');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AService: IQService;
begin
  AService := GetService('/Services/Message');
  (AService as IQMyServiceExt).SayHello;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
(PluginsManager as IQMyServiceExt).SayHello;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AService: TQService;
begin
  AService := TQService.Create(NewId, 'Message');
  AService.AddExtension(TQMyServiceExt.Create);
  RegisterServices('Services', [AService]);
end;

end.
