unit scriptplugin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses qpluginsintf;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  V1,V2:Double;
begin
if TryStrToFloat(Edit1.Text,V1) and TryStrToFloat(Edit2.Text,V2) then
  Label1.Caption:='ºÏ¼Æ='+FloatToStr(V1);
end;

procedure RegisterPlugins(AMgr:IQPluginsManager);
begin
//AMgr.GetServices.Add('{FFBCEC43-60B6-4E60-888D-E0F90E439F15}',)
end;

end.
