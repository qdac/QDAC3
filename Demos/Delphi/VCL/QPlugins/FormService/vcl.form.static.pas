unit vcl.form.static;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, vcl.Graphics,
  vcl.Controls, vcl.Forms, vcl.Dialogs, vcl.StdCtrls, vcl.ExtCtrls;

type
  TForm4 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses QString, QPlugins, qplugins_vcl_formsvc;

procedure TForm4.Button1Click(Sender: TObject);
begin
  FreeObject(Self);
end;

initialization

RegisterFormService('/Services/Docks/Forms', 'DLL_Static', TForm4, False);

finalization

UnregisterServices('/Services/Docks/Forms', ['DLL_Static']);

end.
