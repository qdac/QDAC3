unit dllform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qplugins, qplugins_vcl_formsvc,
  Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  UnloadServices(HInstance, False);
end;

initialization

RegisterFormService('Services/Forms', 'DynamicLoadForm', TForm2, False);

finalization

UnregisterServices('Services/Forms', ['DynamicLoadForm']);

end.
