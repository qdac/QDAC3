unit mdiform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm2 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses qplugins, qplugins_vcl_formsvc;
{$R *.dfm}

initialization

RegisterFormService('/Services/Forms/MDI', 'MDITest', TForm2);

finalization

UnregisterServices('/Services/Forms/MDI', ['MDITest']);

end.
