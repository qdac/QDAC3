unit vcl.form.bpl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, vcl.Graphics,
  vcl.Controls, vcl.Forms, vcl.Dialogs, vcl.StdCtrls, qplugins,
  qplugins_formsvc, qplugins_vcl_formsvc;

type
  TForm7 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

initialization

RegisterFormService('/Services/Docks/Forms', 'BPL_Form', TForm7, False);

finalization

UnregisterServices('/Services/Docks/Forms', ['BPL_Form']);

end.
