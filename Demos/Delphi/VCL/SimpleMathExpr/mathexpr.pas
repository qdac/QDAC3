unit mathexpr;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AExpr: TMathExpression;
begin
  AExpr := TMathExpression.Create;
  AExpr.Add('X', TMathVolatile.mvImmutable).Value := 100;
  ShowMessage(VarToStr(AExpr.Eval(Edit1.Text)));
  FreeAndNil(AExpr);
end;



end.
