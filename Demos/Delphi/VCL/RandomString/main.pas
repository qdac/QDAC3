unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
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

uses qstring;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  ACharTypes: TRandCharTypes;
begin
  ACharTypes := [];
  if CheckBox1.Checked then
    ACharTypes := ACharTypes + [rctAlpha];
  if CheckBox2.Checked then
    ACharTypes := ACharTypes + [rctNum];
  if CheckBox3.Checked then
    ACharTypes := ACharTypes + [rctChinese];
  if CheckBox4.Checked then
    ACharTypes := ACharTypes + [rctSymbol];
  if CheckBox5.Checked then
    ACharTypes := ACharTypes + [rctSpace];
  Memo1.Lines.Add(RandomString(SpinEdit1.Value, ACharTypes));
end;

end.
