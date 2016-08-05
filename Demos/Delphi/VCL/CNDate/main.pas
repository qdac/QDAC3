unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm5 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses math, qcndate, cncalendar;
{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
var
  F: TfrmCnCalendar;
begin
F := TfrmCnCalendar.Create(Application);
F.ShowModal;
F.Free;
end;

procedure TForm5.FormCreate(Sender: TObject);
var
  APicker: TCnDatePicker;
begin
APicker := TCnDatePicker.Create(Self);
APicker.Parent := Self;
APicker.Left := 10;
APicker.Top := 10;
APicker := TCnDatePicker.Create(Self);
APicker.Parent := Self;
APicker.Left := 10;
APicker.Top := 30;
APicker.DisplayCnText:=False;
APicker.DisplayTime:=True;
end;

end.
