unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses unit2;
{$R *.dfm}
procedure ShowNoActive(AForm: TForm);
begin
ShowWindow(AForm.Handle, SW_SHOWNA);
PBoolean(@AForm.Visible)^ := True;
AForm.Perform(CM_VISIBLECHANGED, 1, 0);
end;

procedure TForm1.Label1MouseEnter(Sender: TObject);
begin
if not Assigned(Form2) then
  Form2:=TForm2.Create(Application);
if not Form2.Visible then
  begin
  Form2.SetBounds(Mouse.CursorPos.X,Mouse.CursorPos.Y,Form2.Width,Form2.Height);
  ShowNoActive(Form2);
  end;
end;

procedure TForm1.Label1MouseLeave(Sender: TObject);
begin
if Assigned(Form2) then
  Form2.Hide;
end;

end.
