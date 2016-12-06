unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, qcss_fmx;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qstring;
{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  AObj: TQCSSFMXHelper;
begin
  AObj := TQCSSFMXHelper.Create;
  AObj.Parse(Memo1.Text);
  AObj.ApplyStyle('div', Layout1);
  AObj.DisposeOf;
end;

end.
