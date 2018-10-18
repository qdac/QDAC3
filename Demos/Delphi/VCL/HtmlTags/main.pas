unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qdac.htmlparser;
{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  ATags: TQHTMLTag;
begin
  ATags := TQHTMLTag.Create;
  try
    ATags.Parse
      ('<body>Hello,world<font color="red" size=9px> is not word.</Font></BODY>');
    Memo1.Lines.Add(ATags.InnerHtml);
  finally
    ATags.Free;
  end;
end;

end.
