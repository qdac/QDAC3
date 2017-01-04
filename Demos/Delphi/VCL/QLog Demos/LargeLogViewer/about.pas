unit about;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TfrmAbout = class(TForm)
    Image1: TImage;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses shellapi;
{$R *.dfm}

procedure TfrmAbout.Label2Click(Sender: TObject);
begin
  ShellExecute(0, nil, 'http://blog.qdac.cc', nil, nil, SW_SHOWNORMAL);
end;

end.
