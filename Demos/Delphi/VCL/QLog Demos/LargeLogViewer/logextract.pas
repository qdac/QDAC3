unit logextract;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmLogExtractor = class(TForm)
    Label1: TLabel;
    edtToReplace: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtToSearch: TEdit;
    Button1: TButton;
    Button2: TButton;
    chkDistinct: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLogExtractor: TfrmLogExtractor;

implementation

{$R *.dfm}

end.
