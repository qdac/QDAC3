unit openurl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TfrmUrlInput = class(TForm)
    Label1: TLabel;
    edtUrl: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUrlInput: TfrmUrlInput;

implementation

{$R *.dfm}

end.
