unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,QMacros,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
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

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  AMacros:TQMacroManager;
  AComplied:TQMacroComplied;
begin
AMacros:=TQMacroManager.Create;
AMacros.Push('Year','2014');
AComplied:=AMacros.Complie('%Year% is a good year','%','%',0);
ShowMessage(AComplied.Replace);
AComplied.DisposeOf;
AMacros.DisposeOf;
end;

end.
