unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QString,QMacros, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
var
  AMacros: TQMacroManager;
  AReplace:TQMacroComplied;
  I: Integer;
begin
  AMacros := TQMacroManager.Create;
  AMacros.Push('n',
    procedure(AMacro: TQMacroItem; const AQuoter: QCharW)
    begin
      AMacro.Value.Value := IntToStr(Random(10));
    end);
  Memo1.Lines.Add('替换 186[n][n][n][n]5678 十次的结果');
  AReplace:=AMacros.Complie('186[n][n][n][n]5678','[',']');
  for I := 0 to 9 do
    begin
    Memo1.Lines.Add(AReplace.Replace);
    end;
  FreeObject(AReplace);
  FreeObject(AMacros);
end;

end.
