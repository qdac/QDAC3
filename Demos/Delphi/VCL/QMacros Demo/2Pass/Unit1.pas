unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QString, QMacros, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoPass1(ASender: TQMacroManager; AName: QStringW;
      const AQuoter: QCharW; var AHandled: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AMacros: TQMacroManager;
begin
  AMacros := TQMacroManager.Create;
  try
    AMacros.OnMacroMissed := DoPass1;
    Memo1.Lines.Add(AMacros.Replace(Edit1.Text,'<%','%>'));
  finally
    FreeAndNil(AMacros);
  end;
end;

procedure TForm1.DoPass1(ASender: TQMacroManager; AName: QStringW;
  const AQuoter: QCharW; var AHandled: Boolean);
var
  AMacros: TQMacroManager;
begin
  AMacros := TQMacroManager.Create;
  try
    AMacros.Push('NickName', 'Doggle');
    ASender.Push(AName, AMacros.Replace(AName, '$', '$'));
    AHandled:=True;
  finally
    FreeAndNil(AMacros);
  end;
end;

end.
