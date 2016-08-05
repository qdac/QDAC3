unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QString, QMacros;

type
  TForm1 = class(TForm)
    edtExpr: TEdit;
    Label1: TLabel;
    Button1: TButton;
    mmResults: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoRandValue(AMacro: TQMacroItem; const AQuoter: QCharW);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AMgr: TQMacroManager;
  ACompiled: TQMacroComplied;
  I: Integer;
begin
  AMgr := TQMacroManager.Create;
  mmResults.Lines.BeginUpdate;
  try
    AMgr.Push('Rand', DoRandValue);
    ACompiled := AMgr.Complie(edtExpr.Text, '%', '%', MRF_PARSE_PARAMS);
    for I := 0 to 9 do
      mmResults.Lines.Add(ACompiled.Replace);
  finally
    if Assigned(ACompiled) then
      FreeAndNil(ACompiled);
    FreeAndNil(AMgr);
    mmResults.Lines.EndUpdate;
  end;
end;

procedure TForm1.DoRandValue(AMacro: TQMacroItem; const AQuoter: QCharW);
var
  AMin: Integer;
begin
  // Rand(),Rand(Max),Rand(Min,Max)
  if Assigned(AMacro.Params) and (AMacro.Params.Count > 0) then
  begin
    if AMacro.Params.Count = 1 then
      AMacro.Value.Value := IntToStr(Random(AMacro.Params[0].AsInteger))
    else
      AMacro.Value.Value := IntToStr(AMacro.Params[0].AsInteger +
        Random(AMacro.Params[1].AsInteger - AMacro.Params[0].AsInteger));
  end
  else
    AMacro.Value.Value := FloatToStr(Random);
end;

end.
