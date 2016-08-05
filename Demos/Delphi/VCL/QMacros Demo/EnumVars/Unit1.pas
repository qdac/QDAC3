unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, qstring, qmacros,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

{ 本示例演示了如何使用QMacros从一段HTML和PHP混合的代码中，解析出所有的PHP变量。
  This example demo parse php variant names from source code with php and html.
}
type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoCodeBlockFound(ASender: TQMacroManager; AName: QStringW;
      const AQuoter: QCharW; var AHandled: Boolean);
    procedure DoVarNameFound(ASender: TQMacroManager; AName: QStringW;
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
  AResult: TQMacroComplied;
begin
  AMacros := TQMacroManager.Create;
  try
    AMacros.OnMacroMissed := DoCodeBlockFound;
    //Complie is enough
    AResult := AMacros.Complie(Memo1.Text, '<?php', '?>');
    if Assigned(AResult) then //We don't use the result for replace,so free it.
      FreeAndNil(AResult);
  finally
    AMacros.Free;
  end;
end;

procedure TForm1.DoCodeBlockFound(ASender: TQMacroManager; AName: QStringW;
  const AQuoter: QCharW; var AHandled: Boolean);
var
  AVarParser: TQMacroManager;
  AReplace: TQMacroComplied;
begin
  AVarParser := TQMacroManager.Create;
  try
    AVarParser.OnMacroMissed := DoVarNameFound;
    AReplace := AVarParser.Complie(AName, '$', '', MRF_END_WITH_INVALID_CHAR);
    if AReplace <> nil then
    begin
      ASender.Push(AName, '');
      AHandled := True;
      FreeAndNil(AReplace);
    end;
  finally
    FreeAndNil(AVarParser);
  end;
end;

procedure TForm1.DoVarNameFound(ASender: TQMacroManager; AName: QStringW;
  const AQuoter: QCharW; var AHandled: Boolean);
begin
  Memo2.Lines.Add(AName);
  ASender.Push(AName, '');
  AHandled := True;
end;

end.
