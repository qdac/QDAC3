unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,qxml,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TForm1 = class(TForm)
    GB2312: TButton;
    IdHTTP1: TIdHTTP;
    UTF8: TButton;
    procedure GB2312Click(Sender: TObject);
    procedure UTF8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.GB2312Click(Sender: TObject);
var
  AXML:TQXMLNode;
  AStream:TMemoryStream;
begin
AXML:=TQXMLNode.Create;
AStream:=TMemoryStream.Create;
try
  IdHttp1.Get('http://www.eryatang.com/question.xml',AStream);
  AStream.Position:=0;
  AXML.LoadFromStream(AStream);
  ShowMessage(AXML.AsXML);
finally
  FreeAndNil(AXML);
  AStream.Free;
end;
end;

procedure TForm1.UTF8Click(Sender: TObject);
var
  AXML:TQXMLNode;
  AStream:TMemoryStream;
begin
AXML:=TQXMLNode.Create;
AStream:=TMemoryStream.Create;
try
  IdHttp1.Get('http://www.eryatang.com/question_utf8.xml',AStream);
  AStream.Position:=0;
  AXML.LoadFromStream(AStream);
  ShowMessage(AXML.AsXML);
finally
  FreeAndNil(AXML);
  AStream.Free;
end;

end;

end.
