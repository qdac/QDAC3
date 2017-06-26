unit Unit1;

interface

{
  本程序演示了如何使用QPlugins来做模块间的松散耦合，使用了Execute来传递，实际上
  也可以约定接口直接调用（请参考 MultiInstance 示例）
}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qplugins, qplugins_base,qplugins_params, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  ARoot: IQServices;
  I: Integer;
  ATabSheet: TTabSheet;
  AParams: IQParams;
begin
  ARoot := PluginsManager.ByPath('Services/Docks') as IQServices;
  if Assigned(ARoot) then
  begin
    AParams := TQParams.Create;
    AParams.Add('Parent', ptUInt64);
    for I := 0 to ARoot.Count - 1 do
    begin
      ATabSheet := TTabSheet.Create(PageControl1);
      ATabSheet.PageControl := PageControl1;
      ATabSheet.Caption := ARoot[I].Name;
      AParams[0].AsInt64 := IntPtr(ATabSheet);
      ARoot[I].Execute(AParams, nil);
    end;
  end;
end;

end.
