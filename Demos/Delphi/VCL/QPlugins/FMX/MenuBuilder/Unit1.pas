unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, qstring,qplugins,qplugins_params;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
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

type
  ISumService = interface
    ['{7D635E80-4898-485F-A99F-DDF2D9BF7FB1}']
    function Add(X, Y: Integer): Integer;
  end;

  TSumService = class(TQService, ISumService)
    function Add(X, Y: Integer): Integer;
  end;
  { TQSumService }

function TSumService.Add(X, Y: Integer): Integer;
begin
  Result := X + Y;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AService: ISumService;
begin
  AService := PluginsManager.ByPath('Services/Sum') as ISumService;
  ShowMessage(IntToStr(AService.Add(100, 200)));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterServices('/Services', [TSumService.Create(NewId, 'Sum')]);
end;

end.
