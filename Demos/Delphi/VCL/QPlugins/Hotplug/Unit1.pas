unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  qplugins_base, qplugins;

type
  IEchoService = interface
    ['{E9A9E0D8-F278-46CC-9AA4-7FCD9CDAB77F}']
    function Echo(S: String): String;
  end;

  TEchoService = class(TQService, IEchoService)
    function Echo(S: String): String;
  end;

  TEchoReplacement = class(TInterfacedObject, IEchoService)
    function Echo(S: String): String;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FReplaced: IEchoService;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AService: IEchoService;
begin
  AService := PluginsManager as IEchoService;
  if Assigned(AService) then
    Memo1.Lines.Add(AService.Echo('Service hotplug demo'));
end;

{ TEchoService }

function TEchoService.Echo(S: String): String;
begin
  Result := ClassName + ':' + S;
end;

{ TEchoReplacement }

function TEchoReplacement.Echo(S: String): String;
begin
  Result := ClassName + ':' + S;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AService: IQService;
begin
  AService := FindService('/Services/Echo');
  if Assigned(AService) then
  begin
    if Assigned(FReplaced) then
    begin
      AService.RemoveExtension(FReplaced);
      FReplaced := nil;
      Button2.Caption := 'Ìæ»»·þÎñ';
    end
    else
    begin
      FReplaced := TEchoReplacement.Create;
      AService.AddExtension(FReplaced);
      Button2.Caption := '³·ÏúÌæ»»';
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterServices('/Services', [TEchoService.Create(IEchoService, 'Echo')]);
end;

end.
