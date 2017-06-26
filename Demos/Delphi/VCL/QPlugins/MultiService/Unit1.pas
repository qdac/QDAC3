unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Rtti,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QString, QPlugins,
  qplugins_base,QPlugins_params,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TIntAddService = class(TQService)
  public
    function Execute(AParams: IQParams; AResult: IQParams): Boolean;
      override; stdcall;
  end;

  TFloatAddService = class(TQService)
  public
    function Execute(AParams: IQParams; AResult: IQParams): Boolean;
      override; stdcall;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function IsIntType(AType: TQParamType): Boolean;
begin
  Result := AType in [ptInt8, ptInt16, ptInt32, ptInt64, ptUInt8, ptUInt16,
    ptUInt32, ptUInt64];
end;

function IsFloatType(AType: TQParamType): Boolean;
begin
  Result := AType in [ptFloat4, ptFloat8];
end;
{ TFloatAddService }

function TFloatAddService.Execute(AParams, AResult: IQParams): Boolean;
begin
  if IsFloatType(AParams[0].ParamType) and IsFloatType(AParams[1].ParamType)
  then
  begin
    AResult.Add('Result', ptFloat8).AsFloat := AParams[0].AsFloat +
      AParams[1].AsFloat;
    Result := True;
  end
  else
    Result := False;
end;

{ TIntAddService }

function TIntAddService.Execute(AParams, AResult: IQParams): Boolean;
begin
  if IsIntType(AParams[0].ParamType) and IsIntType(AParams[1].ParamType) then
  begin
    AResult.Add('Result', ptInt64).AsInt64 := AParams[0].AsInt64 +
      AParams[1].AsInt64;
    Result := True;
  end
  else
    Result := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AServices: IQServices;
  AParams, AResult: IQParams;
  procedure Calc;
  var
    I: Integer;
  begin
    for I := 0 to AServices.Count - 1 do
    begin
      if AServices[I].Execute(AParams, AResult) then
      begin
        Memo1.Lines.Add('通过服务 ' + AServices[I].Name + ' 计算完成。');
        Break;
      end;
    end;
  end;

begin
  AParams := TQParams.Create;
  AResult := TQParams.Create;
  AServices := PluginsManager.ByPath('Services/Adds') as IQServices;
  AParams.Add('X', ptUInt8).AsInteger := 100;
  AParams.Add('Y', ptUInt8).AsInteger := 200;
  Calc;
  Memo1.Lines.Add('  计算表达式 100+200=' + IntToStr(AResult[0].AsInt64));
  AParams.Clear;
  AResult.Clear;
  AParams.Add('X', ptFloat8).AsFloat := 100.3;
  AParams.Add('Y', ptFloat8).AsFloat := 20.05;
  Calc;
  Memo1.Lines.Add('  计算表达式 100.3+20.05=' + FloatToStr(AResult[0].AsFloat));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  RegisterServices('/Services/Adds', [TIntAddService.Create(NewId, 'AddInt'),
    TFloatAddService.Create(NewId, 'AddFloat')]);
end;

end.
