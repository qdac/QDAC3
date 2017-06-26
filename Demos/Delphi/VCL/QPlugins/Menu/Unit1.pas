unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qplugins, qplugins_params, qplugins_base,menusvc,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Image1: TImage;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TShowFormAction = class(TQInterfacedObject, IQNotify)
  protected
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean); stdcall;
  end;
  { TShowFormAction }

procedure TShowFormAction.Notify(const AId: Cardinal; AParams: IQParams;
  var AFireNext: Boolean);
var
  F: TForm1;
  I: Integer;
  AName: String;
begin
  if Assigned(AParams) and (ParamAsString(AParams.ByName('Name')) = 'Exit') then
    Application.Terminate
  else
  begin
    F := TForm1.Create(Application);
    with F.Memo1.Lines do
    begin
      BeginUpdate;
      try
        for I := 0 to AParams.Count - 1 do
        begin
          Add(IntToStr(I) + ': ' + AParams[I].Name + '=' +
            ParamAsString(AParams[I]));
        end;
      finally
        EndUpdate;
      end;
    end;
    F.ShowModal;
    F.Free;
  end;
end;

var
  AShowForm: IQNotify;

procedure DoMenuServiceReady(const AService: IQService); stdcall;
var
  F: TForm1;
begin
  with AService as IQMenuService do
  begin
    AShowForm := TShowFormAction.Create;
    with RegisterMenu('/File/ShowForm', AShowForm) do
    begin
      Caption := 'ÏÔÊ¾´°Ìå(&S)';
      F := TForm1.Create(nil);
      SetImage(TBitmap(F.Image1.Picture.Graphic).Handle);
      Params := NewParams([1, 'Hello,world']);
      F.Free;
    end;
    with RegisterMenu('/File/Exit', AShowForm) do
    begin
      Caption := 'ÍË³ö(&X)';
      Params := NewParams([]);
      Params.Add('Name', ptUnicodeString).AsString := NewString('Exit');
    end;
  end;
end;

initialization

AShowForm := nil;
PluginsManager.WaitService(IQMenuService, DoMenuServiceReady);

finalization

if Assigned(AShowForm) then
begin
  with PluginsManager as IQMenuService do
    UnregisterMenu('/File/ShowForm', AShowForm);
  AShowForm := nil;
end;

end.
