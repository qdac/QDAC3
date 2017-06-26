unit frmCertList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,
  qplugins, qplugins_base,qplugins_params, menusvc, StdCtrls, ExtCtrls;

type
  TForm3 = class(TForm)
    mmo1: TMemo;
    img1: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  Form3: TForm3;

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
  F: TForm3;
  I: Integer;
  AName: String;
begin
  if Assigned(AParams) and (ParamAsString(AParams.ByName('Name')) = 'Exit') then
    Application.Terminate
  else
  begin
    F := TForm3.Create(Application);
    with F.mmo1.Lines do
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
  AFormAction: IQNotify;

procedure DoMenuServiceReady(const AService: IQService); stdcall;
var
  F: TForm3;
begin
  with AService as IQMenuService do
  begin
    AFormAction := TShowFormAction.Create;
    with RegisterMenu('/File/ShowForm', AFormAction) do
    begin
      Caption := 'ÏÔÊ¾´°Ìå(&S)';
      //F := TForm3.Create(nil);
      //SetImage(TBitmap(F.img1.Picture.Graphic).Handle);
      Params := NewParams([1, 'Hello,world']);
      //F.Free;
    end;
    with RegisterMenu('/File/Exit', AFormAction) do
    begin
      Caption := 'ÍË³ö(&X)';
      Params := NewParams([]);
      Params.Add('Name', ptUnicodeString).AsString := NewString('Exit');
    end;
  end;
end;

initialization

AFormAction := nil;
PluginsManager.WaitService(IQMenuService, DoMenuServiceReady);

finalization

if Assigned(AFormAction) then
begin
  with PluginsManager as IQMenuService do
    UnregisterMenu('/File/ShowForm', AFormAction);
  AFormAction := nil;
end;

end.
