unit vcl.form.customaction;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, vcl.Graphics,
  vcl.Controls, vcl.Forms, vcl.Dialogs, vcl.StdCtrls, qplugins,
  qplugins_params, qplugins_formsvc, qplugins_vcl_formsvc;

type
  TForm6 = class(TForm, IQCustomAction)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FClickMsg: String;
    function DoAction(AParams: IQParams; AResult: IQParams): Boolean;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
  ShowMessage(FClickMsg);
end;

function TForm6.DoAction(AParams, AResult: IQParams): Boolean;
var
  ACaption, AMsg, AName: IQParam;
  procedure DoSetMessage;
  begin
    ACaption := AParams.ByName('Caption');
    AMsg := AParams.ByName('Message');
    if Assigned(ACaption) then
      Button1.Caption := ParamAsString(ACaption);
    if Assigned(AMsg) then
      FClickMsg := ParamAsString(AMsg);
  end;
  procedure DoGetMessage;
  begin
    if Assigned(AResult) then
    begin
      AResult.Add('ClassName', ptUnicodeString).AsString.Value := 'TForm6';
      AResult.Add('Message', ptUnicodeString).AsString.Value := '来自插件的自定义消息返回值';
    end;
  end;

begin
  AName := AParams.ByName('@');
  if (not Assigned(AName)) or (ParamAsString(AName) = 'SetMessage') then
    DoSetMessage
  else if ParamAsString(AName) = 'GetMessage' then
    DoGetMessage;
  Result := True;
end;

initialization

RegisterFormService('/Services/Docks/Forms', 'CustomAction', TForm6, True);

finalization

UnregisterServices('/Services/Docks/Forms', ['CustomAction']);

end.
