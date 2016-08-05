unit Login;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Buttons, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, qprov_pgsql;

type
  TfrmPgLogin = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    edUserName: TEdit;
    edServer: TEdit;
    Label2: TLabel;
    edPassword: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edDatabase: TEdit;
    sbLogin: TSpeedButton;
    procedure sbLoginClick(Sender: TObject);
    procedure edServerKeyPress(Sender: TObject; var Key: Char);
  private
    FProvider: TQPgSQLProvider;
    procedure SetProvider(const Value: TQPgSQLProvider);
    { Private declarations }
  public
    { Public declarations }
    property Provider: TQPgSQLProvider read FProvider write SetProvider;
  end;

function PgLogin(AProvider: TQPgSQLProvider): Boolean;

implementation

{$R *.dfm}

uses qstring;

function PgLogin(AProvider: TQPgSQLProvider): Boolean;
var
  F: TfrmPgLogin;
begin
  F := TfrmPgLogin.Create(Application);
  F.Provider := AProvider;
  F.ShowModal;
  Result := F.ModalResult = mrOk;
  F.Free;
end;

procedure TfrmPgLogin.edServerKeyPress(Sender: TObject; var Key: Char);
var
  ACtrl: TWinControl;
begin
  if (Key = #13) and Assigned(ActiveControl) then
  begin
    if ActiveControl = edDatabase then
      sbLoginClick(Self)
    else
    begin
      ACtrl := FindNextControl(ActiveControl, True, True, True);
      if ACtrl.TabOrder > ActiveControl.TabOrder then
        ACtrl.SetFocus;
    end;
  end;
end;

procedure TfrmPgLogin.sbLoginClick(Sender: TObject);
var
  S: String;
  APort: Integer;
begin
  FProvider.Close;
  FProvider.ServerHost := NameOfW(edServer.Text, ':');
  S := ValueOfW(edServer.Text, ':');
  if not TryStrToInt(S, APort) then
    APort := 5432;
  FProvider.ServerPort := APort;
  FProvider.UserName := edUserName.Text;
  FProvider.Password := edPassword.Text;
  FProvider.Database := edDatabase.Text;
  if FProvider.Open then
    ModalResult := mrOk
  else
    ShowMessage(FProvider.LastErrorMsg);
end;

procedure TfrmPgLogin.SetProvider(const Value: TQPgSQLProvider);
begin
  if FProvider <> Value then
  begin
    FProvider := Value;
    if Length(Value.ServerHost) > 0 then
    begin
      edServer.Text := Value.ServerHost + ':' + IntToStr(Value.ServerPort);
      edUserName.Text := Value.UserName;
      edPassword.Text := Value.Password;
      edDatabase.Text := Value.Database;
    end;
  end;
end;

end.
