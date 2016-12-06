unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit, StdCtrls, ExtCtrls, ComCtrls, QJson;

type
  TFrame_Compiler = class(TFrame)
    L: TLabel;
    PC: TPageControl;
    TabInstall: TTabSheet;
    RootDir: TLabeledEdit;
    App: TLabeledEdit;
    BrowsingPath: TLabeledEdit;
    TabEnv: TTabSheet;
    Env: TValueListEditor;
  private
    { Private declarations }
  public
    procedure Init(QJson: TQJson);
  end;

implementation

{$R *.dfm}

procedure TFrame_Compiler.Init(QJson: TQJson);
begin
  L.Caption := QJson.ForcePath('Name').AsString;
  RootDir.Text := QJson.ForcePath('RootDir').AsString;
  App.Text := QJson.ForcePath('App').AsString;
  BrowsingPath.Text := QJson.ForcePath('BrowsingPath').AsString;
  Env.InsertRow('BDS', QJson.ForcePath('RootDir').AsString, True);
  Env.InsertRow('BDSINCLUDE', QJson.ForcePath('RootDir').AsString + '\Include',
    True);
  if QJson.ForcePath('CommonDir').AsString <> '' then
    Env.InsertRow('BDSCOMMONDIR', QJson.ForcePath('CommonDir').AsString, True);
  if QJson.ForcePath('BoostDir').AsString <> '' then
    Env.InsertRow('CG_BOOST_ROOT', QJson.ForcePath('BoostDir').AsString, True);
  if QJson.ForcePath('BoostDir64').AsString <> '' then
    Env.InsertRow('CG_64_BOOST_ROOT', QJson.ForcePath('BoostDir64').AsString,
      True);
end;

end.
