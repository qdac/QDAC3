unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus,
  QPlugins,MenuSvc;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterServices('/Services/Menus', [TQMenuService.Create(MainMenu1)]);

end;

end.
