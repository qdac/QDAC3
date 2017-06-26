unit fmdock;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QString, QPlugins, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.AxCtrls,qplugins_base, qplugins_vcl_messages;

type

  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Label1: TLabel;
  private
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public

    { Public declarations }
  end;

  IDockableControl = interface
    ['{D0A4BDFA-CB29-4725-9158-C199B9C373C9}']
    procedure DockTo(AHandle: HWND); stdcall;
    procedure HostSizeChanged; stdcall;
  end;

  TDockableService = class(TQService, IDockableControl)
  protected
    FForm: TForm;
    procedure DockTo(AHandle: HWND); stdcall;
    procedure HostSizeChanged; stdcall;
  public
    constructor Create(const AId: TGuid; AName: QStringW); overload; override;
    destructor Destroy; override;
  end;

  TDockInstanceService = class(TQService)
  public
    function GetInstance: IQService; override; stdcall;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}
{ TDockableService }

constructor TDockableService.Create(const AId: TGuid; AName: QStringW);
begin
  inherited Create(AId, AName);
end;

destructor TDockableService.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

procedure TDockableService.DockTo(AHandle: HWND);
begin
  FForm := TForm2.Create(nil);
  FForm.BorderStyle := bsNone;
  FForm.ParentWindow := AHandle;
  FForm.DoubleBuffered := True;
  FForm.Show;
  HostSizeChanged;

  //
end;

procedure TDockableService.HostSizeChanged;
var
  R: TRect;
begin
  GetClientRect(Winapi.Windows.GetParent(FForm.Handle), R);
  FForm.SetBounds(0, 0, R.Width, R.Height);
end;

{ TDockInstanceService }

function TDockInstanceService.GetInstance: IQService;
begin
  Result := TDockableService.Create(NewId, 'DockableService');
end;

{ TForm2 }

procedure TForm2.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and (not(WS_CAPTION or WS_THICKFRAME));
  // 不要标题和边框，实际上就是 BorderStyle 为 bsNone 的效果
end;

initialization

// 注册 /Services/Docks/Frame 服务
RegisterServices('Services/Docks',
  [TDockInstanceService.Create(IDockableControl, 'Frame')]);

finalization

// 取消服务注册
UnregisterServices('Services/Docks', ['Frame']);

end.
