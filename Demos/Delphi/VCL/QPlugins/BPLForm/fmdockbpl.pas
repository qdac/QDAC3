unit fmdockbpl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qstring, qplugins, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.AxCtrls, qplugins_base,QPlugins_Vcl_Messages;

type
  TForm3 = class(TForm)
    Button1: TButton;
  private
    procedure CreateParams(var Params: TCreateParams);
    { Private declarations }
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
  Form3: TForm3;

implementation

{$R *.dfm}

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
  FForm := TForm3.Create(Application.MainForm);
  FForm.BorderStyle := bsNone;
  FForm.ParentWindow := AHandle;
  FForm.DoubleBuffered := True;
  FForm.Show;
  HostSizeChanged;
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

procedure TForm3.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and (not(WS_CAPTION or WS_THICKFRAME));
  // 不要标题和边框，实际上就是 BorderStyle 为 bsNone 的效果
end;

initialization

// 注册 /Services/Docks/Frame 服务
RegisterServices('Services/Docks',
  [TDockInstanceService.Create(IDockableControl, 'bplFrame')]);

finalization

// 取消服务注册
UnregisterServices('Services/Docks', ['bplFrame']);

end.
