unit dockservice;

interface

uses classes, qstring, qplugins,qplugins_params,qplugins_base, controls;

type
  TDockService = class(TQService)
  private
    FControlClass: TControlClass;
  public
    function Execute(AParams: IQParams; AResult: IQParams): Boolean;
      override; stdcall;
    property ControlClass: TControlClass read FControlClass write FControlClass;
  end;

const
  IDockServices: TGuid = '{9DDD6DD9-3053-4EE2-90D5-759267DBB10C}';
procedure RegisterDock(AClass: TControlClass);

implementation

{ TDockService }

function TDockService.Execute(AParams, AResult: IQParams): Boolean;
var
  AParent: TWinControl;
  AControl: TControl;
begin
  AParent := Pointer(AParams[0].AsInt64);
  AControl := ControlClass.Create(AParent);
  AControl.HostDockSite := AParent;
  AControl.Visible := True;
  AControl.Align := alClient;
  Result := True;
end;

procedure RegisterDock(AClass: TControlClass);
var
  AParent: IQServices;
  AService: TDockService;
begin
  AParent := PluginsManager.ById(IDockServices) as IQServices;
  AService := TDockService.Create(NewId, AClass.ClassName);
  AService.ControlClass := AClass;
  AParent.Add(AService);
end;

procedure RegisterClass;
begin
  PluginsManager.Services.Add(TQServices.Create(IDockServices, 'Docks'));
end;

initialization

RegisterClass;

end.
