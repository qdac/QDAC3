unit directsvc;

interface
uses classes,sysutils,qpluginsintf,qplugins;
type
  IMyService=interface
    ['{C3FF72E3-B59A-49F3-AC67-FF79FBA23DE0}']
    function Sum(x,y:Integer):Integer;
    function Min(x,y:Integer):Integer;
    function Max(x,y:Integer):Integer;
  end;
  TMyService=class(TQPluginService,IMyService)
  protected
    function Sum(x,y:Integer):Integer;
    function Min(x,y:Integer):Integer;
    function Max(x,y:Integer):Integer;
    function Execute(AParams:IQParams):Boolean;
  public
    constructor Create;overload;
  end;
implementation
const
  ServiceName:PWideChar='MyService';
  ServiceId:TGuid='{433F8A79-0A15-45E7-92AC-D63814B3C1BF}';
{ TMyService }

constructor TMyService.Create;
begin
inherited Create;
end;

function TMyService.Execute(AParams: IQParams): Boolean;
var
  pAction,pX,pY:IQParam;
  Action:String;
begin
//这个Demo演示不是按顺序，而是按名称来取值数值
pAction:=AParams.ParamByName('Action');
pX:=AParams.ParamByName('X');
pY:=AParams.ParamByName('Y');
//if pAction.AsString then

end;

function TMyService.Max(x, y: Integer): Integer;
begin
if x<y then
  Result:=y
else
  Result:=x;
end;

function TMyService.Min(x, y: Integer): Integer;
begin
if x<y then
  Result:=x
else
  Result:=y;
end;

function TMyService.Sum(x, y: Integer): Integer;
begin
Result:=x+y;
end;


initialization
  PluginsManager.GetServices.Add(TMyService.Create(ServiceId,ServiceName));
end.
