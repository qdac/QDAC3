unit menu_demo;

interface

uses classes, sysutils,dialogs, qstring, qplugins, qpluginsintf;
//本单元演示了如何利用 QPlugins 来建立松散的菜单，主程序负责建立菜单，子模块注册菜单
type
  TNotifyEventG=procedure (ASender:TObject);
  TMenuService = class(TQPluginService)
  protected
    FOnClick: TNotifyEventG;
    function Execute(const AParams:IQParams;AResult:IQParams):Boolean; override;
  public
    property OnClick: TNotifyEventG read FOnClick write FOnClick;
  end;

implementation

const
  QPID_MENU: TGuid = '{4906D287-4B10-47AC-8BD6-B7847DCB2C59}';

procedure RegisterMenu(const AId:TGuid;ANamePath: QStringW; AOnClick: TNotifyEventG);
var
  AParent: IQPluginServices;
  AService: IQPLuginService;
  AMenu: TMenuService;
  AName: QStringW;
const
  AMenuDelimeter:PWideChar='/';
begin
  AParent := PluginsManager.ByPath('Services/Menu') as IQPluginServices;
  if not Assigned(AParent) then
  begin
    AParent := TQPluginServices.Create(QPID_MENU, 'Menu');
    PluginsManager.Services.Add(AParent as IQPLuginService);
  end;
  repeat
    AName := DecodeTokenW(ANamePath, AMenuDelimeter, WideChar(#0), True, True);
    if Length(AName) > 0 then
    begin
      AService := AParent.ByPath(PWideChar(AName));
      if Assigned(AService) then
      begin
        if not Supports(AService, IQPluginServices, AParent) then
        begin
          if Length(ANamePath) > 0 then // 这应该是一个菜单目录
          begin
            AParent.Remove(AService); // Replace Service as Service list
            AService := TQPluginServices.Create(AService.Id, AService.Name);
            AParent.Add(AService);
            AParent := AService as IQPluginServices;
          end
        end;
      end
      else if Length(ANamePath)>0 then
        begin
        AService := TQPluginServices.Create(AName);
        AParent.Add(AService);
        AParent:=AService as TQPluginServices;
        end
      else
      begin
        AMenu:=TMenuService.Create(AId,AName);
        AMenu.OnClick:=AOnClick;
        AParent.Add(AMenu);
      end;
    end;
  until Length(ANamePath) = 0;
end;

{ TMenuService }

function TMenuService.Execute(const AParams:IQParams;AResult:IQParams):Boolean;
begin
  inherited;
  Result:=True;
  if Assigned(FOnClick) then
    begin
    if (AParams.Count>0) and (AParams[0].ParamType=ptPointer) then
      FOnClick(AParams[0].AsPointer)
    else
      FOnClick(Self);
    end;
end;

procedure DoOpenFile(ASender:TObject);
begin
ShowMessage('你点击了菜单 文件|打开');
end;

procedure DoSaveFile(ASender:TObject);
begin
ShowMessage('你点击了菜单 文件|保存');
end;

initialization

RegisterMenu(NewId,'文件(&F)/打开(&O)',@DoOpenFile);
RegisterMenu(NewId,'文件(&F)/保存(&S)',@DoSaveFile);

end.
