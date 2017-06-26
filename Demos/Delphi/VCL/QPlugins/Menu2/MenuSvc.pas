unit MenuSvc;

interface
uses
  windows,classes, SysUtils,Graphics,ImgList, menus,
  qstring, qplugins, qplugins_params,qplugins_base;



const

  MN_CLICK = 0;

type
  // 注册的菜单项Name属性会自动加上'mi'前缀，防止控件名称与保留关键字冲突
  // 这里只实现了菜单服务的部分接口，如果要实现更多的接口，请自己扩展实现
  IQMenuItem = interface
    ['{83323919-93DE-4D40-87FB-7266AE804D6C}']
    function GetCaption: PWideChar;
    procedure SetCaption(const S: PWideChar);
    function GetHint: PWideChar;
    procedure SetHint(const S: PWideChar);

    function getParams: IQParams;
    procedure setParams(AParams: IQParams);

    function SetImage(AHandle: HBITMAP): Boolean;

    function GetParentMenu: IQMenuItem;

    property Caption: PWideChar read GetCaption write SetCaption;
    property Hint: PWideChar read GetHint write SetHint;
    property ParentMenu: IQMenuItem read GetParentMenu;
    property Params: IQParams read getParams write setParams;
  end;


  IQMenuService = interface
    ['{667BD198-2F9A-445C-8A7D-B85C4B222DFC}']
    function RegisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
      ADelimitor: WideChar = '/'): IQMenuItem;
    procedure UnregisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
      ADelimitor: WideChar = '/');
  end;


 TQMenuService = class(TQService, IQMenuService)
  private
    FMainMenu:TMainMenu;
    FQMenuItems:TList;
  protected
    function RegisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
      ADelimitor: WideChar): IQMenuItem;
    procedure UnregisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
      ADelimitor: WideChar);
  public
    constructor Create(aMainMenu:TMainMenu);
    destructor Destroy; override;


  end;

  TQMenuItem = class(TQInterfacedObject, IQMenuItem)
  private
  protected
    FMenuItem: TMenuItem;
    FOnClick: IQNotify;
    FName: String;
    FParams: IQParams;
    function  GetCaption: PWideChar;
    procedure SetCaption(const S: PWideChar);
    function  GetHint: PWideChar;
    procedure SetHint(const S: PWideChar);

    function SetImage(AHandle: HBITMAP): Boolean;

    function getParams: IQParams;
    procedure setParams(AParams: IQParams);

    function GetParentMenu: IQMenuItem;
    procedure DoClick(ASender: TObject);
  public
    constructor Create(AMenuItem: TMenuItem; AOnClick: IQNotify); overload;
    destructor destroy; override;
    property Name: String read FName write FName;
    property Params: IQParams read getParams write setParams;
  end;

implementation

{ TQMenuService }
const MENUITEMNAME_PREFIX  = 'mi';


constructor TQMenuService.Create(aMainMenu:TMainMenu);
begin
  inherited Create(IQMenuService, 'QMenuService');
  FMainMenu :=   aMainMenu;
  FQMenuItems:= TList.Create ;

end;

destructor TQMenuService.Destroy;
var
  i:Integer;
  aIdx:Integer;
  aMenu:TMenuItem;
//  procedure  RemoveAQMenuItem(aMenuItem:TMenuItem);
//  var
//    k:integer;
//  begin
//     if aMenuItem.Count = 0 then
//     begin
//       //删除自己
//       //判断是否是注册的菜单项  , 暂时用Tag 是否大于0 作为识别标志
//       //但是这样就无法利用菜单项的Tag属性用于其他用途,需要优化.
//       if aMenuItem.Tag > 0  then
//       begin
//          IQMenuItem(Pointer(aMenuItem.Tag)).Params._Release ;
//          IQMenuItem(Pointer(aMenuItem.Tag))._Release;
//       end;
//
//       aMenuItem.Free;
//     end
//     else
//       for k := aMenuItem.Count -1 downto 0  do
//         RemoveAQMenuItem(aMenuItem[k]);
//
//  end;
begin
   //清理所有未注销的菜单对象
//   aMenu := FMainMenu.Items;
//   for i := aMenu.Count -1  downto 0 do
//      RemoveAQMenuItem(aMenu[i]);

   //清除所有注册的菜单
   //FQMenuItems
   for I := FQMenuItems.Count -1 downto 0  do
   begin
     TQMenuItem(FQMenuItems[I]).Free;
   end;
   FQMenuItems.Free;

  inherited;

end;

function TQMenuService.RegisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
  ADelimitor: WideChar): IQMenuItem;
var
  p: PWideChar;
  AName: QStringW;
  AMenu, ANewMenu: TMenuItem;
  AItem: IQMenuItem;
  AChildMenu: TQMenuItem;
  AIdx: Integer;
  function IndexOfMenuName: Integer;
  var
    I: Integer;
    AIntf: IQMenuItem;
  begin
    Result := -1;
    for I := 0 to AMenu.Count - 1 do
    begin

        if SameText(AMenu.Items[I].Name , MENUITEMNAME_PREFIX + AName ) then
        begin
          Result := I;
          Break;
        end;
    end;
  end;

begin
  AMenu := FMainMenu.Items ;
  p := PWideChar(APath);
  while p^ <> #0 do
  begin
    AName := DecodeTokenW(p, [ADelimitor], #0, true);
    if Length(AName) > 0 then
    begin
      AIdx := IndexOfMenuName;
      if AIdx = -1 then
      begin
        ANewMenu := TMenuItem.Create(FMainMenu);

        //TQMenuItem
        if p^ = #0 then
          AChildMenu := TQMenuItem.Create(ANewMenu, AOnEvent)
        else
          AChildMenu := TQMenuItem.Create(ANewMenu, nil);

        FQMenuItems.Add(AChildMenu);

        //AChildMenu.Name:= MENUITEMNAME_PREFIX + AName;     //添加命名前缀'mi_'，避免保留字冲突
        Result := AChildMenu;
        Result._AddRef;

        //TMenuItem
        ANewMenu.Name := MENUITEMNAME_PREFIX + AName;
        ANewMenu.Tag := IntPtr(Pointer(AChildMenu));
        ANewMenu.Caption := AName;
        AMenu.Add(ANewMenu);
        AMenu := ANewMenu;
      end
      else
      begin
        Result := IQMenuItem(Pointer(AMenu.Items[AIdx].Tag));
        AMenu := AMenu.Items[AIdx];

      end;
    end;
  end;
end;

procedure TQMenuService.UnregisterMenu(const APath: PWideChar;
  AOnEvent: IQNotify; ADelimitor: WideChar);

   //找到菜单项并删除之
   //逐级查找从叶到枝
var
  MenuItemIndexs:TList;
  k:Integer;
  p: PWideChar;
  AName: QStringW;
  AMenu: TMenuItem;
  AQMenuItem: TQMenuItem;
  AIdx: Integer;
  I: Integer;
  function IndexOfMenuName: Integer;
  var
    I: Integer;
    AIntf: IQMenuItem;
  begin
    Result := -1;
    for I := 0 to AMenu.Count - 1 do
    begin
        if SameText(AMenu.Items[I].Name , MENUITEMNAME_PREFIX + AName ) then
        begin
          Result := I;
          Break;
        end;


    end;
  end;

begin
  AMenu := FMainMenu.Items ;
  for k := FMainMenu.Items.Count -1 downto 0 do
    Debugout(FMainMenu.Items[k].Caption );


  MenuItemIndexs:=TList.Create;
  try
    p := PWideChar(APath);
    while p^ <> #0 do
    begin
      AName := DecodeTokenW(p, [ADelimitor], #0, true);
      if Length(AName) > 0 then
      begin
        AIdx := IndexOfMenuName;
        if AIdx = -1 then
        begin
            Break;
        end
        else
        begin

            MenuItemIndexs.Add(Pointer(AMenu.Items[AIdx])) ;
            AMenu := AMenu.Items[AIdx];

        end;
      end;
    end;

   //开始倒序删除 MenuItemIndexs 中的菜单项
    for k := MenuItemIndexs.Count -1 downto 0 do
    begin

       if TMenuItem(MenuItemIndexs[k]).Count = 0 then
       begin
         if TMenuItem(MenuItemIndexs[k]).Tag > 0 then
         begin
            AQMenuItem := TQMenuItem(Pointer(TMenuItem(MenuItemIndexs[k]).Tag));
            //清除内部列表中对象的引用
            for I := 0 to FQMenuItems.Count -1 do
            begin
               if FQMenuItems[i] = AQMenuItem  then
               begin
                 FQMenuItems[i]:=nil;
                 FQMenuItems.Delete(i);
                 break;
               end;
            end;
            FreeAndNil(AQMenuItem);

            TMenuItem(MenuItemIndexs[k]).Free;
           //MenuItemIndexs.Delete(k);
         end;

       end ;
    end;
     AOnEvent:= nil;
  finally
     MenuItemIndexs.Free;
  end;

end;


{ TQMenuItem }

constructor TQMenuItem.Create(AMenuItem: TMenuItem; AOnClick: IQNotify);
var
  ATemp: Pointer;
begin
  inherited Create;
  FMenuItem := AMenuItem;
  FMenuItem.OnClick := DoClick;
  FOnClick := AOnClick;
end;

destructor TQMenuItem.destroy;
begin

  FOnClick := nil;
  //FMenuItem.Free;
  inherited;
end;

procedure TQMenuItem.DoClick(ASender: TObject);
var
  AFireNext: Boolean;
begin
  AFireNext := true;
  if Assigned(FOnClick) then
    FOnClick.Notify(MN_CLICK, Params, AFireNext);
end;

function TQMenuItem.GetCaption: PWideChar;
begin
  Result := PWideChar(FMenuItem.Caption);
end;

function TQMenuItem.GetHint: PWideChar;
begin
  Result := PWideChar(FMenuItem.Hint);
end;

function TQMenuItem.getParams: IQParams;
begin
  Result := FParams;
end;

function TQMenuItem.GetParentMenu: IQMenuItem;
begin
  if Assigned(FMenuItem.Parent) then
    Result := IQMenuItem(FMenuItem.Parent.Tag)
  else
    Result := nil;
end;

procedure TQMenuItem.SetCaption(const S: PWideChar);
begin
  FMenuItem.Caption := S;
end;

procedure TQMenuItem.SetHint(const S: PWideChar);
begin
  FMenuItem.Hint := S;
end;

function TQMenuItem.SetImage(AHandle: HBITMAP): Boolean;
var
  ABitmap: TBitmap;
  AIcon: TBitmap;
  AImages: TCustomImageList;
begin
  AImages := (FMenuItem.Owner as TMenu).Images;
  AIcon := nil;
  ABitmap := TBitmap.Create;
  try
    ABitmap.Handle := AHandle;
    // 图标尺寸如果不对，则生成临时的位图，否则ImageList会添加失败
    if (ABitmap.Width <> AImages.Width) or (ABitmap.Height <> AImages.Height)
    then
    begin
      AIcon := TBitmap.Create;
      AIcon.SetSize(AImages.Width, AImages.Height);
      AIcon.Canvas.Brush.Color := ABitmap.TransparentColor;
      AIcon.Canvas.FillRect(Rect(0, 0, AImages.Width, AImages.Height));
      AIcon.Canvas.Draw((AImages.Width - ABitmap.Width) shr 1,
        (AImages.Height - ABitmap.Height) shr 1, ABitmap);
      AIcon.Transparent := true;
      FMenuItem.ImageIndex := AImages.AddMasked(AIcon,
        ABitmap.TransparentColor);
    end
    else
      FMenuItem.ImageIndex := AImages.AddMasked(ABitmap,
        ABitmap.TransparentColor);
  finally
    FreeAndNil(AIcon);
    FreeAndNil(ABitmap);
  end;
  Result := FMenuItem.ImageIndex <> -1;
end;

procedure TQMenuItem.setParams(AParams: IQParams);
begin
  FParams := AParams;
end;


end.
