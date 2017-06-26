unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, CommCtrl,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, qplugins, qplugins_params,qplugins_base,
  menusvc, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.StdCtrls;

type

  TQMenuService = class(TQService, IQMenuService)
  protected
    function RegisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
      ADelimitor: WideChar): IQMenuItem;
    procedure UnregisterMenu(const APath: PWideChar; AOnEvent: IQNotify;
      ADelimitor: WideChar);
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TQMenuItem = class(TInterfacedObject, IQMenuItem)
  private
  protected
    FMenuItem: TMenuItem;
    FOnClick: IQNotify;
    FName: String;
    FParams: IQParams;
    function GetCaption: PWideChar;
    procedure SetCaption(const S: PWideChar);
    function GetHint: PWideChar;
    procedure SetHint(const S: PWideChar);

    function SetImage(AHandle: HBITMAP): Boolean;

    function getParams: IQParams;
    procedure setParams(AParams: IQParams);

    function GetParentMenu: IQMenuItem;
    procedure DoClick(ASender: TObject);
  public
    constructor Create(AMenuItem: TMenuItem; AOnClick: IQNotify); overload;
    property Name: String read FName write FName;
    property Params: IQParams read getParams write setParams;
  end;

  TForm2 = class(TForm)
    MainMenu1: TMainMenu;
    ilMenus: TImageList;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses qstring;

var
  MainMenu: TMainMenu;
{$R *.dfm}
  { TMenuService }

constructor TQMenuService.Create;
begin
  inherited Create(IQMenuService, 'QMenuService');
end;

destructor TQMenuService.Destroy;
begin
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
      AIntf := IQMenuItem(Pointer(AMenu.Items[I].Tag));
      if (InstanceOf(AIntf) as TQMenuItem).Name = AName then
      begin
        Result := I;
        Break;
      end;
    end;
  end;

begin
  AMenu := Form2.MainMenu1.Items;
  p := PWideChar(APath);
  while p^ <> #0 do
  begin
    AName := DecodeTokenW(p, [ADelimitor], #0, true);
    if Length(AName) > 0 then
    begin
      AIdx := IndexOfMenuName;
      if AIdx = -1 then
      begin
        ANewMenu := TMenuItem.Create(MainMenu);
        if p^ = #0 then
          AChildMenu := TQMenuItem.Create(ANewMenu, AOnEvent)
        else
          AChildMenu := TQMenuItem.Create(ANewMenu, nil);
        AChildMenu.Name := AName;
        Result := AChildMenu;
        Result._AddRef;
        ANewMenu.Tag := IntPtr(Pointer(Result));
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
begin

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  MainMenu := MainMenu1;
  RegisterServices('/Services/Menus', [TQMenuService.Create(IQMenuService,
    'MenuService')]);
  PluginsManager.ById(IQMenuService);
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

procedure TQMenuItem.DoClick(ASender: TObject);
var
  AFireNext: Boolean;
begin
  AFireNext := true;
  if Assigned(FOnClick) then
    FOnClick.Notify(MN_CLICK, Params, AFireNext); // 演示版本，不传递参数
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

procedure TForm2.FormDestroy(Sender: TObject);
  procedure DestoryMenus(AParent: TMenuItem);
  var
    I: Integer;
    AMenu: TMenuItem;
  begin
    for I := 0 to AParent.Count - 1 do
    begin
      AMenu := AParent.Items[I];
      if AMenu.Tag <> 0 then
        IQMenuItem(Pointer(AMenu.Tag))._Release;
      DestoryMenus(AMenu);
    end;
  end;

begin
  DestoryMenus(MainMenu.Items);
end;

end.
