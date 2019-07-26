unit qdac_fmx_vkhelper;

{
  Force focused control visible when Android Virtual Keyboard showed or hiden
  How to use:
  place qdac_fmx_vkhelper into your project uses section.No more code needed.

  Changes
  =======
  2018.12.26
  ===========
  + Support for 10.3 only,good news for 10.3,memo support has some error,you can fix it by yourself
  2017.3.27
  =========
  + Add support for 10.2

  2016.9.26
  ========
  * Fixed:When a edit has focused and the virtual keyboard hidden,click back on android will exit directly
  2016.8.24
  ========
  * Fixed:When the scrollbox is scrolling,the adjust is too early make position not correct
  2016.7.29
  ========
  * Fixed:Access volation when you set control return action to Next and the control is the last one
  * Fixed:Segment fatal in special devices because scrollbox adjust render
  * Optimize the speed when add the adjust layout

  2016.7.19
  ========
  * Fixed:Adjust is overload in specail scene
  * Fixed:Form fill style is working correct

  2016.7.18
  ========
  * Fixed:When FLastControl is removed ,FLastControl not set to nil

  2016.7.7
  ========
  * Add Fix for system iOS virtualkeyboard error,detail see:http://blog.qdac.cc/?p=4003 (Chinese)

  2016.6.8
  ========
  * Add support for automic change focus control when you set edit ReturnKeyType to Next(You can disable it by set EnableReturnKeyHook to false

  2016.6.1
  =========
  * rewrite the adjust code for speed and other bug fix
  * rename to qdac_fmx_vkhelper
  2016.4.15
  + Add support for iOS

  2016.4.14
  + Add support for TMemo

  2016.1.16
  * Remove the timer for fast response(Use Idle message replace)
  * Fix a bug when a focus control reshow virtualkeyboard(Thanks 似水流年)
  * Fix a bug when use hide virtual keyboard by use hardware back key(Thanks 阿笨猫)
  * Fix a FMX bug :after virtual keyboard shown,can't use hardware back key to exit app
  2016.1.8
  * Fix a bug when user switch to other app
  2015.7.12
  * Fix space after hide ime and rotate
  * Fix rotate detection

}
interface

uses classes, sysutils, math, FMX.controls, FMX.Layouts,
  System.Types, System.Messaging;

type
  TControlHelper = class helper for TControl
    function OffsetOf(AParent: TControl): TPointF;
    function LocalToParent(AParent: TControl; APoint: TPointF)
      : TPointF; overload;
    function LocalToParent(AParent: TControl; R: TRectF): TRectF; overload;
  end;

  TScrollBoxHelper = class helper for TCustomScrollBox
  public
    procedure ScrollInView(ACtrl: TControl);
  end;

  TFocusChanged = class(TMessage)

  end;

var
  EnableReturnKeyHook: Boolean;

function IsVKVisible: Boolean;
function GetVKBounds: TRectF; overload;

implementation

uses System.UITypes,
  FMX.Types, System.Rtti,
  FMX.text, FMX.scrollbox, FMX.VirtualKeyboard, FMX.Forms,
  FMX.Platform, typinfo
{$IFDEF ANDROID}, FMX.Platform.Android, FMX.Helpers.Android, Androidapi.Helpers,
  FMX.VirtualKeyboard.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Embarcadero
{$IF RTLVersion>=32}
    , Androidapi.NativeActivity, Androidapi.AppGlue
{$ENDIF}
{$IF RTLVersion=33}
    , FMX.Platform.UI.Android
{$ENDIF}
{$ENDIF}
{$IFDEF IOS}
    , Macapi.Helpers, FMX.Platform.iOS, FMX.VirtualKeyboard.iOS,
  iOSapi.Foundation, iOSapi.UIKit
{$ENDIF}
    ;

type

  TVKNextHelper = class(TFMXObject)
  protected
    FOriginEvent: TKeyEvent;
    procedure SetParent(const Value: TFMXObject); override;
    procedure DoFocusNext(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);

  end;

  TVKStateHandler = class(TComponent)
  protected
    class var FContentRect: TRect;
  protected
    FVKMsgId: Integer; // TVKStateChangeMessage 消息的订阅ID
    FSizeMsgId: Integer; // TSizeChangedMessage 消息的订阅ID
    FIdleMsgId: Integer;
    FLastIdleTick: Cardinal;
    FLastControl: TControl;
    FLastControlForm: TCommonCustomForm;
    FLastRect: TRectF;
    [Weak] FLastFocused: IControl;
    FCaretTarget: TPointF;
    FAdjusting: Boolean;
    procedure DoVKVisibleChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure DoAppIdle(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure AdjustCtrl(ACtrl: TControl; AVKBounds: TRectF;
      AVKVisible: Boolean);
    function NeedAdjust(ACtrl: TControl; var ACaretRect: TRectF): Boolean;
    procedure AdjustIfNeeded;
    procedure Restore;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
  end;

  TAndroidContentChangeMessage = TMessage<TRect>;

var
  VKHandler: TVKStateHandler;
{$IFDEF ANDROID}
{$IF RTLVersion>=33}// 10.3+
  _AndroidVKBounds: TRectF;
{$ENDIF}

function JRectToRectF(R: JRect): TRectF;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right;
  Result.Bottom := R.Bottom;
end;

function GetVKPixelBounds: TRect;
var
  TotalRect: JRect;
  Content, Total: TRectF;
  ContentRect: JRect;
  AView: JView;
begin
  TotalRect := TJRect.Create;
  ContentRect := TJRect.Create;
  AView := TAndroidHelper.Activity.getWindow.getDecorView;
  AView.getDrawingRect(ContentRect);
  Content := JRectToRectF(ContentRect);
  TVKStateHandler.FContentRect := Content.Truncate;
  AView.getDrawingRect(TotalRect);
  Total := JRectToRectF(TotalRect);
  Result.Left := Trunc(Total.Left);
  Result.Top := Trunc(Total.Top + AView.getHeight);
  Result.Right := Trunc(Total.Right);
  Result.Bottom := Trunc(Total.Bottom);
end;

function GetVKBounds(var ARect: TRectF): Boolean; overload;
begin
{$IF RTLVersion>=33}// 10.3+
  if MainActivity.getVirtualKeyboard.isVirtualKeyboardShown then
  begin
    ARect := _AndroidVKBounds;
    Result := not ARect.IsEmpty;
  end
  else
  begin
    ARect := TRectF.Empty;
    Result := false;
  end;
{$ELSE}
  ARect := GetVKPixelBounds;
  Result := ARect.Bottom <> TVKStateHandler.FContentRect.Bottom;
  ARect := TRectF.Create(ConvertPixelToPoint(ARect.TopLeft),
    ConvertPixelToPoint(ARect.BottomRight)).Truncate;
{$ENDIF}
end;

function GetVKBounds: TRectF; overload;
var
  b: TRectF;
begin
  if not GetVKBounds(Result) then
    Result := TRectF.Empty;
end;

function GetVKBounds(var ARect: TRect): Boolean; overload;
var
  R: TRectF;
begin
  Result := GetVKBounds(R);
  ARect := R.Truncate;
end;

{$ELSE}
{$IFDEF IOS}
  _IOS_VKBounds: TRectF;

function GetVKBounds: TRectF; overload;
var
  ATop: Integer;
begin
  Result := _IOS_VKBounds;
  ATop := Screen.WorkAreaTop;
  Result.Top := Result.Top - ATop;
  Result.Bottom := Result.Bottom - ATop;
end;

function GetVKBounds(var ARect: TRect): Boolean; overload;
var
  ATemp: TRectF;
  AService: IFMXScreenService;
  AScale: Single;
begin
  ATemp := GetVKBounds;
  Result := not ATemp.IsEmpty;
  if Result then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
      AService) then
    begin
      AScale := AService.GetScreenScale;
      ARect.Left := Trunc(ATemp.Left * AScale);
      ARect.Top := Trunc(ATemp.Top * AScale);
      ARect.Right := Trunc(ATemp.Right * AScale);
      ARect.Bottom := Trunc(ATemp.Bottom * AScale);
    end;
  end;
end;
{$ELSE}

function GetVKBounds: TRectF; overload;
begin
  Result := TRectF.Empty;
end;

function GetVKBounds(var ARect: TRect): Boolean; overload;
begin
  Result := false;
end;
{$ENDIF}
{$ENDIF}

/// 根据MainActivity的可视区域和绘图区域大小来确定是否显示了虚拟键盘
function IsVKVisible: Boolean;
{$IFDEF NEXTGEN}var R: TRect; {$ENDIF}
begin
{$IFDEF NEXTGEN}
  Result := GetVKBounds(R);
{$ELSE}
  Result := false;
{$ENDIF}
end;
{ TControlHelper }

function TControlHelper.LocalToParent(AParent: TControl;
  APoint: TPointF): TPointF;
var
  AOffset: TPointF;
begin
  AOffset := OffsetOf(AParent);
  Result.X := APoint.X + AOffset.X;
  Result.Y := APoint.Y + AOffset.Y;
end;

function TControlHelper.LocalToParent(AParent: TControl; R: TRectF): TRectF;
var
  AOffset: TPointF;
begin
  AOffset := OffsetOf(AParent);
  Result := R;
  Result.Offset(AOffset.X, AOffset.Y);
end;

function TControlHelper.OffsetOf(AParent: TControl): TPointF;
var
  ACtrl: TControl;
begin
  ACtrl := Self;
  Result.X := 0;
  Result.Y := 0;
  while (ACtrl <> nil) and (ACtrl <> AParent) do
  begin
    Result.X := Result.X + ACtrl.Position.X;
    Result.Y := Result.Y + ACtrl.Position.Y;
    ACtrl := ACtrl.ParentControl;
  end;
  if not Assigned(ACtrl) then
    raise Exception.CreateFmt('指定的控件 %s 不是 %s 的子控件', [Name, AParent.Name]);
end;
{ TScrollBoxHelper }

procedure TScrollBoxHelper.ScrollInView(ACtrl: TControl);
var
  R, LR: TRectF;
  dx, dy: Single;
begin
  R := ACtrl.LocalToParent(Self, ACtrl.LocalRect);
  LR := LocalRect;
  if not LR.Contains(R) then
  begin
    if R.Left > LR.Right then
      dx := LR.Right - R.Right
    else if R.Right < R.Left then
      dx := R.Left
    else
      dx := 0;
    if R.Top > LR.Bottom then
      dy := LR.Bottom - R.Bottom
    else if R.Bottom < LR.Top then
      dy := R.Top
    else
      dy := 0;
    ScrollBy(dx, dy);
  end;
end;

{ TVKStateHandler }

procedure TVKStateHandler.AdjustCtrl(ACtrl: TControl; AVKBounds: TRectF;
  AVKVisible: Boolean);
var
  ACaretRect: TRectF;
  AForm: TCommonCustomForm;
  I: Integer;
  ADelta: Integer;
begin
  if AVKVisible and Assigned(ACtrl) then
  begin
    if FLastControl <> ACtrl then
    begin
      if Assigned(FLastControl) then
        FLastControl.RemoveFreeNotification(Self);
      FLastControl := ACtrl;
      FLastControl.FreeNotification(Self);
    end;
    AForm := (ACtrl.Root as TCommonCustomForm);
    if FLastControlForm <> AForm then
    begin
      if Assigned(FLastControlForm) then
        FLastControlForm.RemoveFreeNotification(Self);
      FLastControlForm := AForm;
      FLastControlForm.FreeNotification(Self);
      FLastRect := AForm.Padding.Rect;
    end;
    if NeedAdjust(ACtrl, ACaretRect) then
    begin
      if (ACaretRect.Bottom > AVKBounds.Top) or (AForm.Padding.Top < 0) or
        (ACaretRect.Top < 0) then
        ADelta := Trunc(ACaretRect.Bottom - AVKBounds.Top)
      else
        ADelta := 0;
      //移不动？
      if AForm.Padding.Bottom + ADelta < AVKBounds.Height then
        AForm.Padding.Rect := RectF(AForm.Padding.Left, AForm.Padding.Top - ADelta,
          AForm.Padding.Right, AForm.Padding.Bottom + ADelta);
    end;
  end
  else if Assigned(FLastControl) then
  begin
    Restore;
  end;
end;

procedure TVKStateHandler.AdjustIfNeeded;
var
  ACtrl: TControl;
begin
  if IsVKVisible then // 解决掉虚拟键盘隐藏后的问题
  begin
    ACtrl := Screen.FocusControl as TControl;
    AdjustCtrl(ACtrl, GetVKBounds, true);
  end
  else
    AdjustCtrl(nil, RectF(0, 0, 0, 0), false)
end;

// 构造函数，订阅消息
constructor TVKStateHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVKMsgId := TMessageManager.DefaultManager.SubscribeToMessage
    (TVKStateChangeMessage, DoVKVisibleChanged);
  FIdleMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage,
    DoAppIdle);
end;

/// 析构函数，取消消息订阅
destructor TVKStateHandler.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, FIdleMsgId);
  inherited;
end;

/// 在应用空闲时，检查虚拟键盘是否隐藏或是否覆盖住了当前获得焦点的控件
procedure TVKStateHandler.DoAppIdle(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
begin
  if FLastFocused <> Screen.FocusControl then
  begin
{$IFDEF VER320}   // Tokyo Only
    if Assigned(FLastFocused) then
      with (FLastFocused as TControl) do
        InvalidateRect(LocalRect);
{$ENDIF}
    TMessageManager.DefaultManager.SendMessage(Sender, TFocusChanged.Create);
    FLastFocused := Screen.FocusControl;
  end;
  if TThread.GetTickCount - FLastIdleTick > 100 then
  begin
    FLastIdleTick := TThread.GetTickCount;
    AdjustIfNeeded;
  end;
end;

/// 虚拟键盘可见性变更消息，调整或恢复控件位置
procedure TVKStateHandler.DoVKVisibleChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  AVKMsg: TVKStateChangeMessage absolute Msg;
  ACtrl: TControl;
begin
{$IFDEF IOS}
  _IOS_VKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
{$ENDIF}
{$IFDEF ANDROID}
{$IF RTLVersion>=33}// 10.3+
  _AndroidVKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
{$ENDIF}
{$ENDIF}
  if AVKMsg.KeyboardVisible then // 键盘可见
  begin
    if Screen.FocusControl <> nil then
    begin
      ACtrl := Screen.FocusControl.GetObject as TControl;
      AdjustCtrl(ACtrl, GetVKBounds, true);
    end;
  end
  else
    AdjustCtrl(nil, RectF(0, 0, 0, 0), false);
end;

/// 响应组件释放通知，以避免访问无效地址
function TVKStateHandler.NeedAdjust(ACtrl: TControl;
  var ACaretRect: TRectF): Boolean;
var
  ACaret: ICaret;
  AFlasher: IFlasher;
  ACtrlBounds, AVKBounds: TRectF;
  function ClientToParent(ARoot: TControl): TPointF;
  var
    AParent: TControl;
  begin
    AParent := ACtrl;
    Result := AFlasher.Pos;
    while AParent <> ARoot do
    begin
      if AParent is TCustomScrollBox then
        Result := Result - TCustomScrollBox(AParent).ViewportPosition
      else if AParent is TCustomPresentedScrollBox then
      begin
        Result := Result - TCustomPresentedScrollBox(AParent).ViewportPosition;
      end;
      Result := Result + AParent.Position.Point;
      AParent := AParent.ParentControl;
    end;
  end;
  function CaretVisible: Boolean;
  var
    pt: TPointF;
    AParent, AChild: TControl;
  begin
    pt := AFlasher.Pos;
    AChild := ACtrl;
    Result := AFlasher.Visible;
    while Assigned(AChild) and Result do
    begin
      if AChild is TCustomScrollBox then
      begin
        pt := pt - TCustomScrollBox(AChild).ViewportPosition;
        if not AChild.LocalRect.Contains(pt) then
          Result := false;
      end
      else if AChild is TCustomPresentedScrollBox then
      begin
        pt := pt - TCustomPresentedScrollBox(AChild).ViewportPosition;
        if not AChild.LocalRect.Contains(pt) then
          Result := false;
      end
      else if AChild.ClipChildren and not AChild.LocalRect.Contains(pt) then
        Result := false;
      pt := pt + AChild.Position.Point;
      AChild := AChild.ParentControl;
    end;
  end;

begin
  if Supports(ACtrl, ICaret, ACaret) then
  begin
    AVKBounds := GetVKBounds;
    ACtrlBounds := ACtrl.AbsoluteRect;
    AFlasher := ACaret.GetObject.Flasher;
    if CaretVisible then
    begin
      ACaretRect.TopLeft := ClientToParent(nil);
      // 加上标题栏的高度
      ACaretRect.TopLeft := ACaretRect.TopLeft +
        (ACtrl.Root as TCommonCustomForm).ClientToScreen(PointF(0, 0));
      if FAdjusting and (not SameValue(ACaretRect.Top, FCaretTarget.Y, 1.0))
      then
        Result := false;
      FAdjusting := false;
      ACaretRect.Right := ACaretRect.Left + AFlasher.Size.cx;
      ACaretRect.Bottom := ACaretRect.Top + AFlasher.Size.cy + 20; // 下面加点余量
      Result := ACaretRect.IntersectsWith(AVKBounds) or (ACaretRect.Top < 0) or
        (ACaretRect.Top > AVKBounds.Bottom);
      if Result then
      begin
        FCaretTarget.Y := ACaretRect.Top + ACaretRect.Bottom - AVKBounds.Top;
        FAdjusting := true;
      end;
    end
    else
      Result := false;
  end
  else
    Result := false;
end;

procedure TVKStateHandler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FLastControl then
    begin
      FLastControl.RemoveFreeNotification(Self);
      Restore;
    end else if AComponent = FLastControlForm then
    begin
      FLastControlForm.RemoveFreeNotification(Self);
      Restore;
    end
  end;
  inherited;
end;

procedure TVKStateHandler.Restore;
var
  AForm: TCommonCustomForm;
begin
  if Assigned(FLastControl) then
    AForm := (FLastControl.Root as TCommonCustomForm)
  else
    AForm := nil;
  if (not Assigned(AForm)) and Assigned(FLastControlForm) then
    AForm := FLastControlForm;
  if Assigned(AForm) and (AForm.Padding.Rect <> FLastRect) then
    AForm.Padding.Rect := FLastRect;

  FLastControl := nil;
  FLastControlForm := nil;
  FLastFocused := nil;
end;

{ TVKNextHelper }

procedure TVKNextHelper.DoFocusNext(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  AVKCtrl: IVirtualKeyboardControl;
  procedure FocusNext(ACtrl: TControl);
  var
    AParent: TControl;
    ANext: IControl;
    ATabList: ITabList;
  begin
    if Assigned(ACtrl) and Assigned(ACtrl.ParentControl) then
    begin
      AParent := ACtrl.ParentControl;
      ATabList := AParent.GetTabList;
      if Assigned(ATabList) then
      begin
        ANext := ATabList.FindNextTabStop(ACtrl, not(ssShift in Shift), true);
        if Assigned(ANext) then
          ANext.SetFocus
        else
          FocusNext(AParent);
      end;
    end;
  end;

begin
  if Assigned(FOriginEvent) then
    FOriginEvent(Sender, Key, KeyChar, Shift);
  if Supports(Sender, IVirtualKeyboardControl, AVKCtrl) then
  begin
    if (Key = vkReturn) and (AVKCtrl.ReturnKeyType = TReturnKeyType.Next) then
      FocusNext(Sender as TControl);
  end;
end;

procedure TVKNextHelper.SetParent(const Value: TFMXObject);
begin
  if Value <> Parent then
  begin
    inherited;
    with Parent as TControl do
    begin
      FOriginEvent := OnKeyDown;
      OnKeyDown := DoFocusNext;
    end;
  end;
end;

initialization

// 仅支持Android+IOS
{$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  VKHandler := TVKStateHandler.Create(nil);
{$ENDIF}
EnableReturnKeyHook := true;

finalization

{$IF DEFINED(ANDROID)  OR DEFINED(IOS)}
  VKHandler.DisposeOf;
VKHandler := nil;
{$ENDIF}

end.
