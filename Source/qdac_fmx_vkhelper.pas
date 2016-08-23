unit qdac_fmx_vkhelper;

{
  Force focused control visible when Android Virtual Keyboard showed or hiden
  How to use:
  place qdac_fmx_vkhelper into your project uses section.No more code needed.

  Changes
  =======

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

uses classes, sysutils, math, System.Types;

var
  EnableReturnKeyHook: Boolean;

function IsVKVisible: Boolean;
function GetVKBounds: TRectF; overload;

implementation

uses System.UITypes, System.Messaging,
  FMX.Types, FMX.Controls, System.Rtti,
  FMX.Layouts, FMX.text, FMX.scrollbox, FMX.VirtualKeyboard, FMX.Forms,
  FMX.Platform, typinfo
{$IFDEF ANDROID}, FMX.Platform.Android, FMX.Helpers.Android,
  FMX.VirtualKeyboard.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Embarcadero {$ENDIF}
{$IFDEF IOS}
    , Macapi.Helpers, FMX.Platform.iOS, FMX.VirtualKeyboard.iOS,
  iOSapi.Foundation, iOSapi.UIKit
{$ENDIF}
    ;

type
  PAdjustItem = ^TAdjustItem;

  TAdjustItem = record
    Prior: PAdjustItem;
    Control: TControl;
    LastMargin: TPointF;
    LastViewPos: TPointF;
    LastBounds: TRectF;
    LastAlign: TAlignLayout;
  end;

  TQAdjustStack = class
  protected
    FLast: PAdjustItem;
    function GetAdjusted: Boolean;
    procedure RemoveLast;
  public
    procedure Save(ACtrl: TControl);
    procedure Restore;
    procedure Remove(ACtrl: TComponent);
    property Adjusted: Boolean read GetAdjusted;
  end;

  TVKNextHelper = class(TFMXObject)
  protected
    FOriginEvent: TKeyEvent;
    procedure SetParent(const Value: TFMXObject); override;
    procedure DoFocusNext(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);

  end;

  TVKStateHandler = class(TComponent)
  protected
    FVKMsgId: Integer; // TVKStateChangeMessage 消息的订阅ID
    FSizeMsgId: Integer; // TSizeChangedMessage 消息的订阅ID
    FIdleMsgId: Integer;
    FLastControl: TControl;
    FAdjustStack: TQAdjustStack; // 最后一次调整的ScrollBox
{$IFDEF ANDROID}
    FVKState: PByte;
{$ENDIF}
    procedure DoVKVisibleChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure DoSizeChanged(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure DoAppIdle(const Sender: TObject;
      const Msg: System.Messaging.TMessage);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function AdjustByLayout(ARoot: TFMXObject; AVOffset: Single): Single;
    function AdjustByScrollBox(AScrollBox: TCustomScrollBox;
      AVOffset: Single): Single;
    function AdjustByPresentedScrollBox(AScrollBox: TCustomPresentedScrollBox;
      AVOffset: Single): Single;
    procedure AdjustCtrl(ACtrl: TControl; AVKBounds, ACtrlBounds: TRectF;
      AVKVisible: Boolean);
    function NeedAdjust(ACtrl: TControl; var ACaretRect: TRectF): Boolean;
    function IsAnimating(AScrollBox: TCustomScrollBox): Boolean;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
  end;

var
  VKHandler: TVKStateHandler;
{$IFDEF ANDROID}

function GetVKBounds(var ARect: TRect): Boolean; overload;
var
  ContentRect, TotalRect: JRect;
begin
  ContentRect := TJRect.Create;
  TotalRect := TJRect.Create;
  MainActivity.getWindow.getDecorView.getWindowVisibleDisplayFrame(ContentRect);
  MainActivity.getWindow.getDecorView.getDrawingRect(TotalRect);
  Result := TotalRect.Bottom <> ContentRect.Bottom;
  if Result then
  begin
    ARect.Left := TotalRect.Left;
    ARect.Top := ContentRect.Bottom;
    ARect.Right := TotalRect.Right;
    ARect.Bottom := TotalRect.Bottom;
  end;
end;

function GetVKBounds: TRectF; overload;
var
  ContentRect, TotalRect: JRect;
begin
  ContentRect := TJRect.Create;
  TotalRect := TJRect.Create;
  MainActivity.getWindow.getDecorView.getWindowVisibleDisplayFrame(ContentRect);
  MainActivity.getWindow.getDecorView.getDrawingRect(TotalRect);
  Result := TRectF.Create(ConvertPixelToPoint(TPointF.Create(TotalRect.Left,
    TotalRect.Top + ContentRect.height)),
    ConvertPixelToPoint(TPointF.Create(TotalRect.Right, TotalRect.Bottom)));
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
var
  R: TRect;
begin
{$IFDEF NEXTGEN}
  Result := GetVKBounds(R);
{$ELSE}
  Result := false;
{$ENDIF}
end;

{ TVKStateHandler }
/// Adjust by layout,return the real adjust offset
function TVKStateHandler.AdjustByLayout(ARoot: TFMXObject;
  AVOffset: Single): Single;
var
  ALayout: TControl;
  // 移动指定父上的子对象到新的父对象
  procedure BeginUpdate(AObj: TFMXObject);
  begin
    if AObj is TControl then
      (AObj as TControl).BeginUpdate
    else if AObj is TCustomForm then
      (AObj as TCustomForm).BeginUpdate;
  end;

  procedure EndUpdate(AObj: TFMXObject);
  begin
    if AObj is TControl then
      (AObj as TControl).EndUpdate
    else if AObj is TCustomForm then
      (AObj as TCustomForm).EndUpdate;
  end;

  procedure MoveCtrls(AOldParent, ANewParent: TFMXObject);
  var
    I: Integer;
    AChild: TFMXObject;
  begin
    I := 0;
    BeginUpdate(AOldParent);
    BeginUpdate(ANewParent);
    try
      while I < AOldParent.ChildrenCount do
      begin
        AChild := AOldParent.Children[I];
        if AChild <> ANewParent then
        begin
          if AChild.Parent = AOldParent then
          begin
            AChild.Parent := ANewParent;
            Continue;
          end;
        end;
        Inc(I);
      end;
    finally
      EndUpdate(AOldParent);
      EndUpdate(ANewParent);
    end;
  end;

  function RootLayout(ARoot: TFMXObject): TControl;
  var
    ACtrl: TFMXObject;
    I: Integer;
    ALastRootStyle: String;
  begin
    Result := nil;
    if (ARoot.ChildrenCount > 0) then
    begin
      for I := 0 to ARoot.ChildrenCount - 1 do
      begin
        if ACtrl is TLayout then
        begin
          Result := ACtrl as TLayout;
          if Result.TagObject <> Self then
            Result := nil;
        end;
      end;
    end;
    if Result = nil then
    begin
      if ARoot is TCustomForm then
      begin
        with ARoot as TCustomForm do
        begin
          ALastRootStyle := StyleLookup;
          StyleLookup := '';
        end;
      end
      else
        ALastRootStyle := '';
      Result := TLayout.Create(ARoot);
      Result.Parent := ARoot;
      Result.TagObject := Self;
      with ARoot as IContainerObject do
        Result.SetBounds(0, 0, ContainerWidth, ContainerHeight);
      MoveCtrls(ARoot, Result);
      // 修正窗体样式被错误删除造成的问题
      if Length(ALastRootStyle) > 0 then
        (ARoot as TCustomForm).StyleLookup := ALastRootStyle;
    end;
  end;

begin
  ALayout := RootLayout(ARoot); // 确认存在用于调整的根布局
  FAdjustStack.Save(ALayout);
  ALayout.Position.y := ALayout.Position.y + AVOffset;
  Result := AVOffset;
end;

function TVKStateHandler.AdjustByPresentedScrollBox
  (AScrollBox: TCustomPresentedScrollBox; AVOffset: Single): Single;
var
  ALastY: Single;
begin
  FAdjustStack.Save(AScrollBox);
  ALastY := AScrollBox.ViewportPosition.y;
  AScrollBox.ScrollBy(0, AVOffset);
  Result := ALastY - AScrollBox.ViewportPosition.y;
  if IsZero(Result) then
    FAdjustStack.RemoveLast;
end;

function TVKStateHandler.AdjustByScrollBox(AScrollBox: TCustomScrollBox;
  AVOffset: Single): Single;
var
  ALastY: Single;
begin
  FAdjustStack.Save(AScrollBox);
  ALastY := AScrollBox.ViewportPosition.y;
  AScrollBox.ScrollBy(0, AVOffset);
  Result := ALastY - AScrollBox.ViewportPosition.y;
end;

procedure TVKStateHandler.AdjustCtrl(ACtrl: TControl;
  AVKBounds, ACtrlBounds: TRectF; AVKVisible: Boolean);
var
  ACaretRect: TRectF;
  function TryByScrollBox(AParent: TFMXObject; var AOffset: Single): TFMXObject;
  begin
    Result := AParent.Parent;
    // 父有滚动框，则尝试滚动解决
    while Assigned(AParent) and (AOffset < 0) do
    begin
      if AParent is TCustomScrollBox then
      begin
        // 正在滚动时不需要调整
        if IsAnimating(AParent as TCustomScrollBox) then
          Exit;
        AOffset := AOffset - AdjustByScrollBox
          (AParent as TCustomScrollBox, AOffset)
      end
      else if AParent is TCustomPresentedScrollBox then
        AOffset := AOffset - AdjustByPresentedScrollBox
          (AParent as TCustomPresentedScrollBox, AOffset);
      Result := AParent;
      AParent := AParent.Parent;
    end;
  end;
/// 将指定的区域移动可视区
  procedure ScrollInToRect;
  var
    AParent: TFMXObject;
    AOffset: Single;
  begin
    AOffset := AVKBounds.Top - ACaretRect.Bottom;
    AParent := TryByScrollBox(ACtrl, AOffset);
    if AOffset < 0 then
      AdjustByLayout(AParent, AOffset);
  end;
  procedure AddNextHelper;
  var
    AHelper: TVKNextHelper;
    I: Integer;
    AVKCtrl: IVirtualKeyboardControl;
  begin
    if Supports(ACtrl, IVirtualKeyboardControl, AVKCtrl) then
    begin
      for I := 0 to ACtrl.ComponentCount - 1 do
      begin
        if ACtrl.Components[I] is TVKNextHelper then
          Exit;
      end;
      AHelper := TVKNextHelper.Create(ACtrl);
      AHelper.SetParent(ACtrl);
    end;
  end;

begin
  if AVKVisible then
  begin
    if NeedAdjust(ACtrl, ACaretRect) then
    begin
      if FLastControl <> ACtrl then
      begin
        if Assigned(FLastControl) then
          FLastControl.RemoveFreeNotification(Self);
        FLastControl := ACtrl;
        FLastControl.FreeNotification(Self);
      end;
      ScrollInToRect;
    end;
    if EnableReturnKeyHook then
      AddNextHelper;
  end
  else
  begin
    FAdjustStack.Restore;
    FLastControl := nil;
  end;
end;

// 构造函数，订阅消息
constructor TVKStateHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdjustStack := TQAdjustStack.Create;
  FVKMsgId := TMessageManager.DefaultManager.SubscribeToMessage
    (TVKStateChangeMessage, DoVKVisibleChanged);
  FSizeMsgId := TMessageManager.DefaultManager.SubscribeToMessage
    (TSizeChangedMessage, DoSizeChanged);
  FIdleMsgId := TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage,
    DoAppIdle);
end;

/// 析构函数，取消消息订阅
destructor TVKStateHandler.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVKMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, FSizeMsgId);
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, FIdleMsgId);
  FAdjustStack.Restore;
  inherited;
end;

/// 在应用空闲时，检查虚拟键盘是否隐藏或是否覆盖住了当前获得焦点的控件
procedure TVKStateHandler.DoAppIdle(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
{$IFDEF ANDROID}
  procedure FMXAndroidFix;
  var
    AService: IFMXVirtualKeyboardService;
    AVK: TVirtualKeyboardAndroid;
    AListener: TVKListener;
    AContext: TRttiContext;
    AType: TRttiType;
    AField: TRttiField;
  begin
    if TPlatformServices.Current.SupportsPlatformService
      (IFMXVirtualKeyboardService, AService) then
    begin
      AVK := AService as TVirtualKeyboardAndroid;
      AContext := TRttiContext.Create;
      AType := AContext.GetType(AVK.ClassType);
      if Assigned(AType) then
      begin
        AField := AType.GetField('FVKListener');
        if Assigned(AField) then
        begin
          AListener := AField.GetValue(AVK).AsObject as TVKListener;
          AListener.onVirtualKeyboardHidden;
        end;
      end;
      Screen.ActiveForm.Focused := nil;
    end;
  end;

  procedure UpdateAndroidKeyboardServiceState;
  var
    ASvc: IFMXVirtualKeyboardService;
    AContext: TRttiContext;
    AType: TRttiType;
    AField: TRttiField;
    AInst: TVirtualKeyboardAndroid;
  begin
    if not Assigned(FVKState) then
    begin
      if (not Assigned(Screen.FocusControl)) and
        TPlatformServices.Current.SupportsPlatformService
        (IFMXVirtualKeyboardService, ASvc) then
      begin
        AInst := ASvc as TVirtualKeyboardAndroid;
        AContext := TRttiContext.Create;
        AType := AContext.GetType(TVirtualKeyboardAndroid);
        AField := AType.GetField('FState');
        if AField.GetValue(AInst).AsOrdinal <> 0 then
        begin
          FVKState := PByte(AInst);
          Inc(FVKState, AField.Offset);
        end;
      end;
    end;
    if Assigned(FVKState) and (FVKState^ <> 0) then
      FVKState^ := 0;
  end;
{$ENDIF}
{$IFDEF IOS}
  procedure VKHide;
  var
    ASvc: IFMXVirtualKeyboardService;
  begin
    if TPlatformServices.Current.SupportsPlatformService
      (IFMXVirtualKeyboardService, ASvc) then
      ASvc.HideVirtualKeyboard;
  end;
{$ENDIF}
  procedure RestoreCtrls;
  begin
{$IFDEF ANDROID}
    UpdateAndroidKeyboardServiceState;
{$ENDIF}
    if Assigned(FLastControl) then
      AdjustCtrl(FLastControl, RectF(0, 0, 0, 0),
        FLastControl.AbsoluteRect, false);
  end;
  procedure CheckHidden;
  var
    ACaretRect: TRectF;
    ACtrl: TControl;
  begin
    if Assigned(Screen.FocusControl) then
    begin
      ACtrl := Screen.FocusControl.GetObject as TControl;
      if Assigned(ACtrl) then
      begin
        if NeedAdjust(ACtrl, ACaretRect) then
          AdjustCtrl(ACtrl, GetVKBounds, ACtrl.AbsoluteRect, True);
      end;
    end
    else
    begin
{$IFDEF IOS}
      VKHide;
{$ENDIF}
      RestoreCtrls;
    end;
  end;

begin
  if IsVKVisible then // 解决掉虚拟键盘隐藏后的问题
    CheckHidden
  else
    RestoreCtrls;
end;

/// 在横竖屏切换时，处理控件位置
procedure TVKStateHandler.DoSizeChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  ASizeMsg: TSizeChangedMessage absolute Msg;
  R: TRect;
begin
  if Sender = Screen.ActiveForm then
  begin
    FAdjustStack.Restore;
    if GetVKBounds(R) then
    begin
      TMessageManager.DefaultManager.SendMessage(Sender,
        TVKStateChangeMessage.Create(True, R));
    end
  end;
end;

/// 虚拟键盘可见性变更消息，调整或恢复控件位置
procedure TVKStateHandler.DoVKVisibleChanged(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  AVKMsg: TVKStateChangeMessage absolute Msg;
  ACtrl: TControl;
begin
  if AVKMsg.KeyboardVisible then // 键盘可见
  begin
{$IFDEF IOS}
    _IOS_VKBounds := TRectF.Create(AVKMsg.KeyboardBounds);
{$ENDIF}
    if Screen.FocusControl <> nil then
    begin
      ACtrl := Screen.FocusControl.GetObject as TControl;
      AdjustCtrl(ACtrl, GetVKBounds, ACtrl.AbsoluteRect, True);
    end;
  end
  else
  begin
{$IFDEF IOS}
    _IOS_VKBounds := TRectF.Empty;
{$ENDIF}
    FAdjustStack.Restore;
  end;
end;

function TVKStateHandler.IsAnimating(AScrollBox: TCustomScrollBox): Boolean;
var
  AService: IFMXVirtualKeyboardService;
  AContext: TRttiContext;
  AType: TRttiType;
  AField: TRttiField;
begin
  AContext := TRttiContext.Create;
  AType := AContext.GetType(AScrollBox.AniCalculations.ClassType);
  if Assigned(AType) then
  begin
    AField := AType.GetField('FStarted');
    if Assigned(AField) then
      Result := AField.GetValue(AScrollBox.AniCalculations).AsBoolean
    else
      Result := false;
  end
  else
    Result := false;
end;

/// 响应组件释放通知，以避免访问无效地址
function TVKStateHandler.NeedAdjust(ACtrl: TControl;
  var ACaretRect: TRectF): Boolean;
var
  ACaret: ICaret;
  ACaretObj: TCustomCaret;
  ACtrlBounds, AVKBounds: TRectF;
  function GetRootTop: Single;
  var
    ALayout: TLayout;
    AObj: TFMXObject;
  begin
    AObj := ACtrl.Root.GetObject;
    Result := 0;
    if AObj is TForm then
    begin
      if TForm(AObj).ChildrenCount > 0 then
      begin
        AObj := TForm(AObj).Children[0];
        if AObj is TLayout then
        begin
          ALayout := AObj as TLayout;
          Result := ALayout.Position.y;
        end;
      end;
    end;
  end;

begin
  if Supports(ACtrl, ICaret, ACaret) then
  begin
    AVKBounds := GetVKBounds;
    ACtrlBounds := ACtrl.AbsoluteRect;
    ACaretObj := ACaret.GetObject;
    ACaretRect.TopLeft := ACtrl.LocalToAbsolute(ACaretObj.Pos);
    ACaretRect.Right := ACaretRect.Left + ACaretObj.Size.cx + 1;
    ACaretRect.Bottom := ACaretRect.Top + ACaretObj.Size.cy+1; // 下面加点余量
    if ACaretRect.Bottom > ACtrlBounds.Bottom then
    begin
      if ACtrl is TCustomPresentedScrollBox then
      begin
        AdjustByPresentedScrollBox(ACtrl as TCustomPresentedScrollBox,
          ACtrlBounds.Bottom - ACaretRect.Bottom);
        Result := false;
        Exit;
      end;
    end;
    Result := ACaretRect.IntersectsWith(AVKBounds) or (ACaretRect.Top < 0) or
      (ACaretRect.Top > AVKBounds.Bottom);
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
      FLastControl := nil;
    end
    else
      FAdjustStack.Remove(AComponent);
  end;
  inherited;
end;

{ TQAdjustStack }

function TQAdjustStack.GetAdjusted: Boolean;
begin
  Result := Assigned(FLast);
end;

procedure TQAdjustStack.Remove(ACtrl: TComponent);
var
  ANext, ACurrent: PAdjustItem;
begin
  if Assigned(ACtrl) then
  begin
    ACurrent := FLast;
    ANext := nil;
    while Assigned(ACurrent) do
    begin
      if ACurrent.Control = ACtrl then
      begin
        if Assigned(ANext) then
          ANext.Prior := ACurrent.Prior
        else
          FLast := ACurrent.Prior;
        Dispose(ACurrent);
        break;
      end;
      ANext := ACurrent;
      ACurrent := ANext.Prior;
    end;
    ACtrl.RemoveFreeNotification(VKHandler);
  end;
end;

procedure TQAdjustStack.RemoveLast;
var
  APrior: PAdjustItem;
begin
 if Assigned(FLast) then
  begin
    APrior := FLast.Prior;
    Dispose(FLast);
    FLast := APrior;
  end;
end;

procedure TQAdjustStack.Restore;
var
  APrior: PAdjustItem;
begin
  while Assigned(FLast) do
  begin
    APrior := FLast.Prior;
    with FLast^ do
    begin
      if Control is TCustomScrollBox then
      begin
        Control.Margins.Bottom := LastMargin.y;
        Control.Margins.Left := LastMargin.x;
        (Control as TCustomScrollBox).ViewportPosition := LastViewPos;
      end
      else if Control is TCustomPresentedScrollBox then
      begin
        Control.Margins.Bottom := LastMargin.y;
        Control.Margins.Left := LastMargin.x;
        (Control as TCustomPresentedScrollBox).ViewportPosition := LastViewPos;
      end
      else
      begin
        if LastAlign = TAlignLayout.None then
          Control.Position.Point := LastMargin
        else
        begin
          Control.BoundsRect := LastBounds;
          Control.Align := LastAlign;
        end;
      end;
    end;
    Dispose(FLast);
    FLast := APrior;
  end;
end;

procedure TQAdjustStack.Save(ACtrl: TControl);
var
  AItem: PAdjustItem;
begin
  New(AItem);
  AItem.Prior := FLast;
  AItem.Control := ACtrl;
  if ACtrl is TCustomScrollBox then
  begin
    AItem.LastViewPos := (ACtrl as TCustomScrollBox).ViewportPosition;
    AItem.LastMargin.x := ACtrl.Margins.Left;
    AItem.LastMargin.y := ACtrl.Margins.Bottom;
  end
  else if ACtrl is TCustomPresentedScrollBox then
  begin
    AItem.LastViewPos := (ACtrl as TCustomPresentedScrollBox).ViewportPosition;
    AItem.LastMargin.x := ACtrl.Margins.Left;
    AItem.LastMargin.y := ACtrl.Margins.Bottom;
  end
  else
  begin
    AItem.LastMargin := ACtrl.Position.Point;
    AItem.LastBounds := ACtrl.BoundsRect;
    AItem.LastAlign := ACtrl.Align;
  end;
  FLast := AItem;
  ACtrl.FreeNotification(VKHandler);
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
        ANext := ATabList.FindNextTabStop(ACtrl, not(ssShift in Shift), True);
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
EnableReturnKeyHook := True;

finalization

{$IF DEFINED(ANDROID)  OR DEFINED(IOS)}
  VKHandler.DisposeOf;
VKHandler := nil;
{$ENDIF}

end.
