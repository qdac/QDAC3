unit formdropdown;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ComCtrls, ToolWin;
(*
(C)2013,swish,吉林省金港计算机网络有限公司
本单元用于实现下拉窗口的效果，用户程序继承TfrmDropDownBase窗口，然后调用DropDownForm
函数就可以在指定的控件位置上下拉窗口（根据位置会自动做适当调整）
2014.7.30
==========
  * 修正了切换到其它程序再切换回来时，由于Application.Active值仍为False造成无法正常
下拉显示的问题
2013.6.19
==========
  + 加入HIDE_WHEN_IDE选项，以决定在调试模式时，是否不关闭窗口
  + 加入HIDE_WHEN_DRAG选项，以决定在拖动宿主窗口是，是否跟着移动
  * 修正了在模态窗口下显示问题
  * 修正了响应WM_NCLBUTTONUP事件时忘检查指针是否为空的疸
  * 修正了忘记加入对关联控件的FreeNotification注册和移除的问题
  * 修正了AWait为True时，如果在下拉窗口操作时弹出其它窗口可能造成访问无效地址的问题
*)
{.$DEFINE HIDE_WHEN_IDE}
{.$DEFINE HIDE_WHEN_DRAG}
type

  TDropDownFormClass=class of TfrmDropDownBase;

  TfrmDropDownBase = class(TForm)
  private
    FDropDownParam: Integer;
    FFreeAfterClosed: Boolean;
    procedure SetLinkedControl(const Value: TControl);
    { Private declarations }
  protected
    FHookedForm:TForm;
    FLinkedControl:TControl;
    FOriginWndProc:TWndMethod;
    FCtrlOriginWndProc:TWndMethod;
    FNCPainting:Boolean;
    FInWait:Boolean;
    FActiveCheckNeeded:Boolean;
    procedure HookedWndProc(var AMsg:TMessage);virtual;
    procedure CtrlWndProc(var AMsg:TMessage);virtual;
    procedure AdjustPosition;virtual;
    procedure AddHook;
    procedure RemoveHook;
    procedure WndProc(var AMsg:TMessage);override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction);override;
    procedure DoDestroy;override;
    procedure DoShow;override;
    procedure CheckActive;
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
  public
    { Public declarations }
    procedure DropDown(ACtrl:TControl;AWait:Boolean;AParam:Integer);
    property LinkedControl:TControl read FLinkedControl write SetLinkedControl;//关联的控件
    property DropDownParam:Integer read FDropDownParam write FDropDownParam;//用户调用赋加的额外参数，具体如何处理子类处理
    property InWait:Boolean read FInWait;
    property FreeAfterClosed:Boolean read FFreeAfterClosed write FFreeAfterClosed;
  end;

var
  FormInDropDown:TfrmDropDownBase;//全局的当前激活的下拉窗口
{
在指定的控件位置显示下拉窗口
Parameters
  AFormClass : 下拉的窗口类型
  ACtrl : 在下拉窗口的位置
  AWait : 是否等待下拉窗口关闭
  AParam : 用户自定义的参数
  AfterCreate : 在窗口创建完成后触发的事件，以方便用户设置额外的控制
Remarks
  1.下拉的窗口由AFormClass指定的子类实例创建，默认情况下，窗体在关闭时会自动释放，
如果要不自动释放，则应重载OnClose事件设置。
  2.如果指定AWait为False，则应保证弹出的下拉窗口不会弹出其它窗口，否则其会自动关闭，
所以一般推荐设置为True
}
procedure DropDownForm(AFormClass:TDropDownFormClass;ACtrl:TControl;AWait:Boolean;AParam:Integer;AfterCreate:TNotifyEvent=nil);overload;
procedure DropDownForm(AForm:TfrmDropDownBase;ACtrl:TControl;AWait:Boolean;AParam:Integer;AFreeAfterClosed:Boolean);overload;
implementation
function IsDebuggerPresent:Boolean;stdcall;external kernel32;
{$R *.dfm}
function IsAppActive:Boolean;
  var
    AWnd:HWND;
    APId:Cardinal;
  begin
  AWnd:=GetForegroundWindow;
  if AWnd<>0 then
    begin
    GetWindowThreadProcessId(AWnd,APId);
    Result:=(APId=GetCurrentProcessId);
    end
  else
    Result:=False;
  end;
{ TfrmDropDownBase }
{设置消息挂钩}
procedure TfrmDropDownBase.AddHook;
begin
FHookedForm:=GetParentForm(FLinkedControl,True) as TForm;
if Assigned(FHookedForm) then
  begin
  FOriginWndProc:=FHookedForm.WindowProc;
  FHookedForm.WindowProc:=HookedWndProc;
  if FHookedForm.FormStyle=fsStayOnTop then
    FormStyle:=fsStayOnTop;
  FHookedForm.FreeNotification(Self);
  end;
FCtrlOriginWndProc:=FLinkedControl.WindowProc;
FLinkedControl.WindowProc:=CtrlWndProc;
FLinkedControl.FreeNotification(Self);
end;
{调整下拉框的位置}
procedure TfrmDropDownBase.AdjustPosition;
var
  APos:TPoint;
begin
if Assigned(LinkedControl) then
  begin
  APos:=LinkedControl.ClientOrigin;
  if APos.X<FHookedForm.Monitor.Left then
    APos.X:=FHookedForm.Monitor.Left;
  if APos.Y<FHookedForm.Monitor.Top then
    APos.Y:=FHookedForm.Monitor.Top;
  if APos.X+Width>FHookedForm.Monitor.Width then
    APos.X:=FHookedForm.Monitor.Width-Width;
  if APos.Y+LinkedControl.Height+Height>FHookedForm.Monitor.Height then
    begin
    if APos.Y-Height>0 then
      Dec(APos.Y,Height)
    else
      APos.Y:=FHookedForm.Monitor.Height-Height;
    end
  else
    Inc(APos.Y,LinkedControl.Height);
  SetWindowPos(Handle,0,APos.X,APos.Y,0,0,SWP_NOZORDER + SWP_NOACTIVATE+SWP_NOSIZE);
  end;
end;
{重载CreateParams来保证包含WS_BORDER属性}
procedure TfrmDropDownBase.CheckActive;
begin
if InWait then
  FActiveCheckNeeded:=True
else
  Close;
end;

procedure TfrmDropDownBase.CreateParams(var Params: TCreateParams);
begin
  inherited;
//BugFix : AlphaControls支持的窗体要求必需有WS_BORDER属性，而默认的bsNone没有，强制加入  
Params.Style:=(Params.Style or WS_BORDER) and (not WS_DLGFRAME);
end;
procedure TfrmDropDownBase.CtrlWndProc(var AMsg: TMessage);
begin
FCtrlOriginWndProc(AMsg);
if AMsg.Msg=CM_EXIT then
  CheckActive;
end;

{重载关闭操作}
procedure TfrmDropDownBase.DoClose(var Action: TCloseAction);
begin
if (not FInWait) and FreeAfterClosed then
  Action:=caFree
else
  Action:=caHide;
RemoveHook;
if FormInDropDown=Self then
  FormInDropDown:=nil;
inherited DoClose(Action);
end;
{重载释放操作,以应对直接Free，而没有调用Close的情况}
procedure TfrmDropDownBase.DoDestroy;
begin
RemoveHook;
if FormInDropDown=Self then
  FormInDropDown:=nil;
inherited;
end;
{重载显示操作，以在真正显示前调整显示位置}
procedure TfrmDropDownBase.DoShow;
begin
inherited;
AdjustPosition;
end;
procedure TfrmDropDownBase.DropDown(ACtrl: TControl; AWait: Boolean;
  AParam: Integer);
begin
DropDownForm(Self,ACtrl,AWait,AParam,FreeAfterClosed);
end;

{外挂的消息处理过程}
procedure TfrmDropDownBase.HookedWndProc(var AMsg: TMessage);
var
  AMethod:TWndMethod;
begin
AMethod:=HookedWndProc;
if AMsg.Msg=WM_NCPAINT then
  begin
  if not FNCPainting then
    begin
    FNCPainting:=True;
    FHookedForm.Perform(WM_NCACTIVATE,1,-1);//强制将标题的状态改为激活,然后再交给原来的过程绘制
    FOriginWndProc(AMsg);
    FNCPainting:=False;
    end;
  end
else if (AMsg.Msg=WM_MOVE) or (AMsg.Msg=WM_SIZE) then  //父窗体位置移动或者尺寸发生变更时，重新调整位置
  begin
  //未处理LinkedControl尺寸变更时的情况，如果需要，再挂钩控件的过程即可
  FOriginWndProc(AMsg);
  {$IFNDEF HIDE_WHEN_DRAG}
  PostMessage(Handle,WM_MOVE,1,0);
  {$ENDIF}
  end
else if (AMsg.Msg>=WM_MOUSEFIRST) and (AMsg.Msg<=WM_MOUSELAST) then//处理鼠标点击消息
  begin
  if (AMsg.Msg=WM_LBUTTONDOWN) or (AMsg.Msg=WM_RBUTTONDOWN) or (AMsg.Msg=WM_MBUTTONDOWN) then
    CheckActive;
  FOriginWndProc(AMsg);
  end
else if AMsg.Msg=WM_NCLBUTTONDOWN then//鼠标在标题栏按下时，设置窗体始终在前，避免被覆盖
  begin
  FormStyle:=fsStayOnTop;
  FOriginWndProc(AMsg);
  {$IFDEF HIDE_WHEN_DRAG}
  SetBounds(-32767,-32767,Width,Height);
  {$ENDIF}
  end
else if AMsg.Msg=WM_NCLBUTTONUP then
  begin
  FOriginWndProc(AMsg);
  if Assigned(FHookedForm) then
    FormStyle:=FHookedForm.FormStyle
  else if FormStyle=fsStayOnTop then
    FormStyle:=fsNormal;
  end
else if AMsg.Msg=WM_WINDOWPOSCHANGED then//拖动完成时，恢复原始状态
  begin
  FOriginWndProc(AMsg);
  {$IFDEF HIDE_WHEN_DRAG}
  CheckActive;
  {$ELSE}
  if (PWindowPos(AMsg.LParam).flags and SWP_NOMOVE)=0 then
    begin
    FormStyle:=FHookedForm.FormStyle;
    BringToFront;
    end;
  {$ENDIF}
  end
else if AMsg.Msg=WM_SYSCOMMAND then
  begin
  FOriginWndProc(AMsg);
  if (AMsg.WParam=SC_CLOSE) or (AMsg.WParam=SC_MINIMIZE) or (AMsg.WParam=SC_TASKLIST) then
    CheckActive;
  end
else
  FOriginWndProc(AMsg);
if (not InWait) and (not IsAppActive ) then
  PostMessage(Handle,WM_SYSCOMMAND,SC_CLOSE,0);
end;
//重载以便在宿主窗口或控件释放时，关闭自己
procedure TfrmDropDownBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
if Operation=opRemove then
  begin
  if (AComponent=FHookedForm) or (AComponent=FLinkedControl) then
    begin
    RemoveHook;
    Close;
    end;
  end;
end;

{移除挂钩}
procedure TfrmDropDownBase.RemoveHook;
begin
if Assigned(FHookedForm) then
  begin
  FHookedForm.WindowProc:=FOriginWndProc;
  FHookedForm.RemoveFreeNotification(Self);
  FHookedForm:=nil;
  end;
if Assigned(FLinkedControl) then
  begin
  FLinkedControl.RemoveFreeNotification(Self);
  FLinkedControl.WindowProc:=FCtrlOriginWndProc;
  end;
FLinkedControl:=nil;
end;
{设置关联的控件}
procedure TfrmDropDownBase.SetLinkedControl(const Value: TControl);
begin
if Value<>FLinkedControl then
  begin
  if Assigned(FLinkedControl) then
    RemoveHook;
  FLinkedControl := Value;
  if Assigned(Value) then
    AddHook;
  end;
end;
{重载的消息处理过程}
procedure TfrmDropDownBase.WndProc(var AMsg: TMessage);
  function MouseInLinkedControl:Boolean;
  var
    R:TRect;
  begin
  R.TopLeft:=LinkedControl.ClientOrigin;
  R.Right:=R.Left+LinkedControl.Width;
  R.Bottom:=R.Top+LinkedControl.Height;
  Result:=PtInRect(R,Mouse.CursorPos);
  end;
begin
if (AMsg.Msg=WM_MOVE)  then
  begin
  if AMsg.WParam<>0 then//默认WM_MOVE的WParam未使用，始终为０，此处不为０则意味着是自己发送的位置调整消息
    AdjustPosition
  else
    Inherited WndProc(AMsg);
  end
else if AMsg.Msg=WM_SIZE then //自己尺寸如何调整了，则要重新调整位置
  AdjustPosition
else
  inherited WndProc(AMsg);
if Assigned(FHookedForm) then
  begin
  if AMsg.Msg=WM_SYSCOMMAND then
    begin
    if (AMsg.WParam=SC_CLOSE) or (AMsg.WParam=SC_MINIMIZE) or (AMsg.WParam=SC_TASKLIST) then
      RemoveHook;
    end
  else if AMsg.Msg=WM_ACTIVATE then
    begin
    if (AMsg.WParam=WA_INACTIVE) then
      begin
      if (AMsg.LParam<>Longint(FHookedForm.Handle)) then
        CheckActive
      else if FHookedForm.ActiveControl<>FLinkedControl then
        begin
        if not MouseInLinkedControl then
          CheckActive;
        end;
      end;
    end;
  end;
end;
{外部接口函数}
procedure DropDownForm(AFormClass:TDropDownFormClass;ACtrl:TControl;AWait:Boolean;AParam:Integer;AfterCreate:TNotifyEvent);
var
  F:TfrmDropDownBase;
begin
if Assigned(FormInDropDown) then
  begin
  if (FormInDropDown.ClassType=AFormClass) and (FormInDropDown.LinkedControl=ACtrl) and (FormInDropDown.DropDownParam=AParam) then
    begin
    FormInDropDown.BringToFront;
    Exit;
    end;
  FormInDropDown.Close;
  end;
F:=AFormClass.Create(Application);
try
  if Assigned(AfterCreate) then
    AfterCreate(F);
  DropDownForm(F,ACtrl,AWait,AParam,true);
finally
  if AWait then
    F.Free;
end;
end;

procedure DropDownForm(AForm:TfrmDropDownBase;ACtrl:TControl;AWait:Boolean;AParam:Integer;AFreeAfterClosed:Boolean);
begin
AForm.Position:=poDefault;
AForm.DropDownParam:=AParam;
FormInDropDown:=AForm;
AForm.LinkedControl:=ACtrl;
AForm.FreeAfterClosed:=AFreeAfterClosed;
AForm.Show;
AForm.Update;
if AWait then
  begin
  AForm.FInWait:=True;
  try
    while WaitMessage do
      begin
      Application.ProcessMessages;
      //避免调试时自动关闭
      if Application.Terminated or ((not IsAppActive)
          {$IFNDEF HIDE_WHEN_IDE}
          and (not IsDebuggerPresent)
          {$ENDIF}
          )
          then
          AForm.Close
      else if AForm.FActiveCheckNeeded then
        begin
        if Screen.ActiveForm<>AForm then
          AForm.Close;
        end;
      if not (Assigned(FormInDropDown) and FormInDropDown.Visible) then
        Break;
      end;
  finally
    AForm.FInWait:=False;
  end;
  end;
end;
end.
