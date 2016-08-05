unit Unit1;

interface
{本示例演示了 QWorker 和 QAndroidShell 两个单元的一部分用法：
1.使用 Workers.Post 的定时作业和主线程异步执行能力
2.使用 QAndroidShell.AskForRoot 获取管理员权限并执行关机和重启操作
[已知问题]
* 无法响应遥控器上的确认键，没有触发 KeyDown事件
}
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, QString, QWorker,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  QAndroid.Shell, Androidapi.JNI.App, FMX.Notification.Android,
  Androidapi.Helpers, Androidapi.JNIBridge,FMX.Platform,
  FMX.Effects, FMX.Notification;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    btnReboot: TButton;
    btnShutdown: TButton;
    Label1: TLabel;
    Popup1: TPopup;
    btnCancel: TButton;
    GlowEffect1: TGlowEffect;
    StyleBook1: TStyleBook;
    procedure btnRebootClick(Sender: TObject);
    procedure btnShutdownClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    FRootShell: TQAndroidShell;
    FShutdownTime: Int64;
    FTimerHandle: IntPtr;
    procedure DoRootDeny(AJob: PQJob);
    procedure DoShutdownCounter(AJob: PQJob);
  public
    { Public declarations }
    property RootShell: TQAndroidShell read FRootShell;
    function RootNeeded: Boolean;
    procedure PendShutdown(AMinutes: Integer);
  end;

var
  Form1: TForm1;
procedure SendAppToBack;

const
  vkReply = $E8; { 232 }

implementation

{$R *.fmx}

uses unit2, unit3, dateutils, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes;

// 通过激活桌面将应用切换到后台
procedure SendAppToBack;
var
  intent: JIntent;
begin
intent := TJIntent.Create;
intent.setAction(TJIntent.JavaClass.ACTION_MAIN);
intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
intent.addCategory(TJIntent.JavaClass.CATEGORY_HOME);
SharedActivityContext.startActivity(intent);
end;
// 共享的 ActivityManager 实例
function SharedActivityManager: JActivityManager;
var
  AService: JObject;
begin
AService := SharedActivityContext.getSystemService
  (TJContext.JavaClass.ACTIVITY_SERVICE);
Result := TJActivityManager.Wrap((AService as ILocalObject).GetObjectID);
end;
// 将当前应用推送到前台，需要 reorder tasks 权限，将moveTaskToBack就可以反过来推送到后台
procedure BringAppToFront;
begin
SharedActivityManager.moveTaskToFront(SharedActivity.getTaskId,
  TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
end;
// 判断当前应用程序是否是前台呈现给用户，如果不是，可以调用BringAppToFront来实现推送到前台
function IsAppActive: Boolean;
var
  AList: JList;
  AProcess: JActivityManager_RunningAppProcessInfo;
  AName: JString;
  AIterator: JIterator;
begin
AList := SharedActivityManager.getRunningAppProcesses;
AName := SharedActivityContext.getPackageName;
Result := False;
if Assigned(AList) then
  begin
  AIterator := AList.iterator;
  while AIterator.hasNext do
    begin
    AProcess := TJActivityManager_RunningAppProcessInfo.Wrap
      ((AIterator.next as ILocalObject).GetObjectID);
    if AProcess.processName.equals(AName) then
      begin
      if AProcess.importance = TJActivityManager_RunningAppProcessInfo.
        JavaClass.IMPORTANCE_FOREGROUND then
        begin
        Result := True;
        Break;
        end;
      end;
    end;
  end;
end;
// 重启
procedure TForm1.btnRebootClick(Sender: TObject);
begin
if RootNeeded then
  FRootShell.Execute('reboot');
end;
// 关机
procedure TForm1.btnShutdownClick(Sender: TObject);
begin
if not Assigned(Form2) then
  Form2 := TForm2.Create(Application);
Form2.Show;
end;
// 获取 Root 权限失败时弹出的错误提示
procedure TForm1.DoRootDeny(AJob: PQJob);
begin
Form3 := TForm3.Create(Application);
Form3.Show;
end;
// 关机倒计时，大于1分钟显示为分钟，小于1分钟显示秒数，时间到则关机
procedure TForm1.DoShutdownCounter(AJob: PQJob);
var
  ARemain: Int64;
begin
ARemain := FShutdownTime - GetTimeStamp;
if ARemain <= Q1Minute then
  begin
  if not IsAppActive then
    begin
    BringAppToFront;
    Workers.Post(
      procedure(AJob: PQJob)
      begin
      Workers.ClearSingleJob(FTimerHandle);
      FTimerHandle := Workers.Post(DoShutdownCounter, Q1Second, nil, True);
      end, nil, False);
    end;
  btnCancel.Text := '取消关机(还差 ' + RollupTime(ARemain div Q1Second) + ' 关机)';
  if ARemain <= 0 then
    begin
    if RootNeeded then
      FRootShell.Execute('reboot -p');
    end;
  end
else
  btnCancel.Text := '取消关机(还差 ' + RollupTime((ARemain div Q1Minute) *
    60) + ' 关机)';
end;
// 取消关机操作
procedure TForm1.btnCancelClick(Sender: TObject);
begin
Workers.ClearSingleJob(FTimerHandle);
FTimerHandle := 0;
btnCancel.Text := '取消关机';
btnCancel.Enabled := False;
end;
// 构造函数
procedure TForm1.FormCreate(Sender: TObject);
begin
FRootShell.Initliaize;
RegisterKeyMapping(23,vkReturn,TKeyKind.Functional);
end;
// 响应硬件按键，主要是要提供对红外遥控器的支持
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
Shift: TShiftState);
var
  AEvent: TNotifyEvent;
begin
btnCancel.Text:='KeyCode='+IntToStr(Key);
case Key of
  vkUp, vkLeft, vkVolumeUp:
    begin
    if ActiveControl = btnReboot then
      begin
      if btnCancel.Enabled then
        ActiveControl := btnCancel
      else
        ActiveControl := btnShutdown;
      end
    else if ActiveControl = btnShutdown then
      ActiveControl := btnReboot
    else if ActiveControl = btnCancel then
      ActiveControl := btnShutdown
    else if btnCancel.Enabled then
      ActiveControl := btnCancel
    else
      ActiveControl := btnShutdown;
    Key := 0;
    end;
  vkDown, vkRight, vkVolumeDown:
    begin
    if ActiveControl = btnReboot then
      ActiveControl := btnShutdown
    else if ActiveControl = btnShutdown then
      begin
      if btnCancel.Enabled then
        ActiveControl := btnCancel
      else
        ActiveControl := btnReboot;
      end
    else if ActiveControl = btnCancel then
      ActiveControl := btnReboot
    else
      ActiveControl := btnShutdown;
    Key := 0;
    end;
  vkReturn:
    begin
    AEvent := (Focused.GetObject as TControl).OnClick;
    if Assigned(AEvent) then
      AEvent(Self);
    Key := 0;
    Exit;
    end;
  vkHardwareBack://返回键时，如果当前已经计划关机，则返回桌面，否则应用退出
    begin
    if FTimerHandle<>0 then
      SendAppToBack
    else
      Application.Terminate;
    Key := 0;
    end;
end;
if Assigned(ActiveControl) then
  GlowEffect1.Parent := ActiveControl;
end;
// 计划关机
procedure TForm1.PendShutdown(AMinutes: Integer);
begin
if AMinutes = 0 then
  begin
  Form1.RootShell.Execute('reboot -p ');
  end
else
  begin
  FShutdownTime := GetTimeStamp + AMinutes * Q1Minute;
  FTimerHandle := Workers.Post(DoShutdownCounter, Q1Minute, nil, True);
  btnCancel.Text := '取消关机(还差 ' + RollupTime(AMinutes * 60) + ' 关机)';
  btnCancel.Enabled := True;
  SendAppToBack;
  end;
end;
// 检查 Root 权限
function TForm1.RootNeeded: Boolean;
begin
Result := FRootShell.AskForRoot;
if not Result then
  Workers.Post(DoRootDeny, nil, True);
end;

end.
