unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qstring, QPlugins, qplugins_params,
  qvalue,
  qplugins_loader_process, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qmsgpack;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AStartupInfo: TStartupInfo;
  AProcessInfo: TProcessInformation;
  AFlags: Integer;
  AEnvData: QStringW;
  pEnvData: PQCharW;
  APath,ACmdline: QStringW;
begin
  FillChar(AProcessInfo, SizeOf(AProcessInfo), 0);
  AFlags := CREATE_SUSPENDED or NORMAL_PRIORITY_CLASS;
  if Length(AEnvData) > 0 then
    pEnvData := PQCharW(AEnvData)
  else
    pEnvData := nil;
  FillChar(AStartupInfo, SizeOf(AStartupInfo), 0);
  AStartupInfo.cb := SizeOf(AStartupInfo); // 一切都默认
  // 加入参数，以便让插件能够得到宿主进程的
  APath:='C:\Windows\Notepad.exe C:\Windows\Win.ini';
  ACmdline :=APath;
  SetLength(ACmdline,MAX_PATH);
  PQCharW(ACmdLine)[Length(APath)]:=#0;
  if not CreateProcessW(nil, PQCharW(ACmdline), nil, nil, False, AFlags,
    pEnvData, nil, AStartupInfo, AProcessInfo) then
  begin
    if AProcessInfo.hProcess <> 0 then
      CloseHandle(AProcessInfo.hProcess);
    if AProcessInfo.hThread <> 0 then
      CloseHandle(AProcessInfo.hThread);
    RaiseLastOSError;
  end
  else
  begin
    ShowMessage('ProcessId='+IntToStr(AProcessInfo.dwProcessId));
    ResumeThread(AProcessInfo.hThread);
  end;
end;

end.
