program consoledemo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils, QWorker;

type
  TTestJob = class
  public
    procedure DoNullJob(AJob: PQJob);

  end;

  { TTestJob }
var
  ATest:TTestJob;
procedure TTestJob.DoNullJob(AJob: PQJob);
begin
TMonitor.Enter(ATest);
WriteLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' 作业 ' +
  IntToStr(IntPtr(AJob.Data)) + ' 已经执行。');
TMonitor.Exit(ATest);
end;


begin
try
  { TODO -oUser -cConsole Main : Insert code here }
  ATest:=TTestJob.Create;
  Workers.Post(ATest.DoNullJob, 10000, Pointer(1));
  Workers.Post(ATest.DoNullJob, 10000, Pointer(2));
  Workers.Post(ATest.DoNullJob, 10000, Pointer(3));
  ATest.Free;
  ReadLn;
except
  on E: Exception do
    Writeln(E.ClassName, ': ', E.Message);
end;

end.
