program qlogconsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  SysUtils, qlog;

begin
  try
    // 直接将日志输出到控制台
    Logs.Castor.AddWriter(TQLogConsoleWriter.Create(False));
    // 输出一句日志，虽然是先输出的，但由于是异步日志写入，会后显示
    PostLog(llHint, 'Application.Start');
    // 输出到控制台
    WriteLn('Hello,world');
    PostLog(llDebug, 'Application Waiting input');
    ReadLn(Input);
    PostLog(llDebug, 'Application Stop');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
