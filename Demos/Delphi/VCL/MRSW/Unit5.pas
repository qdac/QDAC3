unit Unit5;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qstring, QWorker, Vcl.StdCtrls,
  syncobjs, SyncMultipeReadersOneWriter, Vcl.Samples.Spin;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Memo1: TMemo;
    WinMRSW: TButton;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Edit1: TEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WinMRSWClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FBuffer: TBytes;
    FCS: TCriticalSection;
    FMRSW: TMultipleReaderOneWriter;
    FOS: THandle;
    FRWSync: TMultiReadExclusiveWriteSynchronizer;
    FReadTimes, FWriteTimes: Integer;
    FReadRatio: Integer;
    procedure ReadTest;
    procedure WriteTest;
    procedure TestMRSW(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
    procedure TestCS(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
    procedure TestOS(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
    procedure TestSysutils(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}
procedure InitializeSRWLock(var SRWLOCK: THandle); stdcall; external kernel32;
procedure ReleaseSRWLockExclusive(var SRWLOCK: THandle); stdcall;
  external kernel32;
procedure ReleaseSRWLockShared(var SRWLOCK: THandle); stdcall;
  external kernel32;
procedure AcquireSRWLockExclusive(var SRWLOCK: THandle); stdcall;
  external kernel32;
procedure AcquireSRWLockShared(var SRWLOCK: THandle); stdcall;
  external kernel32;
function TryAcquireSRWLockExclusive(var SRWLOCK: THandle): Boolean; stdcall;
  external kernel32;
function TryAcquireSRWLockShared(var SRWLOCK: THandle): Boolean; stdcall;
  external kernel32;

procedure TForm5.Button1Click(Sender: TObject);
var
  T: Cardinal;
begin
SetLength(FBuffer, 2048);
FReadTimes := 0;
FWriteTimes := 0;
FReadRatio := SpinEdit1.Value;
T := GetTimeStamp;
TQForJobs.For(0, StrToInt(Edit1.Text) - 1, TestMRSW);
T := GetTimeStamp - T;
Label1.Caption := 'MRSW:Read ' + IntToStr(FReadTimes) + ' times ,Write ' +
  IntToStr(FWriteTimes) + ' times,Used Time ' + FloatToStr(T / 10) + ' ms';
Memo1.Lines.Add(Label1.Caption);
end;

procedure TForm5.Button2Click(Sender: TObject);
var
  T: Cardinal;
begin
SetLength(FBuffer, 2048);
FReadTimes := 0;
FWriteTimes := 0;
FReadRatio := SpinEdit1.Value;
T := GetTimeStamp;
TQForJobs.For(0, StrToInt(Edit1.Text) - 1, TestCS);
T := GetTimeStamp - T;
Label1.Caption := 'CS:Read ' + IntToStr(FReadTimes) + ' times ,Write ' +
  IntToStr(FWriteTimes) + ' times,Used Time ' + FloatToStr(T / 10) + ' ms';
Memo1.Lines.Add(Label1.Caption);
end;

procedure TForm5.Button3Click(Sender: TObject);
var
  T: Cardinal;
begin
SetLength(FBuffer, 2048);
FReadTimes := 0;
FWriteTimes := 0;
FReadRatio := SpinEdit1.Value;
T := GetTimeStamp;
TQForJobs.For(0, StrToInt(Edit1.Text) - 1, TestSysutils);
T := GetTimeStamp - T;
Label1.Caption := 'Sysutils:Read ' + IntToStr(FReadTimes) + ' times ,Write ' +
  IntToStr(FWriteTimes) + ' times,Used Time ' + FloatToStr(T / 10) + ' ms';
Memo1.Lines.Add(Label1.Caption);

end;

procedure TForm5.FormCreate(Sender: TObject);
begin
FCS := TCriticalSection.Create;
FMRSW := TMultipleReaderOneWriter.Create;
FRWSync := TMultiReadExclusiveWriteSynchronizer.Create;
FOS := 0;
InitializeSRWLock(FOS);
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
FreeObject(FCS);
FreeObject(FMRSW);
FreeObject(FRWSync);
end;

procedure TForm5.ReadTest;
var
  I: Integer;
  C: Byte;
begin
AtomicIncrement(FReadTimes);
C := FBuffer[0];
for I := 1 to Length(FBuffer) - 1 do
  begin
  assert(FBuffer[I] = C, 'BufferError');
  end;
end;

procedure TForm5.TestCS(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
begin
if (AIndex mod 10) < FReadRatio then
  begin
  FCS.Enter;
  try
    ReadTest;
  finally
    FCS.Leave;
  end;
  end
else
  begin
  FCS.Enter;
  try
    WriteTest;
  finally
    FCS.Leave;
  end;
  end;
end;

procedure TForm5.TestMRSW(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
begin
if (AIndex mod 10) < FReadRatio then
  begin
  FMRSW.AcquireReadLock;
  try
    ReadTest;
  finally
    FMRSW.ReleaseReadLock;
  end;
  end
else
  begin
  FMRSW.AcquireWriteLock;
  try
    WriteTest;
  finally
    FMRSW.ReleaseWriteLock;
  end;
  end;
end;

procedure TForm5.TestOS(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
begin
if (AIndex mod 10) < FReadRatio then
  begin
  AcquireSRWLockShared(FOS);
  try
    ReadTest;
  finally
    ReleaseSRWLockShared(FOS);
  end;
  end
else
  begin
  AcquireSRWLockExclusive(FOS);
  try
    WriteTest;
  finally
    ReleaseSRWLockExclusive(FOS);
  end;
  end;

end;

procedure TForm5.TestSysutils(ALoopMgr: TQForJobs; AJob: PQJob;
  AIndex: NativeInt);
begin
if (AIndex mod 10) < FReadRatio then
  begin
  FRWSync.BeginRead;
  try
    ReadTest;
  finally
    FRWSync.EndRead;
  end;
  end
else
  begin
  FRWSync.BeginWrite;
  try
    WriteTest;
  finally
    FRWSync.EndWrite;
  end;
  end;
end;

procedure TForm5.WinMRSWClick(Sender: TObject);
var
  T: Cardinal;
begin
SetLength(FBuffer, 2048);
FReadTimes := 0;
FWriteTimes := 0;
FReadRatio := SpinEdit1.Value;
T := GetTimeStamp;
TQForJobs.For(0, StrToInt(Edit1.Text) - 1, TestOS);
T := GetTimeStamp - T;
Label1.Caption := 'API: Read ' + IntToStr(FReadTimes) + ' times ,Write ' +
  IntToStr(FWriteTimes) + ' times,Used Time ' + FloatToStr(T / 10) + ' ms';
Memo1.Lines.Add(Label1.Caption);
end;

procedure TForm5.WriteTest;
var
  I: Integer;
  C: Byte;
begin
C := Random(255);
AtomicIncrement(FWriteTimes);
for I := 0 to Length(FBuffer) - 1 do
  FBuffer[I] := C;
end;

end.
