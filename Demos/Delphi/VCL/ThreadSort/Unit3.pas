unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, qworker, Vcl.Samples.Spin;

{ .$DEFINE FLOAT }  // 使用浮点数进行测试
type
  TTestItemType = {$IFDEF FLOAT}Double{$ELSE}Integer{$ENDIF};
  TTestArray = array of TTestItemType;
  PTestArray = ^TTestArray;

  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    seWorkers: TSpinEdit;
    Label3: TLabel;
    seNum: TSpinEdit;
    Button4: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FBlockCount: Integer;
    FCount: Integer;
    FBlockSize: Integer;
    FOrigin: TTestArray;
    FSource: TTestArray;
    FAsynSortStartTime: Cardinal;
    { Private declarations }
    procedure GenerateData;
    procedure CopyData;
    procedure DoSort(AJob: PQJob);
    procedure DoBlockSorted(AJob: PQJob);
    procedure QuickSort(var A: TTestArray; iLo, iHi: Integer);
    procedure MainThreadSort;
    procedure SortWithWait;
    procedure SortAsyn;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

type
  TSortRange = record
    StartIndex, EndIndex: Integer;
  end;

  PSortRange = ^TSortRange;
{$R *.dfm}

procedure TForm3.GenerateData;
var
  I: Integer;
begin
Randomize;
FCount := seNum.Value;
SetLength(FOrigin, FCount);
for I := 0 to FCount - 1 do
  FOrigin[I] := Random(MaxInt);
end;

procedure TForm3.CopyData;
begin
SetLength(FSource, FCount);
Move(FOrigin[0], FSource[0], FCount * SizeOf(TTestItemType));
end;

procedure TForm3.MainThreadSort;
var
  T: Cardinal;
begin
T := GetTickCount;
QuickSort(FSource, 0, FCount - 1);
T := GetTickCount - T;
Memo1.Lines.Add('主线程排序用时:' + IntToStr(T) + 'ms');
end;

procedure TForm3.SortWithWait;
var
  I, C: Integer;
  AMax, AMin, AGateValue, V: TTestItemType;
  AGroup: TQJobGroup;
  T, T1, T2: Cardinal;
  ARange: PSortRange;
  AStartIdx: Integer;
  J: Integer;
begin
AGroup := TQJobGroup.Create(False);
AGroup.Prepare;
FBlockCount := seWorkers.Value;
T := GetTickCount;
AMax := FSource[0];
AMin := AMax;
for I := 1 to FCount - 1 do
  begin
  if AMax < FSource[I] then
    AMax := FSource[I];
  if AMin > FSource[I] then
    AMin := FSource[I];
  end;
T2 := GetTickCount;
T1 := T2 - T;
FBlockSize := FCount div FBlockCount;
if (FCount mod FBlockSize) <> 0 then
  Inc(FBlockSize);
// 第一步：多线程分块
AStartIdx := 0;
C := FBlockCount;

for I := 0 to C - 1 do
  begin
  AGateValue := AMin + ((AMax - AMin) {$IFDEF FLOAT} / {$ELSE} div
{$ENDIF} FBlockCount) * (I + 1);
  New(ARange);
  ARange.StartIndex := AStartIdx;
  ARange.EndIndex := AStartIdx;
  for J := AStartIdx to FCount - 1 do
    begin
    if FSource[J] <= AGateValue then
      begin
      if J > ARange.EndIndex then
        begin
        V := FSource[ARange.EndIndex];
        FSource[ARange.EndIndex] := FSource[J];
        FSource[J] := V;
        Inc(ARange.EndIndex);
        end;
      end;
    end;
  AStartIdx := ARange.EndIndex;
  Dec(ARange.EndIndex);
  AGroup.Add(DoSort, ARange, False);
  end;
T2 := GetTickCount - T2;
AGroup.Run;
AGroup.WaitFor();
T := GetTickCount - T;
Memo1.Lines.Add('多线程排序(选择法分区)用时:' + IntToStr(T) + 'ms(范围检测用时' + IntToStr(T1) +
  'ms,数据分区用时:' + IntToStr(T2) + 'ms)');
AGroup.Free;
FBlockCount := 0;
end;

procedure TForm3.SortAsyn;
var
  I, C, AWorkers: Integer;
  AMax, AMin: TTestItemType;
  ARange: PSortRange;
  T1, T2: Cardinal;

  procedure QuickPartion(L, R, ABlockCount: Integer;
    AMinValue, AMaxValue: TTestItemType);
  var
    Lo, Hi: Integer;
    Mid, T: TTestItemType;
    AExchanged: Boolean;
  begin
  Lo := L;
  Hi := R;
  Mid := (AMinValue{$IFDEF FLOAT} / {$ELSE} div
{$ENDIF} 2) + (AMaxValue{$IFDEF FLOAT} / {$ELSE} div {$ENDIF} 2);
  repeat
    while FSource[Lo] <= Mid do
      Inc(Lo);
    while (Lo < Hi) and (FSource[Hi] > Mid) do
      Dec(Hi);
    if Lo < Hi then
      begin
      T := FSource[Lo];
      FSource[Lo] := FSource[Hi];
      FSource[Hi] := T;
      Inc(Lo);
      Dec(Hi);
      end;
  until Lo >= Hi;
  if Lo = Hi then
    Dec(Hi);
  Dec(ABlockCount, 2);
  if ABlockCount <= 0 then
    begin
    New(ARange);
    ARange.StartIndex := L;
    ARange.EndIndex := Hi;
    Inc(AWorkers, 2);
    Workers.Post(DoSort, ARange);
    New(ARange);
    ARange.StartIndex := Lo;
    ARange.EndIndex := R;
    Workers.Post(DoSort, ARange);
    end
  else
    begin
    QuickPartion(L, Hi, ABlockCount shr 1, AMinValue, Mid);
    QuickPartion(Lo, R, ABlockCount shr 1, Mid, AMaxValue);
    end;
  end;

begin
FBlockCount := seWorkers.Value;
FBlockSize := FCount div FBlockCount;
FAsynSortStartTime := GetTickCount;
AMax := FSource[0];
AMin := AMax;
for I := 1 to FCount - 1 do
  begin
  if AMax < FSource[I] then
    AMax := FSource[I];
  if AMin > FSource[I] then
    AMin := FSource[I];
  end;
C := 0;
AWorkers := 0;
T2 := GetTickCount;
T1 := T2 - FAsynSortStartTime;
QuickPartion(0, FCount - 1, FBlockCount, AMin, AMax);
FBlockCount := AWorkers;
T2 := GetTickCount - T2;
Memo1.Lines.Add('排序作业请求已经提交，值范围扫描用时' + IntToStr(T1) + 'ms,分区(快速排序法分区)用时' +
  IntToStr(T2) + 'ms');
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
GenerateData;
CopyData;
SortWithWait;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
GenerateData;
CopyData;
MainThreadSort;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
Button1Click(Sender); // 线程等待
Application.ProcessMessages;
Button2Click(Sender); // 主线程
Application.ProcessMessages;
Button4Click(Sender); // 异步排序
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
GenerateData;
CopyData;
SortAsyn;
end;

function DoSortCompare(P1, P2: Pointer): Integer;
begin
Result := IntPtr(P1) - IntPtr(P2);
end;

procedure TForm3.DoBlockSorted(AJob: PQJob);
var
  T: Cardinal;
begin
Dec(FBlockCount);
if FBlockCount = 0 then
  begin
  T := GetTickCount - FAsynSortStartTime;
  Memo1.Lines.Add('异步排序完成，用时:' + IntToStr(T) + 'ms');
  end;
end;

procedure TForm3.DoSort(AJob: PQJob);
var
  ARange: PSortRange;
begin
ARange := AJob.Data;
QuickSort(FSource, ARange.StartIndex, ARange.EndIndex);
Dispose(ARange);
Workers.Post(DoBlockSorted, nil, true);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
seWorkers.Value := GetCpuCount;
seWorkers.MaxValue := seWorkers.Value * 4;
seNum.Value := 20000000;
end;

procedure TForm3.QuickSort(var A: TTestArray; iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid, T: TTestItemType;
begin
Lo := iLo;
Hi := iHi;
Mid := A[(Lo + Hi) div 2];
repeat
  while A[Lo] < Mid do
    Inc(Lo);
  while A[Hi] > Mid do
    Dec(Hi);
  if Lo <= Hi then
    begin
    T := A[Lo];
    A[Lo] := A[Hi];
    A[Hi] := T;
    Inc(Lo);
    Dec(Hi);
    end;
until Lo > Hi;
if Hi > iLo then
  QuickSort(A, iLo, Hi);
if Lo < iHi then
  QuickSort(A, Lo, iHi);
end;

end.
