unit sortinthread;

interface
uses classes,qworker;

type
  TThreadSort=class
  private
    FEnableThreads: Boolean;
  protected
    FItems:TList;
    FCompare:TListSortCompare;
    FCounter:Integer;
    FNotify:THandle;
    function Median(const A,B,C:Pointer):Pointer;
    function Lg(ASize:Integer):Integer;
    procedure IntroSort(L,R,DepthLimit:Integer);overload;
    procedure FinalInsertionSort(L,R:Integer);
    procedure UnguardedInsertionSort(L,R:Integer);
    procedure UnguardedInsertionSortAux(L,R:Integer);
    procedure InsertionSort(L,R:Integer);
    procedure LinearInsert(L,R:Integer);
    procedure UnguardedLinearInsert(R:Integer;Value:Pointer);
    procedure PartialSort(L,M,R:Integer);
    procedure MakeHeap(L,R:Integer);
    procedure AdjustHeap(L,H,B:Integer;Value:Pointer);
    procedure PushHeap(L,H,T:Integer;Value:Pointer);
    procedure PopHeap(L,R,D:Integer;Value:Pointer);overload;
    procedure PopHeapAux(L,R:Integer);
    procedure PopHeap(L,R:Integer);overload;
    procedure SortHeap(L,R:Integer);
    function Partition(L,R:Integer;P:Pointer):Integer;
    procedure DoIntroSort(AJob:PQJob);
    procedure QueueIntroSort(L,R,DL:Integer);
  public
    constructor Create(AItems:TList;ACompare:TListSortCompare);overload;
    procedure IntroSort;overload;
    procedure HeapSort;overload;
    property EnableThreads:Boolean read FEnableThreads write FEnableThreads;
    destructor Destroy;override;
  end;
  PThreadSortParam=^TThreadSortParam;
  TThreadSortParam=record
    Left,Right,DepthLimit:Integer;
  end;
procedure SortInThreads(AItems:TList;AOnCompare:TListSortCompare;AEnableThreads:Boolean);
implementation
uses sysutils,windows;
const Threshold=16;//数据量的分界线，决定了使用Quick Sort/Heap Sort还是Insertion sort
  ThreadThreshold=100000;//使用线程单独处理的最小分区大小
procedure OutputOrders(AItems:TList);
  var
    J:Integer;
    S:AnsiString;
  begin
  SetLength(S,0);
  for J := 0 to AItems.Count - 1 do
    S:=S+IntToStr(Integer(AItems[J]))+',';
  SetLength(S,Length(S)-1);
//  OutputDebugString(PAnsiChar(S));
  end;


procedure SortInThreads(AItems:TList;AOnCompare:TListSortCompare;AEnableThreads:Boolean);
var
  ASorter:TThreadSort;
begin
ASorter:=TThreadSort.Create(AItems,AOnCompare);
ASorter.EnableThreads:=AEnableThreads;
ASorter.IntroSort;
ASorter.Free;
end;
{ TThreadSort }

//将STL的Introsort排序算法移植过来
//排序入口
procedure TThreadSort.AdjustHeap(L, H, B: Integer;Value:Pointer);
var
  T,I:Integer;
begin
T:=H;
I:=(H+1) shl 1;
while I < B do
  begin
  if FCompare(FItems[L+I],FItems[L+I-1])<0 then
    Dec(I);
  FItems[L+H]:=FItems[L+I];
  H:=I;
  I:=(I+1) shl 1;
  end;
if I=B then
  begin
  FItems[L+H]:=FItems[L+(B-1)];
  H:=B-1;
  end;
PushHeap(L,H,T,Value);
end;

constructor TThreadSort.Create(AItems: TList; ACompare: TListSortCompare);
begin
FItems:=AItems;
FCompare:=ACompare;
FNotify:=CreateEvent(nil,false,false,nil);
end;

destructor TThreadSort.Destroy;
begin
CloseHandle(FNotify);
  inherited;
end;

procedure TThreadSort.DoIntroSort(AJob:PQJob);
var
  AParams:PThreadSortParam;
begin
AParams:=AJob.Data;
try
  IntroSort(AParams.Left,AParams.Right,AParams.DepthLimit);
finally
  if InterlockedDecrement(FCounter)=0 then
    SetEvent(FNotify);
  Dispose(AParams);
end;
end;

procedure TThreadSort.FinalInsertionSort(L, R: Integer);
begin
if R-L>ThresHold then
  begin
  //分为两段前者调用插入排序，因为后段的元素总是比前段大（由Quick Sort性质可知），所以先  
  //调用前者完成前段排序，然后将后段从尾部遍历的方式插入已序的元素中  
  InsertionSort(L,L+ThresHold);
  UnguardedInsertionSort(L+ThresHold,R);
  end
else
  InsertionSort(L,R);
end;

procedure TThreadSort.HeapSort;
begin
if FItems.Count>1 then
  begin
  MakeHeap(0,FItems.Count);
  SortHeap(0,FItems.Count);
  end;
end;

// 对指定区域完成插入排序
procedure TThreadSort.InsertionSort(L, R: Integer);
var
  I:Integer;
begin
if L<>R then
  begin
  for I := L+1 to R - 1 do
    LinearInsert(L,I);
  end;
end;
procedure TThreadSort.IntroSort;
begin
if FItems.Count>1 then
  begin
  FCounter:=0;
  ResetEvent(FNotify);
  IntroSort(0,FItems.Count,Lg(FItems.Count) shl 1);
  if FCounter>0 then
    WaitForSingleObject(FNotify,INFINITE);
//  while FCounter>0 do
//    SwitchToThread;
  FinalInsertionSort(0,FItems.Count);
  end;
end;

//完成后将返回母函数sort()在进入__final_insertion_sort()最终完成排序
procedure TThreadSort.IntroSort(L, R, DepthLimit:Integer);
var
  Cut:Integer;
begin
//InterlockedIncrement(FCounter);
// 以下，Threshold 是个全局常数，稍早定义为 const int 16。
//判断序列大小，如果小于等于16使用Quick Sort的排序，留给Insertion Sort最终完成排序
if R-L>Threshold then
  begin
  if DepthLimit=0 then// 至此，切割恶化，改用 heapsort
    begin
    PartialSort(L,R,R); // partial_sort是以Heap Sort实现
    Exit;
    end;
  Dec(DepthLimit);
  // 以下是 median-of-three partition，选择一个够好的枢轴并决定切割点。
  // 切割点将落在迭代器 cut 身上。
  Cut:= Partition(L,R,Median(FItems[L],FItems[(L+R) shr 1],FItems[R-1]));
  QueueIntroSort(L,Cut-1,DepthLimit);//对左半段递归进行 sort.
  // 对右半段递归进行 sort.
  QueueIntroSort(Cut,R,DepthLimit);//
  end;
//InterlockedDecrement(FCounter);
end;

//Lg用来控制分割恶化的情况
//找出 2^k <= n 的最大值k。例，n=7，得k=2，n=20，得k=4，n=8，得k=3。
function TThreadSort.Lg(ASize: Integer): Integer;
begin
Result:=0;
while ASize>1 do
  begin
  Inc(Result);
  ASize:=ASize shr 1;
  end;
end;

procedure TThreadSort.LinearInsert(L, R: Integer);
var
  Value:Pointer;
  I:Integer;
begin
Value:=FItems[R];// 记录尾元素
if FCompare(Value,FItems[L])<0 then// 尾比头还小（那就别一个个比较了，一次做完…）
  begin
  //将最左边的元素位置让出来，以插入新值
  for I := R downto L+1 do
    FItems.Exchange(I,I-1);
  FItems[L]:=Value;
  end
else
  UnguardedLinearInsert(R,Value);
end;

procedure TThreadSort.MakeHeap(L, R: Integer);
var
  D,P:Integer;
begin
if R-L>=2 then
  begin
  D:=R-L;
  P:=(D-1) shr 1;
  while True do
    begin
    AdjustHeap(L,P,D,FItems[L+P]);
    if P=0 then
      Break;
    Dec(P);
    end;
  end;
end;

function TThreadSort.Median(const A, B, C: Pointer): Pointer;
begin
if FCompare(A,B)<0 then
  begin
  if FCompare(B,C)<0 then
    Result:=B
  else if FCompare(A,C)<0 then
    Result:=C
  else
    Result:=A;
  end
else if FCompare(A,C)<0 then
  Result:=A
else if FCompare(B,C)<0 then
  Result:=C
else
  Result:=B;
end;

procedure TThreadSort.PartialSort(L, M, R: Integer);
var
  I:Integer;
  AFirst,ANext:Pointer;
begin
MakeHeap(L,M);
AFirst:=FItems[L];
for I:=M to R - 1 do
  begin
  ANext:=FItems[I];
  if FCompare(ANext,AFirst)<0 then
    PopHeap(L,M,I,ANext);
  SortHeap(L,M);
  end;
end;

function TThreadSort.Partition(L, R: Integer; P: Pointer): Integer;
begin
repeat
  begin
  while FCompare(FItems[L],P)<0 do
    Inc(L);
  Dec(R);
  while FCompare(P,FItems[R])<0 do
    Dec(R);
  if L<R then
    begin
    FItems.Exchange(L,R);
    Inc(L);
    end
  else
    Break;
  end;
until False;
Result:=L;
end;

procedure TThreadSort.PopHeap(L, R: Integer);
begin
if L<R then
  PopHeapAux(L,R);
end;

procedure TThreadSort.PopHeapAux(L, R: Integer);
begin
PopHeap(L,R-1,R-1,FItems[R-1]);
end;

procedure TThreadSort.PopHeap(L, R, D: Integer; Value: Pointer);
begin
FItems[D]:=FItems[L];
AdjustHeap(L,0,R-L,Value);
end;

procedure TThreadSort.PushHeap(L, H, T: Integer;Value:Pointer);
var
  I:Integer;
begin
I:=(H-1) shr 1;
while (T<H) and (FCompare(FItems[L+I],Value)<0) do
  begin
  FItems[L+H]:=FItems[L+I];
  H:=I;
  I:=(H-1) shr 1;
  end;
FItems[L+H]:=Value;
end;

procedure TThreadSort.QueueIntroSort(L, R, DL: Integer);
var
  AParams:PThreadSortParam;
begin
if (not EnableThreads) or (R-L<ThreadThreshold) then
  IntroSort(L,R,DL)
else
  begin
  New(AParams);
  AParams.Left:=L;
  AParams.Right:=R;
  AParams.DepthLimit:=DL;
  InterlockedIncrement(FCounter);
  Workers.Post(DoIntroSort,AParams);
  end;
end;

procedure TThreadSort.SortHeap(L, R: Integer);
begin
while R>L do
  begin
  OutputOrders(FItems);
  PopHeap(L,R);
  Dec(R);
  end;
end;

procedure TThreadSort.UnguardedInsertionSort(L, R: Integer);
begin
UnguardedInsertionSortAux(L,R);
end;

procedure TThreadSort.UnguardedInsertionSortAux(L, R: Integer);
var
  I:Integer;
begin
for I := L to R - 1 do
  UnguardedLinearInsert(I,FItems[I]);
end;

procedure TThreadSort.UnguardedLinearInsert(R: Integer; Value: Pointer);
var
  Next:Integer;
  ANextVal:Pointer;
begin
Next:=R;
Dec(Next);
ANextVal:=FItems[Next];
if ANextVal<>Value then
  begin
  while FCompare(Value,ANextVal)<0 do
    begin
    FItems[R]:=FItems[Next];
    R:=Next;
    Dec(Next);
    ANextVal:=FItems[Next];
    end;
  FItems[R]:=Value;
  end;
end;

end.
