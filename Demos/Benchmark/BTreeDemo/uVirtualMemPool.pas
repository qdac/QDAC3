{******************************************************************************}
{ @UnitName     : uVirtualMemPool.pas                                          }
{ @Project      : QsIOCP                                                       }
{ @Copyright    : -                                                            }
{ @Author       : 奔腾的心(7180001)                                            }
{ @Description  : 分页式虚拟内存管理                                           }
{ @FileVersion  : 1.0.0.0                                                      }
{ @CreateDate   : 2011-07-16                                                   }
{ @Comment      : -                                                            }
{ @LastUpdate   : 2011-07-20                                                   }
{******************************************************************************}
{$inline on}
unit uVirtualMemPool;

interface

uses
  SysUtils, Windows;

type
  PSBTNode = ^TSBTNode;
  TSBTNode = record
    data:           Integer; //节点数据
    size:           Integer; //节点大小
    lch,rch:        PSBTNode; //左节点，右节点
  end;

  // 平衡二叉查找树SBT
  TSizeBalancedTree = class
  private
    // 节点数
    NodeCount:      Integer;
    // 根节点,空节点
    root,null:      PSBTNode;
    // 左旋转
    procedure lrotate(var x: PSBTNode); inline;
    // 右旋转
    procedure rrotate(var x: PSBTNode); inline;
    // 保持性质
    procedure maintain(var t: PSBTNode; const flag: Boolean); inline;
    // 增加
    procedure TreeAdd(var t: PSBTNode; v: Integer); inline;
    // 移除
    function TreeRemove(var t: PSBTNode; var n: PSBTNode; v: Integer): Integer; inline;
    // 返回第 x 大的元素
    function TreeSelect(var t: PSBTNode; k: Integer): Integer; inline;
    // 查找
    function TreeFind(var t: PSBTNode; v: Integer): Boolean; inline;
    // 排名
    function TreeRank(var t: PSBTNode; v: Integer): Integer; inline;
    // 向前,大
    function TreeSucc(var t: PSBTNode; v: Integer): Integer; inline;
    // 向后,小
    function TreePred(var t: PSBTNode; v: Integer): Integer; inline;
  public
    constructor Create();
    destructor Destroy; override;
    procedure add(v:Integer);
    function remove(v: Integer): PSBTNode;
    function select(k: Integer): Integer; inline;
    function find(v: Integer): Boolean; inline;
    function rank(v: Integer): Integer; inline;
    function succ(v: Integer): Integer; inline;
    function pred(v: Integer): Integer; inline;
  end;

  // 内存管理
  TVirtualMemPool = class
  private
    m_VMLock:         TRTLCriticalSection;
    m_NMLock:         TRTLCriticalSection;
    m_PageSize:       Cardinal;
    m_Count:          Cardinal;
    m_UseHeightCount: Cardinal;
    m_UseHeightime:   TDateTime;
    m_lpBase:         Pointer;
    m_Buckets:        array of PSBTNode;

    m_SBTStorage:     TSizeBalancedTree;
  private
    function GetCount: Cardinal;
    function GetUseCount: Cardinal;
    function GetFreeCount: Cardinal;
    function GetUseHeightCount: Cardinal;
    procedure InitMemPool(ACount: Integer);
    procedure Clear;
  public
    property Count: Cardinal read GetCount;
    property UseCount: Cardinal read GetUseCount;
    property FreeCount: Cardinal read GetFreeCount;
    property UseHeightCount: Cardinal read GetUseHeightCount;
    property UseHeightTime: TDateTime read m_UseHeightime;
  public
    constructor Create(ACount: Integer);
    destructor Destroy; override;
  public
    function VMAlloc(dwSize: Cardinal; IsLock: Boolean = True): Pointer;
    function VMReAlloc(var P; dwSize: Cardinal): Pointer;
    function VMFree(var P; IsLock: Boolean = True): Boolean;
  end;

implementation

{ TSizeBalancedTree }
constructor TSizeBalancedTree.Create();
begin
  NodeCount := 0;
  new(null);
  null^.data := Cardinal(-1);
  null^.size := 0;
  null^.lch := null;
  null^.rch := null;
  root := null;
end;

destructor TSizeBalancedTree.Destroy;
begin
  NodeCount := 0;
  Dispose(null);
  inherited Destroy;
end;

procedure TSizeBalancedTree.lrotate(var x: PSBTNode);
var
  y: PSBTNode;
begin
  y := x^.rch;
  x^.rch := y^.lch;
  y^.lch := x;
  y^.size := x^.size;
  x^.size := x^.lch^.size+x^.rch^.size+1;
  x := y;
end;

procedure TSizeBalancedTree.rrotate(var x: PSBTNode);
var
  y: PSBTNode;
begin
  y := x^.lch;
  x^.lch := y^.rch;
  y^.rch := x;
  y^.size := x^.size;
  x^.size := x^.lch^.size+x^.rch^.size+1;
  x := y;
end;

procedure TSizeBalancedTree.maintain(var t: PSBTNode; const flag: Boolean);
begin
  if t=null then
    exit;
  if not flag then
    if t^.lch^.lch^.size>t^.rch^.size then
      rrotate(t)
    else if t^.lch^.rch^.size>t^.rch^.size then
    begin
      lrotate(t^.lch);
      rrotate(t);
    end
    else
      exit
  else if t^.rch^.rch^.size>t^.lch^.size then
    lrotate(t)
  else if t^.rch^.lch^.size>t^.lch^.size then
  begin
    rrotate(t^.rch);
    lrotate(t);
  end
  else
    exit;
  maintain(t^.lch, false);
  maintain(t^.rch, true);
  maintain(t, false);
  maintain(t, true);
end;

procedure TSizeBalancedTree.TreeAdd(var t: PSBTNode; v: Integer);
begin
  if t=null then
  begin
    new(t);
    t := v;
    //进入的内存设置为未使用
    t^.size := 1;
    t^.lch := null;
    t^.rch := null;
    Inc(NodeCount);
  end
  else begin
    inc(t^.size);
    if v^.data<t^.data then
      TreeAdd(t^.lch, v)
    else
      TreeAdd(t^.rch, v);
    maintain(t, v^.data>=t^.data);
  end;
end;

function TSizeBalancedTree.TreeRemove(var t: PSBTNode; var n: PSBTNode; v: Integer): Integer;
var
  tmp: PSBTNode;
begin
  dec(t^.size);
  if(v=t^.data) or ((v<t^.data) and (t^.lch=null)) or ((v>t^.data) and (t^.rch=null)) then
  begin
    Result := t^.data;
    if(t^.lch=null) or (t^.rch=null) then
    begin
      if t^.lch=null then
      begin
        tmp := t;
        t := tmp^.rch;
        if tmp<>null then
        begin
          n := tmp;
          Dec(NodeCount);
          Exit;
        end;
      end;
      if t^.rch=null then
      begin
        tmp := t;
        t := tmp^.lch;
        if tmp<>null then
        begin
          n := tmp;
          Dec(NodeCount);
          Exit;
        end;
      end;
    end
    else
      t^.data := TreeRemove(t^.lch, n, t^.data+1);
  end
  else if v<t^.data then
    Result := TreeRemove(t^.lch, n, v)
  else
    Result := TreeRemove(t^.rch, n, v);
end;

function TSizeBalancedTree.TreeSelect(var t: PSBTNode; k: Integer): Integer;
var
  off: Integer;
begin
  if (t=null) then
  begin
    Result := -1;
    Exit;
  end;

  //off表示比根大多少个位置
  off := k - t^.lch^.size-1;
  if off=0 then
  begin
    Result := t^.data;
  end
  else if off<0 then
  begin
    Result := TreeSelect(t^.lch, k);
  end
  else
  begin
    Result := TreeSelect(t^.rch, off);
  end;
end;

function TSizeBalancedTree.TreeFind(var t: PSBTNode; v: Integer): Boolean;
begin
  if t=null then
  begin
    Result := false;
    exit;
  end;

  if v<t^.data then
    Result := TreeFind(t^.lch,v)
  else
    Result := (v=t^.data) or TreeFind(t^.rch,v);
end;

function TSizeBalancedTree.TreeRank(var t: PSBTNode; v: Integer): Integer;
begin
  if t=null then
  begin
    //*空树，插入即是最大*
    Result := 1;
    exit;
  end;

  if v<t^.data then
    Result := TreeRank(t^.lch,v)
  else if v>t^.data then
    Result := t^.lch^.size+1+TreeRank(t^.rch,v)
  else {if x=t->data then}
    Result := t^.lch^.size+1;
end;

function TSizeBalancedTree.TreeSucc(var t: PSBTNode; v: Integer): Integer;
var
  tmp: Integer;
begin
  if t=null then
  begin
    Result := v;
    exit;
  end;
  if v>=t^.data then
    Result := TreeSucc(t^.rch,v)
  else
  begin
    tmp:=TreeSucc(t^.lch,v);
    if tmp=v then
      tmp := t^.data;
    Result := tmp;
  end;
end;

function TSizeBalancedTree.TreePred(var t: PSBTNode; v: Integer): Integer;
var
  tmp: Integer;
begin
  if t=null then
  begin
    Result := v;
    exit;
  end;
  if v<=t^.data then
    Result := TreePred(t^.lch, v)
  else
  begin
    tmp := TreePred(t^.rch,v);
    if tmp=v then
      tmp := t^.data;
    Result := tmp;
  end;
end;

procedure TSizeBalancedTree.add(v: Integer);
begin
  TreeAdd(root, v);
end;

function TSizeBalancedTree.remove(v: Integer): PSBTNode;
var
  v2: Integer;
  C: Pointer;
  P: PSBTNode;
begin
  Result := nil;
  TreeRemove(root, Result, v);
  if Result=nil then
    Exit;
  v2 := Result^.data;
  Result^.data := v;
  //移出的内存设置为使用
  Result^.IsUse := True;
  //value交换
  C := VMPool.m_Buckets[v2]^.value;
  VMPool.m_Buckets[v2]^.value := VMPool.m_Buckets[v]^.value;
  VMPool.m_Buckets[v]^.value := C;
  //位置交换
  P := VMPool.m_Buckets[v2];
  VMPool.m_Buckets[v2] := VMPool.m_Buckets[v];
  VMPool.m_Buckets[v] := P;
end;

function TSizeBalancedTree.select(k: Integer): Integer;
begin
  Result := TreeSelect(root, k);
end;

function TSizeBalancedTree.find(v: Integer): Boolean;
begin
  Result := TreeFind(root, v);
end;

function TSizeBalancedTree.rank(v: Integer): Integer;
begin
  Result := TreeRank(root, v);
end;

function TSizeBalancedTree.succ(v: Integer): Integer;
begin
  Result := TreeSucc(root, v);
end;

function TSizeBalancedTree.pred(v: Integer): Integer;
begin
  Result := TreePred(root, v);
end;

{ TVirtualMemPool }
constructor TVirtualMemPool.Create(ACount: Integer);
var
  SysInfo: TSystemInfo;
begin
  inherited Create;;
  InitializeCriticalSection(m_VMLock);
  InitializeCriticalSection(m_NMLock);
  GetSystemInfo(SysInfo);
  m_PageSize := SysInfo.dwPageSize;
  m_SBTStorage := TSizeBalancedTree.Create(Self);
  m_Count := ACount;
  m_UseHeightCount := 0;
  m_UseHeightime := Now();
  InitMemPool(m_Count);
end;

destructor TVirtualMemPool.Destroy;
begin
  FreeAndNil(m_SBTStorage);
  Clear;
  VirtualFree(m_lpBase, 0, MEM_RELEASE);
  DeleteCriticalSection(m_NMLock);
  DeleteCriticalSection(m_VMLock);
  inherited Destroy;
end;

function TVirtualMemPool.GetCount: Cardinal;
begin
  Result := m_Count;
end;

function TVirtualMemPool.GetUseCount: Cardinal;
begin
  EnterCriticalSection(m_NMLock);
  try
    Result := m_Count - m_SBTStorage.NodeCount;
  finally
    LeaveCriticalSection(m_NMLock);
  end;
end;

function TVirtualMemPool.GetFreeCount: Cardinal;
begin
  EnterCriticalSection(m_NMLock);
  try
    Result := m_SBTStorage.NodeCount;
  finally
    LeaveCriticalSection(m_NMLock);
  end;
end;

function TVirtualMemPool.GetUseHeightCount: Cardinal;
begin
  if m_UseHeightCount<GetUseCount then
  begin
    m_UseHeightCount := GetUseCount;
    m_UseHeightime := Now();
  end;
  Result := m_UseHeightCount;
end;

procedure TVirtualMemPool.InitMemPool(ACount: Integer);
var
  I: Integer;
begin
  EnterCriticalSection(m_VMLock);
  try
    // 申请大块内存
    m_lpBase := VirtualAlloc(nil,
    ACount*m_PageSize,
    MEM_RESERVE,
    PAGE_NOACCESS);

    SetLength(m_Buckets, ACount);
    //debug('m_lpBase: %d, NumberOfNode: %d', [Cardinal(m_lpBase), NumberOfNode]);
    for I := 0 to ACount-1 do
    begin
      { 为第I页地址提交内存。 }
      New(m_Buckets[I]);
      m_Buckets[I]^.IsUse := True;
      m_Buckets[I]^.data := I;
      m_Buckets[I]^.value := VirtualAlloc(Pointer(Cardinal(m_lpBase)+(I*m_PageSize)),
                             m_PageSize,
                             MEM_COMMIT,
                             PAGE_READWRITE);
      //debug('I: %d=%d', [I, Cardinal(m_Buckets[I]^.value)]);
      //ZeroMemory(m_Buckets[I]^.value, m_PageSize);
      m_SBTStorage.add(m_Buckets[I]);
    end;
  finally
    LeaveCriticalSection(m_VMLock);
  end;
end;

procedure TVirtualMemPool.Clear;
var
  I: Integer;
begin
  EnterCriticalSection(m_VMLock);
  try
    for I := 0 to Length(m_Buckets)-1 do
    begin
      Dispose(m_Buckets[I]);
    end;
  finally
    LeaveCriticalSection(m_VMLock);
  end;
end;

function TVirtualMemPool.VMAlloc(dwSize: Cardinal; IsLock: Boolean = True): Pointer;
var
  N, M, D1, D2, NStart, NEnd: Integer;
  P: PSBTNode;
begin
  if FreeCount<=0 then
  begin
    raise Exception.Create('No free pages in main memory.');
    Exit;
  end;

  if IsLock then
    EnterCriticalSection(m_VMLock);
  try
    N := dwSize div m_PageSize;
    if (dwSize mod m_PageSize)<>0 then
      Inc(N);
    M := 1;
    D2 := -1;
    D1 := m_SBTStorage.select(1);
    if D1<0 then
    begin
      Result := nil;
      Exit;
    end;
    if N<=1 then
    begin
      NStart := D1;
      //移出使用中的对像
      P := m_SBTStorage.remove(NStart);
      if P=nil then
      begin
        Result := nil;
        Exit;
      end;
      P^.count := 1;
      Result := P^.value;
      Exit;
    end;

    while True do
    begin
      //右旋转
      D2 := m_SBTStorage.succ(D1);
      if D2=D1 then
        Break;
      if D2=D1+1 then
      begin
        Inc(M);
      end
      else
      begin
        M := 1;
      end;
      if M>=N then
        Break;
      D1 := D2;
    end;
    NStart := D2 - N + 1;
    NEnd := NStart + N;
    P := m_SBTStorage.remove(NStart);
    P^.count := N;
    Result := P^.value;
    Inc(NStart);
    while NStart<NEnd do
    begin
      m_SBTStorage.remove(NStart);
      Inc(NStart);
    end;
  finally
    if IsLock then
      LeaveCriticalSection(m_VMLock);
  end;
end;

function TVirtualMemPool.VMReAlloc(var P; dwSize: Cardinal): Pointer;
var
  OldN, M, NewN, NEnd: Cardinal;
  NewP: Pointer;
begin
  Result := nil;
  if Pointer(P)=nil then
  begin
    Result := VMAlloc(dwSize);
    Exit;
  end;
  EnterCriticalSection(m_VMLock);
  try
    M := (Cardinal(Pointer(P))-Cardinal(m_lpBase)) div m_PageSize;
    // 原页数
    OldN := m_Buckets[M]^.count;
    // 新页数
    NewN := dwSize div m_PageSize;
    if (dwSize mod m_PageSize)<>0 then
      Inc(NewN);
    // 新页数=原页数
    if NewN=OldN then
    begin
      Result := Pointer(P);
    end
    // 新页数<原页数,多余的页放回页表
    else if NewN<OldN then
    begin
      NEnd := M + OldN;
      m_Buckets[M]^.count := OldN-NewN;
      M := M + NewN;
      while M<NEnd do
      begin
        m_SBTStorage.add(m_Buckets[M]);
        Inc(M);
      end;
      Result := Pointer(P);
    end
    // 新页数>原页数,重新申请并Copy原数据到新数据
    else if NewN>OldN then
    begin
      NewP := VMAlloc(dwSize, False);
      //原数据Copy到新数据中
      if NewP<>nil then
        CopyMemory(NewP, Pointer(P), OldN*m_PageSize);
      //放回原页数
      VMFree(P, False);
      //返回
      Pointer(P) := NewP;
      Result := NewP;
    end;
  finally
    LeaveCriticalSection(m_VMLock);
  end;
end;

function TVirtualMemPool.VMFree(var P; IsLock: Boolean = True): Boolean;
var
  M, N, NEnd: Cardinal;
begin
  if Pointer(P)=nil then
    Exit;
  if IsLock then
    EnterCriticalSection(m_VMLock);
  try
    M := (Cardinal(Pointer(P))-Cardinal(m_lpBase)) div m_PageSize;
    Pointer(P) := nil;
    // 页数
    N := m_Buckets[M]^.count;
    NEnd := M + N;
    // 放回
    while M<NEnd do
    begin
      m_SBTStorage.add(m_Buckets[M]);
      Inc(M);
    end;
  finally
    if IsLock then
      LeaveCriticalSection(m_VMLock);
  end;
  Result := True;
end;

end.
