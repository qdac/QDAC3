unit sWZ_SysCoreUnit;

interface

uses
  classes,sysutils,syncobjs;

type
  PSwzSingleNode = ^TSwzSingleNode;
  TSwzSingleNode = record
    lpNext: PSwzSingleNode;
    lpData: Pointer;
  end;

  PSwzSingleListHeader = ^TSwzSingleListHeader;
  TSwzSingleListHeader = record
    lpNext:     PSwzSingleListHeader;
    nSize:      UIntPtr;
    nCount:     UIntPtr;
    nCurrPos:   UIntPtr;
  end;

type
  TSwzSingleListWithLock = Class(TObject)
  private
    m_lpCurrNode:         PSwzSingleNode;
    m_nNodeCount:         UIntPtr;

    m_lpFreeNode:         PSwzSingleNode;
    m_nFreeCount:         UIntPtr;

    m_lpCurrListHeader:   PSwzSingleListHeader;
    m_nListHeaderCount:   UIntPtr;

    m_Flags: Integer;
    m_stLock: TCriticalSection;
  private
    function AddNewListPage(nNodeSize: UIntPtr): boolean;

    procedure Enter(); inline;
    procedure Leave(); inline;
  public
    constructor Create(nNodeCount: UIntPtr = 0);
    destructor Destroy; override;

    function GetCount(): UIntPtr;
    procedure Clear;

    function Push(p: Pointer): boolean;
    function Pop(): Pointer;
  public
    property NodeCount: UIntPtr read GetCount;
  end;

implementation

const
  MAX_PAGE_SIZE   = 1024 * 64;

//位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask:Integer): Integer; inline;
var
  i:Integer;
begin
  repeat
    Result := Dest;
    i := Result and AMask;
  until AtomicCmpExchange(Dest,i,Result)=Result;
end;

//位或，返回原值
function AtomicOr(var Dest: Integer; const AMask:Integer): Integer; inline;
var
  i:Integer;
begin
  repeat
    Result := Dest;
    i := Result or AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;

constructor TSwzSingleListWithLock.Create(nNodeCount: UIntPtr = 0);
var
  nPageSize: UIntPtr;
begin
  inherited Create;

  m_lpCurrNode        := nil;
  m_nNodeCount        := 0;

  m_lpFreeNode        := nil;
  m_nFreeCount        := 0;

  m_lpCurrListHeader  := nil;
  m_nListHeaderCount  := 0;

  m_stLock:=TCriticalSection.Create;
  //InitializeCriticalSectionAndSpinCount(m_stLock, 2000);


  if (nNodeCount = 0) then
    nPageSize := MAX_PAGE_SIZE
  else
    nPageSize := SizeOf(TSwzSingleListHeader) + (nNodeCount * SizeOf(TSwzSingleNode));
  
  AddNewListPage(nPageSize);
end;

destructor TSwzSingleListWithLock.Destroy;
begin
  Clear();

  if (m_lpCurrListHeader <> nil) then
    FreeMemory(m_lpCurrListHeader);

  FreeAndNil(m_stLock);
  inherited Destroy;
end;

procedure TSwzSingleListWithLock.Enter();
begin
  m_stlock.Enter;
end;

procedure TSwzSingleListWithLock.Leave();
begin
  m_stLock.Leave;
end;

procedure TSwzSingleListWithLock.Clear;
var
  lpListHeader: PSwzSingleListHeader;
begin
  Enter();

  if (m_nListHeaderCount > 0) then
  begin
    while (m_nListHeaderCount > 1) do
    begin
      lpListHeader := m_lpCurrListHeader;
      m_lpCurrListHeader := m_lpCurrListHeader^.lpNext;

      FreeMemory(lpListHeader);
      Dec(m_nListHeaderCount);
    end;

    m_lpCurrListHeader^.nCurrPos := 0;

    m_lpCurrNode := nil;
    m_nNodeCount := 0;

    m_lpFreeNode := nil;
    m_nFreeCount := 0;
  end;

  Leave();
end;

function TSwzSingleListWithLock.GetCount(): UIntPtr;
begin
  Enter();
  Result := m_nNodeCount;
  Leave();
end;

function TSwzSingleListWithLock.Pop(): Pointer;
var
  lpNode: PSwzSingleNode;
begin
  Result := nil;

  Enter();

  if ((m_nNodeCount > 0) and (m_lpCurrNode <> nil)) then
  begin
    Result := m_lpCurrNode^.lpData;

    lpNode := m_lpCurrNode;
    m_lpCurrNode := m_lpCurrNode^.lpNext;
    Dec(m_nNodeCount);

    lpNode^.lpNext := m_lpFreeNode;
    m_lpFreeNode := lpNode;
    Inc(m_nFreeCount);
  end;

  Leave();
end;

function TSwzSingleListWithLock.AddNewListPage(nNodeSize: UIntPtr): boolean;
var
  nPageSize: UIntPtr;
  lpListHeader: PSwzSingleListHeader;
begin
  Result := False;

  if ((nNodeSize mod MAX_PAGE_SIZE) = 0) then
    nPageSize := (nNodeSize div MAX_PAGE_SIZE) * MAX_PAGE_SIZE
  else
    nPageSize := ((nNodeSize div MAX_PAGE_SIZE) + 1) * MAX_PAGE_SIZE;

  lpListHeader := GetMemory(nPageSize);
  if (lpListHeader = nil) then Exit;

  with lpListHeader^ do
  begin
    lpListHeader^.lpNext    := m_lpCurrListHeader;
    lpListHeader^.nSize     := nPageSize;
    lpListHeader.nCount     := ((nPageSize - SizeOf(TSwzSingleListHeader)) div SizeOf(TSwzSingleNode));
    lpListHeader^.nCurrPos  := 0;
  end;
  m_lpCurrListHeader := lpListHeader;
  Inc(m_nListHeaderCount);

  Result := True;
end;

function TSwzSingleListWithLock.Push(p: Pointer): boolean;
label
  __ADD_NODE, __ADD_PAGE;
var
  lpListHeader: PSwzSingleListHeader;
  lpNode: PSwzSingleNode;
begin
  Result := False;

  Enter();

  //  1.优先从偏移数组中获取链表数据
  if (m_lpCurrListHeader <> nil) then
  begin
__ADD_NODE:
    lpListHeader := m_lpCurrListHeader;
    {
    if (m_nFreeCount > 0) then
    begin
      lpNode := m_lpFreeNode;
      m_lpFreeNode := m_lpFreeNode^.lpNext;
      Dec(m_nFreeCount);
    end else
    begin
      if (lpListHeader^.nCurrPos < lpListHeader^.nCount) then
      begin
        lpNode := Pointer(UIntPtr(lpListHeader) + SizeOf(TSwzSingleListHeader) + (SizeOf(TSwzSingleNode) * lpListHeader^.nCurrPos));
        Inc(lpListHeader^.nCurrPos);
      end else
      begin
        //  3.添加新的节点页
        goto __ADD_PAGE;
      end;
    end;
    }

    //  1.判断是否有空闲结点
    if (lpListHeader^.nCurrPos < lpListHeader^.nCount) then
    begin
      lpNode := Pointer(UIntPtr(lpListHeader) + SizeOf(TSwzSingleListHeader) + (SizeOf(TSwzSingleNode) * lpListHeader^.nCurrPos));
      Inc(lpListHeader^.nCurrPos);
    end else
    begin
      //  2.判断是否有空闲结点
      if (m_nFreeCount > 0) then
      begin
        lpNode := m_lpFreeNode;
        m_lpFreeNode := m_lpFreeNode^.lpNext;
        Dec(m_nFreeCount);
      end else
      begin
        //  3.添加新的节点页
        goto __ADD_PAGE;
      end;
    end;

    //  Add New Node
    if (lpNode <> nil) then
    begin
      lpNode^.lpNext := m_lpCurrNode;
      lpNode^.lpData := p;

      m_lpCurrNode := lpNode;
      Inc(m_nNodeCount);
      Result := True;
    end;
  end else
  begin
__ADD_PAGE:
    if AddNewListPage(MAX_PAGE_SIZE) then
    begin
      goto __ADD_NODE;
    end;
  end;

  Leave();
end;

end.
