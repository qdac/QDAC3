unit main;

interface

{
  本示例结合了QWorker、QDigest、QRBTree、QString四个单元，构成一个比较复杂的例子。
  其中，QDigest使用了其中的MD5哈希摘要用于比较文件签名，QRBTree用了其中的哈希表来
  检测重复，QString用到其中的DecodeToken函数来分隔字符串。
  【算法说明】
  1、多磁盘并行扫描算法
  如果同时搜索多个磁盘中的重复文件，可以并行的扫描数据内容。实际在计算重复时，磁盘
  IO占据了绝大部分时间，采用此算法，理论上同样大小的N个磁盘扫描，可以降低时间到1个
  磁盘的扫描时间。本程序检测各个分区所在的物理磁盘，从而分配不同的工作者来扫描。
  2、快速哈希算法
  快速哈希算法与普通的哈希算法的不同在于它不试图扫描整个文件，而是只扫描文件的头、
  中、尾三部分的部分数据加上文件大小进行MD5哈希，从而得到一个快速计算的哈希值。
  如果他们不同，则内容肯定不一样，当然，它们相同我们也不能肯定内容就一定一样，所
  以这个只是初步筛选。对初步筛选完成的结果，我们需要进一步校验完整的哈希值，才能
  确定文件是不是真的相同。
}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, qstring, qdigest, qworker,
  qrbtree,
  Vcl.CheckLst, VirtualTrees, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons,
  Vcl.Samples.Gauges, Vcl.Menus;

type
  TDupFiles = class;
  PHashedFileInfo = ^THashedFileInfo;
  THashedFileInfoArray = array of PHashedFileInfo;
  PHashedFileInfoArray = ^THashedFileInfoArray;

  TSearchWorkerState = record
    Panel: TPanel;
    Hint: TLabel;
    Progress: TGauge;
    PathList: QStringW;
    First, Last: PHashedFileInfo;
    Total: Integer;
    Searched: Integer;
    Found: Integer;
    StartTime: Cardinal;
    StopTime: Cardinal;
  end;

  PSearchWorkerState = ^TSearchWorkerState;

  THashedFileInfo = record
    FileName: QStringW;
    MD5Hash: TQMD5Digest;
    HashCode: TQHashType;
    Size: Int64;
    DupHelper: TDupFiles;
    State: PSearchWorkerState;
    Next: PHashedFileInfo;
  end;

  TDupFiles = class
  private
    FFileInfo: THashedFileInfo;
    function GetCount: Integer;
    function GetFiles(AIndex: Integer): TDupFiles;
  protected
    FItems: TList;
    FNode: PVirtualNode;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(AInfo: PHashedFileInfo): TDupFiles;
    procedure Clear;
    property Count: Integer read GetCount;
    property Files[AIndex: Integer]: TDupFiles read GetFiles;
    property Node: PVirtualNode read FNode write FNode;
    property FileInfo: THashedFileInfo read FFileInfo;
  end;

  PDupFiles = ^TDupFiles;

  TForm6 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    vstDupFiles: TVirtualStringTree;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Label1: TLabel;
    clbDrives: TCheckListBox;
    rbByDrive: TRadioButton;
    rbByDir: TRadioButton;
    edtSearchDir: TEdit;
    btnBrowse: TButton;
    btnStart: TButton;
    pnlLog: TPanel;
    mmLogs: TMemo;
    Panel6: TPanel;
    Label2: TLabel;
    sbCloseLog: TSpeedButton;
    tmProgress: TTimer;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    lblLastResults: TLabel;
    chkByQuickHash: TCheckBox;
    sbxProgress: TScrollBox;
    PopupMenu1: TPopupMenu;
    miOpenFolder: TMenuItem;
    miOpenFile: TMenuItem;
    N1: TMenuItem;
    Label3: TLabel;
    edtMinFileSize: TEdit;
    miBinaryCmpAll: TMenuItem;
    procedure sbCloseLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure vstDupFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDupFilesInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstDupFilesInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure tmProgressTimer(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure rbByDriveClick(Sender: TObject);
    procedure clbDrivesClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure miOpenFolderClick(Sender: TObject);
    procedure miOpenFileClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure miBinaryCmpAllClick(Sender: TObject);
  private
    { Private declarations }
    FHashTable: TQHashTable;
    FDupFiles: TDupFiles;
    FPendings: Integer;
    FByQuickHash:Boolean;
    FMinFileSize:Int64;
    FWorkerStates: array of TSearchWorkerState;
    FScaningWorkers: Integer;
    procedure DoHashDelete(ATable: TQHashTable; AHash: TQHashType;
      AData: Pointer);
    procedure DoLogError(AJob: PQJob);
    procedure DoLookupFiles(AJob: PQJob);
    procedure DoFileHashDone(AJob: PQJob);
    function GetPhysicalDriveNo(APath: QStringW): Integer;
    procedure ClearProgress;
    function CanOpenFile(AFileName: QStringW): Boolean;
    procedure HashFile(var AFile: PHashedFileInfo);
    procedure DoHashFile(ALoopMgr: TQForJobs; AJob: PQJob; AIndex: NativeInt);
    function BinaryCompare(AFiles:TDupFiles):String;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses shlobj, shellapi;
{$R *.dfm}

function TForm6.BinaryCompare(AFiles: TDupFiles): String;
var
  FS1,FS2:TFileStream;
  I:Integer;
  ABuf1,ABuf2:array[0..65535] of Byte;
  AReaded1,AReaded2:Integer;
  AMsg:String;
begin
SetLength(Result,0);
FS1:=TFileStream.Create(AFiles.FileInfo.FileName,fmOpenRead or fmShareDenyWrite);
try
  for I := 0 to AFiles.Count-1 do
    begin
    FS2:=TFileStream.Create(AFiles.Files[I].FileInfo.FileName,fmOpenRead or fmShareDenyWrite);
    try
      FS1.Position:=0;
      repeat
        AReaded1:=FS1.Read(ABuf1[0],65536);
        AReaded2:=FS2.Read(ABuf2[0],65536);
        if (AReaded1<>AReaded2) or (not CompareMem(@ABuf1[0],@ABuf2[0],AReaded1)) then
          Result:=Result+'    '+FS2.FileName+' :不同'#13#10;
      until (AReaded1=0);
    finally
      FreeObject(FS2);
    end;
    end;
  if Length(Result)>0 then
    Result:=FS1.FileName+' 比较结果：'#13#10+Result;
finally
  FreeObject(FS1);
end;

end;

procedure TForm6.btnBrowseClick(Sender: TObject);
var
  APath: String;
  function BrowseForFolder(const ATitle: string): String;
  var
    Info: TBrowseInfo;
    IDList: pItemIDList;
    Buffer: PChar;
  begin
  Result := '';
  Buffer := StrAlloc(MAX_PATH);
  with Info do
    begin
    hwndOwner := Handle; { 目录对话框所属的窗口句柄 }
    pidlRoot := nil; { 起始位置，缺省为我的电脑 }
    pszDisplayName := Buffer; { 用于存放选择目录的指针 }
    lpszTitle := PChar(ATitle);
    ulFlags := BIF_RETURNONLYFSDIRS;
    lpfn := nil; { 指定回调函数指针 }
    lParam := 0; { 传递给回调函数参数 }
    IDList := SHBrowseForFolder(Info); { 读取目录信息 }
    end;
  if IDList <> nil then
    begin
    SHGetPathFromIDList(IDList, Buffer); { 将目录信息转化为路径字符串 }
    Result := strpas(Buffer);
    end;
  StrDispose(Buffer);
  end;

begin
APath := BrowseForFolder('浏览目录');
if Length(APath) > 0 then
  begin
  edtSearchDir.Text := APath;
  btnStart.Enabled := Length(APath) > 0;
  rbByDir.Checked:=True;
  end;
end;

procedure TForm6.btnStartClick(Sender: TObject);
  function CreateProgress(APath: QStringW; AIdx: Integer): PSearchWorkerState;
  begin
  FWorkerStates[AIdx].Panel := TPanel.Create(Self);
  FWorkerStates[AIdx].Panel.Parent := sbxProgress;
  FWorkerStates[AIdx].Panel.AlignWithMargins := True;
  FWorkerStates[AIdx].Panel.BevelOuter := bvNone;
  FWorkerStates[AIdx].Panel.SetBounds(0, AIdx * 70,
    sbxProgress.ClientWidth, 70);
  FWorkerStates[AIdx].Hint := TLabel.Create(FWorkerStates[AIdx].Panel);
  FWorkerStates[AIdx].Hint.AutoSize := False;
  FWorkerStates[AIdx].Hint.Parent := FWorkerStates[AIdx].Panel;
  FWorkerStates[AIdx].Hint.Align := alTop;
  FWorkerStates[AIdx].Hint.Layout := tlCenter;
  FWorkerStates[AIdx].Hint.Height := 30;
  FWorkerStates[AIdx].Progress := TGauge.Create(FWorkerStates[AIdx].Panel);
  FWorkerStates[AIdx].Progress.Parent := FWorkerStates[AIdx].Panel;
  FWorkerStates[AIdx].Progress.Margins.Bottom := 10;
  FWorkerStates[AIdx].Progress.AlignWithMargins := True;
  FWorkerStates[AIdx].Progress.Align := alBottom;
  FWorkerStates[AIdx].Progress.Height := 20;
  FWorkerStates[AIdx].Hint.Caption := '正在查找 ' + APath + ' ,已找到 0 个重复文件';
  FWorkerStates[AIdx].PathList := APath;
  FWorkerStates[AIdx].Searched := 0;
  FWorkerStates[AIdx].Found := 0;
  FWorkerStates[AIdx].StartTime := GetTickCount;
  FWorkerStates[AIdx].StopTime := 0;
  Result := @FWorkerStates[AIdx];
  end;
  procedure ScanByDrive;
  var
    I, AMaxIdx, AIdx: Integer;
    AList: array of QStringW;
    APath: String;
  begin
  AMaxIdx := 0;
  for I := 0 to clbDrives.Count - 1 do
    begin
    if clbDrives.Checked[I] then
      begin
      AIdx := Integer(clbDrives.Items.Objects[I]);
      if AIdx > AMaxIdx then
        AMaxIdx := AIdx;
      end;
    end;
  SetLength(AList, AMaxIdx + 1);
  FScaningWorkers := 0;
  for I := 0 to clbDrives.Count - 1 do
    begin
    if clbDrives.Checked[I] then
      begin
      AIdx := Integer(clbDrives.Items.Objects[I]);
      if Length(AList[AIdx]) = 0 then
        Inc(FScaningWorkers);
      AList[AIdx] := AList[AIdx] + ';' + Copy(clbDrives.Items[I], 1, 3);
      end;
    end;
  SetLength(FWorkerStates, FScaningWorkers);
  AIdx := 0;
  for I := 0 to AMaxIdx do
    begin
    if Length(AList[I]) > 0 then
      begin
      APath := Copy(AList[I], 2, MaxInt);
      Workers.Post(DoLookupFiles, CreateProgress(APath, AIdx), False);
      Inc(AIdx);
      end;
    end;
  if FScaningWorkers > 0 then
    btnStart.Caption := '停止(&S)';
  end;
  procedure CalcMinSize;
  var
    V:Int64;
    S:QStringW;
    p:PWideChar;
  begin
  S:=CNFullToHalf(edtMinFileSize.Text);
  p:=PWideChar(S);
  if ParseInt(p,V)<>0 then
    begin
    SkipSpaceW(p);
    if (p^='b') or (p^='B') then
      FMinFileSize:=V
    else if (p^='k') or (p^='K') then
      FMinFileSize:=V shl 10
    else if (p^='m') or (p^='M') then
      FMinFileSize:=V shl 20
    else if (p^='G') or (p^='g') then
      FMinFileSize:=V shl 30
    else
      FMinFileSize:=V;
    end
  else
    FMinFileSize:=0;
  end;
begin
if btnStart.Tag = 0 then
  begin
  ClearProgress;
  btnStart.Tag := 1;
  FDupFiles.Clear;
  FHashTable.Clear;
  vstDupFiles.RootNodeCount := 0;
  FPendings := 0;
  FByQuickHash:=chkByQuickHash.Checked;
  tmProgress.Enabled := True;
  CalcMinSize;
  if rbByDir.Checked then // 单一目录，只能在一个线程中查找和哈希
    begin
    if not DirectoryExists(edtSearchDir.Text) then
      begin
      application.MessageBox('要查找的目录不存在或无法访问，请检查后重新输入。', '错误',
        MB_OK or MB_ICONSTOP);
      Exit;
      end;
    SetLength(FWorkerStates, 1);
    FScaningWorkers := 1;
    Workers.Post(DoLookupFiles, CreateProgress(edtSearchDir.Text, 0), False);
    btnStart.Caption := '停止(&S)';
    end
  else
    ScanByDrive;
  end
else
  begin
  btnStart.Tag := 0;
  btnStart.Enabled := False;
  while (FScaningWorkers > 0) and (FPendings <> 0) do
    application.ProcessMessages;
  btnStart.Caption := '扫描(&S)';
  btnStart.Enabled := True;
  ClearProgress;
  end
end;

procedure TForm6.btnSelectAllClick(Sender: TObject);
begin
clbDrives.CheckAll(cbChecked);
end;

procedure TForm6.btnDeselectAllClick(Sender: TObject);
begin
clbDrives.CheckAll(cbUnchecked);
end;

function TForm6.CanOpenFile(AFileName: QStringW): Boolean;
var
  AHandle: THandle;
begin
AHandle := CreateFileW(PQCharW(AFileName), GENERIC_READ, FILE_SHARE_READ, nil,
  OPEN_EXISTING, 0, 0);
Result := AHandle <> INVALID_HANDLE_VALUE;
if Result then
  CloseHandle(AHandle);
end;

procedure TForm6.clbDrivesClick(Sender: TObject);
begin
if rbByDrive.Checked then
  rbByDriveClick(Sender);
end;

procedure TForm6.ClearProgress;
var
  I: Integer;
  AFound, ASearched: Integer;
begin
AFound := 0;
ASearched := 0;
for I := 0 to High(FWorkerStates) do
  begin
  Inc(AFound, FWorkerStates[I].Found);
  Inc(ASearched, FWorkerStates[I].Searched);
  FreeObject(FWorkerStates[I].Panel);
  end;
SetLength(FWorkerStates, 0);
tmProgress.Enabled := False;
lblLastResults.Caption := '共查找 ' + IntToStr(ASearched) + ' 个文件，其中重复 ' +
  IntToStr(AFound) + ' 个';
end;

procedure TForm6.DoHashDelete(ATable: TQHashTable; AHash: TQHashType;
  AData: Pointer);
begin
Dispose(PHashedFileInfo(AData));
end;

procedure TForm6.DoHashFile(ALoopMgr: TQForJobs; AJob: PQJob;
  AIndex: NativeInt);
var
  AList: PHashedFileInfoArray;
  AState: PSearchWorkerState;
begin
AList := AJob.Data;
AState:=AList^[AIndex].State;
HashFile(AList^[AIndex]);
AList^[AIndex] := nil;
AtomicIncrement(AState.Searched);
if (application.Terminated) or (btnStart.Tag <> 1) then
  ALoopMgr.BreakIt;
end;

procedure TForm6.DoFileHashDone(AJob: PQJob);
var
  AList: PQHashList;
  AData, AExist: PHashedFileInfo;

  procedure AddDupFile;
  begin
  Inc(AData.State.Found);
  if not Assigned(AExist.DupHelper) then
    AExist.DupHelper := FDupFiles.Add(AExist);
  AExist.DupHelper.Add(AData);
  if Assigned(AExist.DupHelper.Node) then
    vstDupFiles.ReinitNode(AExist.DupHelper.Node, False);
  Dispose(AData);
  end;

begin
application.ProcessMessages;
AData := AJob.Data;
AtomicDecrement(FPendings);
if not application.Terminated then
  begin
  AList := FHashTable.FindFirst(AData.HashCode);
  while AList <> nil do
    begin
    AExist := AList.Data;
    if (AData.Size = AExist.Size) and CompareMem(@AExist.MD5Hash,
      @AData.MD5Hash, SizeOf(TQMD5Digest)) then
      begin
      AddDupFile;
      Exit;
      end;
    AList := FHashTable.FindNext(AList);
    end;
  FHashTable.Add(AData, AData.HashCode);
  end
else
  Dispose(AData);
end;

procedure TForm6.DoLogError(AJob: PQJob);
begin
mmLogs.Lines.Add(AJob.ExtData.AsString);
end;

procedure TForm6.DoLookupFiles(AJob: PQJob);
  function BreakScan: Boolean;
  begin
  Result := application.Terminated or (btnStart.Tag <> 1);
  end;
  procedure ScanFile(APath: QStringW);
  var
    wfd: WIN32_FIND_DATAW;
    hFind: THandle;
    AHash: PHashedFileInfo;
    AFilter: QStringW;
    ASize:Int64;
  begin
  if Copy(APath, Length(APath), 1) <> '\' then
    APath := APath + '\';
  AFilter := APath + '*.*';
  hFind := FindFirstFileW(PQCharW(AFilter), wfd);
  if hFind <> INVALID_HANDLE_VALUE then
    begin
    repeat
      if (wfd.cFileName[0] = '.') and
        ((wfd.cFileName[1] = #0) or ((wfd.cFileName[1] = '.') and
        (wfd.cFileName[2] = #0))) then
        Continue
      else
        begin
        if (wfd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then // 目录
          ScanFile(APath + wfd.cFileName)
        else if (wfd.dwFileAttributes and (FILE_ATTRIBUTE_TEMPORARY or
          FILE_ATTRIBUTE_REPARSE_POINT)) = 0 then // 不是符号链接和临时文件
          begin
          ASize:=Int64(wfd.nFileSizeHigh) shl 32 + wfd.nFileSizeLow;
          if ASize>=FMinFileSize then
            begin
            New(AHash);
            AHash.FileName := APath + wfd.cFileName;
            FillChar(AHash.MD5Hash, SizeOf(TQMD5Digest), 0);
            AHash.HashCode := 0;
            AHash.Size :=ASize;
            AHash.DupHelper := nil;
            AHash.State := AJob.Data;
            AHash.Next := nil;
            if AHash.State.Last <> nil then
              AHash.State.Last.Next := AHash
            else
              AHash.State.First := AHash;
            AHash.State.Last := AHash;
            Inc(AHash.State.Total);
            end;
          end;
        end;
    until (not FindNextFileW(hFind, wfd)) or BreakScan;
    end;
  end;
  procedure ScanPathList;
  var
    APathList: QStringW;
    APath: QStringW;
    AProg: PSearchWorkerState;
    AItem: PHashedFileInfo;
    AList: THashedFileInfoArray;
    AIdx: Integer;
    I: Integer;
  begin
  AProg := AJob.Data;
  APathList := AProg.PathList;
  while (Length(APathList) > 0) and (not BreakScan) do
    begin
    APath := DecodeTokenW(APathList, ';', #0, False, True);
    if Length(APath) > 0 then
      ScanFile(APath);
    end;
    //下面是单一线程的代码
//    while (AProg.First<>nil) and (not BreakScan) do
//      begin
//      AItem:=AProg.First.Next;
//      HashFile(AProg.First);
//      AProg.First:=AItem;
//      AtomicIncrement(AProg.Searched);
//      end;
//    while AProg.First<>nil do
//      begin
//      AItem:=AProg.First.Next;
//      Dispose(AProg.First);
//      AProg.First:=AItem;
//      end;
  // 多线程哈希代码
  AItem := AProg.First;
  SetLength(AList, AProg.Total);
  AIdx := 0;
  while (not BreakScan) and (AItem <> nil) do
    begin
    AList[AIdx] := AItem;
    AItem := AItem.Next;
    Inc(AIdx);
    end;
  if not BreakScan then
    Workers.&For(0, AProg.Total - 1, DoHashFile, False, @AList);
  for I := 0 to AProg.Total - 1 do
    begin
    if Assigned(AList[I]) then
      Dispose(AList[I]);
    end;
  AProg.StopTime := GetTickCount;
  end;

begin
ScanPathList;
AtomicDecrement(FScaningWorkers);
end;

procedure TForm6.FormCreate(Sender: TObject);
var
  ASize: Cardinal;
  S, ADrive: QStringW;
  p, pe: PQCharW;
  ADrvNo: Integer;
begin
FHashTable := TQHashTable.Create();
FHashTable.AutoSize := True;
FHashTable.OnDelete := DoHashDelete;
FDupFiles := TDupFiles.Create;
vstDupFiles.NodeDataSize := SizeOf(Pointer);
ASize := GetLogicalDriveStringsW(0, nil);
if ASize <> 0 then
  begin
  SetLength(S, ASize);
  GetLogicalDriveStringsW(ASize, PQCharW(S));
  p := PQCharW(S);
  while p^ <> #0 do
    begin
    pe := p;
    while pe^ <> #0 do
      Inc(pe);
    ADrive := StrDupX(p, pe - p);
    if (GetDriveTypeW(PQCharW(ADrive)) in [DRIVE_FIXED, DRIVE_REMOVABLE]) then
      begin
      ADrvNo := GetPhysicalDriveNo(ADrive);
      if ADrvNo <> -1 then
        clbDrives.AddItem(ADrive + '(磁盘' + IntToStr(ADrvNo) + ')',
          Pointer(ADrvNo));
      end;
    Inc(pe);
    p := pe;
    end;
  clbDrives.CheckAll(cbChecked);
  end;
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
FreeObject(FHashTable);
FreeObject(FDupFiles);
end;

type
  STORAGE_DEVICE_NUMBER = record
    DeviceType: Word;
    DeviceNumber: Cardinal;
    PartitionNumber: ULONG;
  end;
  /// <summary>获取物理路径对应的磁盘序号</summary>
  /// <param name="APath">目录名，只支持绝对路径名，如"C:\temp"，不能是相对路径</param>
  /// <returns>返回当前目录对应的磁盘序号，-1代表未找到。</returns>

function TForm6.GetPhysicalDriveNo(APath: QStringW): Integer;
var
  hDev: THandle;
  AResult: Boolean;
  AReaded: Cardinal;
  ANumber: STORAGE_DEVICE_NUMBER;
begin
APath := '\\.\' + Copy(APath, 1, 2);
Result := -1;
hDev := CreateFileW(PQCharW(APath), FILE_READ_ATTRIBUTES or SYNCHRONIZE or
  FILE_TRAVERSE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
if hDev <> INVALID_HANDLE_VALUE then
  begin
  AResult := DeviceIoControl(hDev, IOCTL_STORAGE_GET_DEVICE_NUMBER, nil, 0,
    @ANumber, SizeOf(ANumber), AReaded, nil);
  if AResult then
    Result := ANumber.DeviceNumber;
  CloseHandle(hDev);
  end;
end;

procedure TForm6.HashFile(var AFile: PHashedFileInfo);
begin
if CanOpenFile(AFile.FileName) then
  begin
  if FByQuickHash then
    AFile.MD5Hash := MD5QuickHash(AFile.FileName)
  else
    AFile.MD5Hash:=MD5File(AFile.FileName);
  AFile.HashCode := HashOf(@AFile.MD5Hash,
    SizeOf(TQMD5Digest));
  AtomicIncrement(FPendings);
  Workers.Post(DoFileHashDone, AFile, True);
  end
else
  begin
  Workers.Post(DoLogError, TQJobExtData.Create('文件 ' + AFile.FileName +
    ' 无法访问。'), True, jdfFreeAsObject);
  Dispose(AFile);
  end;
end;

procedure TForm6.miBinaryCmpAllClick(Sender: TObject);
var
  I: Integer;
  S:String;
begin
SetLength(S,0);
for I := 0 to FDupFiles.Count-1 do
  begin
  S:=S+BinaryCompare(FDupFiles.Files[I]);
  end;
if Length(S)>0 then
  ShowMessage(S)
else
  ShowMessage('全部文件二进制比较结果与当前结果一致。');
end;

procedure TForm6.miOpenFileClick(Sender: TObject);
var
  AData:PDupFiles;
begin
AData:=vstDupFiles.GetNodeData(vstDupFiles.FocusedNode);
ShellExecuteW(Handle,nil,PWideChar(AData.FileInfo.FileName),nil,nil,SW_SHOWNORMAL);
end;

procedure TForm6.miOpenFolderClick(Sender: TObject);
var
  AData:PDupFiles;
begin
AData:=vstDupFiles.GetNodeData(vstDupFiles.FocusedNode);
ShellExecuteW(Handle,nil,PWideChar(ExtractFilePath(AData.FileInfo.FileName)),nil,nil,SW_SHOWNORMAL);
end;

procedure TForm6.N1Click(Sender: TObject);
var
  FS1,FS2:TFileStream;
  AData:PDupFiles;
  I:Integer;
  ABuf1,ABuf2:array[0..65535] of Byte;
  AReaded1,AReaded2:Integer;
  AMsg:String;
begin
AData:=vstDupFiles.GetNodeData(vstDupFiles.FocusedNode);
AMsg:=BinaryCompare(AData^);
if Length(AMsg)=0 then
  AMsg:=AData.FileInfo.FileName+' 比较结果：文件全部相同';
ShowMessage(AMsg);
end;

procedure TForm6.PopupMenu1Popup(Sender: TObject);
begin
if vstDupFiles.FocusedNode=nil then
  begin
  miOpenFolder.Visible:=False;
  miOpenFile.Visible:=False;
  end
else
  begin
  miOpenFolder.Visible:=True;
  miOpenFile.Visible:=True;
  end;
end;

procedure TForm6.rbByDriveClick(Sender: TObject);
var
  I: Integer;
begin
if rbByDir.Checked then
  btnStart.Enabled := Length(edtSearchDir.Text) > 0
else
  begin
  for I := 0 to clbDrives.Count - 1 do
    begin
    if clbDrives.Checked[I] then
      begin
      btnStart.Enabled := True;
      Exit;
      end;
    end;
  btnStart.Enabled := False;
  end;
end;

procedure TForm6.sbCloseLogClick(Sender: TObject);
begin
pnlLog.Visible := False;
end;

procedure TForm6.tmProgressTimer(Sender: TObject);
var
  I: Integer;
  ASpeed: Integer;
  ADelta: Cardinal;
begin
for I := 0 to High(FWorkerStates) do
  begin
  if FWorkerStates[I].Total > 0 then
    begin
    if FWorkerStates[I].StopTime <> 0 then
      ADelta := FWorkerStates[I].StopTime - FWorkerStates[I].StartTime
    else
      ADelta := GetTickCount - FWorkerStates[I].StartTime;
    if ADelta <> 0 then
      ASpeed := FWorkerStates[I].Searched * 1000 div ADelta
    else
      ASpeed := 0;
    FWorkerStates[I].Hint.Caption := '正在查找 ' + FWorkerStates[I].PathList +
      '(已完成' + FormatFloat('0.##', FWorkerStates[I].Searched * 100 /
      FWorkerStates[I].Total) + '%)' + ' 总数:' + IntToStr(FWorkerStates[I].Total)
      + ' 已查找:' + IntToStr(FWorkerStates[I].Searched) + ' 已发现:' +
      IntToStr(FWorkerStates[I].Found) + ' 速度:' + IntToStr(ASpeed) + '/秒';
    FWorkerStates[I].Progress.Progress := FWorkerStates[I].Searched *
      100 div FWorkerStates[I].Total;
    end;
  end;
if (FScaningWorkers = 0) and (btnStart.Tag <> 0) and (FPendings = 0) then
  begin
  btnStart.Caption := '扫描(&S)';
  btnStart.Enabled := True;
  btnStart.Tag := 0;
  ClearProgress;
  end;
vstDupFiles.RootNodeCount := FDupFiles.Count;
end;

procedure TForm6.vstDupFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  AData: PDupFiles;
begin
AData := Sender.GetNodeData(Node);
if AData^ <> nil then
  CellText := AData.FileInfo.FileName + '(' +
    RollupSize(AData.FileInfo.Size) + '),重复'+IntToStr(AData.Count)+'次';
end;

procedure TForm6.vstDupFilesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  AData: PDupFiles;
begin
AData := Sender.GetNodeData(Node);
ChildCount := AData.Count;
end;

procedure TForm6.vstDupFilesInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  AData, APData: PDupFiles;
begin
AData := Sender.GetNodeData(Node);
APData := Sender.GetNodeData(ParentNode);
if APData <> nil then
  AData^ := APData.Files[Node.Index]
else
  AData^ := FDupFiles.Files[Node.Index];
if AData.Count > 0 then
  InitialStates := InitialStates + [ivsHasChildren];
AData.Node := Node;
end;

{ TDupFiles }

function TDupFiles.Add(AInfo: PHashedFileInfo): TDupFiles;
begin
if not Assigned(FItems) then
  FItems := TList.Create;
Result := TDupFiles.Create;
Result.FFileInfo := AInfo^;
FItems.Add(Result);
end;

procedure TDupFiles.Clear;
var
  I: Integer;
begin
if Assigned(FItems) then
  begin
  for I := FItems.Count - 1 downto 0 do
    begin
    FreeObject(Files[I]);
    FItems.Delete(I);
    end;
  FItems.Clear;
  end;
end;

constructor TDupFiles.Create;
begin
inherited;

end;

destructor TDupFiles.Destroy;
begin
if Assigned(FItems) then
  begin
  Clear;
  FreeObject(FItems);
  end;
inherited;
end;

function TDupFiles.GetCount: Integer;
begin
if Assigned(FItems) then
  Result := FItems.Count
else
  Result := 0;
end;

function TDupFiles.GetFiles(AIndex: Integer): TDupFiles;
begin
Result := FItems[AIndex];
end;

end.
