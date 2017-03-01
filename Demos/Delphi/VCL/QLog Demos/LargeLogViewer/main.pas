unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees,
  Vcl.ExtCtrls, Winapi.Shellapi, Winapi.ShlObj, System.Generics.Collections,
  Vcl.ComCtrls, System.Actions, Vcl.ActnList, Vcl.StdActns, qworker, qstring,
  qmacros, qdigest, qrbtree, logextract, System.RegularExpressionsConsts,
  Registry,
  System.RegularExpressionsCore, Vcl.Imaging.pngimage, Vcl.Buttons;

type
  TTextLine = record
    Offset: Int64;
    // Index:Cardinal;//行索引
    Count: Word; // 一行不超过64KB
  end;

  PTextLine = ^TTextLine;

  TLineWithText = record
    Line: TTextLine;
    Text: String;
  end;

  PLineWithText = ^TLineWithText;

  TfrmLogViewer = class(TForm)
    Panel1: TPanel;
    vstLogs: TVirtualStringTree;
    pnlButtons: TPanel;
    Label1: TLabel;
    edtLogFile: TEdit;
    btnBrowseLog: TButton;
    Splitter1: TSplitter;
    ActionList1: TActionList;
    actOpenLog: TFileOpen;
    Panel3: TPanel;
    mmLineText: TMemo;
    pbProgress: TProgressBar;
    pnlEncoding: TPanel;
    rbUTF8Encoding: TRadioButton;
    rbUTF16LE: TRadioButton;
    RadioButton2: TRadioButton;
    tbAnsiEncoding: TRadioButton;
    rbAutoEncoding: TRadioButton;
    pnlSearch: TPanel;
    Panel5: TPanel;
    Label2: TLabel;
    edtSearchText: TEdit;
    chkByRegex: TCheckBox;
    chkIgnoreCase: TCheckBox;
    btnBack: TButton;
    btnForward: TButton;
    btnSearchAll: TButton;
    sbCloseSearch: TSpeedButton;
    sbAbout: TSpeedButton;
    sbCharset: TSpeedButton;
    sbExtract: TSpeedButton;
    sbSearch: TSpeedButton;
    Panel4: TPanel;
    sbCloseEncoding: TSpeedButton;
    sbLinkLog: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure actOpenLogAccept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstLogsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormDestroy(Sender: TObject);
    procedure vstLogsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure btnBackClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnSearchAllClick(Sender: TObject);
    procedure sbExtractClick(Sender: TObject);
    procedure sbAboutClick(Sender: TObject);
    procedure sbCloseSearchClick(Sender: TObject);
    procedure sbCloseEncodingClick(Sender: TObject);
    procedure sbSearchClick(Sender: TObject);
    procedure sbCharsetClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbLinkLogClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    FStream: TFileStream;
    FIndexStream: TFileStream;
    FFileName, FIndexFile: String;
    FCached: array [0 .. 1024] of TLineWithText;
    FCachedStart, FCachedLines, FTotalLines: Cardinal;
    FEncoding: TTextEncoding;
    FLastProgressUpdateTime: Cardinal;
    FDeleteFileBeforeClose: Boolean;
    FVSTWndProc: TWndMethod;
    procedure DoVstWndProc(var AMsg: TMessage);
    procedure ClearLogs;
    procedure DoLoadLogs(AJob: PQJob);
    procedure CacheLines(AStartIndex: Integer);
    procedure DoSearch(AForward: Boolean; ASearchAll: Boolean); overload;
    procedure DoSearch(const AToSearch, AToReplace: String;
      AForward, ASearchAll, ADoReplace, AMergeDuplicaties: Boolean); overload;
    function DoCompareGuid(P1, P2: Pointer): Integer;
    procedure DoDeleteGuid(ASender: TQRBTree; ANode: TQRBNode);
  public
    { Public declarations }
    procedure OpenLog;
    property DeleteFileBeforeClose: Boolean read FDeleteFileBeforeClose
      write FDeleteFileBeforeClose;
  end;

var
  frmLogViewer: TfrmLogViewer;

implementation

uses about, sftpbrowser;
{$R *.dfm}
{ 对于大型的日志文件，由于其内容无法全部放入内存，所以我们在内存中，只创建其有限的行的副本，
  其它的行仍存在于文件中，按需加载
}

function IsAdministrator: Boolean;
var
  RelativeGroupID: DWORD;
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  ov: OSVERSIONINFO;
  NtAuthority: SID_IDENTIFIER_AUTHORITY;
  I: Integer;
const
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
begin
  RelativeGroupID := DOMAIN_ALIAS_RID_ADMINS;
  NtAuthority := SECURITY_NT_AUTHORITY;
  ov.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
  GetVersionEx(&ov);
  if ov.dwPlatformId <> VER_PLATFORM_WIN32_NT then
    Exit(true);
  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := false;
  try
    Token := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, true, Token);
    if (not HaveToken) and (GetLastError() = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      if not AllocateAndInitializeSid(NtAuthority, 2,
        SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID, 0, 0, 0, 0, 0, 0,
        psidAdmin) then
        Exit(false);
    end
    else
    begin
      Exit(false);
    end;
    if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
      (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
      Exit(false);
    TokenInfo := AllocMem(Count);
    if not GetTokenInformation(Token, TokenGroups, TokenInfo, Count, &Count)
    then
      Exit(false);
    for I := 0 to TokenInfo.GroupCount - 1 do
    begin
      if EqualSid(psidAdmin, TokenInfo.Groups[I].Sid) then
      begin
        // consider denied ACE with Administrator SID
        Exit((TokenInfo.Groups[I].Attributes and SE_GROUP_USE_FOR_DENY_ONLY) <>
          SE_GROUP_USE_FOR_DENY_ONLY);
      end
    end
  finally
    if Assigned(TokenInfo) then
      FreeMemory(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if Assigned(psidAdmin) then
      FreeSid(psidAdmin);
  end;
  Result := false;
end;

procedure TfrmLogViewer.actOpenLogAccept(Sender: TObject);
begin
  edtLogFile.Text := actOpenLog.Dialog.FileName;
  FFileName := actOpenLog.Dialog.FileName;
  if FileExists(edtLogFile.Text) then
  begin
    Workers.Clear(); // 清掉正在执行的作业
    OpenLog;
  end;
end;

procedure TfrmLogViewer.btnSearchAllClick(Sender: TObject);
begin
  DoSearch(true, true);
end;

procedure TfrmLogViewer.btnBackClick(Sender: TObject);
begin
  if Assigned(vstLogs.FocusedNode) then
    DoSearch(false, false)
  else
    DoSearch(false, false);
end;

procedure TfrmLogViewer.btnForwardClick(Sender: TObject);
begin
  if Assigned(vstLogs.FocusedNode) then
    DoSearch(true, false)
  else
    DoSearch(true, false);
end;

procedure TfrmLogViewer.CacheLines(AStartIndex: Integer);
var
  I: Integer;
  APos, ASize: Int64;
  AIndexes: array of TTextLine;
  ATemp: TFileStream;
  ABuf: TBytes;
begin
  // 避免上翻时需要重新缓存，此处可以很大的优化余地
  if AStartIndex > 500 then
    Dec(AStartIndex, 500);
  APos := AStartIndex * sizeof(TTextLine);
  if FIndexStream.Size > APos then
  begin
    FIndexStream.Position := APos;
    ASize := FIndexStream.Size;
    if APos + 1024 * sizeof(TTextLine) < ASize then
      SetLength(AIndexes, 1024)
    else
      SetLength(AIndexes, (ASize - APos) div sizeof(TTextLine));
    FIndexStream.ReadBuffer(AIndexes[0], Length(AIndexes) * sizeof(TTextLine));
    SetLength(ABuf, 65536);
    FCachedStart := AStartIndex;
    ATemp := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      for I := 0 to High(AIndexes) do
      begin
        ATemp.Position := AIndexes[I].Offset;
        ATemp.ReadBuffer(ABuf[0], AIndexes[I].Count);
        FCached[I].Line := AIndexes[I];
        FCached[I].Text := DecodeText(@ABuf[0], AIndexes[I].Count, FEncoding);
      end;
      FCachedLines := Length(AIndexes);
      FCachedStart := AStartIndex;
    finally
      FreeAndNil(ATemp);
    end;
  end;
end;

procedure TfrmLogViewer.ClearLogs;
begin
  vstLogs.RootNodeCount := 0;
  FCachedStart := 0;
  FCachedLines := 0;
end;

function TfrmLogViewer.DoCompareGuid(P1, P2: Pointer): Integer;
begin
  Result := BinaryCmp(P1, P2, sizeof(TGUID));
end;

procedure TfrmLogViewer.DoDeleteGuid(ASender: TQRBTree; ANode: TQRBNode);
begin
  Dispose(PGuid(ANode.Data));
end;

procedure TfrmLogViewer.DoLoadLogs(AJob: PQJob);
var
  ABuf: TBytes;
  AReaded: Integer;
  ABomExists: Boolean;
  AOffset: Int64;
  AIsEof: Boolean;
  ATempLines: array of TLineWithText;
  ALineCount, ABufOffset: Integer;
  AStartTime: Cardinal;
  AEncoding: TTextEncoding;
  function DecodeLine: Boolean;
  var
    pw: PQCharW;
    pa: PQCharA absolute pw;
{$IFDEF DEBUG}
    pt: PAnsiChar absolute pw;
{$ENDIF}
    ps: Pointer;
    O: Integer;
    ANL, ACR: QCharW;
    ALine: PLineWithText;
  begin
    Result := false;
    ALine := @ATempLines[ALineCount];
    ALine.Line.Offset := AOffset;
    ALine.Line.Count := 0;
    if AOffset >= 968722719 then
      DebugBreak;
    O := ABufOffset;
    ps := @ABuf[ABufOffset];
    if AEncoding in [teUnicode16LE, teUnicode16BE] then
    begin
      if AEncoding = teUnicode16LE then
      begin
        ANL := #$A;
        ACR := #$D;
      end
      else
      begin
        ANL := #$0A00;
        ACR := #$0D00;
      end;
      pw := ps;
      while O < AReaded do
      begin
        if pw^ = ANL then
        begin
          ALine.Line.Count := O - ABufOffset;
          if pw[-1] = ACR then
            Dec(ALine.Line.Count, 2);
          if ALine.Line.Count > 0 then
            ALine.Text := DecodeText(ps, ALine.Line.Count, AEncoding)
          else
            ALine.Text := '';
          Inc(O, 2);
          Inc(AOffset, O - ABufOffset);
          ABufOffset := O;
          Result := true;
          Break;
        end;
        Inc(pw);
        Inc(O, 2);
      end;
      if (not Result) and AIsEof and (O = AReaded) then
      begin
        ALine.Line.Count := O - ABufOffset;
        ALine.Text := DecodeText(ps, ALine.Line.Count);
        Result := true;
        Inc(AOffset, O - ABufOffset);
        ABufOffset := AReaded;
      end;
    end
    else // ANSI/UTF8
    begin
      pa := ps;
      while O < AReaded do
      begin
        if pa^ = 10 then
        begin
          ALine.Line.Count := O - ABufOffset;
          if PQCharA(IntPtr(pa) - 1)^ = 13 then
            Dec(ALine.Line.Count);
          if ALine.Line.Count > 0 then
            ALine.Text := DecodeText(ps, ALine.Line.Count, AEncoding)
          else
            ALine.Text := '';
          Inc(O);
          Inc(AOffset, O - ABufOffset);
          ABufOffset := O;
          Result := true;
          Break;
        end;
        Inc(pa);
        Inc(O);
      end;
      // 已经到结尾，则
      if (not Result) and AIsEof and (O = AReaded) then
      begin
        ALine.Line.Count := O - ABufOffset;
        ALine.Text := DecodeText(PQCharA(ps), ALine.Line.Count, AEncoding);
        Result := true;
        Inc(AOffset, O - ABufOffset);
        ABufOffset := AReaded;
      end;
    end;
    if not Result then
    begin
      if ABufOffset = 0 then // 行太长
        ABufOffset := -1
      else // 剩下的内容不足一行，待续读下一行
      begin
        O := AReaded - ABufOffset;
        Move(ABuf[ABufOffset], ABuf[0], O);
        ABufOffset := O;
      end;
    end
    else
    begin
      Inc(ALineCount);
    end;
  end;

begin
  SetLength(ABuf, 65536);
  ABufOffset := 0;
  FTotalLines := 0;
  SetLength(ATempLines, 1024); // 每1024行提交到主线程更新一次
  FStream.Position := 0;
  FLastProgressUpdateTime := GetTickCount;
  AStartTime := FLastProgressUpdateTime;
  AReaded := FStream.Read(ABuf[0], 65536);
  if FEncoding in [TTextEncoding.teUnknown, TTextEncoding.teAuto] then
  begin
    AEncoding := DetectTextEncoding(@ABuf[0], AReaded, ABomExists);
    if ABomExists then
    begin
      if AEncoding = teUtf8 then
        ABufOffset := 3
      else
        ABufOffset := 2;
    end;
  end
  else
    AEncoding := FEncoding;
  AOffset := ABufOffset;
  while not AJob.IsTerminated do
  begin
    ALineCount := 0;
    AIsEof := AReaded < 65536;
    while (ABufOffset < AReaded) and (ALineCount < 1024) and DecodeLine do;
    if ALineCount > 0 then
    begin
      Inc(FTotalLines, ALineCount);
      RunInMainThread(
        procedure
        var
          I: Integer;
          AIdxBuf: array of TTextLine;
          T: Cardinal;
        begin
          T := GetTickCount;
          I := 0;
          while (FCachedLines < 1024) and (I < ALineCount) do
          begin
            FCached[FCachedLines] := ATempLines[I];
            Inc(I);
            Inc(FCachedLines);
          end;
          SetLength(AIdxBuf, ALineCount);
          for I := 0 to ALineCount - 1 do
            AIdxBuf[I] := ATempLines[I].Line;
          FIndexStream.Seek(0, TSeekOrigin.soEnd);
          FIndexStream.WriteBuffer(AIdxBuf[0], ALineCount * sizeof(TTextLine));
          if (FStream.Size > 0) and (T - FLastProgressUpdateTime > 500) then
          begin
            pbProgress.Position := AOffset * 100 div FStream.Size;
            FLastProgressUpdateTime := T;
            if vstLogs.RootNodeCount < FTotalLines then
              vstLogs.RootNodeCount := vstLogs.RootNodeCount + 10000;
            Caption := '已加载 ' + IntToStr(FTotalLines) + ' 行 ' +
              RollupSize(AOffset);
          end;
        end);
      if AIsEof then
        Break;
      if ABufOffset = -1 then // 出错，行太长
      begin
        RunInMainThread(
          procedure
          begin
            vstLogs.RootNodeCount := FTotalLines;
            Application.MessageBox('日志中行太长（大于65536字节）不受支持。', '错误',
              MB_OK or MB_ICONSTOP);
          end);
        Break;
      end
      else if ALineCount < 1024 then
      begin
        ABufOffset := ABufOffset and $FFFF;
        AReaded := ABufOffset + FStream.
          Read(ABuf[ABufOffset], 65536 - ABufOffset);
        ABufOffset := 0;
      end
      else
        DebugOut('Repeat for 1024 Line');
    end
    else
      Break;
  end;
  RunInMainThread(
    procedure
    var
      S: String;
    begin
      vstLogs.RootNodeCount := FTotalLines;
      AStartTime := GetTickCount - AStartTime;
      S := '日志共 ' + IntToStr(FTotalLines) + ' 行 ' + RollupSize(AOffset) + ' 用时:'
        + RollupTime(AStartTime div 1000, true);
      if (AStartTime mod 1000) <> 0 then
        S := S + IntToStr(AStartTime mod 1000) + '毫秒';
      Caption := S;
      pbProgress.Visible := false;
    end);
end;

procedure TfrmLogViewer.DoSearch(const AToSearch, AToReplace: String;
AForward, ASearchAll, ADoReplace, AMergeDuplicaties: Boolean);
type
  TNodeMoveProc = function(Node: PVirtualNode): PVirtualNode of object;
var
  S: String;
  ARegex: TPerlRegex;
  ANode: PVirtualNode;
  ADoMove: TNodeMoveProc;
  AIgnoreCase: Boolean;
  AFound: Boolean;
  AResultFile: String;
  AResultStream: TStream;
  AUTF8Str: QStringA;
  ALastTime: Cardinal;
  AMacros: TQMacroManager;
  AHashList: TQRBTree;
  AHash: PGuid;
  ASP: Integer;
const
  AUTF8Bom: array [0 .. 2] of Byte = ($EF, $BB, $BF);
begin
  if Length(AToSearch) = 0 then
  begin
    Application.MessageBox('请输入要检索的内容关键词！', '格式错误', MB_OK or MB_ICONSTOP);
    Exit;
  end;
  AIgnoreCase := chkIgnoreCase.Checked;
  if chkByRegex.Checked or ADoReplace then
  begin
    ARegex := TPerlRegex.Create;
    ARegex.RegEx := AToSearch;
    if AIgnoreCase then
      ARegex.Options := [preCaseLess, preSingleLine]
    else
      ARegex.Options := [preSingleLine];
    ARegex.Compile;
    if not ARegex.Compiled then
    begin
      FreeAndNil(ARegex);
      Application.MessageBox('不支持的正则表达式，无法按正则进行内容检索', '格式错误',
        MB_OK or MB_ICONSTOP);
      Exit;
    end;
  end
  else
    ARegex := nil;
  AHash := nil;
  if ADoReplace then
  begin
    if AMergeDuplicaties then
    begin
      AHashList := TQRBTree.Create(DoCompareGuid);
      AHashList.OnDelete := DoDeleteGuid;
    end
    else
      AHashList := nil;
    AMacros := TQMacroManager.Create;
    AMacros.SetMacroMissed(
      procedure(ASender: TQMacroManager; AName: QStringW; const AQuoter: QCharW;
        var AHandled: Boolean)
      var
        I: Integer;
      begin
        if TryStrToInt(AName, I) then
        begin
          if I = 0 then
            ASender.Push(AName, S)
          else if I <= ARegex.GroupCount then
            ASender.Push(AName, ARegex.Groups[I])
          else
            ASender.Push(AName, '');
        end
        else
        begin
          I := ARegex.NamedGroup(AName);
          if I <> -1 then
            ASender.Push(AName, ARegex.Groups[I])
          else
            ASender.Push(AName, '');
        end;
        AHandled := true;
      end);
  end
  else
  begin
    AMacros := nil;
    AHashList := nil;
  end;
  pbProgress.Position := 0;
  pbProgress.Visible := true;
  if ASearchAll then
  begin
    ANode := vstLogs.GetFirst();
    ADoMove := vstLogs.GetNextSibling;
  end
  else
  begin
    if AForward then
    begin
      ANode := vstLogs.FocusedNode;
      ADoMove := vstLogs.GetNextSibling;
      if Assigned(ANode) then
        ANode := ADoMove(ANode)
      else
        ANode := vstLogs.GetFirst;
    end
    else
    begin
      ANode := vstLogs.FocusedNode;
      ADoMove := vstLogs.GetPreviousSibling;
      if Assigned(ANode) then
        ANode := ADoMove(ANode)
      else
        ANode := vstLogs.GetLast;
    end;
  end;
  //
  if ASearchAll then
  begin
    SetLength(AResultFile, MAX_PATH);
    if ADoReplace then
      GetTempFileName(PChar(ExtractFilePath(Application.ExeName)), 'rep_', 0,
        PChar(AResultFile))
    else
      GetTempFileName(PChar(ExtractFilePath(Application.ExeName)), 'sr', 0,
        PChar(AResultFile));
    AResultFile := PChar(AResultFile);
    AResultStream := TFileStream.Create(AResultFile, fmCreate);
    // 写入BOM，使用UTF8编码
    AResultStream.WriteBuffer(AUTF8Bom, 3);
  end
  else
    AResultStream := nil;
  try
    ALastTime := GetTickCount;
    while Assigned(ANode) and (not Application.Terminated) do
    begin
      S := vstLogs.Text[ANode, 1];
      if Assigned(ARegex) then
      begin
        ARegex.Subject := S;
        AFound := ARegex.Match;
      end
      else if AIgnoreCase then
        AFound := StrIStrW(PQCharW(S), PQCharW(AToSearch)) <> nil
      else
        AFound := StrStrW(PQCharW(S), PQCharW(AToSearch)) <> nil;
      if AFound then
      begin
        if ASearchAll then
        begin
          if Assigned(AMacros) then
          begin
            ASP := AMacros.SavePoint;
            S := AMacros.Replace(AToReplace, '$', '',
              MRF_END_WITH_INVALID_CHAR or MRF_ENABLE_ESCAPE or
              MRF_IN_DBL_QUOTER or MRF_IN_SINGLE_QUOTER);
            AMacros.Restore(ASP);
          end;
          if Assigned(AHashList) then
          begin
            if not Assigned(AHash) then
              New(AHash);
            AHash^ := MD5Hash(S).Id;
            if AHashList.Insert(AHash) then
            begin
              AHash := nil;
              AUTF8Str := qstring.UTF8Encode(S + SLineBreak);
              AResultStream.WriteBuffer(PQCharA(AUTF8Str)^, AUTF8Str.Length);
            end;
          end
          else
          begin
            AUTF8Str := qstring.UTF8Encode(S + SLineBreak);
            AResultStream.WriteBuffer(PQCharA(AUTF8Str)^, AUTF8Str.Length);
          end;
        end
        else
        begin
          vstLogs.ClearSelection;
          vstLogs.FocusedNode := ANode;
          vstLogs.Selected[ANode] := true;
          vstLogs.SetFocus;
          Break;
        end;
      end;
      if GetTickCount - ALastTime > 500 then
      begin
        pbProgress.Position := ANode.Index * 100 div vstLogs.RootNodeCount;
        Application.ProcessMessages;
      end;
      ANode := ADoMove(ANode);
    end;
  finally
    if Assigned(ARegex) then
      FreeAndNil(ARegex);
    if Assigned(AMacros) then
      FreeAndNil(AMacros);
    if Assigned(AResultStream) then
    begin
      FreeAndNil(AResultStream);
      S := '/f ' + QuotedStrW(AResultFile, '"');
      if not ADoReplace then
        S := S + ' /del';
      ShellExecute(0, nil, PChar(QuotedStrW(Application.ExeName, '"')),
        PChar(S), nil, SW_SHOWNORMAL);
    end;
    pbProgress.Visible := false;
    if Assigned(AHashList) then
      FreeAndNil(AHashList);
  end;
end;

procedure TfrmLogViewer.DoSearch(AForward, ASearchAll: Boolean);
begin
  DoSearch(edtSearchText.Text, '', AForward, ASearchAll, false, false);
end;

procedure TfrmLogViewer.DoVstWndProc(var AMsg: TMessage);
  procedure DoDragFile;
  var
    ADropMsg: TWMDropFiles absolute AMsg;
    ACount: Integer;
    AFileName: String;
  begin
    ACount := DragQueryFile(ADropMsg.Drop, $FFFFFFFF, nil, 0);
    if ACount > 0 then
    begin
      SetLength(AFileName, MAX_PATH);
      SetLength(AFileName, DragQueryFile(ADropMsg.Drop, 0, PChar(AFileName),
        MAX_PATH));
      edtLogFile.Text := AFileName;
      OpenLog;
    end;
  end;

begin
  if AMsg.Msg = WM_DROPFILES then
    DoDragFile
  else
  begin
    FVSTWndProc(AMsg);
    if AMsg.Msg = WM_CREATE then // 窗口句柄重建时，重新激活文件拖放
      DragAcceptFiles(vstLogs.Handle, true);
  end;
end;

procedure TfrmLogViewer.FormCreate(Sender: TObject);
var
  I, C: Integer;
begin
  FVSTWndProc := vstLogs.WindowProc;
  vstLogs.WindowProc := DoVstWndProc;
  if vstLogs.HandleAllocated then
    DragAcceptFiles(vstLogs.Handle, true);
  vstLogs.NodeDataSize := 0;
  vstLogs.RootNodeCount := 0;
  C := ParamCount;
  for I := 1 to C do
  begin
    if ParamStr(I) = '/f' then
    begin
      edtLogFile.Text := ParamStr(I + 1);
      OpenLog;
    end
    else if ParamStr(I) = '/del' then
      FDeleteFileBeforeClose := true;
  end;
  // mmLineText.Text := CmdLine;
end;

procedure TfrmLogViewer.FormDestroy(Sender: TObject);
begin
  Workers.Clear(true);
  if Assigned(FStream) then
    FreeAndNil(FStream);
  if Assigned(FIndexStream) then
  begin
    FreeAndNil(FIndexStream);
    DeleteFile(FIndexFile);
  end;
  if FDeleteFileBeforeClose and FileExists(FFileName) then
    DeleteFile(FFileName);
end;

procedure TfrmLogViewer.FormResize(Sender: TObject);
begin
  pnlSearch.Left := pnlButtons.Left - pnlSearch.Width;
  pnlSearch.Top := sbSearch.ClientToParent(Point(0, 0), Self).Y;
  pnlEncoding.Left := pnlButtons.Left - pnlEncoding.Width;
  pnlEncoding.Top := sbCharset.ClientToParent(Point(0, 0), Self).Y;
end;

procedure TfrmLogViewer.OpenLog;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  if FDeleteFileBeforeClose and FileExists(FFileName) then
    DeleteFile(FFileName);
  FDeleteFileBeforeClose := false;
  FFileName := edtLogFile.Text;
  if FileExists(FFileName) then
  begin
    FStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    if not Assigned(FIndexStream) then
    begin
      FIndexFile := StringReplace(Application.ExeName, '.exe',
        '.' + IntToStr(GetCurrentProcessId) + '.idx',
        [rfReplaceAll, rfIgnoreCase]);
      FIndexStream := TFileStream.Create(FIndexFile, fmCreate);
    end
    else
      FIndexStream.Size := 0;
    pbProgress.Position := 0;
    pbProgress.Visible := true;
    ClearLogs;
    if rbAutoEncoding.Checked then
      FEncoding := TTextEncoding.teAuto
    else if rbUTF8Encoding.Checked then
      FEncoding := TTextEncoding.teUtf8
    else if rbUTF16LE.Checked then
      FEncoding := TTextEncoding.teUnicode16LE
    else
      FEncoding := TTextEncoding.teUnicode16BE;
    Workers.Post(DoLoadLogs, nil);
    mmLineText.Text:='打开日志成功';
  end
  else
    mmLineText.Text := '未找到请求打开的日志文件.';
end;

procedure TfrmLogViewer.sbAboutClick(Sender: TObject);
var
  F: TfrmAbout;
begin
  F := TfrmAbout.Create(Application);
  F.ShowModal;
  F.Free;
end;

procedure TfrmLogViewer.sbCharsetClick(Sender: TObject);
begin
  pnlEncoding.Visible := not pnlEncoding.Visible;
  if pnlEncoding.Visible then
    pnlSearch.Visible := false;
end;

procedure TfrmLogViewer.sbCloseEncodingClick(Sender: TObject);
begin
  pnlEncoding.Visible := false;
end;

procedure TfrmLogViewer.sbCloseSearchClick(Sender: TObject);
begin
  pnlSearch.Visible := false;
end;

procedure TfrmLogViewer.sbExtractClick(Sender: TObject);
var
  F: TfrmLogExtractor;
begin
  F := TfrmLogExtractor.Create(Application);
  F.ShowModal;
  if F.ModalResult = mrOk then
    DoSearch(F.edtToSearch.Text, F.edtToReplace.Text, true, true, true,
      F.chkDistinct.Checked);
  F.Free;
end;

procedure RegisterFileType(ExtName, AppName: string; IconName: string;
IconIndex: Integer);
var
  Reg: TRegistry;
  ExtKey: string;
begin
  if IconName = '' then
    IconName := AppName;
  ExtName := LowerCase(ExtName);
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Copy(ExtName, 1, 1) = '.' then
      ExtKey := Copy(ExtName, 2, Length(ExtName) - 1)
    else
    begin
      ExtKey := ExtName;
      ExtName := '.' + ExtName;
    end;
    ExtKey := ExtKey + 'file';
    Reg.OpenKey(ExtName, true);
    Reg.WriteString('', ExtKey);
    Reg.CloseKey;
    Reg.OpenKey(ExtKey, true);
    Reg.OpenKey('DefaultIcon', true);
    if ExtractFileExt(LowerCase(IconName)) = '.ico' then
      Reg.WriteString('', IconName)
    else
      Reg.WriteString('', IconName + ',' + IntToStr(IconIndex));
    Reg.CloseKey;
    Reg.OpenKey(ExtKey + '\shell\open\command', true);
    Reg.WriteString('', AppName + ' "%1"');
    Reg.CloseKey;
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  finally
    Reg.Free;
  end;
end;

procedure TfrmLogViewer.sbLinkLogClick(Sender: TObject);
begin
  if IsAdministrator then
  begin
    RegisterFileType('.log', QuotedStrW(Application.ExeName, '"') + ' /f ',
      Application.ExeName, 0);
    Application.MessageBox('日志文件扩展名 .log 已经与本程序关联，可以双击 log 文件打开。', '关联完成',
      MB_OK or MB_ICONSTOP);
  end
  else
    Application.MessageBox('请使用管理员身份运行本程序，以访问注册表建立关联。', '权限不足',
      MB_OK or MB_ICONSTOP);
end;

procedure TfrmLogViewer.sbSearchClick(Sender: TObject);
begin
  pnlSearch.Visible := not pnlSearch.Visible;
  if pnlSearch.Visible then
    pnlEncoding.Visible := false;
end;

procedure TfrmLogViewer.SpeedButton1Click(Sender: TObject);
var
  F: TfrmSftpBrowser;
begin
  F := TfrmSftpBrowser.Create(Application);
  F.ShowModal;
  F.Free;
end;

procedure TfrmLogViewer.vstLogsFocusChanged(Sender: TBaseVirtualTree;
Node: PVirtualNode; Column: TColumnIndex);
var
  ANode: PVirtualNode;
begin
  if Assigned(Node) then
  begin
    if (Node.Index >= FCachedStart) and
      (Node.Index < FCachedStart + FCachedLines) then
    begin
      if vstLogs.SelectedCount > 1 then
      begin
        mmLineText.Lines.BeginUpdate;
        try
          mmLineText.Lines.Clear;
          for ANode in vstLogs.SelectedNodes do
          begin
            mmLineText.Lines.Add(vstLogs.Text[ANode, 1]);
          end;
        finally
          mmLineText.Lines.EndUpdate;
        end;
      end
      else
        mmLineText.Text := FCached[Node.Index - FCachedStart].Text;
    end;
  end;
end;

procedure TfrmLogViewer.vstLogsGetText(Sender: TBaseVirtualTree;
Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
var CellText: string);
begin
  if Assigned(Node) then
  begin
    if Column = 0 then
      CellText := IntToStr(Node.Index + 1)
    else if (Node.Index >= FCachedStart) and
      (Node.Index < FCachedStart + FCachedLines) then
      CellText := FCached[Node.Index - FCachedStart].Text
    else
      CacheLines(Node.Index);
  end;
end;

end.
