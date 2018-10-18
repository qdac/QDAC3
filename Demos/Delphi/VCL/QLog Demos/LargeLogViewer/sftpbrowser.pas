unit sftpbrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, qmsgpack,
  Vcl.Buttons, VirtualTrees, ScSFTPClient, ScSSHClient;

type
  TfrmSftpBrowser = class(TForm)
    pcSession: TPageControl;
    tsSessions: TTabSheet;
    tsDirectory: TTabSheet;
    lbxSessions: TListBox;
    Splitter1: TSplitter;
    Panel1: TPanel;
    lvFiles: TListView;
    Panel2: TPanel;
    cbxCurrentDir: TComboBox;
    sbSearch: TSpeedButton;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edtUser: TEdit;
    Label3: TLabel;
    edtPass: TEdit;
    btnConnect: TButton;
    tvDirList: TTreeView;
    edtServer: TEdit;
    sftpClient: TScSFTPClient;
    sshClient: TScSSHClient;
    procedure btnConnectClick(Sender: TObject);
    procedure tvDirListExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure lvFilesDblClick(Sender: TObject);
    procedure sftpClientProgress(Sender: TObject;
      ABytesProceed, ATotalBytes: Int64);
    procedure cbxCurrentDirKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure lbxSessionsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbSearchClick(Sender: TObject);
  private
    { Private declarations }
    FLastProgress: Cardinal;
    FSettingsFile: String;
    FSettings, FActiveSession: TQMsgPack;
    function NodeExpanded(ANode: TTreeNode): Boolean;
    procedure ExpandNode(ANode: TTreeNode; ASync: Boolean = false);
    procedure ListDir(ANode: TTreeNode);
    procedure GotoDir(APath: String);
    function NodeOfPath(APath: String): TTreeNode;
    procedure SaveActiveSession;
  public
    { Public declarations }
  end;

var
  frmSftpBrowser: TfrmSftpBrowser;

implementation

uses qstring, qworker, dateutils, main;
{$R *.dfm}

type
  TSFTPItem = class
  private
    FAttrs: String; // 项目属性：drwxrwxrwx
    FChildCount: Integer; // 子结点数量
    FOwner: String;
    FGroup: String;
    FSize: Int64;
    FCreateTime: TDateTime;
    FName: String;
  end;

procedure TfrmSftpBrowser.btnConnectClick(Sender: TObject);
  procedure SaveSession;
  var
    I: Integer;
    AItem: TQMsgPack;
  begin
    FSettings.DataType := mptArray;
    for I := 0 to FSettings.Count - 1 do
    begin
      if FSettings[I].ValueByName('name', '') = edtServer.Text then
      begin
        AItem := FSettings[I];
        break;
      end;
    end;
    if not Assigned(AItem) then
    begin
      AItem := FSettings.Add;
      AItem.ForcePath('name').AsString := edtServer.Text;
      lbxSessions.Items.AddObject(edtServer.Text, AItem);
    end;
    AItem.ForcePath('uname').AsString := edtUser.Text;
    AItem.ForcePath('pwd').AsString := edtPass.Text;
    FActiveSession := AItem;
    FSettings.SaveToFile(FSettingsFile);
  end;

begin
  if Assigned(FActiveSession) then
    SaveActiveSession;
  sftpClient.Disconnect;
  sshClient.HostName := edtServer.Text;
  sshClient.User := edtUser.Text;
  sshClient.Password := edtPass.Text;
  sshClient.Connect;
  SaveSession;
  tvDirList.Items.Clear;
  pcSession.ActivePage := tsDirectory;
  ListDir(nil);
end;

function DoSFtpItemSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := NaturalCompareW(PQCharW((List.Objects[Index1] as TSFTPItem).FName),
    PQCharW((List.Objects[Index2] as TSFTPItem).FName), true);
end;

procedure TfrmSftpBrowser.cbxCurrentDirKeyPress(Sender: TObject; var Key: Char);
begin
  //
  if Key = #13 then
    GotoDir(cbxCurrentDir.Text);
end;

procedure TfrmSftpBrowser.ExpandNode(ANode: TTreeNode; ASync: Boolean);
begin
  if not NodeExpanded(ANode) then
  begin
    ANode[0].Delete;
    if ASync then
      ListDir(ANode)
    else
      Workers.Post(
        procedure(AJob: PQJob)
        begin
          ListDir(AJob.Data);
        end, ANode, true);
  end;
end;

procedure TfrmSftpBrowser.FormCreate(Sender: TObject);
begin
  FSettings := TQMsgPack.Create;
  FSettingsFile := ExtractFilePath(Application.ExeName) + 'sessions.dat';
  if FileExists(FSettingsFile) then
  begin
    FSettings.LoadFromFile(FSettingsFile);
    Workers.Post(
      procedure(AJob: PQJob)
      var
        AItem: TQMsgPack;
        I: Integer;
      begin
        lbxSessions.Items.BeginUpdate;
        try
          for I := 0 to FSettings.Count - 1 do
          begin
            AItem := FSettings[I];
            lbxSessions.Items.AddObject(AItem.ValueByName('name', ''), AItem);
          end;
        finally
          lbxSessions.Items.EndUpdate;
        end;
      end, nil, true);
  end;
  pcSession.ActivePage := tsSessions;
end;

procedure TfrmSftpBrowser.FormDestroy(Sender: TObject);
begin
  SaveActiveSession;
  FreeAndNil(FSettings);
end;

function TfrmSftpBrowser.NodeExpanded(ANode: TTreeNode): Boolean;
begin
  Result := (ANode.Count = 0) or Assigned(ANode[0].Data);
end;

procedure TfrmSftpBrowser.GotoDir(APath: String);
var
  ANode: TTreeNode;
begin
  ANode := NodeOfPath(APath);
  if Assigned(ANode) then
  begin
    ANode.Selected := true;
  end;
end;

procedure TfrmSftpBrowser.lbxSessionsDblClick(Sender: TObject);
var
  AItem: TQMsgPack;
begin
  if lbxSessions.ItemIndex <> -1 then
  begin
    AItem := lbxSessions.Items.Objects[lbxSessions.ItemIndex] as TQMsgPack;
    edtServer.Text := AItem.ValueByName('name', '');
    edtUser.Text := AItem.ValueByName('uname', '');
    edtPass.Text := AItem.ValueByName('pwd', '');
    btnConnect.Click;
  end;
end;

procedure TfrmSftpBrowser.ListDir(ANode: TTreeNode);
var
  ADir: String;
  AList: TStringList;
  I: Integer;
  AItem: TSFTPItem;
  AChildNode: TTreeNode;
  AFileItem: TListItem;
  function DecodeSFtpTime(var p: PQCharW): TDateTime;
  var
    Y, M, D, H, N: Word;
    I: Integer;
    V: Int64;
    // SFtp Date format
    // Month Day [Year] Hour:Minute
  const
    MonthNames: array [0 .. 11] of QStringW = ('Jan', 'Feb', 'Mar', 'Apr',
      'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  begin
    // 月
    M := 0;
    Result := 0;
    for I := 0 to High(MonthNames) do
    begin
      if StartWithW(p, PWideChar(MonthNames[I]), true) then
      begin
        M := I + 1;
        Inc(p, Length(MonthNames[I]) + 1);
        break;
      end;
    end;
    if M = 0 then
      Exit;
    // 日
    SkipSpaceW(p);
    if (ParseInt(p, V) <> 0) and (V >= 1) and (V <= MonthDays[true][M]) then
      D := M
    else
      Exit;
    // 年
    SkipSpaceW(p);
    if ParseInt(p, V) <> 0 then
    begin
      if p^ = ':' then
      begin
        if (V < 0) or (V > 23) then
          Exit;
        Y := YearOf(Now);
        H := V;
        Inc(p);
        if (ParseInt(p, V) <> 0) and (V >= 0) and (V < 60) then
        begin
          N := V;
          TryEncodeDateTime(Y, M, D, H, N, 0, 0, Result);
          SkipSpaceW(p);
        end;
      end
      else
      begin
        Y := V;
        if (ParseInt(p, V) <> 0) and (V >= 0) and (V <= 23) and (p^ = ':') then
        begin
          Inc(p);
          H := V;
          if (ParseInt(p, V) <> 0) and (V >= 0) and (V < 60) then
          begin
            N := V;
            TryEncodeDateTime(Y, M, D, H, N, 0, 0, Result);
            SkipSpaceW(p);
          end;
        end
        else
        begin
          TryEncodeDate(Y, M, D, Result);
          SkipSpaceW(p);
        end;
      end;
    end
    else // 没有年份
    begin
      Y := YearOf(Now);
      TryEncodeDate(Y, M, D, Result);
      SkipSpaceW(p);
    end;
  end;

  function DecodeItem(ALine: QStringW): TSFTPItem;
  var
    p: PQCharW;
    S: String;
    V: Int64;
  begin
    p := PQCharW(ALine);
    Result := TSFTPItem.Create;
    Result.FAttrs := DecodeTokenW(p, #9' ', #0, true);
    if ParseInt(p, V) <> 0 then
      Result.FChildCount := V
    else
      Result.FChildCount := 0;
    SkipSpaceW(p);
    Result.FOwner := DecodeTokenW(p, #9' ', #0, true);
    Result.FGroup := DecodeTokenW(p, #9' ', #0, true);
    if ParseInt(p, V) <> 0 then
      Result.FSize := V
    else
      Result.FSize := 0;
    SkipSpaceW(p);
    Result.FCreateTime := DecodeSFtpTime(p);
    Result.FName := p;
    if (Result.FName = '.') or (Result.FName = '..') then
      FreeAndNil(Result);
  end;

  function GetNodePath: String;
  var
    AParent: TTreeNode;
  begin
    AParent := ANode;
    Result := '';
    repeat
      Result := AParent.Text + '/' + Result;
      AParent := AParent.Parent;
    until AParent = nil;
    Result := '/' + Result;
  end;

begin
  if not Assigned(ANode) then
    ADir := '/'
  else
    ADir := GetNodePath;
  if sftpClient.CurrentDir <> ADir then
    sftpClient.ChangeCurrentDir(ADir);
  AList := TStringList.Create;
  tvDirList.Items.BeginUpdate;
  try
    cbxCurrentDir.Text := ADir;
    cbxCurrentDir.SelStart := Length(cbxCurrentDir.Text);
    sftpClient.GetList(AList);
    I := 0;
    while I < AList.Count do
    begin
      AList.Objects[I] := DecodeItem(AList[I]);
      if AList.Objects[I] = nil then
        AList.Delete(I)
      else
        Inc(I);
    end;
    AList.CustomSort(DoSFtpItemSort);
    lvFiles.Items.Clear;
    for I := 0 to AList.Count - 1 do
    begin
      AItem := (AList.Objects[I] as TSFTPItem);
      if Assigned(AItem) then
      begin
        if StartWithW(PQCharW(AItem.FAttrs), 'd', true) then // Dir
        begin
          AChildNode := tvDirList.Items.AddChildObject(ANode,
            AItem.FName, AItem);
          if AItem.FChildCount > 0 then
            tvDirList.Items.AddChildFirst(AChildNode, '__Expand');
        end
        else
        begin
          AFileItem := lvFiles.Items.Add;
          AFileItem.Caption := AItem.FName;
          AFileItem.Data := AItem;
          AFileItem.SubItems.Add(AItem.FAttrs);
          AFileItem.SubItems.Add(AItem.FOwner);
          AFileItem.SubItems.Add(AItem.FGroup);
          AFileItem.SubItems.Add(RollupSize(AItem.FSize));
          AFileItem.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn',
            AItem.FCreateTime));
        end;
      end;
    end;
  finally
    tvDirList.Items.EndUpdate;
    if Assigned(ANode) then
      ANode.Expanded := true
    else if Assigned(FActiveSession) then
    begin
      ADir := FActiveSession.ValueByName('lastdir', '');
      if Length(ADir) > 0 then
      begin
        cbxCurrentDir.Text := ADir;
        GotoDir(ADir);
      end;
    end;
    FreeAndNil(AList);
  end;
end;

procedure TfrmSftpBrowser.lvFilesDblClick(Sender: TObject);
var
  AListItem: TListItem;
  ATempFile, APath: String;
  AName: array [0 .. MAX_PATH] of PChar;
begin
  AListItem := lvFiles.Selected;
  if Assigned(AListItem) then
  begin
    APath := ExtractFilePath(Application.ExeName);
    GetTempFileName(PChar(APath), 'sf', 0, PChar(@AName[0]));
    ATempFile := PChar(@AName[0]);
    sftpClient.DownloadFile(AListItem.Caption, ATempFile,true);
    frmLogViewer.edtLogFile.Text := ATempFile;
    frmLogViewer.OpenLog;
    frmLogViewer.DeleteFileBeforeClose := true;
    ModalResult := mrOk;
  end;
end;

function TfrmSftpBrowser.NodeOfPath(APath: String): TTreeNode;
  function GotoNodeOfPath(AParent: TTreeNode; APath: String): TTreeNode;
  var
    I: Integer;
    ANode: TTreeNode;
    AText: String;
  begin
    Result := nil;
    while (Length(APath) > 0) and (Length(AText) = 0) do
      AText := DecodeTokenW(APath, '/\', '"', false, true);
    if Length(AText) > 0 then
    begin
      if not Assigned(AParent) then
      begin
        ANode := tvDirList.Items.GetFirstNode;
        while Assigned(ANode) do
        begin
          if ANode.Text = AText then
          begin
            ExpandNode(ANode, true);
            if Length(APath) > 0 then
              Result := GotoNodeOfPath(ANode, APath)
            else
              Result := ANode;
            break;
          end;
          ANode := ANode.getNextSibling;
        end;
      end
      else
      begin
        for I := 0 to AParent.Count - 1 do
        begin
          ANode := AParent[I];
          if ANode.Text = AText then
          begin
            ExpandNode(ANode, true);
            if Length(APath) > 0 then
              Result := GotoNodeOfPath(ANode, APath)
            else
              Result := ANode;
            break;
          end;
        end;
      end;
    end;
  end;

begin
  Result := GotoNodeOfPath(nil, APath);
  if Assigned(Result) and (not NodeExpanded(Result)) then
    ExpandNode(Result);
end;

procedure TfrmSftpBrowser.SaveActiveSession;
begin
  if Assigned(FActiveSession) then
  begin
    FActiveSession.ForcePath('lastdir').AsString := sftpClient.CurrentDir;
    FSettings.SaveToFile(FSettingsFile);
  end;
end;

procedure TfrmSftpBrowser.sbSearchClick(Sender: TObject);
begin
  GotoDir(cbxCurrentDir.Text);
end;

procedure TfrmSftpBrowser.sftpClientProgress(Sender: TObject;
ABytesProceed, ATotalBytes: Int64);
var
  ADelta: Cardinal;
begin
  ADelta := GetTickCount - FLastProgress;
  if ADelta >= 1000 then
  begin
    FLastProgress := GetTickCount;
    Caption := '已下载 ' + RollupSize(ABytesProceed) + '/' +
      RollupSize(ATotalBytes);
  end;
end;

procedure TfrmSftpBrowser.tvDirListExpanding(Sender: TObject; Node: TTreeNode;
var AllowExpansion: Boolean);
begin
  ExpandNode(Node);
end;

end.
