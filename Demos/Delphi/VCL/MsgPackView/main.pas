unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Types,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, VirtualTrees,
  Grids, ValEdit, ComCtrls, Vcl.Buttons, Vcl.Imaging.jpeg, QString, qmsgpack,
  Vcl.ImgList, ActiveX,
  Vcl.Imaging.pngimage, Vcl.Menus, System.ImageList;

type
  TSourceFileType = (sftUnknown, sftJson, sftMsgPack);

  TfrmMain = class(TForm)
    Panel1: TPanel;
    vstItems: TVirtualStringTree;
    Splitter1: TSplitter;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    pnlHint: TPanel;
    pnlCopyright: TPanel;
    Button2: TButton;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    cbxPathDelimiter: TComboBox;
    edtPath: TEdit;
    lblCopyright: TLabel;
    Image1: TImage;
    pnlSearched: TPanel;
    lbxSearched: TListBox;
    pnlSearchTitle: TPanel;
    sbSearchSize: TSpeedButton;
    ImageList1: TImageList;
    chkShowValue: TCheckBox;
    Bevel1: TBevel;
    pmActions: TPopupMenu;
    miAddNode: TMenuItem;
    miDeleteNode: TMenuItem;
    miEditNode: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure vstItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText:
{$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
    procedure vstItemsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstItemsInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstItemsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure chkShowTextInNodeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbxPathDelimiterChange(Sender: TObject);
    procedure lbxSearchedClick(Sender: TObject);
    procedure vstItemsResize(Sender: TObject);
    procedure sbSearchSizeClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstItemsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure chkShowValueClick(Sender: TObject);
    procedure vstItemsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstItemsDblClick(Sender: TObject);
    procedure miAddNodeClick(Sender: TObject);
    procedure miDeleteNodeClick(Sender: TObject);
    procedure miEditNodeClick(Sender: TObject);
    procedure pmActionsPopup(Sender: TObject);
    procedure vstItemsDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure vstItemsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
  private
    { Private declarations }
    FMsgPack: TQMsgPack;
    FFileName: QStringW;
    procedure SaveLastFile(const AFileName: QStringW; IsUrl: Boolean;
      AFileType: TSourceFileType);
    procedure LoadLastFile;
    procedure LoadFromUrl(const S: QStringW);
    procedure LoadFromFile(const AFileName: QStringW;
      var AFileType: TSourceFileType);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses qjson, ExtActns, openurl, iduri, find, valueeditor;
{$R *.dfm}

function GetFileSize(AFileName: String): Int64;
var
  sr: TSearchRec;
  AHandle: Integer;
begin
  AHandle := FindFirst(AFileName, faAnyFile, sr);
  if AHandle = 0 then
  begin
    Result := sr.Size;
    FindClose(sr);
  end
  else
    Result := 0;
end;

procedure JsonToMsgPack(AJson: TQJson; AMsgPack: TQMsgPack);
var
  I: Integer;
  AChild: TQJson;
  procedure CopyChild(AJsonChild: TQJson; AMsgPackChild: TQMsgPack);
  var
    J: Integer;
  begin
    case AJsonChild.DataType of
      jdtNull:
        AMsgPackChild.DataType := mptNull;
      jdtString:
        AMsgPackChild.AsString := AJsonChild.AsString;
      jdtInteger:
        AMsgPackChild.AsInt64 := AJsonChild.AsInt64;
      jdtFloat:
        AMsgPackChild.AsFloat := AJsonChild.AsFloat;
      jdtDateTime:
        AMsgPackChild.AsDateTime := AJsonChild.AsDateTime;
      jdtBoolean:
        AMsgPackChild.AsBoolean := AJsonChild.AsBoolean;
      jdtArray:
        begin
          AMsgPackChild.DataType := mptArray;
          J := 0;
          while J < AJsonChild.Count do
          begin
            CopyChild(AJsonChild[J], AMsgPackChild.Add);
            Inc(J);
          end;
        end;
      jdtObject:
        begin
          AMsgPackChild.DataType := mptMap;
          J := 0;
          while J < AJsonChild.Count do
          begin
            AChild := AJsonChild[J];
            CopyChild(AChild, AMsgPackChild.Add(AChild.Name));
            Inc(J);
          end;
        end;
    end;
  end;

begin
  AMsgPack.Clear;
  I := 0;
  while I < AJson.Count do
  begin
    AChild := AJson[I];
    CopyChild(AChild, AMsgPack.Add(AChild.Name));
    Inc(I);
  end;
end;

procedure MsgPackToJson(AMsgPack: TQMsgPack; AJson: TQJson);
var
  I: Integer;
  AChild: TQMsgPack;
  procedure CopyChild(AMsgPackChild: TQMsgPack; AJsonChild: TQJson);
  var
    J: Integer;
  begin
    case AMsgPackChild.DataType of
      mptInteger:
        AJsonChild.AsInt64 := AMsgPackChild.AsInt64;
      mptNull:
        AJsonChild.ResetNull;
      mptBoolean:
        AJsonChild.AsBoolean := AMsgPackChild.AsBoolean;
      mptSingle, mptFloat:
        AJsonChild.AsFloat := AMsgPackChild.AsFloat;
      mptString, mptExtended:
        AJsonChild.AsString := AMsgPackChild.AsString;
      mptBinary:
        AJsonChild.AsBytes := AMsgPackChild.AsBytes;
      mptDateTime:
        AJsonChild.AsDateTime := AMsgPackChild.AsDateTime;
      mptArray:
        begin
          AJsonChild.DataType := jdtArray;
          J := 0;
          while J < AMsgPackChild.Count do
          begin
            CopyChild(AMsgPackChild[J], AJsonChild.Add);
            Inc(J);
          end;
        end;
      mptMap:
        begin
          AJsonChild.DataType := jdtObject;
          J := 0;
          while J < AMsgPackChild.Count do
          begin
            AChild := AMsgPackChild[J];
            CopyChild(AChild, AJsonChild.Add(AChild.Name));
            Inc(J);
          end;
        end;
    end;
  end;

begin
  AJson.Clear;
  I := 0;
  while I < AMsgPack.Count do
  begin
    AChild := AMsgPack[I];
    CopyChild(AChild, AJson.Add(AChild.Name));
    Inc(I);
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  AFileType: TSourceFileType;
begin
  if OpenDialog1.Execute then
  begin
    if OpenDialog1.FilterIndex = 1 then
      AFileType := sftMsgPack
    else if OpenDialog1.FilterIndex = 2 then
      AFileType := sftJson
    else
      AFileType := sftUnknown;
    LoadFromFile(OpenDialog1.FileName, AFileType);
    vstItems.RootNodeCount := 0;
    vstItems.RootNodeCount := FMsgPack.Count;
    SaveLastFile(OpenDialog1.FileName, False, AFileType);
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  frmUrlInput := TfrmUrlInput.Create(Application);
  frmUrlInput.ShowModal;
  if (frmUrlInput.ModalResult = mrOk) and (Length(frmUrlInput.edtUrl.Text) > 0)
  then
    LoadFromUrl(frmUrlInput.edtUrl.Text);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  ADoCompact: Boolean;
  procedure Compact(ANode: TQMsgPack);
  var
    I: Integer;
  begin
    if ANode.DataType = mptFloat then
      ANode.AsSingle := ANode.AsFloat
    else if ANode.DataType in [mptArray, mptMap] then
    begin
      for I := 0 to ANode.Count - 1 do
        Compact(ANode[I]);
    end;
  end;
  procedure SaveAsJson;
  var
    AJson: TQJson;
  begin
    AJson := TQJson.Create;
    try
      AJson.Parse(FMsgPack.AsJson);
      AJson.SaveToFile(SaveDialog1.FileName);
    finally
      FreeObject(AJson);
    end;
  end;

begin
  ADoCompact := ssShift in KeyboardStateToShiftState;
  if SaveDialog1.Execute then
  begin
    if SaveDialog1.FilterIndex = 1 then
    begin
      if ADoCompact then
        Compact(FMsgPack);
      FMsgPack.SaveToFile(SaveDialog1.FileName);
      vstItems.Invalidate;
    end
    else
      SaveAsJson
  end;
end;

procedure TfrmMain.cbxPathDelimiterChange(Sender: TObject);
begin
  case cbxPathDelimiter.ItemIndex of
    0:
      QMsgPackPathDelimiter := '\';
    1:
      QMsgPackPathDelimiter := '/';
    2:
      QMsgPackPathDelimiter := '.';
  end;
  if vstItems.FocusedNode <> nil then
    vstItemsFocusChanged(vstItems, vstItems.FocusedNode, -1);
end;

procedure TfrmMain.chkShowTextInNodeClick(Sender: TObject);
begin
  vstItems.InvalidateToBottom(vstItems.GetFirstVisible);
end;

procedure TfrmMain.chkShowValueClick(Sender: TObject);
begin
  vstItems.InvalidateToBottom(vstItems.GetFirstVisible());
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMsgPack := TQMsgPack.Create;
  vstItems.NodeDataSize := SizeOf(Pointer);
  lblCopyright.Caption :=
    ' MessagePack View 1.0@QDAC.QMsgPack'#13#10#13#10' 官方QQ群:250530692';
  LoadLastFile;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('F')) then
  begin
    if frmFind = nil then
      frmFind := TfrmFind.Create(Application);
    frmFind.Show;
  end;
end;

procedure TfrmMain.lbxSearchedClick(Sender: TObject);
var
  ANode: PVirtualNode;
begin
  if lbxSearched.ItemIndex <> -1 then
  begin
    ANode := PVirtualNode(lbxSearched.Items.Objects[lbxSearched.ItemIndex]);
    vstItems.FocusedNode := ANode;
    vstItems.Selected[ANode] := True;
  end;
end;

procedure TfrmMain.LoadFromFile(const AFileName: QStringW;
  var AFileType: TSourceFileType);
var
  AJson: TQJson;
  S: QStringW;
  T, Speed: Cardinal;
  AFileSize: Int64;
begin
  T := GetTickCount;
  if AFileType in [sftJson, sftUnknown] then
  begin
    AJson := TQJson.Create;
    try
      S := LoadTextW(AFileName);
      if AJson.TryParse(S) then
        JsonToMsgPack(AJson, FMsgPack)
      else
        FMsgPack.LoadFromFile(AFileName);
    finally
      FreeObject(AJson);
    end;
  end
  else
    FMsgPack.LoadFromFile(AFileName);
  T := GetTickCount - T;
  FFileName:=AFileName;
  AFileSize := GetFileSize(AFileName);
  if T > 0 then
    Speed := AFileSize * 1000 div T
  else
    Speed := 0;
  cbxPathDelimiter.Visible := False;
  lbxSearched.Clear;
  pnlSearched.Visible := False;
  edtPath.Visible := False;
  pnlHint.Caption := AFileName + ' - 大小' + RollupSize(AFileSize) + ', 用时:' +
    IntToStr(T) + 'ms，速度：' + RollupSize(Speed) + '/s';
end;

procedure TfrmMain.LoadFromUrl(const S: QStringW);
var
  Action: TDownloadUrl;
  T: Cardinal;
  AFileSize, ASpeed: Int64;
  AFileType: TSourceFileType;
begin
  Action := TDownloadUrl.Create(Self);
  try
    randomize;
    Action.URL := TIdUri.URLEncode(S);
    Action.FileName := ExtractFilePath(Application.ExeName) + '_url.html';
    T := GetTickCount;
    if Action.Execute then
    begin
      AFileType := sftUnknown;
      LoadFromFile(Action.FileName, AFileType);
      T := GetTickCount - T;
      AFileSize := GetFileSize(Action.FileName);
      if T > 0 then
        ASpeed := AFileSize * 1000 div T
      else
        ASpeed := 0;
      cbxPathDelimiter.Visible := False;
      edtPath.Visible := False;
      pnlHint.Caption := Action.URL + ' - 大小' + RollupSize(AFileSize) + ', 用时:'
        + IntToStr(T) + 'ms，速度：' + RollupSize(ASpeed) + '/s';
      vstItems.RootNodeCount := 0;
      vstItems.RootNodeCount := FMsgPack.Count;
      lbxSearched.Clear;
      pnlSearched.Visible := False;
      DeleteFile(Action.FileName);
      SaveLastFile(S, True, AFileType);
    end;
  finally
    Action.Free;
  end;

end;

procedure TfrmMain.LoadLastFile;
var
  AOpt: TQMsgPack;
  AOptFile: QStringW;
  AFileType: TSourceFileType;
begin
  AOptFile := ExtractFilePath(Application.ExeName) + 'msgpackview.opt';
  if FileExists(AOptFile) then
  begin
    AOpt := TQMsgPack.Create;
    try
      AOpt.LoadFromFile(AOptFile);
      if AOpt.ValueByName('FileType', '') = 'MsgPackViewConfig' then
      begin
        AOptFile := AOpt.ValueByName('LastFile', '');
        if FileExists(AOptFile) then
        begin
          AFileType := TSourceFileType(AOpt.IntByName('ContentType', 0));
          if AOpt.BoolByName('IsUrl', False) then
            LoadFromUrl(AOptFile)
          else
            LoadFromFile(AOptFile, AFileType);
          vstItems.RootNodeCount := 0;
          vstItems.RootNodeCount := FMsgPack.Count;
        end;
      end;
    finally
      FreeObject(AOpt);
    end;
  end;
end;

procedure TfrmMain.miAddNodeClick(Sender: TObject);
var
  AMsgPack: PQMsgPack;
  ANode: PVirtualNode;
begin
  ANode := vstItems.GetNodeAt(vstItems.ScreenToClient(pmActions.PopupPoint));
  if Assigned(ANode) then
  begin
    AMsgPack := vstItems.GetNodeData(ANode);
    if AddNode(AMsgPack^) then
      vstItems.ReinitNode(ANode, False);
  end
  else if AddNode(FMsgPack) then
    vstItems.RootNodeCount := FMsgPack.Count;
end;

procedure TfrmMain.miDeleteNodeClick(Sender: TObject);
var
  ANode: PVirtualNode;
  AItem, AParent: TQMsgPack;
  S: QStringW;
begin
  ANode := vstItems.FocusedNode;
  if ANode <> nil then
  begin
    AItem := PQMsgPack(vstItems.GetNodeData(ANode))^;
    S := '您确实想删除结点 [' + AItem.Path + '] 及其所有子结点吗？';
    if Application.MessageBox(PChar(S), '删除结点', MB_YESNO or MB_ICONQUESTION) = IDYES
    then
    begin
      AParent := AItem.Parent;
      AParent.Delete(AItem.ItemIndex);
      vstItems.DeleteNode(ANode);
    end;
  end;
end;

procedure TfrmMain.miEditNodeClick(Sender: TObject);
begin
  vstItems.OnDblClick(Sender);
end;

procedure TfrmMain.pmActionsPopup(Sender: TObject);
begin
  miDeleteNode.Enabled := vstItems.FocusedNode <> nil;
  miEditNode.Enabled := miDeleteNode.Enabled;
end;

procedure TfrmMain.SaveLastFile(const AFileName: QStringW; IsUrl: Boolean;
  AFileType: TSourceFileType);
var
  AOpt: TQMsgPack;
  AOptFile: QStringW;
begin
  AOptFile := ExtractFilePath(Application.ExeName) + 'msgpackview.opt';
  AOpt := TQMsgPack.Create;
  try
    AOpt.Add('FileType').AsString := 'MsgPackViewConfig';
    AOpt.Add('LastFile').AsString := AFileName;
    AOpt.Add('IsUrl').AsBoolean := IsUrl;
    AOpt.Add('ContentType').AsInteger := Integer(AFileType);
    AOpt.SaveToFile(AOptFile);
  finally
    FreeObject(AOpt);
  end;
end;

procedure TfrmMain.sbSearchSizeClick(Sender: TObject);
begin
  pnlSearched.Visible := False;
end;

procedure TfrmMain.vstItemsDblClick(Sender: TObject);
var
  AMsgPack: PQMsgPack;
begin
  if Assigned(vstItems.FocusedNode) then
  begin
    AMsgPack := vstItems.GetNodeData(vstItems.FocusedNode);
    EditNode(AMsgPack^);
  end;
end;

procedure TfrmMain.vstItemsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  ASource, ADest: PQMsgPack;
  ACopy: TQMsgPack;
  ASelected: PVirtualNode;
begin
  ASelected := vstItems.FocusedNode;
  ASource := vstItems.GetNodeData(ASelected);
  ADest := vstItems.GetNodeData(vstItems.DropTargetNode);
  if ssCtrl in Shift then // Shift+Drag 复制
  begin
    ACopy := ASource.Copy;
    ADest.Add(ACopy);
  end
  else
  begin
    ASource.AttachTo(ADest^);
    vstItems.ReinitNode(ASelected.Parent, False);
  end;
  vstItems.ReinitNode(vstItems.DropTargetNode, False);
  vstItems.Expanded[vstItems.DropTargetNode] := True;
end;

procedure TfrmMain.vstItemsDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
var
  AMsgPack: PQMsgPack;
  ANode, ASelected: PVirtualNode;
begin
  ANode := vstItems.GetNodeAt(Pt);
  if ANode <> nil then
  begin
    AMsgPack := vstItems.GetNodeData(ANode);
    ASelected := vstItems.FocusedNode;
    Accept := (ANode <> ASelected) and (AMsgPack.DataType in [mptArray, mptMap])
      and (ANode.Parent <> ASelected);
  end;
end;

procedure TfrmMain.vstItemsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AMsgPack: PQMsgPack;
begin
  if Node <> nil then
  begin
    AMsgPack := vstItems.GetNodeData(Node);
    edtPath.Text := AMsgPack.Path;
  end
  else
    edtPath.Text := '';
end;

procedure TfrmMain.vstItemsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  AMsgPack: PQMsgPack;
begin
  if Kind <> ikOverlay then
  begin
    AMsgPack := vstItems.GetNodeData(Node);
    if AMsgPack^ <> nil then
      ImageIndex := Integer(AMsgPack.DataType) - 1;
  end;
end;

procedure TfrmMain.vstItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText:
{$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
var
  AMsgPack: PQMsgPack;
  S: QStringW;
begin
  AMsgPack := vstItems.GetNodeData(Node);
  if AMsgPack^ <> nil then
  begin
    CellText := AMsgPack^.Name;
    if chkShowValue.Checked then
    begin
      if AMsgPack.Count = 0 then
      begin
        S := AMsgPack.AsString;
        if Length(S) > 256 then
          S := LeftStrW(S, 256, True) + '...';
        if Length(CellText) > 0 then
          CellText := CellText + '=' + S
        else
          CellText := S;
      end
      else
        CellText := CellText + '<' + IntToStr(AMsgPack.Count) + ' 项>';
    end;
  end;
end;

procedure TfrmMain.vstItemsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := PQMsgPack(vstItems.GetNodeData(Node)).Count;
end;

procedure TfrmMain.vstItemsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  AMsgPackParent: TQMsgPack;
  AMsgPack: TQMsgPack;
begin
  if ParentNode <> nil then
    AMsgPackParent := PQMsgPack(vstItems.GetNodeData(ParentNode))^
  else
    AMsgPackParent := FMsgPack;
  AMsgPack := AMsgPackParent.Items[Node.Index];
  PQMsgPack(vstItems.GetNodeData(Node))^ := AMsgPack;
  if AMsgPack.Count > 0 then
    InitialStates := InitialStates + [ivsHasChildren];
end;

procedure TfrmMain.vstItemsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    miDeleteNodeClick(Sender);
end;

procedure TfrmMain.vstItemsResize(Sender: TObject);
begin
  vstItems.InvalidateToBottom(vstItems.GetFirstVisible);
end;

end.
