unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, VirtualTrees,
  Grids, ValEdit,qxml, ComCtrls, Vcl.Buttons, Vcl.Imaging.jpeg;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    pnlAttrs: TPanel;
    vstXML: TVirtualStringTree;
    Splitter1: TSplitter;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    pnlHint: TPanel;
    chkShowTextInNode: TCheckBox;
    pnlCopyright: TPanel;
    Button2: TButton;
    chkShowPropInNode: TCheckBox;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    chkNoShortTagClose: TCheckBox;
    cbxPathDelimiter: TComboBox;
    edtPath: TEdit;
    mmText: TMemo;
    vleAttrs: TValueListEditor;
    Splitter2: TSplitter;
    pnlSearched: TPanel;
    lbxSearched: TListBox;
    pnlSearchTitle: TPanel;
    sbSearchSize: TSpeedButton;
    lblCopyright: TLabel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure vstXMLGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
    procedure vstXMLGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure vstXMLInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstXMLInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure vstXMLFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure chkShowTextInNodeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkNoShortTagCloseClick(Sender: TObject);
    procedure cbxPathDelimiterChange(Sender: TObject);
    procedure lbxSearchedClick(Sender: TObject);
    procedure vstXMLResize(Sender: TObject);
    procedure sbSearchSizeClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FXML:TQXMLNode;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses qstring,ExtActns,openurl,iduri,find;
{$R *.dfm}
function GetFileSize(AFileName:String):Int64;
var
  sr:TSearchRec;
  AHandle:Integer;
begin
AHandle:=FindFirst(AFileName,faAnyFile,sr);
if AHandle=0 then
  begin
  Result:=sr.Size;
  FindClose(sr);
  end
else
  Result:=0;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  T,Speed:Cardinal;
  AFileSize:Int64;
begin
if OpenDialog1.Execute then
  begin
  T:=GetTickCount;
  FXML.LoadFromFile(OpenDialog1.FileName);
  T:=GetTickCount-T;
  AFileSize:=GetFileSize(OpenDialog1.FileName);
  if T>0 then
    Speed:=AFileSize *1000 div T
  else
    Speed:=0;
  cbxPathDelimiter.Visible:=False;
  lbxSearched.Clear;
  pnlSearched.Visible:=False;
  edtPath.Visible:=False;
  pnlHint.Caption:=OpenDialog1.FileName+' - 大小'+RollupSize(AFileSize)+', 用时:'+IntToStr(T)+'ms，速度：'+RollupSize(Speed);
  vstXML.RootNodeCount:=0;
  vstXML.RootNodeCount:=FXML.Count;
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  Action:TDownloadUrl;
  T,Speed:Cardinal;
  AFileSize:Int64;
begin
frmUrlInput:=TfrmUrlInput.Create(Application);
frmUrlInput.ShowModal;
if (frmUrlInput.ModalResult=mrOk) and (Length(frmUrlInput.edtUrl.Text)>0) then
  begin
  Action:=TDownloadUrl.Create(Self);
  try
    randomize;
    Action.URL:=TIdUri.URLEncode(frmUrlInput.edtUrl.Text);
    Action.Filename:=ExtractFilePath(Application.ExeName)+'_url.html';
    T:=GetTickCount;
    if Action.Execute then
      begin
      FXML.LoadFromFile(Action.Filename);
      T:=GetTickCount-T;
      AFileSize:=GetFileSize(Action.FileName);
      if T>0 then
        Speed:=AFileSize *1000 div T
      else
        Speed:=0;
      cbxPathDelimiter.Visible:=False;
      edtPath.Visible:=False;
      pnlHint.Caption:=Action.URL+' - 大小'+RollupSize(AFileSize)+', 用时:'+IntToStr(T)+'ms，速度：'+RollupSize(Speed);
      vstXML.RootNodeCount:=0;
      vstXML.RootNodeCount:=FXML.Count;
      lbxSearched.Clear;
      pnlSearched.Visible:=False;
      DeleteFile(Action.Filename);
      end;
  finally
    Action.Free;
  end;
  end;

end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
if SaveDialog1.Execute then
  begin
  FXML.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.cbxPathDelimiterChange(Sender: TObject);
begin
case cbxPathDelimiter.ItemIndex of
  0:XMLPathDelimiter:='\';
  1:XMLPathDelimiter:='/';
  2:XMLPathDelimiter:='.';
end;
if vstXML.FocusedNode<>nil then
  vstXMLFocusChanged(vstXML,vstXML.FocusedNode,-1);
end;

procedure TfrmMain.chkNoShortTagCloseClick(Sender: TObject);
begin
XMLTagShortClose:=not chkNoShortTagClose.Checked;
end;

procedure TfrmMain.chkShowTextInNodeClick(Sender: TObject);
begin
vstXML.InvalidateToBottom(vstXML.GetFirstVisible);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
FXML:=TQXMLNode.Create;
vstXML.NodeDataSize:=SizeOf(Pointer);
lblCopyright.Caption:=' XMLView 1.0@QDAC.QXML'#13#10#13#10' 官方QQ群:250530692';
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Shift=[ssCtrl]) and (Key=Ord('F')) then
  begin
  if frmFind=nil then
    frmFind:=TfrmFind.Create(Application);
  frmFind.Show;
  end;
end;

procedure TfrmMain.lbxSearchedClick(Sender: TObject);
var
  ANode:PVirtualNode;
begin
if lbxSearched.ItemIndex<>-1 then
  begin
  ANode:=PVirtualNode(lbxSearched.Items.Objects[lbxSearched.ItemIndex]);
  vstXML.FocusedNode:=ANode;
  vstXML.Selected[ANode]:=True;
  end;
end;

procedure TfrmMain.sbSearchSizeClick(Sender: TObject);
begin
if sbSearchSize.Caption='6' then
  begin
  sbSearchSize.Tag:=pnlSearched.ClientHeight;
  sbSearchSize.Caption:='5';
  pnlSearched.ClientHeight:=pnlSearchTitle.Height;
  end
else
  begin
  pnlSearched.ClientHeight:=sbSearchSize.Tag;
  sbSearchSize.Caption:='6';
  end;
end;

procedure TfrmMain.vstXMLFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AXML:PQXMLNode;
  I:Integer;
begin
if Node<>nil then
  begin
  AXML:=vstXML.GetNodeData(Node);
  if AXML^<>nil then
    begin
    case AXML^.NodeType of
      xntNode:
        begin
        vleAttrs.Strings.Clear;
        vleAttrs.Visible:=True;
        mmText.Visible:=False;
        for I := 0 to AXML.Attrs.Count-1 do
          begin
          vleAttrs.InsertRow(AXML.Attrs[I].Name,AXML.Attrs[I].Value,True);
          end;
        end
      else
        begin
        mmText.Visible:=True;
        vleAttrs.Visible:=False;
        mmText.Text:=AXML.Text;
        end;
    end;
    cbxPathDelimiter.Visible:=True;
    edtPath.Visible:=True;
    edtPath.Text:=AXML.Path;
    end
  end
else
  begin
  vleAttrs.Strings.Clear;
  mmText.Clear;
  end;
end;

procedure TfrmMain.vstXMLGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  AXML:PQXMLNode;
begin
if Kind<>ikOverlay then
  begin
  AXML:=vstXML.GetNodeData(Node);
  if AXML^<>nil then
    begin
    case AXML^.NodeType of
      xntNode:
        ImageIndex:=0;
      xntText:
        ImageIndex:=1;
      xntComment:
        ImageIndex:=2;
      xntCData:
        ImageIndex:=3;
    end;
    end
  end;
end;

procedure TfrmMain.vstXMLGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF});
var
  AXML:PQXMLNode;
  Attr:TQXMLAttr;
begin
AXML:=vstXML.GetNodeData(Node);
if AXML^<>nil then
  begin
  case AXML.NodeType of
    xntNode:
      begin
      CellText:=AXML^.Name;
      if chkShowPropInNode.Checked and (AXML.Attrs.Count>0) then
        begin
        for Attr in AXML.Attrs do
          begin
          if Length(Attr.Value)>0 then
            CellText:=CellText+' '+Attr.Name+' = '+QuotedStrW(Attr.Value,'"')
          else
            CellText:=CellText+' '+Attr.Name;
          end;
        end;
      end;
    xntText:
      begin
      if chkShowTextInNode.Checked then
        CellText:=AXML.Text
      else
        CellText:='<文本>';
      end;
    xntComment:
      begin
      if chkShowTextInNode.Checked then
        CellText:=AXML.Text
      else
        CellText:='<注释>';
      end;
    xntCData:
      begin
      if chkShowTextInNode.Checked then
        CellText:=AXML.Text
      else
        CellText:='<CDATA>';
      end;
  end;
  end;
end;

procedure TfrmMain.vstXMLInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
ChildCount:=PQXMLNode(vstXML.GetNodeData(Node)).Count;
end;

procedure TfrmMain.vstXMLInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  AXMLParent:TQXMLNode;
  AXML:TQXMLNode;
begin
if ParentNode<>nil then
  AXMLParent:=PQXMLNode(vstXML.GetNodeData(ParentNode))^
else
  AXMLParent:=FXML;
AXML:=AXMLParent.Items[Node.Index];
PQXMLNode(vstXML.GetNodeData(Node))^:=AXML;
if AXML.Count>0 then
  InitialStates:=InitialStates+[ivsHasChildren];
end;

procedure TfrmMain.vstXMLResize(Sender: TObject);
begin
vstXML.InvalidateToBottom(vstXML.GetFirstVisible);
end;

end.
