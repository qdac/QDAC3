unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, VirtualTrees,
  Grids, ValEdit,qjson, ImgList, ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    vstJson: TVirtualStringTree;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    pnlHint: TPanel;
    Panel2: TPanel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure vstJsonGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;var CellText: {$IFDEF UNICODE} string{$ELSE} WideString{$ENDIF});
    procedure vstJsonGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure vstJsonInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstJsonInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure chkShowTextInNodeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure vstJsonFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  private
    { Private declarations }
    FJson:TQJson;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qstring,ExtActns,openurl,iduri;
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

procedure TForm1.Button1Click(Sender: TObject);
var
  T,Speed:Cardinal;
  AFileSize:Int64;
begin
if OpenDialog1.Execute then
  begin
  T:=GetTickCount;
  FJson.LoadFromFile(OpenDialog1.FileName);
  T:=GetTickCount-T;
  AFileSize:=GetFileSize(OpenDialog1.FileName);
  if T>0 then
    Speed:=AFileSize *1000 div T
  else
    Speed:=0;
  pnlHint.Caption:=OpenDialog1.FileName+' - 大小'+RollupSize(AFileSize)+', 用时:'+IntToStr(T)+'ms，速度：'+RollupSize(Speed);
  vstJson.RootNodeCount:=0;
  vstJson.RootNodeCount:=FJson.Count;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
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
      FJson.LoadFromFile(Action.Filename);
      T:=GetTickCount-T;
      AFileSize:=GetFileSize(Action.FileName);
      if T>0 then
        Speed:=AFileSize *1000 div T
      else
        Speed:=0;
      pnlHint.Caption:=Action.URL+' - 大小'+RollupSize(AFileSize)+', 用时:'+IntToStr(T)+'ms，速度：'+RollupSize(Speed);
      vstJson.RootNodeCount:=0;
      vstJson.RootNodeCount:=FJson.Count;
      DeleteFile(Action.Filename);
      end;
  finally
    Action.Free;
  end;
  end;
end;

procedure TForm1.chkShowTextInNodeClick(Sender: TObject);
begin
vstJson.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FJson:=TQJson.Create;
vstJson.NodeDataSize:=SizeOf(Pointer);
end;

procedure TForm1.vstJsonFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AJson:PQJson;
begin
if Assigned(Node) then
  begin
  AJson:=vstJson.GetNodeData(Node);
  if AJson^<>nil then
    pnlHint.Caption:=' 路径: '+AJson.Path;
  end;
end;

procedure TForm1.vstJsonGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  AJson:PQJson;
begin
if Kind<>ikOverlay then
  begin
  AJson:=vstJson.GetNodeData(Node);
  if AJson^<>nil then
    begin
    case AJson^.DataType of
      jdtNull:
        ImageIndex:=0;
      jdtString:
        begin
        if AJson.IsDateTime then
          ImageIndex:=5
        else
          ImageIndex:=1;
        end;
      jdtInteger:
        ImageIndex:=2;
      jdtFloat:
        ImageIndex:=3;
      jdtBoolean:
        ImageIndex:=4;
      jdtDateTime:
        ImageIndex:=5;
      jdtArray:
        ImageIndex:=6;
      jdtObject:
        ImageIndex:=7;
    end;
    end
  end;
end;
{$IFDEF UNICODE}
procedure TForm1.vstJsonGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
{$ELSE}
procedure TForm1.vstJsonGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
{$ENDIF UNICODE}
var
  AJson:PQJson;
begin
AJson:=vstJson.GetNodeData(Node);
if AJson^<>nil then
  begin
  if AJson.DataType in [jdtArray,jdtObject] then
    CellText:=AJson.Name
  else if AJson.DataType=jdtString then
    begin
    if AJson.IsDateTime then
      CellText:=AJson.Name+' = '+AJson.AsString+' ('+FormatDateTime(JsonDateTimeFormat,AJson.AsDateTime)+')'
    else
      CellText:=AJson.Name+' = '+AJson.AsString;
    end
  else
    CellText:=AJson.Name+' = '+AJson.AsString;
  end;
end;

procedure TForm1.vstJsonInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
ChildCount:=PQJson(vstJson.GetNodeData(Node)).Count;
end;

procedure TForm1.vstJsonInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  AJsonParent:TQJson;
  AJson:TQJson;
begin
if ParentNode<>nil then
  AJsonParent:=PQJson(vstJson.GetNodeData(ParentNode))^
else
  AJsonParent:=FJson;
AJson:=AJsonParent.Items[Node.Index];
PQJson(vstJson.GetNodeData(Node))^:=AJson;
if AJson.Count>0 then
  InitialStates:=InitialStates+[ivsHasChildren];
end;

end.
