unit UViewerMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,UJSON, Clipbrd, SHellApi, Registry, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ImgList, Vcl.StdActns, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    JSONTreeViewer: TTreeView;
    TBOpen: TToolButton;
    TBPaste: TToolButton;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    TBCopy: TToolButton;
    TBClear: TToolButton;
    ToolButton1: TToolButton;
    ESearch: TEdit;
    BtnFindNode: TToolButton;
    StatusBar1: TStatusBar;
    TBGeneration: TToolButton;
    ToolButton3: TToolButton;
    procedure TBOpenClick(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EditDelete1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JSONTreeViewerEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure JSONTreeViewerClick(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditCopy1Update(Sender: TObject);
    procedure BtnFindNodeClick(Sender: TObject);
    procedure JSONTreeViewerExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure TBGenerationClick(Sender: TObject);
  private
    FRootObj : IJSONObject;
    FNodeMap : TDictionary<IJSONObject,TTreeNode>;
    function FindNode(Tree: TTreeView; Node: TTreeNode; SubS: String): TTreeNode;
    procedure RegisterFileType(AExt, AFileName: string);
    procedure Clear;
    function ParseStr(AStr : string) : IJSONObject;
    function LoadFile(AFileName : string) : IJSONObject;
  public
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;//получение сообщений о переносе файла в окно приложени€
    procedure ShowObject(AParentNode : TTreeNode; AObject : IJSONObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.RegisterFileType(AExt: string; AFileName: string);
var
  tmpReg: TRegistry;
begin
  tmpReg := TRegistry.Create;
  with tmpReg do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + AExt,True);
    WriteString('',AExt + 'file');
    CloseKey;
    CreateKey(AExt + 'file');
    OpenKey(AExt + 'file\DefaultIcon',True);
    WriteString('',AFileName+',0');
    CloseKey;
    OpenKey(AExt + 'file\shell\open\command',True);
    WriteString('',AFileName + ' "%1"');
    CloseKey;
    Free;
  end;
end;

procedure TMainForm.Clear;
begin
  JSONTreeViewer.Items.Clear;
  FNodeMap.Clear;
end;

procedure TMainForm.EditCopy1Execute(Sender: TObject);
begin
  if Assigned(FRootObj) then
    Clipboard.AsText := FRootObj.Text;
end;

procedure TMainForm.EditCopy1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := true;
end;

procedure TMainForm.EditDelete1Execute(Sender: TObject);
begin
  Clear;
end;

procedure TMainForm.EditPaste1Execute(Sender: TObject);
begin
  FRootObj := ParseStr(Clipboard.AsText);
  JSONTreeViewer.Items.BeginUpdate;
  ShowObject(nil,FRootObj);
  JSONTreeViewer.Items.EndUpdate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  tmpFileName : string;
begin
  ReportMemoryLeaksOnShutdown := true;
  FNodeMap := TDictionary<IJSONObject,TTreeNode>.Create;
  RegisterFileType('json',Application.ExeName);
  DragAcceptFiles(Handle, True);

  if ParamStr(1) <> '' then
  begin
    tmpFileName := ParamStr(1);
    FRootObj := LoadFile(tmpFileName);
    ShowObject(nil,FRootObj);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Clear;
  FreeAndNil(FNodeMap);
end;

procedure TMainForm.ShowObject(AParentNode: TTreeNode; AObject: IJSONObject);
var
  tmpObjNode : TTreeNode;
  tmpStr : string;
  I: Integer;
begin
  if not Assigned(AParentNode) then
  begin
    Clear;
  end;

  tmpStr := AObject.Name;
  tmpObjNode := JSONTreeViewer.Items.AddChildObject(AParentNode,'',Pointer(AObject));
  FNodeMap.Add(AObject,tmpObjNode);
  case AObject.JType of
    jt_unknown: tmpStr := 'jt_unknown = ' + AObject.Text;
    jt_object:
      begin
        tmpStr := tmpStr + ' {} (' + IntToStr(AObject.Count) + ')';
        JSONTreeViewer.Items.AddChild(tmpObjNode,'...');
      end;
    jt_array:
      begin
        tmpStr := tmpStr + ' [] (' + IntToStr(AObject.Count) + ')';
        JSONTreeViewer.Items.AddChild(tmpObjNode,'...');
      end;
    jt_string:
      begin
        if tmpStr <> '' then
          tmpStr := tmpStr + ' : ';
        tmpStr := tmpStr + '"' + AObject.AsString + '"';
      end;
    jt_variant:
      begin
        if tmpStr <> '' then
          tmpStr := tmpStr + ' : ';
        tmpStr := tmpStr + AObject.AsString;
      end;
  end;
  tmpObjNode.Text := tmpStr;
end;

procedure TMainForm.TBOpenClick(Sender: TObject);
begin
  if not OpenDialog1.Execute(handle) then Exit;
  FRootObj := LoadFile(OpenDialog1.FileName);
  ShowObject(nil,FRootObj);
end;

procedure TMainForm.TBGenerationClick(Sender: TObject);
var
  tmpObj : IJSONObject;
begin
  tmpObj := ArrayToJSON(['sdfsdf',234234,3.546,'s',Self,TObject($ff)]);
  tmpObj['NewObj'].Value := 'null';
  ShowObject(nil,tmpObj);
end;

procedure TMainForm.BtnFindNodeClick(Sender: TObject);
var
  Node : TTreeNode;
  tmpObject,tmpLastObj,tmpParentObject : IJSONObject;
  tmpStartIndex : Integer;
  tmpObjectStack : TList<IJSONObject>;
  I: Integer;
begin
  if not Assigned(FRootObj) then Exit;
  tmpStartIndex := 0;
  node := nil;
  JSONTreeViewer.SetFocus;
  if Assigned(JSONTreeViewer.Selected) then
  begin
    if Assigned(JSONTreeViewer.Selected.Parent) then
    begin
      tmpLastObj := IJSONObject(JSONTreeViewer.Selected.Data);
      tmpStartIndex := tmpLastObj.Finish + 1;
    end;
  end;
  tmpObject := FRootObj.Find(ESearch.Text,tmpStartIndex);
  if not Assigned(tmpObject) then Exit;
  tmpObjectStack := TList<IJSONObject>.Create;
  tmpParentObject := tmpObject;
  while Assigned(tmpParentObject) do
  begin
    tmpObjectStack.Add(tmpParentObject);
    tmpParentObject := tmpParentObject.Parent;
  end;

  for I := tmpObjectStack.Count - 1 downto 0 do
  begin
    Node := FNodeMap.Items[tmpObjectStack[I]];
    Node.MakeVisible;
    Node.Expand(false);
    JSONTreeViewer.Selected := Node;
  end;
  FreeAndNil(tmpObjectStack);
end;

function TMainForm.FindNode(Tree: TTreeView; Node: TTreeNode; SubS: String): TTreeNode;
begin
  Result:=nil;
  if not Assigned(Node) then
    Node:=Tree.Items.GetFirstNode;

  while Assigned(Node) Do
  begin
    if Pos(SubS.ToUpperInvariant,Node.Text.ToUpperInvariant) > 0 then Exit(Node);
    Node:=Node.GetNext;
  end;
end;

procedure TMainForm.JSONTreeViewerClick(Sender: TObject);
var
  tmpObj : IJSONObject;
begin
  if not Assigned(JSONTreeViewer.Selected) then Exit;
  tmpObj := IJSONObject(JSONTreeViewer.Selected.Data);
end;

procedure TMainForm.JSONTreeViewerEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  Caption := S;
end;

procedure TMainForm.JSONTreeViewerExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  tmpObject : IJSONObject;
  I: Integer;
begin
  if Node.getFirstChild.Text <> '...' then Exit;
  Node.DeleteChildren;
  tmpObject := IJSONObject(Node.Data);
  for I := 0 to tmpObject.Count - 1 do
    ShowObject(Node,tmpObject.Items[I]);
end;

function TMainForm.LoadFile(AFileName: string): IJSONObject;
var
  tmpCount : Cardinal;
begin
  tmpCount := GetTickCount;
  Result := LoadJSON(AFileName);
  StatusBar1.Panels[0].Text := '¬рем€ разбора: ' + FormatFloat('0.0',(GetTickCount - tmpCount) / 1000) + ' Ёлементов: ' + IntToStr(ElementsCount);
end;

function TMainForm.ParseStr(AStr: string): IJSONObject;
var
  tmpCount : Cardinal;
begin
  tmpCount := GetTickCount;
  Result := ParseJSON(AStr);
  StatusBar1.Panels[0].Text := '¬рем€ разбора: ' + FormatFloat('0.0',(GetTickCount - tmpCount) / 1000) + ' Ёлементов: ' + IntToStr(ElementsCount);
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char; // переменна€, хран€ща€ им€ файла
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH)> 0 then
    begin
      FRootObj := LoadJSON(CFileName);
      ShowObject(nil,FRootObj);
      Msg.Result := 0;
    end;
	finally
	  DragFinish(Msg.Drop); // отпустить файл
	end;
end;

end.
