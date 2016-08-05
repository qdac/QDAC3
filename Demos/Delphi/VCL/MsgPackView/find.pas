unit find;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, virtualtrees,
  RegularExpressionsCore, QMsgPack;

type
  TfrmFind = class(TForm)
    Label1: TLabel;
    edtKey: TEdit;
    gbxRanges: TGroupBox;
    chkIncNodeName: TCheckBox;
    chkIncText: TCheckBox;
    GroupBox2: TGroupBox;
    chkByCaseSensitive: TCheckBox;
    chkByRegex: TCheckBox;
    btnNext: TButton;
    btnPrior: TButton;
    btnList: TButton;
    chkRestart: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnListClick(Sender: TObject);
  private
    { Private declarations }
    FRegex: TPerlRegEx;
    function IsMatch(AMsgPack: TQMsgPack): Boolean;
    function CanSearch: Boolean;
    procedure DoSearch(AGoForward, ADoList: Boolean);
  public
    { Public declarations }
  end;

var
  frmFind: TfrmFind;

implementation

uses main, qstring;
{$R *.dfm}

type
  TVSTMoveFunction = function(Node: PVirtualNode;
    ConsiderChildrenAbove: Boolean): PVirtualNode of object;

procedure TfrmFind.btnListClick(Sender: TObject);
begin
  if CanSearch then
  begin
    chkRestart.Checked := True;
    frmMain.lbxSearched.Items.BeginUpdate;
    try
      frmMain.lbxSearched.Items.Clear;
      DoSearch(True, True);
      if frmMain.lbxSearched.Items.Count > 0 then
      begin
        frmMain.lbxSearched.ItemIndex := 0;
        frmMain.lbxSearchedClick(frmMain.lbxSearched);
        frmMain.pnlSearched.Visible := True;
      end
      else
        Application.MessageBox('没有找到匹配的项目', ':(找不到你要的内容):',
          MB_OK or MB_ICONSTOP);
    finally
      frmMain.lbxSearched.Items.EndUpdate;
      Close;
    end;
  end;
end;

procedure TfrmFind.btnNextClick(Sender: TObject);
begin
  if CanSearch then
    DoSearch(True, false);
end;

procedure TfrmFind.btnPriorClick(Sender: TObject);
begin
  if CanSearch then
    DoSearch(false, false);
end;

function TfrmFind.CanSearch: Boolean;
var
  I: Integer;
  ACheck: TCheckBox;
begin
  Result := false;
  if Length(edtKey.Text) = 0 then
  begin
    Application.MessageBox('请输入要查询的关键词', '错误', MB_OK or MB_ICONSTOP);
    Exit;
  end;
  for I := 0 to gbxRanges.ControlCount - 1 do
  begin
    ACheck := gbxRanges.Controls[I] as TCheckBox;
    if ACheck.Checked then
    begin
      Result := True;
      Break;
    end;
  end;
  if not Result then
    Application.MessageBox('请选择要查询的内容范围', '错误', MB_OK or MB_ICONSTOP);
end;

procedure TfrmFind.DoSearch(AGoForward, ADoList: Boolean);
var
  AFound: Boolean;
  procedure SelectNode(ANode: PVirtualNode);
  begin
    frmMain.vstItems.FocusedNode := ANode;
    frmMain.vstItems.Selected[ANode] := True;
  end;

  function InternalSearch(ANode: PVirtualNode): Boolean;
  var
    AMove: TVSTMoveFunction;
    AMsgPack: PQMsgPack;
  begin
    Result := false;
    if AGoForward then
      AMove := frmMain.vstItems.GetNext
    else
      AMove := frmMain.vstItems.GetPrevious;
    ANode := AMove(ANode, false);
    while ANode <> nil do
    begin
      AMsgPack := frmMain.vstItems.GetNodeData(ANode);
      if IsMatch(AMsgPack^) then
      begin
        Result := True;
        if ADoList then
          frmMain.lbxSearched.Items.AddObject(AMsgPack.Path, TObject(ANode))
        else
        begin
          SelectNode(ANode);
          Break;
        end;
      end;
      ANode := AMove(ANode, false);
    end;
  end;

begin
  if chkByRegex.Checked then
  begin
    if FRegex = nil then
      FRegex := TPerlRegEx.Create;
    FRegex.RegEx := edtKey.Text;
    FRegex.Compile;
    if not FRegex.Compiled then
    begin
      Application.MessageBox('输入的表达式不是有效的正则表达式，请检查后重新输入或取消正则表达式选项', '错误',
        MB_OK or MB_ICONSTOP);
      Exit;
    end;
    if chkByCaseSensitive.Checked then
      FRegex.Options := FRegex.Options - [preCaseLess]
    else
      FRegex.Options := FRegex.Options + [preCaseLess];
  end;
  if chkRestart.Checked then
    AFound := InternalSearch(frmMain.vstItems.GetFirst())
  else if frmMain.vstItems.FocusedNode <> nil then
    AFound := InternalSearch(frmMain.vstItems.FocusedNode)
  else
    AFound := InternalSearch(frmMain.vstItems.GetFirst());
  if not AFound then
    Application.MessageBox('我找呀找，就是找不着。', '失败了:(', MB_OK or MB_ICONSTOP);
end;

procedure TfrmFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

function TfrmFind.IsMatch(AMsgPack: TQMsgPack): Boolean;
  function CheckMatch(const S: QStringW): Boolean;
  var
    ASub: QStringW;
  begin
    ASub := edtKey.Text;
    if chkByRegex.Checked then
    begin
      FRegex.Subject := S;
      Result := FRegex.Match;
    end
    else
    begin
      if chkByCaseSensitive.Checked then
        Result := (StrStrW(PWideChar(S), PWideChar(ASub)) <> nil)
      else
        Result := (StrIStrW(PWideChar(S), PWideChar(ASub)) <> nil);
    end;
  end;

begin
  Result := false;
  if chkIncNodeName.Checked then
  begin
    Result := CheckMatch(AMsgPack.Name);
    if Result then
      Exit;
  end;
  if chkIncText.Checked and (AMsgPack.Count = 0) then
  begin
    Result := CheckMatch(AMsgPack.AsString);
    if Result then
      Exit;
  end;
end;

end.
