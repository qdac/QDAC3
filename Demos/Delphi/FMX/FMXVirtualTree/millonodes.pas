unit millonodes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, qdac_fmx_virtualtree;

type
  TfrmMillioNodes = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTreeView: TQVirtualTreeView;
    FCellData: IQVTCellData;
    procedure DoGetCellData(Sender: TQVirtualTreeView; ANode: TQVTNode;
      ACol: Integer; var AData: IQVTCellData);
  public
    { Public declarations }
  end;

var
  frmMillioNodes: TfrmMillioNodes;

implementation

{$R *.fmx}

type
  TMilloNodeData = class(TQVTDefaultCellData, IQVTTextCellData)
  protected
    function GetText: String;
  end;

procedure TfrmMillioNodes.Button1Click(Sender: TObject);
begin
  FTreeView.RootNodeCount := 1000000;
end;

procedure TfrmMillioNodes.DoGetCellData(Sender: TQVirtualTreeView;
  ANode: TQVTNode; ACol: Integer; var AData: IQVTCellData);
begin
  AData := FCellData;
  FCellData.Node := ANode;
  FCellData.Column := ACol;
end;

procedure TfrmMillioNodes.FormCreate(Sender: TObject);
begin
  FTreeView := TQVirtualTreeView.Create(Self);
  FTreeView.Options := [TQVTOption.toTestHover];
  FTreeView.PaintOptions := [TQVTPaintOption.poHorizLine,
    TQVTPaintOption.poVertLine, TQVTPaintOption.poTreeLine,
    TQVTPaintOption.poRowSelection, TQVTPaintOption.poNodeButton];
  FTreeView.Header.Options := [TQVTHeaderOption.hoVisible,
    TQVTHeaderOption.hoResizable];
  FTreeView.Parent := Self;
  FTreeView.OnGetCellData := DoGetCellData;
  FTreeView.Align := TAlignLayout.Client;
  with FTreeView.Header.Columns.Add do
    begin
    Title.Text:='百万结点';
    Title.TextSettings.FontColor:=TAlphaColors.Black;
    Width := 300;
    end;
  FCellData := TMilloNodeData.Create(nil, 0);
end;

{ TMilloNodeDate }

function TMilloNodeData.GetText: String;
begin
  Result := 'Node ' + IntToStr(Node.Index);
end;

end.
