unit dbtreeview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, qdac_fmx_virtualtree, qdac_fmx_vtdbadapter, Data.DB,
  Datasnap.DBClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TfrmDBTree = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTreeView: TQVirtualTreeView;
    FAdapter: TQDBTreeAdapter;
    FDataSet: TClientDataSet;
  public
    { Public declarations }
  end;

var
  frmDBTree: TfrmDBTree;

implementation

uses main;
{$R *.fmx}

procedure TfrmDBTree.FormCreate(Sender: TObject);
var
  I: Integer;
  ASource, ADest: TField;
begin
  FTreeView := TQVirtualTreeView.Create(Self);
  FTreeView.Options := [TQVTOption.toTestHover, toRowSizable];
  FTreeView.PaintOptions := [TQVTPaintOption.poHorizLine,
    TQVTPaintOption.poVertLine, TQVTPaintOption.poTreeLine,
    TQVTPaintOption.poColSelection, TQVTPaintOption.poRowSelection,
    TQVTPaintOption.poNodeButton, TQVTPaintOption.poHover];
  FTreeView.Header.Options := [TQVTHeaderOption.hoVisible,
    TQVTHeaderOption.hoResizable];
  FTreeView.Parent := Self;
  FTreeView.TextSettings.WordWrap := False;
  FTreeView.Align := TAlignLayout.Client;
  FDataSet := TClientDataSet.Create(Self);
  FDataSet.CloneCursor(frmMain.adsData, true);
  for I := 0 to frmMain.adsData.Fields.Count - 1 do
  begin
    ASource := frmMain.adsData.Fields[I];
    ADest := FDataSet.FieldByName(ASource.FieldName);
    ADest.Visible := ASource.Visible;
    ADest.DisplayLabel := ASource.DisplayLabel;
  end;
  FAdapter := TQDBTreeAdapter.Create(FDataSet.FieldByName('Id'),
    FDataSet.FieldByName('Parent'));
  FAdapter.TreeView := FTreeView;
  ASource := FDataSet.FieldByName('Sold');
  FAdapter.SetFieldCellType(ASource, TQDBProgressCellData.Create(ASource));
  ASource := FDataSet.FieldByName('Packable');
  FAdapter.SetFieldCellType(ASource, TQDBCheckCellData.Create(ASource));
  ASource := FDataSet.FieldByName('Status');
  FAdapter.SetFieldCellType(ASource, TQDBRadioCellData.Create(ASource));
end;

end.
