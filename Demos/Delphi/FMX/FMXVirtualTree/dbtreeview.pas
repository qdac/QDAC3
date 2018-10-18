unit dbtreeview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, qdac_fmx_virtualtree, Data.DB,
  Datasnap.DBClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmDBTree = class(TForm)
    vtDBTree: TQVirtualTreeView;
    QVTDBAdapter1: TQVTDBAdapter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTreeView: TQVirtualTreeView;
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
  vtDBTree.Options := frmMain.vtGrid.Options;
  vtDBTree.PaintOptions := frmMain.vtGrid.PaintOptions;
  vtDBTree.Header.Options := frmMain.vtGrid.Header.Options;
  vtDBTree.TextSettings.WordWrap := False;
  FDataSet := TClientDataSet.Create(Self);
  FDataSet.CloneCursor(frmMain.adsData, true);
  for I := 0 to frmMain.adsData.Fields.Count - 1 do
  begin
    ASource := frmMain.adsData.Fields[I];
    ADest := FDataSet.FieldByName(ASource.FieldName);
    ADest.Visible := ASource.Visible;
    ADest.DisplayLabel := ASource.DisplayLabel;
  end;
  QVTDBAdapter1.DataSet := FDataSet;
  QVTDBAdapter1.KeyField := FDataSet.FieldByName('Id');
  QVTDBAdapter1.ParentField := FDataSet.FieldByName('Parent');
end;

end.
