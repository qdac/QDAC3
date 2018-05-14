unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, qdac_fmx_virtualtree;

type
  TForm2 = class(TForm)
    QVirtualTreeView1: TQVirtualTreeView;
    tcName: TQVTCustomTextCell;
    tcSeqNo: TQVTCustomTextCell;
    pcAge: TQVTCustomProgressCell;
    rcSex: TQVTCustomRadioCell;
    RadioButton1: TRadioButton;
    teName: TQVTTextEditor;
    deAge: TQVTDialogEditor;
    Label1: TLabel;
    procedure QVirtualTreeView1GetCellSpans(ASender: TQVirtualTreeView;
      ANode: TQVTNode; ACol: Integer; var ASpans: Integer);
    procedure QVTTextCellData1GetText(ASender: TObject; var AText: string);
    procedure tcSeqNoGetText(ASender: TObject; var AText: string);
    procedure deAgeShowEditDialog(Sender: TObject);
    procedure tcNameGetText(ASender: TObject; var AText: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.QVirtualTreeView1GetCellSpans(ASender: TQVirtualTreeView;
  ANode: TQVTNode; ACol: Integer; var ASpans: Integer);
begin
  //返回要合并的后续列数
  if ((ANode.Index and $1) = 0) and (ACol = 0) then
    ASpans := 1;
end;

procedure TForm2.deAgeShowEditDialog(Sender: TObject);
begin
  ShowMessage('Haha!I am here!');
end;

procedure TForm2.QVTTextCellData1GetText(ASender: TObject; var AText: string);
begin
  // AText:='Row_'+IntToStr(TQVTTextCellData(ASender).Node.Index);
end;

procedure TForm2.tcNameGetText(ASender: TObject; var AText: string);
begin
    AText := '姓名'+IntToStr(TQVTCustomTextCell(ASender).Node.Index + 1);
end;

procedure TForm2.tcSeqNoGetText(ASender: TObject; var AText: string);
begin
  if (TQVTCustomTextCell(ASender).Node.Index and 1) = 0 then
    AText := '序号:' + IntToStr(TQVTCustomTextCell(ASender).Node.Index + 1) +
      ',当前结果是一个合并的单元格的内容项目'
  else
    AText := '序号:'+IntToStr(TQVTCustomTextCell(ASender).Node.Index + 1);
end;

end.
