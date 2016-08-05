unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs,
  VclTee.Chart, qmemstatics, Data.DB, Data.Win.ADODB, Vcl.Grids, Vcl.DBGrids;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chart1: TChart;
    Series1: TLineSeries;
    cbxChartValueType: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure cbxChartValueTypeChange(Sender: TObject);
  private
    { Private declarations }
    FStatics: TMemStatics;
    FStaticDone: Boolean;
    procedure CreateChart;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
FStaticDone := True;
GetMemStatics(FStatics, ssmNone);
cbxChartValueTypeChange(Sender);
end;

procedure TForm2.cbxChartValueTypeChange(Sender: TObject);
var
  ACopy: TMemStatics;
begin
if FStaticDone then
  begin
  ACopy := Copy(FStatics, Low(FStatics), High(FStatics) + 1);
  case cbxChartValueType.ItemIndex of
    0:
      SortMemStatics(ACopy, ssmAllocateTimes);
    1:
      SortMemStatics(ACopy, ssmFreeTimes);
    2:
      SortMemStatics(ACopy, ssmCount);
    3:
      SortMemStatics(ACopy, ssmMaxCount);
  end;
  Memo1.Text := GetMemStaticsText(ACopy);
  CreateChart;
  end
else
  Button1Click(Sender);
end;

procedure TForm2.CreateChart;
var
  I: Integer;
  S: String;
begin
Series1.Clear;
for I := Low(FStatics) to High(FStatics) do
  begin
  if FStatics[I].Statics.AllocateTimes > 0 then
    begin
    if FStatics[I].Size = -1 then
      S := '>=' + IntToStr(MaxStaticMemSize) + 'B'
    else
      S := IntToStr(FStatics[I].Size) + 'B';
    case cbxChartValueType.ItemIndex of
      0, 4:
        Series1.Add(FStatics[I].Statics.AllocateTimes, S);
      1:
        Series1.Add(FStatics[I].Statics.FreeTimes, S);
      2:
        Series1.Add(FStatics[I].Statics.Count, S);
      3:
        Series1.Add(FStatics[I].Statics.MaxCount, S);
    end;
    end;
  end;
end;

end.
