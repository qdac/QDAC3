unit dlgmemstatics;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls,qmemstatics;

type
  TfrmMemStatics = class(TForm)
    pnlHeader: TPanel;
    btnStart: TButton;
    tsStatics: TPageControl;
    tsSummary: TTabSheet;
    tsChart: TTabSheet;
    cbxChartValueType: TComboBox;
    lblSortBy: TLabel;
    mmSummary: TMemo;
    imgChart: TImage;
    chkIgnoreZeros: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure cbxChartValueTypeChange(Sender: TObject);
    procedure tsChartResize(Sender: TObject);
    procedure chkIgnoreZerosClick(Sender: TObject);
  private
    { Private declarations }
    FStatics: TMemStatics;
    FStaticDone: Boolean;
    procedure CreateChart;
    procedure GenerateReport;
    procedure SetStatics(const Value: TMemStatics);
  public
    { Public declarations }
    property Statics:TMemStatics read FStatics write SetStatics;
  end;

var
  frmMemStatics: TfrmMemStatics;
  procedure ShowMemoryStatics;
implementation
uses qstring;
{$R *.dfm}

procedure TfrmMemStatics.btnStartClick(Sender: TObject);
begin
FStaticDone := True;
GetMemStatics(FStatics, ssmNone);
cbxChartValueTypeChange(Sender);
end;

procedure TfrmMemStatics.cbxChartValueTypeChange(Sender: TObject);
begin
if FStaticDone then
  begin
  GenerateReport;
  CreateChart;
  end
else
  btnStartClick(Sender);
end;

procedure TfrmMemStatics.chkIgnoreZerosClick(Sender: TObject);
begin
if FStaticDone then
  CreateChart;
end;

procedure TfrmMemStatics.CreateChart;
var
  ABitmap:TBitmap;
  I,AValue,AMaxValue,X,Y,W,H,ACount:Integer;
  LX,LW,LH:Integer;
  TS:TSize;
  AXScale,AYScale:Double;
  S:String;
  AIgnoreZeros:Boolean;
begin
ABitmap:=TBitmap.Create;
try
  W:=imgChart.ClientWidth;
  H:=imgChart.ClientHeight;
  ABitmap.SetSize(W,H);
  ABitmap.Canvas.Font.Assign(Font);
  AMaxValue:=0;
  ACount:=0;
  for I := Low(FStatics) to High(FStatics) do
    begin
    case cbxChartValueType.ItemIndex of
      1:
        AValue:=FStatics[I].Statics.FreeTimes;
      2:
        AValue:=FStatics[I].Statics.Count;
      3:
        AValue:=FStatics[I].Statics.MaxCount
      else
        AValue:=FStatics[I].Statics.AllocateTimes;
    end;
    if AValue>AMaxValue then
      AMaxValue:=AValue;
    if AValue<>0 then
      Inc(ACount);
    end;
  if (ACount>0) and (AMaxValue>0) then
    begin
    //‘§¡Ù±Íµ„øÌ∂»
    S:=IntToStr(AMaxValue)+'.00';
    TS:=ABitmap.Canvas.TextExtent(S);
    LW:=TS.cx+10;
    LH:=30;
    AIgnoreZeros:=chkIgnoreZeros.Checked;
    if AIgnoreZeros then
      AXScale:=(W-LW)/ACount
    else
      begin
      ACount:=Length(FStatics);
      repeat
        case cbxChartValueType.ItemIndex of
          1:
            AValue:=FStatics[ACount-1].Statics.FreeTimes;
          2:
            AValue:=FStatics[ACount-1].Statics.Count;
          3:
            AValue:=FStatics[ACount-1].Statics.MaxCount
          else
            AValue:=FStatics[ACount-1].Statics.AllocateTimes;
        end;
        if AValue<>0 then
          Break;
        Dec(ACount);
      until ACount=0;
      AXScale:=(W-LW)/ACount;
      end;
    AYScale:=(ABitmap.Height-LH)/AMaxValue;
    ABitmap.Canvas.Pen.Width:=2;
    X:=LW-5;
    Y:=H-(LH shr 1);
    ABitmap.Canvas.MoveTo(X,5);
    ABitmap.Canvas.LineTo(X,Y);
    ABitmap.Canvas.LineTo(W-5,Y);
    ABitmap.Canvas.Pen.Width:=1;
    ABitmap.Canvas.Pen.Color:=RGB(220,220,220);
    I:=X+5;
    ACount:=0;
    //X÷·
    while I<W-5 do
      begin
      if (ACount>0) and ((ACount mod 10)=0) then
        ABitmap.Canvas.Pen.Width:=2
      else
        ABitmap.Canvas.Pen.Width:=1;
      ABitmap.Canvas.MoveTo(I,5);
      ABitmap.Canvas.LineTo(I,H-15);
      Inc(I,5);
      Inc(ACount);
      end;
    //Y÷·
    I:=Y-5;
    ACount:=0;
    while I>5 do
      begin
      if (ACount>0) and ((ACount mod 10)=0) then
        begin
        ABitmap.Canvas.Pen.Width:=2;
        S:=FormatFloat('0.##',AMaxValue*(Y-5-I)/(H-LH));
        TS:=ABitmap.Canvas.TextExtent(S);
        ABitmap.Canvas.TextOut(LW-7-TS.cx,I-(TS.cy shr 1),S);
        end
      else
        ABitmap.Canvas.Pen.Width:=1;
      ABitmap.Canvas.MoveTo(X,I);
      ABitmap.Canvas.LineTo(W-5,I);
      Dec(I,5);
      Inc(ACount);
      end;
    ABitmap.Canvas.Pen.Color:=clBlue;
    ACount:=0;
    LX:=0;
    for I := Low(FStatics) to High(FStatics) do
      begin
      case cbxChartValueType.ItemIndex of
        1:
          AValue:=FStatics[I].Statics.FreeTimes;
        2:
          AValue:=FStatics[I].Statics.Count;
        3:
          AValue:=FStatics[I].Statics.MaxCount
        else
          AValue:=FStatics[I].Statics.AllocateTimes;
      end;
      if AValue<>0 then
        begin
        SetBKMode(Canvas.Handle,TRANSPARENT);
        if not AIgnoreZeros then
          X:=LW-5+Trunc(I*AXScale)
        else
          X:=LW-5+Trunc(ACount*AXScale);
        Y:=(ABitmap.Height-(LH shr 1))-Trunc(AValue*AYScale);
        ABitmap.Canvas.MoveTo(X,ABitmap.Height-(LH shr 1));
        ABitmap.Canvas.LineTo(X,Y);
        Inc(ACount);
        if FStatics[I].Size<>-1 then
          S:=IntToStr(FStatics[I].Size)+'B'
        else
          S:='>='+IntToStr(MaxStaticMemSize)+'B';
        TS:=ABitmap.Canvas.TextExtent(S);
        if (ACount>0) and ((X-LX)>TS.cx) then
          begin
          LX:=X-(TS.cx div 2);
          ABitmap.Canvas.TextOut(LX,H-14,S);
          ABitmap.Canvas.MoveTo(X,Y);
          Inc(LX,ts.cx);
          end;
        end;
      end;
    end;
  imgChart.Picture.Assign(ABitmap);
finally
  ABitmap.Free;
end;
end;

procedure TfrmMemStatics.GenerateReport;
var
  ACopy: TMemStatics;
begin
ACopy := Copy(FStatics, Low(FStatics), High(FStatics) + 1);
case cbxChartValueType.ItemIndex of
  -1,0:
    SortMemStatics(ACopy, ssmAllocateTimes);
  1:
    SortMemStatics(ACopy, ssmFreeTimes);
  2:
    SortMemStatics(ACopy, ssmCount);
  3:
    SortMemStatics(ACopy, ssmMaxCount);
end;
mmSummary.Text := GetMemStaticsText(ACopy);
end;

procedure TfrmMemStatics.SetStatics(const Value: TMemStatics);
begin
  FStatics := Value;
  FStaticDone:=Length(Value)>0;
  if FStaticDone then
    GenerateReport;
end;

procedure TfrmMemStatics.tsChartResize(Sender: TObject);
begin
CreateChart;
end;
procedure ShowMemoryStatics;
var
  AStatics:TMemStatics;
  F:TfrmMemStatics;
begin
GetMemStatics(AStatics,ssmNone);
F:=TfrmMemStatics.Create(Application);
F.Statics:=AStatics;
F.ShowModal;
FreeObject(F);
end;
end.
