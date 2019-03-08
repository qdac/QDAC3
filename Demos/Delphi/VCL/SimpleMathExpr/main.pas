unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QMathExpr,
  Vcl.ExtCtrls, QString, QJson;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Calc: TButton;
    Button1: TButton;
    chkStdDiv: TCheckBox;
    Panel2: TPanel;
    Memo1: TMemo;
    ListBox1: TListBox;
    Panel3: TPanel;
    chkNumIdentAsMultiply: TCheckBox;
    Button2: TButton;
    procedure CalcClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FExpr: IQMathExpression;
    FJsonVars: TQJson;
    class procedure DoGetYValue(Sender: TObject; AVar: TQMathVar); static;
    class procedure DoStableValue(Sender: TObject; AVar: TQMathVar); static;
    procedure DoGetJsonVar(Sender: IQMathExpression; const AVarName: String;
      var AVar: TQMathVar);
    class procedure DoGetJsonValue(Sender: TObject; AVar: TQMathVar); static;
  public
    { Public declarations }
  end;

  TQJsonVarHelper = class helper for TQJson
  private
    procedure DoGetJsonVar(Sender: IQMathExpression; const AVarName: String;
      var AVar: TQMathVar);
    class procedure DoGetJsonValue(Sender: TObject; AVar: TQMathVar); static;
    function GetAsExpression: Variant;
  public
    property AsExpression: Variant read GetAsExpression;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
const
  TestExpr: array [0 .. 12] of String = ( //
    '1+2*3+4', //
    '1/2+3-4', //
    '1*(2+3)-4', //
    '10*120/180*0.3', '20*((1+2*0.2)*(2+0.1)+2)',
    '(((2.8+2.8)*0.25+(0.5+0.55)*0)*2)*1', //
    'Max(1, 2, 3, 4, 5)', //
    '1+Max(2,3)', //
    'Max(2,3)+1', //
    '1+Max(2,3)+4', //
    'Avg(2,3)+Max(2,3)', //
    '"abc"+"def"', //
    '"abc"+"1"');
begin
  FExpr.UseStdDiv := chkStdDiv.Checked;
  FExpr.NumIdentAsMultiply := chkNumIdentAsMultiply.Checked;
  for I := 0 to High(TestExpr) do
  begin
    Memo1.Lines.Add(TestExpr[I] + '=' + VarToStr(FExpr.Eval(TestExpr[I])));
  end;
  if FExpr.NumIdentAsMultiply then
  begin
    Memo1.Lines.Add('2PI+1=' + VarToStr(FExpr.Eval('2PI+1')));
    Memo1.Lines.Add('1+2PI=' + VarToStr(FExpr.Eval('1+2PI')));
    Memo1.Lines.Add('1+2PI+2=' + VarToStr(FExpr.Eval('1+2PI+2')));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AItem: TQJson;
begin
  AItem := FJsonVars.ItemByName('d');
  Memo1.Lines.Add(AItem.AsString + '=>' + VarToStr(AItem.AsExpression));
end;

procedure TForm1.CalcClick(Sender: TObject);
begin
  FExpr.UseStdDiv := chkStdDiv.Checked;
  FExpr.NumIdentAsMultiply := chkNumIdentAsMultiply.Checked;
  Memo1.Lines.Add(Edit1.Text + '=' + VarToStr(FExpr.Eval(Edit1.Text)));
end;

class procedure TForm1.DoGetJsonValue(Sender: TObject; AVar: TQMathVar);
var
  AJson, AItem: TQJson;
begin
  AJson := TQJson(AVar.Tag);
  if AJson.HasChild(AVar.Name, AItem) then
    AVar.Value := AItem.AsVariant
  else
    AVar.Value := Unassigned;
end;

procedure TForm1.DoGetJsonVar(Sender: IQMathExpression; const AVarName: String;
  var AVar: TQMathVar);
begin
  AVar := Sender.Add(AVarName, 0, 0, DoGetJsonValue);
  AVar.Tag := FJsonVars;
end;

class procedure TForm1.DoGetYValue(Sender: TObject; AVar: TQMathVar);
var
  S: String;
begin
  if InputQuery('输入 Y 的值', '计算表达式需要知道Y值：', S) then
    AVar.Value := StrToFloat(S);
end;

class procedure TForm1.DoStableValue(Sender: TObject; AVar: TQMathVar);
var
  V: Integer;
begin
  V := Random(MaxInt);
  Form1.Memo1.Lines.Add('ST=' + IntToStr(V));
  AVar.Value := V;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I, J: Integer;
  AVar: TQMathVar;
  S: String;
begin
  ReportMemoryLeaksOnShutdown := True;
  FJsonVars := TQJson.Create;
  FJsonVars.Parse('{"a":{"b":1,"c":2},"f":20}');
  // d的值通过表达式计算赋值
  FJsonVars.Add('d').AsString := 'f+$a.c';
  Memo1.Lines.Add('Json 变量:'#13#10 + FJsonVars.AsJson);
  FExpr := TQMathExpression.Create;
  FExpr.OnLookupMissed := DoGetJsonVar;
  FExpr.Add('X', mvImmutable).Value := 100;
  FExpr.Add('Y').OnGetValue := DoGetYValue;
  FExpr.Add('ST', mvStable).OnGetValue := DoStableValue;
  for I := 0 to FExpr.VarCount - 1 do
  begin
    AVar := FExpr.Vars[I];
    S := AVar.Name;
    if AVar.MaxParams > 0 then
    begin
      S := S + '(';
      for J := 0 to AVar.MinParams - 1 do
        S := S + 'p' + IntToStr(J + 1) + ',';
      if AVar.MaxParams > AVar.MinParams then //
      begin
        if AVar.MinParams > 0 then
          SetLength(S, Length(S) - 1);
        if AVar.MaxParams = MaxInt then
          S := S + '[,...pN])'
        else if AVar.MaxParams - AVar.MinParams = 1 then
        begin
          if AVar.MinParams > 0 then
            S := S + '[,p' + IntToStr(AVar.MaxParams) + '])'
          else
            S := S + '[p' + IntToStr(AVar.MaxParams) + '])';
        end
        else if AVar.MinParams = 0 then
          S := S + '[p0...p' + IntToStr(AVar.MaxParams) + '])'
        else
          S := S + '[...p' + IntToStr(AVar.MaxParams) + '])';
      end
      else
      begin
        SetLength(S, Length(S) - 1);
        S := S + ')';
      end
    end;
    ListBox1.Items.Add(S);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FJsonVars);
end;

{ TQJsonVarHelper }

class procedure TQJsonVarHelper.DoGetJsonValue(Sender: TObject;
  AVar: TQMathVar);
var
  AItem: TQJson;
begin
  AItem := TQJson(AVar.Tag);
  if Assigned(AItem) then
    AVar.Value := AItem.AsVariant
  else
    AVar.Value := Unassigned;
end;

procedure TQJsonVarHelper.DoGetJsonVar(Sender: IQMathExpression;
  const AVarName: String; var AVar: TQMathVar);
var
  AJson, AItem: TQJson;
begin
  AJson := TQJson(Sender.Tag);
  if StartWithW(PWideChar(AVarName), '$', false) then
    AItem := AJson.ItemByPath(RightStrW(AVarName,
      Length(AVarName) - 1, false))
  else
    AItem := AJson.ItemByPath(AVarName);
  if Assigned(AItem) then
  begin
    AVar := Sender.Add(AVarName, 0, 0, DoGetJsonValue);
    AVar.Tag := AItem;
  end;
end;

function TQJsonVarHelper.GetAsExpression: Variant;
var
  AExpr: IQMathExpression;
begin
  AExpr := TQMathExpression.Create;
  AExpr.OnLookupMissed := DoGetJsonVar;
  AExpr.Tag := Parent;
  Result := AExpr.Eval(Value);
end;

end.
