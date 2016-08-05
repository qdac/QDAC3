unit main;

interface

uses Windows,Forms,SysUtils,Classes, Controls, StdCtrls,Dialogs;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    function DoSortCompare(v1,v2:Pointer):Integer;
    function FindInList(AList:TList;v:Integer):Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qrbtree;
{$R *.dfm}
function DoSortCompare1(v1,v2:Pointer):Integer;
begin
if NativeInt(v1)>NativeInt(v2) then
  Result:=1
else if NativeInt(v1)<NativeInt(v2) then
  Result:=-1
else
  Result:=0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I,AIndex:Integer;
  AList:TList;
  ATree:TQRBTree;
  T1,T2,T3,T4:Cardinal;
  AData:array of Integer;
const
  ACount:Integer=200000;
begin
SetLength(AData,ACount);
for I := 0 to ACount-1 do
  AData[I]:=Random(MaxInt);
AList:=TList.Create;
T1:=GetTickCount;
for I := 0 to ACount-1 do
//  AList.Insert(0,Pointer(AData[I]));
//  begin
//  AList.Add(Pointer(AData[I]));
//  AList.Sort(DoSortCompare1);
//  end;
  begin
  AList.Insert(FindInList(AList,AData[I]),Pointer(AData[I]));
  end;
//AList.Sort(DoSortCompare1);
T1:=GetTickCount-T1;
T3:=0;
T3:=GetTickCount;
for I := 0 to ACount-1 do
  FindInList(AList,AData[I]);
AList.Sort(DoSortCompare1);
T3:=GetTickCount-T3;
AList.Free;
ATree:=TQRBTree.Create(DoSortCompare);
T2:=GetTickCount;
for I := 0 to ACount-1 do
  ATree.Insert(Pointer(AData[I]));
T2:=GetTickCount-T2;
T4:=GetTickCount;
for I := 0 to ACount-1 do
  ATree.Find(Pointer(AData[I]));
T4:=GetTickCount-T4;
ATree.Free;
ShowMessage('List '#13#10+
  ' Add+Sort:'+IntToStr(T1)+'ms,Find:'+IntToStr(T3)+'ms,Total='+IntToStr(T1+T3)+'ms,'#13#10+
  ' RBTree'#13#10+
  ' Add+Sort:'+IntToStr(T2)+'ms,Find:'+IntToStr(T4)+'ms,Total='+IntToStr(T2+T4)+'ms');
end;

function TForm1.DoSortCompare(v1, v2: Pointer): Integer;
begin
if NativeInt(v1)>NativeInt(v2) then
  Result:=1
else if NativeInt(v1)<NativeInt(v2) then
  Result:=-1
else
  Result:=0;
end;

function TForm1.FindInList(AList: TList; v: Integer): Integer;
var
  L, H, I, C: Integer;
begin
  Result := 0;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := DoSortCompare(AList[I],Pointer(v));
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := L;
        Break;
      end;
    end;
  end;

end;

end.
