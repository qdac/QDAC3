unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,qstring;

type
  TForm1 = class(TForm)
    Button3: TButton;
    Button1: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function RBTreeTestCompare(p1, p2: Pointer): Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qrbtree,sbtree;//uVirtualMemPool;
{$R *.dfm}

function  TForm1.RBTreeTestCompare(p1,p2:Pointer):Integer;
begin
if IntPtr(p1)<IntPtr(p2) then
  Result:=-1
else if IntPtr(p1)>IntPtr(p2) then
  Result:=1
else
  Result:=0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ATable:TQHashTable;
  I: Integer;
  S:String;
  AItem:PQHashList;
begin
ATable:=TQHashTable.Create();
for I := 0 to 100 do
  ATable.Add(Pointer(I),I);
ATable.Resize(0);
for I := 0 to ATable.BucketCount-1 do
  begin
  AItem:=ATable[I];
  S:=S+'Bucket['+IntToStr(I)+']:'#13#10;
  while AItem<>nil do
    begin
    S:=S+IntToStr(Integer(AItem.Data))+',';
    AItem:=AItem.Next;
    end;
  S:=S+#13#10;
  end;
ShowMessage(S);
ATable.Free;
end;

procedure TForm1.Button3Click(Sender: TObject);
const
  ACount:Integer=4000000;
var
  T1,T2,T3:Cardinal;
  AData:array of Pointer;
  procedure GenerateData;
  var
    I:Integer;
  begin
  SetLength(AData,ACount);
  randomize;
  for I := 1 to ACount do
    AData[I-1]:=Pointer(random(MaxInt));
  end;
  procedure TestSBTree;
  var
    K:Integer;
    tree:TSizeBalancedTree;
  begin
  tree:=TSizeBalancedTree.Create;
  T2:=GetTickCount;
  for K := 1 to ACount do
    tree.insert(Integer(AData[K-1]));
//  T2:=GetTickCount;
  for K := 1 to ACount do
    assert(tree.find(Integer(AData[K-1])),'Error in SBTree');
//    tree.add(Integer(AData[K-1]));
  T2:=GetTickCount-T2;
  tree.Free;
  end;
  procedure TestLinuxRBTree;
  var
    I:Integer;
    ATree:TQRBTree;
  begin
  ATree:=TQRBTree.Create(RBTreeTestCompare);
  T1:=GetTickCount;
  for I := 1 to ACount do
    ATree.Insert(AData[I-1]);
  for I := 1 to ACount do
    assert(ATree.Find(AData[I-1]).Data=AData[I-1],'Error in RBTree');
  T1:=GetTickCount-T1;
  ATree.Free;
  end;
  procedure TestHashTable;
  var
    I:Integer;
    ATable:TQHashTable;
  begin
  ATable:=TQHashTable.Create();
  T3:=GetTickCount;
  for I := 1 to ACount do
    ATable.Add(AData[I-1],Integer(AData[I-1]));
  ATable.Resize(0);
  for I := 1 to ACount do
    assert(ATable.Exists(AData[I-1],Integer(AData[I-1])),'Error in HashTable');
  T3:=GetTickCount-T3;
  ATable.Free;
  end;
begin
GenerateData;
TestLinuxRBTree;
TestSBTree;
TestHashTable;
ShowMessage('RBTree='+IntToStr(T1)+'ms('+IntToStr(ACount*Int64(1000) div T1)+') SBTree='+IntToStr(T2)+'ms('+IntToStr(ACount*Int64(1000) div T2)+')'+
  #13#10'(T2-T1)*100/T2='+IntToStr((T2-T1)*100 div T2)+',HashTable='+
  IntToStr(T3)+'ms,(T3-T1)*100/T3='+IntToStr((T3-T1)*100 div T3));
end;

end.
