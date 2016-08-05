unit superobjecthelper;

{$I 'qdac.inc'}

interface
uses qstring,qjson,qrbtree;
type
  TSuperArrayHelper=class;
  TSuperObjectHelper=class(TQHashedJson)
  private
    function GetB(const k: QStringW): Boolean;
    function GetC(const k: QStringW): Currency;
    function GetD(const k: QStringW): Double;
    function GetI(const k: QStringW): Int64;
    function GetN(const k: QStringW): TSuperArrayHelper;
    function GetO(const k: QStringW): TSuperObjectHelper;
    function GetS(const k: QStringW): QStringW;
    procedure PutB(const k: QStringW; const Value: Boolean);
    procedure PutC(const k: QStringW; const Value: Currency);
    procedure PutD(const k: QStringW; const Value: Double);
    procedure PutI(const k: QStringW; const Value: Int64);
    procedure PutN(const k: QStringW; const Value: TSuperArrayHelper);
    procedure PutO(const k: QStringW; const Value: TSuperObjectHelper);
    procedure PutS(const k: QStringW; const Value: QStringW);
  protected
  public
    constructor Create;
    property O[const k: QStringW]: TSuperObjectHelper read GetO write PutO; default;
    property S[const k: QStringW]: QStringW read GetS write PutS;
    property I[const k: QStringW]: Int64 read GetI write PutI;
    property D[const k: QStringW]: Double read GetD write PutD;
    property B[const k: QStringW]: Boolean read GetB write PutB;
    property N[const k: QStringW]: TSuperArrayHelper read GetN write PutN;
    property C[const k: QStringW]: Currency read GetC write PutC;
  end;

  TSuperArrayHelper=class(TQHashedJson)
  private
    function GetB(const k: Integer): Boolean;
    function GetC(const k: Integer): Currency;
    function GetD(const k: Integer): Double;
    function GetI(const k: Integer): Int64;
    function GetN(const k: Integer): TSuperArrayHelper;
    function GetO(const k: Integer): TSuperObjectHelper;
    function GetS(const k: Integer): QStringW;
    procedure PutB(const k: Integer; const Value: Boolean);
    procedure PutC(const k: Integer; const Value: Currency);
    procedure PutD(const k: Integer; const Value: Double);
    procedure PutI(const k: Integer; const Value: Int64);
    procedure PutN(const k: Integer; const Value: TSuperArrayHelper);
    procedure PutO(const k: Integer; const Value: TSuperObjectHelper);
    procedure PutS(const k: Integer; const Value: QStringW);
    procedure ExpandSize(l:Integer);
  protected
  public
    constructor Create;overload;
    property O[const k: Integer]: TSuperObjectHelper read GetO write PutO; default;
    property S[const k: Integer]: QStringW read GetS write PutS;
    property I[const k: Integer]: Int64 read GetI write PutI;
    property D[const k: Integer]: Double read GetD write PutD;
    property B[const k: Integer]: Boolean read GetB write PutB;
    property N[const k: Integer]: TSuperArrayHelper read GetN write PutN;
    property C[const k: Integer]: Currency read GetC write PutC;
  end;


implementation
uses dialogs,sysutils;
{ TSuperObjectHelper }

{ TSuperObjectHelper }

constructor TSuperObjectHelper.Create;
begin
inherited;
end;

function TSuperObjectHelper.GetB(const k: QStringW): Boolean;
begin
Result:=ForcePath(k).AsBoolean;
end;

function TSuperObjectHelper.GetC(const k: QStringW): Currency;
begin
Result:=ForcePath(k).AsFloat;
end;

function TSuperObjectHelper.GetD(const k: QStringW): Double;
begin
Result:=ForcePath(k).AsFloat;
end;

function TSuperObjectHelper.GetI(const k: QStringW): Int64;
begin
Result:=ForcePath(k).AsInt64;
end;

function TSuperObjectHelper.GetN(const k: QStringW): TSuperArrayHelper;
var
  AItem:TQJson;
  I:Integer;
begin
AItem:=ForcePath(k);
if not (AItem is TSuperArrayHelper) then//ÀàÐÍ²»Æ¥Åä£¬Ìæ»»µô
  begin
  Result:=TSuperArrayHelper.Create;
  Result.FName:=AItem.Name;
  for I := 0 to AItem.Count-1 do
    Result.Add.Assign(AItem.Items[I]);
  Replace(AItem.ItemIndex,Result);
  end
else
  Result:=AItem as TSuperArrayHelper;
end;

function TSuperObjectHelper.GetO(const k: QStringW): TSuperObjectHelper;
var
  AItem:TQJson;
  I:Integer;
begin
AItem:=ForcePath(k);
if not (AItem is TSuperObjectHelper) then//ÀàÐÍ²»Æ¥Åä£¬Ìæ»»µô
  begin
  Result:=TSuperObjectHelper.Create;
  Result.FName:=AItem.Name;
  for I := 0 to AItem.Count-1 do
    Result.Add.Assign(AItem.Items[I]);
  Replace(AItem.ItemIndex,Result);
  end
else
  Result:=AItem as TSuperObjectHelper;
end;

function TSuperObjectHelper.GetS(const k: QStringW): QStringW;
begin
Result:=ForcePath(k).AsString;
end;

procedure TSuperObjectHelper.PutB(const k: QStringW; const Value: Boolean);
begin
ForcePath(k).AsBoolean:=Value;
end;

procedure TSuperObjectHelper.PutC(const k: QStringW; const Value: Currency);
begin
ForcePath(k).AsFloat:=Value;
end;

procedure TSuperObjectHelper.PutD(const k: QStringW; const Value: Double);
begin
ForcePath(k).AsFloat:=Value;
end;

procedure TSuperObjectHelper.PutI(const k: QStringW; const Value: Int64);
begin
ForcePath(k).AsInt64:=Value;
end;

procedure TSuperObjectHelper.PutN(const k: QStringW;
  const Value: TSuperArrayHelper);
var
  AItem:TQJson;
  I:Integer;
begin
AItem:=ForcePath(k);
AItem.DataType:=jdtArray;
for I := 0 to Value.Count-1 do
  AItem.Add.Assign(Value.Items[I]);
end;

procedure TSuperObjectHelper.PutO(const k: QStringW;
  const Value: TSuperObjectHelper);
var
  AItem,AChild:TQJson;
  I:Integer;
begin
AItem:=ForcePath(k);
for I := 0 to Value.Count-1 do
  begin
  AChild:=Value.Items[I];
  AItem.Add(AChild.Name).Assign(AChild);
  end;
end;

procedure TSuperObjectHelper.PutS(const k: QStringW; const Value: QStringW);
begin
ForcePath(k).AsString:=Value;
end;

{ TSuperArrayHelper }

constructor TSuperArrayHelper.Create;
begin
inherited;
DataType:=jdtArray;
end;

procedure TSuperArrayHelper.ExpandSize(l: Integer);
begin
while l>Count do
  Add;
end;

function TSuperArrayHelper.GetB(const k: Integer): Boolean;
begin
if k<Count then
  Result:=Items[k].AsBoolean
else
  Result:=False;
end;

function TSuperArrayHelper.GetC(const k: Integer): Currency;
begin
if k<Count then
  Result:=Items[k].AsFloat
else
  Result:=0;
end;

function TSuperArrayHelper.GetD(const k: Integer): Double;
begin
if k<Count then
  Result:=Items[k].AsFloat
else
  Result:=0;
end;

function TSuperArrayHelper.GetI(const k: Integer): Int64;
begin
if k<Count then
  Result:=Items[k].AsInt64
else
  Result:=0;
end;

function TSuperArrayHelper.GetN(const k: Integer): TSuperArrayHelper;
var
  AItem:TQJson;
  I:Integer;
begin
if k<Count then
  begin
  AItem:=Items[k];
  if not (AItem is TSuperArrayHelper) then//ÀàÐÍ²»Æ¥Åä£¬Ìæ»»µô
    begin
    Result:=TSuperArrayHelper.Create;
    Result.FName:=AItem.Name;
    for I := 0 to AItem.Count-1 do
      Result.Add.Assign(AItem.Items[I]);
    Replace(AItem.ItemIndex,Result);
    end
  else
    Result:=AItem as TSuperArrayHelper;
  end
else
  begin
  ExpandSize(k);
  Result:=TSuperArrayHelper.Create;
  Add(Result);
  end;
end;

function TSuperArrayHelper.GetO(const k: Integer): TSuperObjectHelper;
var
  AItem:TQJson;
  I:Integer;
begin
if k<Count then
  begin
  AItem:=Items[k];
  if not (AItem is TSuperObjectHelper) then//ÀàÐÍ²»Æ¥Åä£¬Ìæ»»µô
    begin
    Result:=TSuperObjectHelper.Create;
    Result.FName:=AItem.Name;
    for I := 0 to AItem.Count-1 do
      Result.Add.Assign(AItem.Items[I]);
    Replace(AItem.ItemIndex,Result);
    end
  else
    Result:=AItem as TSuperObjectHelper;
  end
else
  begin
  ExpandSize(k);
  Result:=TSuperObjectHelper.Create;
  Add(Result);
  end;
end;

function TSuperArrayHelper.GetS(const k: Integer): QStringW;
begin
if k<Count then
  Result:=Items[k].AsString
else
  SetLength(Result,0);
end;

procedure TSuperArrayHelper.PutB(const k: Integer; const Value: Boolean);
begin
ExpandSize(k+1);
Items[k].AsBoolean:=Value;
end;

procedure TSuperArrayHelper.PutC(const k: Integer; const Value: Currency);
begin
ExpandSize(k+1);
Items[k].AsFloat:=Value;
end;

procedure TSuperArrayHelper.PutD(const k: Integer; const Value: Double);
begin
ExpandSize(k+1);
Items[k].AsFloat:=Value;
end;

procedure TSuperArrayHelper.PutI(const k: Integer; const Value: Int64);
begin
ExpandSize(k+1);
Items[k].AsInt64:=Value;
end;

procedure TSuperArrayHelper.PutN(const k: Integer;
  const Value: TSuperArrayHelper);
var
  I:Integer;
  AItem:TQJson;
begin
ExpandSize(k+1);
AItem:=Items[k];
AItem.Clear;
for I := 0 to Value.Count-1 do
  AItem.Add.Assign(Value.Items[I]);
end;

procedure TSuperArrayHelper.PutO(const k: Integer;
  const Value: TSuperObjectHelper);
var
  I:Integer;
  AItem:TQJson;
begin
ExpandSize(k+1);
AItem:=Items[k];
AItem.Clear;
for I := 0 to Value.Count-1 do
  AItem.Add.Assign(Value.Items[I]);
end;

procedure TSuperArrayHelper.PutS(const k: Integer; const Value: QStringW);
begin
ExpandSize(k+1);
Items[k].AsString:=Value;
end;

end.
