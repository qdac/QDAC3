unit QAMF3;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Variants;

type
  TArrayVariant = array of variant;
  TStringArray = array of string;
  IntegerList=array of Integer;

  TVariantList = class
  private
    FList: array of variant;
    FCapacity: integer;
    FCount: integer;
    FGrowOnIndex: boolean;
    function GetVariant(i: integer): variant; virtual;
    procedure SetVariant(i: integer; const AVariant: variant); virtual;
    procedure Grow(ANewCapacity: integer = -1);
  protected
    procedure SetAsArray(const AArray: TArrayVariant); virtual;
    function GetAsArray: TArrayVariant; virtual;
    procedure SetAsVariant(const AVariant: variant); virtual;
    function GetAsVariant: variant; virtual;
  public
    constructor Create; overload;
    constructor Create(const AArray: array of variant); overload;
    destructor Destroy; override;
    procedure AssignConstArray(const AArgs: array of variant);
    procedure AssignVariant(const AVariant: variant);
    procedure Add(const AVariant: variant);
    procedure Clear;
    procedure Assign(AList: TVariantList);
    function IndexOf(const AVariant: variant): integer;
    property Count: integer read FCount;
    property Capacity: integer read FCapacity;
    property GrowOnIndex: boolean read FGrowOnIndex write FGrowOnIndex default false;
    property Variants[i: integer]: variant read GetVariant write SetVariant; default;
    property AsArray: TArrayVariant read GetAsArray write SetAsArray;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
  end;

  TAS3Traits = class
  private
    FIsDynamic: boolean;
    FIsExternalized: boolean;
    FProperties: TVariantList;
  protected
    function GetProperty(AIndex: integer): WideString; virtual;
    procedure SetProperty(AIndex: integer; AName: WideString); virtual;
    function GetPropertyCount: integer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property IsDynamic: boolean read FIsDynamic write FIsDynamic;
    property IsExternalized: boolean read FIsExternalized write FIsExternalized;
    property Properties[AIndex: integer]: WideString read GetProperty write SetProperty;
    property PropertyCount: integer read GetPropertyCount;
  end;

  TCustomHashItem = class
  private
    FHash: cardinal;
    FObject: TObject;
    FFreeObjectOnDestroy: boolean;
  protected
    function Equal(const AItem: TCustomHashItem): boolean; virtual; abstract;
  public
    constructor Create(const AHash: cardinal; const AObject: TObject; const AFreeOnDestroy: boolean = false); virtual;
    destructor Destroy; override;
    property Hash: cardinal read FHash;
    property Obj: TObject read FObject;
    property FreeObjectOnDestroy: boolean read FFreeObjectOnDestroy;
  end;

  THashWideKeyValueItem = class(TCustomHashItem)
  private
    FKey: WideString;
    FValue: variant;
  protected
    function Equal(const AItem: TCustomHashItem): boolean; override;
  public
    constructor Create(const AKey: WideString; const AValue: variant); reintroduce; virtual;
    destructor Destroy; override;
    property Key: WideString read FKey;
    property Value: variant read FValue;
  end;

  THashBucket = class
  private
    FItems: TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const AItem: TCustomHashItem); virtual;
    procedure Delete(const AItem: TCustomHashItem); virtual;
    function GetIndexOf(const AItem: TCustomHashItem): integer; virtual;
    function Get(const AItem: TCustomHashItem): TCustomHashItem; virtual;
    function GetByHash(const AHash: cardinal): TCustomHashItem; virtual;
    procedure Clear; virtual;
  end;

  THashBuckets = class
  private
    FCount: integer;
    FSize: integer;
    FBuckets: array of THashBucket;
    FEntryIndex: integer;
    FEntryItemIndex: integer;
  public
    constructor Create(ASize: integer); virtual;
    destructor Destroy; override;
    procedure ResetEnum; virtual;
    function Enum(var AItem: TCustomHashItem): boolean; virtual;
    function HashToBucketIndex(AHash: cardinal): integer; virtual;
    procedure Add(AItem: TCustomHashItem); virtual;
    procedure Delete(AItem: TCustomHashItem); virtual;
    function GetByHash(AHash: cardinal): TCustomHashItem; virtual;
    function Get(AItem: TCustomHashItem): TCustomHashItem; virtual;
    procedure Clear; virtual;
    property Size: integer read FSize;
    property Count: integer read FCount;
  end;

  TCustomHashList = class
  private
    FSize: integer;
    FFreeObjectsOnDestroy: boolean;
    FUnique: boolean;
  protected
    FBuckets: THashBuckets;
    function GetCount: integer; virtual;
  public
    constructor Create(ASize: integer); virtual;
    destructor Destroy; override;
    procedure ResetEnum; virtual;
    function Enum(var AItem: TCustomHashItem): boolean; virtual;
    function GetItem(const AItem: TCustomHashItem): TCustomHashItem; virtual;
    procedure DeleteItem(const AItem: TCustomHashItem); virtual;
    procedure Clear; virtual;
    property Size: integer read FSize;
    property Unique: boolean read FUnique write FUnique;
    property FreeObjectsOnDestroy: boolean read FFreeObjectsOnDestroy write FFreeObjectsOnDestroy;
    property Count: integer read GetCount;
  end;

  THashWideKeyValueList = class(TCustomHashList)
  private
    FCaseSensitive: boolean;
  public
    constructor Create(ASize: integer; const ACaseSensitive: boolean = false); reintroduce; virtual;
    function Add(AKey: WideString; AValue: variant): THashWideKeyValueItem; virtual;
    procedure Delete(AKey: WideString); virtual;
    function Get(AKey: WideString): variant; virtual;
    property CaseSensitive: boolean read FCaseSensitive;
  end;

  IAS3Object = interface
    ['{13713529-2C3B-44B2-A6DE-AD0B03418E6F}']
    function GetValue(APropertyName: WideString): variant;
    procedure SetValue(APropertyName: WideString; AValue: variant);
    function GetProperty(AIndex: integer): WideString;
    procedure SetProperty(AIndex: integer; AName: WideString);
    function GetPropertyCount: integer;
    function GetAS3ClassName: string;
    procedure SetAS3ClassName(AValue: string);
    function GetIsDynamic: boolean;
    procedure SetIsDynamic(AValue: boolean);
    function GetIsExternalized: boolean;
    procedure SetIsExternalized(AValue: boolean);
  end;

  TAS3Object = class(TInterfacedObject, IAS3Object)
  private
    FAS3ClassName: string;
    FIsDynamic: boolean;
    FIsExternalized: boolean;
    FValues: THashWideKeyValueList;
    FProperties: TVariantList;
  protected
    function GetValue(APropertyName: WideString): variant; virtual;
    procedure SetValue(APropertyName: WideString; AValue: variant); virtual;
    function GetProperty(AIndex: integer): WideString; virtual;
    procedure SetProperty(AIndex: integer; AName: WideString); virtual;
    function GetPropertyCount: integer; virtual;
    function GetAS3ClassName: string; virtual;
    procedure SetAS3ClassName(AValue: string); virtual;
    function GetIsDynamic: boolean; virtual;
    procedure SetIsDynamic(AValue: boolean); virtual;
    function GetIsExternalized: boolean; virtual;
    procedure SetIsExternalized(AValue: boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignTraits(ATraits: TAS3Traits); virtual;
    property AS3ClassName: string read GetAS3ClassName write SetAS3ClassName;
    property IsDynamic: boolean read GetIsDynamic write SetIsDynamic;
    property IsExternalized: boolean read GetIsExternalized write SetIsExternalized;
    property Properties[AIndex: integer]: WideString read GetProperty write SetProperty;
    property Values[APropertyName: WideString]: variant read GetValue write SetValue;
    property PropertyCount: integer read GetPropertyCount;
  end;

  IAS3Array = interface
    ['{A458A1F8-7E2C-4AF8-BF9F-CAC3465F88BD}']
    procedure Clear;
    procedure ResetEnum;
    function Enum(var AKey: variant; var AValue: variant): boolean;
    function GetValue(AElementIndex: variant): variant;
    procedure SetValue(AElementIndex: variant; AValue: variant);
    function GetCount: integer;
    function GetIsAssociative: boolean;
  end;

  TAS3Array = class(TInterfacedObject, IAS3Array)
  private
    FAssociative: THashWideKeyValueList;
    FList: TVariantList;
    FIsAssociative: boolean;
    FEnumIndex: integer;
  protected
    function GetValue(AElementIndex: variant): variant; virtual;
    procedure SetValue(AElementIndex: variant; AValue: variant); virtual;
    function GetCount: integer; virtual;
    function GetIsAssociative: boolean; virtual;
    procedure ConvertToAssociative; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure ResetEnum; virtual;
    function Enum(var AKey: variant; var AValue: variant): boolean; virtual;
    property Values[AElementIndex: variant]: variant read GetValue write SetValue;
    property Count: integer read GetCount;
    property IsAssociative: boolean read GetIsAssociative;
  end;

  THashLongintItem = class(TCustomHashItem)
  protected
    function GetKey: cardinal;
    function Equal(const AItem: TCustomHashItem): boolean; override;
  public
    constructor Create(const AKey: cardinal; const AObject: TObject; const AFreeOnDestroy: boolean = false); reintroduce; virtual;
    property Key: cardinal read GetKey;
  end;

  THashLongintList = class(TCustomHashList)
  public
    function AddObject(AKey: cardinal; AObject: TObject): THashLongintItem; virtual;
    function AddManagedObject(AKey: cardinal; AObject: TObject): THashLongintItem; virtual;
    procedure Delete(AKey: cardinal); virtual;
    function GetObject(AKey: cardinal): TObject; virtual;
  end;

  TAMFStreamType = (amf0, amf3);

  TAMF0DataTypes = (amf0dtNumber,        //  0
    amf0dtBoolean,       //  1
    amf0dtString,        //  2
    amf0dtObject,        //  3 Object
    amf0dtUnused_2,      //  4 Movieclip
    amf0dtNull,          //  5
    amf0dtUndefined,     //  6
    amf0dtUnused_3,      //  7 Reference
    amf0dtAssociativeArray, //  8 Associative/mixed array
    amf0dtObjectEnd,     //  9 Object end
    amf0dtArray,         // 10
    amf0dtDateTime,      // 11
    amf0dtLongString,    // 12
    amf0dtUnused_6,      // 13 AsObject
    amf0dtUnused_7,      // 14 Recordset
    amf0dtXML,           // 15 XML
    amf0dtUnused_9,      // 16 Custom class
    amf0dtAMF3           // 17 AMF3 data
);

  TAMF3DataTypes = (amf3dtUndefined,     //  0
    amf3dtNull,          //  1
    amf3dtBooleanFalse,  //  2
    amf3dtBooleanTrue,   //  3
    amf3dtInteger,       //  4
    amf3dtNumber,        //  5
    amf3dtString,        //  6
    amf3dtXML2,          //  7
    amf3dtDateTime,      //  8
    amf3dtArray,         //  9
    amf3dtObject,        // 10
    amf3dtXML,           // 11
    amf3dtByteArray      // 12
);

  TDateTimeObject = class
  private
    FValue: TDateTime;
  public
    constructor Create(ADateTime: TDateTime); virtual;
    property Value: TDateTime read FValue write FValue;
  end;

  TVariantObject = class
  private
    FValue: variant;
  public
    constructor Create(AVariant: variant); virtual;
    property Value: variant read FValue write FValue;
  end;

  TStringObject = class
  private
    FValue: string;
  public
    constructor Create(AString: string); virtual;
    property Value: string read FValue write FValue;
  end;

  TQAMF3Helpher = class
  private
    FStream: TMemoryStream;
    FObjectList: THashLongintList;
    FStringList: THashLongintList;
    FTraitsList: THashLongintList;
    FType: TAMFStreamType;
    FBaseDate: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetData(BuffAdd: Pointer; buffLen: Integer);
    procedure ClearData;
    procedure BeforeWrite;
    procedure AfterWrite;
    procedure BeforeRead;
    procedure AfterRead;
    procedure ClearCache;
    function readAMF3elem: variant;


    function StoreTraitsObject(const ATraits: TAS3Traits): integer;
    function StoreDateObject(const ADate: TDateTime): integer;
    function StoreStringObject(const AString: string): longint;
    function StoreObject(const AObject: variant): integer;
    function GetDateObject(AIndex: longint): TDateTime;
    function GetStringObject(AIndex: longint): string;
    function GetObject(AIndex: longint): variant;
    function GetTraitsObject(AIndex: longint): TAS3Traits;
    function GetTraitsReference(ATraits: TAS3Traits): longint;
    function GetDateReference(ADate: TDateTime): longint;
    function GetStringReference(AString: string): longint;
    function GetObjectReference(AObject: variant): longint;
  public
    function ReadUINT32: cardinal; virtual;
    function ReadUINT16: cardinal; virtual;
    function ReadINT32: integer; virtual;
    function ReadUINT29: integer; virtual;
    procedure WriteUINT32(const AValue: cardinal); virtual;
    procedure WriteUINT16(const AValue: cardinal); virtual;
    procedure WriteINT32(const AValue: integer); virtual;
    procedure WriteUINT29(const AValue: integer); virtual;

    function ReadAMF0Type: TAMF0DataTypes; virtual;
    function ReadAMF0Boolean: boolean; virtual;
    function ReadAMF0String: WideString; virtual;
    function ReadAMF0LongString: WideString; virtual;
    function ReadAMF0Object: variant; virtual;
    function ReadAMF0Number: double; virtual;
    function ReadAMF0XML: WideString; virtual;
    function ReadAMF0DateTime: TDateTime; virtual;
    function ReadAMF0AssociativeArray: variant; virtual;
    function ReadAMF0Array: variant; virtual;
    function ReadAMF0Variant(var AEOI: boolean): variant; virtual;

    procedure WriteAMF0Type(const AValue: TAMF0DataTypes); virtual;
    procedure WriteAMF0Boolean(const AValue: boolean); virtual;
    procedure WriteAMF0String(const AValue: WideString); virtual;
    procedure WriteAMF0LongString(const AValue: WideString); virtual;
    procedure WriteAMF0Object(const AValue: variant); virtual;
    procedure WriteAMF0Number(const AValue: double); virtual;
    procedure WriteAMF0XML(const AValue: variant); virtual;
    procedure WriteAMF0DateTime(const AValue: TDateTime); virtual;
    procedure WriteAMF0Array(const AValue: variant); virtual;
    procedure WriteAMF0Variant(const AValue: variant); virtual;

    function ReadAMF3Type: TAMF3DataTypes; virtual;
    function ReadAMF3Number: double; virtual;
    function ReadAMF3String: WideString; virtual;
    function ReadAMF3DateTime: TDateTime; virtual;
    function ReadAMF3Object: variant; virtual;
    function ReadAMF3Array: variant; virtual;
    function ReadAMF3ByteArray: variant; virtual;
    function ReadAMF3Variant: variant; virtual;
    procedure WriteAMF3Number(const AValue: double); virtual;
    procedure WriteAMF3String(const AValue: WideString); virtual;
    procedure WriteAMF3DateTime(const AValue: TDateTime); virtual;
    procedure WriteAMF3Array(const AValue: variant); virtual;
    procedure WriteAMF3ByteArray(const AValue: variant); virtual;
    procedure WriteAMF3Variant(const AValue: variant); virtual;
    procedure WriteAMF3Object(const AValue: variant); virtual;
    procedure WriteAMF3Type(const AValue: TAMF3DataTypes); virtual;

    procedure WriteWideString(const AName: string; const AWideString: WideString);
    procedure WriteAnsiString(const AName: string; const AString: AnsiString);
    procedure WriteString(const AName: string; const AString: string);
    procedure WriteInteger(const AName: string; const AInteger: integer);
    procedure WriteFloat(const AName: string; const AFloat: double);
    procedure WriteDateTime(const AName: string; const ADateTime: TDateTime);
    procedure WriteBoolean(const AName: string; const ABoolean: boolean);
    procedure WriteObject(const AName: string; const AObject: IInterface);
    procedure WriteVariant(const AName: string; const AVariant: variant);
    procedure WriteVariantArray(const AName: string; const AVariant: Variant);
    procedure WriteVariantByteArray(const AName: string; const AVariant: Variant);
    procedure WriteStream(const AName: string; AStream: TStream; ALength: integer);
    function ReadWideString(const AName: string): WideString;
    function ReadAnsiString(const AName: string): AnsiString;
    function ReadString(const AName: string): string;
    function ReadInteger(const AName: string): integer;
    function ReadFloat(const AName: string): double;
    function ReadDateTime(const AName: string): TDateTime;
    function ReadBoolean(const AName: string): boolean;
    function ReadObject(const AName: string): IInterface;
    function ReadVariant(const AName: string): variant;
    function ReadVariantArray(const AName: string): variant;
    function ReadVariantByteArray(const AName: string): variant;
    procedure ReadStream(const AName: string; AStream: TStream; var ALength: integer);

    procedure WriteCount(const AName: string; const ACount: integer);
    procedure WriteType(const AName: string; const AType: integer);
    procedure WriteRange(const AName: string; const ALow, AHigh: integer);
    procedure WriteIdentifier(const AName: string; const AIdentifier: integer);
    procedure WriteVersion(const AName: string; const AVersion: integer);
    function ReadCount(const AName: string): integer;
    function ReadType(const AName: string): integer;
    procedure ReadRange(const AName: string; var ALow, AHigh: integer);
    function ReadIdentifier(const AName: string): integer;
    function ReadVersion(const AName: string): integer;
    property AMFType: TAMFStreamType read FType write FType;
    property Stream: TMemoryStream read FStream;
  end;

function HashWideString(const AString: WideString): cardinal;

implementation

function Split(const source,ch:string):TStringArray;
var
 temp:string;
 i:integer;
begin
  SetLength(Result,0);
  temp:=source;
  i:=pos(ch,source);
  while i<>0 do
  begin
   SetLength(Result,High(Result)+2);
   Result[High(Result)]:=copy(temp,0,i-1);

   delete(temp,1,i+length(ch)-1);
   i:=pos(ch,temp);
  end;
  SetLength(Result,High(Result)+2);
  Result[High(Result)]:=temp;
end;

function HashWideString(const AString: WideString): cardinal;
var
  i: integer;
  r: cardinal;
begin
  Result := 0;
  for i := 1 to Length(AString) do
  begin
    Result := (Result shl 4) + Ord(AString[i]);
    r := Result and $F0000000;
    if r <> 0 then
      Result := Result xor (r shr 24);
    Result := Result and (not r);
  end;
end;

constructor TVariantList.Create;
begin
  inherited;
  FCapacity := 0;
  FCount := 0;
end;

constructor TVariantList.Create(const AArray: array of variant);
begin
  inherited Create;
  AssignConstArray(AArray);
end;

destructor TVariantList.Destroy;
begin
  Clear;
  Finalize(FList);
  inherited;
end;

procedure TVariantList.SetAsArray(const AArray: TArrayVariant);
var
  i, l: integer;
begin
  Clear;

  l := length(AArray);
  for i := 0 to l - 1 do
    Add(AArray[i]);
end;

function TVariantList.GetAsArray: TArrayVariant;
var
  i: integer;
begin
  if FCount <= 0 then
  begin
    SetLength(Result, 0);
    exit;
  end;
  SetLength(Result, FCount);
  for i := 0 to FCount - 1 do
    Result[i] := Variants[i];
end;

procedure TVariantList.SetAsVariant(const AVariant: variant);
begin
  AssignVariant(AVariant);
end;

function TVariantList.GetAsVariant: variant;
var
  i: integer;
begin
  Result := VarArrayCreate([0, FCount - 1], varVariant);
  for i := 0 to FCount - 1 do
    Result[i] := Variants[i];
end;

procedure TVariantList.Grow(ANewCapacity: integer = -1);
var
  i: integer;
begin
  if ANewCapacity >= 0 then
    FCapacity := ANewCapacity
  else
  begin
    if FCapacity < 8 then
      FCapacity := 8
    else
      FCapacity := FCapacity * 2;
  end;
  SetLength(FList, FCapacity);
  for i := FCount to FCapacity - 1 do
    FList[i] := null;
end;

procedure TVariantList.AssignConstArray(const AArgs: array of variant);
var
  i, n: integer;
begin
  n := length(AArgs);
  SetLength(FList, n);
  for i := 0 to n - 1 do
    FList[i] := AArgs[i];
  FCapacity := n;
  FCount := n;
end;

procedure TVariantList.AssignVariant(const AVariant: variant);
var
  i, l, h: integer;
begin
  Clear;
  if VarIsArray(AVariant) then
  begin
    l := VarArrayLowBound(AVariant, 1);
    h := VarArrayHighBound(AVariant, 1);
    for i := l to h do
      Add(AVariant[i]);
  end
  else
    Add(AVariant);
end;

function TVariantList.GetVariant(i: integer): variant;
begin
  if (i < 0) or (i > FCount - 1) then
    Result := Unassigned
  else
    Result := FList[i];
end;

procedure TVariantList.SetVariant(i: integer; const AVariant: variant);
begin
  if (i >= 0) and (i <= FCount - 1) then
    FList[i] := AVariant;
end;

procedure TVariantList.Add(const AVariant: variant);
begin
  if FCount >= FCapacity then
    Grow;
  FList[FCount] := AVariant;
  inc(FCount);
end;

procedure TVariantList.Clear;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    FList[i] := Unassigned;
  SetLength(FList, 0);
  FCapacity := 0;
  FCount := 0;
end;

function TVariantList.IndexOf(const AVariant: variant): integer;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
  begin
    if FList[i] = AVariant then
    begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

procedure TVariantList.Assign(AList: TVariantList);
var
  i: integer;
begin
  Clear;
  for i := 0 to AList.FCount - 1 do
    Add(AList.Variants[i]);
end;

constructor TAS3Traits.Create;
begin
  inherited Create;

  FProperties := TVariantList.Create;
  FProperties.GrowOnIndex := true;
end;

destructor TAS3Traits.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

function TAS3Traits.GetProperty(AIndex: integer): WideString;
begin
  Result := FProperties.Variants[AIndex];
end;

procedure TAS3Traits.SetProperty(AIndex: integer; AName: WideString);
begin
  FProperties.Variants[AIndex] := AName;
end;

function TAS3Traits.GetPropertyCount: integer;
begin
  Result := FProperties.Count;
end;

constructor TCustomHashItem.Create(const AHash: cardinal; const AObject: TObject; const AFreeOnDestroy: boolean = false);
begin
  inherited Create;
  FHash := AHash;
  FObject := AObject;
  FFreeObjectOnDestroy := AFreeOnDestroy;
end;

destructor TCustomHashItem.Destroy;
begin
  if (FObject <> nil) and FFreeObjectOnDestroy then
    FObject.Free;
  inherited;
end;

constructor THashWideKeyValueItem.Create(const AKey: WideString; const AValue: variant);
begin
  inherited Create(HashWideString(AKey), nil, false);
  FKey := AKey;
  FValue := AValue;
end;

destructor THashWideKeyValueItem.Destroy;
begin
  FValue := null;
  inherited Destroy;
end;

function THashWideKeyValueItem.Equal(const AItem: TCustomHashItem): boolean;
begin
  Result := (THashWideKeyValueItem(AItem).Hash = Hash) and (THashWideKeyValueItem(AItem).Key = Key);
end;

constructor THashBucket.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor THashBucket.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure THashBucket.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    TCustomHashItem(FItems.Items[i]).Free;
    FItems.Items[i] := nil;
  end;
  FItems.Clear;
end;

procedure THashBucket.Add(const AItem: TCustomHashItem);
var
  o: TCustomHashItem;
begin
  o := Get(AItem);
  if o = nil then
    FItems.Add(AItem);
end;

procedure THashBucket.Delete(const AItem: TCustomHashItem);
var
  i: integer;
begin
  i := GetIndexOf(AItem);
  if i < 0 then
    exit;
  TCustomHashItem(FItems.Items[i]).Free;
  FItems.Delete(i);
end;

function THashBucket.GetIndexOf(const AItem: TCustomHashItem): integer;
var
  i: integer;
  o: TCustomHashItem;
begin
  Result := -1;
  for i := 0 to FItems.Count - 1 do
  begin
    o := TCustomHashItem(FItems.Items[i]);
    if o.Equal(AItem) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function THashBucket.Get(const AItem: TCustomHashItem): TCustomHashItem;
var
  i: integer;
begin
  i := GetIndexOf(AItem);
  if i >= 0 then
    Result := TCustomHashItem(FItems.Items[i])
  else
    Result := nil;
end;

function THashBucket.GetByHash(const AHash: cardinal): TCustomHashItem;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    Result := TCustomHashItem(FItems.Items[i]);
    if Result.FHash = AHash then
      exit;
  end;
  Result := nil;
end;

constructor THashBuckets.Create(ASize: integer);
var
  i: integer;
begin
  inherited Create;
  FSize := ASize;
  FCount := 0;
  SetLength(FBuckets, ASize);
  for i := 0 to FSize - 1 do
    FBuckets[i] := THashBucket.Create;
  ResetEnum;
end;

destructor THashBuckets.Destroy;
var
  i: integer;
begin
  for i := 0 to FSize - 1 do
    FBuckets[i].Free;
  SetLength(FBuckets, 0);
end;

procedure THashBuckets.ResetEnum;
begin
  FEntryIndex := 0;
  FEntryItemIndex := 0;
end;

function THashBuckets.Enum(var AItem: TCustomHashItem): boolean;
var
  b: THashBucket;
begin
  AItem := nil;
  Result := false;

  while FEntryIndex < FSize do
  begin
    b := FBuckets[FEntryIndex];
    if FEntryItemIndex >= b.FItems.Count then
    begin
      inc(FEntryIndex);
      FEntryItemIndex := 0;
    end
    else
    begin
      AItem := TCustomHashItem(b.FItems.Items[FEntryItemIndex]);
      inc(FEntryItemIndex);
      Result := true;
      break;
    end;
  end;
end;

function THashBuckets.HashToBucketIndex(AHash: cardinal): integer;
begin
  Result := AHash mod cardinal(FSize);
end;

procedure THashBuckets.Add(AItem: TCustomHashItem);
var
  i: integer;
begin
  i := HashToBucketIndex(AItem.FHash);
  FBuckets[i].Add(AItem);
  inc(FCount);
end;

procedure THashBuckets.Delete(AItem: TCustomHashItem);
var
  i: integer;
begin
  i := HashToBucketIndex(AItem.FHash);
  FBuckets[i].Delete(AItem);
  Dec(FCount);
end;

function THashBuckets.GetByHash(AHash: cardinal): TCustomHashItem;
var
  i: integer;
begin
  i := HashToBucketIndex(AHash);
  Result := FBuckets[i].GetByHash(AHash);
end;

function THashBuckets.Get(AItem: TCustomHashItem): TCustomHashItem;
var
  i: integer;
begin
  i := HashToBucketIndex(AItem.FHash);
  Result := FBuckets[i].Get(AItem);
end;

procedure THashBuckets.Clear;
var
  i: integer;
begin
  for i := 0 to FSize - 1 do
    FBuckets[i].Clear;
  ResetEnum;
  FCount := 0;
end;

constructor TCustomHashList.Create(ASize: integer);
begin
  inherited Create;
  FSize := ASize;
  FFreeObjectsOnDestroy := true;
  FUnique := false;
  FBuckets := THashBuckets.Create(ASize);
end;

destructor TCustomHashList.Destroy;
begin
  FBuckets.Free;
  inherited;
end;

function TCustomHashList.GetCount: integer;
begin
  Result := FBuckets.Count;
end;

function TCustomHashList.GetItem(const AItem: TCustomHashItem): TCustomHashItem;
begin
  Result := FBuckets.Get(AItem);
end;

procedure TCustomHashList.DeleteItem(const AItem: TCustomHashItem);
begin
  FBuckets.Delete(AItem);
end;

procedure TCustomHashList.ResetEnum;
begin
  FBuckets.ResetEnum;
end;

function TCustomHashList.Enum(var AItem: TCustomHashItem): boolean;
begin
  Result := FBuckets.Enum(AItem);
end;

procedure TCustomHashList.Clear;
begin
  FBuckets.Clear;
end;

constructor THashWideKeyValueList.Create(ASize: integer; const ACaseSensitive: boolean = false);
begin
  inherited Create(ASize);
  FCaseSensitive := false;
end;

function THashWideKeyValueList.Add(AKey: WideString; AValue: variant): THashWideKeyValueItem;
begin
  if FUnique then
    Delete(AKey);
  if not FCaseSensitive then
    AKey := UpperCase(AKey);
  Result := THashWideKeyValueItem.Create(AKey, AValue);
  FBuckets.Add(Result);
end;

procedure THashWideKeyValueList.Delete(AKey: WideString);
var
  o: THashWideKeyValueItem;
begin
  if not FCaseSensitive then
    AKey := UpperCase(AKey);
  o := THashWideKeyValueItem.Create(AKey, null);
  try
    FBuckets.Delete(o);
  finally
    o.Free;
  end;
end;

function THashWideKeyValueList.Get(AKey: WideString): variant;
var
  hi: THashWideKeyValueItem;
  o: THashWideKeyValueItem;
begin
  if not FCaseSensitive then
    AKey := UpperCase(AKey);

  o := THashWideKeyValueItem.Create(AKey, null);
  try
    hi := THashWideKeyValueItem(FBuckets.Get(o));
    if hi = nil then
      Result := null
    else
      Result := hi.FValue;
  finally
    o.Free;
  end;
end;

constructor THashLongintItem.Create(const AKey: cardinal; const AObject: TObject; const AFreeOnDestroy: boolean = false);
begin
  inherited Create(AKey, AObject, AFreeOnDestroy);
end;

function THashLongintItem.GetKey: cardinal;
begin
  Result := cardinal(FHash);
end;

function THashLongintItem.Equal(const AItem: TCustomHashItem): boolean;
begin
  Result := (THashLongintItem(AItem).Hash = Hash) and (THashLongintItem(AItem).Key = Key);
end;

function THashLongintList.AddObject(AKey: cardinal; AObject: TObject): THashLongintItem;
begin
  if FUnique then
    Delete(AKey);
  Result := THashLongintItem.Create(AKey, AObject, FFreeObjectsOnDestroy);
  FBuckets.Add(Result);
end;

function THashLongintList.AddManagedObject(AKey: cardinal; AObject: TObject): THashLongintItem;
begin
  if FUnique then
    Delete(AKey);
  Result := THashLongintItem.Create(AKey, AObject, true);
  FBuckets.Add(Result);
end;

procedure THashLongintList.Delete(AKey: cardinal);
var
  o: THashLongintItem;
begin
  o := THashLongintItem.Create(AKey, nil, false);
  try
    FBuckets.Delete(o);
  finally
    o.Free;
  end;
end;

function THashLongintList.GetObject(AKey: cardinal): TObject;
var
  hi: TCustomHashItem;
  o: THashLongintItem;
begin
  o := THashLongintItem.Create(AKey, nil, false);
  try
    hi := FBuckets.Get(o);
    if hi = nil then
      Result := nil
    else
      Result := hi.FObject;
  finally
    o.Free;
  end;
end;

constructor TDateTimeObject.Create(ADateTime: TDateTime);
begin
  inherited Create;
  FValue := ADateTime;
end;

constructor TVariantObject.Create(AVariant: variant);
begin
  inherited Create;
  FValue := AVariant;
end;

constructor TStringObject.Create(AString: string);
begin
  inherited Create;
  FValue := AString;
end;

constructor TAS3Object.Create;
begin
  inherited Create;
  FValues := THashWideKeyValueList.Create(10, true);
  FValues.Unique := true;
  FProperties := TVariantList.Create;
  FProperties.GrowOnIndex := true;
end;

destructor TAS3Object.Destroy;
begin
  FValues.Free;
  FProperties.Free;
  inherited Destroy;
end;

procedure TAS3Object.AssignTraits(ATraits: TAS3Traits);
begin
  FIsDynamic := ATraits.FIsDynamic;
  FIsExternalized := ATraits.FIsExternalized;
  FProperties.Assign(ATraits.FProperties);
end;

function TAS3Object.GetValue(APropertyName: WideString): variant;
begin
  Result := FValues.Get(APropertyName);
end;

procedure TAS3Object.SetValue(APropertyName: WideString; AValue: variant);
begin
  FValues.Add(APropertyName, AValue);
  if FProperties.IndexOf(APropertyName) < 0 then
    FProperties.Add(APropertyname);
end;

function TAS3Object.GetProperty(AIndex: integer): WideString;
begin
  Result := FProperties.Variants[AIndex];
end;

procedure TAS3Object.SetProperty(AIndex: integer; AName: WideString);
begin
  FProperties.Variants[AIndex] := AName;
end;

function TAS3Object.GetPropertyCount: integer;
begin
  Result := FProperties.Count;
end;

function TAS3Object.GetAS3ClassName: string;
begin
  Result := FAS3ClassName;
end;

procedure TAS3Object.SetAS3ClassName(AValue: string);
begin
  FAS3ClassName := AValue;
end;

function TAS3Object.GetIsDynamic: boolean;
begin
  Result := FIsDynamic;
end;

procedure TAS3Object.SetIsDynamic(AValue: boolean);
begin
  FIsDynamic := AValue;
end;

function TAS3Object.GetIsExternalized: boolean;
begin
  Result := FIsExternalized;
end;

procedure TAS3Object.SetIsExternalized(AValue: boolean);
begin
  FIsExternalized := AValue;
end;

constructor TAS3Array.Create;
begin
  inherited Create;
  FAssociative := THashWideKeyValueList.Create(100, true);
  FList := TVariantList.Create;
  FList.GrowOnIndex := true;
  FIsAssociative := false;
  FEnumIndex := -1;
end;

destructor TAS3Array.Destroy;
begin
  FList.Free;
  FAssociative.Free;
  inherited Destroy;
end;

procedure TAS3Array.Clear;
begin
  FList.Clear;
  FAssociative.Clear;
  FIsAssociative := false;
end;

procedure TAS3Array.ConvertToAssociative;
var
  i: integer;
begin
  if FIsAssociative then
    exit;
  for i := 0 to FList.Count - 1 do
    FAssociative.Add(inttostr(i), FList.Variants[i]);
  FList.Clear;
  FIsAssociative := true;
end;

function TAS3Array.GetIsAssociative: boolean;
begin
  Result := FIsAssociative;
end;

function TAS3Array.GetCount: integer;
begin
  if FIsAssociative then
    Result := FAssociative.Count
  else
    Result := FList.Count;
end;

function TAS3Array.GetValue(AElementIndex: variant): variant;
var
  i: integer;
begin
  if FIsAssociative then
    Result := FAssociative.Get(AElementIndex)
  else
  begin
    try
      i := AElementIndex;
    except
      Result := null;
      exit;
    end;
    Result := FList.Variants[i];
  end;
end;

procedure TAS3Array.SetValue(AElementIndex: variant; AValue: variant);
var
  i: integer;
begin
  if FIsAssociative then
    FAssociative.Add(AElementIndex, AValue)
  else
  begin
    try
      i := AElementIndex;
    except
      ConvertToAssociative;
      FAssociative.Add(AElementIndex, AValue);
      exit;
    end;
    FList.Variants[i] := AValue;
  end;
end;

procedure TAS3Array.ResetEnum;
begin
  FEnumIndex := -1;
  FAssociative.ResetEnum;
end;

function TAS3Array.Enum(var AKey: variant; var AValue: variant): boolean;
var
  e: TCustomHashItem;
begin
  if not FIsAssociative then
  begin
    if FEnumIndex < 0 then
      FEnumIndex := 0;
    if FEnumIndex >= Count then
    begin
      Result := false;
      exit;
    end;
    AKey := FEnumIndex;
    AValue := FList.Variants[FEnumIndex];
    inc(FEnumIndex);
    Result := true;
    exit;
  end;

  Result := FAssociative.Enum(e);
  if not Result then
    exit;

  AKey := THashWideKeyValueItem(e).Key;
  AValue := THashWideKeyValueItem(e).Value;
end;

constructor TQAMF3Helpher.Create();
begin
  inherited Create();
  FStream := TMemoryStream.Create;
  FObjectList := THashLongintList.Create(64);
  FStringList := THashLongintList.Create(64);
  FTraitsList := THashLongintList.Create(64);
  FBaseDate := EncodeDate(1970, 1, 1);
end;

destructor TQAMF3Helpher.Destroy;
begin
  FObjectList.Free;
  FStringList.Free;
  FTraitsList.Free;
  FStream.Free;
  inherited Destroy;
end;

procedure TQAMF3Helpher.SetData(BuffAdd: Pointer; buffLen: Integer);
begin
  FStream.SetSize(buffLen);
  CopyMemory(FStream.Memory, BuffAdd, buffLen);
  FStream.Seek(0, soFromBeginning);
end;

procedure TQAMF3Helpher.ClearData;
begin
  FStream.Clear;
  FObjectList.Clear;
  FStringList.Clear;
  FTraitsList.Clear;
end;

procedure TQAMF3Helpher.BeforeWrite;
begin
  FStream.Clear;
  FTraitsList.Clear;
  FObjectList.Clear;
  FStringList.Clear;
end;


procedure TQAMF3Helpher.AfterWrite;
begin

end;

procedure TQAMF3Helpher.BeforeRead;
begin
  FTraitsList.Clear;
  FObjectList.Clear;
  FStringList.Clear;
end;

procedure TQAMF3Helpher.AfterRead;
begin

end;

procedure TQAMF3Helpher.ClearCache;
begin
  FStream.Clear;
  FTraitsList.Clear;
  FObjectList.Clear;
  FStringList.Clear;
end;

function TQAMF3Helpher.readAMF3elem: variant;
var
  b: byte;
  dataType: TAMF3DataTypes;
begin
  Result := Null;
  if FStream.Position >= FStream.Size then
  begin
    Exit;
  end;
  FStream.Read(b, 1);
  dataType := TAMF3DataTypes(b);
  if dataType = amf3dtUndefined then
  begin

  end
  else if dataType = amf3dtNull then
  begin

  end
  else if dataType = amf3dtBooleanFalse then
  begin
    Result := False;
  end
  else if dataType = amf3dtBooleanTrue then
  begin
    Result := True;
  end
  else if dataType = amf3dtInteger then
  begin
    Result := ReadINT32;
  end
  else if dataType = amf3dtNumber then
  begin
    Result := ReadAMF3Number;
  end
  else if dataType = amf3dtString then
  begin
    Result := ReadAMF3String;
  end
  else if dataType = amf3dtXML2 then
  begin
    Result := ReadAMF0XML;
  end
  else if dataType = amf3dtDateTime then
  begin
    Result := ReadAMF3DateTime;
  end
  else if dataType = amf3dtArray then
  begin
    Result := ReadAMF3Array;
  end
  else if dataType = amf3dtObject then
  begin
    Result := ReadAMF3Object;
  end
  else if dataType = amf3dtXML then
  begin
    Result := ReadAMF0XML;
  end
  else if dataType = amf3dtByteArray then
  begin
    Result := ReadAMF3ByteArray;
  end;
end;

function TQAMF3Helpher.StoreTraitsObject(const ATraits: TAS3Traits): integer;
begin
  Result := FTraitsList.Count;
  FTraitsList.AddManagedObject(Result, ATraits);
end;

function TQAMF3Helpher.StoreDateObject(const ADate: TDateTime): integer;
begin
  Result := FObjectList.Count;
  FObjectList.AddManagedObject(Result, TDateTimeObject.Create(ADate));
end;

function TQAMF3Helpher.StoreStringObject(const AString: string): longint;
begin
  Result := FStringList.Count;
  FStringList.AddManagedObject(Result, TStringObject.Create(AString));
end;

function TQAMF3Helpher.StoreObject(const AObject: variant): integer;
begin
  Result := FObjectList.Count;
  FObjectList.AddManagedObject(Result, TVariantObject.Create(AObject));
end;

function TQAMF3Helpher.GetDateObject(AIndex: longint): TDateTime;
var
  o: TDateTimeObject;
begin
  o := TDateTimeObject(FObjectList.GetObject(AIndex));
  Result := o.Value;
end;

function TQAMF3Helpher.GetStringObject(AIndex: longint): string;
var
  o: TStringObject;
begin
  o := TStringObject(FStringList.GetObject(AIndex));
  Result := o.Value;
end;

function TQAMF3Helpher.GetObject(AIndex: longint): variant;
var
  o: TVariantObject;
begin
  o := TVariantObject(FObjectList.GetObject(AIndex));
  Result := o.Value;
end;

function TQAMF3Helpher.GetTraitsObject(AIndex: longint): TAS3Traits;
var
  o: TAS3Traits;
begin
  o := TAS3Traits(FTraitsList.GetObject(AIndex));
  Result := o;
end;

function TQAMF3Helpher.GetTraitsReference(ATraits: TAS3Traits): longint;
var
  e: TCustomHashItem;
begin
  FTraitsList.ResetEnum;
  while FTraitsList.Enum(e) do
  begin
    if e.Obj = ATraits then
    begin
      Result := THashLongintItem(e).Key;
      exit;
    end;
  end;
  Result := -1;
end;

function TQAMF3Helpher.GetDateReference(ADate: TDateTime): longint;
var
  e: TCustomHashItem;
begin
  FObjectList.ResetEnum;
  while FObjectList.Enum(e) do
  begin
    if (e.Obj is TDateTimeObject) and (TDateTimeObject(e.Obj).Value = ADate) then
    begin
      Result := THashLongintItem(e).Key;
      exit;
    end;
  end;
  Result := -1;
end;

function TQAMF3Helpher.GetStringReference(AString: string): longint;
var
  e: TCustomHashItem;
begin
  FStringList.ResetEnum;
  while FStringList.Enum(e) do
  begin
    if TStringObject(e.Obj).Value = AString then
    begin
      Result := THashLongintItem(e).Key;
      exit;
    end;
  end;
  Result := -1;
end;

function TQAMF3Helpher.GetObjectReference(AObject: variant): longint;
var
  ii1, ii2: IInterface;
  v: variant;
  e: TCustomHashItem;
begin
  if VarType(AObject) = varUnknown then 
    ii1 := AObject
  else
    ii1 := nil;
  if ii1 <> nil then
  begin
    FObjectList.ResetEnum;
    while FObjectList.Enum(e) do
    begin
      if (e.Obj is TVariantObject) then
      begin
        v := TVariantObject(e.Obj).Value;
        if VarType(v) = varUnknown then
        begin
          ii2 := v;
          if ii1 = ii2 then
          begin
            Result := THashLongintItem(e).Key;
            exit;
          end;
        end;
      end;
    end;
  end
  else
  begin
    FObjectList.ResetEnum;
    while FObjectList.Enum(e) do
    begin
      if (e.Obj is TVariantObject) then
      begin
        v := TVariantObject(e.Obj).Value;
        if (VarType(v) <> varUnknown) and (v = AObject) then
        begin
          Result := THashLongintItem(e).Key;
          exit;
        end;
      end;
    end;
  end;
  Result := -1;
end;

function TQAMF3Helpher.ReadUINT32: cardinal;
var
  b: array[0..3] of byte;
begin
  FStream.Read(b[0], 4);
  Result := b[0] shl 24 + b[1] shl 16 + b[2] shl 8 + b[3];
  ;
end;

function TQAMF3Helpher.ReadUINT16: cardinal;
var
  b: array[0..1] of byte;
begin
  FStream.Read(b[0], 2);
  Result := b[0] shl 8 + b[1];
end;

function TQAMF3Helpher.ReadINT32: integer;
var
  b: array[0..3] of byte;
begin
  FStream.Read(b[0], 4);
  Result := b[0] shl 24 + b[1] shl 16 + b[2] shl 8 + b[3];
end;

function TQAMF3Helpher.ReadUINT29: integer;
var
  b: array[0..3] of byte;
const
  AMF3_MAX_VALUE = 268435455;
begin
  FStream.Read(b[0], 1);
  if (b[0] and $80) = $80 then
  begin
    FStream.Read(b[1], 1);
    if (b[1] and $80) = $80 then
    begin
      FStream.Read(b[2], 1);
      if (b[2] and $80) = $80 then
      begin
        FStream.Read(b[3], 1);
        Result := ((b[0] and $7F) shl 22) + ((b[1] and $7F) shl 15) + ((b[2] and $7F) shl 8) + b[3];
      end
      else
        Result := ((b[0] and $7F) shl 14) + ((b[1] and $7F) shl 7) + b[2];
    end
    else
      Result := ((b[0] and $7F) shl 7) + b[1];
  end
  else
    Result := b[0];
  if (Result > AMF3_MAX_VALUE) then
    Dec(Result, 1 shl 29);
end;

procedure TQAMF3Helpher.WriteUINT32(const AValue: cardinal);
var
  b: array[0..3] of byte;
begin
  b[3] := AValue and $FF;
  b[2] := (AValue shr 8) and $FF;
  b[1] := (AValue shr 16) and $FF;
  b[0] := (AValue shr 24) and $FF;
  FStream.Write(b[0], 4);
end;

procedure TQAMF3Helpher.WriteUINT16(const AValue: cardinal);
var
  b: array[0..1] of byte;
begin
  b[1] := AValue and $FF;
  b[0] := (AValue shr 8) and $FF;
  FStream.Write(b[0], 2);
end;

procedure TQAMF3Helpher.WriteINT32(const AValue: integer);
var
  b: array[0..3] of byte;
begin
  b[3] := AValue and $FF;
  b[2] := (AValue shr 8) and $FF;
  b[1] := (AValue shr 16) and $FF;
  b[0] := (AValue shr 24) and $FF;
  FStream.Write(b[0], 4);
end;

procedure TQAMF3Helpher.WriteUINT29(const AValue: integer);
var
  b: array[0..3] of byte;
  i, n: integer;
begin
  n := AValue and $1FFFFFFF;
  if n < $80 then
  begin
    b[0] := (n and $7F);
    i := 1;
  end
  else if n < $4000 then
  begin
    b[0] := (n shr 7) and $7F or $80;
    b[1] := (n and $7F);
    i := 2;
  end
  else if n < $200000 then
  begin
    b[0] := (n shr 14) and $7F or $80;
    b[1] := (n shr 7) and $7F or $80;
    b[2] := (n and $7F);
    i := 3;
  end
  else
  begin
    b[0] := (n shr 22) and $7F or $80;
    b[1] := (n shr 15) and $7F or $80;
    b[2] := (n shr 8) and $7F or $80;
    b[3] := (n and $FF);
    i := 4;
  end;
  FStream.Write(b[0], i);
end;

function TQAMF3Helpher.ReadAMF0Type: TAMF0DataTypes;
var
  b: byte;
begin
  FStream.Read(b, 1);
  Result := TAMF0DataTypes(b);
end;

function TQAMF3Helpher.ReadAMF0Boolean: boolean;
var
  b: byte;
begin
  FStream.Read(b, 1);
  Result := b = 1;
end;

function TQAMF3Helpher.ReadAMF0String: WideString;
var
  l: cardinal;
  s: AnsiString;
begin
  l := ReadUINT16;
  SetLength(s, l);
  if l > 0 then
  begin
    FStream.Read(s[1], l);
    Result := UTF8Decode(s);
  end
  else
    Result := WideString(s);
end;

function TQAMF3Helpher.ReadAMF0LongString: WideString;
var
  l: cardinal;
  s: AnsiString;
begin
  l := ReadUINT32;
  SetLength(s, l);
  FStream.Read(s[1], l);
  Result := UTF8Decode(s);
end;

function TQAMF3Helpher.ReadAMF0Object: variant;
var
  o: IAS3Object;
  key: WideString;
  eoi: boolean; 
  v: variant;
begin
  o := TAS3Object.Create;
  while true do
  begin
    key := ReadAMF0String;
    v := ReadAMF0Variant(eoi);
    if eoi then
      break;
    o.SetValue(key, v);
  end;
  Result := o;
end;

function TQAMF3Helpher.ReadAMF0Number: double;
var
  MyDouble: array[0..7] of byte;
  pD: PDouble;
  b: byte;
begin
  FStream.Read(MyDouble[0], 8);
  b := MyDouble[0];
  MyDouble[0] := MyDouble[7];
  MyDouble[7] := b;
  b := MyDouble[1];
  MyDouble[1] := MyDouble[6];
  MyDouble[6] := b;
  b := MyDouble[2];
  MyDouble[2] := MyDouble[5];
  MyDouble[5] := b;
  b := MyDouble[3];
  MyDouble[3] := MyDouble[4];
  MyDouble[4] := b;
  pD := PDouble(@MyDouble[0]);
  Result := pD^;
end;

function TQAMF3Helpher.ReadAMF0XML: WideString;
var
  s: WideString;
begin
  s := ReadAMF0LongString;
  Result := s;
end;

function TQAMF3Helpher.ReadAMF0DateTime: TDateTime;
var
  dt: double;
begin
  dt := ReadAMF0Number;
  Result := FBaseDate + (trunc(dt) / MSecsPerDay);

  ReadUINT16;
end;

function TQAMF3Helpher.ReadAMF0AssociativeArray: variant;
var
  o: IAS3Array;
  key: WideString;
  eoi: boolean;
  v: variant;
begin
  o := TAS3Array.Create;

  ReadUINT32;
  key := ReadAMF0String;
  while true do
  begin
    v := ReadAMF0Variant(eoi);
    if eoi then
      break;
    o.SetValue(key, v);
  end;

  Result := o;
end;

function TQAMF3Helpher.ReadAMF0Array: variant;
var
  i, n: integer;
  eoi: boolean;
begin
  n := ReadINT32;
  Result := VarArrayCreate([0, n - 1], varVariant);
  for i := 0 to n - 1 do
    Result[i] := ReadAMF0Variant(eoi);
end;

function TQAMF3Helpher.ReadAMF0Variant(var AEOI: boolean): variant;
var
  t: TAMF0DataTypes;
begin
  AEOI := false;
  t := ReadAMF0Type;
  case t of
    amf0dtUndefined:
      Result := Unassigned;

    amf0dtNull:
      Result := null;

    amf0dtObjectEnd:
      begin
        Result := null;
        AEOI := true;
      end;

    amf0dtBoolean:
      Result := ReadAMF0Boolean;

    amf0dtNumber:
      Result := ReadAMF0Number;

    amf0dtString:
      Result := ReadAMF0String;

    amf0dtLongString:
      Result := ReadAMF0LongString;

    amf0dtDateTime:
      Result := ReadAMF0DateTime;

    amf0dtXML:
      Result := ReadAMF0XML;

    amf0dtAssociativeArray:
      Result := ReadAMF0AssociativeArray;

    amf0dtArray:
      Result := ReadAMF0Array;

    amf0dtAMF3:
      Result := ReadAMF3Variant;

    amf0dtObject:
      Result := ReadAMF0Object;

  else
    raise Exception.Create('Unsupported AMF0 datatype (' + inttostr(ord(t)) + ')');
  end;
end;

procedure TQAMF3Helpher.WriteAMF0Type(const AValue: TAMF0DataTypes);
var
  b: byte;
begin
  b := ord(AValue);
  FStream.Write(b, 1);
end;

procedure TQAMF3Helpher.WriteAMF0Boolean(const AValue: boolean);
var
  b: byte;
begin
  if AValue then
    b := 1
  else
    b := 0;
  FStream.Write(b, 1);
end;

procedure TQAMF3Helpher.WriteAMF0String(const AValue: WideString);
var
  l: cardinal;
  s: AnsiString;
begin
  s := UTF8Encode(AValue);
  l := length(s);
  WriteUINT16(l);
  FStream.Write(s[1], l);
end;

procedure TQAMF3Helpher.WriteAMF0LongString(const AValue: WideString);
var
  l: cardinal;
  s: AnsiString;
begin
  s := UTF8Encode(AValue);
  l := length(s);
  WriteUINT32(l);
  FStream.Write(s[1], l);
end;

procedure TQAMF3Helpher.WriteAMF0Object(const AValue: variant);
begin
  raise Exception.Create('WriteAMF0Object currently not supported.');
end;

procedure TQAMF3Helpher.WriteAMF0Number(const AValue: double);
var
  MyDouble: array[0..7] of byte;
  pb: PByte;
begin

  pb := PByte(@AValue);
  inc(pb, 7);
  MyDouble[0] := pb^;
  dec(pb);
  MyDouble[1] := pb^;
  dec(pb);
  MyDouble[2] := pb^;
  dec(pb);
  MyDouble[3] := pb^;
  dec(pb);
  MyDouble[4] := pb^;
  dec(pb);
  MyDouble[5] := pb^;
  dec(pb);
  MyDouble[6] := pb^;
  dec(pb);
  MyDouble[7] := pb^;
  FStream.Write(MyDouble[0], 8);
end;

procedure TQAMF3Helpher.WriteAMF0XML(const AValue: variant);
begin
  raise Exception.Create('WriteAMF0XML currently not supported.');
end;

procedure TQAMF3Helpher.WriteAMF0DateTime(const AValue: TDateTime);
var
  dt: double;
  ms: double;
begin
  dt := AValue - FBaseDate;
  ms := dt * MSecsPerDay;
  WriteAMF0Number(ms);
  WriteUINT16(0);
end;

procedure TQAMF3Helpher.WriteAMF0Array(const AValue: variant);
var
  n, l, h, i: integer;
  a: IAS3Array;
  v, vk: variant;
begin
  if VarIsArray(AValue) then
  begin
    l := VarArrayLowBound(AValue, 1);
    h := VarArrayHighBound(AValue, 1);
    n := h - l + 1;
    WriteINT32(n);
    for i := l to h do
      WriteAMF0Variant(AValue[i]);
    exit;
  end;

  a := IInterface(AValue) as IAS3Array;
  if not a.GetIsAssociative then
  begin
    l := 0;
    h := a.GetCount - 1;
    n := h - l + 1;
    WriteINT32(n);
    for i := l to h do
      WriteAMF0Variant(a.GetValue(i));
    exit;
  end;

  a.ResetEnum;
  WriteINT32(a.GetCount);
  while (a.Enum(vk, v)) do
  begin
    WriteAMF0String(vk);
    WriteAMF0Variant(v);
  end;
  WriteAMF0String('');
end;

procedure TQAMF3Helpher.WriteAMF0Variant(const AValue: variant);
var
  ia: IAS3Array;
  io: IAS3Object;
  t: integer;
  s: WideString;
begin
  t := VarType(AValue);
  if VarIsArray(AValue) then
  begin
    WriteAMF0Type(amf0dtArray);
    WriteAMF0Array(AValue);
  end
  else
  begin
    case t of
      varOleStr:
        if length(AValue) < 32768 then
        begin
          WriteAMF0Type(amf0dtString);
          WriteAMF0String(AValue);
        end
        else
        begin
          WriteAMF0Type(amf0dtLongString);
          WriteAMF0LongString(AValue);
        end;

      varBoolean:
        begin
          WriteAMF0Type(amf0dtBoolean);
          WriteAMF0Boolean(AValue);
        end;

      varDate:
        begin
          WriteAMF0Type(amf0dtDateTime);
          WriteAMF0DateTime(AValue);
        end;

      varDouble, varSingle, varCurrency:
        begin
          WriteAMF0Type(amf0dtNumber);
          WriteAMF0Number(AValue);
        end;

      varNull:
        WriteAMF0Type(amf0dtNull);

      varEmpty:
        WriteAMF0Type(amf0dtUndefined);

      varUnknown:
        begin
          if Supports(AValue, IAS3Array, ia) then
          begin
            if ia.GetIsAssociative then
              WriteAMF0Type(amf0dtAssociativeArray)
            else
              WriteAMF0Type(amf0dtArray);
            WriteAMF0Array(AValue);
          end
          else if Supports(AValue, IAS3Object, io) then
          begin
            WriteAMF0Type(amf0dtObject);
            WriteAMF0Object(AValue);
          end;
        end;
    else
      begin
        s := AValue;
        if length(s) < 32768 then
        begin
          WriteAMF0Type(amf0dtString);
          WriteAMF0String(s)
        end
        else
        begin
          WriteAMF0Type(amf0dtLongString);
          WriteAMF0LongString(s);
        end;
      end;
    end;
  end;
end;

function TQAMF3Helpher.ReadAMF3Type: TAMF3DataTypes;
var
  b: byte;
begin
  FStream.Read(b, 1);
  Result := TAMF3DataTypes(b);
end;

function TQAMF3Helpher.ReadAMF3Number: double;
begin
  Result := ReadAMF0Number;
end;

function TQAMF3Helpher.ReadAMF3String: WideString;
var
  l: integer;
  UTF8: UTF8String;
begin
  l := ReadUINT29;
  if (l and $01) = $00 then 
  begin
    Result := GetStringObject(l shr 1);
    exit;
  end
  else
  begin
    l := l shr 1;
    if l = 0 then
    begin
      Result := '';
      exit;
    end;
    SetLength(UTF8, l);
    FStream.Read(UTF8[1], l);
    Result := UTF8Decode(UTF8);
    StoreStringObject(Result);
  end;
end;

function TQAMF3Helpher.ReadAMF3DateTime: TDateTime;
var
  i: integer;
  dt: double;
begin
  i := ReadUINT29;
  if (i and $01) = $01 then
  begin
    dt := ReadAMF0Number;
    Result := FBaseDate + (trunc(dt) / MSecsPerDay);
    StoreDateObject(Result);
  end
  else
  begin
    i := i shr 1;
    Result := GetDateObject(i);
  end;
end;

{function TQAMF3Helpher._getClassAliasRegistry(Alias:string):string;
begin
  Result:='';
  Alias:=UpperCase(Alias);
  if Alias='DSK' then
  begin
    Result:='flex.messaging.messages.AcknowledgeMessageExt';
  end else
  if Alias='DSA' then
  begin
    Result:='flex.messaging.messages.AsyncMessageExt';
  end else
  if Alias='DSC' then
  begin
    Result:='flex.messaging.messages.CommandMessageExt';
  end;
end;

function TQAMF3Helpher._readByte:Byte;
var
 b:Byte;
begin
 FStream.Read(b, 1);
 if b>127 then
 begin
   b:=b-256;
 end;
 Result:=b;
end;

procedure TQAMF3Helpher._readFlags(var Flags:IntegerList);
var
 tmp:Integer;
begin
  SetLength(Flags,0);
  repeat
   tmp:=_readByte;
   SetLength(Flags,High(Flags)+2);
   Flags[High(Flags)]:=tmp;
   if (tmp and 128)=0 then Break;
  until True;
end;


function TQAMF3Helpher._readExternal_AbstractMessage: variant;
var
 tab:IntegerList;
 i,a,b:Integer;
 btx1,btx2:variant;
 c:Integer;
begin

end;  

function TQAMF3Helpher._readExternalClass(cnname:string): variant;
begin
  Result:=Null;
end;  }

function TQAMF3Helpher.ReadAMF3Object: variant;
var
  t: TAS3Traits;
  io: IAS3Object;
  o: TAS3Object;
  i, r, n: integer;
  cn: WideString;
  wbsz2:TStringArray;
  wb3:string;
begin
  r := ReadUINT29;
  if (r and $01) = $00 then
  begin
    r := r shr 1;
    Result := GetObject(r);
    exit;
  end;

  cn := ReadAMF3String;
  if (r and $03) = $01 then 
  begin
    t := GetTraitsObject(r shr 2);
    if t = nil then
      raise Exception.Create('Traits reference invalid.');
  end
  else
  begin
    t := TAS3Traits.Create;
    t.IsExternalized := ((r and $04) = $04);
    t.IsDynamic := ((r and $08) = $08);
    n := r shr 4;
    for i := 0 to n - 1 do
      t.Properties[i] := ReadAMF3String;
    StoreTraitsObject(t);
  end;

  if t.IsExternalized then
    raise Exception.Create('Externalized AS3 objects are not supported.');

  o := TAS3Object.Create;
  o.AS3ClassName := cn;
  o.AssignTraits(t);

  for i := 0 to t.PropertyCount - 1 do
    o.Values[t.Properties[i]] := ReadAMF3Variant;

  io := o;
  Result := io;
  StoreObject(Result);

end;

function TQAMF3Helpher.ReadAMF3Array: variant;
var
  r, l, i: integer;
  a: IAS3Array;
  s: WideString;
  v: variant;
begin
  r := ReadUINT29;
  if (r and $01) = $00 then
  begin
    i := r shr 1;
    Result := GetObject(i);
    exit;
  end;

  a := nil;
  while true do
  begin
    s := ReadAMF3String;
    if s = '' then
      break;

    if a = nil then
      a := TAS3Array.Create;
    v := ReadAMF3Variant;
    a.SetValue(s, v);
  end;

  l := r shr 1;
  if a = nil then
  begin
    Result := VarArrayCreate([0, l - 1], varVariant);
    for i := 0 to l - 1 do
    begin
      v := ReadAMF3Variant;
      Result[i] := v;
    end;
  end
  else
  begin
    for i := 0 to l - 1 do
    begin
      v := ReadAMF3Variant;
      a.SetValue(i, v);
    end;
    Result := a;
  end;
end;

function TQAMF3Helpher.ReadAMF3ByteArray: variant;
var
  i: integer;
  p: Pointer;
begin
  i := ReadUINT29;
  if (i and $01) = $01 then
  begin
    i := i shr 1;
    Result := VarArrayCreate([0, i - 1], varByte);
    p := VarArrayLock(Result);
    try
      FStream.Read(p^, i);
    finally
      VarArrayUnlock(Result);
    end;
  end
  else
  begin
    i := i shr 1;
    Result := GetObject(i);
  end;
end;

function TQAMF3Helpher.ReadAMF3Variant: variant;
var
  t: TAMF3DataTypes;
  b: byte;
begin
  FStream.Read(b, 1);
  t := TAMF3DataTypes(b);
  case t of
    amf3dtUndefined:
      Result := null;

    amf3dtNull:
      Result := null;

    amf3dtBooleanFalse:
      Result := False;

    amf3dtBooleanTrue:
      Result := True;

    amf3dtInteger:
      Result := ReadUINT29;

    amf3dtNumber:
      Result := ReadAMF3Number;

    amf3dtString:
      Result := ReadAMF3String;

    amf3dtDateTime:
      Result := ReadAMF3DateTime;

    amf3dtObject:
      Result := ReadAMF3Object;

    amf3dtArray:
      Result := ReadAMF3Array;

    amf3dtByteArray:
      Result := ReadAMF3ByteArray;

  else
    raise Exception.Create('Unsupported AMF3 datatype (' + inttostr(b) + ')');
  end;
end;

procedure TQAMF3Helpher.WriteAMF3Number(const AValue: double);
begin
  WriteAMF0Number(AValue);
end;

procedure TQAMF3Helpher.WriteAMF3String(const AValue: WideString);
var
  i, l: integer;
  UTF8: UTF8String;
  b:Byte;
begin
  i := GetStringReference(AValue);
  if i >= 0 then
  begin
    WriteUINT29(i shl 1);
    exit;
  end;

  UTF8 := UTF8Encode(AValue);
  l := length(UTF8);
  i := (l shl 1) or $01;
  WriteUINT29(i);
  if l > 0 then
  begin
    FStream.Write(UTF8[1], l);
    StoreStringObject(AValue);
  end;
end;

procedure TQAMF3Helpher.WriteAMF3DateTime(const AValue: TDateTime);
var
  i: integer;
  dt: double;
begin
  i := GetDateReference(AValue);
  if i >= 0 then
  begin
    WriteUINT29(i shl 1);
    exit;
  end;

  WriteUINT29(0 or $01);
  dt := (AValue - FBaseDate) * MSecsPerDay;
  WriteAMF0Number(dt);
  StoreDateObject(dt);
end;

procedure TQAMF3Helpher.WriteAMF3Array(const AValue: variant);
var
  n, l, h, i: integer;
  a: IAS3Array;
  v, vk: variant;
begin
  if VarIsArray(AValue) then
  begin
    l := VarArrayLowBound(AValue, 1);
    h := VarArrayHighBound(AValue, 1);
    n := h - l + 1;
    WriteUINT29((n shl 1) or $01);

    WriteAMF3String('');

    for i := l to h do
      WriteAMF3Variant(AValue[i]);
    exit;
  end;

  i := GetObjectReference(AValue);
  if i >= 0 then
  begin
    WriteUINT29(i shl 1);
    exit;
  end;

  if not Supports(AValue, IAS3Array, a) then
    raise Exception.Create('Variant interface not of type IAS3Array.');

  if not a.GetIsAssociative then
  begin
    l := 0;
    h := a.GetCount - 1;
    n := h - l + 1;
    WriteUINT29((n shl 1) or $01);

    WriteAMF3String('');

    for i := l to h do
      WriteAMF3Variant(a.GetValue(i));

    StoreObject(AValue);
    exit;
  end;

  a.ResetEnum;
  while (a.Enum(vk, v)) do
  begin
    WriteAMF3String(vk);
    WriteAMF3Variant(v);
  end;
  WriteAMF3String('');
  StoreObject(AValue);
end;

procedure TQAMF3Helpher.WriteAMF3ByteArray(const AValue: variant);
var
  i, l, h: integer;
  p: Pointer;
begin
  i := GetObjectReference(AValue);
  if i >= 0 then
  begin
    WriteUINT29(i shl 1);
    exit;
  end;

  if not VarIsArray(AValue) then
    raise Exception.Create('Variant is not a byte array.');

  l := VarArrayLowBound(AValue, 1);
  h := VarArrayHighBound(AValue, 1);
  i := h - l + 1;
  WriteUINT29((i shl 1) or $01);
  ;
  p := VarArrayLock(AValue);
  try
    FStream.Write(p^, i);
  finally
    VarArrayUnlock(AValue);
  end;
  StoreObject(AValue);
end;

procedure TQAMF3Helpher.WriteAMF3Variant(const AValue: variant);
var
  ia: IAS3Array;
  io: IAS3Object;
  t: integer;
begin
  t := VarType(AValue);

  if VarIsArray(AValue) then
  begin
    if (t and VarTypeMask) = VarByte then
    begin
      WriteAMF3Type(amf3dtByteArray);
      WriteAMF3ByteArray(AValue)
    end
    else
    begin
      WriteAMF3Type(amf3dtArray);
      WriteAMF3Array(AValue);
    end;
  end
  else
  begin
    case t of
      varOleStr:
        begin
          WriteAMF3Type(amf3dtString);
          WriteAMF3String(AValue);
        end;

      varByte, varWord, varShortInt, varLongWord, varInteger:
        begin
          WriteAMF3Type(amf3dtInteger);
          WriteUINT29(AValue);
        end;

      varBoolean:
        begin
          if AValue then
            WriteAMF3Type(amf3dtBooleanTrue)
          else
            WriteAMF3Type(amf3dtBooleanFalse);
        end;

      varDate:
        begin
          WriteAMF3Type(amf3dtDateTime);
          WriteAMF3DateTime(AValue);
        end;

      varDouble, varSingle, varCurrency:
        begin
          WriteAMF3Type(amf3dtNumber);
          WriteAMF3Number(AValue);
        end;

      varNull:
        WriteAMF3Type(amf3dtNull);

      varEmpty:
        WriteAMF3Type(amf3dtUndefined);

      varUnknown:
        begin
          if Supports(AValue, IAS3Array, ia) then
          begin
            WriteAMF3Type(amf3dtArray);
            WriteAMF3Array(AValue);
          end
          else if Supports(AValue, IAS3Object, io) then
          begin
            WriteAMF3Type(amf3dtObject);
            WriteAMF3Object(AValue);
          end;
        end;

    else
      begin
        WriteAMF3Type(amf3dtString);
        WriteAMF3String(AValue);
      end;
    end;
  end;
end;

procedure TQAMF3Helpher.WriteAMF3Object(const AValue: variant);
var
  io: IAS3Object;
  i, r, n: integer;
begin
  i := GetObjectReference(AValue);
  if i >= 0 then
  begin
    WriteUINT29(i shl 1);
    exit;
  end;

  if not Supports(AValue, IAS3Object, io) then
    raise Exception.Create('Variant interface not of type IAS3Object.');

  if io.GetIsExternalized then
    raise Exception.Create('Externalized AS3 objects are not supported.');

  n := io.GetPropertyCount;
  r := (n shl 4) or $01 or $02;
  if io.GetIsExternalized then
    r := r or $04;
  if io.GetIsDynamic then
    r := r or $08;
  WriteUINT29(r);

  WriteAMF3String(io.GetAS3ClassName);

  for i := 0 to n - 1 do
    WriteAMF3String(io.GetProperty(i));

  for i := 0 to n - 1 do
    WriteAMF3Variant(io.GetValue(io.GetProperty(i)));
  StoreObject(AValue);
end;

procedure TQAMF3Helpher.WriteAMF3Type(const AValue: TAMF3DataTypes);
var
  b: byte;
begin
  b := ord(AValue);
  FStream.Write(b, 1);
end;

procedure TQAMF3Helpher.WriteWideString(const AName: string; const AWideString: WideString);
begin
  WriteString(AName, AWideString);
end;

procedure TQAMF3Helpher.WriteAnsiString(const AName: string; const AString: AnsiString);
begin
  WriteString(AName, string(AString));
end;

procedure TQAMF3Helpher.WriteString(const AName: string; const AString: string);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtLongString);
    WriteAMF0String(AString);
  end
  else
  begin
    WriteAMF3Type(amf3dtString);
    WriteAMF3String(AString);
  end;
end;

procedure TQAMF3Helpher.WriteInteger(const AName: string; const AInteger: integer);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtNumber);
    WriteAMF0Number(AInteger);
  end
  else
  begin
    WriteAMF3Type(amf3dtInteger);
    WriteUINT29(AInteger);
  end;
end;

procedure TQAMF3Helpher.WriteFloat(const AName: string; const AFloat: double);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtNumber);
    WriteAMF0Number(AFloat);
  end
  else
  begin
    WriteAMF3Type(amf3dtNumber);
    WriteAMF3Number(AFloat);
  end;
end;

procedure TQAMF3Helpher.WriteDateTime(const AName: string; const ADateTime: TDateTime);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtDateTime);
    WriteAMF0DateTime(ADateTime);
  end
  else
  begin
    WriteAMF3Type(amf3dtDateTime);
    WriteAMF3DateTime(ADateTime);
  end;
end;

procedure TQAMF3Helpher.WriteBoolean(const AName: string; const ABoolean: boolean);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtBoolean);
    WriteAMF0Boolean(ABoolean);
  end
  else
  begin
    if ABoolean then
      WriteAMF3Type(amf3dtBooleanTrue)
    else
      WriteAMF3Type(amf3dtBooleanFalse);
  end;
end;

procedure TQAMF3Helpher.WriteObject(const AName: string; const AObject: IInterface);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtObject);
    WriteAMF0Object(AObject);
    WriteAMF0Type(amf0dtObjectEnd);
  end
  else
  begin
    WriteAMF3Type(amf3dtObject);
    WriteAMF3Object(AObject);
  end;
end;

procedure TQAMF3Helpher.WriteVariant(const AName: string; const AVariant: variant);
begin
  if FType = amf0 then
    WriteAMF0Variant(AVariant)
  else
    WriteAMF3Variant(AVariant);
end;

procedure TQAMF3Helpher.WriteVariantArray(const AName: string; const AVariant: Variant);
begin
  if FType = amf0 then
  begin
    WriteAMF0Type(amf0dtArray);
    WriteAMF0Array(AVariant);
  end
  else
  begin
    WriteAMF3Type(amf3dtArray);
    WriteAMF3Array(AVariant);
  end;
end;

procedure TQAMF3Helpher.WriteVariantByteArray(const AName: string; const AVariant: Variant);
begin
  if FType = amf0 then
    raise Exception.Create('ByteArrays not supported in AMF0 mode.')
  else
  begin
    WriteAMF3Type(amf3dtByteArray);
    WriteAMF3ByteArray(AVariant);
  end;
end;

procedure TQAMF3Helpher.WriteStream(const AName: string; AStream: TStream; ALength: integer);
var
  v: variant;
  p: pointer;
  sz: integer;
begin
  sz := AStream.Size;
  v := VarArrayCreate([0, sz - 1], varByte);
  p := VarArrayLock(v);
  try
    AStream.Read(p^, sz);
  finally
    VarArrayUnlock(v);
  end;
  WriteVariantByteArray(AName, v);
end;

function TQAMF3Helpher.ReadWideString(const AName: string): WideString;
begin
  Result := ReadString(AName);
end;

function TQAMF3Helpher.ReadAnsiString(const AName: string): AnsiString;
begin
  Result := AnsiString(ReadString(AName));
end;

function TQAMF3Helpher.ReadString(const AName: string): string;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 = amf0dtLongString then
      Result := ReadAMF0LongString
    else if t0 = amf0dtString then
      Result := ReadAMF0String
    else
      raise Exception.Create('Invalid AMF0 string value.');
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtString then
      raise Exception.Create('Invalid AMF3 string value.');
    Result := ReadAMF3String;
  end;
end;

function TQAMF3Helpher.ReadInteger(const AName: string): integer;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 <> amf0dtNumber then
      raise Exception.Create('Invalid AMF0 integer value.');
    Result := trunc(ReadAMF0Number);
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtInteger then
      raise Exception.Create('Invalid AMF3 integer value.');
    Result := ReadUINT29;
  end;
end;

function TQAMF3Helpher.ReadFloat(const AName: string): double;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 <> amf0dtNumber then
      raise Exception.Create('Invalid AMF0 number value.');

    Result := ReadAMF0Number;
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtNumber then
      raise Exception.Create('Invalid AMF3 number value.');
    Result := ReadAMF3Number;
  end;
end;

function TQAMF3Helpher.ReadDateTime(const AName: string): TDateTime;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 <> amf0dtDateTime then
      raise Exception.Create('Invalid AMF0 datetime value.');
    Result := ReadAMF0DateTime;
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtDateTime then
      raise Exception.Create('Invalid AMF3 datetime value.');
    Result := ReadAMF3DateTime;
  end;
end;

function TQAMF3Helpher.ReadBoolean(const AName: string): boolean;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 <> amf0dtBoolean then
      raise Exception.Create('Invalid AMF0 boolean value.');
    Result := ReadAMF0Boolean;
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 = amf3dtBooleanFalse then
      Result := false
    else if t3 = amf3dtBooleanTrue then
      Result := true
    else
    begin
      raise Exception.Create('Invalid AMF3 boolean value.');
    end;
  end;
end;

function TQAMF3Helpher.ReadObject(const AName: string): IInterface;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 = amf0dtObject then
      Result := ReadAMF0Object
    else
      raise Exception.Create('Invalid AMF0 object value.');
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtObject then
      raise Exception.Create('Invalid AMF3 object value.');
    Result := ReadAMF3Object;
  end;
end;

function TQAMF3Helpher.ReadVariant(const AName: string): variant;
var
  eoi: boolean;
begin
  if FType = amf0 then
    Result := ReadAMF0Variant(eoi)
  else
    Result := ReadAMF3Variant;
end;

function TQAMF3Helpher.ReadVariantArray(const AName: string): variant;
var
  t0: TAMF0DataTypes;
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
  begin
    t0 := ReadAMF0Type;
    if t0 = amf0dtArray then
      Result := ReadAMF0Array
    else if t0 = amf0dtAssociativeArray then
      Result := ReadAMF0AssociativeArray
    else
      raise Exception.Create('Invalid AMF0 array value.');
  end
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtArray then
      raise Exception.Create('Invalid AMF3 array value.');
    Result := ReadAMF3Array;
  end;
end;

function TQAMF3Helpher.ReadVariantByteArray(const AName: string): variant;
var
  t3: TAMF3DataTypes;
begin
  if FType = amf0 then
    raise Exception.Create('ByteArrays not supported in AMF0 mode.')
  else
  begin
    t3 := ReadAMF3Type;
    if t3 <> amf3dtByteArray then
      raise Exception.Create('Invalid AMF3 bytearray value.');
    Result := ReadAMF3ByteArray;
  end;
end;

procedure TQAMF3Helpher.ReadStream(const AName: string; AStream: TStream; var ALength: integer);
var
  v: variant;
  p: pointer;
  sz: integer;
begin
  v := ReadVariantByteArray(AName);
  sz := VarArrayHighBound(v, 1) - VarArrayLowBound(v, 1) + 1;
  p := VarArrayLock(v);
  try
    AStream.Write(p^, sz);
  finally
    VarArrayUnlock(v);
  end;
end;


procedure TQAMF3Helpher.WriteCount(const AName: string; const ACount: integer);
begin
  WriteInteger(AName, ACount);
end;

procedure TQAMF3Helpher.WriteType(const AName: string; const AType: integer);
begin
  WriteInteger(AName, AType);
end;

procedure TQAMF3Helpher.WriteRange(const AName: string; const ALow, AHigh: integer);
begin
  WriteInteger(AName + '_LOW', ALow);
  WriteInteger(AName + '_HIGH', AHigh);
end;

procedure TQAMF3Helpher.WriteIdentifier(const AName: string; const AIdentifier: integer);
begin
  WriteInteger(AName, AIdentifier);
end;

procedure TQAMF3Helpher.WriteVersion(const AName: string; const AVersion: integer);
begin
  WriteInteger(AName, AVersion);
end;

function TQAMF3Helpher.ReadCount(const AName: string): integer;
begin
  Result := ReadInteger(AName);
end;

function TQAMF3Helpher.ReadType(const AName: string): integer;
begin
  Result := ReadInteger(AName);
end;

procedure TQAMF3Helpher.ReadRange(const AName: string; var ALow, AHigh: integer);
begin
  ALow := ReadInteger(AName + '_LOW');
  AHigh := ReadInteger(AName + '_HIGH');
end;

function TQAMF3Helpher.ReadIdentifier(const AName: string): integer;
begin
  raise Exception.Create('ReadIdentifier');
  Result := 0;
end;

function TQAMF3Helpher.ReadVersion(const AName: string): integer;
begin
  Result := 3;
end;

end.

