unit OHashedStrings;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OHashedStrings.pas

  TOHashedStrings
    - hashed unsorted string list
    - always keeps original order of strings (in contrary to TDictionary<,>)
    - every string is unique in the list
    - fast IndexOf() function
    - an object can be associated with every string

  TOHashedStringDictionary
    - a TDictionary<String,String> replacement for FPC and D6-2007
    - always keeps original order of keys
    - every key is unique in the list
}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes,
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    System.Generics.Collections,
    {$ELSE}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}

  OWideSupp;

type

  OHashedStringsIndex = Integer;

  POHashItem = ^TOHashItem;
  TOHashItem = packed {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fNext: POHashItem;
    fTextFast: OFastString;
    fIndex: OHashedStringsIndex;

    {$IFNDEF O_UNICODE}
    function GetText: OWideString;
    {$ENDIF}
  public
    function SameText(const aText: OWideString): Boolean;
  public
    property TextFast: OFastString read fTextFast;
    property Text: OWideString read {$IFDEF O_UNICODE}fTextFast{$ELSE}GetText{$ENDIF};
  end;

  TOHashedStrings = class(TPersistent)
  private
    fItems: Array of POHashItem;//array indexed by index
    fObjects: Array of TObject;
    fNextItemId: OHashedStringsIndex;//next list index to use
    fItemLength: OHashedStringsIndex;//count of allocated items in fList
    fMaxItemsBeforeGrowBuckets: OHashedStringsIndex;
    fBuckets: Array of POHashItem;//array indexed by hash

    fLastHashI: OHashedStringsIndex;
  protected
    function Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POHashItem;
    //function HashOf(const aKey: OWideString): Cardinal; virtual;//must be virtual for better performance in D7 (don't ask me why)
    //function HashOfFast(const aKey: OFastString): Cardinal; virtual;//must be virtual for better performance in D7 (don't ask me why)
    procedure AddItem(const aItem: POHashItem; const aHash: OHashedStringsIndex);

    procedure GrowBuckets;
    procedure ClearBuckets;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function IndexOf(const aText: OWideString): OHashedStringsIndex;
    function Add(const aText: OWideString): OHashedStringsIndex; overload;
    function Add(const aText: OWideString; var outNewEntry: Boolean): OHashedStringsIndex; overload;
    function Get(const aIndex: OHashedStringsIndex): OWideString;
    function GetItem(const aIndex: OHashedStringsIndex): POHashItem;
    procedure SetObject(const aIndex: OHashedStringsIndex; const aObject: TObject);
    function GetObject(const aIndex: OHashedStringsIndex): TObject;
    {$IFNDEF NEXTGEN}
    procedure SetPObject(const aIndex: OHashedStringsIndex; const aObject: Pointer);
    function GetPObject(const aIndex: OHashedStringsIndex): Pointer;
    {$ENDIF}
    property Count: OHashedStringsIndex read fNextItemId;
    procedure Clear(const aFullClear: Boolean = True);
  end;

  TOHashedStringDictionaryEnum = class;
  {$IFDEF O_GENERICS}
  TOHashedStringDictionaryPair = TPair<OWideString,OWideString>;
  {$ELSE}
  TOHashedStringDictionaryPair = record
    Key: OWideString;
    Value: OWideString;
  end;
  {$ENDIF}

  TOHashedStringDictionary = class(TPersistent)
  private
    fKeys: TOHashedStrings;
    fValues: TOWideStringList;
    function GetKey(const aIndex: OHashedStringsIndex): OWideString;
    function GetValue(const aIndex: OHashedStringsIndex): OWideString;
    procedure SetValue(const aIndex: OHashedStringsIndex; aValue: OWideString);
    function GetValueOfKey(const aKey: OWideString): OWideString;
    procedure SetValueOfKey(const aKey, aValue: OWideString);
    function GetPair(const aIndex: OHashedStringsIndex): TOHashedStringDictionaryPair;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function IndexOf(const aKey: OWideString): OHashedStringsIndex;
    function Add(const aKey, aValue: OWideString): OHashedStringsIndex;
    function TryGetValue(const aKey: OWideString; var outValue: OWideString): Boolean;
    function Count: OHashedStringsIndex;
    procedure Clear;
    {procedure Delete(const aIndex: OHashedStringsIndex);
    procedure Remove(const aKey: OWideString);}

    property Keys[const aIndex: OHashedStringsIndex]: OWideString read GetKey;
    property Values[const aIndex: OHashedStringsIndex]: OWideString read GetValue write SetValue;
    property Items[const aKey: OWideString]: OWideString read GetValueOfKey write SetValueOfKey; default;
    property Pairs[const aIndex: OHashedStringsIndex]: TOHashedStringDictionaryPair read GetPair;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TOHashedStringDictionaryEnum;
    {$ENDIF}
  end;

  TOHashedStringDictionaryEnum = class(TObject)
  private
    fIndex: OHashedStringsIndex;
    fDictionary: TOHashedStringDictionary;
  public
    constructor Create(aDictionary: TOHashedStringDictionary);
    function GetCurrent: TOHashedStringDictionaryPair;
    function MoveNext: Boolean;
  public
    property Current: TOHashedStringDictionaryPair read GetCurrent;
  end;

  POVirtualHashItem = ^TOVirtualHashItem;
  TOVirtualHashItem = packed record
    Next: POVirtualHashItem;
    Index: OHashedStringsIndex;
  end;

  {$IFDEF O_ANONYMOUS_METHODS}
  TOVirtualHashIndexGetStringProc = reference to procedure(const aIndex: OHashedStringsIndex;
    var outString: OWideString);
  TOVirtualHashIndexSameStringProc = reference to procedure(const aIndex: OHashedStringsIndex;
    const aString: OWideString; var outSameString: Boolean);
  {$ELSE}
  TOVirtualHashIndexGetStringProc = procedure(const aIndex: OHashedStringsIndex;
    var outString: OWideString) of Object;
  TOVirtualHashIndexSameStringProc = procedure(const aIndex: OHashedStringsIndex;
    const aString: OWideString; var outSameString: Boolean) of Object;
  {$ENDIF}

  TOVirtualHashIndex = class(TObject)
  private
    fList: Array of TOVirtualHashItem;//list of real items (by item index)
    fBuckets: Array of POVirtualHashItem;//indexed by hash
    fOnGetString: TOVirtualHashIndexGetStringProc;
    fOnSameString: TOVirtualHashIndexSameStringProc;
    fNextItemId: OHashedStringsIndex;//next list index to use

    fMaxItemsBeforeGrowBuckets: OHashedStringsIndex;

    procedure DefOnSameString(const aIndex: OHashedStringsIndex;
      const aString: OWideString; var outSameString: Boolean);

    function Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POVirtualHashItem;
    procedure GrowBuckets;
    procedure AddItem(const aItem: POVirtualHashItem; const aHash: OHashedStringsIndex);
  public
    constructor Create(
      const aOnGetString: TOVirtualHashIndexGetStringProc;
      const aOnSameString: TOVirtualHashIndexSameStringProc = nil);
  public
    procedure Clear(const aNewCount: Integer = 0);

    function IndexOf(const aText: OWideString): OHashedStringsIndex;
    function Add(const aText: OWideString): OHashedStringsIndex; overload;
    function Add(const aText: OWideString; var outNewEntry: Boolean): OHashedStringsIndex; overload;
  end;

function OHashedStringsIndexAssigned(const aId: OHashedStringsIndex): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}
function HashOf(const aKey: OWideString): Cardinal; {$IFDEF O_INLINE}inline;{$ENDIF}
function HashOfFast(const aKey: OFastString): Cardinal; {$IFDEF O_INLINE}inline;{$ENDIF}

const
  OHASHEDSTRINGSINDEX_UNASSIGNED = -1;

implementation

uses
  OXmlLng;

function OHashedStringsIndexAssigned(const aId: OHashedStringsIndex): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}
begin
  Result := aId <> OHASHEDSTRINGSINDEX_UNASSIGNED;
end;

function HashOf(const aKey: OWideString): Cardinal;
{$IFDEF O_UNICODE}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(aKey) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(aKey[I]);
end;
{$ELSE}
var
  I, xLen: Integer;
  xK: PAnsiChar;
begin
  Result := 0;
  xLen := Length(aKey);
  if xLen > 0 then
  begin
    xK := @aKey[1];
    for I := 1 to xLen*2 do begin
      Result := ((Result shl 2) or (Result shr (SizeOf(Result)*8 - 2))) xor
        Ord(xK^);
      Inc(xK);
    end;
  end;
end;
{$ENDIF}

function HashOfFast(const aKey: OFastString): Cardinal;
{$IFDEF O_UNICODE}
begin
  Result := HashOf(aKey);
end;
{$ELSE}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(aKey) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(aKey[I]);
end;
{$ENDIF}

{ TOHashedStringDictionary }

{$IFDEF O_ENUMERATORS}
function TOHashedStringDictionary.GetEnumerator: TOHashedStringDictionaryEnum;
begin
  Result := TOHashedStringDictionaryEnum.Create(Self);
end;
{$ENDIF}

function TOHashedStringDictionary.GetKey(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fKeys.Get(aIndex);
end;

function TOHashedStringDictionary.GetPair(
  const aIndex: OHashedStringsIndex): TOHashedStringDictionaryPair;
begin
  Result.Key := Keys[aIndex];
  Result.Value := Values[aIndex];
end;

function TOHashedStringDictionary.GetValue(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fValues[aIndex];
end;

function TOHashedStringDictionary.GetValueOfKey(
  const aKey: OWideString): OWideString;
begin
  Result := fValues[fKeys.IndexOf(aKey)];
end;

function TOHashedStringDictionary.IndexOf(
  const aKey: OWideString): OHashedStringsIndex;
begin
  Result := fKeys.IndexOf(aKey);
end;

procedure TOHashedStringDictionary.SetValue(const aIndex: OHashedStringsIndex; aValue: OWideString
  );
begin
  fValues[aIndex] := aValue;
end;

procedure TOHashedStringDictionary.SetValueOfKey(const aKey,
  aValue: OWideString);
begin
  fValues[fKeys.IndexOf(aKey)] := aValue;
end;

constructor TOHashedStringDictionary.Create;
begin
  inherited Create;

  fKeys := TOHashedStrings.Create;
  fValues := TOWideStringList.Create;
end;

destructor TOHashedStringDictionary.Destroy;
begin
  fKeys.Free;
  fValues.Free;

  inherited Destroy;
end;

function TOHashedStringDictionary.Add(const aKey, aValue: OWideString
  ): OHashedStringsIndex;
var
  xNew: Boolean;
begin
  Result := fKeys.Add(aKey, {%H-}xNew);
  if xNew then
    fValues.Add(aValue)
  else
    fValues[Result] := aValue;
end;

function TOHashedStringDictionary.TryGetValue(const aKey: OWideString;
  var outValue: OWideString): Boolean;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := fKeys.IndexOf(aKey);
  Result := (xIndex >= 0);
  if Result then
    outValue := fValues[xIndex]
  else
    outValue := '';
end;

function TOHashedStringDictionary.Count: OHashedStringsIndex;
begin
  Result := fKeys.Count;
end;

procedure TOHashedStringDictionary.AssignTo(Dest: TPersistent);
var
  xDest: TOHashedStringDictionary;
begin
  if Dest is TOHashedStringDictionary then begin
    xDest := TOHashedStringDictionary(Dest);

    xDest.fKeys.Assign(Self.fKeys);
    xDest.fValues.Assign(Self.fValues);
  end else
    inherited;
end;

procedure TOHashedStringDictionary.Clear;
begin
  if fKeys.Count > 0 then begin
    fValues.Clear;
    fKeys.Clear(True);
  end;
end;

{ TOHashedStringDictionaryEnum }

constructor TOHashedStringDictionaryEnum.Create(
  aDictionary: TOHashedStringDictionary);
begin
  inherited Create;

  fIndex := -1;
  fDictionary := aDictionary;
end;

function TOHashedStringDictionaryEnum.GetCurrent: TOHashedStringDictionaryPair;
begin
  Result := fDictionary.Pairs[fIndex];
end;

function TOHashedStringDictionaryEnum.MoveNext: Boolean;
begin
  Result := (fIndex < fDictionary.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ TOHashedStrings }

function TOHashedStrings.Add(const aText: OWideString): OHashedStringsIndex;
var
  x: Boolean;
begin
  Result := Add(aText, {%H-}x);
end;

function TOHashedStrings.Add(const aText: OWideString;
  var outNewEntry: Boolean): OHashedStringsIndex;
var
  xBucket: POHashItem;
  xHash: OHashedStringsIndex;
begin
  xBucket := Find(aText, {%H-}xHash);
  if Assigned(xBucket) then begin
    Result := xBucket.fIndex;
    outNewEntry := False;
    Exit;
  end;

  if fNextItemId = fMaxItemsBeforeGrowBuckets then
  begin
    GrowBuckets;
    xHash := HashOf(aText) mod Cardinal(Length(fBuckets));//must be here!!! -> the hash is changed!!!
  end;

  if fNextItemId = fItemLength then begin
    New(fItems[fNextItemId]);
    Inc(fItemLength);
  end;

  xBucket := fItems[fNextItemId];
  {$IFDEF O_UNICODE}
  xBucket.fTextFast := aText;
  {$ELSE}
  OWideToFast(aText, xBucket.fTextFast);
  {$ENDIF}
  xBucket.fIndex := fNextItemId;
  fObjects[fNextItemId] := nil;
  Result := fNextItemId;

  AddItem(xBucket, xHash);

  Inc(fNextItemId);

  outNewEntry := True;
end;

procedure TOHashedStrings.AddItem(const aItem: POHashItem;
  const aHash: OHashedStringsIndex);
begin
  aItem.fNext := fBuckets[aHash];
  fBuckets[aHash] := aItem;
end;

procedure TOHashedStrings.AssignTo(Dest: TPersistent);
var
  xDest: TOHashedStrings;
  I: Integer;
  x: Boolean;
begin
  if Dest is TOHashedStrings then begin
    xDest := TOHashedStrings(Dest);

    xDest.Clear(False);
    while xDest.fMaxItemsBeforeGrowBuckets < Self.Count do
      xDest.GrowBuckets;

    for I := 0 to Self.Count-1 do
      xDest.Add(Self.fItems[I].Text, {%H-}x);
  end else
    inherited;
end;

procedure TOHashedStrings.Clear(const aFullClear: Boolean);
var
  I: OHashedStringsIndex;
begin
  fNextItemId := 0;

  if aFullClear then
  begin
    for I := 0 to fItemLength-1 do
      Dispose(POHashItem(fItems[I]));
    fItemLength := 0;

    fLastHashI := 0;
    GrowBuckets;
  end else begin
    ClearBuckets;
  end;
end;

procedure TOHashedStrings.ClearBuckets;
var
  I: OHashedStringsIndex;
begin
  for I := 0 to Length(fBuckets)-1 do
    fBuckets[I] := nil;
end;

constructor TOHashedStrings.Create;
begin
  inherited;

  GrowBuckets;
end;

destructor TOHashedStrings.Destroy;
begin
  Clear(True);

  inherited;
end;

function TOHashedStrings.Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POHashItem;
begin
  outHash := HashOf(aKey) mod Cardinal(Length(fBuckets));
  Result := fBuckets[outHash];
  while Result <> nil do
  begin
    if Result.SameText(aKey) then
      Exit
    else
      Result := Result.fNext;
  end;
end;

function TOHashedStrings.Get(const aIndex: OHashedStringsIndex): OWideString;
begin
  if (aIndex < 0) or (aIndex >= fNextItemId) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := {$IFNDEF O_UNICODE}OFastToWide{$ENDIF}(fItems[aIndex].fTextFast);
end;

function TOHashedStrings.GetItem(const aIndex: OHashedStringsIndex): POHashItem;
begin
  Result := fItems[aIndex];
end;

function TOHashedStrings.GetObject(const aIndex: OHashedStringsIndex): TObject;
begin
  Result := fObjects[aIndex];
end;

const
  cHashTable: Array[0..27] of LongWord =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );

procedure TOHashedStrings.GrowBuckets;
var
  I: OHashedStringsIndex;
  xTableSize: LongWord;
begin
  ClearBuckets;

  xTableSize := cHashTable[fLastHashI];

  SetLength(fBuckets, xTableSize);
  fMaxItemsBeforeGrowBuckets := (xTableSize * 2) div 3;

  SetLength(fItems, fMaxItemsBeforeGrowBuckets);
  SetLength(fObjects, fMaxItemsBeforeGrowBuckets);

  for I := 0 to fNextItemId-1 do
    AddItem(fItems[I], HashOfFast(fItems[I].fTextFast) mod Cardinal(Length(fBuckets)));

  Inc(fLastHashI);
end;

function TOHashedStrings.IndexOf(
  const aText: OWideString): OHashedStringsIndex;
var
  xP: POHashItem;
  xH: OHashedStringsIndex;
begin
  xP := Find(aText, {%H-}xH);
  if xP <> nil then
    Result := xP.fIndex
  else
    Result := -1;
end;

procedure TOHashedStrings.SetObject(const aIndex: OHashedStringsIndex;
  const aObject: TObject);
begin
  fObjects[aIndex] := aObject;
end;

{$IFNDEF NEXTGEN}
function TOHashedStrings.GetPObject(
  const aIndex: OHashedStringsIndex): Pointer;
begin
  Result := Pointer(fObjects[aIndex]);//unsave but fine
end;

procedure TOHashedStrings.SetPObject(const aIndex: OHashedStringsIndex;
  const aObject: Pointer);
begin
  fObjects[aIndex] := TObject(aObject);//unsave but fine
end;
{$ENDIF}

{ TOHashItem }

{$IFNDEF O_UNICODE}
function TOHashItem.GetText: OWideString;
begin
  Result := OFastToWide(fTextFast);
end;
{$ENDIF}

function TOHashItem.SameText(const aText: OWideString): Boolean;
begin
  {$IFDEF O_UNICODE}
  Result := (fTextFast = aText);
  {$ELSE}
  Result :=
      (Length(fTextFast) = Length(aText)*SizeOf(OWideChar)) and
      ((fTextFast = '') or //must be here, otherwise @fTextFast[1] causes range check error
        CompareMem(@fTextFast[1], @aText[1], Length(fTextFast)));
  {$ENDIF}
end;

{ TOVirtualHashIndex }

constructor TOVirtualHashIndex.Create(
  const aOnGetString: TOVirtualHashIndexGetStringProc;
  const aOnSameString: TOVirtualHashIndexSameStringProc);
begin
  inherited Create;

  Assert(Assigned(aOnGetString));
  fOnGetString := aOnGetString;
  if Assigned(aOnSameString) then
    fOnSameString := aOnSameString
  else
    fOnSameString := DefOnSameString;

  GrowBuckets;
end;

procedure TOVirtualHashIndex.DefOnSameString(const aIndex: OHashedStringsIndex;
  const aString: OWideString; var outSameString: Boolean);
var
  xString: OWideString;
begin
  fOnGetString(aIndex, {%H-}xString);
  outSameString := (xString = aString);
end;

function TOVirtualHashIndex.Find(const aKey: OWideString;
  var outHash: OHashedStringsIndex): POVirtualHashItem;
var
  xSameString: Boolean;
begin
  outHash := HashOf(aKey) mod Cardinal(Length(fBuckets));
  Result := fBuckets[outHash];
  while Result <> nil do
  begin
    fOnSameString(Result.Index, aKey, {%H-}xSameString);
    if xSameString then
      Exit
    else
      Result := Result.Next;
  end;
end;

procedure TOVirtualHashIndex.GrowBuckets;
var
  I, xLastItemCount: OHashedStringsIndex;
  xKey: OWideString;
begin
  xLastItemCount := fNextItemId;
  Clear(fMaxItemsBeforeGrowBuckets+1);

  for I := 0 to xLastItemCount-1 do
  begin
    fOnGetString(I, {%H-}xKey);
    AddItem(@fList[I], HashOf(xKey) mod Cardinal(Length(fBuckets)));
  end;

  fNextItemId := xLastItemCount;
end;

function TOVirtualHashIndex.Add(const aText: OWideString): OHashedStringsIndex;
var
  x: Boolean;
begin
  Result := Add(aText, {%H-}x);
end;

function TOVirtualHashIndex.Add(const aText: OWideString;
  var outNewEntry: Boolean): OHashedStringsIndex;
var
  xBucket: POVirtualHashItem;
  xHash: OHashedStringsIndex;
begin
  xBucket := Find(aText, {%H-}xHash);
  if Assigned(xBucket) then begin
    Result := xBucket.Index;
    outNewEntry := False;
    Exit;
  end;

  if fNextItemId = fMaxItemsBeforeGrowBuckets then
  begin
    GrowBuckets;
    xHash := HashOf(aText) mod Cardinal(Length(fBuckets));//must be here!!! -> the hash is changed!!!
  end;

  AddItem(@fList[fNextItemId], xHash);
  Result := fNextItemId;

  Inc(fNextItemId);

  outNewEntry := True;
end;

procedure TOVirtualHashIndex.AddItem(const aItem: POVirtualHashItem;
  const aHash: OHashedStringsIndex);
begin
  aItem.Next := fBuckets[aHash];
  fBuckets[aHash] := aItem;
end;

function TOVirtualHashIndex.IndexOf(
  const aText: OWideString): OHashedStringsIndex;
var
  xP: POVirtualHashItem;
  xH: OHashedStringsIndex;
begin
  xP := Find(aText, {%H-}xH);
  if xP <> nil then
    Result := xP.Index
  else
    Result := -1;
end;

procedure TOVirtualHashIndex.Clear(const aNewCount: Integer);
var
  I: OHashedStringsIndex;
  xTableSize: LongWord;
begin
  if aNewCount > fMaxItemsBeforeGrowBuckets then
  begin
    for I := Low(cHashTable) to High(cHashTable) do
    if cHashTable[I] > Cardinal(aNewCount)*3 div 2 then
    begin
      xTableSize := cHashTable[I];

      SetLength(fBuckets, xTableSize);
      fMaxItemsBeforeGrowBuckets := (xTableSize * 2) div 3;

      Break;
    end;
  end;

  if Length(fList) < fMaxItemsBeforeGrowBuckets then
    SetLength(fList, fMaxItemsBeforeGrowBuckets*3 div 2);

  for I := Low(fList) to High(fList) do//must set all to nil!
  begin
    fList[I].Index := I;
    fList[I].Next := nil;
  end;

  for I := Low(fBuckets) to High(fBuckets) do//must set all to nil!
    fBuckets[I] := nil;

  fNextItemId := 0;
end;

end.

