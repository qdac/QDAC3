unit Hashes;

// Source is altered by IVO GELOV to match the needs of JSON.pas

(*
      This library is Copyright (c) 2002 Ciaran McCreesh.

      Permission is granted to anyone to use this software for any purpose on
      any computer system, and to redistribute it freely, subject to the
      following restrictions:

      1. This software is distributed in the hope that it will be useful,
         but WITHOUT ANY WARRANTY; without even the implied warranty of
         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

      2. The origin of this software must not be misrepresented.

      3. Altered versions must be plainly marked as such, and must not be
         misrepresented as being the original software.

    Documentation:
      Please see:
        * http://www.opensourcepan.co.uk/libraries/hashes/
        * http://www.undu.com/articles/020604.html

    Other notes:
      This unit provides three hashes, TIntegerHash, TStringHash and
      TObjectHash. If you want a more precise kind (eg TComponentHash), it's
      easiest to descend from THash and copy the TObjectHash code. Note that
      TObjectHash is slightly different from TIntegerHash and TStringHash
      because it has to free items -- it cannot just overwrite them.

    Internal data representation:
      Each hash object has an array (potentially dynamically sized, but this
      isn't used yet) of 'buckets' (dynamic arrays). Each bucket is mapped
      to a series of hash values (we take the high order bits of the value
      calculated), so that every possible hash value refers to exactly one
      bucket. This reduces the amount of searching that has to be done to
      find an item, so it's much faster than linear or B-Tree storage.

      Each bucket contains a series of integers. These are indexes into an
      items array, which for type reasons is maintained by the descendant
      classes. These are recycled when the hash detects that it is becoming
      inefficient.
*)

interface

  uses SysUtils;

  const
    // This constant controls the initial size of the hash.
    c_HashInitialItemShift = 7;

    // How inefficient do we have to be before we automatically Compact?
    c_HashCompactR         = 2;   // This many spaces per item.
    c_HashCompactM         = 100; // Never for less than this number of spaces.

  type
    EHashError = class(Exception);
    EHashErrorClass = class of EHashError;
    EHashFindError = class(EHashError);

    // Record, should really be private but OP won't let us...
    THashRecord = record
      Hash: Cardinal;
      ItemIndex: integer;
      Key: WideString;
    end;

    // Base Hash class. Don't use this directly.
    THash = class
    protected
      f_Keys: array of array of THashRecord;
      f_CurrentItemShift: integer;

      // These are calculated from f_CurrentItemShift.
      f_CurrentItemCount: integer;
      f_CurrentItemMask: integer;
      f_CurrentItemMaxIdx: integer;

      f_SpareItems: array of integer;
      f_AllowCompact: boolean;

      procedure FUpdateMasks;
      procedure FUpdateBuckets;
      function FFindKey(const Key: WideString; var k, x: integer): boolean;

      // Add a new key, or change an existing one. Don't call this directly.
      procedure FSetOrAddKey(const Key: WideString; ItemIndex: integer);
      procedure FDeleteIndex(i: integer); virtual; abstract;
      function FAllocItemIndex: integer;
      procedure FMoveIndex(oldIndex, newIndex: integer); virtual; abstract;
      procedure FTrimIndexes(count: integer); virtual; abstract;
      procedure FClearItems; virtual; abstract;

      // Tell us where to start our compact count from. Override this.
      function FIndexMax: integer; virtual; abstract;

      // Compact, but only if we're inefficient.
      procedure FAutoCompact;
    public
      constructor Create; reintroduce; virtual;
      function Exists(const Key: WideString): boolean;
      procedure Rename(const Key, NewName: WideString);
      procedure Delete(const Key: WideString);
      procedure Compact;
      procedure Clear;
      property AllowCompact: boolean read f_AllowCompact write f_AllowCompact;
    end;

    // Hash of integers.
    TIntegerHash = class(THash)
    protected
      f_Items: array of integer;
      procedure FDeleteIndex(i: integer); override;
      function FGetItem(const Key: WideString): integer;
      procedure FSetItem(const Key: WideString; Value: integer);
      procedure FMoveIndex(oldIndex, newIndex: integer); override;
      procedure FTrimIndexes(count: integer); override;
      procedure FClearItems; override;
      function FIndexMax: integer; override;
    public
      property Items[const Key: WideString]: integer read FGetItem write FSetItem; default;
    end;

implementation

function HashOf(const key: WideString): cardinal;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to length(key) do
  begin
    Result := ((Result shl 5) or (Result shr 27)) xor Cardinal(key[I]);
  end;
end;

function Hash32(const Text: WideString): cardinal;

  function SubHash(P: PCardinal): cardinal;
  var
    s1,s2: cardinal;
    i, L: Cardinal;
  const
    Mask: array[0..3] of cardinal = (0,$ff,$ffff,$ffffff);
  begin
    if P<>nil then
    begin
      L := PCardinal(Cardinal(P)-4)^; // fast lenght(Text)
      s1 := 0;
      s2 := 0;
      for i := 1 to L shr 4 do
      begin
        // 16 bytes (4 DWORD) by loop - aligned read
        inc(s1,P^);
        inc(s2,s1);
        inc(P);
        inc(s1,P^);
        inc(s2,s1);
        inc(P);
        inc(s1,P^);
        inc(s2,s1);
        inc(P);
        inc(s1,P^);
        inc(s2,s1);
        inc(P);
      end;
      for i := 1 to (L shr 2)and 3 do
      begin
        // 4 bytes (DWORD) by loop
        inc(s1,P^);
        inc(s2,s1);
        inc(P);
      end;
      inc(s1,P^ and Mask[L and 3]);      // remaining 0..3 bytes
      inc(s2,s1);
      result := s1 xor (s2 shl 16);
    end
    else result := 0;
  end;

begin 
  // use a sub function for better code generation under Delphi
  result := SubHash(@Text[1]);
end;

// ===== THash =====

constructor THash.Create;
begin
  inherited;
  f_CurrentItemShift := c_HashInitialItemShift;
  FUpdateMasks;
  FUpdateBuckets;
  f_AllowCompact := true;
end;

procedure THash.Delete(const Key: WideString);
var
  k, x, i: integer;
begin
  if (FFindKey(Key, k, x)) then
  begin
    // Delete the Index entry.
    i := f_Keys[k][x].ItemIndex;
    FDeleteIndex(i);
    // Add the index to the Spares list.
    SetLength(f_SpareItems, Length(f_SpareItems) + 1);
    f_SpareItems[High(f_SpareItems)] := i;
    // Overwrite key with the last in the list.
    f_Keys[k][x] := f_Keys[k][High(f_Keys[k])];
    // Delete the last in the list.
    SetLength(f_Keys[k], Length(f_Keys[k]) - 1);
  end;
  //else raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
  FAutoCompact;
end;

function THash.Exists(const Key: WideString): boolean;
var
  dummy1, dummy2: integer;
begin
  result := FFindKey(Key, dummy1, dummy2);
end;

procedure THash.FSetOrAddKey(const Key: WideString; ItemIndex: integer);
var
  k, x, i: integer;
begin
  // Exists already?
  if (FFindKey(Key, k, x)) then
  begin
    // Yep. Delete the old stuff and set the new value.
    i := f_Keys[k][x].ItemIndex;
    FDeleteIndex(i);
    f_Keys[k][x].ItemIndex := ItemIndex;
    // Add the index to the spares list.
    SetLength(f_SpareItems, Length(f_SpareItems) + 1);
    f_SpareItems[High(f_SpareItems)] := i;
  end
  else
  begin
    // No, create a new one.
    SetLength(f_Keys[k], Length(f_Keys[k]) + 1);
    f_Keys[k][High(f_Keys[k])].Key := Key;
    f_Keys[k][High(f_Keys[k])].ItemIndex := ItemIndex;
    f_Keys[k][High(f_Keys[k])].Hash := Hash32(Key);
  end;
end;

function THash.FFindKey(const Key: WideString; var k, x: integer): boolean;
var
  i: integer;
  h: cardinal;
begin
  // Which bucket?
  h := Hash32(Key);
  k := h and f_CurrentItemMask;
  result := false;
  // Look for it.
  for i := 0 to High(f_Keys[k]) do
    //if (f_Keys[k][i].Hash = h) or true then
      if (f_Keys[k][i].Key = Key) then
      begin
        // Found it!
        result := true;
        x := i;
        break;
      end;
end;

procedure THash.Rename(const Key, NewName: WideString);
var
  k, x, i: integer;
begin
  if (FFindKey(Key, k, x)) then
  begin
    // Remember the ItemIndex.
    i := f_Keys[k][x].ItemIndex;
    // Overwrite key with the last in the list.
    f_Keys[k][x] := f_Keys[k][High(f_Keys[k])];
    // Delete the last in the list.
    SetLength(f_Keys[k], Length(f_Keys[k]) - 1);
    // Create the new item.
    FSetOrAddKey(NewName, i);
  end
  else raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
  FAutoCompact;
end;

function THash.FAllocItemIndex: integer;
begin
  if (Length(f_SpareItems) > 0) then
  begin
    // Use the top SpareItem.
    result := f_SpareItems[High(f_SpareItems)];
    SetLength(f_SpareItems, Length(f_SpareItems) - 1);
  end
  else result := FIndexMax + 1;
end;

procedure THash.Compact;
var
  aSpaces: array of boolean;
  aMapping: array of integer;
  i, j: integer;
begin
  { Find out where the gaps are. We could do this by sorting, but that's at
    least O(n log n), and sometimes O(n^2), so we'll go for the O(n) method,
    even though it involves multiple passes. Note that this is a lot faster
    than it looks. Disabling this saves about 3% in my benchmarks, but uses a
    lot more memory. }
  if (AllowCompact) then
  begin
    SetLength(aSpaces, FIndexMax + 1);
    SetLength(aMapping, FIndexMax + 1);
    for i := 0 to High(aSpaces) do
      aSpaces[i] := false;
    for i := 0 to High(aMapping) do
      aMapping[i] := i;
    for i := 0 to High(f_SpareItems) do
      aSpaces[f_SpareItems[i]] := true;

    // Starting at the low indexes, fill empty ones from the high indexes.
    i := 0;
    j := FIndexMax;
    while (i < j) do
    begin
      if (aSpaces[i]) then
      begin
        while ((i < j) and (aSpaces[j])) do
          dec(j);
        if (i < j) then
        begin
          aSpaces[i] := false;
          aSpaces[j] := true;
          FMoveIndex(j, i);
          aMapping[j] := i
        end;
      end
      else inc(i);
    end;

    j := FIndexMax;
    while (aSpaces[j]) do
      dec(j);

    // Trim the items array down to size.
    FTrimIndexes(j + 1);
    // Clear the spaces.
    SetLength(f_SpareItems, 0);
    // Update our buckets.
    for i := 0 to f_CurrentItemMaxIdx do
      for j := 0 to High(f_Keys[i]) do
        f_Keys[i][j].ItemIndex := aMapping[f_Keys[i][j].ItemIndex];
  end;
end;

procedure THash.FAutoCompact;
begin
  if (AllowCompact) then
    if (Length(f_SpareItems) >= c_HashCompactM) then
      if (FIndexMax * c_HashCompactR > Length(f_SpareItems)) then
        Compact;
end;

procedure THash.Clear;
var
  i: integer;
begin
  FClearItems;
  SetLength(f_SpareItems, 0);
  for i := 0 to f_CurrentItemMaxIdx do
    SetLength(f_Keys[i], 0);
end;

procedure THash.FUpdateMasks;
begin
  f_CurrentItemMask := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemMaxIdx := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemCount := (1 shl f_CurrentItemShift);
end;

procedure THash.FUpdateBuckets;
begin
  // This is just a temporary thing.
  SetLength(f_Keys, f_CurrentItemCount);
end;

// ===== TIntegerHash =====

procedure TIntegerHash.FDeleteIndex(i: integer);
begin
  f_Items[i] := 0;
end;

function TIntegerHash.FGetItem(const Key: WideString): integer;
var
  k, x: integer;
begin
  if (FFindKey(Key, k, x)) then
    result := f_Items[f_Keys[k][x].ItemIndex]
  else Result:=-1;
    //raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TIntegerHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  f_Items[newIndex] := f_Items[oldIndex];
end;

procedure TIntegerHash.FSetItem(const Key: WideString; Value: integer);
var
  k, x, i: integer;
begin
  if (FFindKey(Key, k, x)) then
    f_Items[f_Keys[k][x].ItemIndex] := Value
  else
  begin
    // New index entry, or recycle an old one.
    i := FAllocItemIndex;
    if (i > High(f_Items)) then
      SetLength(f_Items, i + 1);
    f_Items[i] := Value;
    // Add it to the hash.
    SetLength(f_Keys[k], Length(f_Keys[k]) + 1);
    f_Keys[k][High(f_Keys[k])].Key := Key;
    f_Keys[k][High(f_Keys[k])].ItemIndex := i;
    f_Keys[k][High(f_Keys[k])].Hash := Hash32(Key);
  end;
end;

function TIntegerHash.FIndexMax: integer;
begin
  result := High(f_Items);
end;

procedure TIntegerHash.FTrimIndexes(count: integer);
begin
  SetLength(f_Items, count);
end;

procedure TIntegerHash.FClearItems;
begin
  SetLength(f_Items, 0);
end;

end.

