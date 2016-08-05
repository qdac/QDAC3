unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, qstring, IniFiles, Vcl.ExtCtrls, QWorker;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label5: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF XE5UP}

uses Generics.Collections;
{$ENDIF}
{$R *.dfm}

type
  THashFunc = function(p: Pointer; l: Integer): Cardinal;

  THashTest = record
    Name: String;
    Func: THashFunc;
    UsedTime: Int64;
    MaxDeep: Integer;
    Conflicts: Integer;
    AvgDeep: Double;
  end;

function SuperHash(k: Pointer; l: Integer): Cardinal; inline;
var
  h: Cardinal;
  i: Integer;
  p: PByte;
begin
  h := 0;
  p := k;
  for i := 1 to l do
  begin
    // origial
    // h := h * 129 + ord(k[i]) + $9E370001;
    h := h + p^ * (1 shl (i * 3));
    Inc(p);
  end;
  Result := h;
end;

function D2007Hash(k: Pointer; l: Integer): Cardinal;
var
  i: Integer;
  p: PByte;
begin
  Result := 0;
  p := k;
  for i := 1 to l do
  begin
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor p^;
    Inc(p);
  end;
end;

function DXEHash(const k: Pointer; l: Integer): Cardinal;
var
  i: Integer;
  p: PByte;
begin
  Result := 0;
  p := k;
  for i := 1 to l do
  begin
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor p^;
    Inc(p);
  end;
end;

function XE6HashX(k: Pointer; l: Integer): Integer;
var
  LResult: Cardinal;
  i: Integer;
  p: PByte;
begin
  LResult := 0;
  p := k;
  for i := 1 to l do
  begin
    LResult := (LResult shl 5) or (LResult shr 27); // ROL Result, 5
    LResult := LResult xor Cardinal(p^);
    Inc(p);
  end;
  Result := LResult;
end;

function DoCompareInteger(v1, v2: Pointer): Integer;
begin
  Result := Integer(v1) - Integer(v2);
end;

function SDBMHash(buffer: PByte; size: uint32): uint32;
begin
  Result := 0;
  while size > 0 do
  begin
    // equivalent to: hash = 65599*hash + (*str++);
    // hash = (*str++) + (hash << 6) + (hash << 16) - hash;
    Result := buffer^ + (Result shl 6) + (Result shl 16) - Result;
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

function RSHash(buffer: PByte; size: uint32): uint32;
var
  a, b: uint32;
begin
  a := 63689;
  b := 378551;
  Result := 0;
  while size > 0 do
  begin
    // hash = hash * a + (*str++);
    Result := Result * a + buffer^;
    dec(size);
    Inc(buffer);
    a := a * b;
  end;
  Result := Result and $7FFFFFFF;
end;

function JSHash(buffer: PByte; size: uint32): uint32;
begin
  Result := 1315423911;
  while size > 0 do
  begin
    // hash ^= ((hash << 5) + (*str++) + (hash >> 2));
    Result := Result xor (Result shl 5) + buffer^ + (Result shr 2);
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

function PJWHash(buffer: PByte; size: uint32): uint32;
const
  BitsInUnignedInt = SizeOf(uint32) * 8;
  ThreeQuarters = (BitsInUnignedInt * 3) div 4;
  OneEighth = BitsInUnignedInt div 8;
  HighBits = ($FFFFFFFF) shl (BitsInUnignedInt - OneEighth);
var
  test: uint32;
begin
  Result := 0;
  while size > 0 do
  begin
    // hash = (hash << OneEighth) + (*str++);
    // if ((test = hash & HighBits) != 0)
    // hash = ((hash ^ (test >> ThreeQuarters)) & (~HighBits));
    Result := Result shl OneEighth + buffer^;
    test := Result and HighBits;
    if test <> 0 then
      Result := ((Result xor (test shr ThreeQuarters)) and (not HighBits));
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

function php_hash(buffer: PByte; size: uint32): uint32;
var
  g: uint32;
  buffer_end: PAnsiChar;
begin
  Result := 0;
  buffer_end := PAnsiChar(buffer) + size;
  while PAnsiChar(buffer) < buffer_end do
  begin
    Result := (Result shl 4) + buffer^;
    g := Result and $F0000000;
    if g <> 0 then
    begin
      Result := Result xor (g shr 24);
      Result := Result xor g;
    end;
    Inc(buffer);
  end;
end;

function ELFHash(buffer: PByte; size: uint32): uint32;
var
  x: uint32;
begin
  Result := 0;
  while size > 0 do
  begin
    // hash = (hash << 4) + (*str++);
    // if ((x = hash & 0xF0000000L) != 0)
    // {
    // hash ^= (x >> 24);
    // hash &= ~x;
    // }
    Result := (Result shl 4) + buffer^;
    x := Result and $F0000000;
    if x <> 0 then
    begin
      Result := Result xor (x shr 24);
      Result := Result and not x;
    end;
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

function BKDRHash(buffer: PByte; size: uint32): uint32;
const
  seed = 131; // 31 131 1313 13131 131313 etc..
begin
  Result := 0;
  while size > 0 do
  begin
    // hash = hash * seed + (*str++);
    Result := Result * seed + buffer^;
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

function DJBHash(buffer: PByte; size: uint32): uint32;
begin
  Result := 5381;
  while size > 0 do
  begin
    // hash += (hash << 5) + (*str++);
    Result := Result + (Result shl 5) + buffer^;
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

function APHash(buffer: PByte; size: uint32): uint32;
var
  i: int32;
begin
  Result := 5381;
  i := 0;
  while size > 0 do
  begin
    // if ((i & 1) == 0)
    // hash ^= ((hash << 7) ^ (*str++) ^ (hash >> 3));
    // else
    // hash ^= (~((hash << 11) ^ (*str++) ^ (hash >> 5)));
    if (i and 1) = 0 then
      Result := Result xor ((Result shl 7) xor buffer^ xor (Result shr 3))
    else
      Result := Result xor
        (not((Result shl 11) xor buffer^ xor (Result shr 5)));
    Inc(i);
    dec(size);
    Inc(buffer);
  end;
  Result := Result and $7FFFFFFF;
end;

type
  PUInt8 = ^uint8;
  PUInt32 = ^uint32;
  UInt8_p = ^uint8_array;
  uint8_array = array [0 .. MaxInt - 1] of uint8;

  // from sWZ_HashLib.pas
function Fnv32(lpMem: PByte; nSize: uint32): uint32;
const
  FNV_32_PRIME = $01000193;
begin
  Result := 0;
  // if ((lpMem = nil) or (nSize = 0)) then Exit;
  // if (Not sWZ_IsReadPtr(lpMem, nSize)) then Exit;

  // FNV-1 hash each octet in the buffer
  while (nSize > 0) do
  begin
    Result := Result * FNV_32_PRIME;
    Inc(Result, (Result shl 1) + (Result shl 4) + (Result shl 7) +
      (Result shl 8) + (Result shl 24));

    Result := Result xor PUInt8(lpMem)^;

    dec(nSize);
    Inc(PUInt8(lpMem));
  end;
end;

// from sWZ_HashLib.pas
function Fnv32a(lpMem: PByte; nSize: uint32): uint32;
const
  FNV_32_PRIME = $01000193;
begin
  Result := 0;
  // if ((lpMem = nil) or (nSize = 0)) then Exit;
  // if (Not sWZ_IsReadPtr(lpMem, nSize)) then Exit;

  // FNV-1 hash each octet in the buffer
  while (nSize > 0) do
  begin
    Result := Result xor PUInt8(lpMem)^;

    Result := Result * FNV_32_PRIME;
    Inc(Result, (Result shl 1) + (Result shl 4) + (Result shl 7) +
      (Result shl 8) + (Result shl 24));

    dec(nSize);
    Inc(PUInt8(lpMem));
  end;
end;

// from sWZ_HashLib.pas
function SuperFastHash(lpMem: PByte; nSize: uint32): uint32;
var
  nRem, nLen: uint32;
  nHash, nTemp: uint32;
begin
  Result := 0;
  // if ((lpMem = nil) or (nSize = 0)) then Exit;
  // if (Not sWZ_IsReadPtr(lpMem, nSize)) then Exit;

  nHash := nSize;
  nRem := nSize and 3;
  nLen := nSize shr 2;

  // Main loop
  while (nLen > 0) do
  begin
    nHash := nHash + ((UInt8_p(lpMem)[1] shl 8) + UInt8_p(lpMem)[0]);
    nTemp := (((UInt8_p(lpMem)[3] shl 8) + UInt8_p(lpMem)[2]) shl 11) xor nHash;
    nHash := (nHash shl 16) xor nTemp;
    Inc(PUInt8(lpMem), (2 * SizeOf(uint16)));
    nHash := nHash + (nHash shr 11);

    dec(nLen);
  end;

  // Handle end cases
  case (nRem) of
    3:
      begin
        nHash := nHash + ((UInt8_p(lpMem)[1] shl 8) + UInt8_p(lpMem)[0]);
        nHash := nHash xor (nHash shl 16);
        nHash := nHash xor (UInt8_p(lpMem)[SizeOf(uint16)] shl 18);
        nHash := nHash + (nHash shr 11);
      end;

    2:
      begin
        nHash := nHash + ((UInt8_p(lpMem)[1] shl 8) + UInt8_p(lpMem)[0]);
        nHash := nHash xor (nHash shl 11);
        nHash := nHash + (nHash shr 17);
      end;

    1:
      begin
        nHash := nHash + PUInt8(lpMem)^;
        nHash := nHash xor (nHash shl 10);
        nHash := nHash + (nHash shr 1);
      end;

    0:
      ;
  else
    Exit;
  end;

  // Force "avalanching" of final 127 bits
  nHash := nHash xor (nHash shl 3);
  nHash := nHash + (nHash shr 5);
  nHash := nHash xor (nHash shl 4);
  nHash := nHash + (nHash shr 17);
  nHash := nHash xor (nHash shl 25);
  nHash := nHash + (nHash shr 6);
  Result := nHash;
end;

// from sWZ_HashLib.pas
function MurmurHash3_32(lpMem: PByte; nSize: uint32): uint32;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
var
  nRem, nLen: uint32;
  nHash, nTemp: uint32;
  nSeed: uint32;
begin
  Result := 0;
  nSeed := 0;
  // if ((lpMem = nil) or (nSize = 0)) then Exit;
  // if (Not sWZ_IsReadPtr(lpMem, nSize)) then Exit;

  // body
  nHash := nSeed;
  nRem := nSize and 3;
  nLen := nSize shr 2;

  while (nLen > 0) do
  begin
    nTemp := PUInt32(lpMem)^;
    nTemp := nTemp * c1;
    // ROT32(nTemp, 15) = rot(循环移位)
    nTemp := (nTemp shl 15) or (nTemp shr (32 - 15));
    nTemp := nTemp * c2;

    nHash := nHash xor nTemp;
    // ROTL32(nHash, 13)
    nHash := (nHash shl 13) or (nHash shr (32 - 13));
    nHash := nHash * 5 + $E6546B64;

    Inc(PUInt32(lpMem));
    dec(nLen);
  end;

  // tail
  nTemp := 0;
  case (nRem) of
    3:
      begin
        nTemp := nTemp xor (UInt8_p(lpMem)[2] shl 16);
        nTemp := nTemp xor (UInt8_p(lpMem)[1] shl 8);
        nTemp := nTemp xor (UInt8_p(lpMem)[0]);
        nTemp := nTemp * c1;
        // ROTL32(nHash, 15)
        nTemp := (nTemp shl 15) or (nTemp shr (32 - 15));
        nTemp := nTemp * c2;
        nHash := nHash xor nTemp;
      end;

    2:
      begin
        nTemp := nTemp xor (UInt8_p(lpMem)[1] shl 8);
        nTemp := nTemp xor (UInt8_p(lpMem)[0]);
        nTemp := nTemp * c1;
        // ROTL32(nHash, 15)
        nTemp := (nTemp shl 15) or (nTemp shr (32 - 15));
        nTemp := nTemp * c2;
        nHash := nHash xor nTemp;
      end;

    1:
      begin
        nTemp := nTemp xor (UInt8_p(lpMem)[0]);
        nTemp := nTemp * c1;
        // ROTL32(nHash, 15)
        nTemp := (nTemp shl 15) or (nTemp shr (32 - 15));
        nTemp := nTemp * c2;
        nHash := nHash xor nTemp;
      end;
    0:
      ;
  else
    Exit;
  end;

  // finalization
  nHash := nHash xor nSize;

  // fmix32(nHash)
  nHash := nHash xor (nHash shr 16);
  nHash := nHash * $85EBCA6B;
  nHash := nHash xor (nHash shr 13);
  nHash := nHash * $C2B2AE35;
  nHash := nHash xor (nHash shr 16);
  Result := nHash;
end;

const
  PRIME32_1 = 2654435761;
  PRIME32_2 = 2246822519;
  PRIME32_3 = 3266489917;
  PRIME32_4 = 668265263;
  PRIME32_5 = 374761393;

function ROL(v: uint32; shift: int32): uint32;
asm
  MOV   CL,DL
  ROL   EAX,CL
end;

function XXH32_endian_align(buffer: PByte; size, seed: uint32): uint32;
var
  v1, v2, v3, v4: uint32;
  p: ^uint32;
  buffer_end, buffer_limit: PByte;
begin
  p := Pointer(buffer);
  buffer_end := Pointer(PAnsiChar(buffer) + size);
  if size >= 16 then
  begin
    buffer_limit := Pointer(PAnsiChar(buffer_end) - 16);
    v1 := seed + PRIME32_1 + PRIME32_2;
    v2 := seed + PRIME32_2;
    v3 := seed + 0;
    v4 := seed - PRIME32_1;
    repeat
      v1 := v1 + p^ * PRIME32_2;
      v1 := ROL(v1, 13) * PRIME32_1;
      Inc(p);

      v2 := v2 + p^ * PRIME32_2;
      v2 := ROL(v2, 13) * PRIME32_1;
      Inc(p);

      v3 := v3 + p^ * PRIME32_2;
      v3 := ROL(v3, 13) * PRIME32_1;
      Inc(p);

      v4 := v4 + p^ * PRIME32_2;
      v4 := ROL(v4, 13) * PRIME32_1;
      Inc(p);

      if PAnsiChar(p) >= PAnsiChar(buffer_limit) then
        break;
    until false;
    Result := ROL(v1, 1) + ROL(v2, 7) + ROL(v3, 12) + ROL(v4, 18);
  end
  else
  begin
    Result := seed + PRIME32_5;
  end;

  Result := Result + size;
  while (PAnsiChar(p) < (PAnsiChar(buffer_end) - 4)) do
  begin
    Result := Result + p^ * PRIME32_3;
    Result := ROL(Result, 17) * PRIME32_4;
    Inc(p);
  end;

  while (PAnsiChar(p) < PAnsiChar(buffer_end)) do
  begin
    Result := Result + PByte(p)^ * PRIME32_5;
    Result := ROL(Result, 11) * PRIME32_1;
    Inc(PByte(p));
  end;

  Result := Result xor (Result shr 15);
  Result := Result * PRIME32_2;
  Result := Result xor (Result shr 13);
  Result := Result * PRIME32_3;
  Result := Result xor (Result shr 16);
end;

// from xxHash.c
function XXH32(buffer: PByte; size: uint32): uint32;
begin
  Result := XXH32_endian_align(buffer, size, 0);
end;

// from IniFile.TStringHash.HashOf
function DSys_Hash(buffer: PByte; size: uint32): uint32;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to size do
  begin
    Result := ((Result shl 2) or (Result shr 30)) xor buffer^;
    Inc(buffer);
  end;
end;

const
  crc_table: array [0 .. $FF] of uint32 = ($00000000, $77073096, $EE0E612C,
    $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064,
    $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63,
    $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447,
    $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3,
    $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2,
    $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106,
    $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433, $7807C9A2,
    $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1,
    $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49,
    $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541,
    $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010,
    $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F, $5EDEF90E, $29D9C998,
    $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320,
    $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27,
    $7D079EB1, $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F,
    $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C, $36034AF6,
    $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C, $BC66831A,
    $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE,
    $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785,
    $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D,
    $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD,
    $F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354,
    $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC,
    $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9, $BDBDF21C,
    $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B,
    $2D02EF8D);

function crc32(buffer: PByte; size: uint32): uint32;
begin
  Result := $FFFFFFFF;
  while size > 0 do
  begin
    Result := (Result shr 8) xor crc_table[buffer^ xor (Result and $FF)];
    Inc(buffer);
    dec(size);
  end;
  Result := not Result;
end;

function ssl_hash(buffer: PByte; size: uint32): uint32;
var
  r: int32;
  n, v: uint32;
begin
  Result := 0;
  n := $100;
  while size > 0 do
  begin
    v := n or buffer^;
    n := n + $100;
    r := ((v shr 2) xor v) and $0F;
    Result := (Result shl r) or (Result shr (32 - r));
    Result := Result and $FFFFFFFF;
    Result := Result xor (v * v);
    Inc(buffer);
    dec(size);
  end;
  Result := (Result shr 16) xor Result;
end;

function mysql_hash(buffer: PByte; size: uint32): uint32;
begin
  Result := 0;
  while size > 0 do
  begin
    Result := (Result * 16777619) xor buffer^;
    Inc(buffer);
    dec(size);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S: QStringW;
  i, Hash2, c1, c2, J, D1, D2, A2: Integer;
  A1: Double;
  AFound: Boolean;
  T1: Int64;
  AList: TStringList;
  AHashArray: TList;
  ATestItems: array of THashTest;
  function RandomString(l: Integer): QStringW;
  var
    p: PWideChar;
  begin
    SetLength(Result, l);
    p := PWideChar(Result);
    while l > 0 do
    begin
      p^ := WideChar(1 + random(65535));
      Inc(p);
      dec(l);
    end;
  end;

  function ConflictCount(var AMaxLevel: Integer; var AvgLevel: Double): Integer;
  var
    ATimes: Integer;
    J, C: Integer;
  begin
    Result := 0;
    AHashArray.Sort(DoCompareInteger);
    i := 0;
    AMaxLevel := 0;
    ATimes := 0;
    C := AHashArray.Count - 1;
    repeat
      Hash2 := Integer(AHashArray[i]);
      Inc(i);
      J := i;
      AFound := false;
      while J <= C do
      begin
        if Integer(AHashArray[J]) = Hash2 then
        begin
          Inc(J);
          Inc(Result);
          AFound := True;
        end
        else
          break;
      end;
      if J <> i then
      begin
        Inc(ATimes);
        if J - i > AMaxLevel then
          AMaxLevel := J - i;
        i := J;
      end;
    until i >= C;
    AvgLevel := Result / ATimes;
  end;

// 使用C盘上所有的文件或文件夹名称做为测试数据
  procedure GenData(APath: String);
  var
    sr: TSearchRec;
  begin
    if FindFirst(APath + '*.*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory) <> 0 then
        begin
          if (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            AList.Add(sr.Name);
            GenData(APath + sr.Name + '\');
          end;
        end
        else
          AList.Add(sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;

begin
  AList := TStringList.Create;
  AHashArray := TList.Create;
  AList.Capacity := 1000000;
  AList.Duplicates := dupIgnore;
  randomize;
  // 生成随机字符串
  AList.BeginUpdate;
  GenData('c:\');
  // for i := 0 to AMaxItemCount do
  // AList.Add(RandomString(10 + random(40)));
  AList.EndUpdate;
  Memo1.Lines.Add('共找到 ' + IntToStr(AList.Count) + ' 个非重复字符串。');
  SetLength(ATestItems, 23);
  ATestItems[0].Name := 'CRC32';
  ATestItems[0].Func := THashFunc(@crc32);
  ATestItems[1].Name := 'SDBMHash';
  ATestItems[1].Func := THashFunc(@SDBMHash);
  ATestItems[2].Name := 'RSHash';
  ATestItems[2].Func := THashFunc(@RSHash);
  ATestItems[3].Name := 'JSHash';
  ATestItems[3].Func := THashFunc(@JSHash);
  ATestItems[4].Name := 'PJWHash';
  ATestItems[4].Func := THashFunc(@PJWHash);
  ATestItems[5].Name := 'ELFHash';
  ATestItems[5].Func := THashFunc(@ELFHash);
  ATestItems[6].Name := 'BKDRHash';
  ATestItems[6].Func := THashFunc(@BKDRHash);
  ATestItems[7].Name := 'DJBHash';
  ATestItems[7].Func := THashFunc(@DJBHash);
  ATestItems[8].Name := 'APHash';
  ATestItems[8].Func := THashFunc(@APHash);
  ATestItems[9].Name := 'Fnv32';
  ATestItems[9].Func := THashFunc(@Fnv32);
  ATestItems[10].Name := 'Fnv32a';
  ATestItems[10].Func := THashFunc(@Fnv32a);
  ATestItems[11].Name := 'SuperFastHash';
  ATestItems[11].Func := THashFunc(@SuperFastHash);
  ATestItems[12].Name := 'MurmurHash3_32';
  ATestItems[12].Func := THashFunc(@MurmurHash3_32);
  ATestItems[13].Name := 'XXHash32';
  ATestItems[13].Func := THashFunc(@XXH32);
  ATestItems[14].Name := 'D7.Hash';
  ATestItems[14].Func := THashFunc(@DSys_Hash);
  ATestItems[15].Name := 'php_hash';
  ATestItems[15].Func := THashFunc(@php_hash);
  ATestItems[16].Name := 'ssl_hash';
  ATestItems[16].Func := THashFunc(@ssl_hash);
  ATestItems[17].Name := 'mysql_hash';
  ATestItems[17].Func := THashFunc(@mysql_hash);
  ATestItems[18].Name := 'qstring';
  ATestItems[18].Func := THashFunc(@HashOf);
  ATestItems[19].Name := 'D2007Hash';
  ATestItems[19].Func := THashFunc(@D2007Hash);
  ATestItems[20].Name := 'DXEHash';
  ATestItems[20].Func := THashFunc(@DXEHash);
  ATestItems[21].Name := 'SuperHash';
  ATestItems[21].Func := THashFunc(@SuperHash);
  ATestItems[22].Name := 'XE6HashX';
  ATestItems[22].Func := THashFunc(@XE6HashX);
  AHashArray.Capacity := AList.Count;
  for J := 0 to High(ATestItems) do
  begin
    ATestItems[J].UsedTime := GetTimeStamp;
    for i := 0 to AList.Count - 1 do
    begin
      S := AList[i];
      AHashArray.Add(Pointer(ATestItems[J].Func(PQCharW(S), Length(S) shl 1)));
    end;
    ATestItems[J].UsedTime := GetTimeStamp - ATestItems[J].UsedTime;
    ATestItems[J].MaxDeep := 0;
    ATestItems[J].AvgDeep := 0;
    ATestItems[J].Conflicts := ConflictCount(ATestItems[J].MaxDeep,
      ATestItems[J].AvgDeep);
    Memo1.Lines.Add(ATestItems[J].Name + ': ' +
      FloatToStr(ATestItems[J].UsedTime / 10) + 'ms'#13#10'  冲突:总计 ' +
      IntToStr(ATestItems[J].Conflicts) + ',最大深度 ' +
      IntToStr(ATestItems[J].MaxDeep) + ',平均深度 ' + FormatFloat('#.00',
      ATestItems[J].AvgDeep));
    Application.ProcessMessages;
  end;
  AList.Free;
  AHashArray.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S:String;
begin
S:='He';
Memo1.Lines.Add('QString:'+IntToStr(HashOf(PWideChar(S),Length(S) shl 1)));
Memo1.Lines.Add('BKDR:'+IntToStr(BKDRHash(Pointer(S),Length(S) shl 1)));
end;

end.
