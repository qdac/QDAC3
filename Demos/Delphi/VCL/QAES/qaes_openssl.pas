unit qaes_openssl;

{ 下面的代码来自OpenSSL }
interface

uses classes;

const
  //aes.h
  AES_MODE_ENCRYPT = 1;//原来的AES_ENCRYPT
  AES_MODE_DECRYPT = 0;//原来的AES_DECRYPT
  AES_MAXNR=14;
  AES_BLOCK_SIZE=16;
  //end aes.h
  //aes_locl.h
  MAXKC=8;
  MAXKB=32;
  MAXNR=14;
  //end aes_locl.h
type
  //aes.h
  FIPS_AES_SIZE_T=Integer;
  //aes_key_st,aes.h.80
  AES_KEY=record
    rd_key:array[0..4*(AES_MAXNR+1)-1] of Integer;
    rounds:Integer;
  end;
  PAES_KEY=^AES_KEY;
  AESBlock=array [0..AES_BLOCK_SIZE-1] of Byte;
  PAESBlock=^AESBlock;
  function AES_options:PAnsiChar;
  function AES_set_encrypt_key(const userKey:PByte;const bits:Integer;key:PAES_KEY):Integer;
  //int AES_set_decrypt_key(const unsigned char *userKey, const int bits,AES_KEY *key);
  function AES_set_decrypt_key(const userKey:PByte;const bits:Integer;key:PAES_KEY):Integer;
  //void AES_encrypt(const unsigned char *in, unsigned char *out,const AES_KEY *key);
  procedure AES_encrypt(const AIn,AOut:PByte;const key:PAES_KEY);
  //void AES_decrypt(const unsigned char *in, unsigned char *out,const AES_KEY *key);
  procedure AES_decrypt(const AIn,Aout:PByte;const key:PAES_KEY);
  //void AES_ecb_encrypt(const unsigned char *in, unsigned char *out,const AES_KEY *key, const int enc);
  procedure AES_ecb_encrypt(const AIn, AOut:PByte;const key:PAES_KEY;const enc:Integer);
  //void AES_cbc_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char *ivec, const int enc);
  procedure AES_cbc_encrypt(AIn, AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec:PByte; const enc:Integer);
  //void AES_cfb128_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char *ivec, int *num, const int enc);
  procedure AES_cfb128_encrypt(const AIn,AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec:PByte;num:PInteger;const enc:Integer);
  //void AES_cfb1_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char *ivec, int *num, const int enc);
  procedure AES_cfb1_encrypt(const AIn, AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec:PByte;num:PInteger;const enc:Integer);
  //void AES_cfb8_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char *ivec, int *num, const int enc);
  procedure AES_cfb8_encrypt(const AIn,AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivect:PByte;num:PInteger;const enc:Integer);
  //void AES_cfbr_encrypt_block(const unsigned char *in,unsigned char *out,const int nbits,const AES_KEY *key,unsigned char *ivec,const int enc);
  procedure AES_cfbr_encrypt_block(const AIn,AOut:PByte;const nbits:Integer;const key:PAES_KEY;ivec:PByte;const enc:Integer);
  //void AES_ofb128_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char *ivec, int *num);
  procedure AES_ofb128_encrypt(const AIn,AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec:PByte;num:PInteger);
  //void AES_ctr128_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char ivec[AES_BLOCK_SIZE],unsigned char ecount_buf[AES_BLOCK_SIZE],	unsigned int *num);
  procedure AES_ctr128_encrypt(const AIn, AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec,ecount_buf:PAESBlock;num:PInteger);
  //void AES_ige_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,unsigned char *ivec, const int enc);
  procedure AES_ige_encrypt(const AIn,AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec:PByte;const enc:Integer);
  //void AES_bi_ige_encrypt(const unsigned char *in, unsigned char *out,const unsigned long length, const AES_KEY *key,const AES_KEY *key2, const unsigned char *ivec,const int enc);
  procedure AES_bi_ige_encrypt(const AIn, AOut:PByte;const length:Cardinal;const key,key2:PAES_KEY;const ivec:PByte;const enc:Integer);
  //int AES_wrap_key(AES_KEY *key, const unsigned char *iv,unsigned char *out,const unsigned char *in, unsigned int inlen);
  function AES_wrap_key(key:PAES_KEY;const iv,AOut,AIn:PByte;inlen:Cardinal):Integer;
  //int AES_unwrap_key(AES_KEY *key, const unsigned char *iv,unsigned char *out,const unsigned char *in, unsigned int inlen);
  function AES_unwrap_key(key:PAES_KEY;const iv,AOut,AIn:PByte;inlen:Cardinal):Integer;
  //aes.h 结束
implementation
uses qstring;
type
  U32=Cardinal;
  U8=Byte;
  T4Bytes=array[0..3] of Byte;
  P4Bytes=^T4Bytes;
//aes_locl.h
function GETU32(p:PByte):Cardinal;inline;
var
  pt:P4Bytes absolute p;
begin
Result:=(u32(pt[0]) shl 24) xor
  (u32(pt[1]) shl 16) xor
  (u32(pt[2]) shl 8) xor
  u32(pt[3]);
end;
function PUTU32(ct:PByte;st:Cardinal):Cardinal;inline;
var
  pt:P4Bytes absolute ct;
begin
pt[0]:=U8(st shr 24);
pt[1]:=U8(st shr 16);
pt[2]:=U8(st shr 8);
pt[3]:=U8(st);
end;
//end aes_locl.h
//aes_cbc.c
procedure AES_cbc_encrypt(AIn, AOut:PByte;const length:Cardinal;const key:PAES_KEY;ivec:PByte; const enc:Integer);
var
  n,len:Cardinal;
  tmp:AESBlock;
  iv:PAESBlock;
begin
len := length;
iv := PAESBlock(ivec);
Assert(Assigned(AIn) and Assigned(AOut) and Assigned(key) and Assigned(ivec));
Assert(enc in [AES_MODE_ENCRYPT,AES_MODE_DECRYPT]);
if (AES_MODE_ENCRYPT = enc) then
  begin
  while (len >= AES_BLOCK_SIZE) do
    begin
    for n:=0 to AES_BLOCK_SIZE-1 do
      AOut[n] := AIn[n] xor iv[n];
    AES_encrypt(AOut, AOut, key);
    iv := PAESBlock(AOut);
    Dec(len,AES_BLOCK_SIZE);
    Inc(AIn,AES_BLOCK_SIZE);
    Dec(AOut,AES_BLOCK_SIZE);
		end;
  if len>0 then
    begin
    for n:=0 to len-1 do
      AOut[n] := AIn[n] xor iv[n];
    for n:=len to AES_BLOCK_SIZE-1 do
      Aout[n] := iv[n];
    AES_encrypt(AOut, AOut, key);
    iv := PAESBlock(AOut);
		end;
  Move(iv^,ivec^,AES_BLOCK_SIZE);
  end
else if (AIn <> AOut) then
  begin
  while (len >= AES_BLOCK_SIZE) do
    begin
    AES_decrypt(AIn, AOut, key);
    for n:=0 to AES_BLOCK_SIZE-1 do
      AOut[n]:=AOut[n] xor iv[n];
    iv := PAESBlock(AIn);
    Dec(len,AES_BLOCK_SIZE);
    Inc(AIn,AES_BLOCK_SIZE);
    Inc(AOut,AES_BLOCK_SIZE);
		end;
  if len<>0 then
    begin
    AES_decrypt(AIn,@tmp,key);
    for n:=0 to len-1 do
      AOut[n] := tmp[n] xor iv[n];
    iv := PAESBlock(AIn);
		end;
  Move(iv^,ivec^,AES_BLOCK_SIZE);
	end
else
  begin
  while (len >= AES_BLOCK_SIZE) do
    begin
    Move(AIn^,tmp,AES_BLOCK_SIZE);
    AES_decrypt(AIn, AOut, key);
    for n:=0 to AES_BLOCK_SIZE-1 do
      AOut[n]:=AOut[n] xor ivec[n];
    Move(tmp,ivec,AES_BLOCK_SIZE);
    Dec(len,AES_BLOCK_SIZE);
    Inc(AIn,AES_BLOCK_SIZE);
    Inc(AOut,AES_BLOCK_SIZE);
		end;
  if len<>0 then
    begin
    Move(AIn^,tmp, AES_BLOCK_SIZE);
    AES_decrypt(@tmp, aout, key);
    for n:=0 to len-1 do
      AOut[n]:=AOut[n] xor ivec[n];
    for n:=len to AES_BLOCK_SIZE-1 do
      AOut[n] := tmp[n];
    Move(tmp,ivec^, AES_BLOCK_SIZE);
		end;
	end;
end;
//end aes_cbc.c

end.
