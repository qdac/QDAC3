{-------------------------------------------------------------------------------
 
 Copyright (c) 1999-2014 Ralf Junker, Yunqa
 Internet: http://www.yunqa.de
 E-Mail:   delphi@yunqa.de

-------------------------------------------------------------------------------}

unit DIXmlUtils;

{$I DICompilers.inc}

interface

uses
  DISystemCompat,
  {$IFDEF HAS_UNITSCOPE}System.Classes{$ELSE}Classes{$ENDIF},
  DIXml;

function xmlOutputBufferCreateFileStream(
  const FileName: string;
  const Enc: xmlCharEncodingHandlerPtr = nil): xmlOutputBufferPtr;

function xmlOutputBufferCreateRawByteString(
  const s: PRawByteString;
  const Enc: xmlCharEncodingHandlerPtr = nil): xmlOutputBufferPtr;

function xmlOutputBufferCreateStream(
  const Stream: TStream;
  const Enc: xmlCharEncodingHandlerPtr = nil): xmlOutputBufferPtr;

function xmlOutputBufferCreateUnicodeString(
  const s: PUnicodeString): xmlOutputBufferPtr;

function xmlOutputBufferCreateUtf8String(
  const s: PUtf8String): xmlOutputBufferPtr;

function xmlReadFileStream(
  const FileName: string;
  const URL: C_char_ptr;
  const Encoding: C_char_ptr;
  const Options: C_int): xmlDocPtr;

function xmlReadStream(
  const Stream: TStream;
  const URL: C_char_ptr;
  const Encoding: C_char_ptr;
  const Options: C_int): xmlDocPtr;

implementation

uses
  {$IFDEF HAS_UNITSCOPE}System.SysUtils{$ELSE}SysUtils{$ENDIF};

function xmlStreamClose(Context: C_void_ptr): C_int;
var
  s: TStream;
begin
  s := Context;
  try
    s.Free;
    Result := 0;
  except
    Result := -1;
  end;
end;

function xmlStreamWrite(Context: C_void_ptr; Buffer: C_char_ptr; Len: C_int): C_int;
var
  s: TStream;
begin
  s := Context;
  try

    s.Write(Buffer^, Len);

    Result := Len;
  except
    Result := -1;
  end;
end;

function xmlOutputBufferCreateFileStream(const FileName: string; const Enc: xmlCharEncodingHandlerPtr = nil): xmlOutputBufferPtr;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  Result := xmlOutputBufferCreateIO(
    xmlStreamWrite,
    xmlStreamClose,
    Stream,
    Enc);
end;

function xmlRawByteStringWrite(Context: C_void_ptr; Buffer: C_char_ptr; Len: C_int): C_int;
var
  l: NativeInt;
  pString: PRawByteString;
begin
  pString := Context;
  l := Length(pString^);

  SetLength(pString^, l + Len);

  Move(Buffer^, pString^[l + 1], Len);

  Result := Len;
end;

function xmlOutputBufferCreateRawByteString(const s: PRawByteString; const Enc: xmlCharEncodingHandlerPtr = nil): xmlOutputBufferPtr;
begin
  Result := xmlOutputBufferCreateIO(xmlRawByteStringWrite, nil, s, Enc);
end;

function xmlOutputBufferCreateStream(const Stream: TStream; const Enc: xmlCharEncodingHandlerPtr = nil): xmlOutputBufferPtr;
begin
  Result := xmlOutputBufferCreateIO(
    xmlStreamWrite,
    nil,
    Stream,
    Enc);
end;

function xmlUnicodeStringWrite(Context: C_void_ptr; Buffer: C_char_ptr; Len: C_int): C_int;
var
  l: NativeInt;
  pString: PUnicodeString;
begin
  pString := Context;
  l := Length(pString^);

  SetLength(pString^, l + Len div 2);

  Move(Buffer^, pString^[l + 1], Len);

  Result := Len;
end;

function xmlOutputBufferCreateUnicodeString(const s: PUnicodeString): xmlOutputBufferPtr;
begin
  Result := xmlOutputBufferCreateIO(
    xmlUnicodeStringWrite,
    nil,
    s,
    xmlGetCharEncodingHandler(XML_CHAR_ENCODING_UTF16LE));
end;

function xmlUtf8StringWrite(Context: C_void_ptr; Buffer: C_char_ptr; Len: C_int): C_int;
var
  l: NativeInt;
  pString: PUtf8String;
begin
  pString := Context;
  l := Length(pString^);

  SetLength(pString^, l + Len);

  Move(Buffer^, pString^[l + 1], Len);

  Result := Len;
end;

function xmlOutputBufferCreateUtf8String(const s: PUtf8String): xmlOutputBufferPtr;
begin
  Result := xmlOutputBufferCreateIO(
    xmlUtf8StringWrite,
    nil,
    s,
    nil);
end;

function xmlInputCloseStream(Context: C_void_ptr): C_int;
var
  Stream: TStream;
begin
  Stream := Context;
  try
    Stream.Free;
    Result := 0;
  except
    Result := -1;
  end;
end;

function xmlInputReadStream(Context: C_void_ptr; Buffer: C_char_ptr; Len: C_int): C_int;
var
  Stream: TStream;
begin
  Stream := Context;
  try
    Result := Stream.Read(Buffer^, Len);
  except
    Result := -1;
  end;
end;

function xmlReadStream(const Stream: TStream; const URL: C_char_ptr; const Encoding: C_char_ptr; const Options: C_int): xmlDocPtr;
begin
  try
    Result := xmlReadIO(xmlInputReadStream, nil, Stream, URL, Encoding, Options);
  except
    Result := nil;
  end;
end;

function xmlReadFileStream(const FileName: string; const URL: C_char_ptr; const Encoding: C_char_ptr; const Options: C_int): xmlDocPtr;
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := xmlReadIO(xmlInputReadStream, xmlInputCloseStream, Stream, URL, Encoding, Options);
  except
    Result := nil;
  end;
end;

end.

