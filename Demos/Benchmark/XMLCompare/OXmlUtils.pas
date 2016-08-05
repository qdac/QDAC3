unit OXmlUtils;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlUtils.pas

  Collection of types and methods for XML.
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

  OWideSupp, OEncoding;

type
  TXMLNodeType = (ntDocument, ntDocType, ntXMLDeclaration, ntElement,
    ntAttribute, ntText, ntEntityReference, ntCData, ntComment,
    ntProcessingInstruction);

  EXMLDOMException = class(Exception);

  TXMLIndentType = (itNone, itFlat, itIndent);
  TXMLWhiteSpaceHandling = (wsTrim, wsPreserveAll, wsPreserveInTextOnly, wsAutoTag);
  //brNone...read through all nodes
  //brAfterDocumentElement...stop after first root node
  //brAfterDocumentElementReleaseDocument...brAfterDocumentElement + release document
  TXMLBreakReading = (brNone, brAfterDocumentElement);
  TXMLLineBreak = (lbLF, lbCR, lbCRLF, lbDoNotProcess);
  TXMLChildType = (ctChild, ctAttribute);
  //wsInherit: inherit from parent element
  //wsPreserve: preserve white space
  //wsDefault: default handlign (do not preserve)
  TXMLPreserveWhiteSpace = (pwInherit, pwPreserve, pwDefault);
  TXMLCharKind =
    (ckNewLine10, ckNewLine13, ckSingleQuote, ckDoubleQuote, ckAmpersand,
     ckLowerThan, ckGreaterThan, ckSquareBracketOpen, ckSquareBracketClose,
     ckCharacter, ckInvalid);

  {$IFDEF O_GENERICARRAY}
  TXMLIntArray = TArray<Integer>;
  {$ELSE}
  TXMLIntArray = array of Integer;
  {$ENDIF}

const
  {$IFDEF MSWINDOWS}
  XMLDefaultLineBreak = lbCRLF;
  {$ELSE}
  XMLDefaultLineBreak = lbLF;
  {$ENDIF}
  XMLLineBreak: Array[TXMLLineBreak] of OWideString = (#10, #13, #13#10, sLineBreak);
  XML_XML: OWideString = 'xml';
  XML_XMLNS: OWideString = 'xmlns';
  XML_XML_SPACE: OWideString = 'xml:space';

  XMLUseIndexForAttributesLimit = 256;

  // W3C DOM Level 1 :: http://www.w3.org/TR/REC-DOM-Level-1/level-one-core.html
  // index or size is negative, or greater than the allowed value
  INDEX_SIZE_ERR = 1;
  // the specified range of text does not fit into a DOMString
  DOMSTRING_SIZE_ERR = 2;
  // any node is inserted somewhere it doesn't belong
  HIERARCHY_REQUEST_ERR = 3;
  // a node is used in a different document than the one that created it (that doesn't support it)
  WRONG_DOCUMENT_ERR = 4;
  // an invalid character is specified, such as in a name
  INVALID_CHARACTER_ERR = 5;
  // data is specified for a node which does not support data
  NO_DATA_ALLOWED_ERR = 6;
  // an attempt is made to modify an object where modifications are not allowed
  NO_MODIFICATION_ALLOWED_ERR = 7;
  // an attempt was made to reference a node in a context where it does not exist
  NOT_FOUND_ERR = 8;
  // the implementation does not support the type of object requested
  NOT_SUPPORTED_ERR = 9;
  // an attempt is made to add an attribute that is already in use elsewhere
  INUSE_ATTRIBUTE_ERR = 10;
type

  //virtual MS above some custom buffer (may be string, array of byte etc.)
  //  MUST BE TCustomMemoryStream -> SO THAT THE MEMORY POINTER WOULD NOT GET DESTROYED IN .Destroy!!!
  TVirtualMemoryStream = class(TCustomMemoryStream)
  public
    procedure SetPointer(aPtr: Pointer; const aSize: Longint); reintroduce;//public
    function Write(const {%H-}Buffer; {%H-}Count: Longint): Longint; override;
  public
    procedure SetString(const aString: OWideString);
    {$IFDEF O_RAWBYTESTRING}
    procedure SetString_UTF8(const aString: ORawByteString);
    {$ENDIF}
    procedure SetBuffer(const aBuffer: TBytes);
  end;

function OXmlIsNameStartChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsNameChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsWhiteSpaceChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsDecimalChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsHexadecimalChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsSignChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsBreakChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsChar(const aChar: OWideChar): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsChar(const aChar: Integer): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlCharKind(const aChar: OWideChar): TXMLCharKind; overload; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlNeedsPreserveAttribute(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsWhiteSpace(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsNumber(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlValidEntityReference(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidCData(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidComment(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidPIContent(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlValidName(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidChars(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlPreserveToStr(const aPreserveWhiteSpace: TXMLPreserveWhiteSpace): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlStrToPreserve(const aStr: OWideString): TXMLPreserveWhiteSpace; {$IFDEF O_INLINE}inline;{$ENDIF}

procedure OXmlResolveNameSpace(const aNodeName: OWideString; var outNameSpacePrefix, outLocalName: OWideString);
function OXmlCheckNameSpace(const aNodeName, aNameSpacePrefix: OWideString; var outLocalName: OWideString): Boolean;
function OXmlApplyNameSpace(const aNameSpacePrefix, aLocalName: OWideString): OWideString;

implementation

uses
  OXmlLng;

procedure OXmlResolveNameSpace(const aNodeName: OWideString; var outNameSpacePrefix, outLocalName: OWideString);
var
  xPos: Integer;
begin
  xPos := Pos(':', aNodeName);
  if xPos >= 0 then
  begin
    outNameSpacePrefix := Copy(aNodeName, 1, xPos-1);
    outLocalName := Copy(aNodeName, xPos+1, High(Integer));
  end else
  begin
    outNameSpacePrefix := '';
    outLocalName := aNodeName;
  end;
end;

function OXmlCheckNameSpace(const aNodeName, aNameSpacePrefix: OWideString; var outLocalName: OWideString): Boolean;
begin
  Result := (Length(aNodeName) >= Length(aNameSpacePrefix)+1);
  if not Result then
  begin
    outLocalName := '';
    Exit;
  end;

  if Length(aNameSpacePrefix) > 0 then
  begin
    Result := CompareMem(POWideChar(aNodeName), POWideChar(aNameSpacePrefix), Length(aNameSpacePrefix)*SizeOf(OWideChar));
    if not Result then
    begin
      outLocalName := '';
      Exit;
    end;
  end;

  Result := (aNodeName[Length(aNameSpacePrefix)+1] = ':');
  if Result then
    outLocalName := Copy(aNodeName, Length(aNameSpacePrefix)+2, High(Integer));
end;

function OXmlApplyNameSpace(const aNameSpacePrefix, aLocalName: OWideString): OWideString;
begin
  if (aNameSpacePrefix <> '') and (aLocalName <> '') then
    Result := aNameSpacePrefix+':'+aLocalName
  else if aNameSpacePrefix <> '' then
    Result := aNameSpacePrefix
  else
    Result := aLocalName;
end;

function OXmlStrToPreserve(const aStr: OWideString): TXMLPreserveWhiteSpace;
begin
  if (aStr <> '') and ((aStr[1] = 'p') or (aStr[1] = 'P')) then//preserve = true
    Result := pwPreserve
  else
    Result := pwDefault;
end;

function OXmlPreserveToStr(const aPreserveWhiteSpace: TXMLPreserveWhiteSpace): OWideString;
begin
  if aPreserveWhiteSpace = pwPreserve then
    Result := 'preserve'
  else
    Result := 'default';
end;

function OXmlValidName(const aText: OWideString): Boolean;
var I: Integer;
begin
  if aText = '' then begin
    Result := False;
    Exit;
  end;

  Result := OXmlIsNameStartChar(aText[1]);
  if not Result then
    Exit;

  for I := 2 to Length(aText) do begin
    Result := OXmlIsNameChar(aText[I]);
    if not Result then
      Exit;
  end;
end;

function OXmlValidChars(const aText: OWideString): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 1 to Length(aText) do begin
    Result := OXmlIsChar(aText[I]);
    if not Result then
      Exit;
  end;
end;

function OXmlNeedsPreserveAttribute(const aText: OWideString): Boolean;
var
  I, xLength: Integer;
  xThisCharWhiteSpace, xLastCharWhiteSpace: Boolean;
begin
  if aText = '' then begin
    Result := False;
    Exit;
  end;

  xLength := Length(aText);

  Result := OXmlIsWhiteSpaceChar(aText[1]) or OXmlIsWhiteSpaceChar(aText[xLength]);
  if Result then
    Exit;

  xLastCharWhiteSpace := False;
  I := 2;//we can omit first and last characters (already checked)!
  while I < xLength do begin//we can omit first and last characters (already checked)!
    if (aText[I] = #13) and (aText[I+1] = #10) then
      Inc(I);//step over #13#10
    xThisCharWhiteSpace := OXmlIsWhiteSpaceChar(aText[I]);
    if xThisCharWhiteSpace and xLastCharWhiteSpace then begin
      Result := True;
      Exit;
    end;
    xLastCharWhiteSpace := xThisCharWhiteSpace;
    Inc(I);
  end;
end;

function OXmlIsWhiteSpace(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aText) do
  if not OXmlIsWhiteSpaceChar(aText[I]) then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function OXmlIsNumber(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aText) do
  if not (
    OXmlIsDecimalChar(aText[I]) or//'0'..'1'
    ((I = 1) and OXmlIsSignChar(aText[I])))//sign
  then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function OXmlValidEntityReference(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  case Length(aText) of
    0: Result := False;
    1: Result := OXmlIsNameStartChar(aText[1]);
  else
    if aText[1] = '#' then
    begin
      if aText[2] = 'x' then
      begin
        //HEXADECIMAL
        for I := 3 to Length(aText)-1 do
        if not OXmlIsHexadecimalChar(aText[I]) then
        begin
          Result := False;
          Exit;
        end;
      end else
      begin
        //DECIMAL
        for I := 2 to Length(aText)-1 do
        if not OXmlIsDecimalChar(aText[I]) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end else
    begin
      //TEXT
      Result := OXmlIsNameStartChar(aText[1]);
      if not Result then Exit;

      for I := 2 to Length(aText)-1 do
      if not OXmlIsNameChar(aText[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
  end;
end;

function OXmlValidCData(const aText: OWideString): Boolean;
begin
  Result := (Pos(']]>', aText) = 0);
end;

function OXmlValidComment(const aText: OWideString): Boolean;
var
  xL: Integer;
begin
  xL := Length(aText);
  Result := (xL = 0) or ((Pos('--', aText) = 0) and (aText[xL] <> '-'));
end;

function OXmlValidPIContent(const aText: OWideString): Boolean;
var
  xL: Integer;
begin
  xL := Length(aText);
  Result := (xL = 0) or (Pos('?>', aText) = 0);
end;

function OXmlIsDecimalChar(const aChar: OWideChar): Boolean;
begin
  case aChar of
    '0'..'9': Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsHexadecimalChar(const aChar: OWideChar): Boolean;
begin
  case aChar of
    'a'..'f',
    'A'..'F',
    '0'..'9': Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsSignChar(const aChar: OWideChar): Boolean;
begin
  case aChar of
    '-', '+': Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsBreakChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $00..$20,//0..space
    Ord('"'),
    Ord(''''),
    Ord('/'),
    Ord('?'),
    Ord('<'),
    Ord('='),
    Ord('>'): Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsChar(const aChar: OWideChar): Boolean;
begin
  Result := OXmlIsChar(Ord(aChar));
end;

function OXmlIsChar(const aChar: Integer): Boolean;
begin
  case aChar of
    Ord('<'), Ord('>'): Result := True;
    09, 10, 13,
    $20..$3B, $3D, $3F..$FF//except <>
    {$IFNDEF FPC}
    ,
    $0100..$FFFD
    {$ENDIF}
    : Result := True;
  else
    Result := False;
  end;
end;

function OXmlCharKind(const aChar: OWideChar): TXMLCharKind;
begin
  case Ord(aChar) of
    Ord('"'): Result := ckDoubleQuote;//#$22
    Ord('&'): Result := ckAmpersand;//#$26
    Ord(''''): Result := ckSingleQuote;//#$27
    Ord('<'): Result := ckLowerThan;//#$3C
    Ord('>'): Result := ckGreaterThan;//#$3E
    Ord('['): Result := ckSquareBracketOpen;
    Ord(']'): Result := ckSquareBracketClose;
    10: Result := ckNewLine10;
    13: Result := ckNewLine13;
    09,
    $20, $21, $23..$25, $28..$3B, $3D, $3F..$5A, $5C, $5E..$FF//except '"&<>[]
    {$IFNDEF FPC}
    ,
    $0100..$FFFD
    {$ENDIF}
    : Result := ckCharacter;
  else
    Result := ckInvalid;
  end;
end;

function OXmlIsNameStartChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of//MUST BE Ord(aChar) because some Delphi show "E2030 Duplicate case label" error - > the performance is the same
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord(':'),
    Ord('_'),
    $C0..$D6,
    $D8..$F6,
    $F8..$FF
    {$IFNDEF FPC}
    ,
    $100..$2FF,
    $370..$37D,
    $37F..$1FFF,
    $200C..$200D,
    $2070..$218F,
    $2C00..$2FEF,
    $3001..$D7FF,
    $F900..$FDCF,
    $FDF0..$FFFD
    {$ENDIF}
    : Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsWhiteSpaceChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $09, $0A, $0D, $20: Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsNameChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of//MUST BE Ord(aChar) because some Delphi show "E2030 Duplicate case label" error - > the performance is the same
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord('0')..Ord('9'),
    Ord(':'),
    Ord('_'),
    Ord('-'),
    Ord('.'),
    $B7,
    $C0..$D6,
    $D8..$F6,
    $F8..$FF
    {$IFNDEF FPC}
    ,
    $100..$2FF,
    $370..$37D,
    $37F..$1FFF,
    $200C..$200D,
    $2070..$218F,
    $2C00..$2FEF,
    $3001..$D7FF,
    $F900..$FDCF,
    $FDF0..$FFFD,
    $0300..$036F,
    $203F..$2040
    {$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;

{ TVirtualMemoryStream }

procedure TVirtualMemoryStream.SetBuffer(const aBuffer: TBytes);
var
  xLength: Integer;
begin
  xLength := Length(aBuffer);
  if xLength > 0 then
    SetPointer(@aBuffer[0], xLength)
  else
    SetPointer(nil, 0);
end;

procedure TVirtualMemoryStream.SetPointer(aPtr: Pointer; const aSize: Integer);
begin
  inherited SetPointer(aPtr, aSize);
end;

procedure TVirtualMemoryStream.SetString(const aString: OWideString);
var
  xLength: Integer;
begin
  xLength := Length(aString);
  if xLength > 0 then
    SetPointer(@aString[1], xLength * SizeOf(OWideChar))
  else
    SetPointer(nil, 0);
end;

{$IFDEF O_RAWBYTESTRING}
procedure TVirtualMemoryStream.SetString_UTF8(const aString: ORawByteString);
var
  xLength: Integer;
begin
  xLength := Length(aString);
  if xLength > 0 then
    SetPointer(@aString[1], xLength)
  else
    SetPointer(nil, 0);
end;
{$ENDIF}

function TVirtualMemoryStream.{%H-}Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create(OXmlLng_CannotWriteToVirtualMemoryStream);
end;

end.