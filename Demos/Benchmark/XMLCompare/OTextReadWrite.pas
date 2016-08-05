unit OTextReadWrite;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OTextReadWrite.pas

  TOTextReader -> read text from streams with buffer.
    - very fast thanks to internal string and stream buffer
    - read from streams with every supported encoding
    - when reading char-by-char an internal buffer can be used for
      saving last read keyword etc.


  TOTextWriter -> write text to a destination stream with buffer.
    - very fast thanks to internal string buffer
    - write to streams with every supported encoding
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

  OBufferedStreams, OEncoding, OWideSupp;

type

  IOTextParseError = interface;
  TOTextParseError = class;
  TOTextParseErrorClass = class of TOTextParseError;

  TOTextReaderErrorHandling = (ehSilent, ehRaiseAndEat, ehRaise);

  TOTextReader = class(TObject)
  private
    fTempString: OWideString;
    fTempStringPosition: Integer;
    fTempStringLength: Integer;
    fTempStringRemain: Integer;
    fBufferSize: Integer;

    fStream: TStream;
    fStreamSize: OStreamInt;
    fStreamPosition: OStreamInt;
    fStreamStartPosition: OStreamInt;
    fOwnsStream: Boolean;

    fFilePosition: OStreamInt;//current character in file (in character units, not bytes!), 1-based
    fLinePosition: OStreamInt;//current character in line, 1-based
    fLine: OStreamInt;//current line in file, 1-based

    fEncoding: TEncoding;
    fOwnsEncoding: Boolean;
    fBOMFound: Boolean;
    fEOF: Boolean;

    //undo support
    fPreviousChar: OWideChar;
    fReadFromUndo: Boolean;

    fErrorHandling: TOTextReaderErrorHandling;
    fParseError: IOTextParseError;

    procedure SetEncoding(const Value: TEncoding);

    function GetApproxStreamPosition: OStreamInt;

    procedure LoadStringFromStream;
    function GetTempStringPosition: Integer;

  protected
    procedure DoCreate(const aBufferSize: Integer); virtual;
    procedure DoInit(const aNewStream: TStream; const aNewOwnsStream: Boolean;
      const aDefaultEncoding: TEncoding); virtual;
  protected
    procedure DoRaiseException;
  public
    procedure RaiseException(const aErrorClass: TOTextParseErrorClass;
      const aReason: String);
    procedure RaiseExceptionFmt(const aErrorClass: TOTextParseErrorClass;
      const aReason: String; const aArgs: array of OWideString);
  public
    //create
    constructor Create(const aBufferSize: Integer = OBUFFEREDSTREAMS_DEFBUFFERSIZE); overload;
    //create and init
    constructor Create(const aStream: TStream;
      const aDefaultEncoding: TEncoding = nil;
      const aBufferSize: Integer = OBUFFEREDSTREAMS_DEFBUFFERSIZE); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for reading.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached or you call ReleaseDocument!

    //aDefaultEncoding - if no BOM is found, use this encoding,
    //  if BOM is found, always the correct encoding from the BOM is used

    //load document from file
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitFile(const aFileName: String; const aDefaultEncoding: TEncoding = nil);
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitStream(const aStream: TStream; const aDefaultEncoding: TEncoding = nil);
    //loads XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    procedure InitString(const aString: OWideString);
    {$IFDEF O_RAWBYTESTRING}
    procedure InitString_UTF8(const aString: ORawByteString);
    {$ENDIF}
    //load document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aDefaultEncoding: TEncoding = nil); overload;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer; const aDefaultEncoding: TEncoding = nil); overload;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //read char-by-char, returns false if EOF is reached
    function ReadNextChar(var outChar: OWideChar): Boolean;
    //read text
    function ReadString(const aMaxChars: Integer; const aBreakAtNewLine: Boolean = False): OWideString;
    //get text from temp buffer that has been already read
    //  -> it's not assured that some text can be read, use only as extra information
    //     e.g. for errors etc.
    function ReadPreviousString(const aMaxChars: Integer; const aBreakAtNewLine: Boolean = False): OWideString;
    //go back 1 char. only 1 undo operation is supported
    procedure UndoRead;

    //if your original stream does not allow seeking and you want to change encoding at some point
    //  (e.g. the encoding is read from the text itself) you have to block the temporary buffer
    procedure BlockFlushTempBuffer;
    procedure UnblockFlushTempBuffer;
  public
    //encoding of the text that is read from the stream
    //  when changing encoding, the stream is always reset to the starting position
    //  and the stream has to be read again
    property Encoding: TEncoding read fEncoding write SetEncoding;
    property OwnsEncoding: Boolean read fOwnsEncoding write fOwnsEncoding;
    //Returns true if BOM was found in the document
    property BOMFound: Boolean read fBOMFound;
    //Returns true if end-of-file is reached
    property EOF: Boolean read fEOF;

    //Approximate byte position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //Character position in text
    //  -> in Lazarus, the position is always in UTF-8 characters (no way to go around that since Lazarus uses UTF-8).
    //  -> in Delphi the position is always correct
    property FilePosition: OStreamInt read fFilePosition;//absolute character position in file, 1-based
    property LinePosition: OStreamInt read fLinePosition;//current character in line, 1-based
    property Line: OStreamInt read fLine;//current line, 1-based
    property TempStringPosition: Integer read GetTempStringPosition;//Position in current TempStream, 1-based
    //size of original stream
    property StreamSize: OStreamInt read fStreamSize;

    property ParseError: IOTextParseError read fParseError;
    property ErrorHandling: TOTextReaderErrorHandling read fErrorHandling write fErrorHandling;
  end;

  TOTextWriter = class(TObject)
  private
    fTempString: OWideString;
    fTempStringPosition: Integer;
    fTempStringLength: Integer;

    fStream: TStream;
    fOwnsStream: Boolean;

    fEncoding: TEncoding;
    fOwnsEncoding: Boolean;

    fWriteBOM: Boolean;
    fBOMWritten: Boolean;

    procedure WriteStringToStream(const aString: OWideString; const aMaxLength: Integer);
    procedure SetEncoding(const Value: TEncoding);
  protected
    procedure DoCreate(const aBufferSize: Integer);
    procedure DoInit(const aNewStream: TStream; const aNewOwnsStream: Boolean;
      const aEncoding: TEncoding; const aWriteBOM: Boolean);
  public
    //create
    constructor Create(const aCharBufferSize: Integer = OBUFFEREDSTREAMS_DEFCHARBUFFERSIZE); overload;
    //create and init
    constructor Create(const aStream: TStream;
      const aEncoding: TEncoding = nil; const aWriteBOM: Boolean = True;
      const aCharBufferSize: Integer = OBUFFEREDSTREAMS_DEFCHARBUFFERSIZE); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TOTextWriter or call ReleaseDocument!

    procedure InitFile(const aFileName: String;
      const aEncoding: TEncoding = nil; const aWriteBOM: Boolean = True);
    procedure InitStream(const aStream: TStream;
      const aEncoding: TEncoding = nil; const aWriteBOM: Boolean = True);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //write string
    procedure WriteString(const aString: OWideString);
    procedure WriteChar(const aChar: OWideChar);
    //write the whole temporary buffer to the destination stream
    procedure EnsureTempStringWritten;
  public
    //encoding of the resulting stream
    //the encoding will be used only for new text, old text (that has already
    //been written with WriteString()) is written with last used encoding
    property Encoding: TEncoding read fEncoding write SetEncoding;
    property OwnsEncoding: Boolean read fOwnsEncoding write fOwnsEncoding;
    //should BOM be written
    property WriteBOM: Boolean read fWriteBOM write fWriteBOM;
  end;

  EOTextReader = class(Exception);

  EOTextReaderException = class(Exception)
  private
    fReason: String;
    fFilePos: OStreamInt;
    fLinePos: OStreamInt;
    fLine: OStreamInt;
    fSrcText: OWideString;
    fSrcTextPos: Integer;
  protected
    procedure DoCreate(const aError: TOTextParseError; const aReason: string);
  public
    constructor Create(const aError: TOTextParseError; const aReason: string);
  public
    //formatted error text
    function GetFormattedErrorText: OWideString;
    //error code
    class function GetErrorCode: Integer; virtual;
  public
    //error code
    property ErrorCode: Integer read GetErrorCode;
    //reason
    property Reason: String read fReason;
    //Character position in text (when error was detected)
    //  -> in Lazarus, the position is always in UTF-8 characters (no way to go around that since Lazarus uses UTF-8).
    //  -> in Delphi the position is always correct
    property FilePos: OStreamInt read fFilePos;//absolute character position in file (in character units, not bytes), 1-based
    property LinePos: OStreamInt read fLinePos;//current character in line, 1-based
    property Line: OStreamInt read fLine;//current line, 1-based
    //Source code stub
    property SrcText: OWideString read fSrcText;
    //position of error in SrcText, 1-based
    property SrcTextPos: Integer read fSrcTextPos;
  end;
  EOTextReaderExceptionClass = class of EOTextReaderException;

  EOTextReaderInvalidCharacter = class(EOTextReaderException)
  public
    class function GetErrorCode: Integer; override;
  end;

  IOTextParseError = interface
    ['{B2008FD1-65E2-44BE-97DA-38D42E44326C}']

    function GetExceptionClass: EOTextReaderExceptionClass;
    function GetErrorCode: Integer;
    function GetURL: OWideString;
    function GetReason: OWideString;
    function GetSrcText: OWideString;
    function GetSrcTextPos: Integer;
    function GetLine: OStreamInt;
    function GetLinePos: OStreamInt;
    function GetFilePos: OStreamInt;

    procedure RaiseException;
    procedure RaiseAndEatException;

    property ErrorCode: Integer read GetErrorCode;
    property FilePos: OStreamInt read GetFilePos;
    property Line: OStreamInt read GetLine;
    property LinePos: OStreamInt read GetLinePos;
    property Reason: OWideString read GetReason;
    property SrcText: OWideString read GetSrcText;
    property SrcTextPos: Integer read GetSrcTextPos;
    property URL: OWideString read GetURL;
  end;

  TOTextParseError = class(TInterfacedObject, IOTextParseError)
  private
    fURL: string;
    fReason: OWideString;
    fSrcText: OWideString;
    fSrcTextPos: Integer;
    fLine: OStreamInt;
    fLinePos: OStreamInt;
    fFilePos: OStreamInt;
  protected
    function GetExceptionClass: EOTextReaderExceptionClass; virtual; abstract;
    function GetErrorCode: Integer;
    function GetURL: OWideString;
    function GetReason: OWideString;
    function GetSrcText: OWideString;
    function GetSrcTextPos: Integer;
    function GetLine: OStreamInt;
    function GetLinePos: OStreamInt;
    function GetFilePos: OStreamInt;

    procedure DoCreate(const aReader: TOTextReader; const aReason: string);
  public
    constructor Create(const aReader: TOTextReader; const aReason: string);
    constructor CreateFmt(const aReader: TOTextReader; const aReason: string;
      const aArgs: array of const);
  public
    procedure RaiseException;
    procedure RaiseAndEatException;
  public
    property URL: OWideString read GetURL;
    property ErrorCode: Integer read GetErrorCode;
    property FilePos: OStreamInt read GetFilePos;
    property Line: OStreamInt read GetLine;
    property LinePos: OStreamInt read GetLinePos;
    property Reason: OWideString read GetReason;
    property SrcText: OWideString read GetSrcText;
    property SrcTextPos: Integer read GetSrcTextPos;
  end;

  TOTextParseErrorInvalidCharacter = class(TOTextParseError)
    function GetExceptionClass: EOTextReaderExceptionClass; override;
  end;

//decide what encoding is used in a stream (BOM markers are searched for)
//  only UTF-8, UTF-16, UTF-16BE can be recognized
function GetEncodingFromStream(const aStream: TStream;
  var ioTempStringPosition: OStreamInt;
  const aLastPosition: OStreamInt;
  const aDefaultEncoding: TEncoding): TEncoding;

implementation

{$IFDEF FPC}
uses
  LazUTF8;
{$ENDIF}

var
  OTextReadWrite_CannotUndo2Times: OWideString = 'Unsupported: you tried to run the undo function two times in a row.';
  OTextReadWrite_ReadingAt: String =
    'Reading at:'+sLineBreak+
    'Line: %d'+sLineBreak+
    'Char: %d'+sLineBreak+
    //'XML token line: %d'+sLineBreak+
    //'XML token char: %d'+sLineBreak+
    'Position in source stub: %d'+sLineBreak+
    'Source stub:'+sLineBreak+
    '%s';

function GetEncodingFromStream(const aStream: TStream;
  var ioTempStringPosition: OStreamInt;
  const aLastPosition: OStreamInt;
  const aDefaultEncoding: TEncoding): TEncoding;
var
  xSize: Integer;
  xBuffer: TEncodingBuffer;
  xEncoding: TEncoding;
begin
  if Assigned(aDefaultEncoding) then
    Result := aDefaultEncoding
  else
    Result := TEncoding.Default;

  xSize := aLastPosition - aStream.Position;
  if xSize < 2 then
    Exit;//BOM must be at least 2 characters

  if xSize > 4 then
    xSize := 4;//BOM may be up to 4 characters

  SetLength(xBuffer, xSize);
  aStream.ReadBuffer(xBuffer[TEncodingBuffer_FirstElement], xSize);
  xEncoding := nil;
  ioTempStringPosition := ioTempStringPosition +
    TEncoding.GetEncodingFromBOM(xBuffer, xEncoding, Result);

  if Assigned(xEncoding) then
    Result := xEncoding;

  if not Assigned(Result) then
  begin
    if Assigned(aDefaultEncoding) then
      Result := aDefaultEncoding
    else
      Result := TEncoding.Default;
  end;

  aStream.Position := ioTempStringPosition;
end;

{ TOTextReader }

procedure TOTextReader.BlockFlushTempBuffer;
begin
  if fStream is TOBufferedReadStream then
    TOBufferedReadStream(fStream).BlockFlushTempBuffer;
end;

constructor TOTextReader.Create(const aStream: TStream;
  const aDefaultEncoding: TEncoding; const aBufferSize: Integer);
begin
  inherited Create;

  DoCreate(aBufferSize);

  InitStream(aStream, aDefaultEncoding);
end;

constructor TOTextReader.Create(const aBufferSize: Integer);
begin
  inherited Create;

  DoCreate(aBufferSize);
end;

destructor TOTextReader.Destroy;
begin
  ReleaseDocument;

  if fOwnsEncoding then
    fEncoding.Free;

  inherited;
end;

procedure TOTextReader.DoCreate(const aBufferSize: Integer);
begin
  fBufferSize := aBufferSize;
end;

procedure TOTextReader.DoInit(const aNewStream: TStream;
  const aNewOwnsStream: Boolean; const aDefaultEncoding: TEncoding);
var
  xStreamPosition: Integer;
begin
  fEOF := False;
  ReleaseDocument;

  fStream := aNewStream;
  fOwnsStream := aNewOwnsStream;
  fStreamPosition := fStream.Position;
  fStreamStartPosition := fStreamPosition;
  fStreamSize := fStream.Size;
  fParseError := nil;

  BlockFlushTempBuffer;//block because GetEncodingFromStream seeks back in stream!
  try
    xStreamPosition := fStreamPosition;
    fEncoding := GetEncodingFromStream(fStream, fStreamPosition, fStreamSize, aDefaultEncoding);
    fBOMFound := (xStreamPosition < fStreamPosition);//if BOM was found, fStreamPosition increased
  finally
    UnblockFlushTempBuffer;
  end;
  fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);

  fTempStringPosition := 1;
  fTempStringLength := 0;
  fTempStringRemain := 0;
  fPreviousChar := #0;
  fReadFromUndo := False;

  fFilePosition := 0;
  fLinePosition := 0;
  fLine := 1;
end;

procedure TOTextReader.DoRaiseException;
begin
  case fErrorHandling of
    ehRaiseAndEat: fParseError.RaiseAndEatException;
    ehRaise: fParseError.RaiseException;
  end;
end;

function TOTextReader.GetApproxStreamPosition: OStreamInt;
begin
  //YOU CAN'T KNOW IT EXACTLY!!! (due to Lazarus Unicode->UTF8 or Delphi UTF8->Unicode conversion etc.)
  //the char lengths may differ from one character to another
  Result := fStreamPosition - fStreamStartPosition + fTempStringPosition;
end;

function TOTextReader.GetTempStringPosition: Integer;
begin
  Result := fTempStringPosition;
  if fReadFromUndo then
    Dec(Result);
end;

procedure TOTextReader.InitBuffer(const aBuffer: TBytes;
  const aDefaultEncoding: TEncoding);
var
  xLength: Integer;
  xNewStream: TStream;
begin
  xNewStream := TMemoryStream.Create;

  xLength := Length(aBuffer);
  if xLength > 0 then
    xNewStream.WriteBuffer(aBuffer[0], xLength);
  xNewStream.Position := 0;

  DoInit(xNewStream, True, aDefaultEncoding);
end;

procedure TOTextReader.InitBuffer(const aBuffer;
  const aBufferLength: Integer; const aDefaultEncoding: TEncoding);
var
  xNewStream: TStream;
begin
  xNewStream := TMemoryStream.Create;

  if aBufferLength > 0 then
    xNewStream.WriteBuffer(aBuffer, aBufferLength);
  xNewStream.Position := 0;

  DoInit(xNewStream, True, aDefaultEncoding);
end;

procedure TOTextReader.InitFile(const aFileName: String;
  const aDefaultEncoding: TEncoding);
begin
  DoInit(
    TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone),
    True,
    aDefaultEncoding);
end;

procedure TOTextReader.InitStream(const aStream: TStream;
  const aDefaultEncoding: TEncoding);
begin
  if (aStream is TCustomMemoryStream) or (aStream is TFileStream)
  then begin
    //no need for buffering on memory stream or file stream
    //  buffering is here just because some (custom) streams may not support seeking
    //  which is needed when reading encoding from xml header
    DoInit(aStream, False, aDefaultEncoding);
  end else begin
    //we need to buffer streams that do not support seeking (zip etc.)
    DoInit(
      TOBufferedReadStream.Create(aStream, fBufferSize),
      True,
      aDefaultEncoding);
  end;
end;

procedure TOTextReader.InitString(const aString: OWideString);
var
  xLength: Integer;
  xNewStream: TStream;
begin
  xNewStream := TMemoryStream.Create;

  xLength := Length(aString);
  if xLength > 0 then
    xNewStream.WriteBuffer(aString[1], xLength * SizeOf(OWideChar));
  xNewStream.Position := 0;

  DoInit(xNewStream, True, nil);
  Encoding := TEncoding.OWideStringEncoding;
end;

procedure TOTextReader.LoadStringFromStream;
var
  xBuffer: TEncodingBuffer;
  xUTF8Inc: Integer;
  xReadBytes: OStreamInt;
const
  BS = TEncodingBuffer_FirstElement;
begin
  xReadBytes := fStreamSize-fStreamPosition;
  if xReadBytes > fBufferSize then
    xReadBytes := fBufferSize;
  if xReadBytes = 0 then
    Exit;

  SetLength(xBuffer, xReadBytes+5);//5 is maximum UTF-8 increment
  fStream.ReadBuffer(xBuffer[BS], xReadBytes);
  if fEncoding is TUTF8Encoding then begin
    //check if we did not reach an utf-8 character in the middle
    if
     ((Ord(xBuffer[BS+xReadBytes-1]) and $80) = $00)
    then//last byte is 0.......
      xUTF8Inc := 0
    else if
     ((xReadBytes > 1) and ((Ord(xBuffer[BS+xReadBytes-1]) and $E0) = $C0)) or//110..... -> double char
     ((xReadBytes > 2) and ((Ord(xBuffer[BS+xReadBytes-2]) and $F0) = $E0)) or//1110.... -> triple char
     ((xReadBytes > 3) and ((Ord(xBuffer[BS+xReadBytes-3]) and $F8) = $F0)) or//11110... -> 4 char
     ((xReadBytes > 4) and ((Ord(xBuffer[BS+xReadBytes-4]) and $FC) = $F8)) or//111110.. -> 5 char
     ((xReadBytes > 5) and ((Ord(xBuffer[BS+xReadBytes-5]) and $FE) = $FC))   //1111110. -> 6 char
    then
      xUTF8Inc := 1
    else if
     ((xReadBytes > 1) and ((Ord(xBuffer[BS+xReadBytes-1]) and $F0) = $E0)) or//1110.... -> triple char
     ((xReadBytes > 2) and ((Ord(xBuffer[BS+xReadBytes-2]) and $F8) = $F0)) or//11110... -> 4 char
     ((xReadBytes > 3) and ((Ord(xBuffer[BS+xReadBytes-3]) and $FC) = $F8)) or//111110.. -> 5 char
     ((xReadBytes > 4) and ((Ord(xBuffer[BS+xReadBytes-4]) and $FE) = $FC))   //1111110. -> 6 char
    then
      xUTF8Inc := 2
    else if
     ((xReadBytes > 1) and ((Ord(xBuffer[BS+xReadBytes-1]) and $F8) = $F0)) or//11110... -> 4 char
     ((xReadBytes > 2) and ((Ord(xBuffer[BS+xReadBytes-2]) and $FC) = $F8)) or//111110.. -> 5 char
     ((xReadBytes > 3) and ((Ord(xBuffer[BS+xReadBytes-3]) and $FE) = $FC))   //1111110. -> 6 char
    then
      xUTF8Inc := 3
    else if
     ((xReadBytes > 1) and ((Ord(xBuffer[BS+xReadBytes-1]) and $FC) = $F8)) or//111110.. -> 5 char
     ((xReadBytes > 2) and ((Ord(xBuffer[BS+xReadBytes-2]) and $FE) = $FC))   //1111110. -> 6 char
    then
      xUTF8Inc := 4
    else if
     ((xReadBytes > 1) and ((Ord(xBuffer[BS+xReadBytes-1]) and $FE) = $FC))   //1111110. -> 6 char
    then
      xUTF8Inc := 5
    else
      xUTF8Inc := 0;//ERROR ?

    if xUTF8Inc > 0 then
      fStream.ReadBuffer(xBuffer[BS+xReadBytes], xUTF8Inc);
  end else
    xUTF8Inc := 0;

  Inc(fStreamPosition, xReadBytes+xUTF8Inc);
  SetLength(xBuffer, xReadBytes+xUTF8Inc);
  {$IFDEF O_DELPHI_2009_UP}
  fTempString := fEncoding.GetString(xBuffer);
  {$ELSE}
  fEncoding.BufferToString(xBuffer, fTempString);
  {$ENDIF}
  if (Length(xBuffer) > 0) and (fTempString = '') then
  begin
    //must be here -> default is UTF-8 -> if BOM not set and codepage is some ANSI, UTF-8 returns empty string -> try ASCII and read codepage from <?xml?> tag afterwards
    {$IFDEF O_DELPHI_2009_UP}
    fTempString := TEncoding.ASCII.GetString(xBuffer);
    {$ELSE}
    TEncoding.ASCII.BufferToString(xBuffer, fTempString);
    {$ENDIF}
  end;

  fTempStringLength := Length(fTempString);
  fTempStringRemain := fTempStringLength;
  fTempStringPosition := 1;
end;

{$IFDEF O_RAWBYTESTRING}
procedure TOTextReader.InitString_UTF8(const aString: ORawByteString);
var
  xLength: Integer;
  xNewStream: TStream;
begin
  xNewStream := TMemoryStream.Create;

  xLength := Length(aString);
  if xLength > 0 then
    xNewStream.WriteBuffer(aString[1], xLength);
  xNewStream.Position := 0;

  DoInit(xNewStream, True, nil);
  Encoding := TEncoding.UTF8;
end;
{$ENDIF}

procedure TOTextReader.RaiseException(const aErrorClass: TOTextParseErrorClass;
  const aReason: String);
begin
  fParseError := aErrorClass.Create(Self, aReason);
  DoRaiseException;
end;

procedure TOTextReader.RaiseExceptionFmt(
  const aErrorClass: TOTextParseErrorClass; const aReason: String;
  const aArgs: array of OWideString);
var
  xArray: array of TVarRec;
  I: Integer;
begin
  SetLength(xArray, Length(aArgs));
  for I := Low(xArray) to High(xArray) do
  begin
    {$IFDEF O_DELPHI_2009_UP}
    xArray[I].VPWideChar := PWideChar(aArgs[I]);
    xArray[I].VType := vtPWideChar;
    {$ELSE}
    //we need to convert OWideString to (Ansi)String because non-unicode delphi does not support WideString as Format argument
    xArray[I].VPChar := PChar(String(aArgs[I]));
    xArray[I].VType := vtPChar;
    {$ENDIF}
  end;

  fParseError := aErrorClass.CreateFmt(Self, aReason, xArray);
  DoRaiseException;
end;

function TOTextReader.ReadNextChar(var outChar: OWideChar): Boolean;
begin
  if fReadFromUndo then begin
    outChar := fPreviousChar;
    fReadFromUndo := False;
    Result := True;
    Inc(fLinePosition);
    Inc(fFilePosition);
    Exit;
  end;

  if fTempStringRemain = 0 then
    LoadStringFromStream;

  if fTempStringRemain > 0 then begin
    outChar := fTempString[fTempStringPosition];
    case Ord(outChar) of
      10: begin
        if fPreviousChar <> #13 then
          Inc(fLine);
        fLinePosition := 0;
      end;
      13: begin
        fLinePosition := 0;
        Inc(fLine);
      end;
    else
      Inc(fLinePosition);
    end;

    fPreviousChar := outChar;
    Inc(fTempStringPosition);
    Dec(fTempStringRemain);
    Inc(fFilePosition);
    Result := True;
  end else begin
    fEOF := True;
    outChar := #0;
    ReleaseDocument;
    Result := False;
  end;
end;

function TOTextReader.ReadPreviousString(const aMaxChars: Integer;
  const aBreakAtNewLine: Boolean): OWideString;
var
  xTempStringPosition: Integer;
  xReadChars: Integer;
  I: Integer;
begin
  xTempStringPosition := TempStringPosition;

  xReadChars := xTempStringPosition-1;
  if xReadChars > aMaxChars then
    xReadChars := aMaxChars;

  if xReadChars > 0 then
  begin
    Result := Copy(fTempString, xTempStringPosition-xReadChars, xReadChars);

    if aBreakAtNewLine then
    for I := Length(Result) downto 1 do
    case Result[I] of
      #13, #10: begin
        //break at last new line
        if I = Length(Result) then
          Result := ''
        else
          Result := Copy(Result, I+1, Length(Result)-I);

        Exit;
      end;
    end;
  end else
    Result := '';
end;

function TOTextReader.ReadString(const aMaxChars: Integer;
  const aBreakAtNewLine: Boolean): OWideString;
var
  I, R: Integer;
  xC: OWideChar;
const
  cMaxStartBuffer = OBUFFEREDSTREAMS_DEFCHARBUFFERSIZE;
begin
  if aMaxChars <= 0 then begin
    Result := '';
    Exit;
  end;

  R := aMaxChars;
  if aMaxChars > cMaxStartBuffer then
    R := cMaxStartBuffer;
  SetLength(Result, R);
  I := 0;
  while (I < aMaxChars) and ReadNextChar({%H-}xC) do begin
    if aBreakAtNewLine then
    case xC of
      #10, #13: begin
        UndoRead;
        Break;
      end;
    end;

    Inc(I);
    if R = 0 then begin
      R := Length(Result);
      SetLength(Result, Length(Result) + R);
    end;
    Result[I] := xC;
    Dec(R);
  end;

  if I < aMaxChars then
    SetLength(Result, I);
end;

procedure TOTextReader.ReleaseDocument;
begin
  if fOwnsStream then
    fStream.Free;

  fStream := nil;
end;

procedure TOTextReader.SetEncoding(const Value: TEncoding);
begin
  if fEncoding <> Value then begin//the condition fEncoding <> Value must be here!!!
    if fOwnsEncoding then
      fEncoding.Free;
    fEncoding := Value;
    fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);

    //CLEAR ALREADY READ STRING AND GO BACK
    fStream.Position := fStreamStartPosition;
    fStreamPosition := fStreamStartPosition;

    fTempStringLength := 0;
    fTempStringPosition := 1;
    fTempStringRemain := 0;
  end;
end;

procedure TOTextReader.UnblockFlushTempBuffer;
begin
  if fStream is TOBufferedReadStream then
    TOBufferedReadStream(fStream).UnblockFlushTempBuffer;
end;

procedure TOTextReader.UndoRead;
begin
  if fReadFromUndo then
    raise EOTextReader.Create(OTextReadWrite_CannotUndo2Times);

  fReadFromUndo := True;
  Dec(fLinePosition);
  Dec(fFilePosition);
end;

{ TOTextWriter }

constructor TOTextWriter.Create(const aCharBufferSize: Integer);
begin
  inherited Create;

  DoCreate(aCharBufferSize)
end;

constructor TOTextWriter.Create(const aStream: TStream;
  const aEncoding: TEncoding; const aWriteBOM: Boolean;
  const aCharBufferSize: Integer);
begin
  inherited Create;

  DoCreate(aCharBufferSize);

  InitStream(aStream, aEncoding, aWriteBOM);
end;

destructor TOTextWriter.Destroy;
begin
  ReleaseDocument;

  if fOwnsEncoding then
    fEncoding.Free;

  inherited;
end;

procedure TOTextWriter.DoCreate(const aBufferSize: Integer);
begin
  fTempStringLength := aBufferSize;
  SetLength(fTempString, fTempStringLength);

  fEncoding := TEncoding.Default;
  fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);

  fWriteBOM := True;
end;

procedure TOTextWriter.DoInit(const aNewStream: TStream;
  const aNewOwnsStream: Boolean; const aEncoding: TEncoding;
  const aWriteBOM: Boolean);
begin
  ReleaseDocument;

  fStream := aNewStream;
  fOwnsStream := aNewOwnsStream;

  fTempStringPosition := 1;
  fBOMWritten := False;

  if Assigned(aEncoding) then
  begin
    Encoding := aEncoding;
    WriteBOM := aWriteBOM;
  end;
end;

procedure TOTextWriter.EnsureTempStringWritten;
begin
  if fTempStringPosition > 1 then begin
    if fTempStringLength = fTempStringPosition-1 then begin
      WriteStringToStream(fTempString, -1);
    end else begin
      WriteStringToStream(fTempString, fTempStringPosition-1);
    end;
    fTempStringPosition := 1;
  end;
end;

procedure TOTextWriter.InitFile(const aFileName: String;
  const aEncoding: TEncoding; const aWriteBOM: Boolean);
begin
  DoInit(TFileStream.Create(aFileName, fmCreate), True, aEncoding, aWriteBOM);
end;

procedure TOTextWriter.InitStream(const aStream: TStream;
  const aEncoding: TEncoding; const aWriteBOM: Boolean);
begin
  DoInit(aStream, False, aEncoding, aWriteBOM);
end;

procedure TOTextWriter.ReleaseDocument;
begin
  if Assigned(fStream) then
    EnsureTempStringWritten;

  if fOwnsStream then
    fStream.Free;
  fStream := nil;
end;

procedure TOTextWriter.SetEncoding(const Value: TEncoding);
begin
  if fEncoding <> Value then begin
    EnsureTempStringWritten;
    if fOwnsEncoding then
      fEncoding.Free;
    fEncoding := Value;
    fOwnsEncoding := not TEncoding.IsStandardEncoding(fEncoding);
  end;
end;

procedure TOTextWriter.WriteChar(const aChar: OWideChar);
begin
  if fTempStringPosition > fTempStringLength then begin
    EnsureTempStringWritten;//WRITE TEMP BUFFER
  end;

  fTempString[fTempStringPosition] := aChar;
  Inc(fTempStringPosition);
end;

procedure TOTextWriter.WriteString(const aString: OWideString);
var
  xStringLength: Integer;
  {$IFDEF O_DELPHI_2007_DOWN}
  I: Integer;
  {$ENDIF}
begin
  xStringLength := Length(aString);
  if xStringLength = 0 then
    Exit;

  if fTempStringPosition-1 + xStringLength > fTempStringLength then begin
    EnsureTempStringWritten;//WRITE TEMP BUFFER
  end;

  if xStringLength > fTempStringLength then begin
    WriteStringToStream(aString, -1);
  end else begin
    {$IFDEF O_DELPHI_2007_DOWN}
    //Move() is extremly slow here in Delphi 7, copy char-by-char is faster also for long strings!!! (this may be a delphi bug)
    for I := 0 to xStringLength-1 do
      fTempString[fTempStringPosition+I] := aString[I+1];
    {$ELSE}
    Move(aString[1], fTempString[fTempStringPosition], xStringLength*SizeOf(OWideChar));
    {$ENDIF}
    fTempStringPosition := fTempStringPosition + xStringLength;
  end;
end;

procedure TOTextWriter.WriteStringToStream(const aString: OWideString; const aMaxLength: Integer);
var
  xBytes: TEncodingBuffer;
  xBytesLength: Integer;
  xBOM: TEncodingBuffer;
begin
  if fWriteBOM and not fBOMWritten then begin
    //WRITE BOM
    {$IFDEF O_DELPHI_2007_DOWN}
    SetLength(xBOM, 0);//suppress Delphi warnings
    {$ENDIF}
    xBOM := fEncoding.GetBOM;
    if Length(xBOM) > 0 then
      fStream.WriteBuffer(xBOM[TEncodingBuffer_FirstElement], Length(xBOM));
  end;
  fBOMWritten := True;

  if aMaxLength < 0 then begin
    //write complete string
    {$IFDEF O_DELPHI_2009_UP}
    xBytes := fEncoding.GetBytes(aString);
    {$ELSE}
    fEncoding.StringToBuffer(aString, {%H-}xBytes);
    {$ENDIF}
    xBytesLength := Length(xBytes);
  end else begin
    //write part of string
    {$IFDEF O_DELPHI_2009_UP}
    xBytes := fEncoding.GetBytes(Copy(aString, 1, aMaxLength));
    {$ELSE}
    fEncoding.StringToBuffer(Copy(aString, 1, aMaxLength), {%H-}xBytes);
    {$ENDIF}
    xBytesLength := Length(xBytes);
  end;

  if xBytesLength > 0 then
    fStream.WriteBuffer(xBytes[TEncodingBuffer_FirstElement], xBytesLength);
end;

{ EXmlReaderException }

constructor EOTextReaderException.Create(const aError: TOTextParseError;
  const aReason: string);
begin
  DoCreate(aError, aReason);

  inherited Create(
    aReason+
    sLineBreak+sLineBreak+
    GetFormattedErrorText);
end;

procedure EOTextReaderException.DoCreate(const aError: TOTextParseError;
  const aReason: string);
begin
  fReason := aReason;
  fLinePos := aError.LinePos;
  fLine := aError.Line;
  fSrcText := aError.SrcText;
  fSrcTextPos := aError.SrcTextPos;
end;

class function EOTextReaderException.GetErrorCode: Integer;
begin
  Result := 0;
end;

function EOTextReaderException.GetFormattedErrorText: OWideString;
begin
  Result :=
    Format(OTextReadWrite_ReadingAt, [
      Line,
      LinePos,
      SrcTextPos,
      SrcText]);
end;

{ TOTextParseError }

constructor TOTextParseError.Create(const aReader: TOTextReader;
  const aReason: string);
begin
  inherited Create;

  DoCreate(aReader, aReason);
end;

constructor TOTextParseError.CreateFmt(const aReader: TOTextReader;
  const aReason: string; const aArgs: array of const);
var
  xReason: string;
begin
  inherited Create;

  xReason := Format(aReason, aArgs);
  DoCreate(aReader, xReason);
end;

procedure TOTextParseError.DoCreate(const aReader: TOTextReader;
  const aReason: string);
begin
  fReason := aReason;
  fLinePos := aReader.LinePosition;
  fLine := aReader.Line;
  fSrcText := aReader.ReadPreviousString(30, True);
  fSrcTextPos := Length(fSrcText);
  fSrcText := fSrcText + aReader.ReadString(10, True);
end;

function TOTextParseError.GetErrorCode: Integer;
begin
  Result := GetExceptionClass.GetErrorCode;
end;

function TOTextParseError.GetFilePos: OStreamInt;
begin
  Result := fFilePos;
end;

function TOTextParseError.GetLine: OStreamInt;
begin
  Result := fLine;
end;

function TOTextParseError.GetLinePos: OStreamInt;
begin
  Result := fLinePos;
end;

function TOTextParseError.GetReason: OWideString;
begin
  Result := fReason;
end;

function TOTextParseError.GetSrcText: OWideString;
begin
  Result := fSrcText;
end;

function TOTextParseError.GetSrcTextPos: Integer;
begin
  Result := fSrcTextPos;
end;

function TOTextParseError.GetURL: OWideString;
begin
  Result := fURL;
end;

procedure TOTextParseError.RaiseAndEatException;
begin
  try
    RaiseException;
  except
    on EOTextReaderException do
    begin
      //eat EOTextReaderException
    end;
  end;
end;

procedure TOTextParseError.RaiseException;
begin
  raise GetExceptionClass.Create(Self, fReason);
end;

{ TOTextParseErrorInvalidCharacter }

function TOTextParseErrorInvalidCharacter.GetExceptionClass: EOTextReaderExceptionClass;
begin
  Result := EOTextReaderInvalidCharacter;
end;

{ EOTextReaderInvalidCharacter }

class function EOTextReaderInvalidCharacter.GetErrorCode: Integer;
begin
  Result := 0;
end;

end.
