unit UJSON;

interface

uses
  SysUtils,Classes,Generics.Collections;

{$REGION ' About '}
///////////////////////////////////////////////////////////////////////////////////////////////////////
///  Автор: Григорьев Е.В.
///  Описание: Библиотека для работы с большими JSON обьектами
///
///
///////////////////////////////////////////////////////////////////////////////////////////////////////
{$ENDREGION}

type

  TJSONType = (jt_unknown,jt_object,jt_array,jt_string,jt_variant);

  IJSONObject = interface
    ////////////// Служебные методы
    procedure SetAsString(const AValue: string);
    procedure SetName(const AValue: string);
    procedure Shift(AStartIndex, ADistance : Integer);
    procedure Parse(var CurrentPosition : integer);
    function GetChilds(Index: integer): IJSONObject; overload;
    function GetChilds(Index: string): IJSONObject; overload;
    procedure SetChilds(Index: string; const AValue: IJSONObject);
    function GetAsBoolean: boolean;
    function GetCount: integer;
    function GetName: string;
    function GetAsList : TList<IJSONObject>;
    function GetAsString: string;
    function GetAsInt : Integer;
    function GetAsInt64 : Int64;
    function GetAsDouble : double;
    function GetText: string;
    function GetLevel: Integer;
    function GetParent: IJSONObject;
    function GetJType: TJsonType;
    function GetStart : integer;
    function GetFinish : integer;
    function GetValue: string;
    procedure SetValue(const AValue: string);
    function GetDebug: boolean;
    procedure SetIsGenerating(const AValue: boolean);
    function GetIsGenerating: boolean;
    ///////////////////////////////////////
    function Find(AText : string; AStartIndex : Integer = 0) : IJSONObject;  // Поиск обьекта по тексту
    function AddChild(AName,AValue : string) : IJSONObject; // добавить дочерний элемент
    function GetNode(AIndex: integer): IJSONObject; // получить узел по текстовому индексу
    //////////////////////////////////////
    property Name : string read GetName write SetName; // возвращает Name объекта, который представлен как {name:value}
    property Count : integer read GetCount; // количество дочерних обьектов
    property Parent : IJSONObject read GetParent; // Родительский объект
    property Level : Integer read GetLevel; // уровень вложенности
    property AsList : TList<IJSONObject> read GetAsList; // озвращает дочерние обьекты в виде листа
    property AsInt : Integer read GetAsInt; // пытается вернуть Value как Integer
    property AsInt64 : Int64 read GetAsInt64; // пытается вернуть Value как Int64
    property AsDouble : double read GetAsDouble; // пытается вернуть Value как double
    property AsString : string read GetAsString write SetAsString;//возвращает Value ввиде строки
    property AsBoolean : boolean read GetAsBoolean; // пытается вернуть Value как Boolean
    property Text : string read GetText; // полный JSON код объекта
    property JType : TJsonType read GetJType; // типа объекта
    property Childs[Index : string] : IJSONObject read GetChilds write SetChilds; default;  // Дочерние объекты по имени
    property Items[Index : integer] : IJSONObject read GetChilds; // Дочерние объекты по порядковому индексу
    property Start : integer read GetStart; // начальный текстовый индекс обьекта
    property Finish : Integer read GetFinish; // конечный текстовый индекс обьекта
    property Value : string read GetValue write SetValue;// возвращает Value объекта, который представлен как {name:value}
    property IsGenerating : boolean read GetIsGenerating write SetIsGenerating;// если true попытка обратиться к несуществующему элементу приведет к его созданию
  end;

  EJSONException = class(Exception);
  EJSONConvertError = class(EJSONException);

function ParseJSON(AJSON : string) : IJSONObject; // Разбор строки
function LoadJSON(AFileName : string) : IJSONObject; //Загрузка файла с разбором
function NewJSON : IJSONObject; // создает новый объект в режиме генерации
function ArrayToJSON(AArray : array of const):IJSONObject; // создает обьект из массива

var

  EnableJSONException : boolean = false;
  ElementsCount : Uint64 = 0; //переменная увеличивается при создании элемента и уменьшается при уничтожении, используется при тесте производительности

implementation

type

  TStringElement = packed record
    Start,Finish : integer;
  end;

  TJSON = class;

  TJSONObject = class(TInterfacedObject,IJSONObject)
  private
    FOwner : TJSON;
    FJSONText : TStringElement;
    FName : TStringElement;
    FParent : TJSONObject;
    FLevel : Integer;
    FChilds : TList<IJSONObject>;
    FValue : TStringElement;
    FJSONType: TJsonType;
    function GetChilds(Index: integer): IJSONObject; overload;
    function GetChilds(Index: string): IJSONObject; overload;
    procedure SetChilds(Index: string; const AValue: IJSONObject);
    function GetCount: integer;
    function GetName: string;
    function GetLevel: Integer;
    function GetParent: IJSONObject;
    procedure ReadChilds(var CurrentPosition : integer);
    procedure ReadValue(var CurrentPosition : integer);
    function GetStrValue(AStrElement : TStringElement) : string;
    function GetAsList : TList<IJSONObject>;
    function GetAsString: string;
    function GetAsInt : Integer;
    function GetAsInt64 : Int64;
    function GetAsDouble : double;
    function GetText: string;
    function GetAsBoolean: boolean;
    procedure Shift(AStartIndex, ADistance : Integer);
    procedure SetAsString(const AValue: string);
    procedure SetName(const AValue: string);
    function GetJType: TJsonType;
    function AddChild(AName,AValue : string) : IJSONObject;
    function GetNode(AIndex : integer) : IJSONObject;
    function GetStart : integer;
    function GetFinish : integer;
    function GetValue: string;
    procedure SetValue(const AValue: string);
    function GetOwner: TObject;
    function GetDebug: boolean;
    procedure Clear(var AStr : string);
  protected
    procedure SetIsGenerating(const AValue: boolean); virtual;
    function GetIsGenerating: boolean; virtual;
  public
    constructor Create(AOwner : TJSON; AParent : TJSONObject); overload; virtual;
    destructor Destroy; override;
    procedure Parse(var CurrentPosition : integer);
    function Find(AText : string; AStartIndex : Integer = 0) : IJSONObject;
    property AsList : TList<IJSONObject> read GetAsList;
    property AsInt : Integer read GetAsInt;
    property AsInt64 : Int64 read GetAsInt64;
    property AsDouble : double read GetAsDouble;
    property AsString : string read GetAsString write SetAsString;
    property AsBoolean : boolean read GetAsBoolean;
    property Name : string read GetName write SetName;
    property Count : integer read GetCount;
    property Parent : IJSONObject read GetParent;
    property Level : Integer read GetLevel;
    property JType : TJsonType read GetJType;
    property Text : string read GetText;
    property Value : string read GetValue write SetValue;
    property Owner : TObject read GetOwner;
    property Debud : boolean read GetDebug;
    property IsGenerating : boolean read GetIsGenerating write SetIsGenerating;
  end;

  TJSON = class(TJSONObject)
  protected
    procedure SetIsGenerating(const AValue: boolean); override;
    function GetIsGenerating: boolean; override;
  public
    FText: string;
    FIsGenerating: boolean;
    constructor Create; overload;
    procedure LoadFromFile(AFileName : string);
    procedure LoadFromString(AString : string);
  end;

function ArrayToJSON(AArray : array of const):IJSONObject;
var
  I: Integer;
  tmpValue : string;
  tmpFormatSettings : TFormatSettings;
begin
  Result := NewJSON;
  Result.IsGenerating := true;
  Result.AddChild('','[]');
  tmpFormatSettings.DateSeparator := '.';
  for I := Low(AArray) to High(AArray) do
  begin
    case AArray[I].VType of
      vtInteger,vtInt64: tmpValue := IntToStr(AArray[I].VInteger);
      vtBoolean:         tmpValue := BoolToStr(AArray[I].VBoolean,true);
      vtChar:            tmpValue := '"' + AArray[I].VChar + '"';
      vtExtended:        tmpValue := FloatToStr(AArray[I].VExtended^,tmpFormatSettings);
      vtString:          tmpValue := '"' + string(AArray[I].VString) + '"';
      vtPointer:         tmpValue := '$' + IntToHex(Integer(AArray[I].VPointer),8);
      vtPChar:           tmpValue := '"' + AArray[I].VPChar + '"';
      vtObject:          tmpValue := '$' + IntToHex(Integer(AArray[I].VObject),8);
      vtClass:           tmpValue := '$' + IntToHex(Integer(AArray[I].VClass),8);
      vtWideChar:        tmpValue := '"' + AArray[I].VWideChar + '"';
      vtPWideChar:       tmpValue := '"' + AArray[I].VPWideChar + '"';
      vtAnsiString:      tmpValue := '"' + string(AArray[I].VAnsiString) + '"';
      vtCurrency:        tmpValue := CurrToStr(AArray[I].VCurrency^,tmpFormatSettings);
      vtVariant:         tmpValue := string(AArray[I].VVariant);
      vtInterface:       tmpValue := IntToHex(Integer(AArray[I].VObject),8);
      vtWideString:      tmpValue := '"' + string(AArray[I].VWideString) + '"';
      vtUnicodeString:   tmpValue := '"' + string(AArray[I].VUnicodeString) + '"';
    end;
    Result.Items[0].AddChild('',tmpValue);
  end;
end;

function NewJSON : IJSONObject; // создает новый объект в режиме генерации
begin
  Result := ParseJSON('{}');
  Result.IsGenerating := true;
end;

function LoadJSON(AFileName : string) : IJSONObject;
var
  tmpJSON : TJSON;
begin
  tmpJSON := TJSON.Create;
  tmpJSON.LoadFromFile(AFileName);
  Result := tmpJSON;
end;

function ParseJSON(AJSON : string) : IJSONObject;
var
  tmpJSON : TJSON;
begin
  tmpJSON := TJSON.Create;
  tmpJSON.LoadFromString(AJSON);
  Result := tmpJSON;
end;

procedure SkipSymbols(var CurrentPosition : integer; AData : string; ASymbol : Char);
var
  I: Integer;
begin
  for I := CurrentPosition to High(AData) do
    if AData[I] <> ASymbol then
    begin
      CurrentPosition := I;
      Exit;
    end;
end;

procedure SkipTo(var CurrentPosition : integer; AData : string; ASymbol : Char);
var
  I: Integer;
begin
  for I := CurrentPosition to High(AData) do
    if (AData[I] = ASymbol) and (AData[I - 1] <> '\') then
    begin
      CurrentPosition := I;
      Exit;
    end;
end;

{ TJSON }

function TJSON.GetIsGenerating: boolean;
begin
  Result := FIsGenerating;
end;

procedure TJSON.LoadFromFile(AFileName: string);
var
  tmpFile : TStringStream;
begin
  tmpFile := TStringStream.Create;
  try
    tmpFile.LoadFromFile(AFileName);
    LoadFromString(tmpFile.DataString);
  finally
    FreeAndNil(tmpFile);
  end;
end;

constructor TJSON.Create;
begin
  inherited Create(Self,nil);
end;

procedure TJSON.LoadFromString(AString: string);
var
  tmpCur : integer;
begin
  FText := AString;
  Clear(FText);
  tmpCur := 1;
  Parse(tmpCur);
end;

procedure TJSON.SetIsGenerating(const AValue: boolean);
begin
  FIsGenerating := AValue;
end;

{ TJSONObject }

function TJSONObject.GetChilds(Index: integer): IJSONObject;
begin
  Result := FChilds[Index];
end;

destructor TJSONObject.Destroy;
begin
  FreeAndNil(FChilds);
  Dec(ElementsCount);
  inherited;
end;

function TJSONObject.Find(AText: string; AStartIndex : Integer = 0): IJSONObject;
var
  I: Integer;
begin
  Result := nil;
  if AStartIndex >= FJSONText.Finish - FJSONText.Start then Exit;
  I := Pos(AText,FOwner.FText,FJSONText.Start + AStartIndex);
  if (I >= FJSONText.Finish) or (I = 0) then Exit;
  Result := GetNode(I);
end;

function TJSONObject.GetAsBoolean: boolean;
begin
  if not TryStrToBool(GetStrValue(FValue),Result) then
  begin
    Result := False;
    if EnableJSONException then
      raise EJSONConvertError.Create(GetStrValue(FValue) + ' - not boolean');
  end;
end;

function TJSONObject.GetAsDouble: double;
begin
  if not TryStrToFloat(GetStrValue(FValue),Result) then
  begin
    Result := 0.0;
    if EnableJSONException then
      raise EJSONConvertError.Create(GetStrValue(FValue) + ' - not double');
  end;
end;

function TJSONObject.GetAsInt64: Int64;
begin
  if not TryStrToInt64(GetStrValue(FValue),Result) then
  begin
    Result := 0;
    if EnableJSONException then
      raise EJSONConvertError.Create(GetStrValue(FValue) + ' - not int64');
  end;
end;

function TJSONObject.GetAsInt: Integer;
begin
  if not TryStrToInt(GetStrValue(FValue),Result) then
  begin
    Result := 0;
    if EnableJSONException then
      raise EJSONConvertError.Create(GetStrValue(FValue) + ' - not integer');
  end;
end;

function TJSONObject.GetAsList: TList<IJSONObject>;
begin
  Result := FChilds;
end;

function TJSONObject.GetAsString: string;
var
  i : integer;
begin
  Result := GetStrValue(FValue);
  I := 1;
  while i < Length(Result) do
  begin
    if Result[I] = '\' then
      if Result[I + 1] <> '\' then
      begin
        Delete(Result,I,1);
        Continue;
      end;
    inc(I);
  end;
end;

function TJSONObject.AddChild(AName,AValue : string) : IJSONObject;
var
  tmpString : string;
  I : integer;
begin
  Result := nil;

  Clear(AValue);

  case FJSONType of
    jt_object,jt_array: ;
    else Exit;
  end;

  if Count > 0 then
  begin
    Insert(',',FOwner.FText,FJSONText.Finish);
    FOwner.Shift(FJSONText.Finish - 1,1);
  end;

  AValue := AValue.Trim;

  if AName = '' then
    tmpString := AValue
  else
  begin
    tmpString := '"' + AName + '":' + AValue;
    if FJSONType = jt_array then
      tmpString := '{' + tmpString + '}';
  end;

  Insert(tmpString,FOwner.FText,FJSONText.Finish);
  I := FJSONText.Finish;
  FOwner.Shift(FJSONText.Finish - 1,tmpString.Length);

  Result := TJSONObject.Create(FOwner,Self);
  FChilds.Add(Result);
  Result.Parse(I);
end;

procedure TJSONObject.Clear(var AStr: string);
var
  tmpCur,tmpLength : integer;
  I: Integer;
begin
  for I := AStr.Length downto 1 do
  begin
    if AStr[I] in [#10,#13,#9] then
    begin
      Delete(AStr,I,1);
      Continue;
    end;
    if (AStr[I] = ' ') and (I > 1) then
      if AStr[I - 1] = ' ' then
        Delete(AStr,I,1);
  end;
end;

constructor TJSONObject.Create(AOwner: TJSON; AParent: TJSONObject);
begin
  Inc(ElementsCount);
  FOwner := AOwner;
  FParent := AParent;
  FLevel := 0;
  FChilds := TList<IJSONObject>.Create;
  if Assigned(FParent) then
    FLevel := FParent.Level + 1;
end;

function TJSONObject.GetChilds(Index: string): IJSONObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FChilds.Count - 1 do
    if FChilds[I].Name.ToUpperInvariant = Index.ToUpperInvariant then
      Exit(FChilds[I]);

  if IsGenerating then
  begin
    Result := AddChild(Index,'null');
  end
  else
    Result := TJSONObject.Create(FOwner,Self);
end;

function TJSONObject.GetCount: integer;
begin
  Result := FChilds.Count
end;

function TJSONObject.GetDebug: boolean;
var
  tmpStream : TStringStream;
begin
  Result := true;
  tmpStream := TStringStream.Create;
  tmpStream.WriteString(Text);
  tmpStream.SaveToFile('ObjDebug.json');
  FreeandNil(tmpStream);
end;

function TJSONObject.GetFinish: integer;
begin
  Result := FJSONText.Finish
end;

function TJSONObject.GetIsGenerating: boolean;
begin
  Result := FOwner.IsGenerating;
end;

function TJSONObject.GetText: string;
begin
  Result := GetStrValue(FJSONText);
end;

function TJSONObject.GetValue: string;
begin
  Result := GetStrValue(FValue);
end;

function TJSONObject.GetJType: TJsonType;
begin
  Result := FJSONType;
end;

function TJSONObject.GetLevel: Integer;
begin
  Result := FLevel
end;

function TJSONObject.GetName: string;
begin
  Result := GetStrValue(FName);
end;

function TJSONObject.GetNode(AIndex: integer): IJSONObject;
var
  I: Integer;
begin
  Result := nil;
  if (AIndex < FJSONText.Start) or (AIndex > FJSONText.Finish) then Exit;
  Result := Self;
  for I := 0 to FChilds.Count - 1 do
  begin
    if (AIndex >= FChilds[i].Start) and (AIndex <= FChilds[i].Finish) then
    begin
      Result := FChilds[I].GetNode(AIndex);
      Break;
    end;
  end;
end;

function TJSONObject.GetOwner: TObject;
begin
  Result := FOwner;
end;

function TJSONObject.GetParent: IJSONObject;
begin
  Result := FParent
end;

function TJSONObject.GetStart: integer;
begin
  Result := FJSONText.Start;
end;

function TJSONObject.GetStrValue(AStrElement: TStringElement): string;
begin
  Result := '';
  if AStrElement.Start = 0 then Exit;
  Result := Trim(Copy(FOwner.FText,AStrElement.Start,AStrElement.Finish - AStrElement.Start + 1));
end;

procedure TJSONObject.Parse(var CurrentPosition : integer);
begin
  if CurrentPosition >= FOwner.FText.Length then Exit;
  if FOwner.FText[CurrentPosition] in [']','}'] then Exit;

  FJSONText.Start := CurrentPosition;
  ReadValue(CurrentPosition);
  FJSONText.Finish := CurrentPosition - 1;
end;

procedure TJSONObject.ReadChilds(var CurrentPosition: integer);
var
  tmpChaild : IJSONObject;
begin
  repeat
    Inc(CurrentPosition);
    SkipSymbols(CurrentPosition,FOwner.FText,' ');
    tmpChaild := TJSONObject.Create(FOwner,Self);
    tmpChaild.Parse(CurrentPosition);
    if tmpChaild.Text = '' then
      Break;

    FChilds.Add(tmpChaild);

  until FOwner.FText[CurrentPosition] <> ',';
  SkipSymbols(CurrentPosition,FOwner.FText,' ');
  Inc(CurrentPosition);
end;

procedure TJSONObject.ReadValue(var CurrentPosition: integer);
var
  tmpName : TStringElement;
begin
  tmpName.Start := 0;
  FJsonType := jt_unknown;
  SkipSymbols(CurrentPosition,FOwner.FText,' ');
  if FOwner.FText[CurrentPosition] = '{' then
  begin
    FJsonType := jt_object;
  end;
  if FOwner.FText[CurrentPosition] = '[' then
  begin
    FJsonType := jt_array;
  end;
  case FJsonType of
    jt_object,jt_array:
    begin
      FValue.Start := CurrentPosition;
      ReadChilds(CurrentPosition);
      FValue.Finish := CurrentPosition - 1;
    end
    else
    begin
      if FOwner.FText[CurrentPosition] = '"' then
      begin
        Inc(CurrentPosition);
        tmpName.Start := CurrentPosition;
        SkipTo(CurrentPosition,FOwner.FText,'"');
        tmpName.Finish := CurrentPosition - 1;
        Inc(CurrentPosition);
        SkipSymbols(CurrentPosition,FOwner.FText,' ');
        if FOwner.FText[CurrentPosition] = ':' then
          FName := tmpName;

        if FOwner.FText[CurrentPosition] in [']','}',','] then
        begin
          FJSONType := jt_variant;
          FValue := tmpName;
          Exit;
        end;

        Inc(CurrentPosition);
        SkipSymbols(CurrentPosition,FOwner.FText,' ');
        if FOwner.FText[CurrentPosition] = '"' then
        begin
          FJSONType := jt_string;
          Inc(CurrentPosition);
          FValue.Start := CurrentPosition;
          SkipTo(CurrentPosition,FOwner.FText,'"');
          FValue.Finish := CurrentPosition - 1;
          Inc(CurrentPosition);
          SkipSymbols(CurrentPosition,FOwner.FText,' ');
        end
        else
          ReadValue(CurrentPosition);
      end
      else
      begin
        FJSONType := jt_variant;
        SkipSymbols(CurrentPosition,FOwner.FText,' ');
        FValue.Start := CurrentPosition;
        Inc(CurrentPosition);
        FValue.Finish := FOwner.FText.IndexOfAny([' ',',',']','}'],CurrentPosition - 1);
        CurrentPosition := FValue.Finish + 1;
        SkipSymbols(CurrentPosition,FOwner.FText,' ');
      end;
    end;
  end;
end;

procedure TJSONObject.SetAsString(const AValue: string);
var
  I : integer;
  tmpValue : string;
begin
  tmpValue := Value;
  I := 0;
  while I < Length(tmpValue) do
  begin
    Inc(I);
    if tmpValue[I] in ['\',',','[',']','{','}',':','"'] then
    begin
      Insert('\',tmpValue,I);
      Inc(I);
    end;
  end;
  Delete(FOwner.FText,FValue.Start,FValue.Finish - FValue.Start + 1);
  Insert(tmpValue,FOwner.FText,FValue.Start);
  FOwner.Shift(FValue.Start,Length(tmpValue) - FValue.Finish + FValue.Start - 1);
end;

procedure TJSONObject.SetChilds(Index: string; const AValue: IJSONObject);
var
  tmpObj : IJSONObject;
begin
  tmpObj := GetChilds(Index);
  tmpObj.Value := AValue.Value;
end;

procedure TJSONObject.SetIsGenerating(const AValue: boolean);
begin
end;

procedure TJSONObject.SetName(const AValue: string);
begin
  if FName.Start = 0 then Exit;
  Delete(FOwner.FText,FName.Start,FName.Finish - FName.Start + 1);
  Insert(Value,FOwner.FText,FName.Start);
  FOwner.Shift(FName.Start,Length(Value) - FName.Finish + FName.Start - 1);
end;

procedure TJSONObject.SetValue(const AValue: string);
var
  I : integer;
  tmpValue : string;
begin
  tmpValue := AValue;
  Clear(tmpValue);
  FChilds.Clear;
  Delete(FOwner.FText,FValue.Start,FValue.Finish - FValue.Start + 1);
  Insert(tmpValue,FOwner.FText,FValue.Start);
  FOwner.Shift(FValue.Start,Length(tmpValue) - FValue.Finish + FValue.Start - 1);
  I := FJSONText.Start;
  Parse(I);
end;

procedure TJSONObject.Shift(AStartIndex, ADistance: Integer);
var
  I: Integer;
begin
  if FJSONText.Start > AStartIndex then
    FJSONText.Start := FJSONText.Start + ADistance;
  if FJSONText.Finish > AStartIndex then
    FJSONText.Finish := FJSONText.Finish + ADistance;
  if FName.Start > AStartIndex then
    FName.Start := FName.Start + ADistance;
  if FName.Finish > AStartIndex then
    FName.Finish := FName.Finish + ADistance;
  if FValue.Start > AStartIndex then
    FValue.Start := FValue.Start + ADistance;
  if FValue.Finish > AStartIndex then
    FValue.Finish := FValue.Finish + ADistance;
  for I := 0 to FChilds.Count - 1 do
    FChilds[I].Shift(AStartIndex,ADistance);
end;

end.
