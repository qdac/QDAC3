unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.StdActns, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    FileOpen1: TFileOpen;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring, qjson;
{$R *.dfm}

function DFM2Json(AFileName: String): TQJson;
var
  ADFMStream, ATemp: TMemoryStream;
const
  SSpace: PWideChar = ' '#9;
  SColon: PWideChar = ':';
  SNull: WideChar = #0;
  function IsTextDFM(S: PAnsiChar): Boolean;
  begin
    Result := (AnsiStrLComp(S, 'object', 6) = 0) or
      (AnsiStrLComp(S, 'inherited', 9) = 0);
  end;
  procedure DecodeObject(ALine: PWideChar; var AObjName, AObjClass: QStringW);
  begin
    SkipUntilW(ALine, SSpace);
    AObjName := Trim(DecodeTokenW(ALine, SColon, SNull, True));
    AObjClass := Trim(ALine);
  end;
  function DecodeDFMString(S: QStringW): QStringW;
  var
    ps, pd: PWideChar;
    V: Int64;
  begin
    if Length(S) > 0 then
    begin
      ps := PWideChar(S);
      SetLength(Result, Length(S));
      pd := PWideChar(Result);
      while ps^ <> #0 do
      begin
        if ps^ = '#' then
        begin
          Inc(ps);
          if ParseInt(ps, V) > 0 then
          begin
            pd^ := WideChar(V);
            Inc(pd);
          end;
        end
        else if ps^ = '''' then
        begin
          Inc(ps);
          while ps^ <> '''' do
          begin
            pd^ := ps^;
            Inc(pd);
            Inc(ps);
          end;
          Inc(ps);
        end
        else
        begin
          break;
        end;
      end;
      SetLength(Result, pd - PWideChar(Result));
    end
    else
      Result := '';
  end;

  procedure DecodePropAndChildren(AParent: TQJson; var ps: PWideChar;
    AIsCollection: Boolean);
  var
    ALine, AName, AValue: QStringW;
    pl, pv: PWideChar;
    ATemp: TQJson;
    AProps: TQJson;
  const
    SObject: PWideChar = 'object';
    SInline: PWideChar = 'inline';
    SEnd: PWideChar = 'end';
    SEqual: PWideChar = '=';
    SNull: WideChar = #0;
    SItem: PWideChar = 'item';
    SBracket: PWideChar = ')';
  begin
    AProps := AParent;
    repeat
      ALine := DecodeLineW(ps);
      pl := PWideChar(ALine);
      SkipSpaceW(pl);
      SkipSpaceW(ps);
      if StartWithW(pl, SObject, True) or StartWithW(pl, SInline, True) then
      begin
        DecodeObject(pl, AName, AValue);
        DecodePropAndChildren(AParent.Add(AName, jdtObject), ps, False);
      end
      else if StartWithW(pl, SEnd, True) then
        Exit
      else // Property line
      begin
        AName := Trim(DecodeTokenW(pl, SEqual, SNull, True));
        AValue := Trim(pl);
        pv := PWideChar(AValue);
        if (pv^ = '''') or (pv^ = '#') then
        begin
          AProps.ForcePath(AName).AsString := DecodeDFMString(AValue);
        end
        else if pv^ = '<' then
        begin
          Inc(pv);
          SkipSpaceW(pv);
          if pv^ <> '>' then
          begin
            ATemp := TQJson.Create;
            ATemp.DataType := jdtArray;
            try
              SkipSpaceW(ps);
              while StartWithW(ps, SItem, True) do
              begin
                SkipLineW(ps);
                DecodePropAndChildren(ATemp.Add, ps, True);
              end;
              SkipSpaceW(ps);
              if ps^ = '>' then
                SkipLineW(ps);
            finally
              if ATemp.Count > 0 then
                AProps.Add(AName).Assign(ATemp)
              else
                FreeObject(ATemp);
            end;
          end;
        end
        else if pv^ = '(' then // Strings
        begin
          AValue := '';
          while ps^ <> #0 do
          begin
            ALine := Trim(DecodeLineW(ps));
            if Length(AValue) > 0 then
              AValue := AValue + SLineBreak + DecodeDFMString(ALine)
            else
              AValue := DecodeDFMString(ALine);
            if (ps^ = ')') or EndWithW(ALine, SBracket, True) then
              break;
          end;
          SkipSpaceW(ps);
          if ps^ = ')' then
            SkipLineW(ps);
          SkipSpaceW(ps);
          if Length(AValue) > 0 then
            AProps.ForcePath(AName).AsString := AValue;
        end
        else
          AProps.ForcePath(AName).Value := AValue;
      end;
    until Length(ALine) = 0;
  end;

  procedure DoConvert(AText: QStringW);
  var
    p: PWideChar;
    AObjectName, ARootClass: QStringW;
  const
    SObject: PWideChar = 'object ';
    SInherited: PWideChar = 'inherited ';
  begin
    if Length(AText) > 0 then
    begin
      p := PWideChar(AText);
      if StartWithW(p, SObject, True) or StartWithW(p, SInherited, True) then
      begin
        Result := TQJson.Create;
        try
          DecodeObject(PWideChar(DecodeLineW(p)), AObjectName, ARootClass);
          DecodePropAndChildren(Result.Add(ARootClass, jdtObject), p, False);
        finally
          if Result.Count = 0 then
            FreeAndNil(Result);
        end;
      end;
    end;
  end;

begin
  ADFMStream := TMemoryStream.Create;
  try
    ADFMStream.LoadFromFile(AFileName);
    if PCardinal(ADFMStream.Memory)^ = $30465054 then
    begin
      ATemp := TMemoryStream.Create;
      ATemp.CopyFrom(ADFMStream, 0);
      ATemp.Position := 0;
      ADFMStream.Size := 0;
      ObjectBinaryToText(ATemp, ADFMStream);
      FreeAndNil(ATemp);
      ADFMStream.Position := 0;
      DoConvert(LoadTextW(ADFMStream));
    end
    else if IsTextDFM(ADFMStream.Memory) then
      DoConvert(LoadTextW(ADFMStream));
  finally
    FreeObject(ADFMStream);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AJson: TQJson;
begin
  if FileOpen1.Execute then
  begin
    AJson := DFM2Json(FileOpen1.Dialog.FileName);
    if AJson <> nil then
    begin
      Memo1.Lines.Text := AJson.AsString;
      FreeAndNil(AJson);
    end;
  end;
end;

end.
