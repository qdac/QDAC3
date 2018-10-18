unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Rtti, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qjson;
{$R *.dfm}

type
  PConvertStackItem = ^TConvertStackItem;

  TConvertStackItem = record
    Addr: Pointer;
    Prior: PConvertStackItem;
  end;

procedure Class2Json(AObj: TObject; AJson: TQJson);
var
  AContext: TRttiContext;
  AStackLast: PConvertStackItem;

  procedure Push(Addr: Pointer);
  var
    AItem: PConvertStackItem;
  begin
    New(AItem);
    AItem.Addr := Addr;
    AItem.Prior := AStackLast;
    AStackLast := AItem;
  end;

  procedure Pop;
  var
    APrior: PConvertStackItem;
  begin
    if Assigned(AStackLast) then
    begin
      APrior := AStackLast.Prior;
      Dispose(AStackLast);
      AStackLast := APrior;
    end;
  end;

  function IsNest(Addr: Pointer): Boolean;
  var
    AItem: PConvertStackItem;
  begin
    AItem := AStackLast;
    Result := False;
    while Assigned(AItem) do
    begin
      if AItem.Addr = Addr then
      begin
        Result := True;
        Break;
      end
      else
        AItem := AItem.Prior;
    end;
  end;

  procedure DoConvert(APObj: TObject; APJson: TQJson);
  var
    AType: TRttiType;
    AFields: TArray<TRttiField>;
    AField: TRttiField;
    I: Integer;
    AChildComplex: Pointer;
  begin
    AType := AContext.GetType(APObj.ClassType);
    if Assigned(AType) then
    begin
      AFields := AType.GetFields;
      for I := 0 to High(AFields) do
      begin
        AField := AFields[I];
        case AField.FieldType.TypeKind of
          tkInteger:
            APJson.Add(AField.Name).AsInteger := AField.GetValue(APObj)
              .AsInteger;
          tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
            APJson.Add(AField.Name).AsString := AField.GetValue(AObj).AsString;
          tkEnumeration:
            begin
              if AField.FieldType.Handle^ = TypeInfo(Boolean) then
                APJson.Add(AField.Name).AsString := AField.GetValue(AObj)
                  .AsBoolean
              else
                APJson.Add(AField.Name).AsString :=
                  AField.GetValue(AObj).ToString;
            end;
          tkFloat:
            APJson.Add(AField.Name).AsFloat := AField.GetValue(AObj).AsExtended;
          tkSet:
            APJson.Add(AField.Name).AsString := AField.GetValue(AObj).ToString;
          tkClass:
            begin
              AChildComplex := AField.GetValue(AObj).AsObject;
              if Assigned(AChildComplex) and (not IsNest(AChildComplex)) then
                DoConvert(AChildComplex, APJson.Add(AField.Name));
            end;
          tkVariant:
            APJson.Add(AField.Name).AsVariant := AField.GetValue(AObj)
              .AsVariant;
          tkArray:
            ;
          tkRecord:
            ;
          tkInt64:
            ;
          tkDynArray:
            ;
        end;
      end;
    end;
  end;

begin
  AStackLast := nil;
  AContext := TRttiContext.Create;
  Push(AObj);
  try
    DoConvert(AObj, AJson);
  finally
    Pop;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    Memo1.Lines.Text := AJson.AsJson;
  finally
    FreeAndNil;
  end;
end;

end.
