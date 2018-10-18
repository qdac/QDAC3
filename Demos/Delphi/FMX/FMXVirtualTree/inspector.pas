unit inspector;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.TypInfo, System.UIConsts, System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, qdac_fmx_virtualtree
{$IFDEF MSWINDOWS}
    , Winapi.Windows{$ENDIF}{$IFDEF LINUX}, FMUX.Api{$ENDIF};

type
  TQBaseProperty = class(TInterfacedObject)
  protected
    FInstance: Pointer;
    FProp: PPropInfo;
  public

  end;

  TQObjectProperties = class(TQBaseProperty)
  private
    function GetItems(const AIndex: Integer): TQBaseProperty;
  protected
    FProps: PPropList;
    FItems: TList<TQBaseProperty>;
    function GetCount: Integer;
  public
    constructor Create(AInstance: Pointer);
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TQBaseProperty
      read GetItems; default;
  end;

  TfrmObjInspector = class(TForm)
    SpeedButton1: TSpeedButton;
    Layout2: TLayout;
    Layout1: TLayout;
    vtProps: TQVirtualTreeView;
    cdText: TQVTCustomTextCell;
    cdCheck: TQVTCustomCheckCell;
    cdList: TQVTPickListCell;
    ceText: TQVTTextEditor;
    ceList: TQVTListEditor;
    procedure FormCreate(Sender: TObject);
    procedure cdListGetItems(Sender: TObject; AList: TStrings);
    procedure cdTextGetText(ASender: TObject; var AText: string);
    procedure vtPropsInitNode(Sender: TQVirtualTreeView; ANode: TQVTNode);
  private
    { Private declarations }
    FColorCellDrawer: IQVTDrawer;
    FPropList: TQObjectProperties;
  public
    { Public declarations }
  end;

var
  frmObjInspector: TfrmObjInspector;
procedure RegisterTypeEditor(AType: PTypeInfo; ACellData: IQVTCellData;
  ADrawer: IQVTDrawer; AEditorClass: TQVTCellEditorClass);
function FindTypeEditorClass(AType: PTypeInfo): TQVTCellEditorClass;

implementation

{$R *.fmx}

procedure TfrmObjInspector.cdListGetItems(Sender: TObject; AList: TStrings);
begin
  //
end;

procedure TfrmObjInspector.cdTextGetText(ASender: TObject; var AText: string);
begin
  //
end;

// procedure TfrmObjInspector.DoGetCellEditor(Sender: TQVirtualTreeView;
// ANode: TQVTNode; ACol: Integer; var AEditor: IQVTInplaceEditor);
// begin
// if ACol = 1 then
// begin
// AData := GetPropData(ANode);
// AClass := FindTypeEditorClass(AData.PropInfo.PropType^);
// if Assigned(AClass) then
// AEditor := AClass.Create
// else
// begin
// case AData.PropInfo.PropType^.Kind of
// tkUString, tkLString, tkInt64, tkFloat, tkChar, tkWChar{$IFNDEF NEXTGEN}
// , tkString, tkWString{$ENDIF !NEXTGEN}:
// AEditor := TQVTTextEditor.Create;
// tkInteger:
// begin
// if AData.ValueData is TIdentPropCellData then
// AEditor := TQVTListEditor.Create
// else
// AEditor := TQVTTextEditor.Create;
// end;
// tkEnumeration:
// AEditor := TQVTListEditor.Create;
// tkSet:
// if GetPropData(ANode.Parent).PropInfo = AData.PropInfo then
// AEditor := TQVTListEditor.Create;
// end;
// end;
// end;
// end;

procedure TfrmObjInspector.FormCreate(Sender: TObject);
begin
  // 我们来查看和修改SpeedButton1的属性
  FColorCellDrawer := TQVTColorDrawer.Create(Self);
  FPropList := TQObjectProperties.Create(SpeedButton1);
  vtProps.RootNodeCount := FPropList.Count;
end;

procedure TfrmObjInspector.vtPropsInitNode(Sender: TQVirtualTreeView;
  ANode: TQVTNode);
var
  AProp: TQBaseProperty;
begin
  if ANode.Parent = Sender.RootNode then
    AProp := FPropList[ANode.Index]
  else if ANode.Parent.ExtByType(TQBaseProperty, AProp) then
  begin
    if AProp is TQObjectProperties then
      AProp := TQObjectProperties(AProp)[ANode.Index]
    else
      Exit;
  end
  else
    Exit;

end;
//
// procedure TObjectPropData.SortProps;
// procedure QuickSort(L, R: Integer);
// var
// I, J: Integer;
// pivot, temp: PPropInfo;
// begin
// if (Length(FPropList) = 0) or ((R - L) <= 0) then
// Exit;
// repeat
// I := L;
// J := R;
// pivot := FPropList[L + (R - L) shr 1];
// repeat
// while CompareText(FPropList[I].NameFld.ToString,
// pivot.NameFld.ToString) < 0 do
// Inc(I);
// while CompareText(FPropList[J].NameFld.ToString,
// pivot.NameFld.ToString) > 0 do
// Dec(J);
// if I <= J then
// begin
// if I <> J then
// begin
// temp := FPropList[I];
// FPropList[I] := FPropList[J];
// FPropList[J] := temp;
// end;
// Inc(I);
// Dec(J);
// end;
// until I > J;
// if L < J then
// QuickSort(L, J);
// L := I;
// until I >= R;
// end;
//
// begin
// QuickSort(0, High(FPropList));
// end;

{ TPropData }
//
// constructor TPropData.Create(AInstance: Pointer; AProp: PPropInfo);
// begin
// inherited Create;
// FInstance := AInstance;
// FProp := AProp;
// end;
//
// { TTextPropCellData }
//
// function TTextPropCellData.GetText: String;
// var
// AData: TPropData;
// function PropValueText: String;
// var
// AObj: TObject;
// AIntToIdent: TIntToIdent;
// begin
// case AData.PropInfo.PropType^.Kind of
// tkInteger:
// begin
// AIntToIdent := FindIntToIdent(AData.PropInfo.PropType^);
// if Assigned(AIntToIdent) then
// begin
// if not AIntToIdent(GetOrdProp(AData.Instance, AData.PropInfo),
// Result) then
// Result := IntToHex(GetOrdProp(AData.Instance, AData.PropInfo),
// SizeOf(Pointer) shl 1);
// end
// else
// Result := IntToStr(GetOrdProp(AData.Instance, AData.PropInfo));
// end;
// tkChar, tkWChar:
// Result := WideChar(GetOrdProp(AData.Instance, AData.PropInfo));
// tkClass:
// begin
// AObj := AData.Instance;
// if Assigned(AObj) then
// Result := AObj.ToString
// else
// Result := '(Null)';
// end;
// tkEnumeration:
// if GetTypeData(AData.PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean)
// then
// begin
// case Boolean(GetOrdProp(AData.Instance, AData.PropInfo)) of
// false:
// Result := 'False';
// true:
// Result := 'True';
// end;
// end
// else
// Result := GetEnumProp(AData.Instance, AData.PropInfo);
// tkSet:
// Result := '[' + GetSetProp(AData.Instance, AData.PropInfo) + ']';
// tkFloat:
// Result := FloatToStr(GetFloatProp(AData.Instance, AData.PropInfo));
// tkMethod:
// Result := GetTypeName(AData.PropInfo^.PropType^);
// tkLString, tkUString
// {$IFNDEF NEXTGEN}
// , tkString, tkWString{$ENDIF !NEXTGEN}:
// Result := GetStrProp(AData.Instance, AData.PropInfo);
// tkVariant:
// Result := VarToStr(GetVariantProp(AData.Instance, AData.PropInfo));
// tkInt64:
// Result := IntToStr(GetInt64Prop(AData.Instance, AData.PropInfo))
// else
// Result := '<Unsupport Type>';
// end;
// end;
//
// begin
// AData := GetPropData(Node);
// if not Assigned(AData) then
// begin
// AData := GetPropData(Node.Parent);
// if AData.PropInfo.PropType^.Kind = tkSet then
// Result := '';
// // Result := GetEnumName(AData.PropInfo.PropType^.TypeData.BaseType^);
// end
// else
// begin
// case Column of
// 0:
// Result := AData.PropInfo.NameFld.ToString;
// 1:
// Result := PropValueText;
// end;
// end;
// end;
//
// procedure TTextPropCellData.SetText(const S: String);
// var
// AData: TPropData;
// begin
// AData := GetPropData(Node);
// case AData.PropInfo.PropType^.Kind of
// tkUString, tkLString{$IFNDEF NEXTGEN}
// , tkString, tkWString{$ENDIF !NEXTGEN}:
// SetStrProp(AData.Instance, AData.PropInfo, S);
// tkInteger:
// SetOrdProp(AData.Instance, AData.PropInfo, StrToInt(S));
// tkChar, tkWChar:
// begin
// if Length(S) = 1 then
// SetOrdProp(AData.Instance, AData.PropInfo, Ord(PChar(S)^));
// end;
// tkFloat:
// SetFloatProp(AData.Instance, AData.PropInfo, StrToFloat(S));
// tkInt64:
// SetInt64Prop(AData.Instance, AData.PropInfo, StrToInt64(S));
// end;
// end;
//
// { TColorCellDrawer }
//
// procedure TColorCellDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
// var
// R: TRectF;
// AColorData: IColorCellData;
// begin
// if Supports(AData, IColorCellData, AColorData) then
// begin
// R.Left := ARect.Left + 2;
// R.Right := ARect.Left + 14;
// R.Top := (ARect.Top + ARect.Bottom) / 2 - 6;
// R.Bottom := R.Top + 12;
// with AData.TreeView.Canvas do
// begin
// Fill.Kind := TBrushKind.Solid;
// Fill.Color := AColorData.Color;
// FillRect(R.SnapToPixel(Scale), 0, 0, [], AData.TreeView.Opacity);
// Stroke.Kind := TBrushKind.Solid;
// Stroke.Color := TAlphaColors.Black;
// DrawRect(R.SnapToPixel(Scale), 0, 0, [], AData.TreeView.Opacity);
// end;
// ARect.Left := ARect.Left + 16;
// end;
// inherited;
// end;
//
// { TAlphaColorPropCellData }
//
// constructor TAlphaColorPropCellData.Create;
// begin
// inherited Create(GetAlphaColorValues);
// end;
//
// function TAlphaColorPropCellData.GetColor: TAlphaColor;
// var
// AData: TPropData;
// begin
// AData := GetPropData(Node);
// Result := GetOrdProp(AData.Instance, AData.PropInfo);
// end;
//
// function TAlphaColorPropCellData.GetText: String;
// begin
// Result := inherited;
// if Result.StartsWith('cla') then
// Result := PChar(Result) + 3;
// end;
//
// procedure TAlphaColorPropCellData.SetText(const S: String);
// begin
// inherited SetText('cla' + S);
// end;
//
// { TBooleanPropCellData }
//
// function TBooleanPropCellData.GetCheckBounds: TRectF;
// begin
// Result := FBounds;
// end;
//
// function TBooleanPropCellData.GetCheckStates: TQVTCheckStates;
// var
// AData: TPropData;
// begin
// Result := [TQVTCheckState.csEnabled];
// AData := GetPropData(Node);
// if GetOrdProp(AData.Instance, AData.PropInfo) <> 0 then
// Result := Result + [TQVTCheckState.csChecked];
// end;
//
// function TBooleanPropCellData.GetFollowStates: Boolean;
// begin
// Result := False;
// end;
//
// function TBooleanPropCellData.GetItems(AList: TStrings): Integer;
// begin
// AList.Add('True');
// AList.Add('False');
// Result := 2;
// end;
//
// procedure TBooleanPropCellData.SetCheckBounds(const R: TRectF);
// begin
// FBounds := R;
// end;
//
// procedure TBooleanPropCellData.SetCheckStates(AStates: TQVTCheckStates);
// var
// AData: TPropData;
// begin
// AData := GetPropData(Node);
// if TQVTCheckState.csChecked in AStates then
// SetOrdProp(AData.Instance, AData.PropInfo, 1)
// else
// SetOrdProp(AData.Instance, AData.PropInfo, 0);
// end;
//
// procedure TBooleanPropCellData.SetFollowStates(const value: Boolean);
// begin
// // Ignore
// end;
//
// procedure TBooleanPropCellData.SetText(const S: String);
// var
// V: String;
// AData: TPropData;
// begin
// V := LowerCase(S);
// AData := GetPropData(Node);
// if Assigned(AData) then
// begin
// if V = 'false' then
// SetOrdProp(AData.Instance, AData.PropInfo, 0)
// else if V = 'true' then
// SetOrdProp(AData.Instance, AData.PropInfo, 1);
// end;
// end;
//
// { TSetPropItemCellData }
//
// function TSetPropItemCellData.GetCheckStates: TQVTCheckStates;
// var
// AData: TPropData;
// ASet: TIntegerSet;
// begin
// Result := [TQVTCheckState.csEnabled];
// AData := GetPropData(Node);
// Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
// if Node.Index in ASet then
// Result := Result + [TQVTCheckState.csChecked];
// end;
//
// function TSetPropItemCellData.GetText: String;
// var
// AData: TPropData;
// ASet: TIntegerSet;
// begin
// AData := GetPropData(Node);
// Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
// if Node.Index in ASet then
// Result := 'True'
// else
// Result := 'False';
// end;
//
// procedure TSetPropItemCellData.SetCheckStates(AStates: TQVTCheckStates);
// var
// AData: TPropData;
// ASet: TIntegerSet;
// begin
// AData := GetPropData(Node.Parent);
// Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
// ASet := ASet + [Node.Index];
// SetOrdProp(AData.Instance, AData.PropInfo, Integer(ASet));
// end;
//
// procedure TSetPropItemCellData.SetText(const S: String);
// var
// AData: TPropData;
// ASet: TIntegerSet;
// begin
// AData := GetPropData(Node);
// Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
// if S = 'True' then
// ASet := ASet + [Node.Index]
// else
// ASet := ASet - [Node.Index];
// SetOrdProp(AData.Instance, AData.PropInfo, Integer(ASet));
// end;
//
// { TSetPropItemNameData }
//
// function TSetPropItemNameData.GetText: String;
// var
// AData: TPropData;
// begin
// AData := GetPropData(Node);
// Result := GetEnumName(GetSetEnumType(AData.PropInfo.PropType^), Node.Index);
// end;
//
// { TEnumPropCellData }
//
// function TEnumPropCellData.GetItems(AList: TStrings): Integer;
// var
// I: Integer;
// AData: TPropData;
// ATypeData: PTypeData;
// begin
// AData := GetPropData(Node);
// ATypeData := AData.PropInfo.PropType^.TypeData;
// if ATypeData.MinValue < 0 then // longbool/wordbool/bytebool
// begin
// AList.Add(GetEnumName(AData.PropInfo.PropType^, 0));
// AList.Add(GetEnumName(AData.PropInfo.PropType^, 1));
// Result := 2;
// end
// else
// begin
// Result := ATypeData.MaxValue - ATypeData.MinValue + 1;
// for I := ATypeData.MinValue to ATypeData.MaxValue do
// AList.Add(GetEnumName(AData.PropInfo.PropType^, I));
// end;
// end;
//
// procedure TEnumPropCellData.SetText(const S: String);
// var
// AData: TPropData;
// begin
// AData := GetPropData(Node);
// SetEnumProp(AData.Instance, AData.PropInfo, S);
// end;
//
// { TIdentPropCellData }
//
// constructor TIdentPropCellData.Create(AGetValuesProc: TGetValuesProc);
// begin
// inherited Create;
// FGetValuesProc := AGetValuesProc;
// end;
//
// procedure TIdentPropCellData.DoEnumItem(const S: string);
// begin
// FItems.Add(S);
// Inc(FCount);
// end;
//
// function TIdentPropCellData.GetItems(AList: TStrings): Integer;
// var
// AIntToIdent: TIntToIdent;
// AData: TPropData;
// begin
// FItems := AList;
// FCount := 0;
// FGetValuesProc(DoEnumItem);
// Result := FCount;
// FItems := nil;
// end;
//
// procedure TIdentPropCellData.SetText(const S: String);
// var
// AIdentToInt: TIdentToInt;
// AData: TPropData;
// Val: Integer;
// begin
// AData := GetPropData(Node);
// AIdentToInt := FindIdentToInt(AData.PropInfo.PropType^);
// if Assigned(AIdentToInt) and AIdentToInt(S, Val) then
// SetOrdProp(AData.Instance, AData.PropInfo, Val);
// end;
//
// { TCursorPropCellData }
//
// constructor TCursorPropCellData.Create;
// begin
// inherited Create(TGetValuesProc(@GetCursorValues));
// end;
//
// { TFontNamePropCellData }
// {$IFDEF MSWINDOWS}
//
// function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
// FontType: Integer; Data: Pointer): Integer; stdcall;
// var
// S: TStrings;
// Temp: string;
// begin
// S := TStrings(Data);
// Temp := LogFont.lfFaceName;
// if (S.Count = 0) or (AnsiCompareText(S[S.Count - 1], Temp) <> 0) then
// S.Add(Temp);
// Result := 1;
// end;
// {$ENDIF}
//
// function TFontNamePropCellData.GetItems(AList: TStrings): Integer;
// var
// AFonts: TStringList;
// procedure EnumFonts;
// {$IFDEF MSWINDOWS}
// var
// DC: HDC;
// LFont: TLogFont;
// begin
// DC := GetDC(0);
// try
// FillChar(LFont, sizeof(LFont), 0);
// LFont.lfCharset := DEFAULT_CHARSET;
// EnumFontFamiliesEx(DC, LFont, @EnumFontsProc,
// Winapi.Windows.LPARAM(AFonts), 0);
// finally
// ReleaseDC(0, DC);
// end;
// end;
// {$ELSE}
// {$IFDEF LINUX}
//
// var
// I, ACount: Integer;
// begin
// ACount := FmuxGetFontCount;
// for I := 0 to ACount - 1 do
// AFonts.Add(FmuxGetFontName(I));
// end;
// {$ENDIF}
// {$ENDIF}
//
// begin
// Result := 0;
// AFonts := TStringList.Create;
// AFonts.BeginUpdate;
// try
// EnumFonts;
// AFonts.Sorted := true;
// AList.AddStrings(AFonts);
// Result := AFonts.Count;
// finally
// FreeAndNil(AFonts);
// end;
// end;
//
// { TModalResultPropCellData }
// const
// ModalResults: array [mrNone .. mrYesToAll] of string = ('mrNone', 'mrOk',
// 'mrCancel', 'mrAbort', 'mrRetry', 'mrIgnore', 'mrYes', 'mrNo', 'mrClose',
// 'mrHelp', 'mrTryAgain', 'mrContinue', 'mrAll', 'mrNoToAll', 'mrYesToAll');
//
// function TModalResultPropCellData.GetItems(AList: TStrings): Integer;
// var
// I: Integer;
// begin
// for I := Low(ModalResults) to High(ModalResults) do
// AList.Add(ModalResults[I]);
// Result := Length(ModalResults);
// end;
//
// function TModalResultPropCellData.GetText: String;
// var
// AData: TPropData;
// begin
// AData := GetPropData(Node);
// Result := ModalResults[GetOrdProp(AData.Instance, AData.PropInfo)];
// end;
//
// procedure TModalResultPropCellData.SetText(const S: String);
// var
// AData: TPropData;
// I: Integer;
// begin
// AData := GetPropData(Node);
// for I := Low(ModalResults) to High(ModalResults) do
// begin
// if CompareText(ModalResults[I], S) = 0 then
// begin
// SetOrdProp(AData.Instance, AData.PropInfo, I);
// Break;
// end;
// end;
// end;

{ TQObjectProperties }

constructor TQObjectProperties.Create(AInstance: Pointer);
var
  I, J,C: Integer;
begin
  inherited Create;
  FItems := TList<TQBaseProperty>.Create(TComparer<TQBaseProperty>.Construct(
    function(const V1, V2: TQBaseProperty): Integer
    begin
      Result := CompareText(V1.FProp.NameFld.ToString,
        V2.FProp.NameFld.ToString);
    end));
  C := GetPropList(TObject(AInstance), FProps);
  J := 0;
  try
    for I := 0 to C - 1 do
    begin
      // 将不支持的属性放到最后
      case FProps[I].PropType^.Kind of
        tkMethod,tkMethod, tkInterface, tkVariant,
        tkArray, tkDynArray, tkClassRef, tkPointer, tkProcedure:
          ;
        tkClass:
          FItems.Add(TQObjectProperties.Create(GetOrdProp(AInstance,FProps[I])));
        tkRecord
      end;
      if not(FProps[I].PropType^.Kind in [) then
      begin
        FItems.Add()
        FPropList[J] := APropList[I];
        Inc(J);
      end;
    end;
    SetLength(FPropList, J);
    SortProps;
  finally
    FreeMem(APropList);
  end;
end;

function TQObjectProperties.GetCount: Integer;
begin

end;

function TQObjectProperties.GetItems(const AIndex: Integer): TQBaseProperty;
begin
  Result := FItems[AIndex];
end;

initialization

finalization

end.
