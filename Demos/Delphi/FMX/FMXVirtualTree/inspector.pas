unit inspector;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.TypInfo, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, qdac_fmx_virtualtree,
  fmxvteditors;

type
  TfrmObjInspector = class(TForm)
    SpeedButton1: TSpeedButton;
    Layout2: TLayout;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FPropView: TQVirtualTreeView;
    FTextCellData, FColorCellData, FCheckCellData, FSetItemCellData,
      FSetItemNameCellData: IQVTCellData;
    FColorCellDrawer: IQVTDrawer;
    procedure DoInitNode(ASender: TQVirtualTreeView; ANode: TQVTNode);
    procedure DoInitChildren(ASender: TQVirtualTreeView; ANode: TQVTNode);
    procedure DoGetCellData(ASender: TQVirtualTreeView; ANode: TQVTNode;
      AColumn: Integer; var AData: IQVTCellData);
    procedure DoGetCellDrawer(ASender: TQVirtualTreeView; ANode: TQVTNode;
      AColumn: Integer; var ADrawer: IQVTDrawer);
    procedure DoGetCellEditor(Sender: TQVirtualTreeView; ANode: TQVTNode;
      ACol: Integer; var AEditor: IQVTInplaceEditor);
    procedure DoCellClick(ASender: TQVirtualTreeView; ANode: TQVTNode;
      AColumn: Integer; APos: TPointF);
  public
    { Public declarations }
  end;

var
  frmObjInspector: TfrmObjInspector;
procedure RegisterTypeEditor(AType: PTypeInfo; ACellData: IQVTCellData;
  ADrawer: IQVTDrawer; AEditorClass: TQVTCellEditorClass);
function FindTypeEditorClass(AType: PTypeInfo): TQVTCellEditorClass;

implementation

uses System.Generics.Collections{$IFDEF MSWINDOWS},
  Winapi.Windows{$ENDIF}{$IFDEF LINUX}, FMUX.Api{$ENDIF};
{$R *.fmx}

type
  IColorCellData = interface
    ['{9BC415E3-19A2-4164-9271-88B36498FB9D}']
    function GetColor: TAlphaColor;
    property Color: TAlphaColor read GetColor;
  end;

  TTextPropCellData = class(TQVTDefaultCellData, IQVTTextCellData,
    IQVTEditableTextCellData)
  protected
    function GetText: String; virtual;
    procedure SetText(const S: String); virtual;
  end;

  TBooleanPropCellData = class(TTextPropCellData, IQVTCheckCellData,
    IQVTListCellData, IQVTEditableTextCellData)
  protected
    FBounds: TRectF;
    function GetCheckStates: TQVTCheckStates; virtual;
    procedure SetCheckStates(AStates: TQVTCheckStates); virtual;
    function GetCheckBounds: TRectF; virtual;
    procedure SetCheckBounds(const R: TRectF); virtual;
    // 是否跟随直属子结点或父结点变更状态
    function GetFollowStates: Boolean; virtual;
    procedure SetFollowStates(const value: Boolean); virtual;
    procedure SetText(const S: String); override;
    function GetItems(AList: TStrings): Integer;
  end;

  TEnumPropCellData = class(TTextPropCellData, IQVTListCellData,
    IQVTEditableTextCellData)
  protected
    function GetItems(AList: TStrings): Integer;
    procedure SetText(const S: String);
  end;

  TSetPropItemNameData = class(TTextPropCellData)
  protected
    function GetText: String; override;
  end;

  TSetPropItemCellData = class(TBooleanPropCellData)
  protected
    function GetText: String; override;
    procedure SetText(const S: String); override;
    function GetCheckStates: TQVTCheckStates; override;
    procedure SetCheckStates(AStates: TQVTCheckStates); override;
  end;

  TGetValuesProc = procedure(AProc: TGetStrProc);

  TIdentPropCellData = class(TTextPropCellData, IQVTListCellData,
    IQVTEditableTextCellData)
  protected
    FItems: TStrings;
    FGetValuesProc: TGetValuesProc;
    FCount: Integer;
    procedure DoEnumItem(const S: string);
    function GetItems(AList: TStrings): Integer; virtual;
    procedure SetText(const S: String); virtual;
  public
    constructor Create(AGetValuesProc: TGetValuesProc);
  end;

  TCursorPropCellData = class(TIdentPropCellData)
  protected
  public
    constructor Create;
  end;

  TAlphaColorPropCellData = class(TIdentPropCellData, IQVTTextCellData,
    IColorCellData)
  protected
    function GetText: String; override;
    procedure SetText(const S: String); override;
    function GetColor: TAlphaColor;
  public
    constructor Create;
  end;

  TFontNamePropCellData = class(TTextPropCellData, IQVTListCellData,
    IQVTEditableTextCellData)
  protected
    function GetItems(AList: TStrings): Integer;
  end;

  TModalResultPropCellData = class(TTextPropCellData, IQVTListCellData,
    IQVTEditableTextCellData)
  protected
    function GetItems(AList: TStrings): Integer;
    function GetText: String; override;
    procedure SetText(const S: String); override;
  end;

  TObjectPropData = class;

  TPropData = class(TInterfacedObject)
  protected
    FInstance: Pointer;
    FParent: TObjectPropData;
    FProp: PPropInfo;
    FValueData: IQVTCellData;
    FNameData: IQVTCellData;
    FValueDrawer: IQVTDrawer;
  public
    constructor Create(AInstance: Pointer; AProp: PPropInfo); overload;
    property Instance: Pointer read FInstance;
    property PropInfo: PPropInfo read FProp;
    property Parent: TObjectPropData read FParent;
    property NameData: IQVTCellData read FNameData write FNameData;
    property ValueData: IQVTCellData read FValueData write FValueData;
    property ValueDrawer: IQVTDrawer read FValueDrawer write FValueDrawer;
  end;

  TObjectPropData = class(TPropData)
  protected
    FPropList: TArray<PPropInfo>;
    function GetProps(AIndex: Integer): PPropInfo;
    procedure SortProps;
    function GetPropCount: Integer;
  public
    constructor Create(AInstance: Pointer; AProp: PPropInfo); overload;
    destructor Destroy; override;
    property PropCount: Integer read GetPropCount;
    property Props[AIndex: Integer]: PPropInfo read GetProps;
  end;

  TColorCellDrawer = class(TQVTTextDrawer)
  protected
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  end;

  TTypeCellData = record
    Data: IQVTCellData;
    Drawer: IQVTDrawer;
    EditorClass: TQVTCellEditorClass;
  end;

var
  _TypeEditors: TDictionary < PTypeInfo, TTypeCellData >= nil;

function TypeEditors: TDictionary<PTypeInfo, TTypeCellData>; inline;
begin
  if not Assigned(_TypeEditors) then
    _TypeEditors := TDictionary<PTypeInfo, TTypeCellData>.Create;
  Result := _TypeEditors;
end;

procedure RegisterTypeEditor(AType: PTypeInfo; ACellData: IQVTCellData;
  ADrawer: IQVTDrawer; AEditorClass: TQVTCellEditorClass);
var
  ARec: TTypeCellData;
begin
  ARec.Data := ACellData;
  ARec.Drawer := ADrawer;
  ARec.EditorClass := AEditorClass;
  TypeEditors.AddOrSetValue(AType, ARec);
end;

function FindTypeCellData(AType: PTypeInfo; var AData: TTypeCellData): Boolean;
begin
  Result := TypeEditors.TryGetValue(AType, AData);
end;

function FindTypeEditorClass(AType: PTypeInfo): TQVTCellEditorClass;
var
  ARec: TTypeCellData;
begin
  if TypeEditors.TryGetValue(AType, ARec) then
    Result := ARec.EditorClass
  else
    Result := nil;
end;

procedure FreeTypeEditorDictionary;
begin
  if Assigned(_TypeEditors) then
    FreeAndNil(_TypeEditors);
end;

function GetObjectPropData(ANode: TQVTNode): TObjectPropData;
var
  I: Integer;
begin
  for I := 0 to ANode.Exts.Count - 1 do
  begin
    if ANode.Exts[I] is TObjectPropData then
    begin
      Result := ANode.Exts[I] as TObjectPropData;
      Exit;
    end;
  end;
  Result := nil;
end;

function GetPropData(ANode: TQVTNode): TPropData;
var
  I: Integer;
begin
  for I := 0 to ANode.Exts.Count - 1 do
  begin
    if ANode.Exts[I] is TPropData then
    begin
      Result := ANode.Exts[I] as TPropData;
      Exit;
    end;
  end;
  Result := nil;
end;

function GetSetEnumType(ATypeInfo: PTypeInfo): PTypeInfo;
begin
  Result := GetTypeData(ATypeInfo.TypeData.CompType^).BaseType^;
  // GetTypeData(AType.TypeData.CompType^)^.BaseType^
end;

procedure TfrmObjInspector.DoCellClick(ASender: TQVirtualTreeView;
  ANode: TQVTNode; AColumn: Integer; APos: TPointF);
begin

end;

procedure TfrmObjInspector.DoGetCellData(ASender: TQVirtualTreeView;
  ANode: TQVTNode; AColumn: Integer; var AData: IQVTCellData);
var
  APropData: TPropData;
begin
  APropData := GetPropData(ANode);
  if Assigned(APropData) then
  begin
    if AColumn = 0 then
      AData := APropData.NameData
    else if (AColumn = 1) then
      AData := APropData.ValueData
  end;
  if not Assigned(AData) then
    AData := FTextCellData;
  AData.Node := ANode;
  AData.Column := AColumn;
end;

procedure TfrmObjInspector.DoGetCellDrawer(ASender: TQVirtualTreeView;
  ANode: TQVTNode; AColumn: Integer; var ADrawer: IQVTDrawer);
var
  APropData: TPropData;
begin
  if (AColumn = 1) then
  begin
    APropData := GetPropData(ANode);
    if Assigned(APropData) then
      ADrawer := APropData.ValueDrawer;
  end;
end;

procedure TfrmObjInspector.DoGetCellEditor(Sender: TQVirtualTreeView;
  ANode: TQVTNode; ACol: Integer; var AEditor: IQVTInplaceEditor);
var
  AData: TPropData;
  AClass: TQVTCellEditorClass;
begin
  if ACol = 1 then
  begin
    AData := GetPropData(ANode);
    AClass := FindTypeEditorClass(AData.PropInfo.PropType^);
    if Assigned(AClass) then
      AEditor := AClass.Create
    else
    begin
      case AData.PropInfo.PropType^.Kind of
        tkUString, tkLString, tkInt64, tkFloat, tkChar, tkWChar{$IFNDEF NEXTGEN}
          , tkString, tkWString{$ENDIF !NEXTGEN}:
          AEditor := TQVTTextEditor.Create;
        tkInteger:
          begin
            if AData.ValueData is TIdentPropCellData then
              AEditor := TQVTListEditor.Create
            else
              AEditor := TQVTTextEditor.Create;
          end;
        tkEnumeration:
          AEditor := TQVTListEditor.Create;
        tkSet:
          if GetPropData(ANode.Parent).PropInfo = AData.PropInfo then
            AEditor := TQVTListEditor.Create;
      end;
    end;
  end;
end;

procedure TfrmObjInspector.DoInitChildren(ASender: TQVirtualTreeView;
  ANode: TQVTNode);
var
  AData: TPropData;
  function GetEnumValueCount(AType: PTypeInfo): Integer;
  var
    AEnumType: PTypeData;
    P: PByte;
  begin
    AEnumType := GetTypeData(GetSetEnumType(AType));
    Result := AEnumType.MaxValue - AEnumType.MinValue + 1;
  end;

begin
  AData := GetPropData(ANode);
  if Assigned(AData) then
  begin
    if AData.PropInfo.PropType^.Kind = tkSet then
      ANode.ChildCount := GetEnumValueCount(AData.PropInfo.PropType^)
    else if AData is TObjectPropData then
      ANode.ChildCount := TObjectPropData(AData).PropCount;
  end;
end;

procedure TfrmObjInspector.DoInitNode(ASender: TQVirtualTreeView;
  ANode: TQVTNode);
var
  AParentData: TObjectPropData;
  APropData: TPropData;
  AProp: PPropInfo;
  AObj: Pointer;
  ATypeCellData: TTypeCellData;
begin
  AParentData := GetObjectPropData(ANode.Parent);
  if Assigned(AParentData) then
  begin
    AProp := AParentData.Props[ANode.Index];
    if AProp.PropType^.Kind = tkClass then
    begin
      AObj := Pointer(GetOrdProp(AParentData.Instance, AProp));
      if Assigned(AObj) then
      begin
        APropData := TObjectPropData.Create(AObj, AProp);
        if TObjectPropData(APropData).PropCount > 0 then
          ANode.States := ANode.States + [TQVTNodeState.nsHasChildren];
      end
      else
        APropData := TPropData.Create(nil, AProp);
    end
    else
    begin
      APropData := TPropData.Create(AParentData.Instance, AProp);
      if AProp.PropType^.Kind = tkSet then
        ANode.States := ANode.States + [TQVTNodeState.nsHasChildren];
    end;
    APropData.FParent := AParentData;
    ANode.Exts.Add(APropData);
    if FindTypeCellData(APropData.PropInfo.PropType^, ATypeCellData) then
    begin
      APropData.ValueData := ATypeCellData.Data;
      APropData.ValueDrawer := ATypeCellData.Drawer;
    end
    else if APropData.PropInfo.PropType^.Kind = tkEnumeration then
      APropData.ValueData := TEnumPropCellData.Create;
  end
  else // tkSet 子项
  begin
    APropData := GetPropData(ANode.Parent);
    if APropData.PropInfo.PropType^.Kind = tkSet then
    begin
      APropData := TPropData.Create(APropData.Instance, APropData.PropInfo);
      APropData.NameData := FSetItemNameCellData;
      APropData.ValueData := TSetPropItemCellData.Create;
      APropData.ValueDrawer := TQVirtualTreeView.GetDefaultDrawer
        (TQVTDrawerType.dtCheck);
      ANode.Exts.Add(APropData);
    end;
  end;
end;

procedure TfrmObjInspector.FormCreate(Sender: TObject);
var
  APropData: TObjectPropData;
begin
  FPropView := TQVirtualTreeView.Create(Self);
  FPropView.Parent := Layout1;
  FPropView.Align := TAlignLayout.Client;
  FPropView.Header.Columns.Add.Width := 100;
  FPropView.Header.Columns.Add;
  FPropView.Header.AutoSizeColumn := 1;
  FPropView.TextSettings.WordWrap := False;
  FPropView.Options := [TQVTOption.toEditable];
  FPropView.PaintOptions := [TQVTPaintOption.poHorizLine,
    TQVTPaintOption.poVertLine, TQVTPaintOption.poNodeButton,
    TQVTPaintOption.poRowSelection];
  FPropView.Header.Options := FPropView.Header.Options +
    [TQVTHeaderOption.hoResizable];
  FPropView.OnInitNode := DoInitNode;
  FPropView.OnInitChildren := DoInitChildren;
  FPropView.OnGetCellData := DoGetCellData;
  FPropView.OnGetCellDrawer := DoGetCellDrawer;
  FPropView.OnGetCellEditor := DoGetCellEditor;
  // 我们来查看和修改SpeedButton1的属性
  APropData := TObjectPropData.Create(SpeedButton1, nil);
  FPropView.RootNode.Exts.Add(APropData);
  FPropView.RootNodeCount := APropData.PropCount;
  FTextCellData := TTextPropCellData.Create();
  FColorCellData := TAlphaColorPropCellData.Create;
  FSetItemNameCellData := TSetPropItemNameData.Create;
  FSetItemCellData := TSetPropItemCellData.Create;

  FColorCellDrawer := TColorCellDrawer.Create;
end;

{ TObjectPropData }

constructor TObjectPropData.Create(AInstance: Pointer; AProp: PPropInfo);
var
  I, J, C: Integer;
  APropList: PPropList;
begin
  inherited Create(AInstance, AProp);
  C := GetPropList(TObject(AInstance), APropList);
  J := 0;
  SetLength(FPropList, C);
  try
    for I := 0 to C - 1 do
    begin
      // 将不支持的属性放到最后
      if not(APropList[I].PropType^.Kind in [tkMethod, tkInterface, tkVariant,
        tkArray, tkDynArray, tkClassRef, tkPointer, tkProcedure]) then
      begin
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

destructor TObjectPropData.Destroy;
begin
  inherited;
end;

function TObjectPropData.GetPropCount: Integer;
begin
  Result := Length(FPropList);
end;

function TObjectPropData.GetProps(AIndex: Integer): PPropInfo;
begin
  Result := FPropList[AIndex];
end;

procedure TObjectPropData.SortProps;
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    pivot, temp: PPropInfo;
  begin
    if (Length(FPropList) = 0) or ((R - L) <= 0) then
      Exit;
    repeat
      I := L;
      J := R;
      pivot := FPropList[L + (R - L) shr 1];
      repeat
        while CompareText(FPropList[I].NameFld.ToString,
          pivot.NameFld.ToString) < 0 do
          Inc(I);
        while CompareText(FPropList[J].NameFld.ToString,
          pivot.NameFld.ToString) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            temp := FPropList[I];
            FPropList[I] := FPropList[J];
            FPropList[J] := temp;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  QuickSort(0, High(FPropList));
end;

{ TPropData }

constructor TPropData.Create(AInstance: Pointer; AProp: PPropInfo);
begin
  inherited Create;
  FInstance := AInstance;
  FProp := AProp;
end;

{ TTextPropCellData }

function TTextPropCellData.GetText: String;
var
  AData: TPropData;
  function PropValueText: String;
  var
    AObj: TObject;
    AIntToIdent: TIntToIdent;
  begin
    case AData.PropInfo.PropType^.Kind of
      tkInteger:
        begin
          AIntToIdent := FindIntToIdent(AData.PropInfo.PropType^);
          if Assigned(AIntToIdent) then
          begin
            if not AIntToIdent(GetOrdProp(AData.Instance, AData.PropInfo),
              Result) then
              Result := IntToHex(GetOrdProp(AData.Instance, AData.PropInfo),
                SizeOf(Pointer) shl 1);
          end
          else
            Result := IntToStr(GetOrdProp(AData.Instance, AData.PropInfo));
        end;
      tkChar, tkWChar:
        Result := WideChar(GetOrdProp(AData.Instance, AData.PropInfo));
      tkClass:
        begin
          AObj := AData.Instance;
          if Assigned(AObj) then
            Result := AObj.ToString
          else
            Result := '(Null)';
        end;
      tkEnumeration:
        if GetTypeData(AData.PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean)
        then
        begin
          case Boolean(GetOrdProp(AData.Instance, AData.PropInfo)) of
            false:
              Result := 'False';
            true:
              Result := 'True';
          end;
        end
        else
          Result := GetEnumProp(AData.Instance, AData.PropInfo);
      tkSet:
        Result := '[' + GetSetProp(AData.Instance, AData.PropInfo) + ']';
      tkFloat:
        Result := FloatToStr(GetFloatProp(AData.Instance, AData.PropInfo));
      tkMethod:
        Result := GetTypeName(AData.PropInfo^.PropType^);
      tkLString, tkUString
{$IFNDEF NEXTGEN}
        , tkString, tkWString{$ENDIF !NEXTGEN}:
        Result := GetStrProp(AData.Instance, AData.PropInfo);
      tkVariant:
        Result := VarToStr(GetVariantProp(AData.Instance, AData.PropInfo));
      tkInt64:
        Result := IntToStr(GetInt64Prop(AData.Instance, AData.PropInfo))
    else
      Result := '<Unsupport Type>';
    end;
  end;

begin
  AData := GetPropData(Node);
  if not Assigned(AData) then
  begin
    AData := GetPropData(Node.Parent);
    if AData.PropInfo.PropType^.Kind = tkSet then
      Result := '';
    // Result := GetEnumName(AData.PropInfo.PropType^.TypeData.BaseType^);
  end
  else
  begin
    case Column of
      0:
        Result := AData.PropInfo.NameFld.ToString;
      1:
        Result := PropValueText;
    end;
  end;
end;

procedure TTextPropCellData.SetText(const S: String);
var
  AData: TPropData;
begin
  AData := GetPropData(Node);
  case AData.PropInfo.PropType^.Kind of
    tkUString, tkLString{$IFNDEF NEXTGEN}
      , tkString, tkWString{$ENDIF !NEXTGEN}:
      SetStrProp(AData.Instance, AData.PropInfo, S);
    tkInteger:
      SetOrdProp(AData.Instance, AData.PropInfo, StrToInt(S));
    tkChar, tkWChar:
      begin
        if Length(S) = 1 then
          SetOrdProp(AData.Instance, AData.PropInfo, Ord(PChar(S)^));
      end;
    tkFloat:
      SetFloatProp(AData.Instance, AData.PropInfo, StrToFloat(S));
    tkInt64:
      SetInt64Prop(AData.Instance, AData.PropInfo, StrToInt64(S));
  end;
end;

{ TColorCellDrawer }

procedure TColorCellDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  R: TRectF;
  AColorData: IColorCellData;
begin
  if Supports(AData, IColorCellData, AColorData) then
  begin
    R.Left := ARect.Left + 2;
    R.Right := ARect.Left + 14;
    R.Top := (ARect.Top + ARect.Bottom) / 2 - 6;
    R.Bottom := R.Top + 12;
    with AData.TreeView.Canvas do
    begin
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := AColorData.Color;
      FillRect(R.SnapToPixel(Scale), 0, 0, [], AData.TreeView.Opacity);
      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := TAlphaColors.Black;
      DrawRect(R.SnapToPixel(Scale), 0, 0, [], AData.TreeView.Opacity);
    end;
    ARect.Left := ARect.Left + 16;
  end;
  inherited;
end;

{ TAlphaColorPropCellData }

constructor TAlphaColorPropCellData.Create;
begin
  inherited Create(GetAlphaColorValues);
end;

function TAlphaColorPropCellData.GetColor: TAlphaColor;
var
  AData: TPropData;
begin
  AData := GetPropData(Node);
  Result := GetOrdProp(AData.Instance, AData.PropInfo);
end;

function TAlphaColorPropCellData.GetText: String;
begin
  Result := inherited;
  if Result.StartsWith('cla') then
    Result := PChar(Result) + 3;
end;

procedure TAlphaColorPropCellData.SetText(const S: String);
begin
  inherited SetText('cla' + S);
end;

{ TBooleanPropCellData }

function TBooleanPropCellData.GetCheckBounds: TRectF;
begin
  Result := FBounds;
end;

function TBooleanPropCellData.GetCheckStates: TQVTCheckStates;
var
  AData: TPropData;
begin
  Result := [TQVTCheckState.csEnabled];
  AData := GetPropData(Node);
  if GetOrdProp(AData.Instance, AData.PropInfo) <> 0 then
    Result := Result + [TQVTCheckState.csChecked];
end;

function TBooleanPropCellData.GetFollowStates: Boolean;
begin
  Result := False;
end;

function TBooleanPropCellData.GetItems(AList: TStrings): Integer;
begin
  AList.Add('True');
  AList.Add('False');
  Result := 2;
end;

procedure TBooleanPropCellData.SetCheckBounds(const R: TRectF);
begin
  FBounds := R;
end;

procedure TBooleanPropCellData.SetCheckStates(AStates: TQVTCheckStates);
var
  AData: TPropData;
begin
  AData := GetPropData(Node);
  if TQVTCheckState.csChecked in AStates then
    SetOrdProp(AData.Instance, AData.PropInfo, 1)
  else
    SetOrdProp(AData.Instance, AData.PropInfo, 0);
end;

procedure TBooleanPropCellData.SetFollowStates(const value: Boolean);
begin
  // Ignore
end;

procedure TBooleanPropCellData.SetText(const S: String);
var
  V: String;
  AData: TPropData;
begin
  V := LowerCase(S);
  AData := GetPropData(Node);
  if Assigned(AData) then
  begin
    if V = 'false' then
      SetOrdProp(AData.Instance, AData.PropInfo, 0)
    else if V = 'true' then
      SetOrdProp(AData.Instance, AData.PropInfo, 1);
  end;
end;

{ TSetPropItemCellData }

function TSetPropItemCellData.GetCheckStates: TQVTCheckStates;
var
  AData: TPropData;
  ASet: TIntegerSet;
begin
  Result := [TQVTCheckState.csEnabled];
  AData := GetPropData(Node);
  Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
  if Node.Index in ASet then
    Result := Result + [TQVTCheckState.csChecked];
end;

function TSetPropItemCellData.GetText: String;
var
  AData: TPropData;
  ASet: TIntegerSet;
begin
  AData := GetPropData(Node);
  Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
  if Node.Index in ASet then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TSetPropItemCellData.SetCheckStates(AStates: TQVTCheckStates);
var
  AData: TPropData;
  ASet: TIntegerSet;
begin
  AData := GetPropData(Node.Parent);
  Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
  ASet := ASet + [Node.Index];
  SetOrdProp(AData.Instance, AData.PropInfo, Integer(ASet));
end;

procedure TSetPropItemCellData.SetText(const S: String);
var
  AData: TPropData;
  ASet: TIntegerSet;
begin
  AData := GetPropData(Node);
  Integer(ASet) := GetOrdProp(AData.Instance, AData.PropInfo);
  if S = 'True' then
    ASet := ASet + [Node.Index]
  else
    ASet := ASet - [Node.Index];
  SetOrdProp(AData.Instance, AData.PropInfo, Integer(ASet));
end;

{ TSetPropItemNameData }

function TSetPropItemNameData.GetText: String;
var
  AData: TPropData;
begin
  AData := GetPropData(Node);
  Result := GetEnumName(GetSetEnumType(AData.PropInfo.PropType^), Node.Index);
end;

{ TEnumPropCellData }

function TEnumPropCellData.GetItems(AList: TStrings): Integer;
var
  I: Integer;
  AData: TPropData;
  ATypeData: PTypeData;
begin
  AData := GetPropData(Node);
  ATypeData := AData.PropInfo.PropType^.TypeData;
  if ATypeData.MinValue < 0 then // longbool/wordbool/bytebool
  begin
    AList.Add(GetEnumName(AData.PropInfo.PropType^, 0));
    AList.Add(GetEnumName(AData.PropInfo.PropType^, 1));
    Result := 2;
  end
  else
  begin
    Result := ATypeData.MaxValue - ATypeData.MinValue + 1;
    for I := ATypeData.MinValue to ATypeData.MaxValue do
      AList.Add(GetEnumName(AData.PropInfo.PropType^, I));
  end;
end;

procedure TEnumPropCellData.SetText(const S: String);
var
  AData: TPropData;
begin
  AData := GetPropData(Node);
  SetEnumProp(AData.Instance, AData.PropInfo, S);
end;

{ TIdentPropCellData }

constructor TIdentPropCellData.Create(AGetValuesProc: TGetValuesProc);
begin
  inherited Create;
  FGetValuesProc := AGetValuesProc;
end;

procedure TIdentPropCellData.DoEnumItem(const S: string);
begin
  FItems.Add(S);
  Inc(FCount);
end;

function TIdentPropCellData.GetItems(AList: TStrings): Integer;
var
  AIntToIdent: TIntToIdent;
  AData: TPropData;
begin
  FItems := AList;
  FCount := 0;
  FGetValuesProc(DoEnumItem);
  Result := FCount;
  FItems := nil;
end;

procedure TIdentPropCellData.SetText(const S: String);
var
  AIdentToInt: TIdentToInt;
  AData: TPropData;
  Val: Integer;
begin
  AData := GetPropData(Node);
  AIdentToInt := FindIdentToInt(AData.PropInfo.PropType^);
  if Assigned(AIdentToInt) and AIdentToInt(S, Val) then
    SetOrdProp(AData.Instance, AData.PropInfo, Val);
end;

{ TCursorPropCellData }

constructor TCursorPropCellData.Create;
begin
  inherited Create(TGetValuesProc(@GetCursorValues));
end;

{ TFontNamePropCellData }
{$IFDEF MSWINDOWS}

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count - 1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;
{$ENDIF}

function TFontNamePropCellData.GetItems(AList: TStrings): Integer;
var
  AFonts: TStringList;
  procedure EnumFonts;
{$IFDEF MSWINDOWS}
  var
    DC: HDC;
    LFont: TLogFont;
  begin
    DC := GetDC(0);
    try
      FillChar(LFont, sizeof(LFont), 0);
      LFont.lfCharset := DEFAULT_CHARSET;
      EnumFontFamiliesEx(DC, LFont, @EnumFontsProc,
        Winapi.Windows.LPARAM(AFonts), 0);
    finally
      ReleaseDC(0, DC);
    end;
  end;
{$ELSE}
{$IFDEF LINUX}

  var
    I, ACount: Integer;
  begin
    ACount := FmuxGetFontCount;
    for I := 0 to ACount - 1 do
      AFonts.Add(FmuxGetFontName(I));
  end;
{$ENDIF}
{$ENDIF}

begin
Result := 0;
AFonts := TStringList.Create;
AFonts.BeginUpdate;
try
  EnumFonts;
  AFonts.Sorted := true;
  AList.AddStrings(AFonts);
  Result := AFonts.Count;
finally
  FreeAndNil(AFonts);
end;
end;

{ TModalResultPropCellData }
const
  ModalResults: array [mrNone .. mrYesToAll] of string = ('mrNone', 'mrOk',
    'mrCancel', 'mrAbort', 'mrRetry', 'mrIgnore', 'mrYes', 'mrNo', 'mrClose',
    'mrHelp', 'mrTryAgain', 'mrContinue', 'mrAll', 'mrNoToAll', 'mrYesToAll');

function TModalResultPropCellData.GetItems(AList: TStrings): Integer;
var
  I: Integer;
begin
  for I := Low(ModalResults) to High(ModalResults) do
    AList.Add(ModalResults[I]);
  Result := Length(ModalResults);
end;

function TModalResultPropCellData.GetText: String;
var
  AData: TPropData;
begin
  AData := GetPropData(Node);
  Result := ModalResults[GetOrdProp(AData.Instance, AData.PropInfo)];
end;

procedure TModalResultPropCellData.SetText(const S: String);
var
  AData: TPropData;
  I: Integer;
begin
  AData := GetPropData(Node);
  for I := Low(ModalResults) to High(ModalResults) do
  begin
    if CompareText(ModalResults[I], S) = 0 then
    begin
      SetOrdProp(AData.Instance, AData.PropInfo, I);
      Break;
    end;
  end;
end;

initialization

RegisterTypeEditor(TypeInfo(TFontName), TFontNamePropCellData.Create, nil,
  TQVTListEditor);
RegisterTypeEditor(TypeInfo(Boolean), TBooleanPropCellData.Create,
  TQVirtualTreeView.GetDefaultDrawer(TQVTDrawerType.dtCheck), TQVTListEditor);
RegisterTypeEditor(TypeInfo(TAlphaColor), TAlphaColorPropCellData.Create,
  TColorCellDrawer.Create, TQVTColorEditor);
RegisterTypeEditor(TypeInfo(TCursor), TCursorPropCellData.Create, nil,
  TQVTListEditor);
RegisterTypeEditor(TypeInfo(TModalResult), TModalResultPropCellData.Create, nil,
  TQVTListEditor);

finalization

FreeTypeEditorDictionary;

end.
