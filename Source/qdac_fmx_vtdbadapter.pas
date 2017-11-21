unit qdac_fmx_vtdbadapter;

interface

uses System.Classes, System.Sysutils, System.Types, System.UITypes,
  System.Generics.Collections, FMX.Types, FMX.TextLayout,
  Data.Db, FMX.Graphics, qdac_fmx_virtualtree, qstring;

type
  TQDBCellData = class(TQVTDefaultCellData)
  private
    function GetDataSet: TDataSet;
  protected
    FField: TField;
  public
    constructor Create(AField: TField); overload; virtual;
    destructor Destroy; override;
    property Field: TField read FField;
    property DataSet: TDataSet read GetDataSet;
  end;

  TQDBTextCellData = class(TQDBCellData, IQVTTextCellData)
  protected
    function GetText: String;
  end;

  TQDBProgressCellData = class(TQDBCellData, IQVTTextCellData,
    IQVTProgressCellData)
  protected
    FTextSettings: TTextSettings;
    function GetProgress: Single;
    function GetText: String;
  public
    constructor Create(AField: TField); override;
    destructor Destroy; override;
  end;

  TQDBCheckCellData = class(TQDBCellData, IQVTCheckCellData, IQVTTextCellData)
  protected
    FCheckBounds: TRectF;
    function GetText: String;
    function GetCheckStates: TQVTCheckStates;
    procedure SetCheckStates(AStates: TQVTCheckStates);
    function GetFollowStates: Boolean;
    procedure SetFollowStates(const value: Boolean);
    function GetCheckBounds: TRectF;
    procedure SetCheckBounds(const R: TRectF);
  public
    constructor Create(AField: TField); override;
  end;

  TQDBRadioCellData = class(TQDBCellData, IQVTRadioCellData, IQVTTextCellData)
  protected
    FRadioBounds: TRectF;
    function GetText: String;
    function GetGroupId: Integer;
    procedure SetGroupId(const AId: Integer);
    function GetCheckStates: TQVTCheckStates;
    procedure SetCheckStates(AStates: TQVTCheckStates);
    function GetRadioLayout: TQVTLayout;
    procedure SetRadioLayout(value: TQVTLayout);
    function GetRadioBounds: TRectF;
    procedure SetRadioBounds(const R: TRectF);
  public
    constructor Create(AField: TField); override;
  end;

  TQDBTreeAdapter = class
  protected
    FDataSet: TDataSet;
    FTreeView: TQVirtualTreeView;
    FKeyField, FParentField: TField;
    FFieldCellDatas: TArray<IQVTCellData>;
    FRootList: TArray<Integer>;
    FColumnMap: TArray<Integer>;
    FDefaultTextData: IQVTCellData;
    procedure PrepareData;
    procedure DoGetCellData(Sender: TQVirtualTreeView; ANode: TQVTNode;
      ACol: Integer; var AData: IQVTCellData);
    procedure DoGetCellDrawer(Sender: TQVirtualTreeView; ANode: TQVTNode;
      ACol: Integer; var ADrawer: IQVTDrawer);
    procedure DoInitChildren(Sender: TQVirtualTreeView; ANode: TQVTNode);
    procedure DoInitNode(Sender: TQVirtualTreeView; ANode: TQVTNode);
    procedure SetTreeView(const Value: TQVirtualTreeView);
  public
    constructor Create(AKeyField, AParentField: TField); overload;
    destructor Destroy; override;
    procedure SetFieldCellType(AField: TField; AType: TQDBCellData);
    property TreeView: TQVirtualTreeView read FTreeView write SetTreeView;
  end;

implementation

type
  IDBLocation = interface
    ['{419A90FE-0FF2-4F68-B3BF-F20412C238F4}']
    procedure GotoRecord;
    function GetChildCount: Integer;
    function GetChild(const Index: Integer): IDBLocation;
  end;

  TQDBLocation = class(TInterfacedObject, IDBLocation)
  protected
    FAdapter: TQDBTreeAdapter;
    FBookmark: TBookmark;
    FChildren: TList<TQDBLocation>;
    function GetChildCount: Integer;
    function GetChild(const Index: Integer): IDBLocation;
    procedure GotoRecord;
  public
    constructor Create(Adapter: TQDBTreeAdapter); overload;
    destructor Destroy; override;
    procedure InitChildren;
  end;
  { TQDBCellData }

constructor TQDBCellData.Create(AField: TField);
begin
  inherited Create(nil, -1);
  FField := AField;
end;

destructor TQDBCellData.Destroy;
begin

  inherited;
end;

function TQDBCellData.GetDataSet: TDataSet;
begin
  if Assigned(FField) then
    Result := FField.DataSet
  else
    Result := nil;
end;

{ TQDBTextCellData }

function TQDBTextCellData.GetText: String;
begin
  Result := FField.AsString;
end;

{ TQDBProgressCellData }

constructor TQDBProgressCellData.Create(AField: TField);
begin
  inherited;
end;

destructor TQDBProgressCellData.Destroy;
begin
  FreeAndNil(FTextSettings);
  inherited;
end;

function TQDBProgressCellData.GetProgress: Single;
begin
  if FField.IsNull then
    Result := 0
  else
    Result := FField.AsFloat;
end;

function TQDBProgressCellData.GetText: String;
begin
  if FField.IsNull then
    Result := ''
  else
    Result := FormatFloat('0.00', FField.AsFloat) + '%';
end;

{ TQDBCheckCellData }

constructor TQDBCheckCellData.Create(AField: TField);
begin
  inherited;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
end;

function TQDBCheckCellData.GetCheckBounds: TRectF;
begin
  Result := FCheckBounds;
end;

function TQDBCheckCellData.GetCheckStates: TQVTCheckStates;
begin
  if FField.IsNull then
    Result := []
  else if FField.AsBoolean then
    Result := [TQVTCheckState.csChecked, TQVTCheckState.csEnabled]
  else
    Result := [TQVTCheckState.csEnabled];
end;

function TQDBCheckCellData.GetFollowStates: Boolean;
begin
  Result := True;
end;

function TQDBCheckCellData.GetText: String;
begin
  Result := '';
end;

procedure TQDBCheckCellData.SetCheckBounds(const R: TRectF);
begin
  FCheckBounds := R;
end;

procedure TQDBCheckCellData.SetCheckStates(AStates: TQVTCheckStates);
begin

end;

procedure TQDBCheckCellData.SetFollowStates(const value: Boolean);
begin

end;

{ TQDBRadioCellData }

constructor TQDBRadioCellData.Create(AField: TField);
begin
  inherited;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
end;

function TQDBRadioCellData.GetCheckStates: TQVTCheckStates;
begin
  if FField.IsNull then
    Result := []
  else if FField.AsBoolean then
    Result := [TQVTCheckState.csChecked, TQVTCheckState.csEnabled]
  else
    Result := [TQVTCheckState.csEnabled];
end;

function TQDBRadioCellData.GetGroupId: Integer;
begin
  Result := 0;
end;

function TQDBRadioCellData.GetRadioBounds: TRectF;
begin
  Result := FRadioBounds;
end;

function TQDBRadioCellData.GetRadioLayout: TQVTLayout;
begin
  Result := TQVTLayout.clLeftCenter;
end;

function TQDBRadioCellData.GetText: String;
begin
  Result := '';
end;

procedure TQDBRadioCellData.SetCheckStates(AStates: TQVTCheckStates);
begin

end;

procedure TQDBRadioCellData.SetGroupId(const AId: Integer);
begin

end;

procedure TQDBRadioCellData.SetRadioBounds(const R: TRectF);
begin
  FRadioBounds := R;
end;

procedure TQDBRadioCellData.SetRadioLayout(value: TQVTLayout);
begin

end;

{ TQDBTreeAdapter }

constructor TQDBTreeAdapter.Create(AKeyField, AParentField: TField);
begin
  inherited Create;
  FDataSet := AKeyField.DataSet;
  FKeyField := AKeyField;
  FParentField := AParentField;
  SetLength(FFieldCellDatas, FDataSet.Fields.Count);
  PrepareData;
end;

destructor TQDBTreeAdapter.Destroy;
begin
  inherited;
end;

procedure TQDBTreeAdapter.DoGetCellData(Sender: TQVirtualTreeView;
  ANode: TQVTNode; ACol: Integer; var AData: IQVTCellData);
var
  ALoc: IDBLocation;
begin
  if Supports(ANode, IDBLocation, ALoc) then
  begin
    ALoc.GotoRecord;
    AData := FFieldCellDatas[FColumnMap[ACol]];
    if not Assigned(AData) then
    begin
      AData := TQDBTextCellData.Create(FDataSet.Fields[FColumnMap[ACol]]);
      FFieldCellDatas[FColumnMap[ACol]] := AData;
    end;
    with AData as TQDBCellData do
    begin
      Node := ANode;
      Column := ACol;
    end;
  end;
end;

procedure TQDBTreeAdapter.DoGetCellDrawer(Sender: TQVirtualTreeView;
  ANode: TQVTNode; ACol: Integer; var ADrawer: IQVTDrawer);
var
  AData: IQVTCellData;
begin
  if Assigned(ANode) then
  begin
    AData := ANode.CellData[ACol];
    if Supports(AData, IQVTCheckCellData) then
      ADrawer := TQVirtualTreeView.GetDefaultDrawer(TQVTDrawerType.dtCheck)
    else if Supports(AData, IQVTRadioCellData) then
      ADrawer := TQVirtualTreeView.GetDefaultDrawer(TQVTDrawerType.dtRadio)
    else if Supports(AData, IQVTProgressCellData) then
      ADrawer := TQVirtualTreeView.GetDefaultDrawer(TQVTDrawerType.dtProgress);
  end;
end;

procedure TQDBTreeAdapter.DoInitChildren(Sender: TQVirtualTreeView;
  ANode: TQVTNode);
var
  ALocation: IDBLocation;
begin
  if Supports(ANode, IDBLocation, ALocation) then
    ANode.ChildCount := ALocation.GetChildCount
  else
    ANode.ChildCount := 0;
end;

procedure TQDBTreeAdapter.DoInitNode(Sender: TQVirtualTreeView;
  ANode: TQVTNode);
var
  ALocation: IDBLocation;
begin
  if ANode.Parent = Sender.RootNode then
  begin
    FDataSet.RecNo := FRootList[ANode.Index];
    ALocation := TQDBLocation.Create(Self);
    ANode.Exts.Add(ALocation);
    if ALocation.GetChildCount > 0 then
      ANode.States := ANode.States + [TQVTNodeState.nsHasChildren];
  end
  else if Supports(ANode.Parent, IDBLocation, ALocation) then
  begin
    ALocation := ALocation.GetChild(ANode.Index);
    ANode.Exts.Add(ALocation);
    if ALocation.GetChildCount > 0 then
      ANode.States := ANode.States + [TQVTNodeState.nsHasChildren];
  end;
end;

procedure TQDBTreeAdapter.PrepareData;
var
  AKeyList, AParentList: TDictionary<String, Integer>;
  APair: TPair<String, Integer>;
  AIdx: Integer;
  AKey: String;
begin
  AKeyList := TDictionary<String, Integer>.Create;
  AParentList := TDictionary<String, Integer>.Create;
  try
    FDataSet.DisableControls;
    try
      FDataSet.First;
      // FKeyField的值不应该有重复，此处假设不重
      while not FDataSet.Eof do
      begin
        AKeyList.Add(FKeyField.AsString, FDataSet.RecNo);
        FDataSet.Next;
      end;
      FDataSet.First;
      while not FDataSet.Eof do
      begin
        AKey := FParentField.AsString;
        if not AKeyList.ContainsKey(AKey) then
          AParentList.Add(FKeyField.AsString, FDataSet.RecNo);
        FDataSet.Next;
      end;
      SetLength(FRootList, AParentList.Count);
      AIdx := 0;
      for APair in AParentList do
      begin
        FRootList[AIdx] := APair.Value;
        Inc(AIdx);
      end;
    finally
      FDataSet.EnableControls;
    end;
  finally
    FreeAndNil(AKeyList);
    FreeAndNil(AParentList);
  end;
end;

procedure TQDBTreeAdapter.SetFieldCellType(AField: TField; AType: TQDBCellData);
var
  AIdx: Integer;
begin
  AIdx := FDataSet.Fields.IndexOf(AField);
  if AIdx <> -1 then
  begin
    FFieldCellDatas[AIdx] := AType;
  end;
end;

procedure TQDBTreeAdapter.SetTreeView(const Value: TQVirtualTreeView);
var
  I: Integer;
  AField: TField;
  ACol: TQVTColumn;
begin
  if FTreeView <> Value then
  begin
    FTreeView := Value;
    FTreeView.OnGetCellData := DoGetCellData;
    FTreeView.OnGetCellDrawer := DoGetCellDrawer;
    FTreeView.OnInitNode := DoInitNode;
    FTreeView.OnInitChildren := DoInitChildren;
    FTreeView.RootNodeCount := Length(FRootList);
    with FTreeView.Header.Columns do
    begin
      SetLength(FColumnMap, FDataSet.Fields.Count);
      BeginUpdate;
      try
        for I := 0 to FDataSet.Fields.Count - 1 do
        begin
          AField := FDataSet.Fields[I];
          if AField.Visible then
          begin
            FColumnMap[Count] := I;
            ACol := Add;
            ACol.Title.Text := AField.DisplayLabel;
          end;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

{ TQDBLocation }

constructor TQDBLocation.Create(Adapter: TQDBTreeAdapter);
begin
  inherited Create;
  FAdapter := Adapter;
  FBookmark := Adapter.FDataSet.Bookmark;
end;

destructor TQDBLocation.Destroy;
begin
  FAdapter.FDataSet.FreeBookmark(FBookmark);
  if Assigned(FChildren) then
    FreeAndNil(FChildren);
  inherited;
end;

function TQDBLocation.GetChild(const Index: Integer): IDBLocation;
begin
  InitChildren;
  Result := FChildren[Index];
end;

function TQDBLocation.GetChildCount: Integer;
begin
  InitChildren;
  Result := FChildren.Count;
end;

procedure TQDBLocation.GotoRecord;
begin
  FAdapter.FDataSet.Bookmark := FBookmark;
end;

procedure TQDBLocation.InitChildren;
begin
  if not Assigned(FChildren) then
  begin
    FChildren := TList<TQDBLocation>.Create;
    FAdapter.FDataSet.DisableControls;
    try
      GotoRecord;
      if (FAdapter.FKeyField is TStringField) or
        (FAdapter.FKeyField is TDateTimeField) then
        FAdapter.FDataSet.Filter := FAdapter.FParentField.FieldName + '=' +
          QuotedStr(FAdapter.FKeyField.AsString)
      else
        FAdapter.FDataSet.Filter := FAdapter.FParentField.FieldName + '=' +
          FAdapter.FKeyField.AsString;
      FAdapter.FDataSet.Filtered := True;
      FChildren.Capacity := FAdapter.FDataSet.RecordCount;
      FAdapter.FDataSet.First;
      while not FAdapter.FDataSet.Eof do
      begin
        FChildren.Add(TQDBLocation.Create(FAdapter));
        FAdapter.FDataSet.Next;
      end;
    finally
      FAdapter.FDataSet.Filtered := False;
      FAdapter.FDataSet.EnableControls;
    end;
  end;
end;

end.
