unit fmxvteditors;

interface

uses System.Classes, System.Types, System.UITypes, System.Sysutils,
  FMX.Controls, qdac_fmx_virtualtree;

type
  TQVTCellEditorClass = class of TQVTBaseEditor;

  TQVTBaseEditor = class(TInterfacedObject, IQVTInplaceEditor)
  protected
    FControlClass: TComponentClass;
    FControl: TControl;
    FNode: TQVTNode;
    FColumn: Integer;
    function GetControl: TControl;
    procedure SetBounds(R: TRectF); virtual;
    function BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean; virtual;
    function EndEdit: Boolean; virtual;
    procedure CancelEdit; virtual;
    function GetEditing(var ACol: Integer): TQVTNode; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    property Node: TQVTNode read FNode;
    property Column: Integer read FColumn;
    property Control: TControl read GetControl;
    constructor Create(AControlClass: TComponentClass); overload;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
  end;

  TQVTTextEditor = class(TQVTBaseEditor)
  protected
    function BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean; override;
    function EndEdit: Boolean; override;
    procedure CancelEdit; override;
  public
    constructor Create; override;
  end;

  IQVTListCellData = interface(IQVTTextCellData)
    ['{F2A7BA74-2E3C-4A4A-BAB5-CBA703BE2BFD}']
    function GetItems(AList: TStrings): Integer;
  end;

  TQVTListEditor = class(TQVTBaseEditor)
  protected
    function BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean; override;
    function EndEdit: Boolean; override;
  public
    constructor Create; override;
  end;

  TQVTColorEditor = class(TQVTBaseEditor)
  protected
    function BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean; override;
    function EndEdit: Boolean; override;
  public
    constructor Create; override;
  end;

implementation

uses FMX.Edit, FMX.ListBox, FMX.Colors;
{ TQVTBaseEditor }

function TQVTBaseEditor.BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
begin
  FNode := ANode;
  FColumn := ACol;
  Result := True;
  Control.Parent := ANode.TreeView;
end;

procedure TQVTBaseEditor.CancelEdit;
begin
  // Do Nothing
end;

constructor TQVTBaseEditor.Create;
begin
  inherited Create;
end;

constructor TQVTBaseEditor.Create(AControlClass: TComponentClass);
begin
  inherited Create;
  FControlClass := AControlClass;
end;

destructor TQVTBaseEditor.Destroy;
begin
  if Assigned(FControl) then
    FreeAndNil(FControl);
  inherited;
end;

function TQVTBaseEditor.EndEdit: Boolean;
begin
  Result := True;
end;

function TQVTBaseEditor.GetControl: TControl;
begin
  if not Assigned(FControl) then
  begin
    FControl := FControlClass.Create(nil) as TControl;
    FControl.Parent := Node.TreeView;
  end;
  Result := FControl;
end;

function TQVTBaseEditor.GetEditing(var ACol: Integer): TQVTNode;
begin
  ACol := FColumn;
  Result := FNode;
end;

procedure TQVTBaseEditor.Hide;
begin
  if Assigned(FControl) then
    FControl.Visible := False;
end;

procedure TQVTBaseEditor.SetBounds(R: TRectF);
begin
  Control.SetBounds(R.Left, R.Top, R.Width, R.Height);
end;

procedure TQVTBaseEditor.Show;
begin
  Control.Visible := True;
  Control.SetFocus;
end;

{ TQVTTextEditor }

function TQVTTextEditor.BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
var
  AData: IQVTEditableTextCellData;
begin
  if Supports(ANode.CellData[ACol], IQVTTextCellData, AData) then
  begin
    inherited;
    (Control as TCustomEdit).Text := AData.Text;
    Result := True;
  end
  else
    Result := false;
end;

procedure TQVTTextEditor.CancelEdit;
begin
  (Control as TEdit).Text := '';
end;

constructor TQVTTextEditor.Create;
begin
  inherited Create(TEdit);
end;

function TQVTTextEditor.EndEdit: Boolean;
var
  AData: IQVTEditableTextCellData;
begin
  Result := True;
  if Supports(Node.CellData[Column], IQVTEditableTextCellData, AData) then
  begin
    AData.Text := (Control as TCustomEdit).Text;
    Hide;
  end;
end;

{ TQVTEnumEditor }

function TQVTListEditor.BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
var
  AData: IQVTListCellData;
begin
  inherited;
  if Supports(ANode.CellData[ACol], IQVTListCellData, AData) then
  begin
    with Control as TComboBox do
    begin
      Items.BeginUpdate;
      try
        Items.Clear;
        if AData.GetItems(Items) > 0 then
          ItemIndex := Items.IndexOf(AData.Text);
      finally
        Items.EndUpdate;
      end;
    end;
    Result := True;
  end
  else
    Result := false;
end;

constructor TQVTListEditor.Create;
begin
  inherited Create(TComboBox);
end;

function TQVTListEditor.EndEdit: Boolean;
var
  AData: IQVTEditableTextCellData;
begin
  Result := True;
  if Supports(Node.CellData[Column], IQVTEditableTextCellData, AData) then
  begin
    with Control as TComboBox do
    begin
      if ItemIndex <> -1 then
        AData.Text := Items[ItemIndex];
    end;
    Hide;
  end;
end;

{ TQVTColorEditor }

function TQVTColorEditor.BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
var
  AData: IQVTTextCellData;
begin
  inherited;
  if Supports(ANode.CellData[ACol], IQVTTextCellData, AData) then
  begin
    with Control as TColorComboBox do
      ItemIndex := Items.IndexOf(AData.Text);
    Result := True;
  end
  else
    Result := false;
end;

constructor TQVTColorEditor.Create;
begin
  inherited Create(TColorComboBox);
end;

function TQVTColorEditor.EndEdit: Boolean;
var
  AData: IQVTEditableTextCellData;
begin
  Result := True;
  if Supports(Node.CellData[Column], IQVTEditableTextCellData, AData) then
  begin
    with Control as TColorComboBox do
    begin
      if ItemIndex <> -1 then
        AData.Text := Items[ItemIndex];
    end;
    Hide;
  end;
end;

end.
