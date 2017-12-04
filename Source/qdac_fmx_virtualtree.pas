unit qdac_fmx_virtualtree;
{ FMX VirtualTree 是一个基于 FMX 设计优化的一种树形结构显示控件，它可以被用来当做
  普通的表格或者是树形结构使用。
  本单元将树形结构的数据管理分成几个部分：
  1、数据存贮部分，这部分由用户负责，VirtualTreeView 需要用户将数据与 VirtualTreeView 做显示适配；
  2、树形管理部分，这部分由 VirtualTreeView 负责，用户不需要特殊处理
  3、用户界面绘制部分，这部分由不同的绘制器负责，它们统一实现 IQVTDrawer 接口，来完成实际的绘制工作。


}

interface

uses System.Classes, System.Sysutils, System.Types, System.UITypes,
  System.Generics.Collections, System.RTLConsts,
  Math, FMX.TextLayout, FMX.Layouts, FMX.StdCtrls, FMX.InertialMovement,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  {
    结点的状态：
    Initialized 已经初始化完成
    Hover 当前鼠标或手指在上面移动
    MouseDown 当前鼠标或手指按在上面
    Unchecked 当前结点主列未被选中
    Checked 当前结点主列已选中
    SomeChecked 当前结点主列一部分子结点被选中了
    Selected 当前结点被选择
    Expanded 当前结点已经被展开
    Invalid 当前结点需要刷新显示
    HasChildren 当前结点拥有至少一个子结点
  }
  TQVTNode = class;
  TQVirtualTreeView = class;
  TQVTColumn = class;
  TQVTNodeState = (nsInitialized, nsVisible, nsHover, nsMouseDown, nsUnchecked,
    nsChecked, nsSomeChecked, nsSelected, nsExpanded, nsInvalid, nsHasChildren);

  TQVTNodeStates = set of TQVTNodeState;
  TQVTCheckState = (csChecked, csEnabled, csSomeChecked);
  TQVTCheckStates = set of TQVTCheckState;

  TQVTLayout = (clLeftTop, clTopCenter, clRightTop, clRightCenter,
    clRightBottom, clBottomCenter, clLeftBottom, clLeftCenter, clCenter);
  // 排序标记
  TQVTColumnSortMarker = (smNone, smAsc, smDesc);
  // 列绘制器定义
  TQVTDrawerType = (dtDefault, dtText, dtCheck, dtRadio, dtImage, dtStateText,
    dtTree, dtProgress, dtMoney, dtRowIndicator, dtHeader, dtCustom);

  IQVTDrawer = interface;
  IQVTCellData = interface;

  IQVTDrawable = interface
    ['{066AE9A9-18F1-484E-BA57-28BFF5221F06}']
    // 获取行画刷
    function GetFill: TBrush;
    // 获取行画笔
    function GetStroke: TStrokeBrush;
    property Fill: TBrush read GetFill;
    property Stroke: TStrokeBrush read GetStroke;
  end;

  IQVTTextDrawable = interface(IQVTDrawable)
    ['{84C5A8EA-F212-4CE7-B7EA-13FFFE8E7C39}']
    // 获取当前结点文本设置，如果返回空，则使用列的默认设置
    function GetTextSettings: TTextSettings;
    property TextSettings: TTextSettings read GetTextSettings;
  end;


  // 基本的单元格数据类型定义

  // 单元格数据
  IQVTCellData = interface
    ['{BBF731E7-B5A1-4783-85B7-8C76A46A289B}']
    // 列号
    function GetColumn: Integer;
    procedure SetColumn(const AColumn: Integer);
    // 关联的结点
    function GetNode: TQVTNode;
    procedure SetNode(ANode: TQVTNode);
    // 关联的树
    function GetTreeView: TQVirtualTreeView;
    // 是否处于允许状态
    function GetEnabled: Boolean;
    // 边框
    function GetSides: TSides;
    property Enabled: Boolean read GetEnabled;
    property Column: Integer read GetColumn write SetColumn;
    property Node: TQVTNode read GetNode write SetNode;
    property Sides: TSides read GetSides;
    property TreeView: TQVirtualTreeView read GetTreeView;
  end;

  IQVTProgressCellData = interface
    ['{EDEA1C95-A417-4635-8380-28A89872BB1D}']
    function GetProgress: Single;
  end;

  IQVTCurrencyCellData = interface
    ['{38E6E3AD-6383-42D1-B85F-CAC75876EEF3}']
    function GetValue: Currency;
    function GetMaxValue: Currency;
  end;

  // 带文本的单元格数据
  IQVTTextCellData = interface(IQVTCellData)
    ['{ECAB714B-3EBD-4296-8121-61D541CE8758}']
    // 获取当前结点文本内容
    function GetText: String;
    property Text: String read GetText;
  end;

  IQVTEditableTextCellData = interface(IQVTTextCellData)
    ['{40F451FF-D797-43DC-8DA2-7E43D4FF1B75}']
    procedure SetText(const S: String);
    property Text: String read GetText write SetText;
  end;

  IQVTCellLayoutData = interface
    ['{4DFBCBE6-CBF1-4E99-80CC-C7426CD00EDD}']
    function GetLayout: TQVTLayout;
    procedure SetLayout(ALayout: TQVTLayout);
    function GetStateSize: TSizeF;
    property StateSize: TSizeF read GetStateSize;
    property Layout: TQVTLayout read GetLayout write SetLayout;
  end;

  IQVTStateCellData = interface(IQVTCellLayoutData)
    ['{CE7EE186-2909-4A90-A3C6-7FB14D8866CA}']
    function GetStateSize: TSizeF;
    function StateSizable: Boolean;
  end;

  TQCellBlockData = record
    Bounds: TRectF;
    Size: TSizeF;
    DrawerType: TQVTDrawerType;
    Drawer: IQVTDrawer;
    Align: TAlignLayout;
  end;

  //
  IQVTMultiBlockCellData = interface(IQVTCellData)
    ['{5DB34749-AB5F-4705-98DC-6EF8436F6D8E}']
    function GetBlockCount: Integer;
    function GetBlockData(AIndex: Integer; var AData: TQCellBlockData): Boolean;
    procedure SetBlockBounds(const AIndex: Integer; ABounds: TRectF);
  end;

  // 带复选框的单元格数据
  IQVTCheckCellData = interface(IQVTTextCellData)
    ['{7C327040-43E8-4341-96BB-7BDE619401EE}']
    // 选择状态
    function GetCheckStates: TQVTCheckStates;
    procedure SetCheckStates(AStates: TQVTCheckStates);
    function GetCheckBounds: TRectF;
    procedure SetCheckBounds(const R: TRectF);
    // 是否跟随直属子结点或父结点变更状态
    function GetFollowStates: Boolean;
    procedure SetFollowStates(const value: Boolean);
    property CheckStates: TQVTCheckStates read GetCheckStates
      write SetCheckStates;
    property FollowStates: Boolean read GetFollowStates write SetFollowStates;
    property CheckBounds: TRectF read GetCheckBounds write SetCheckBounds;
  end;

  // 带单选框的单元格数据
  IQVTRadioCellData = interface(IQVTTextCellData)
    ['{04881B5A-7085-48EE-B2B6-0561E3B851AE}']
    // 单选框分组ID
    function GetGroupId: Integer;
    procedure SetGroupId(const AId: Integer);
    // 选择状态，csSomeChecked 等价于csChecked
    function GetCheckStates: TQVTCheckStates;
    procedure SetCheckStates(AStates: TQVTCheckStates);
    function GetRadioBounds: TRectF;
    procedure SetRadioBounds(const R: TRectF);
    property GroupId: Integer read GetGroupId write SetGroupId;
    property CheckStates: TQVTCheckStates read GetCheckStates
      write SetCheckStates;
    property RadioBounds: TRectF read GetRadioBounds write SetRadioBounds;
  end;

  // 带图标的单元格数据
  IQVTImageCellData = interface
    ['{C018C791-B524-465A-8AE1-7C8ACEEF1E1A}']
    // 图像
    function GetBitmap: TBitmap;
    property Bitmap: TBitmap read GetBitmap;
  end;

  IQVTDrawer = interface
    ['{3F8FF8B2-F5D4-470F-ADFB-F21D357E0B64}']
    procedure Draw(ARect: TRectF; AData: IQVTCellData);
  end;

  IQVTStateDrawer = interface
    ['{D8B76D75-A662-413F-BC83-5B35E20CC786}']
    function GetMargins(ALayout: TQVTLayout): TRectF;
    function GetLayout(AData: IQVTCellData; var AStatSize: TSizeF): TQVTLayout;
    function CalcStateRect(const ARect: TRectF; AData: IQVTCellData): TRectF;
    function CalcContentRect(const ARect: TRectF; AData: IQVTCellData): TRectF;
  end;

  // 结点数据
  IQVTNodeData = interface
    ['{900D06CC-A19C-43FD-BC31-F072F0911CA6}']
    // 获取子结点数量
    function GetChildCount: Integer;
    // 遍历
    function GetFirstChildData: IQVTNodeData;
    function GetLastChildData: IQVTNodeData;
    function GetPriorData: IQVTNodeData;
    function GetNextData: IQVTNodeData;
    // 获取指定单元格的绘制方式
    function GetCellDrawer(const AIndex: Integer): IQVTDrawer;
    // 获取指定单元格的数据
    function GetCellData(const AIndex: Integer): IQVTCellData;
    // 获取结点的行号
    function GetRowIndex: Integer;
    property ChildCount: Integer read GetChildCount;
    property CellDrawer[const ACol: Integer]: IQVTDrawer read GetCellDrawer;
    property CellData[const ACol: Integer]: IQVTCellData read GetCellData;
  end;

  IVirtualNode = interface
    ['{DEFFEB6D-538D-4AFE-BEE6-9A617CB60177}']
    // 结点状态
    function GetStates: TQVTNodeStates;
    procedure SetStates(const States: TQVTNodeStates);
    // 链接
    function GetParent: TQVTNode;
    function GetPrior: TQVTNode;
    function GetNext: TQVTNode;
    function GetFirstChild: TQVTNode;
    function GetLastChild: TQVTNode;
    function GetLevel: Integer;
    function GetHeight: Single;
    procedure SetHeight(const value: Single);
  end;

  IQVTInplaceEditor = interface
    ['{C88AD51A-53ED-4902-8012-9924193AF749}']
    procedure SetBounds(R: TRectF);
    function BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
    function EndEdit: Boolean;
    procedure CancelEdit;
    function GetEditing(var ACol: Integer): TQVTNode;
    procedure Show;
    procedure Hide;
  end;

  TQVTDrawableObject = class(TInterfacedObject, IQVTDrawable)
  protected
    FFill: TBrush;
    FStroke: TStrokeBrush;
    // 获取行画刷
    function GetFill: TBrush; virtual;
    procedure SetFill(AFill: TBrush); virtual;
    // 获取行画笔
    function GetStroke: TStrokeBrush;
    procedure SetStroke(AStroke: TStrokeBrush);
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Assign(src: TQVTDrawableObject); virtual;
    property Fill: TBrush read GetFill write SetFill;
    property Stroke: TStrokeBrush read GetStroke write SetStroke;
  end;

  TQVTTextDrawableObject = class(TQVTDrawableObject, IQVTTextDrawable)
  protected
    FTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings; virtual;
    procedure SetTextSettings(value: TTextSettings); virtual;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Assign(src: TQVTDrawableObject); override;
    property TextSettings: TTextSettings read GetTextSettings
      write SetTextSettings;
  end;
{$M+}

  TQVTColumnTitle = class(TQVTTextDrawableObject, IQVTCellData,
    IQVTTextCellData, IQVTTextDrawable)
  protected
    FColumn: TQVTColumn;
    FText: String;
    FDrawer: IQVTDrawer;
    FSortMarker: TQVTColumnSortMarker;
    FDrawerType: TQVTDrawerType;
    FClickable: Boolean;
    FSortOnClick: Boolean;
    FIsMouseDown: Boolean;
    FIsHover: Boolean;
    FTextSettingsInfo: TTextSettingsInfo;
    function GetColIndex: Integer;
    function GetColumn: Integer;
    procedure SetColumn(const AColumn: Integer);
    function GetNode: TQVTNode;
    procedure SetNode(ANode: TQVTNode);
    function GetTreeView: TQVirtualTreeView;
    function GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetDrawerType(const value: TQVTDrawerType);
    function GetDrawer: IQVTDrawer;
    function GetSides: TSides;
    procedure SetSortMarker(const Value: TQVTColumnSortMarker);
    function GetTextSettings: TTextSettings; override;
    procedure SetTextSettings(value: TTextSettings); override;
  public
    constructor Create(AColumn: TQVTColumn); overload; virtual;
    destructor Destroy; override;
    procedure Assign(src: TQVTDrawableObject); override;
    property Drawer: IQVTDrawer read GetDrawer write FDrawer;
  published
    property TextSettings;
    property Fill;
    property Stroke;
    property Text: String read FText write SetText;
    property DrawerType: TQVTDrawerType read FDrawerType write SetDrawerType;
    property Clickable: Boolean read FClickable write FClickable;
    property SortOnClick: Boolean read FSortOnClick write FSortOnClick;
    property SortMarker: TQVTColumnSortMarker read FSortMarker
      write SetSortMarker;
  end;

  TQVTColumn = class(TCollectionItem)
  private
    FTextSettingsInfo: TTextSettingsInfo;
    FTitle: TQVTColumnTitle;
    FDrawer: IQVTDrawer;
    FFrozen: Boolean;
    FVisible: Boolean;
    FEnabled: Boolean;
    FReadOnly: Boolean;
    FDrawerType: TQVTDrawerType;
    FWidth: Single;
    FMinWidth, FMaxWidth: Single;
    FSortIndex: Integer;
    procedure SetTextSettings(const Value: TTextSettings);
    procedure SetFrozen(const Value: Boolean);
    procedure SetTitle(const ATitle: TQVTColumnTitle);
    function GetDrawer: IQVTDrawer;
    procedure SetVisible(value: Boolean);
    procedure SetEnabled(value: Boolean);
    procedure SetReadOnly(value: Boolean);
    procedure SetDrawerType(const Value: TQVTDrawerType);
    function GetTreeView: TQVirtualTreeView;
    procedure SetWidth(const Value: Single);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMinWidth(const Value: Single);
    procedure SetSortIndex(const Value: Integer);
    function GetTextSettings: TTextSettings;
  published
    property TextSettings: TTextSettings read GetTextSettings
      write SetTextSettings;
    property Title: TQVTColumnTitle read FTitle write SetTitle;
    property Frozen: Boolean read FFrozen write SetFrozen;
    property Visible: Boolean read FVisible write SetVisible;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property DrawerType: TQVTDrawerType read FDrawerType write SetDrawerType;
    property Width: Single read FWidth write SetWidth;
    property MinWidth: Single read FMinWidth write SetMinWidth;
    property MaxWidth: Single read FMaxWidth write SetMaxWidth;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(src: TPersistent); override;
    property Drawer: IQVTDrawer read GetDrawer write FDrawer;
    property TreeView: TQVirtualTreeView read GetTreeView;
    property SortIndex: Integer read FSortIndex write SetSortIndex;
  end;

  TQVTColumns = class(TCollection)
  protected
    FOwner: TPersistent;
    function GetItems(const AIndex: Integer): TQVTColumn;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); overload;
    function Add: TQVTColumn; overload;
    property Items[const AIndex: Integer]: TQVTColumn read GetItems; default;
  end;

  TQVTHeaderOption = (hoVisible, hoResizable, hoSelectColumn, hoSelectAll,
    hoMultiSortColumns);
  TQVTHeaderOptions = set of TQVTHeaderOption;

  TQVTHeader = class(TPersistent)
  private
    FColumns: TQVTColumns;
    FOptions: TQVTHeaderOptions;
    FMasterColumn: Integer;
    FAutoSizeColumn: Integer;
    FTreeView: TQVirtualTreeView;
    FHeight: Single;
    FMinHeight, FMaxHeight: Single;
    procedure SetColumns(const Value: TQVTColumns);
    procedure SetOptions(const value: TQVTHeaderOptions);
    procedure SetMasterColumn(const value: Integer);
    function GetWidth: Single;
    procedure SetHeight(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    procedure SetMinHeight(const Value: Single);
    procedure SetAutoSizeColumn(const Value: Integer);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TQVirtualTreeView); overload;
    destructor Destroy; override;
    procedure Assign(src: TPersistent); override;
    property TreeView: TQVirtualTreeView read FTreeView;
  published
    property Columns: TQVTColumns read FColumns write SetColumns;
    property Options: TQVTHeaderOptions read FOptions write SetOptions;
    property MasterColumn: Integer read FMasterColumn write SetMasterColumn;
    property AutoSizeColumn: Integer read FAutoSizeColumn
      write SetAutoSizeColumn;
    property Height: Single read FHeight write SetHeight;
    property MinHeight: Single read FMinHeight write SetMinHeight;
    property MaxHeight: Single read FMaxHeight write SetMaxHeight;
    property Width: Single read GetWidth;

  end;

  // 结点
  TQVTNodeClass = class of TQVTNode;

  TNodeInsertPosition = (ipBefore, ipAfter, ipFirstChild, ipLastChild);

  TQVTNode = class(TInterfacedObject, IVirtualNode, IQVTNodeData, IInterface)
  protected
    FStates: TQVTNodeStates; // 结点的状态
    FParent: TQVTNode; // 父结点
    FTreeView: TQVirtualTreeView; // 所隶属的树
    FNext: TQVTNode; // 下一个邻居结点
    FPrior: TQVTNode; // 前一个邻居结点
    FFirstChild: TQVTNode; // 首个子结点
    FLastChild: TQVTNode; // 最后一个子结点
    FLastInitChild: TQVTNode; // 最后一个初始化的子结点
    FFirstDirtyChild: TQVTNode;
    FCreatedCount: Integer; // 已经创建的子结点数量
    FCount: Integer; // 子结点数量，默认为-1，未知
    FLevel: Integer; // 结点的级别（初始为-1，所以不要直接读取它）
    FRowIndex: Integer; // 当前行号，默认为-1，未知，如果大于当前脏读行号或为-1，则会临时计算
    FDistance: Single; // 当前行距离首个显示的结点的距离
    FHeight: Single; // 当前行高，如果<0，则继承TreeView的默认行高，否则为缓存的本行行高
    FMinHeight: Single;
    FMaxHeight: Single;
    FIndex: Integer; // 当前结点在父结点中的索引号
    FLevelDirtyCounter: Integer;
    FVisibleRowIndex: Integer; // 当结点可见时，它所在的行号
    FDisplayRect: TRectF; // 当前结点的显示区域，仅可见时有意义
    FButtonRect: TRectF; // 当前结点如果包含子结点，则展开/收起按钮的位置
    FExts: TList<IInterface>;
    function GetCanFocus: Boolean;
    procedure SetChildCount(const Value: Integer); virtual;
    function GetHeight: Single; virtual;
    procedure SetHeight(const value: Single);
    procedure NeedInitialized;
    procedure InitChildren;
    procedure CleanDirty(ANode: TQVTNode);
    procedure Dirty(ANode: TQVTNode);
    procedure SetMaxHeight(const Value: Single);
    procedure SetMinHeight(const Value: Single);
    function GetDisplayRect: TRectF;
    function GetCellRect(AColumn: Integer): TRectF;
    function GetExts: TList<IInterface>;
    function GetIndex: Integer;
  public
    constructor Create(AOwner: TQVirtualTreeView); overload; virtual;
    destructor Destroy; override;
    // 结点状态
    function GetStates: TQVTNodeStates;
    procedure SetStates(const value: TQVTNodeStates);
    // 链接
    function GetParent: TQVTNode;
    function GetPrior: TQVTNode;
    function GetNext: TQVTNode;
    function GetFirstChild: TQVTNode;
    function GetLastChild: TQVTNode;
    // IQVTNodeData
    function GetFirstChildData: IQVTNodeData;
    function GetLastChildData: IQVTNodeData;
    function GetPriorData: IQVTNodeData;
    function GetNextData: IQVTNodeData;
    function GetFirstVisibleChild: TQVTNode;
    function GetNextVisibleChild(AChild: TQVTNode): TQVTNode;
    function GetPriorVisible(AChild: TQVTNode): TQVTNode;
    function GetLastVisibleChild: TQVTNode;
    function AddChild: TQVTNode;
    procedure Insert(ANode: TQVTNode; APos: TNodeInsertPosition);
    function HasChildren: Boolean;
    function GetLevel: Integer;
    function GetChildCount: Integer; virtual;
    function GetCellDrawer(const AIndex: Integer): IQVTDrawer; virtual;
    function GetCellData(const AIndex: Integer): IQVTCellData; virtual;
    function GetRowIndex: Integer;
    procedure ScrollToView;
    procedure Delete;
    procedure Clear;
    procedure Expand(const ANest: Boolean = false);
    procedure SetFocus;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    property Parent: TQVTNode read FParent;
    property States: TQVTNodeStates read FStates write SetStates;
    property Prior: TQVTNode read FPrior;
    property Next: TQVTNode read GetNext;
    property Level: Integer read GetLevel;
    property ChildCount: Integer read GetChildCount write SetChildCount;
    property CellDrawer[const AIndex: Integer]: IQVTDrawer read GetCellDrawer;
    property CellData[const AIndex: Integer]: IQVTCellData read GetCellData;
    property RowIndex: Integer read GetRowIndex;
    property Height: Single read GetHeight write SetHeight;
    property TreeView: TQVirtualTreeView read FTreeView;
    property CanFocus: Boolean read GetCanFocus;
    property MinHeight: Single read FMinHeight write SetMinHeight;
    property MaxHeight: Single read FMaxHeight write SetMaxHeight;
    property DisplayRect: TRectF read GetDisplayRect;
    property CellRect[AColumn: Integer]: TRectF read GetCellRect;
    property Index: Integer read GetIndex;
    property Exts: TList<IInterface> read GetExts;
  end;

  // 默认的绘制器类定义
  TQVTTextDrawer = class(TInterfacedObject, IQVTDrawer)
  protected
    function GetFill(AData: IQVTCellData; AllowNull: Boolean): TBrush;
    function GetStroke(AData: IQVTCellData; AllowNull: Boolean): TStrokeBrush;
    function GetTextSettings(AData: IQVTCellData; AllowNull: Boolean)
      : TTextSettings;
    procedure DrawText(ARect: TRectF; ATextData: IQVTTextCellData;
      ATextSettings: TTextSettings); overload;
    procedure DrawText(ATreeView: TQVirtualTreeView; ARect: TRectF;
      AText: String; ATextSettings: TTextSettings); overload;
    procedure Draw(ARect: TRectF; AData: IQVTCellData); virtual;
  end;

  TQVTIndicatorDrawer = class(TQVTTextDrawer)
  protected
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  end;

  // 默认的主列绘制器，用于绘制带树形视图的文本内容
  TQVTMasterDrawer = class(TQVTTextDrawer)
  private
    class var FStatePath: array [Boolean] of TPathData;
    class function GetCascadedPath: TPathData; static;
    class function GetExpandedPath: TPathData; static;
    class procedure SetCascadedPath(const Value: TPathData); static;
    class procedure SetExpandedPath(const Value: TPathData); static;
  protected
    FCurrentPath: array [Boolean] of TPathData;
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create; overload;
    destructor Destroy; override;
    class property ExpandedPath: TPathData read GetExpandedPath
      write SetExpandedPath;
    class property CascadedPath: TPathData read GetCascadedPath
      write SetCascadedPath;
  end;

  // 默认的进度状态绘制器，用于绘制进度条
  TQVTProgressDrawer = class(TQVTTextDrawer)
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  end;

  TQVTStateDrawer = class(TQVTTextDrawer, IQVTStateDrawer)
  protected
    function GetLayout(AData: IQVTCellData; var AStateSize: TSizeF): TQVTLayout;
    function GetMargins(ALayout: TQVTLayout): TRectF;
    function CalcStateRect(const ARect: TRectF; AData: IQVTCellData): TRectF;
      overload; virtual;
    function CalcStateRect(const ARect: TRectF; const ASize: TSizeF;
      ALayout: TQVTLayout): TRectF; overload;
    function CalcContentRect(const ARect: TRectF; AData: IQVTCellData): TRectF;
      overload; virtual;
    function CalcContentRect(const ARect, AStateRect: TRectF;
      ALayout: TQVTLayout): TRectF; overload;
    procedure CalcLayouts(AData: IQVTCellData; const ARect: TRectF;
      var AStateRect, AContentRect: TRectF);
    procedure DrawPath(AData: IQVTCellData; APath: TPathData;
      AOpacity: Single); virtual;
  end;

  TQVTHeaderDrawer = class(TQVTStateDrawer)
  private
    class var FStatePath: array [0 .. 1] of TPathData;
    class function GetAscPath: TPathData; static;
    class function GetDescPath: TPathData; static;
  protected
    FCurrentPath: array [0 .. 1] of TPathData;
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  public
    class constructor Create;
    class destructor Destroy;
    class property AscPath: TPathData read GetAscPath;
    class property DescPath: TPathData read GetDescPath;
  end;

  // 默认复选框绘制器，用于绘制复选状态
  TQVTCheckDrawer = class(TQVTStateDrawer)
  private
    // 六种状态：未选中、选中、部分选中各启用和禁用两种
    class var FCheckStatePath: array [0 .. 2] of TPathData;
    class function GetPathData(AStates: TQVTCheckStates): TPathData; static;
    class function StatePathIndex(AStates: TQVTCheckStates): Integer; static;
    class function GetCheckedPath: TPathData; static;
    class function GetSomeCheckedPath: TPathData; static;
    class function GetUncheckPath: TPathData; static;
  protected
    FCurrentPath: array [0 .. 2] of TPathData;
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
    class property StatePath[AStates: TQVTCheckStates]: TPathData
      read GetPathData;
  public
    class property UncheckPath: TPathData read GetUncheckPath;
    class property CheckedPath: TPathData read GetCheckedPath;
    class property SomeCheckedPath: TPathData read GetSomeCheckedPath;
  end;

  // 默认的单选框绘制器，用于绘制单选状态
  TQVTRadioDrawer = class(TQVTStateDrawer)
  private
    class var FCheckStatePath: array [Boolean] of TPathData;
    class function GetCheckedPath: TPathData; static;
    class function GetUncheckPath: TPathData; static;
  protected
    FCurrentPath: array [Boolean] of TPathData;
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
    class property UncheckPath: TPathData read GetUncheckPath;
    class property CheckedPath: TPathData read GetCheckedPath;
  end;

  // 默认的图片绘制器，用于绘制图片结点
  TQVTImageDrawer = class(TQVTTextDrawer)
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  end;

  // 默认带状态图片绘制器，用于绘制带状态的结点
  TQVTImageStateDrawer = class(TQVTStateDrawer)
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  end;

  TQVTMoneyDrawer = class(TQVTTextDrawer)
    procedure Draw(ARect: TRectF; AData: IQVTCellData); override;
  end;

  // 默认的结点单元格数据类型定义
  TQVTDefaultCellData = class(TQVTDrawableObject, IQVTCellData)
  private
  protected
    FNode: TQVTNode;
    FColumn: Integer;
    function GetColumn: Integer; virtual;
    procedure SetColumn(const AColumn: Integer);
    function GetNode: TQVTNode; virtual;
    procedure SetNode(ANode: TQVTNode); virtual;
    function GetTreeView: TQVirtualTreeView; virtual;
    function GetEnabled: Boolean; virtual;
    function GetSides: TSides; virtual;
  public
    constructor Create(ANode: TQVTNode; AColIndex: Integer); overload; virtual;
    constructor Create; overload;
    property Node: TQVTNode read FNode write FNode;
    property Column: Integer read FColumn write FColumn;
    property TreeView: TQVirtualTreeView read GetTreeView;
    property Enabled: Boolean read GetEnabled;
    property Sides: TSides read GetSides;
  end;

  TQVTCheckCellData = class(TQVTDefaultCellData, IQVTTextCellData,
    IQVTCheckCellData)
  protected
    FCheckStates: TQVTCheckStates;
    FCheckBounds: TRectF;
    FFollowStates: Boolean;
    FUpdating: Boolean;
    function GetText: String; virtual;
    function GetCheckStates: TQVTCheckStates; virtual;
    procedure SetCheckStates(AStates: TQVTCheckStates); virtual;
    function GetFollowStates: Boolean;
    procedure SetFollowStates(const value: Boolean);
    function GetCheckBounds: TRectF;
    procedure SetCheckBounds(const R: TRectF);
    procedure UpdateStates;
  public

  end;

  TQVTGetCellDataEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode;
    ACol: Integer; var AData: IQVTCellData) of object;
  TQVTGetCellDrawerEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode;
    ACol: Integer; var ADrawer: IQVTDrawer) of object;
  TQVTNodeNotifyEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode)
    of object;
  TQVTTitleNotifyEvent = procedure(Sender: TQVirtualTreeView; ACol: Integer)
    of object;
  TQVTColumnNotifyEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode;
    ACol: Integer) of object;
  TQVTCellClickEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode;
    ACol: Integer; APos: TPointF) of object;
  TQVTAcceptEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode;
    ACol: Integer; var Accept: Boolean) of object;
  TQVTGetCellEditorEvent = procedure(Sender: TQVirtualTreeView; ANode: TQVTNode;
    ACol: Integer; var AEditor: IQVTInplaceEditor) of object;

  TQVTPaintOption = (poHorizLine, poVertLine, poTreeLine, poColSelection,
    poRowSelection, poNodeButton, poHover);
  TQVTPaintOptions = set of TQVTPaintOption;
  TQVTOption = (toTestHover, toRowSizable, toEditable);
  TQVTOptions = set of TQVTOption;
  TQVTState = (tsScrolling, tsDragging, tsRowSizing, tsColSizing,
    tsVisibleChanged, tsContentChanged);
  TQVTStates = set of TQVTState;
  TQVTHitTestResult = (hrNone, hrHeader, hrNode, hrColumnSpace, hrRowSpace);
  TQVTSortColumns = TList<TQVTColumn>;

  TQVirtualTreeView = class(TPresentedTextControl, IQVTDrawable)
  private
    class var DefaultDrawers: array [TQVTDrawerType] of IQVTDrawer;
  protected
    // Vars
    FRootNode: TQVTNode;
    FFirstVisibleNode: TQVTNode;
    FFirstVisibleColumn: Integer;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FSelectionColor: TAlphaColor;
    FHoverColor: TAlphaColor;
    FLineStyle: TStrokeBrush;
    FHeader: TQVTHeader;
    FNodeIndent: Single;
    FFocusNode: TQVTNode;
    FFocusColumn: Integer;
    FHoverNode: TQVTNode;
    FHoverColumn: Integer;
    FTextLayout: TTextLayout;
    FNodeClass: TQVTNodeClass;
    FDirtyRowIndex: Integer;
    FLevelDirtyCounter: Integer;
    FDefaultRowHeight: Single;
    FPaintOptions: TQVTPaintOptions;
    FPaintOffset: TPointF;
    FHorzScrollBar: TScrollBar;
    FVertScrollBar: TScrollBar;
    FVisibleNodes: TList<TQVTNode>;
    FStates: TQVTStates;
    FSpace: TPointF;
    FOptions: TQVTOptions;
    FContentSize: TSizeF;
    FSizingNode: TQVTNode;
    FSizingNodeOriginHeight: Single;
    FSizingColumn: TQVTColumn;
    FSizingColumnOriginWidth: Single;
    FMouseDownPos: TPointF;
    FMouseDownTime: Cardinal;
    FMouseDownOffset: TPointF;
    FMouseDownNode: TQVTNode;
    FMouseDownColumn: Integer;
    FTintColor: TAlphaColor;
    FInplaceEditor: IQVTInplaceEditor; // 当前编辑器
    FSortColumns: TQVTSortColumns;
    FAniCalculations: TAniCalculations;
    // Events
    FOnHoverChanged: TNotifyEvent;
    FOnInitChildren: TQVTNodeNotifyEvent;
    FOnInitNode: TQVTNodeNotifyEvent;
    FOnFocusChanging: TQVTAcceptEvent;
    FOnFocusChanged: TQVTColumnNotifyEvent;
    FOnGetCellData: TQVTGetCellDataEvent;
    FOnGetCellDrawer: TQVTGetCellDrawerEvent;
    FOnCellClick: TQVTCellClickEvent;
    FOnCanEdit: TQVTAcceptEvent;
    FOnGetCellEditor: TQVTGetCellEditorEvent;
    FOnTitleClick: TQVTTitleNotifyEvent;
    FOnSortmarkerChanged: TNotifyEvent;
    procedure SetFocusColumn(const Value: Integer);
    procedure SetPaintOptions(const Value: TQVTPaintOptions);
    procedure SetSpace(const Value: TPointF);
    function GetRootNode: TQVTNode;
    function GetFill: TBrush;
    function GetStroke: TStrokeBrush;
    function GetDisplayRect(ANode: TQVTNode): TRectF;
    function GetCellRect(ANode: TQVTNode; AColumn: Integer): TRectF;
    procedure SetFocusNode(const Value: TQVTNode);
    procedure Paint; override;
    procedure Resize; override;
    procedure AdjustAutoSizeColumn;
    procedure DoSortMarkerChanged(Sender: TObject);
    procedure CheckForBrowseMode(ASender: TObject);
    procedure RowDirty(ARowIndex: Integer);
    procedure InitNode(ANode: TQVTNode);
    function CreateNode: TQVTNode;
    function GetRootNodeCount: Integer;
    procedure SetRootNodeCount(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
    function CreateScrollBar(AOrientation: TOrientation): TScrollBar;
    procedure DoScrollChanged(ASender: TObject);
    function CalcScrollRange: TSizeF;
    procedure NodeVisibleChanged;
    procedure NodeContentChanged;
    procedure CheckScrollBars;
    function FocusChanging(ANewNode: TQVTNode; ANewCol: Integer): Boolean;
    procedure FocusChanged;
    function DoHitTest(const APos: TPointF; var ANode: TQVTNode;
      var AColumn: Integer): TQVTHitTestResult;
    procedure CellClick(ANode: TQVTNode; ACol: Integer; const APos: TPointF);
    function CreateEditor(ANode: TQVTNode; ACol: Integer): IQVTInplaceEditor;
    function GetSortColumns: TQVTSortColumns;
    procedure DoSortColumnsChanged(Sender: TObject; const Item: TQVTColumn;
      Action: TCollectionNotification);
    procedure AniChange(Sender: TObject);
  published
    property LineStyle: TStrokeBrush read FLineStyle;
    property Fill: TBrush read FFill;
    property Stroke: TStrokeBrush read FStroke;
    property SelectionColor: TAlphaColor read FSelectionColor
      write FSelectionColor;
    property HoverColor: TAlphaColor read FHoverColor write FHoverColor;
    property NodeIndent: Single read FNodeIndent write FNodeIndent;
    property Header: TQVTHeader read FHeader;
    property DefaultRowHeight: Single read FDefaultRowHeight
      write FDefaultRowHeight;
    property Options: TQVTOptions read FOptions write FOptions;
    property RootNodeCount: Integer read GetRootNodeCount
      write SetRootNodeCount;
    property AniCalculations: TAniCalculations read FAniCalculations;
    // Events
    property OnGetCellData: TQVTGetCellDataEvent read FOnGetCellData
      write FOnGetCellData;
    property OnGetCellDrawer: TQVTGetCellDrawerEvent read FOnGetCellDrawer
      write FOnGetCellDrawer;
    property OnHoverChanged: TNotifyEvent read FOnHoverChanged
      write FOnHoverChanged;
    property OnInitNode: TQVTNodeNotifyEvent read FOnInitNode write FOnInitNode;
    property OnInitChildren: TQVTNodeNotifyEvent read FOnInitChildren
      write FOnInitChildren;
    property OnFocusChanged: TQVTColumnNotifyEvent read FOnFocusChanged
      write FOnFocusChanged;
    property OnFocusChanging: TQVTAcceptEvent read FOnFocusChanging
      write FOnFocusChanging;
    property OnCellClick: TQVTCellClickEvent read FOnCellClick
      write FOnCellClick;
    property OnCanEdit: TQVTAcceptEvent read FOnCanEdit write FOnCanEdit;
    property OnGetCellEditor: TQVTGetCellEditorEvent read FOnGetCellEditor
      write FOnGetCellEditor;
    property OnTitleClick: TQVTTitleNotifyEvent read FOnTitleClick
      write FOnTitleClick;
    property OnSortmarkerChanged: TNotifyEvent read FOnSortmarkerChanged
      write FOnSortmarkerChanged;
    // inherited event

    property Action;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleLookup;
    property TextSettings;
    property Text;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    { events }
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
{$IF RTLVersioN>=32}
    property OnResized;
{$ENDIF}
  public
    class constructor Create;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetCellData(ANode: TQVTNode; AColIndex: Integer): IQVTCellData;
    function GetCellDrawer(ANode: TQVTNode; AColIndex: Integer): IQVTDrawer;
    function GetChildCount(ANode: TQVTNode): Integer;
    procedure Invalidate;
    procedure InvalidateColumn(const AColumn: Integer);
    procedure InvalidateNode(const ANode: TQVTNode);
    function GetNext(ANode: TQVTNode): TQVTNode;
    function GetNextVisible(ANode: TQVTNode): TQVTNode;
    function GetPriorVisible(ANode: TQVTNode): TQVTNode;
    function GetNodeAt(const APos: TPointF; AColumn: PInteger = nil): TQVTNode;
    procedure MakeNodeVisible(ANode: TQVTNode; ACenterInView: Boolean = false);
    procedure ScrollBy(dx, dy: Single);
    function CanEdit(ANode: TQVTNode; ACol: Integer): Boolean;
    function BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
    function EndEdit: Boolean;
    procedure CancelEdit;
    class function GetDefaultDrawer(AType: TQVTDrawerType): IQVTDrawer;
    property VertScrollBar: TScrollBar read FVertScrollBar;
    property HorzScrollBar: TScrollBar read FHorzScrollBar;
    property FocusNode: TQVTNode read FFocusNode write SetFocusNode;
    property FocusColumn: Integer read FFocusColumn write SetFocusColumn;
    property FirstVisibleNode: TQVTNode read FFirstVisibleNode;
    property MouseDownNode: TQVTNode read FMouseDownNode;
    property MouseDownColumn: Integer read FMouseDownColumn;
    property DisplayRect[ANode: TQVTNode]: TRectF read GetDisplayRect;
    property CellRect[ANode: TQVTNode; AColumn: Integer]: TRectF
      read GetCellRect;
    property RootNode: TQVTNode read GetRootNode;
    property PaintOptions: TQVTPaintOptions read FPaintOptions
      write SetPaintOptions;
    property Space: TPointF read FSpace write SetSpace;
    property HoverNode: TQVTNode read FHoverNode;
    property HoverColumn: Integer read FHoverColumn;
    property TintColor: TAlphaColor read FTintColor write FTintColor;
    property SortColumns: TQVTSortColumns read GetSortColumns;
  end;

implementation

resourcestring
  SCantEndEdit = 'Can'' end editing for browse mode.';

  { TQVTDrawableObject }

procedure TQVTDrawableObject.Assign(src: TQVTDrawableObject);
var
  ASource: TQVTDrawableObject;
begin
  ASource := src as TQVTDrawableObject;
  SetFill(ASource.FFill);
  SetStroke(ASource.FStroke);
end;

constructor TQVTDrawableObject.Create;
begin
  inherited;
end;

destructor TQVTDrawableObject.Destroy;
begin
  if Assigned(FFill) then
    FreeAndNil(FFill);
  if Assigned(FStroke) then
    FreeAndNil(FStroke);
  inherited;
end;

function TQVTDrawableObject.GetFill: TBrush;
begin
  Result := FFill;
end;

function TQVTDrawableObject.GetStroke: TStrokeBrush;
begin
  Result := FStroke;
end;

procedure TQVTDrawableObject.SetFill(AFill: TBrush);
begin
  if AFill <> nil then
  begin
    if not Assigned(FFill) then
      FFill := TBrush.Create(TBrushKind.Solid, TAlphaColors.White);
    FFill.Assign(AFill);
  end
  else if Assigned(FFill) then
    FreeAndNil(FFill);
end;

procedure TQVTDrawableObject.SetStroke(AStroke: TStrokeBrush);
begin
  if Assigned(AStroke) then
  begin
    if not Assigned(AStroke) then
      FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
    FStroke.Assign(AStroke);
  end
  else if Assigned(FStroke) then
    FreeAndNil(FStroke);
end;

{ TQVTTextDrawableObject }

procedure TQVTTextDrawableObject.Assign(src: TQVTDrawableObject);
begin
  inherited;
  if src is TQVTTextDrawableObject then
    SetTextSettings(TQVTTextDrawableObject(src).TextSettings);
end;

constructor TQVTTextDrawableObject.Create;
begin
  inherited;
end;

destructor TQVTTextDrawableObject.Destroy;
begin
  if Assigned(FTextSettings) then
    FreeAndNil(FTextSettings);
  inherited;
end;

function TQVTTextDrawableObject.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

procedure TQVTTextDrawableObject.SetTextSettings(value: TTextSettings);
begin
  if not Assigned(FTextSettings) then
    FTextSettings := TTextSettings.Create(nil);
  FTextSettings.Assign(value);
end;

{ TQVTHeader }

procedure TQVTHeader.Assign(src: TPersistent);
begin
  inherited;
  if src is TQVTHeader then
  begin
    FColumns.Assign(TQVTHeader(src).FColumns);
    FOptions := TQVTHeader(src).Options;
    FMasterColumn := TQVTHeader(src).FMasterColumn;
  end;
end;

constructor TQVTHeader.Create(AOwner: TQVirtualTreeView);
begin
  inherited Create;
  Pointer(FTreeView) := AOwner;
  FColumns := TQVTColumns.Create(Self);
  FOptions := [hoResizable];
  FMasterColumn := 0;
  FAutoSizeColumn := -1;
  FHeight := 20;
end;

destructor TQVTHeader.Destroy;
begin
  Pointer(FTreeView) := nil;
  inherited;
end;

function TQVTHeader.GetOwner: TPersistent;
begin
  Result := FTreeView;
end;

function TQVTHeader.GetWidth: Single;
var
  I: Integer;
  ACol: TQVTColumn;
begin
  Result := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    ACol := Columns[I];
    if ACol.Visible then
      Result := Result + ACol.Width + TreeView.Space.X;
  end;
  // 减去多加的那一次
  Result := Result - TreeView.Space.X;
end;

procedure TQVTHeader.SetAutoSizeColumn(const Value: Integer);
begin
  if FAutoSizeColumn <> Value then
  begin
    FAutoSizeColumn := Value;
    FTreeView.Resize;
  end;
end;

procedure TQVTHeader.SetColumns(const Value: TQVTColumns);
begin
  FColumns.Assign(Value);
end;

procedure TQVTHeader.SetHeight(const Value: Single);
begin
  if not SameValue(FHeight, Value) then
  begin
    if (Value > MinHeight) then
    begin
      if IsZero(MaxHeight) or (Value <= MaxHeight) then
      begin
        FHeight := Value;
        TreeView.NodeVisibleChanged;
      end;
    end;
  end;
end;

procedure TQVTHeader.SetMasterColumn(const value: Integer);
begin
  FMasterColumn := value;
  FTreeView.InvalidateColumn(value);
end;

procedure TQVTHeader.SetMaxHeight(const Value: Single);
begin
  if not SameValue(FMaxHeight, Value) then
  begin
    FMaxHeight := Value;
    if (Value > 0) and (Height > Value) then
      Height := Value;
  end;
end;

procedure TQVTHeader.SetMinHeight(const Value: Single);
begin
  if not SameValue(FMinHeight, Value) then
  begin
    FMinHeight := Value;
    if (Value > 0) and (Height < FMinHeight) then
      Height := Value;
  end;
end;

procedure TQVTHeader.SetOptions(const value: TQVTHeaderOptions);
begin
  FOptions := value;
  TreeView.NodeContentChanged;
end;

{ TQVTColumnTitle }

procedure TQVTColumnTitle.Assign(src: TQVTDrawableObject);
begin
  inherited;
  if src is TQVTColumnTitle then
  begin
    FText := TQVTColumnTitle(src).Text;
    FDrawerType := TQVTColumnTitle(src).DrawerType;
    FClickable := TQVTColumnTitle(src).Clickable;
    FSortMarker := TQVTColumnTitle(src).SortMarker;
    FSortOnClick := TQVTColumnTitle(src).SortOnClick;
  end;
end;

constructor TQVTColumnTitle.Create(AColumn: TQVTColumn);
begin
  inherited Create;
  Pointer(FColumn) := Pointer(AColumn);
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColors.Whitesmoke);
  FStroke := TStrokeBrush.Create(TBrushKind.None, TAlphaColors.LtGray);
  FDrawerType := TQVTDrawerType.dtHeader;
  FSortMarker := TQVTColumnSortMarker.smNone;
  FSortOnClick := False;
  FClickable := False;
  FTextSettingsInfo := TTextSettingsInfo.Create(AColumn,
    AColumn.TreeView.GetTextSettingsClass);
  TextSettings.HorzAlign := TTextAlign.Center;
end;

destructor TQVTColumnTitle.Destroy;
begin
  Pointer(FColumn) := nil; // 避免自动引用计数引起的问题
  inherited;
end;

function TQVTColumnTitle.GetColIndex: Integer;
begin
  Result := FColumn.Index;
end;

function TQVTColumnTitle.GetColumn: Integer;
begin
  Result := FColumn.Index;
end;

function TQVTColumnTitle.GetDrawer: IQVTDrawer;
begin
  Result := FColumn.TreeView.GetCellDrawer(nil, FColumn.Index);
end;

function TQVTColumnTitle.GetEnabled: Boolean;
begin
  Result := FColumn.Enabled;
end;

function TQVTColumnTitle.GetNode: TQVTNode;
begin
  // 标题与结点无关，所以返回nil
  Result := nil;
end;

function TQVTColumnTitle.GetSides: TSides;
begin
  Result := [TSide.Top, TSide.Left, TSide.Right, TSide.Bottom];
end;

function TQVTColumnTitle.GetText: String;
begin
  Result := FText;
end;

function TQVTColumnTitle.GetTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

function TQVTColumnTitle.GetTreeView: TQVirtualTreeView;
begin
  Result := FColumn.TreeView;
end;

procedure TQVTColumnTitle.SetColumn(const AColumn: Integer);
begin
  // 列标题是与列相关的，所以忽略修改
end;

procedure TQVTColumnTitle.SetDrawerType(const value: TQVTDrawerType);
begin
  if FDrawerType <> Value then
  begin
    FDrawerType := value;
    FDrawer := nil;
  end;
end;

procedure TQVTColumnTitle.SetEnabled(value: Boolean);
begin
  FColumn.Enabled := value;
end;

procedure TQVTColumnTitle.SetNode(ANode: TQVTNode);
begin
  // 列标题的Node始终为空，所以忽略修改
end;

procedure TQVTColumnTitle.SetSortMarker(const Value: TQVTColumnSortMarker);
begin
  if FSortMarker <> Value then
  begin
    FSortMarker := Value;
    FColumn.TreeView.DoSortMarkerChanged(FColumn);
  end;
end;

procedure TQVTColumnTitle.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    if hoVisible in FColumn.TreeView.Header.Options then
      FColumn.TreeView.InvalidateColumn(FColumn.Index);
  end;
end;

procedure TQVTColumnTitle.SetTextSettings(value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(value);
end;

{ TQVirtualTreeView }

procedure TQVirtualTreeView.AdjustAutoSizeColumn;
var
  W: Single;
  I: Integer;
  ACol: TQVTColumn;
begin
  if (Header.AutoSizeColumn >= 0) and
    (Header.AutoSizeColumn < Header.Columns.Count) then
  begin
    W := 0;
    for I := 0 to Header.Columns.Count - 1 do
    begin
      ACol := Header.Columns[I];
      if (I <> Header.AutoSizeColumn) and ACol.Visible then
        W := W + ACol.Width + Space.X;
    end;
    if Header.Columns[Header.AutoSizeColumn].Visible then
    begin
      W := Width - Padding.Left - Padding.Right - W;
      if FVertScrollBar.Visible then
        W := W - FVertScrollBar.Width;
      Header.Columns[Header.AutoSizeColumn].Width := W;
    end;
  end;
end;

procedure TQVirtualTreeView.AniChange(Sender: TObject);
begin
  FHorzScrollBar.Value := FAniCalculations.ViewportPosition.X;
  FVertScrollBar.Value := FAniCalculations.ViewportPosition.Y;
end;

function TQVirtualTreeView.BeginEdit(ANode: TQVTNode; ACol: Integer): Boolean;
var
  LR, R: TRectF;
  ARightMax: Single;
begin
  if Assigned(FInplaceEditor) then
    Result := FInplaceEditor.EndEdit and CanEdit(ANode, ACol)
  else
    Result := CanEdit(ANode, ACol);
  if Result then
  begin
    FInplaceEditor := CreateEditor(ANode, ACol);
    if Assigned(FInplaceEditor) then
    begin
      Result := FInplaceEditor.BeginEdit(ANode, ACol);
      if Result then
      begin
        R := ANode.CellRect[ACol];
        LR := LocalRect;
        LR.Left := LR.Left + Padding.Left;
        LR.Right := LR.Right - Padding.Right;
        if FVertScrollBar.Visible then
          LR.Right := LR.Right - FVertScrollBar.Width;
        if R.Left < LR.Left then
          R.Left := LR.Left;
        if R.Right > LR.Right then
          R.Right := LR.Right;
        FInplaceEditor.SetBounds(R);
        FInplaceEditor.Show;
      end
      else
        FInplaceEditor := nil;
    end
    else
      Result := false;
  end;
end;

function TQVirtualTreeView.CalcScrollRange: TSizeF;
var
  ANode: TQVTNode;
  ACol: TQVTColumn;
  I: Integer;
begin
  ANode := RootNode.GetFirstVisibleChild;
  Result.cx := 0;
  Result.cy := 0;
  I := 0;
  while Assigned(ANode) do
  begin
    ANode.NeedInitialized;
    ANode.FRowIndex := I;
    ANode.FDistance := Result.cy;
    Inc(I);
    Result.cy := Result.cy + ANode.Height + FSpace.Y;
    ANode := GetNextVisible(ANode);
  end;
  for I := 0 to Header.Columns.Count - 1 do
  begin
    ACol := Header.Columns[I];
    if ACol.Visible then
      Result.cx := Result.cx + ACol.Width + FSpace.X;
  end;
  if Result.cy > 0 then
    Result.cy := Result.cy - FSpace.Y;
  if Result.cx > 0 then
    Result.cx := Result.cx - FSpace.X;
end;

procedure TQVirtualTreeView.CancelEdit;
begin
  if Assigned(FInplaceEditor) then
  begin
    FInplaceEditor.CancelEdit;
    FInplaceEditor := nil;;
  end;
end;

function TQVirtualTreeView.CanEdit(ANode: TQVTNode; ACol: Integer): Boolean;
begin
  if (ACol < 0) or (ACol >= Header.Columns.Count) then
    Result := False
  else if TQVTOption.toEditable in Options then
  begin
    Result := not Header.Columns[ACol].ReadOnly;
    if Assigned(FOnCanEdit) then
      FOnCanEdit(Self, ANode, ACol, Result);
  end
  else
    Result := false;
end;

procedure TQVirtualTreeView.CellClick(ANode: TQVTNode; ACol: Integer;
  const APos: TPointF);
begin
  if Assigned(FOnCellClick) then
    FOnCellClick(Self, ANode, ACol, APos);
  if CanEdit(ANode, ACol) then
    BeginEdit(ANode, ACol);
end;

procedure TQVirtualTreeView.CheckForBrowseMode(ASender: TObject);
begin
  if Assigned(FInplaceEditor) then
  begin
    if not FInplaceEditor.EndEdit then
      raise Exception.Create(SCantEndEdit);
    FInplaceEditor := nil;
  end;
end;

procedure TQVirtualTreeView.CheckScrollBars;
var
  R: TRectF;
  Targets: array of TAniCalculations.TTarget;
begin
  if TQVTState.tsContentChanged in FStates then
  begin
    FContentSize := CalcScrollRange;
    FStates := FStates - [TQVTState.tsContentChanged];
    R := LocalRect;
    R.Left := R.Left + Padding.Left;
    R.Top := R.Top + Padding.Top;
    if TQVTHeaderOption.hoVisible in FHeader.Options then
      R.Top := R.Top + Header.Height;
    R.Right := R.Right - Padding.Right;
    R.Bottom := R.Bottom - Padding.Bottom;
    // 计算是否需要垂直滚动条
    if FContentSize.cy > R.Height then
    begin
      FVertScrollBar.Visible := true;
      FVertScrollBar.Max := FContentSize.cy;
      if hoVisible in Header.Options then
        FVertScrollBar.ViewportSize := R.Height - FHorzScrollBar.Height -
          FHeader.Height
      else
        FVertScrollBar.ViewportSize := R.Height - FHorzScrollBar.Height;
    end
    else
      FVertScrollBar.Visible := false;
    if FContentSize.cx > R.Width then
    begin
      FHorzScrollBar.Visible := true;
      FHorzScrollBar.Max := FContentSize.cx;
      FHorzScrollBar.ViewportSize := R.Width - FVertScrollBar.Width;
    end
    else
      FHorzScrollBar.Visible := false;
    SetLength(Targets, 2);

    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);
    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    Targets[1].Point := TPointD.Create(FHorzScrollBar.Max -
      FHorzScrollBar.ViewportSize, FVertScrollBar.Max -
      FVertScrollBar.ViewportSize);
    FAniCalculations.SetTargets(Targets);
  end;
end;

constructor TQVirtualTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColors.White);
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.LtGray);
  FLineStyle := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
  FLineStyle.Dash := TStrokeDash.Dot;
  FHeader := TQVTHeader.Create(Self);
  FNodeClass := TQVTNode;
  FDefaultRowHeight := 20;
  FNodeIndent := 16;
  FSpace.X := 1;
  FSpace.Y := 1;
  CanFocus := True;
  FVisibleNodes := TList<TQVTNode>.Create;
  FStates := [tsVisibleChanged];
  FPaintOptions := [TQVTPaintOption.poNodeButton, TQVTPaintOption.poTreeLine,
    TQVTPaintOption.poRowSelection];
  FHorzScrollBar := CreateScrollBar(TOrientation.Horizontal);
  FVertScrollBar := CreateScrollBar(TOrientation.Vertical);
  FSelectionColor := TAlphaColors.Lightblue;
  FHoverColor := TAlphaColors.Lightpink;
  FHoverColumn := -1;
  FFocusColumn := -1;
  FOptions := [];
  FTintColor := TAlphaColors.Black;
  FAniCalculations := TAniCalculations.Create(Self);
  FAniCalculations.TouchTracking := [ttVertical, ttHorizontal];
  FAniCalculations.Animation := True;
  FAniCalculations.BoundsAnimation := True;
  FAniCalculations.DecelerationRate := DecelerationRateNormal;
  FAniCalculations.OnChanged := AniChange;
  TextSettings.WordWrap := False;
  TextSettings.VertAlign := TTextAlign.Center;
  Width := 180;
  Height := 300;
end;

function TQVirtualTreeView.CreateEditor(ANode: TQVTNode; ACol: Integer)
  : IQVTInplaceEditor;
begin
  Result := nil;
  if Assigned(FOnGetCellEditor) then
    FOnGetCellEditor(Self, ANode, ACol, Result);
end;

class constructor TQVirtualTreeView.Create;
begin
  DefaultDrawers[TQVTDrawerType.dtText] := TQVTTextDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtCheck] := TQVTCheckDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtRadio] := TQVTRadioDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtImage] := TQVTImageDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtStateText] := TQVTStateDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtTree] := TQVTMasterDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtProgress] := TQVTProgressDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtMoney] := TQVTMoneyDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtRowIndicator] := TQVTIndicatorDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtHeader] := TQVTHeaderDrawer.Create;
  DefaultDrawers[TQVTDrawerType.dtCustom] := DefaultDrawers[dtText];
  DefaultDrawers[TQVTDrawerType.dtDefault] := DefaultDrawers[dtText];
end;

function TQVirtualTreeView.CreateNode: TQVTNode;
begin
  Result := FNodeClass.Create(Self);
end;

function TQVirtualTreeView.CreateScrollBar(AOrientation: TOrientation)
  : TScrollBar;
begin
  Result := TSmallScrollBar.Create(Self);
  Result.Parent := Self;
  Result.Orientation := AOrientation;
  case AOrientation of
    TOrientation.Horizontal: // 水平
      begin
        Result.Align := TAlignLayout.Bottom;
        Result.Height := 8;
      end;
    TOrientation.Vertical:
      begin
        Result.Align := TAlignLayout.Right;
        Result.Width := 8;
      end;
  end;
  Result.OnChange := DoScrollChanged;
  Result.Visible := false;
end;

destructor TQVirtualTreeView.Destroy;
begin
  if Assigned(FAniCalculations) then
    FreeAndNil(FAniCalculations);
  if Assigned(FInplaceEditor) then
  begin
    FInplaceEditor.CancelEdit;
    FInplaceEditor := nil;
  end;
  FreeAndNil(FFill);
  FreeAndNil(FStroke);
  FreeAndNil(FLineStyle);
  FreeAndNil(FHeader);
  if Assigned(FTextLayout) then
    FreeAndNil(FTextLayout);
  if Assigned(FSortColumns) then
  begin
    FSortColumns.Clear;
    FreeAndNil(FSortColumns);
  end;
  FreeAndNil(FVisibleNodes);
  if Assigned(FRootNode) then
  begin
    FRootNode._Release;
    FRootNode := nil;
  end;
  inherited;
end;

procedure TQVirtualTreeView.DialogKey(var Key: Word; Shift: TShiftState);
var
  ANode: TQVTNode;
begin
  inherited;
  case Key of
    vkUp:
      begin
        if Assigned(FocusNode) and EndEdit then
        begin
          ANode := GetPriorVisible(FocusNode);
          if Assigned(ANode) then
            FocusNode := ANode;
        end;
      end;
    vkDown:
      begin
        if Assigned(FocusNode) and EndEdit then
        begin
          ANode := GetNextVisible(FocusNode);
          if Assigned(ANode) then
            FocusNode := ANode;
        end;
      end;
    vkLeft:
      begin
        if Assigned(FocusNode) and EndEdit then
        begin
          if FocusColumn > 0 then
            FocusColumn := FocusColumn - 1;
        end;
      end;
    vkRight:
      begin
        if Assigned(FocusNode) and EndEdit then
        begin
          if FocusColumn + 1 < Header.Columns.Count then
            FocusColumn := FocusColumn + 1;
        end;
      end;
    vkPrior: // PageUp 上翻半篇
      if EndEdit then
        VertScrollBar.Value := VertScrollBar.Value -
          (LocalRect.Height - Padding.Top - Padding.Bottom) / 2;
    vkNext: // PageDown 下翻半篇
      if EndEdit then
        VertScrollBar.Value := VertScrollBar.Value +
          (LocalRect.Height - Padding.Top - Padding.Bottom) / 2;
  end;
end;

procedure TQVirtualTreeView.DoScrollChanged(ASender: TObject);
  procedure CheckFirstVisibleNode;
  var
    ANode: TQVTNode;
    V: Single;
  begin
    if not Assigned(FFirstVisibleNode) then
      FFirstVisibleNode := RootNode.GetFirstVisibleChild;
    ANode := FFirstVisibleNode;
    V := FVertScrollBar.Value;
    while Assigned(ANode) and (ANode.FDistance + ANode.Height < V) do
    begin
      FFirstVisibleNode := ANode;
      ANode := GetNextVisible(ANode);
    end;
    while Assigned(ANode) do
    begin
      FFirstVisibleNode := ANode;
      if ANode.FDistance < V then
        break;
      ANode := GetPriorVisible(ANode);
    end;
    if Assigned(FFirstVisibleNode) then
      FPaintOffset.Y := V - FFirstVisibleNode.FDistance
    else
      FPaintOffset.Y := 0;
  end;

  procedure CheckColumnVisible;
  var
    I: Integer;
    L, V: Single;
  begin
    L := 0;
    V := FHorzScrollBar.Value;
    FFirstVisibleColumn := -1;
    for I := 0 to FHeader.Columns.Count - 1 do
    begin
      L := L + FHeader.Columns[I].Width + FSpace.X;
      if L > V then
      begin
        FFirstVisibleColumn := I;
        Break;
      end;
    end;
    if FFirstVisibleColumn <> -1 then
      FPaintOffset.X := V;
  end;

begin
  if ASender = FVertScrollBar then
    CheckFirstVisibleNode
  else
    CheckColumnVisible;
  FAniCalculations.ViewportPositionF := PointF(FHorzScrollBar.Value,
    FVertScrollBar.Value);
  NodeVisibleChanged;
end;

procedure TQVirtualTreeView.DoSortColumnsChanged(Sender: TObject;
  const Item: TQVTColumn; Action: TCollectionNotification);
var
  I: Integer;
begin
  for I := 0 to FSortColumns.Count - 1 do
    FSortColumns[I].FSortIndex := I;
  DoSortMarkerChanged(Self);
end;

procedure TQVirtualTreeView.DoSortMarkerChanged(Sender: TObject);
begin
  if Assigned(FOnSortMarkerChanged) then
    FOnSortMarkerChanged(Self);
  Invalidate;
end;

function TQVirtualTreeView.EndEdit: Boolean;
begin
  if Assigned(FInplaceEditor) then
  begin
    Result := FInplaceEditor.EndEdit;
    if Result then
      FInplaceEditor := nil;
  end
  else
    Result := True;
end;

procedure TQVirtualTreeView.FocusChanged;
begin
  if Assigned(FOnFocusChanged) then
    FOnFocusChanged(Self, FocusNode, FocusColumn);
  Invalidate;
end;

function TQVirtualTreeView.FocusChanging(ANewNode: TQVTNode;
  ANewCol: Integer): Boolean;
begin
  Result := True;
  CheckForBrowseMode(Self);
  if Assigned(FOnFocusChanging) then
    FOnFocusChanging(Self, ANewNode, ANewCol, Result);
end;

function TQVirtualTreeView.GetCellData(ANode: TQVTNode; AColIndex: Integer)
  : IQVTCellData;
begin
  Result := nil;
  if Assigned(OnGetCellData) then
    OnGetCellData(Self, ANode, AColIndex, Result);
  if not Assigned(Result) then
    Result := TQVTDefaultCellData.Create(ANode, AColIndex);
end;

function TQVirtualTreeView.GetCellDrawer(ANode: TQVTNode; AColIndex: Integer)
  : IQVTDrawer;
var
  AType: TQVTDrawerType;
begin
  Result := nil;
  if Assigned(OnGetCellDrawer) then
    OnGetCellDrawer(Self, ANode, AColIndex, Result);
  if not Assigned(Result) then
  begin
    if (AColIndex >= 0) and (AColIndex < Header.Columns.Count) then
    begin
      if Assigned(ANode) then
        AType := Header.Columns[AColIndex].DrawerType
      else
        AType := Header.Columns[AColIndex].Title.DrawerType;
      if (AColIndex = Header.MasterColumn) and (AType = TQVTDrawerType.dtDefault)
      then
        Result := GetDefaultDrawer(TQVTDrawerType.dtTree)
      else
        Result := GetDefaultDrawer(AType);
    end
    else
      Result := GetDefaultDrawer(TQVTDrawerType.dtDefault);
  end;
end;

function TQVirtualTreeView.GetCellRect(ANode: TQVTNode;
  AColumn: Integer): TRectF;
var
  I: Integer;
  R: TRectF;
  ACol: TQVTColumn;
begin
  if FVisibleNodes.IndexOf(ANode) <> -1 then
  begin
    R := ANode.FDisplayRect;
    Result.Top := R.Top;
    Result.Bottom := R.Bottom;
    Result.Left := R.Left;
    for I := 0 to AColumn do
    begin
      ACol := Header.Columns[I];
      if ACol.Visible then
      begin
        Result.Right := Result.Left + ACol.Width;
        if I <> AColumn then
          Result.Left := Result.Right + FSpace.X
        else
          break;
      end
      else if I = AColumn then
      begin
        Result := TRect.Empty;
        Break;
      end;
    end;
    if Result.Right > R.Right then
      Result.Right := R.Right;
  end
  else
    Result := TRectF.Empty;
end;

function TQVirtualTreeView.GetChildCount(ANode: TQVTNode): Integer;
begin
  ANode.InitChildren;
  Result := ANode.FCount;
end;

class function TQVirtualTreeView.GetDefaultDrawer(AType: TQVTDrawerType)
  : IQVTDrawer;
begin
  if AType in [TQVTDrawerType.dtDefault .. TQVTDrawerType.dtCustom] then
    Result := DefaultDrawers[AType]
  else
    Result := nil;
end;

function TQVirtualTreeView.GetDisplayRect(ANode: TQVTNode): TRectF;
begin
  if FVisibleNodes.IndexOf(ANode) <> -1 then
    Result := ANode.FDisplayRect
  else
    Result := TRectF.Empty;
end;

function TQVirtualTreeView.GetFill: TBrush;
begin
  Result := FFill;
end;

function TQVirtualTreeView.GetNext(ANode: TQVTNode): TQVTNode;
begin
  if TQVTNodeState.nsHasChildren in ANode.States then
  begin
    Result := ANode.GetFirstChild;
    if Assigned(Result) then
      Exit;
  end;
  repeat
    Result := ANode.GetNext;
    if not Assigned(Result) then
      ANode := ANode.Parent
    else
      Break;
  until not Assigned(ANode);
end;

function TQVirtualTreeView.GetNextVisible(ANode: TQVTNode): TQVTNode;
begin
  if ANode.HasChildren and (TQVTNodeState.nsExpanded in ANode.FStates) then
  begin
    Result := ANode.GetFirstVisibleChild;
    if Assigned(Result) then
      Exit;
  end;
  repeat
    Result := ANode.GetNext;
    if not Assigned(Result) then
      ANode := ANode.Parent
    else if TQVTNodeState.nsVisible in Result.FStates then
      Break;
  until not Assigned(ANode);
end;

function TQVirtualTreeView.GetNodeAt(const APos: TPointF; AColumn: PInteger)
  : TQVTNode;
var
  Dummy: Integer;
begin
  DoHitTest(APos, Result, Dummy);
  if Assigned(AColumn) then
    AColumn^ := Dummy;
end;

function TQVirtualTreeView.GetPriorVisible(ANode: TQVTNode): TQVTNode;
var
  APrior: TQVTNode;
begin
  Result := nil;
  APrior := ANode.GetPriorVisible(ANode);
  if not Assigned(APrior) then
  begin
    if ANode.Parent <> RootNode then
      Result := ANode.Parent;
  end
  else
  begin
    while APrior.HasChildren and (TQVTNodeState.nsExpanded in APrior.FStates) do
    begin
      Result := APrior.GetLastVisibleChild;
      if Assigned(Result) then
        APrior := Result;
    end;
    Result := APrior;
  end;
end;

function TQVirtualTreeView.GetRootNode: TQVTNode;
begin
  if not Assigned(FRootNode) then
  begin
    FRootNode := CreateNode;
    FRootNode.FCount := 0; // 初始为没有子结点
    FRootNode.FStates := FRootNode.FStates + [nsExpanded, nsInitialized];
    FRootNode._AddRef;
    // 增加引用计数，避免被释放
  end;
  Result := FRootNode;
end;

function TQVirtualTreeView.GetRootNodeCount: Integer;
begin
  Result := RootNode.ChildCount;
end;

function TQVirtualTreeView.GetSortColumns: TQVTSortColumns;
begin
  if not Assigned(FSortColumns) then
  begin
    FSortColumns := TQVTSortColumns.Create;
    FSortColumns.OnNotify := DoSortColumnsChanged;
  end;
  Result := FSortColumns;
end;

function TQVirtualTreeView.GetStroke: TStrokeBrush;
begin
  Result := FStroke;
end;

function TQVirtualTreeView.DoHitTest(const APos: TPointF; var ANode: TQVTNode;
  var AColumn: Integer): TQVTHitTestResult;
var
  I: Integer;
  L, T, AHeaderBottom: Single;
  AColResizable, ARowResizable: Boolean;
  LR: TRectF;
  ACurrentNode: TQVTNode;
const
  MinSpace = 4;
  MinHalfSpace = 2;
begin
  Result := hrNone;
  AColumn := -1;
  ANode := nil;
  LR := LocalRect;
  LR.Left := LR.Left + Padding.Left;
  LR.Top := LR.Top + Padding.Top;
  LR.Right := LR.Right - Padding.Right;
  LR.Bottom := LR.Bottom - Padding.Bottom;
  // 不再显示区域内，则忽略
  if (APos.Y < LR.Top) or (APos.Y > LR.Bottom) or (APos.X < LR.Left) or
    (APos.X > LR.Right) then
    Exit;
  // 如果标题行可见，计算标题行下边界
  if TQVTHeaderOption.hoVisible in Header.Options then
  begin
    AHeaderBottom := Padding.Top + Header.Height;
    if TQVTPaintOption.poHorizLine in PaintOptions then
      AHeaderBottom := AHeaderBottom + Stroke.Thickness;
  end
  else
    AHeaderBottom := LR.Top;
  AColResizable := (TQVTHeaderOption.hoResizable in Header.Options);
  L := LR.Left;
  if TQVTPaintOption.poVertLine in PaintOptions then
    L := L + Stroke.Thickness;
  // X 坐标不在范围内的话，直接退出
  if (APos.X < L) or (APos.X > FPaintOffset.X + FHeader.Width + 5) or
    (APos.X > Width - Padding.Right) then
    Exit;
  L := L - FPaintOffset.X;
  for I := 0 to FHeader.Columns.Count - 1 do
  begin
    L := L + FHeader.Columns[I].Width;
    if L > APos.X then
    begin
      AColumn := I;
      if (Result = TQVTHitTestResult.hrNone) and (APos.Y < AHeaderBottom) then
        Result := TQVTHitTestResult.hrHeader;
      Break;
    end;
    if AColResizable then // 判断是否落在分隔线上
    begin
      if FSpace.X < MinSpace then
      begin
        T := FSpace.X / 2;
        if T < MinHalfSpace then
          T := MinHalfSpace;
        if (APos.X > L - T) and (APos.X < L + T) then
          Result := TQVTHitTestResult.hrColumnSpace;
      end
      else if (APos.X > L) and (APos.X < L + FSpace.X) then
        Result := TQVTHitTestResult.hrColumnSpace;
    end;
    L := L + FSpace.X;
  end;
  ARowResizable := TQVTOption.toRowSizable in Options;
  // 获取结点
  if Assigned(FFirstVisibleNode) then
  begin
    if APos.Y < AHeaderBottom then
      Exit;
    for I := 0 to FVisibleNodes.Count - 1 do
    begin
      ACurrentNode := FVisibleNodes[I];
      if ARowResizable then
      begin
        if FSpace.Y < MinSpace then
        begin
          T := FSpace.Y / 2;
          if T < MinHalfSpace then
            T := MinHalfSpace;
          if (APos.Y < ACurrentNode.FDisplayRect.Top + T) and
            (APos.Y > ACurrentNode.FDisplayRect.Top - T) then
          begin
            Result := TQVTHitTestResult.hrRowSpace;
            ANode := ACurrentNode;
            Break;
          end;
        end
        else if (APos.Y < ACurrentNode.FDisplayRect.Top) and
          (APos.Y > ACurrentNode.FDisplayRect.Top - FSpace.Y) then
        begin
          Result := TQVTHitTestResult.hrRowSpace;
          if I > 0 then
            ANode := ACurrentNode;
        end;
      end;
      if PtInRect(ACurrentNode.FDisplayRect, APos) then
      begin
        ANode := FVisibleNodes[I];
        if Result = TQVTHitTestResult.hrNone then
          Result := TQVTHitTestResult.hrNode;
        Break;
      end;
    end;
  end;
end;

procedure TQVirtualTreeView.InitNode(ANode: TQVTNode);
begin
  if Assigned(FOnInitNode) then
    FOnInitNode(Self, ANode);
  ANode.FStates := ANode.FStates + [TQVTNodeState.nsInitialized];
end;

procedure TQVirtualTreeView.Invalidate;
begin
  InvalidateRect(LocalRect);
end;

procedure TQVirtualTreeView.InvalidateColumn(const AColumn: Integer);
begin
  // 暂时全刷新
  Invalidate;
end;

procedure TQVirtualTreeView.InvalidateNode(const ANode: TQVTNode);
begin
  // 暂时全刷新
  Invalidate;
end;

procedure TQVirtualTreeView.MakeNodeVisible(ANode: TQVTNode;
  ACenterInView: Boolean);
var
  AMin, AMax, APos, H: Single;
  R: TRectF;
begin
  CheckScrollBars;
  if FVisibleNodes.IndexOf(ANode) = -1 then
  begin
    AMin := ANode.FDistance;
    AMax := ANode.FDistance + ANode.Height;
    R := LocalRect;
    if TQVTHeaderOption.hoVisible in Header.Options then
      R.Top := R.Top + Padding.Top + Header.Height + FSpace.Y
    else
      R.Top := R.Top + Padding.Top;
    R.Bottom := R.Bottom - Padding.Bottom;
    // H := R.Height;
    // 暂时直接过去
    FVertScrollBar.Value := AMin;
  end;
  // if (FVertScrollBar.Value-AMin)<H then

end;

procedure TQVirtualTreeView.MouseClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  ANode: TQVTNode;
  ACol: Integer;
  pt: TPointF;
  AHitTest: TQVTHitTestResult;
  procedure DoTitleClick;
  var
    AColumn: TQVTColumn;
    I: Integer;
  begin
    AColumn := Header.Columns[ACol];
    if AColumn.Title.Clickable then
    begin
      if Assigned(FOnTitleClick) then
        FOnTitleClick(Self, ACol);
      if AColumn.Title.SortOnClick then
      begin
        case AColumn.Title.SortMarker of
          smNone:
            AColumn.Title.FSortMarker := smAsc;
          smAsc:
            AColumn.Title.FSortMarker := smDesc;
          smDesc:
            AColumn.Title.FSortMarker := smNone;
        end;
        if AColumn.Title.SortMarker <> smNone then
        begin
          if TQVTHeaderOption.hoMultiSortColumns in Header.Options then
          begin
            if SortColumns.IndexOf(AColumn) = -1 then
              FSortColumns.Add(AColumn)
            else
              DoSortMarkerChanged(Self);
          end
          else
          begin
            SortColumns.OnNotify := nil;
            I := FSortColumns.Count - 1;
            while I >= 0 do
            begin
              if FSortColumns[I] <> AColumn then
              begin
                with FSortColumns[I] do
                begin
                  FSortIndex := -1;
                  Title.FSortMarker := TQVTColumnSortMarker.smNone;
                end;
              end;
              FSortColumns.Delete(FSortColumns.Count - 1);
              Dec(I);
            end;
            FSortColumns.OnNotify := DoSortColumnsChanged;
            FSortColumns.Add(AColumn);
          end
        end
        else
          FSortColumns.Remove(AColumn);
      end;
      FocusColumn := ACol;
    end;
  end;

begin
  inherited;
  pt := PointF(X, Y);
  AHitTest := DoHitTest(FMouseDownPos, ANode, ACol);
  if Assigned(ANode) then
  begin
    // 点击展开/收缩按钮
    if ANode.HasChildren and PtInRect(ANode.FButtonRect, pt) then
    begin
      if TQVTNodeState.nsExpanded in ANode.FStates then
        ANode.States := ANode.States - [TQVTNodeState.nsExpanded]
      else
        ANode.Expand(false);
    end
    else
    begin
      if FocusChanging(ANode, ACol) then
      begin
        FFocusColumn := ACol;
        FFocusNode := ANode;
        FocusChanged;
      end;
      CellClick(ANode, ACol, pt);
    end;
  end
  else if AHitTest in [TQVTHitTestResult.hrNone] then
  begin
    if FocusChanging(nil, -1) then
    begin
      FFocusColumn := -1;
      FFocusNode := nil;
      FocusChanged;
    end;
  end
  else if AHitTest = TQVTHitTestResult.hrHeader then
    DoTitleClick;
end;

procedure TQVirtualTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  AHitTest: TQVTHitTestResult;
begin
  inherited;
  if (([TQVTOption.toTestHover, TQVTOption.toRowSizable] * Options) <> []) or
    (TQVTHeaderOption.hoResizable in Header.Options) then
  begin
    FMouseDownPos.X := X;
    FMouseDownPos.Y := Y;
    FMouseDownOffset.X := HorzScrollBar.Value;
    FMouseDownOffset.Y := VertScrollBar.Value;
    FMouseDownTime := TThread.GetTickCount;
    AHitTest := DoHitTest(FMouseDownPos, FMouseDownNode, FMouseDownColumn);
    case AHitTest of
      hrNode, hrHeader:
        // To drag node?
        begin
          if Header.Columns[FMouseDownColumn].Title.Clickable then
            Invalidate;
        end;
      hrColumnSpace:
        begin
          if FMouseDownColumn = -1 then
            FSizingColumn := FHeader.Columns[FHeader.Columns.Count - 1]
          else
            FSizingColumn := FHeader.Columns[FMouseDownColumn - 1];
          FSizingColumnOriginWidth := FSizingColumn.Width;
          FStates := FStates + [TQVTState.tsColSizing];
          Capture;
        end;
      hrRowSpace:
        begin
          if not Assigned(FMouseDownNode) then
            DoHitTest(FMouseDownPos, FMouseDownNode, FMouseDownColumn);
          FSizingNode := GetPriorVisible(FMouseDownNode);
          if Assigned(FSizingNode) then
            FSizingNodeOriginHeight := FSizingNode.Height
          else // 最前面调整标题高度
            FSizingNodeOriginHeight := FHeader.Height;
          FStates := FStates + [TQVTState.tsRowSizing];
          Capture;
        end;
    end;
    if (Button = TMouseButton.mbLeft) and
      (FStates * [TQVTState.tsColSizing, TQVTState.tsRowSizing] = []) then
    begin
      FAniCalculations.Averaging := ssTouch in Shift;
      FAniCalculations.MouseDown(X, Y);
    end;
  end;
end;

procedure TQVirtualTreeView.MouseMove(Shift: TShiftState; X, Y: Single);
var
  ANode: TQVTNode;
  ACol: Integer;
  AHitTest: TQVTHitTestResult;
begin
  if TQVTState.tsColSizing in FStates then
  begin
    FSizingColumn.Width := FSizingColumnOriginWidth - (FMouseDownPos.X - X);
    AdjustAutoSizeColumn;
  end
  else if TQVTState.tsRowSizing in FStates then
  begin
    if Assigned(FSizingNode) then
      FSizingNode.Height := FSizingNodeOriginHeight - (FMouseDownPos.Y - Y)
    else
      FHeader.Height := FSizingNodeOriginHeight - (FMouseDownPos.Y - Y);
  end
  else if TQVTState.tsScrolling in FStates then
  begin
    HorzScrollBar.Value := FMouseDownOffset.X + (FMouseDownPos.X - X);
    VertScrollBar.Value := FMouseDownOffset.Y + (FMouseDownPos.Y - Y);
  end
  else
  begin
    if (([TQVTOption.toTestHover, TQVTOption.toRowSizable] * Options) <> []) or
      (TQVTHeaderOption.hoResizable in Header.Options) then
    begin
      AHitTest := DoHitTest(PointF(X, Y), ANode, ACol);
      case AHitTest of
        hrNone, hrHeader:
          Cursor := crDefault;
        hrNode:
          begin
            Cursor := crDefault;
            if (TQVTOption.toTestHover in Options) and
              ((ANode <> FHoverNode) or (ACol <> FHoverColumn)) then
            begin
              FHoverNode := ANode;
              FHoverColumn := ACol;
              if Assigned(FOnHoverChanged) then
                FOnHoverChanged(Self);
              if TQVTPaintOption.poHover in PaintOptions then
                Invalidate;
            end;
          end;
        hrColumnSpace:
          Cursor := crHSplit;
        hrRowSpace:
          Cursor := crVSplit;
      end;
    end;
    if (ssTouch in Shift) and ([TQVTState.tsDragging, TQVTState.tsScrolling] *
      FStates = []) then
    begin
      if (TThread.GetTickCount - FMouseDownTime < 500) or
        (DragMode <> TDragMode.dmManual) then
      begin
        FStates := FStates + [TQVTState.tsScrolling];
      end;
    end;
    if FAniCalculations.Down then
      FAniCalculations.MouseMove(X, Y);
  end;
  inherited;
end;

procedure TQVirtualTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if FStates * [TQVTState.tsColSizing, TQVTState.tsRowSizing] <> [] then
    NodeContentChanged
  else if Button = TMouseButton.mbLeft then
    FAniCalculations.MouseUp(X, Y);
  FStates := FStates - [TQVTState.tsColSizing, TQVTState.tsRowSizing,
    TQVTState.tsScrolling, TQVTState.tsDragging];
  if FMouseDownColumn <> -1 then
    Invalidate;
  FMouseDownColumn := -1;
  FMouseDownNode := nil;
  ReleaseCapture;
end;

procedure TQVirtualTreeView.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
  procedure DoWheel(ABar: TScrollBar);
  var
    V: Single;
  begin
    V := ABar.Value - WheelDelta / 10;
    if V > ABar.Max then
      ABar.Value := ABar.Max
    else if V < ABar.Min then
      ABar.Value := ABar.Min
    else
      ABar.Value := V;
  end;

begin
  inherited;
  if ssCtrl in Shift then
    DoWheel(FHorzScrollBar)
  else
    DoWheel(FVertScrollBar);
end;

procedure TQVirtualTreeView.NodeContentChanged;
begin
  FStates := FStates + [TQVTState.tsVisibleChanged, TQVTState.tsContentChanged];
  Invalidate;
end;

procedure TQVirtualTreeView.NodeVisibleChanged;
begin
  FStates := FStates + [TQVTState.tsVisibleChanged];
  Invalidate;
end;

procedure TQVirtualTreeView.Paint;
var
  R, AClientRect: TRectF;
  AMaxRight: Single;
  ADrawer: IQVTDrawer;
  ACol: TQVTColumn;
  ANode: TQVTNode;
  AState: TCanvasSaveState;
  procedure DrawGridLines;
  var
    pt1, pt2: TPointF;
    I: Integer;
    YStart, W: Single;
    ADrawHorz, ADrawVert: Boolean;
  begin
    W := FHeader.Width - FPaintOffset.X;
    ADrawHorz := TQVTPaintOption.poHorizLine in PaintOptions;
    ADrawVert := TQVTPaintOption.poVertLine in PaintOptions;
    // 先画水平线
    pt1.X := AClientRect.Left;
    pt2.X := pt1.X + W;
    if TQVTHeaderOption.hoVisible in Header.Options then
      YStart := AClientRect.Top + Header.Height + FSpace.Y - FPaintOffset.Y
    else
      YStart := AClientRect.Top - FPaintOffset.Y;
    pt1.Y := YStart;
    for I := 0 to FVisibleNodes.Count do
    begin
      pt2.Y := pt1.Y;
      if ADrawHorz then
        Canvas.DrawLine(pt1.SnapToPixel(Canvas.Scale),
          pt2.SnapToPixel(Canvas.Scale), Opacity, FStroke);
      if I < FVisibleNodes.Count then
      begin
        pt1.Y := pt1.y + FVisibleNodes[I].Height;
        if I + 1 < FVisibleNodes.Count then
          pt1.Y := pt1.Y + FSpace.Y;
      end;
    end;
    if ADrawVert then
    begin
      pt1 := AClientRect.TopLeft;
      pt1.X := pt1.X - FPaintOffset.X;
      pt1.Y := YStart;
      for I := 0 to FHeader.Columns.Count do
      begin
        pt2.X := pt1.X;
        if I >= FFirstVisibleColumn then
          Canvas.DrawLine(pt1.SnapToPixel(Canvas.Scale),
            pt2.SnapToPixel(Canvas.Scale), Opacity, FStroke);
        if I < FHeader.Columns.Count then
        begin
          pt1.x := pt1.x + FHeader.Columns[I].Width;
          if I + 1 < FHeader.Columns.Count then
            pt1.X := pt1.X + FSpace.X;
        end;
      end;
    end;
  end;
  function DrawHeader: Boolean;
  var
    I: Integer;
  begin
    Result := TQVTHeaderOption.hoVisible in FHeader.Options;
    R := AClientRect;
    if Result then
    begin
      R.Bottom := R.Top + FHeader.Height;
      R.Left := R.Left - FPaintOffset.X;
      Canvas.FillRect(R, 0, 0, [], Opacity, Fill);
      for I := 0 to FHeader.Columns.Count - 1 do
      begin
        ACol := FHeader.Columns[I];
        R.Right := R.Left + ACol.Width;
        if R.Right > AMaxRight then
          R.Right := AMaxRight;
        if I >= FFirstVisibleColumn then
        begin
          ADrawer := ACol.Title.Drawer;
          if Assigned(ADrawer) then
            ADrawer.Draw(R, ACol.Title as IQVTCellData);
        end;
        R.Left := R.Right + FSpace.x;
        if R.Left > AMaxRight then
          Break;
      end;
    end
  end;

  procedure CalcVisibleNodes;
  var
    W: Single;
  begin
    W := Header.Width;
    if TQVTState.tsVisibleChanged in FStates then
    begin
      CheckScrollBars;
      FVisibleNodes.Clear;
      if not Assigned(FFirstVisibleNode) then
        FFirstVisibleNode := RootNode.GetFirstVisibleChild;
      if Assigned(FFirstVisibleNode) then
      begin
        ANode := FFirstVisibleNode;
        R.Left := AClientRect.Left - FPaintOffset.X;
        R.Right := R.Left + W;
        if R.Right > AClientRect.Right then
          R.Right := AClientRect.Right;
        while Assigned(ANode) do
        begin
          ANode.NeedInitialized;
          ANode.FVisibleRowIndex := FVisibleNodes.Add(ANode);
          R.Bottom := R.Top + ANode.GetHeight;
          ANode.FDisplayRect := R;
          R.Top := R.Bottom + FSpace.Y;
          if R.Top > AClientRect.Bottom then
            break;
          ANode := GetNextVisible(ANode);
        end;
      end;
      FStates := FStates - [TQVTState.tsVisibleChanged];
    end;
  end;

  procedure SetEditorPos(R: TRectF);
  var
    AEditingNode: TQVTNode;
    AEditingCol: Integer;
  begin
    if R.Left < AClientRect.Left then
      R.Left := AClientRect.Left;
    if R.Right > AClientRect.Right then
      R.Right := AClientRect.Right;
    FInplaceEditor.SetBounds(R);
    FInplaceEditor.Show;
  end;

  procedure DrawNodes;
  var
    I, J, AEditingCol: Integer;
    ADrawHover: Boolean;
    ADrawRowSelection, ADrawColSelection, AdjustEditor, AEditorHide: Boolean;
  begin
    ADrawHover := Assigned(FHoverNode) and
      (TQVTPaintOption.poHover in PaintOptions);
    ADrawRowSelection := TQVTPaintOption.poRowSelection in PaintOptions;
    ADrawColSelection := TQVTPaintOption.poColSelection in PaintOptions;
    AEditorHide := Assigned(FInplaceEditor);
    for I := 0 to FVisibleNodes.Count - 1 do
    begin
      ANode := FVisibleNodes[I];
      AdjustEditor := False;
      if Assigned(FInplaceEditor) then
      begin
        if FInplaceEditor.GetEditing(AEditingCol) = ANode then
        begin
          AdjustEditor := True;
          AEditorHide := false;
        end;
      end;
      R := ANode.FDisplayRect;
      if ADrawHover and (ANode = FHoverNode) then
      begin
        if (ANode <> FFocusNode) or (not ADrawRowSelection) then
          Canvas.ClearRect(R, FHoverColor);
      end;
      if ADrawRowSelection and (ANode = FFocusNode) then
        Canvas.ClearRect(R, FSelectionColor);
      for J := 0 to FHeader.Columns.Count - 1 do
      begin
        ACol := FHeader.Columns[J];
        R.Right := R.Left + ACol.Width;
        // if R.Right > AMaxRight then
        // R.Right := AMaxRight;
        ADrawer := ANode.CellDrawer[J];
        if Assigned(ADrawer) then
        begin
          if ADrawHover and (J = FHoverColumn) then
          begin
            if (J <> FFocusColumn) or (not ADrawColSelection) then
              Canvas.ClearRect(R, FHoverColor);
          end;
          if ADrawColSelection and (J = FFocusColumn) then
            Canvas.ClearRect(R, FSelectionColor);
          if J >= FFirstVisibleColumn then
            ADrawer.Draw(R, ANode.CellData[J]);
        end;
        if AdjustEditor and (J = AEditingCol) then
          SetEditorPos(R);
        R.Left := R.Right + FSpace.x;
        if R.Left > AMaxRight then
          Break;
      end;
    end;
    if AEditorHide then
      FInplaceEditor.Hide
    else if Assigned(FInplaceEditor) then
      FInplaceEditor.Show;
  end;

begin
  R := LocalRect;
  if Canvas.BeginScene then
  begin
    AState := Canvas.SaveState;
    try
      Canvas.FillRect(R, 0, 0, [], Opacity, Fill);
      Canvas.DrawRect(R.SnapToPixel(Canvas.Scale), 0, 0, [], Opacity, Stroke);
      if not Assigned(FTextLayout) then
        FTextLayout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType)
          .Create(Canvas);
      AClientRect := RectF(Padding.Left, Padding.Top, Width - Padding.Right,
        Height - Padding.Top);
      if HorzScrollBar.Visible then
        AClientRect.Bottom := AClientRect.Bottom - HorzScrollBar.Height;
      if VertScrollBar.Visible then
        AClientRect.Right := AClientRect.Right - VertScrollBar.Width;
      R := AClientRect;
      AMaxRight := AClientRect.Right;
      if TQVTHeaderOption.hoVisible in FHeader.Options then
        R.Top := R.Top + FHeader.Height + FSpace.Y
      else
        R.Top := R.Top + FSpace.X;
      R.Top := R.Top - FPaintOffset.Y;
      // 设置结点绘制区域
      Canvas.IntersectClipRect(RectF(R.Left, Padding.Top, R.Right, R.Bottom));
      R.Left := R.Left - FPaintOffset.X;
      CalcVisibleNodes;
      DrawNodes;
      DrawGridLines;
      DrawHeader;
    finally
      Canvas.RestoreState(AState);
      Canvas.EndScene;
    end;
  end;
end;

procedure TQVirtualTreeView.Resize;
begin
  inherited;
  AdjustAutoSizeColumn;
  NodeContentChanged;
end;

procedure TQVirtualTreeView.RowDirty(ARowIndex: Integer);
begin
  if ARowIndex < FDirtyRowIndex then
    FDirtyRowIndex := ARowIndex;
end;

procedure TQVirtualTreeView.ScrollBy(dx, dy: Single);
begin
  VertScrollBar.Value := VertScrollBar.Value + dy;
  HorzScrollBar.Value := HorzScrollBar.Value + dx;
end;

procedure TQVirtualTreeView.SetFocusColumn(const Value: Integer);
begin
  if FFocusColumn <> Value then
  begin
    FFocusColumn := Value;
    Invalidate;
  end;
end;

procedure TQVirtualTreeView.SetFocusNode(const Value: TQVTNode);
begin
  if FFocusNode <> Value then
  begin
    if (Value = nil) or (Value.CanFocus) then
    begin
      FFocusNode := Value;
      if Assigned(Value) then
        Value.ScrollToView;
      Invalidate;
    end;
  end;
end;

procedure TQVirtualTreeView.SetPaintOptions(const Value: TQVTPaintOptions);
begin
  if FPaintOptions <> Value then
  begin
    FPaintOptions := Value;
    NodeContentChanged;
  end;
end;

procedure TQVirtualTreeView.SetRootNodeCount(const Value: Integer);
begin
  if RootNode.ChildCount <> Value then
  begin
    RootNode.ChildCount := Value;
    RootNode.States := [TQVTNodeState.nsInitialized,
      TQVTNodeState.nsHasChildren, TQVTNodeState.nsExpanded];
  end;
end;

procedure TQVirtualTreeView.SetSpace(const Value: TPointF);
begin
  if not FSpace.EqualsTo(Value) then
  begin
    FSpace := Value;
    Invalidate;
  end;
end;

{ TQVTColumn }

procedure TQVTColumn.Assign(src: TPersistent);
var
  ASource: TQVTColumn;
begin
  inherited;
  if src is TQVTColumn then
  begin
    ASource := TQVTColumn(src);
    FTitle.Assign(ASource.Title);
    FTextSettingsInfo.TextSettings.Assign(ASource.TextSettings);
    FFrozen := ASource.Frozen;
    FVisible := ASource.Visible;
    FEnabled := ASource.Enabled;
    FReadOnly := ASource.ReadOnly;
    TreeView.InvalidateColumn(Index);
  end;
end;

constructor TQVTColumn.Create(ACollection: TCollection);
begin
  inherited;
  FTitle := TQVTColumnTitle.Create(Self);
  FTitle._AddRef;
  FFrozen := false;
  FVisible := true;
  FEnabled := true;
  FReadOnly := false;
  FWidth := 75; // 默认大小
  FDrawerType := TQVTDrawerType.dtDefault;
  FSortIndex := -1;
  FTextSettingsInfo := TTextSettingsInfo.Create(Self,
    TreeView.GetTextSettingsClass);
end;

destructor TQVTColumn.Destroy;
begin
  FTitle._Release;
  FreeAndNil(FTextSettingsInfo);
  inherited;
end;

function TQVTColumn.GetDrawer: IQVTDrawer;
begin
  if not Assigned(FDrawer) then
    Result := TreeView.GetCellDrawer(nil, Index);
  Result := FDrawer;
end;

function TQVTColumn.GetTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

function TQVTColumn.GetTreeView: TQVirtualTreeView;
begin
  Result := ((Collection as TQVTColumns).Owner as TQVTHeader).TreeView;
end;

procedure TQVTColumn.SetDrawerType(const Value: TQVTDrawerType);
begin
  if FDrawerType <> Value then
  begin
    FDrawerType := Value;
    FDrawer := nil;
  end;
end;

procedure TQVTColumn.SetEnabled(value: Boolean);
begin
  FEnabled := Value;
  if not Value then
    TreeView.CheckForBrowseMode(Self);
end;

procedure TQVTColumn.SetFrozen(const Value: Boolean);
begin
  FFrozen := Value;
end;

procedure TQVTColumn.SetMaxWidth(const Value: Single);
begin
  if not SameValue(FMaxWidth, Value) then
  begin
    FMaxWidth := Value;
    if (Value > 0) and (FMaxWidth < Width) then
      Width := MaxWidth;
  end;
end;

procedure TQVTColumn.SetMinWidth(const Value: Single);
begin
  if not SameValue(FMinWidth, Value) then
  begin
    FMinWidth := Value;
    if (Value > 0) and (FMinWidth > Value) then
      Width := FMinWidth;
  end;
end;

procedure TQVTColumn.SetReadOnly(value: Boolean);
begin
  FReadOnly := Value;
  if Value then
    TreeView.CheckForBrowseMode(Self);
end;

procedure TQVTColumn.SetSortIndex(const Value: Integer);
begin
  if FSortIndex <> Value then
  begin
    if Value < 0 then
      TreeView.SortColumns.Remove(Self)
    else
    begin
      if FSortIndex >= 0 then
        TreeView.SortColumns.Delete(FSortIndex);
      TreeView.SortColumns.Insert(Value, Self);
    end;
  end;
end;

procedure TQVTColumn.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
  TreeView.InvalidateColumn(Index);
end;

procedure TQVTColumn.SetTitle(const ATitle: TQVTColumnTitle);
begin
  FTitle.Assign(ATitle);
end;

procedure TQVTColumn.SetVisible(value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    TreeView.Invalidate;
  end;
end;

procedure TQVTColumn.SetWidth(const Value: Single);
begin
  if not SameValue(FWidth, Value) then
  begin
    if Value >= MinWidth then
    begin
      if (Value <= MaxWidth) or IsZero(MaxWidth) then
        FWidth := Value
      else
        FWidth := MaxWidth;
    end
    else
      FWidth := MinWidth;
    if Visible then
      TreeView.NodeContentChanged;
  end;
end;

{ TQVTColumns }

function TQVTColumns.Add: TQVTColumn;
begin
  Result := inherited Add as TQVTColumn;
  if Result.Index = Result.TreeView.Header.AutoSizeColumn then
    Result.TreeView.Resize;
end;

constructor TQVTColumns.Create(AOwner: TPersistent);
begin
  inherited Create(TQVTColumn);
  FOwner := AOwner;
end;

function TQVTColumns.GetItems(const AIndex: Integer): TQVTColumn;
begin
  Result := GetItem(AIndex) as TQVTColumn;
end;

function TQVTColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TQVTNode }

function TQVTNode.AddChild: TQVTNode;
begin
  NeedInitialized;
  GetLastChild;
  Result := TreeView.CreateNode;
  Pointer(Result.FParent) := Self;
  Pointer(Result.FPrior) := FLastChild;
  Result.FIndex := FCount;
  if Assigned(FLastChild) then
    Pointer(FLastChild.FNext) := Result;
  FStates := FStates + [TQVTNodeState.nsHasChildren];
  Result._AddRef;
  FLastInitChild := Result;
  Result.NeedInitialized;
  if not Assigned(FFirstChild) then
    Pointer(FFirstChild) := Result;
  Pointer(FLastChild) := Result;
  Inc(FCreatedCount);
  Inc(FCount);
  if TQVTNodeState.nsExpanded in FStates then
    TreeView.NodeContentChanged;
end;

procedure TQVTNode.CleanDirty(ANode: TQVTNode);
var
  AIndex: Integer;
begin
  if Assigned(FFirstDirtyChild) then
  begin
    if Assigned(FFirstDirtyChild.FPrior) then
      AIndex := FFirstDirtyChild.FPrior.FIndex + 1
    else
      AIndex := 0;
    while Assigned(FFirstDirtyChild) do
    begin
      FFirstDirtyChild.FIndex := AIndex;
      if FFirstDirtyChild = ANode then
      begin
        Pointer(FFirstDirtyChild) := FFirstDirtyChild.Next;
        Break;
      end
      else
        Pointer(FFirstDirtyChild) := FFirstDirtyChild.Next;;
    end;
  end;
end;

procedure TQVTNode.Clear;
var
  ANext: TQVTNode;
begin
  FCount := 0;
  while Assigned(FFirstChild) do
  begin
    Pointer(ANext) := FFirstChild.FNext;
    FFirstChild._Release;
    Pointer(FFirstChild) := ANext;
  end;
  Pointer(FLastChild) := nil;
  Pointer(FLastInitChild) := nil;
  Pointer(FFirstDirtyChild) := nil;
  FCount := 0;
  FStates := FStates - [TQVTNodeState.nsHasChildren];
end;

constructor TQVTNode.Create(AOwner: TQVirtualTreeView);
begin
  inherited Create;
  Pointer(FTreeView) := AOwner;
  FCount := -1;
  FLevel := -1;
  FRowIndex := -1;
  FHeight := -1;
  FIndex := -1;
  FStates := [TQVTNodeState.nsVisible];
end;

procedure TQVTNode.Delete;
begin
  FParent.Dirty(FNext);
  if Assigned(FPrior) then
    Pointer(FPrior.FNext) := Next;
  if Assigned(FNext) then
    Pointer(FNext.FPrior) := Prior;
  if Assigned(FParent) then
  begin
    Dec(FParent.FCount);
    Dec(FParent.FCreatedCount);
  end;
  if RowIndex > 0 then
    TreeView.RowDirty(RowIndex);
  _Release;
end;

destructor TQVTNode.Destroy;
begin
  Clear;
  Pointer(FParent) := nil;
  Pointer(FNext) := nil;
  Pointer(FPrior) := nil;
  if Assigned(FExts) then
    FreeAndNil(FExts);
  Pointer(FTreeView) := nil;
  inherited;
end;

procedure TQVTNode.Dirty(ANode: TQVTNode);
begin
  if Assigned(ANode) then
  begin
    if not Assigned(FFirstDirtyChild) then
      FFirstDirtyChild := ANode
    else if ANode.FIndex < FFirstDirtyChild.FIndex then
      FFirstDirtyChild := ANode;
  end;
end;

procedure TQVTNode.Expand(const ANest: Boolean);
var
  AChild: TQVTNode;
begin
  if HasChildren then
  begin
    InitChildren;
    FStates := FStates + [TQVTNodeState.nsExpanded];
    if ANest then
    begin
      AChild := GetFirstVisibleChild;
      while Assigned(AChild) do
      begin
        AChild.Expand(ANest);
        AChild := GetNextVisibleChild(AChild);
      end;
    end
  end;
  TreeView.NodeContentChanged;
end;

function TQVTNode.GetCanFocus: Boolean;
var
  AParent: TQVTNode;
begin
  Result := TQVTNodeState.nsVisible in FStates;
  if Result then
  begin
    AParent := Parent;
    while Assigned(AParent) and (AParent <> TreeView.RootNode) do
    begin
      if not(TQVTNodeState.nsVisible in AParent.FStates) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TQVTNode.GetCellData(const AIndex: Integer): IQVTCellData;
begin
  Result := TreeView.GetCellData(Self, AIndex);
end;

function TQVTNode.GetCellDrawer(const AIndex: Integer): IQVTDrawer;
begin
  Result := TreeView.GetCellDrawer(Self, AIndex);
end;

function TQVTNode.GetCellRect(AColumn: Integer): TRectF;
begin
  Result := TreeView.GetCellRect(Self, AColumn);
end;

function TQVTNode.GetChildCount: Integer;
begin
  if FCount < 0 then
  begin
    Result := TreeView.GetChildCount(Self);
    FCount := Result;
  end;
  Result := FCount;
end;

function TQVTNode.GetDisplayRect: TRectF;
begin
  Result := TreeView.DisplayRect[Self];
end;

function TQVTNode.GetExts: TList<IInterface>;
begin
  if not Assigned(FExts) then
    FExts := TList<IInterface>.Create;
  Result := FExts;
end;

function TQVTNode.GetFirstChild: TQVTNode;
begin
  NeedInitialized;
  // 如果有子结点
  if (TQVTNodeState.nsHasChildren in FStates) then
  begin
    // 检查第一个子结点是否赋值了
    if not Assigned(FFirstChild) then
    begin
      Pointer(FFirstChild) := TreeView.CreateNode;
      FFirstChild.FIndex := 0;
      Pointer(FFirstChild.FParent) := Self;
      FFirstChild.FRowIndex := RowIndex + 1;
      FFirstChild._AddRef;
      Pointer(FLastInitChild) := FFirstChild;
      FCreatedCount := 1;
      if FCount = 1 then // 只有一个时，也同时是尾结点
        Pointer(FLastChild) := FFirstChild;
    end;
  end;
  Result := FFirstChild;
end;

function TQVTNode.GetFirstChildData: IQVTNodeData;
begin
  Result := GetFirstChild as IQVTNodeData;
end;

function TQVTNode.GetFirstVisibleChild: TQVTNode;
begin
  Result := GetFirstChild;
  while Assigned(Result) and (not(TQVTNodeState.nsVisible in Result.FStates)) do
    Result := Result.GetNext;
end;

function TQVTNode.GetHeight: Single;
begin
  if FHeight < 0 then
    Result := TreeView.DefaultRowHeight
  else
    Result := FHeight;
end;

function TQVTNode.GetIndex: Integer;
begin
  if Assigned(FParent) then
    FParent.CleanDirty(Self);
  Result := FIndex;
end;

function TQVTNode.GetLastChild: TQVTNode;
begin
  NeedInitialized;
  InitChildren;
  // 如果有子结点
  if (TQVTNodeState.nsHasChildren in FStates) then
  begin
    // 检查第后一个子结点是否赋值了
    if not Assigned(FLastChild) then
    begin
      if not Assigned(FLastInitChild) then
        Pointer(FLastInitChild) := GetFirstChild as TQVTNode;
      if Assigned(FLastInitChild) then
      begin
        // 从最后一个初始化的结点开，将中间的结点都初始
        while not Assigned(FLastChild) do
          FLastInitChild.GetNext;
      end;
    end;
  end;
  Result := FLastChild;
end;

function TQVTNode.GetLastChildData: IQVTNodeData;
begin
  Result := GetLastChild as IQVTNodeData;
end;

function TQVTNode.GetLastVisibleChild: TQVTNode;
begin
  Result := GetLastChild;
  while Assigned(Result) and (not(TQVTNodeState.nsVisible in Result.FStates)) do
    Result := Result.Prior;
end;

function TQVTNode.GetLevel: Integer;
var
  AParent: TQVTNode;
begin
  if (FLevel < 0) or (FLevelDirtyCounter < TreeView.FLevelDirtyCounter) then
  begin
    AParent := Self;
    FLevel := 0;
    while Assigned(AParent) and (AParent <> TreeView.RootNode) do
    begin
      AParent := AParent.Parent;
      Inc(FLevel);
    end;
    // 记录下末次级别可能变更的计数，这个计数器在每调整一次树结点级别时加1，可以用来区分是否能使用缓存的Level
    FLevelDirtyCounter := TreeView.FLevelDirtyCounter;
  end;
  Result := FLevel;
end;

function TQVTNode.GetNext: TQVTNode;
begin
  if not Assigned(FNext) then
  begin
    if Assigned(FParent) then
    begin
      if FParent.FCreatedCount < FParent.FCount then
      begin
        Pointer(FNext) := TreeView.CreateNode;
        FNext._AddRef;
        Pointer(FNext.FPrior) := Self;
        Pointer(FNext.FParent) := FParent;
        FNext.FIndex := FIndex + 1;
        Pointer(FLastInitChild) := FNext;
        Inc(FParent.FCreatedCount);
        if FParent.FCreatedCount = FParent.FCount then
          Pointer(FParent.FLastChild) := FNext;
      end;
    end;
  end;
  Result := FNext;
end;

function TQVTNode.GetNextData: IQVTNodeData;
begin
  Result := GetNext as IQVTNodeData;
end;

function TQVTNode.GetNextVisibleChild(AChild: TQVTNode): TQVTNode;
begin
  Result := AChild.GetNext;
  while Assigned(Result) and (not(TQVTNodeState.nsVisible in Result.FStates)) do
    Result := Result.GetNext;
end;

function TQVTNode.GetParent: TQVTNode;
begin
  Result := FParent;
end;

function TQVTNode.GetPrior: TQVTNode;
begin
  Result := FPrior;
end;

function TQVTNode.GetPriorData: IQVTNodeData;
begin
  Result := FPrior;
end;

function TQVTNode.GetPriorVisible(AChild: TQVTNode): TQVTNode;
begin
  Result := AChild.FPrior;
  while Assigned(Result) and (not(TQVTNodeState.nsVisible in Result.FStates)) do
    Result := Result.FPrior;
end;

{ RowIndex要求从第一个子结点到当前结点都是已知的，是一种开销比较大的操作，慎用 }
function TQVTNode.GetRowIndex: Integer;
begin
  Result := FRowIndex;
end;

function TQVTNode.GetStates: TQVTNodeStates;
begin
  Result := FStates;
end;

function TQVTNode.HasChildren: Boolean;
begin
  Result := TQVTNodeState.nsHasChildren in FStates;
end;

procedure TQVTNode.InitChildren;
begin
  if FCount < 0 then
  begin
    if Assigned(TreeView.FOnInitChildren) then
      TreeView.FOnInitChildren(TreeView, Self);
    if FCount < 0 then
    begin
      FCount := 0;
      FStates := FStates - [TQVTNodeState.nsHasChildren];
    end;
  end;
end;

procedure TQVTNode.Insert(ANode: TQVTNode; APos: TNodeInsertPosition);
begin
  // 检查并从原来的关系中移除
  if Assigned(ANode) and (ANode <> Self) and (ANode <> TreeView.RootNode) then
  begin
    NeedInitialized;
    if Assigned(ANode.FParent) then // 原来的父结点数量减1
    begin
      Dec(ANode.FParent.FCount);
      Dec(ANode.FParent.FCreatedCount);
      ANode.FParent.Dirty(ANode.Next);
    end;
    if Assigned(ANode.FPrior) then // 断开前一个的连接
      Pointer(ANode.FPrior.FNext) := ANode.FNext;
    if Assigned(ANode.FNext) then
      // 断开后一个的连接
      Pointer(ANode.FNext.FPrior) := ANode.Prior;
    case APos of
      ipBefore:
        begin
          // 插入到当前结点前面
          if Assigned(FParent) then
          begin
            if Assigned(FPrior) then
              Pointer(FPrior.FNext) := ANode
            else
              Pointer(FParent.FFirstChild) := ANode;
            Pointer(FPrior) := ANode;
            Pointer(ANode.FNext) := Self;
            Pointer(ANode.FParent) := FParent;
            ANode.FIndex := FIndex;
            FParent.FStates := FParent.FStates + [TQVTNodeState.nsHasChildren];
            Inc(FParent.FCount);
            Inc(FParent.FCreatedCount);
            TreeView.NodeContentChanged;
            FParent.Dirty(Self);
          end
          else
            raise Exception.Create('无法将结点插入到根结点之前');
        end;
      ipAfter:
        begin
          // 插入到当前结点后面
          if Assigned(FParent) then
          begin
            if Assigned(FNext) then
              Pointer(FNext.FPrior) := ANode
            else if FParent.FLastChild = Self then
              Pointer(FParent.FLastChild) := ANode;
            Pointer(ANode.FNext) := FNext;
            Pointer(FNext) := ANode;
            Pointer(ANode.FPrior) := Self;
            Pointer(ANode.FParent) := FParent;
            FParent.FStates := FParent.FStates + [TQVTNodeState.nsHasChildren];
            Inc(FParent.FCount);
            Inc(FParent.FCreatedCount);
            TreeView.NodeContentChanged;
            FParent.Dirty(ANode);
          end
          else
            raise Exception.Create('无法将结点插入到根结点之后');
        end;
      ipFirstChild:
        begin
          // 插入为第一个子结点
          if Assigned(FFirstChild) then
            Pointer(FFirstChild.FPrior) := ANode;
          Pointer(ANode.FNext) := FFirstChild;
          Pointer(ANode.FPrior) := nil;
          Pointer(ANode.FParent) := Self;
          ANode.FIndex := 0;
          Pointer(FFirstChild) := ANode;
          Inc(FCount);
          Inc(FCreatedCount);
          Dirty(Self);
          FStates := FStates + [TQVTNodeState.nsHasChildren];
          TreeView.NodeContentChanged;
        end;
      ipLastChild:
        begin
          // 插入为最后一个子结点
          if TQVTNodeState.nsHasChildren in FStates then
            GetLastChild;
          // 保证已经初始化了原始的最后一个结点
          if Assigned(FLastChild) then
            Pointer(FLastChild.FNext) := ANode;
          Pointer(ANode.FPrior) := FLastChild;
          Pointer(ANode.FParent) := Self;
          Pointer(ANode.FNext) := nil;
          ANode.FIndex := FCount;
          Pointer(FLastChild) := ANode;
          Inc(FCount);
          Inc(FCreatedCount);
          FStates := FStates + [TQVTNodeState.nsHasChildren];
          TreeView.NodeContentChanged;
        end;
    end;
  end;
end;

procedure TQVTNode.NeedInitialized;
begin
  if not(TQVTNodeState.nsInitialized in FStates) then
    TreeView.InitNode(Self);
end;

function TQVTNode.QueryInterface(const IID: TGUID; out Obj): HResult;
var
  I: Integer;
begin
  Result := inherited;
  if (Result = E_NOINTERFACE) and Assigned(FExts) then
  begin
    for I := 0 to FExts.Count - 1 do
    begin
      Result := FExts[I].QueryInterface(IID, Obj);
      if Result = S_OK then
        Break;
    end;
  end;
end;

procedure TQVTNode.ScrollToView;
begin
  TreeView.MakeNodeVisible(Self);
end;

procedure TQVTNode.SetChildCount(const Value: Integer);
var
  AChild, ANext: TQVTNode;
  I: Integer;
begin
  if (FCount <> Value) and (Value > 0) then
  begin
    if FCount < Value then
    // 如果新的子结点数量增大，则末个结点变空，以便后续初始化
    begin
      Pointer(FLastChild) := nil;
      FCount := Value;
    end
    else
    // FCount>Value，需要删除多余的子结点
    begin
      AChild := FFirstChild;
      I := 0;
      while Assigned(AChild) and (FCount > Value) do
      begin
        AChild.FIndex := I;
        AChild := AChild.Next;
      end;
      if Assigned(AChild) then
      begin
        Pointer(FFirstDirtyChild) := nil;
        if Assigned(AChild.Prior) then
          Pointer(AChild.Prior.FNext) := nil;
        // 释放掉多余的结点
        while Assigned(AChild) do
        begin
          ANext := AChild.FNext;
          Pointer(AChild.FNext) := nil;
          if Assigned(ANext) then
            Pointer(ANext.FPrior) := nil;
          AChild._Release;
          AChild := ANext;
        end;
      end;
    end;
    if Value > 0 then
      FStates := FStates + [TQVTNodeState.nsHasChildren]
    else
      FStates := FStates - [TQVTNodeState.nsHasChildren];
    TreeView.InvalidateNode(Self);
  end;
end;

procedure TQVTNode.SetFocus;
var
  AParent: TQVTNode;
  AContentChanged: Boolean;
begin
  if CanFocus then
  begin
    // Expand all parent
    AParent := Parent;
    AContentChanged := False;
    while Assigned(AParent) do
    begin
      if not(TQVTNodeState.nsExpanded in AParent.FStates) then
      begin
        AContentChanged := true;
        AParent.FStates := AParent.FStates + [TQVTNodeState.nsExpanded];
      end;
      AParent := AParent.FParent;
    end;
    TreeView.FFocusNode := Self;
    TreeView.FocusChanged;
    if AContentChanged then
      TreeView.NodeContentChanged;
    ScrollToView;
  end;
end;

// 加限制最小最大高度
procedure TQVTNode.SetHeight(const value: Single);
begin
  if not SameValue(value, FHeight) then
  begin
    if Value >= FMinHeight then
    begin
      if (Value < FMaxHeight) or IsZero(FMaxHeight) then
      begin
        FHeight := value;
        if TQVTNodeState.nsVisible in FStates then
          TreeView.NodeContentChanged;
      end;
    end;
  end;
end;

procedure TQVTNode.SetMaxHeight(const Value: Single);
begin
  if not SameValue(Value, FMaxHeight) then
  begin
    if Value >= 0 then
    begin
      FMaxHeight := Value;
      if (Value > 0) and (Height > FMaxHeight) then
        Height := MaxHeight;
    end
    else
      FMaxHeight := 0;
  end;
end;

procedure TQVTNode.SetMinHeight(const Value: Single);
begin
  if not SameValue(Value, FMinHeight) then
  begin
    if Value >= 0 then
    begin
      FMinHeight := Value;
      if (Value > 0) and (Height < FMinHeight) then
        Height := FMinHeight;
    end
    else
      FMinHeight := 0;
  end;
end;

procedure TQVTNode.SetStates(const value: TQVTNodeStates);
begin
  if FStates <> value then
  begin
    if ([TQVTNodeState.nsVisible, TQVTNodeState.nsExpanded] * FStates) <>
      ([TQVTNodeState.nsVisible, TQVTNodeState.nsExpanded] * value) then
      TreeView.NodeContentChanged;
    FStates := value;
  end;
end;

{ TQVTTextDrawer }

procedure TQVTTextDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  ATextData: IQVTTextCellData;
  ATreeView: TQVirtualTreeView;
  ATextSettings: TTextSettings;
  ADrawable: IQVTTextDrawable;
begin
  ATreeView := AData.TreeView;
  if Supports(AData, IQVTTextCellData, ATextData) then
  begin
    if Supports(AData, IQVTTextDrawable, ADrawable) then
    begin
      ATextSettings := ADrawable.TextSettings;
      if not Assigned(ATextSettings) then
        ATextSettings := ATreeView.TextSettings;
      if ADrawable.Fill <> nil then
        ATreeView.Canvas.FillRect(ARect, 0, 0, [], ATreeView.Opacity,
          ADrawable.Fill);
      DrawText(ARect, ATextData, ATextSettings);
      if ADrawable.Stroke <> nil then
      begin
        ATreeView.Canvas.DrawRectSides(ARect, 0, 0, [], ATreeView.Opacity,
          ATextData.Sides, ADrawable.Stroke);
      end
    end
    else
    begin
      ATextSettings := ATreeView.TextSettings;
      DrawText(ARect, ATextData, ATextSettings);
    end;
  end;
end;

procedure TQVTTextDrawer.DrawText(ARect: TRectF; ATextData: IQVTTextCellData;
  ATextSettings: TTextSettings);
begin
  DrawText(ATextData.TreeView, ARect, ATextData.Text, ATextSettings);
end;

procedure TQVTTextDrawer.DrawText(ATreeView: TQVirtualTreeView; ARect: TRectF;
  AText: String; ATextSettings: TTextSettings);
begin
  with ATreeView.FTextLayout do
  begin
    BeginUpdate;
    TopLeft := ARect.TopLeft;
    MaxSize := PointF(ARect.Width, ARect.Height);
    Text := AText;
    WordWrap := ATextSettings.WordWrap;
    Opacity := Opacity;
    HorizontalAlign := ATextSettings.HorzAlign;
    VerticalAlign := ATextSettings.VertAlign;
    Font := ATextSettings.Font;
    Color := ATextSettings.FontColor;
    RightToLeft := false;
    Padding.Rect := Rect(2, 2, 2, 2);
    // 不支持从右到左阅读TFillTextFlag.RightToLeft in ATextSettings.;
    EndUpdate;
    RenderLayout(ATreeView.Canvas);
  end;
end;

function TQVTTextDrawer.GetFill(AData: IQVTCellData;
  AllowNull: Boolean): TBrush;
var
  ADrawable: IQVTDrawable;
begin
  if Supports(AData, IQVTDrawable, ADrawable) then
    Result := ADrawable.Fill
  else
    Result := nil;
  if not Assigned(Result) then
  begin
    if not AllowNull then
      Result := AData.TreeView.Fill;
  end;
end;

function TQVTTextDrawer.GetStroke(AData: IQVTCellData; AllowNull: Boolean)
  : TStrokeBrush;
var
  ADrawable: IQVTDrawable;
begin
  if Supports(AData, IQVTDrawable, ADrawable) then
    Result := ADrawable.Stroke
  else
    Result := nil;
  if not Assigned(Result) then
  begin
    if not AllowNull then
      Result := AData.TreeView.Stroke;
  end;
end;

function TQVTTextDrawer.GetTextSettings(AData: IQVTCellData; AllowNull: Boolean)
  : TTextSettings;
var
  ADrawable: IQVTTextDrawable;
begin
  if Supports(AData, IQVTTextDrawable, ADrawable) then
    Result := ADrawable.TextSettings
  else
    Result := nil;
  if not Assigned(Result) then
  begin
    if not AllowNull then
      Result := AData.TreeView.TextSettings;
  end;
end;

{ TQVTProgressDrawer }

procedure TQVTProgressDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  ATreeView: TQVirtualTreeView;
  AProgress: IQVTProgressCellData;
  ATextData: IQVTTextCellData;
  AFill: TBrush;
  AStroke: TStrokeBrush;
  ATextSettings: TTextSettings;
  R: TRectF;
  PW: Single;
  AText: String;
  function CalcTextColor(AColor: TAlphaColor): TAlphaColor;
  var
    c: TAlphaColorRec absolute AColor;
    f: TAlphaColorRec;
    r: TAlphaColorRec absolute Result;
  begin
    f.Color := ATreeView.Fill.Color;
    r.R := (f.R + c.R) shr 1;
    r.G := (f.G + c.G) shr 1;
    r.B := (f.B + c.B) shr 1;
    r.A := c.A;
  end;

begin
  ATreeView := AData.TreeView;
  if Supports(AData, IQVTProgressCellData, AProgress) then
  begin
    AFill := GetFill(AData, true);
    if not Assigned(AFill) then
    begin
      AFill := ATreeView.Canvas.Fill;
      AFill.Kind := TBrushKind.Solid;
      AFill.Color := ATreeView.TintColor;
    end;
    AStroke := GetStroke(AData, true);
    if not Assigned(AStroke) then
    begin
      AStroke := ATreeView.Canvas.Stroke;
      AStroke.Kind := TBrushKind.Solid;
      AStroke.Color := ATreeView.TintColor;
    end;
    R := ARect;
    R.Inflate(-2, -2);
    ATreeView.Canvas.DrawRect(R.SnapToPixel(ATreeView.Canvas.Scale), 0, 0, [],
      ATreeView.Opacity, AStroke);
    R.Inflate(-2, -2);
    PW := R.Width * AProgress.GetProgress / 100;
    R.Right := R.Left + PW;
    ATreeView.Canvas.FillRect(R.SnapToPixel(ATreeView.Canvas.Scale), 0, 0, [],
      ATreeView.Opacity, AFill);
    if Supports(AData, IQVTTextCellData, ATextData) then
    begin
      ARect.Left := R.Left + 3;
      AText := ATextData.Text;
      ATextSettings := TTextSettings.Create(nil);
      try
        ATextSettings.Assign(GetTextSettings(AData, false));
        ATextSettings.FontColor := CalcTextColor(ATextSettings.FontColor);
        DrawText(ARect, ATextData, ATextSettings);
      finally
        FreeAndNil(ATextSettings);
      end;
    end;
  end;
end;
{ TQVTMasterDrawer }

class constructor TQVTMasterDrawer.Create;
begin
  FStatePath[false] := nil;
  FStatePath[true] := nil;
end;

class destructor TQVTMasterDrawer.Destroy;
begin
  if Assigned(FStatePath[false]) then
    FreeAndNil(FStatePath[false]);
  if Assigned(FStatePath[true]) then
    FreeAndNil(FStatePath[true]);
end;

destructor TQVTMasterDrawer.Destroy;
begin
  if Assigned(FCurrentPath[false]) then
    FreeAndNil(FCurrentPath[false]);
  if Assigned(FCurrentPath[true]) then
    FreeAndNil(FCurrentPath[true]);
end;

procedure TQVTMasterDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  ATreeView: TQVirtualTreeView;
  ANode: TQVTNode;
  R: TRectF;
  ACanvas: TCanvas;
  pt1, pt2, ct: TPointF;
  APaintButton, APaintTreeLine: Boolean;
  procedure DrawLeftLine(ALevelNode: TQVTNode);
  begin
    if ANode <> ATreeView.RootNode.GetFirstVisibleChild then
    begin
      pt1.Y := R.Top;
      // Todo:修正竖线连接问题
      // if (ALevelNode <> ATreeView.FFirstVisibleNode) and (ALevelNode.Level > 1)
      // then
      // pt1.Y := pt1.Y - ATreeView.GetPriorVisible(ALevelNode).Height / 2;
    end
    else
      pt1.Y := (R.Top + R.Bottom) / 2;
    if (ALevelNode = ANode) or (ALevelNode.GetNext <> nil) then
    begin
      if ALevelNode.GetNext <> nil then
        pt2.Y := R.Bottom
      else
        pt2.Y := (R.Top + R.Bottom) / 2;
      pt1.X := ct.X - (ANode.Level - ALevelNode.Level) * ATreeView.NodeIndent;
      if pt1.X < R.Right then
      begin
        pt2.X := pt1.X;
        pt1 := pt1.SnapToPixel(ACanvas.Scale);
        pt2 := pt2.SnapToPixel(ACanvas.Scale);
        ACanvas.DrawLine(pt1, pt2, ATreeView.Opacity, ATreeView.FLineStyle);
      end;
    end;
    if ALevelNode.Level > 1 then
      DrawLeftLine(ALevelNode.Parent);
  end;
  procedure DefaultDrawButton;
  begin
    ACanvas.FillRect(R, 0, 0, [], ATreeView.Opacity, ATreeView.Fill);
    ACanvas.DrawRect(R, 0, 0, [], ATreeView.Opacity, ATreeView.FStroke);
    ACanvas.DrawLine(PointF(ct.X - 2, ct.Y).SnapToPixel(ACanvas.Scale),
      PointF(ct.x + 2, ct.y).SnapToPixel(ACanvas.Scale), ATreeView.Opacity,
      ATreeView.Stroke);
    if not(TQVTNodeState.nsExpanded in ANode.FStates) then // 未展开
      ACanvas.DrawLine(PointF(ct.X, ct.Y - 2).SnapToPixel(ACanvas.Scale),
        PointF(ct.X, ct.Y + 2).SnapToPixel(ACanvas.Scale), ATreeView.Opacity,
        ATreeView.Stroke);
  end;
  procedure DrawButton;
  var
    ALastBounds: TRectF;
    AIsExpanded: Boolean;
  begin
    ALastBounds := ANode.FDisplayRect;
    if ANode.FButtonRect.Right > R.Right then
      ANode.FButtonRect.Right := R.Right;
    if ANode.FButtonRect.Left < R.Right then
    begin
      R := ANode.FButtonRect.SnapToPixel(ACanvas.Scale);
      AIsExpanded := TQVTNodeState.nsExpanded in ANode.FStates;
      if Assigned(FStatePath[AIsExpanded]) then
      begin
        if not Assigned(FCurrentPath[AIsExpanded]) then
          FCurrentPath[AIsExpanded] := TPathData.Create;
      end;
      if Assigned(FCurrentPath[AIsExpanded]) then
      begin
        FCurrentPath[AIsExpanded].Assign(FStatePath[AIsExpanded]);
        if (Length(FCurrentPath[AIsExpanded].Data) > 0) then
        begin
          FCurrentPath[AIsExpanded].FitToRect(R);
          ATreeView.Canvas.Fill.Kind := TBrushKind.Solid;
          ATreeView.Canvas.Fill.Color := ATreeView.TintColor;
          ATreeView.Canvas.FillPath(FCurrentPath[AIsExpanded],
            ATreeView.Opacity);
        end
        else
          DefaultDrawButton;
      end
      else
        DefaultDrawButton;
    end;
  end;

begin
  // 绘制树形的线
  R := ARect;
  ATreeView := AData.TreeView;
  if TQVTPaintOption.poVertLine in ATreeView.PaintOptions then
    R.Left := R.Left + 2;
  ANode := AData.Node;
  ACanvas := ATreeView.Canvas;
  ct.X := R.Left + ATreeView.NodeIndent * (ANode.Level - 1) + 8;
  ct.Y := (R.Top + R.Bottom) / 2;
  APaintButton := // ANode.HasChildren and
    (TQVTPaintOption.poNodeButton in ATreeView.PaintOptions);
  APaintTreeLine := TQVTPaintOption.poTreeLine in ATreeView.PaintOptions;
  if APaintTreeLine then
  begin
    // 绘制最左侧的|线
    DrawLeftLine(ANode);
    // 绘制连结到内容的连接线
    pt1 := ct;
    if pt1.X < R.Right then
    begin
      pt2.Y := pt1.Y;
      pt2.X := pt1.X + ATreeView.NodeIndent;
      if pt2.X > R.Right then
        pt2.X := R.Right;
      pt1 := pt1.SnapToPixel(ACanvas.Scale);
      pt2 := pt2.SnapToPixel(ACanvas.Scale);
      ACanvas.DrawLine(pt2, pt1, ATreeView.Opacity, ATreeView.FLineStyle);
    end;
  end;
  // 如果有子结点，则需要先覆盖+/-
  if APaintButton then
  begin
    ANode.FButtonRect := RectF(ct.X - 6, ct.Y - 6, ct.X + 6, ct.Y + 6);
    if ANode.HasChildren then
      DrawButton;
  end;
  // 有空闲空间，绘制文本
  if APaintTreeLine then
    ARect.Left := ct.X + ATreeView.NodeIndent
  else if APaintButton then
    ARect.Left := ANode.FButtonRect.Right + 4
  else
    ARect.Left := ct.X - 4;
  if ARect.Left < ARect.Right then
  begin
    if Supports(AData, IQVTCheckCellData) then
      TQVirtualTreeView.DefaultDrawers[TQVTDrawerType.dtCheck]
        .Draw(ARect, AData)
    else if Supports(AData, IQVTRadioCellData) then
      TQVirtualTreeView.DefaultDrawers[TQVTDrawerType.dtRadio]
        .Draw(ARect, AData)
    else
      inherited;
  end;
end;

class function TQVTMasterDrawer.GetCascadedPath: TPathData;
begin
  if not Assigned(FStatePath[false]) then
    FStatePath[false] := TPathData.Create;
  Result := FStatePath[false];
end;

class function TQVTMasterDrawer.GetExpandedPath: TPathData;
begin
  if not Assigned(FStatePath[true]) then
    FStatePath[true] := TPathData.Create;
  Result := FStatePath[true];
end;

class procedure TQVTMasterDrawer.SetCascadedPath(const Value: TPathData);
begin
  if Assigned(Value) then
    CascadedPath.Data := Value.Data
  else if Assigned(FStatePath[false]) then
    FStatePath[false].Data := '';
end;

class procedure TQVTMasterDrawer.SetExpandedPath(const Value: TPathData);
begin
  if Assigned(Value) then
    ExpandedPath.Data := Value.Data
  else if Assigned(FStatePath[true]) then
    ExpandedPath.Data := '';
end;

constructor TQVTMasterDrawer.Create;
begin

end;

{ TQVTDefaultCellData }

constructor TQVTDefaultCellData.Create(ANode: TQVTNode; AColIndex: Integer);
begin
  inherited Create;
  FNode := ANode;
  FColumn := AColIndex;
end;

constructor TQVTDefaultCellData.Create;
begin
  inherited;
end;

function TQVTDefaultCellData.GetColumn: Integer;
begin
  Result := FColumn;
end;

function TQVTDefaultCellData.GetEnabled: Boolean;
begin
  Result := True;
end;

function TQVTDefaultCellData.GetNode: TQVTNode;
begin
  Result := FNode;
end;

function TQVTDefaultCellData.GetSides: TSides;
begin
  Result := [TSide.Top, TSide.Left, TSide.Right, TSide.Bottom];
end;

function TQVTDefaultCellData.GetTreeView: TQVirtualTreeView;
begin
  if Assigned(FNode) then
    Result := FNode.TreeView
  else
    Result := nil;
end;

procedure TQVTDefaultCellData.SetColumn(const AColumn: Integer);
begin
  if AColumn < 0 then
    FColumn := -1
  else if Assigned(FNode) then
  begin
    if AColumn < Node.TreeView.Header.Columns.Count then
      FColumn := AColumn
    else
      FColumn := Node.TreeView.Header.Columns.Count - 1;
  end
  else
    FColumn := AColumn;
end;

procedure TQVTDefaultCellData.SetNode(ANode: TQVTNode);
begin
  FNode := ANode;
end;

{ TQVTCheckDrawer }

procedure TQVTCheckDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  R, ABox: TRectF;
  ACheckData: IQVTCheckCellData;
  ATreeView: TQVirtualTreeView;
  AStroke: TStrokeBrush;
  AOpacity: Single;
  // 绘制默认的CheckBox样式
  procedure DefaultDrawCheckBox;
  var
    pt1, pt2: TPointF;
  begin
    if (ABox.Top < ARect.Bottom) and (ABox.Left < ARect.Right) then
    begin
      if not(TQVTCheckState.csEnabled in ACheckData.CheckStates) then
        AOpacity := ATreeView.Opacity * 0.5
      else
        AOpacity := ATreeView.Opacity;
      AStroke := GetStroke(AData, true);
      if not Assigned(AStroke) then
      begin
        AStroke := ATreeView.Canvas.Stroke;
        AStroke.Kind := TBrushKind.Solid;
        AStroke.Color := ATreeView.TintColor;
      end;
      ATreeView.Canvas.DrawRect(ABox.SnapToPixel(ATreeView.Canvas.Scale), 0, 0,
        [], AOpacity, AStroke);
      if TQVTCheckState.csChecked in ACheckData.CheckStates then
      begin
        R := ABox;
        R.Inflate(-3, -3);
        if TQVTCheckState.csSomeChecked in ACheckData.CheckStates then
          ATreeView.Canvas.FillRect(R.SnapToPixel(ATreeView.Canvas.Scale), 0, 0,
            [], AOpacity, AStroke)
        else
        begin
          pt1 := R.TopLeft;
          pt2 := R.BottomRight;
          ATreeView.Canvas.DrawLine(pt1.SnapToPixel(ATreeView.Canvas.Scale),
            pt2.SnapToPixel(ATreeView.Canvas.Scale), AOpacity, AStroke);
          pt1.X := R.Right;
          pt2.X := R.Left;
          ATreeView.Canvas.DrawLine(pt1.SnapToPixel(ATreeView.Canvas.Scale),
            pt2.SnapToPixel(ATreeView.Canvas.Scale), AOpacity, AStroke);
        end;
      end;
    end;
  end;

  procedure DrawCheckBox;
  var
    AIdx: Integer;
  begin
    ACheckData.CheckBounds := ABox;
    AIdx := StatePathIndex(ACheckData.CheckStates);
    if Assigned(FCheckStatePath[AIdx]) and
      (Length(FCheckStatePath[AIdx].Data) > 0) then
    begin
      if not Assigned(FCurrentPath[AIdx]) then
      begin
        FCurrentPath[AIdx] := TPathData.Create;
        FCurrentPath[AIdx].Assign(FCheckStatePath[AIdx]);
      end;
      FCurrentPath[AIdx].FitToRect(ABox);
      if TQVTCheckState.csEnabled in ACheckData.CheckStates then
        DrawPath(AData, FCurrentPath[AIdx], ATreeView.Opacity)
      else
        DrawPath(AData, FCurrentPath[AIdx], ATreeView.Opacity * 0.5);
    end
    else
      DefaultDrawCheckBox
  end;

begin
  ATreeView := AData.TreeView;
  if Supports(AData, IQVTCheckCellData, ACheckData) then
  begin
    CalcLayouts(AData, ARect, ABox, ARect);
    DrawCheckBox;
    inherited Draw(ARect, AData);
  end;
end;

class function TQVTCheckDrawer.GetCheckedPath: TPathData;
begin
  Result := GetPathData([TQVTCheckState.csChecked]);
end;

class function TQVTCheckDrawer.GetPathData(AStates: TQVTCheckStates): TPathData;
var
  AIdx: Integer;
begin
  AIdx := StatePathIndex(AStates);
  if not Assigned(FCheckStatePath[AIdx]) then
    FCheckStatePath[AIdx] := TPathData.Create;
  Result := FCheckStatePath[AIdx];
end;

class function TQVTCheckDrawer.GetSomeCheckedPath: TPathData;
begin
  Result := GetPathData([TQVTCheckState.csChecked,
    TQVTCheckState.csSomeChecked]);
end;

class function TQVTCheckDrawer.GetUncheckPath: TPathData;
begin
  Result := GetPathData([]);
end;

class function TQVTCheckDrawer.StatePathIndex(AStates: TQVTCheckStates)
  : Integer;
begin
  Result := 0;
  if TQVTCheckState.csChecked in AStates then
    Inc(Result);
  if TQVTCheckState.csSomeChecked in AStates then
    Inc(Result);
end;

{ TQVTRadioDrawer }

procedure TQVTRadioDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  R, ABox: TRectF;
  ARadioData: IQVTRadioCellData;
  ATreeView: TQVirtualTreeView;
  AStroke: TStrokeBrush;
  AOpacity: Single;
  procedure DrawDefaultRadioButton;
  begin
    if (ABox.Top < ARect.Bottom) and (ABox.Left < ARect.Right) then
    begin
      if not(TQVTCheckState.csEnabled in ARadioData.CheckStates) then
        AOpacity := ATreeView.Opacity * 0.5
      else
        AOpacity := ATreeView.Opacity;
      AStroke := GetStroke(AData, true);
      if not Assigned(AStroke) then
      begin
        AStroke := ATreeView.Canvas.Stroke;
        AStroke.Kind := TBrushKind.Solid;
        AStroke.Color := ATreeView.TintColor;
      end;
      ATreeView.Canvas.DrawEllipse(ABox.SnapToPixel(ATreeView.Canvas.Scale),
        AOpacity, AStroke);
      if TQVTCheckState.csChecked in ARadioData.CheckStates then
      begin
        ABox.Inflate(-3, -3);
        if TQVTCheckState.csSomeChecked in ARadioData.CheckStates then
        begin
          ATreeView.Canvas.FillEllipse(ABox.SnapToPixel(ATreeView.Canvas.Scale),
            AOpacity * 0.5, AStroke);
        end
        else
          ATreeView.Canvas.FillEllipse(ABox.SnapToPixel(ATreeView.Canvas.Scale),
            AOpacity, AStroke);
      end;
    end;
  end;

  procedure DrawRadioButton;
  var
    AIdx: Boolean;
  begin
    ARadioData.RadioBounds := ABox;
    AIdx := TQVTCheckState.csChecked in ARadioData.CheckStates;
    if Assigned(FCheckStatePath[AIdx]) and
      (Length(FCheckStatePath[AIdx].Data) > 0) then
    begin
      if not Assigned(FCurrentPath[AIdx]) then
      begin
        FCurrentPath[AIdx] := TPathData.Create;
        FCurrentPath[AIdx].Assign(FCheckStatePath[AIdx]);
      end;
      FCurrentPath[AIdx].FitToRect(ABox);
      if TQVTCheckState.csEnabled in ARadioData.CheckStates then
        DrawPath(AData, FCurrentPath[AIdx], ATreeView.Opacity)
      else
        DrawPath(AData, FCurrentPath[AIdx], ATreeView.Opacity * 0.5);
    end
    else
      DrawDefaultRadioButton;
  end;

begin
  ATreeView := AData.TreeView;
  if Supports(AData, IQVTRadioCellData, ARadioData) then
  begin
    CalcLayouts(AData, Arect, ABox, R);
    DrawRadioButton;
    if not R.IsEmpty then
      inherited Draw(R, AData);
  end;
end;

class function TQVTRadioDrawer.GetCheckedPath: TPathData;
begin
  if not Assigned(FCheckStatePath[true]) then
    FCheckStatePath[false] := TPathData.Create;
  Result := FCheckStatePath[true];
end;

class function TQVTRadioDrawer.GetUncheckPath: TPathData;
begin
  if not Assigned(FCheckStatePath[false]) then
    FCheckStatePath[false] := TPathData.Create;
  Result := FCheckStatePath[false];
end;

{ TQVTImageDrawer }

procedure TQVTImageDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
begin
  inherited;

end;

{ TQVTStateDrawer }

function TQVTStateDrawer.CalcContentRect(const ARect: TRectF;
  AData: IQVTCellData): TRectF;
var
  ALayout: TQVTLayout;
  ASize: TSizeF;
  AStateRect: TRectF;
begin
  ALayout := GetLayout(AData, ASize);
  AStateRect := CalcStateRect(ARect, AData);
  Result := CalcContentRect(ARect, AStateRect, ALayout);
end;

function TQVTStateDrawer.CalcContentRect(const ARect, AStateRect: TRectF;
  ALayout: TQVTLayout): TRectF;
var
  AMargins: TRectF;
begin
  AMargins := GetMargins(ALayout);
  case ALayout of
    clLeftTop:
      begin
        Result.Left := AStateRect.Right;
        Result.Top := AStateRect.Bottom;
        Result.Right := ARect.Right;
        Result.Bottom := ARect.Bottom;
      end;
    clTopCenter:
      begin
        Result.Left := ARect.Left;
        Result.Top := AStateRect.Bottom;
        Result.Right := ARect.Right;
        Result.Bottom := ARect.Bottom;
      end;
    clRightTop:
      begin
        Result.Left := ARect.Left;
        Result.Top := AStateRect.Bottom;
        Result.Right := AStateRect.Left;
        Result.Bottom := ARect.Bottom;
      end;
    clRightCenter:
      begin
        Result.Left := ARect.Left;
        Result.Top := ARect.Top;
        Result.Right := AStateRect.Left;
        Result.Bottom := ARect.Bottom;
      end;
    clRightBottom:
      begin
        Result.Left := ARect.Left;
        Result.Top := ARect.Top;
        Result.Right := AStateRect.Left;
        Result.Bottom := AStateRect.Bottom;
      end;
    clBottomCenter:
      begin
        Result.Left := ARect.Left;
        Result.Top := ARect.Top;
        Result.Right := ARect.Right;
        Result.Bottom := AStateRect.Top;
      end;
    clLeftBottom:
      begin
        Result.Left := AStateRect.Right;
        Result.Top := ARect.Top;
        Result.Right := ARect.Right;
        Result.Bottom := AStateRect.Top;
      end;
    clLeftCenter:
      begin
        Result.Left := AStateRect.Right;
        Result.Top := ARect.Top;
        Result.Right := ARect.Right;
        Result.Bottom := ARect.Bottom;
      end;
    clCenter:
      // 你非要占中间，我也很绝望
      Result := ARect;
  end;
  Result.Left := Result.Left + AMargins.Left;
  Result.Top := Result.Top + AMargins.Top;
  Result.Bottom := Result.Bottom - AMargins.Bottom;
  Result.Right := Result.Right - AMargins.Right;
  if (Result.Right <= Result.Left) or (Result.Bottom <= Result.Top) then
    Result := TRectF.Empty;
end;

procedure TQVTStateDrawer.CalcLayouts(AData: IQVTCellData; const ARect: TRectF;
  var AStateRect, AContentRect: TRectF);
var
  ALayout: TQVTLayout;
  ASize: TSizeF;
begin
  ALayout := GetLayout(AData, ASize);
  AStateRect := CalcStateRect(ARect, ASize, ALayout);
  AContentRect := CalcContentRect(ARect, AStateRect, ALayout);
end;

function TQVTStateDrawer.CalcStateRect(const ARect: TRectF; const ASize: TSizeF;
  ALayout: TQVTLayout): TRectF;
var
  ARatio: TPointF;
  R, AMargins: TRectF;
  V: Single;
begin
  if ASize.IsZero then
  begin
    Result := TRectF.Empty;
    Exit;
  end;
  AMargins := GetMargins(ALayout);
  R := ARect;
  R.Left := R.Left + AMargins.Left;
  R.Top := R.Top + AMargins.Top;
  R.Right := R.Right - AMargins.Right;
  R.Bottom := R.Bottom - AMargins.Bottom;
  case ALayout of
    clLeftTop:
      begin
        Result.Left := R.Left;
        Result.Top := R.Top;
      end;
    clTopCenter:
      begin
        Result.Left := (R.Left + R.Right - ASize.cx) / 2;
        Result.Top := R.Top;
      end;
    clRightTop:
      begin
        Result.Left := R.Right - ASize.cx;
        Result.Top := R.Top;
      end;
    clRightCenter:
      begin
        Result.Left := R.Right - ASize.cx;
        Result.Top := (R.Top + R.Bottom - ASize.cy) / 2;
      end;
    clRightBottom:
      begin
        Result.Left := R.Right - ASize.cx;
        Result.Top := R.Bottom - ASize.cy;
      end;
    clBottomCenter:
      begin
        Result.Left := (R.Left + R.Right - ASize.cx) / 2;
        Result.Top := R.Bottom - ASize.cy;
      end;
    clLeftBottom:
      begin
        Result.Left := R.Left;
        Result.Top := R.Bottom - ASize.cy;
      end;
    clLeftCenter:
      begin
        Result.Left := R.Left;
        Result.Top := (R.Top + R.Bottom - ASize.cy) / 2;
      end;
    clCenter:
      begin
        Result.Left := (R.Left + R.Right - ASize.cx) / 2;
        Result.Top := (R.Top + R.Bottom - ASize.cy) / 2;
      end;
  end;
  Result.Right := Result.Left + ASize.cx;
  Result.Bottom := Result.Top + ASize.cy;
  if Result.Left < R.Left then
    Result.Left := R.Left;
  if Result.Top < R.Top then
    Result.Top := R.Top;
  if Result.Right > R.Right then
    Result.Right := R.Right;
  if Result.Bottom > R.Bottom then
    Result.Bottom := R.Bottom;
  ARatio.X := Result.Width / ASize.cx;
  ARatio.Y := Result.Height / ASize.cy;
  if not SameValue(ARatio.X, ARatio.Y) then // 比例不一样，有一个需要缩小尺寸
  begin
    if ARatio.X > ARatio.Y then // 垂直不足，需要缩放,水平需要适应垂直的尺寸
    begin
      case ALayout of
        clLeftTop, clLeftBottom, clLeftCenter:
          Result.Right := Result.Left + Result.Width * ARatio.Y;
        clTopCenter, clBottomCenter, clCenter:
          begin
            V := Result.Width * ARatio.Y;
            Result.Left := (Result.Left + Result.Right - V) / 2;
            Result.Right := Result.Left + V;
          end;
        clRightTop, clRightCenter, clRightBottom:
          Result.Left := Result.Right - Result.Width * ARatio.Y;
      end;
    end
    else
    begin
      case ALayout of
        clLeftTop, clTopCenter, clRightTop:
          Result.Bottom := Result.Top + Result.Height * ARatio.X;
        clRightCenter, clLeftCenter, clCenter:
          begin
            V := Result.Height * ARatio.X;
            Result.Top := (Result.Top + Result.Bottom - V) / 2;
            Result.Bottom := V;
          end;
        clRightBottom, clBottomCenter, clLeftBottom:
          Result.Top := Result.Bottom - Result.Height * ARatio.X;
      end;
    end;
  end;
end;

procedure TQVTStateDrawer.DrawPath(AData: IQVTCellData; APath: TPathData;
  AOpacity: Single);
var
  APathFill: TBrush;
begin
  APathFill := GetFill(AData, true);
  with AData.TreeView do
  begin
    if not Assigned(APathFill) then
    begin
      Canvas.Fill.Color := TintColor;
      Canvas.Fill.Kind := TBrushKind.Solid;
      APathFill := Canvas.Fill;
    end;
    Canvas.FillPath(APath, AOpacity, APathFill);
  end;
end;

function TQVTStateDrawer.CalcStateRect(const ARect: TRectF;
  AData: IQVTCellData): TRectF;
var
  ALayout: TQVTLayout;
  ASize: TSizeF;
begin
  ALayout := GetLayout(AData, ASize);
  Result := CalcStateRect(ARect, ASize, ALayout);
end;

function TQVTStateDrawer.GetLayout(AData: IQVTCellData; var AStateSize: TSizeF)
  : TQVTLayout;
var
  ALayoutData: IQVTCellLayoutData;
begin
  // 如果不支持布局接口，则按左中布局来
  if Supports(AData, IQVTCellLayoutData, ALayoutData) then
  begin
    AStateSize := ALayoutData.StateSize;
    Result := ALayoutData.Layout;
  end
  else
  begin
    AStateSize := TSizeF.Create(12, 12);
    Result := TQVTLayout.clLeftCenter;
  end;
end;

function TQVTStateDrawer.GetMargins(ALayout: TQVTLayout): TRectF;
begin
  case ALayout of
    clLeftTop:
      Result := Rect(3, 3, 1, 1);
    clTopCenter:
      Result := Rect(1, 3, 1, 1);
    clRightTop:
      Result := Rect(1, 3, 3, 1);
    clRightCenter:
      Result := Rect(1, 1, 3, 1);
    clRightBottom:
      Result := Rect(1, 1, 3, 3);
    clBottomCenter:
      Result := Rect(1, 1, 1, 3);
    clLeftBottom:
      Result := Rect(3, 1, 1, 3);
    clLeftCenter:
      Result := Rect(3, 1, 1, 1);
    clCenter:
      Result := Rect(1, 1, 1, 1);
  end;
end;

{ TQVTMoneyDrawer }

procedure TQVTMoneyDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
begin

  // inherited;

end;

{ TQVTIndicatorDrawer }

procedure TQVTIndicatorDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
begin
  // 只在当前行标记
  if AData.Node = AData.TreeView.FocusNode then
    DrawText(AData.TreeView, ARect, '>', AData.TreeView.TextSettings);
end;

{ TQVTImageStateDrawer }

procedure TQVTImageStateDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
begin
  inherited;

end;

{ TQVTHeaderDrawer }

class constructor TQVTHeaderDrawer.Create;
begin

end;

class destructor TQVTHeaderDrawer.Destroy;
begin
  if Assigned(FStatePath[0]) then
    FreeAndNil(FStatePath[0]);
  if Assigned(FStatePath[1]) then
    FreeAndNil(FStatePath[1]);
end;

procedure TQVTHeaderDrawer.Draw(ARect: TRectF; AData: IQVTCellData);
var
  AColumn: TQVTColumn;
  ATreeView: TQVirtualTreeView;
  ACanvas: TCanvas;
  AFill: TBrush;
  ATextSettings: TTextSettings;
  AColors: array [0 .. 1] of TAlphaColor;
  R: TRectF;
  S: String;

  procedure DrawDefaultMarkers;
  begin
    if ATreeView.SortColumns.Count > 1 then
    begin
      case AColumn.Title.SortMarker of
        smAsc:
          DrawText(ATreeView, R, '↑' + IntToStr(AColumn.SortIndex + 1),
            ATextSettings);
        smDesc:
          DrawText(ATreeView, R, '↓' + IntToStr(AColumn.SortIndex + 1),
            ATextSettings);
      end;
    end
    else
    begin
      case AColumn.Title.SortMarker of
        smAsc:
          DrawText(ATreeView, R, '↑', ATextSettings);
        smDesc:
          DrawText(ATreeView, R, '↓', ATextSettings);
      end;
    end;
  end;

  procedure DrawSortMarkers;
  var
    MR: TRectF;
  begin
    ATextSettings := TTextSettings.Create(nil);
    try
      ATextSettings.Assign(GetTextSettings(AData, false));
      // Marker的字体大小设置为9pt
      ATextSettings.Font.Size := 10;
      if Assigned(FStatePath[0]) and (Length(FStatePath[0].Data) > 0) and
        (AColumn.Title.SortMarker = TQVTColumnSortMarker.smAsc) then
      begin
        if not Assigned(FCurrentPath[0]) then
        begin
          FCurrentPath[0] := TPathData.Create;
          FCurrentPath[0].Assign(FStatePath[0]);
        end;
        MR := R;
        MR.Right := MR.Left + (MR.Width * 0.4);
        FCurrentPath[0].FitToRect(MR);
        ATreeView.Canvas.Fill.Color := ATreeView.TintColor;
        ATreeView.Canvas.FillPath(FCurrentPath[0], ATreeView.Opacity);
        if ATreeView.SortColumns.Count > 1 then
        begin
          R.Left := MR.Right;
          DrawText(ATreeView, R, IntToStr(AColumn.SortIndex + 1),
            ATextSettings);
        end;
      end
      else if Assigned(FStatePath[1]) and (Length(FStatePath[1].Data) > 0) and
        (AColumn.Title.SortMarker = TQVTColumnSortMarker.smDesc) then
      begin
        if not Assigned(FCurrentPath[1]) then
        begin
          FCurrentPath[1] := TPathData.Create;
          FCurrentPath[1].Assign(FStatePath[1]);
        end;
        MR := R;
        MR.Right := MR.Left + (MR.Width * 0.4);
        FCurrentPath[1].FitToRect(MR);
        ATreeView.Canvas.Fill.Color := ATreeView.TintColor;
        ATreeView.Canvas.FillPath(FCurrentPath[1], ATreeView.Opacity);
        if ATreeView.SortColumns.Count > 1 then
        begin
          R.Left := MR.Right;
          DrawText(ATreeView, R, IntToStr(AColumn.SortIndex + 1),
            ATextSettings);
        end;
      end
      else
        DrawDefaultMarkers;
    finally
      FreeAndNil(ATextSettings);
    end;
  end;

begin
  ATreeView := AData.TreeView;
  AColumn := ATreeView.Header.Columns[AData.Column];
  ACanvas := ATreeView.Canvas;
  R := ARect;
  if AColumn.Title.Clickable then
    R.Inflate(-2, -2);
  if AColumn.Title.SortMarker <> TQVTColumnSortMarker.smNone then
  begin
    R.Right := R.Right - 16;
    AFill := GetFill(AData, false);
    if AFill.Kind <> TBrushKind.None then
      ACanvas.FillRect(RectF(R.Right, R.Top, ARect.Right, R.Bottom)
        .SnapToPixel(ACanvas.Scale), 0, 0, [], ATreeView.Opacity, AFill);
  end;
  inherited Draw(R, AData);

  // 要在右侧绘制排序状态
  if AColumn.Title.SortMarker <> smNone then
  begin
    R := ARect;
    R.Left := R.Right - 16;
    if R.Left < ARect.Left then
      R.Left := ARect.Left;
    DrawSortMarkers;
  end;
  if AColumn.Title.Clickable then // 如果可以点击，则绘制成按钮的形状
  begin
    if (not Assigned(ATreeView.FMouseDownNode)) and
      (AColumn.Index = ATreeView.MouseDownColumn) then // 当前列标题被按下
    begin
      AColors[0] := TAlphaColors.LtGray;
      AColors[1] := TAlphaColors.Darkgray;
      ACanvas.Fill.Color := TAlphaColors.MedGray;
      ACanvas.FillRect(ARect, 0, 0, [], 0.5);
    end
    else
    begin
      AColors[0] := TAlphaColors.Darkgray;
      AColors[1] := TAlphaColors.LtGray;
    end;
    ACanvas.Stroke.Color := AColors[0]; //
    // 底部
    ACanvas.DrawLine(PointF(ARect.Left, ARect.Bottom - 1)
      .SnapToPixel(ACanvas.Scale), PointF(ARect.Right - 1, ARect.Bottom - 1)
      .SnapToPixel(ACanvas.Scale), ATreeView.Opacity);
    // 右侧
    ACanvas.DrawLine(PointF(ARect.Right - 1, ARect.Top)
      .SnapToPixel(ACanvas.Scale), PointF(ARect.Right - 1, ARect.Bottom - 1)
      .SnapToPixel(ACanvas.Scale), ATreeView.Opacity);
    ACanvas.Stroke.Color := AColors[1]; //
    // 左侧
    ACanvas.DrawLine(PointF(ARect.Left, ARect.Top).SnapToPixel(ACanvas.Scale),
      PointF(ARect.Left, ARect.Bottom - 1).SnapToPixel(ACanvas.Scale),
      ATreeView.Opacity);
    // 上部
    ACanvas.DrawLine(PointF(ARect.Left, ARect.Top).SnapToPixel(ACanvas.Scale),
      PointF(ARect.Right - 1, ARect.Top).SnapToPixel(ACanvas.Scale),
      ATreeView.Opacity);
    ARect.Inflate(-1, -1);
  end
  else
  begin
    ACanvas.Stroke.Color := TAlphaColors.LtGray;
    ACanvas.DrawRect(RectF(ARect.Left, ARect.Top, ARect.Right - 1,
      ARect.Bottom - 1).SnapToPixel(ACanvas.Scale), 0, 0, [],
      ATreeView.Opacity);
  end;
end;

class function TQVTHeaderDrawer.GetAscPath: TPathData;
begin
  if not Assigned(FStatePath[0]) then
    FStatePath[0] := TPathData.Create;
  Result := FStatePath[0];
end;

class function TQVTHeaderDrawer.GetDescPath: TPathData;
begin
  if not Assigned(FStatePath[1]) then
    FStatePath[1] := TPathData.Create;
  Result := FStatePath[1];
end;

{ TQVTCheckCellData }

function TQVTCheckCellData.GetCheckBounds: TRectF;
begin
  Result := FCheckBounds;
end;

function TQVTCheckCellData.GetCheckStates: TQVTCheckStates;
begin
  Result := FCheckStates;
end;

function TQVTCheckCellData.GetFollowStates: Boolean;
begin
  Result := FFollowStates;
end;

function TQVTCheckCellData.GetText: String;
begin
  Result := '';
end;

procedure TQVTCheckCellData.SetCheckBounds(const R: TRectF);
begin
  FCheckBounds := R;
end;

procedure TQVTCheckCellData.SetCheckStates(AStates: TQVTCheckStates);
begin
  if not FUpdating then
  begin
    if FCheckStates <> AStates then
    begin
      FCheckStates := AStates;
      if FFollowStates then
        UpdateStates;
    end;
  end;
end;

procedure TQVTCheckCellData.SetFollowStates(const value: Boolean);
begin
  if FFollowStates <> Value then
  begin
    FFollowStates := Value;
    if Value then
      UpdateStates;
  end;
end;

procedure TQVTCheckCellData.UpdateStates;
  procedure UpdateParent(AParent: TQVTNode);
  var
    AData: IQVTCellData;
    AParentData, ACheckData: IQVTCheckCellData;
    I, ACheckNodeCount, ACheckedCount, AUncheckedCount: Integer;
    AChild: TQVTNode;
  begin
    ACheckedCount := 0;
    AUncheckedCount := 0;
    ACheckNodeCount := 0;
    if Supports(AParent.CellData[Column], IQVTCheckCellData) then
    begin
      AChild := AParent.GetFirstChild;
      while Assigned(AChild) do
      begin
        if Supports(AChild.CellData[Column], IQVTCheckCellData, ACheckData) then
        begin
          Inc(ACheckNodeCount);
          if TQVTCheckState.csChecked in ACheckData.CheckStates then
          begin
            if not(TQVTCheckState.csSomeChecked in ACheckData.CheckStates) then
              Inc(ACheckedCount);
          end
          else
            Inc(AUncheckedCount);
        end;
        AChild := AChild.Next;
      end;
      // 我们不知道是不是共享同一个IQVTCheckCellData还是单独，所以为了安全，我们需要重来一次
      AParentData := AParent.CellData[Column] as IQVTCheckCellData;
      if AUncheckedCount = ACheckNodeCount then
        AParentData.CheckStates := AParentData.CheckStates -
          [TQVTCheckState.csChecked, TQVTCheckState.csSomeChecked]
      else if ACheckedCount = ACheckNodeCount then
        AParentData.CheckStates := AParentData.CheckStates +
          [TQVTCheckState.csChecked] - [TQVTCheckState.csSomeChecked]
      else
        AParentData.CheckStates := AParentData.CheckStates +
          [TQVTCheckState.csChecked, TQVTCheckState.csSomeChecked];
    end;
  end;
  procedure UpdateChildren;
  var
    AChild: TQVTNode;
    AChildData: IQVTCheckCellData;
  begin
    if (Node.ChildCount > 0) and ((not(TQVTCheckState.csChecked in FCheckStates)
      ) or (not(TQVTCheckState.csSomeChecked in FCheckStates))) then
    begin
      AChild := Node.GetFirstChild;
      while Assigned(AChild) do
      begin
        if Supports(AChild.CellData[Column], IQVTCheckCellData, AChildData) then
        begin
          if TQVTCheckState.csChecked in FCheckStates then
            AChildData.CheckStates := AChildData.CheckStates +
              [TQVTCheckState.csChecked]
          else
            AChildData.CheckStates := AChildData.CheckStates -
              [TQVTCheckState.csChecked, TQVTCheckState.csSomeChecked];
        end;
        AChild := AChild.Next;
      end;
    end;
  end;

begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    UpdateParent(Node.Parent);
    UpdateChildren;
  finally
    FUpdating := False;
  end;
end;

end.
