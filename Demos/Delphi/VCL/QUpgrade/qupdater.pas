unit qupdater;

interface

uses classes, sysutils, types, qstring, qjson;

type
  { 更新结果
    usUnknown - 未知
    usPending - 等待更新中
    usDownloading - 正在下载中
    usUpdated - 已经更新
    usReboot - 在重新启动后完成更新
  }

  TQUpdateStatus = (usUnknown,usPending,usDownloading,usUpdated, usReboot);
  TQVersionCompareType=(vctByFileTime,vctByVersion);
  TQFileUpdateMethod=(fumNew,fumServer);
  TQBeforeUpdateFileEvent = procedure(Sender: TObject;
    const AFileName: QStringW; var Allow: Boolean);
  TQAfterUpdateFileEvent = procedure(Sender: TObject;
    const AFileName: QStringW);
  TQUpdateFileEvent = procedure(Sender: TObject;
    const AOldFile, ANewFile: QStringW; var AResult: TQUpdateStatus);

  TQFileVersion = record
    case Integer of
      0:
        (Major, Minor, Release, Build: Word);
      1:
        (Value: Int64);
  end;

  TQUpgradeItem = class(TCollectionItem)
  private
    FVersion: TQFileVersion;
    FSourceUrl: QStringW;
    FTargetFile: QStringW;
    FTimeStamp:TDateTime;
    FDesc: QStringW;
    FMethod: TQFileUpdateMethod;
    FCompareType: TQVersionCompareType;
    FResult: TQUpdateStatus;
  public
    constructor Create;
    property SourceUrl: QStringW read FSourceUrl write FSourceUrl;
    property TargetFile: QStringW read FTargetFile write FTargetFile;
    property Version: TQFileVersion read FVersion write FVersion;
    property TimeStamp:TDateTime read FTimeStamp write FTimeStamp;
    property Desc:QStringW read FDesc write FDesc;
    property Method:TQFileUpdateMethod read FMethod write FMethod;
    property CompareType:TQVersionCompareType read FCompareType write FCompareType;
    property Result:TQUpdateStatus read FResult write FResult;
  end;

  TQUpgradeItems = class(TCollection)

  end;

  TQUpgrade = class(TComponent)
  private
    FTempDir: QStringW;
    FInfUrl: QStringW;
    FSocket: THandle;
    FOnUpdateFile: TQUpdateFileEvent;
    FBeforeUpdateFile: TQBeforeUpdateFileEvent;
    FAfterUpdateFile: TQAfterUpdateFileEvent;
    FItems: TQUpgradeItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property InfUrl: QStringW read FInfUrl write FInfUrl;
    property TempDir: QStringW read FTempDir write FTempDir;
    property BeforeUpdateFile: TQBeforeUpdateFileEvent read FBeforeUpdateFile
      write FBeforeUpdateFile;
    property AfterUpdateFile: TQAfterUpdateFileEvent read FAfterUpdateFile
      write FAfterUpdateFile;
    property OnUpdateFile: TQUpdateFileEvent read FOnUpdateFile
      write FOnUpdateFile;
    property Items: TQUpgradeItems read FItems;
  end;

implementation

{ TQUpgrade }

constructor TQUpgrade.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TQUpgrade.Destroy;
begin

  inherited;
end;

{ TQUpgradeItem }

constructor TQUpgradeItem.Create;
begin

end;

end.
