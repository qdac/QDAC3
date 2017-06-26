unit dbthread;

interface

uses classes, sysutils, adodb, adoint, db, qstring, qplugins_params, qplugins,qplugins_base,
  qplugins_vcl_messages, activex;

type
  IDBService = interface
    ['{D0B7605F-6E95-4F0E-81AD-E282BBC2A1E1}']
    function OpenDataStream(AStream: IQStream; ACmdText: PWideChar): Boolean;
    function ExecuteCmd(ACmdText: PWideChar): Boolean;
  end;

  TQDBService = class(TQService, IDBService)
  protected
    FDataSet: TADODataSet;
    FConnection: TADOConnection;
    procedure CheckConnection;
    function OpenDataStream(AStream: IQStream; ACmdText: PWideChar): Boolean;
    function ExecuteCmd(ACmdText: PWideChar): Boolean;
  public
    constructor Create(const AId: TGuid; AName: QStringW); override;
    destructor Destroy; override;
  end;

implementation

const
  IID_ADODBService: TGuid = '{2D774A42-82B5-4C03-82C0-0AB04EE606BE}';
  { TQDBService }

procedure TQDBService.CheckConnection;
var
  S: WideString;
begin
  if not FConnection.Connected then
  begin
    S := PromptDataSource(0, '');
    if Length(S) > 0 then
    begin
      FConnection.ConnectionString := S;
      FConnection.Connected := True;
    end;
  end;
end;

constructor TQDBService.Create(const AId: TGuid; AName: QStringW);
begin
  inherited;
  FConnection := TADOConnection.Create(nil);
  FDataSet := TADODataSet.Create(nil);
  FDataSet.Connection := FConnection;
end;

destructor TQDBService.Destroy;
begin
  FreeObject(FConnection);
  FreeObject(FDataSet);
  inherited;
end;

function TQDBService.ExecuteCmd(ACmdText: PWideChar): Boolean;
var
  L: Integer;
begin
  try
    CheckConnection;
    FConnection.Execute(ACmdText, L);
    Result := True;
  except
    on E: Exception do
    begin
      SetLastError(1, E.Message);
      Result := False;
    end;
  end;
end;

function TQDBService.OpenDataStream(AStream: IQStream;
  ACmdText: PWideChar): Boolean;
var
  ATemp: TQStream;
begin
  ATemp := QStream(AStream);
  try
    CheckConnection;
    FDataSet.Close;
    FDataSet.CommandText := ACmdText;
    FDataSet.Open;
    OLEVariant(FDataSet.Recordset).Save(TStreamAdapter.Create(ATemp)
      as IUnknown, adPersistADTG); // adPersistXML
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      SetLastError(2, E.Message);
    end;
  end;
  FreeObject(ATemp);
end;

initialization

CoInitialize(nil);
RegisterServices('Services/DBProxy',
  [TQDBService.Create(IID_ADODBService, 'ADO')]);

finalization

UnregisterServices('Services/DBProxy', ['ADO']);
CoUninitialize;

end.
