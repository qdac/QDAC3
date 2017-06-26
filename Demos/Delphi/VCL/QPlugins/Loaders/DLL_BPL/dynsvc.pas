unit dynsvc;

interface

uses classes, qstring, qplugins_params, qplugins,qplugins_base;

type
  TQSumService = class(TQService)
  public
    function Execute(AParams, AResult: IQParams): Boolean; override;
  end;

implementation

{ TQSumService }

function TQSumService.Execute(AParams, AResult: IQParams): Boolean;
begin
  AResult.Add('Result', ptInt64).AsInteger := AParams[0].AsInteger +
    AParams[1].AsInteger;
  Result := True;
end;

initialization

RegisterServices('Services/Math', [TQSumService.Create(NewId, 'Sum')]);

finalization

UnregisterServices('Services/Math', ['Sum']);

end.
