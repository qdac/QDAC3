unit bplsvc;

interface

uses classes, sysutils, qstring, qplugins;

type
  ISumService = interface
    ['{8501374C-793F-48FF-ACE7-898E32EBA174}']
    function Sum(X, Y: Integer): Integer; stdcall;
  end;

  TSumService = class(TQService, ISumService)
  protected
    function Sum(X, Y: Integer): Integer; stdcall;
  public

  end;

implementation

const
  IID_SumService: TGuid = '{F6C30733-5BE0-454B-ACA0-401665D06407}';
  { TSumService }

function TSumService.Sum(X, Y: Integer): Integer;
begin
  Result := X + Y;
end;

initialization

RegisterServices('Services', [TSumService.Create(IID_SumService, 'Sum')]);

finalization

UnregisterServices('Services', ['Sum']);

end.
