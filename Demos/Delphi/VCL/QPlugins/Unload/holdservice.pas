unit holdservice;

interface

uses qstring, qplugins, qplugins_params;

type
  // 这个只是用来测试，实际上什么也不干
  THoldService = class(TQService)

  end;

implementation

initialization

RegisterServices('/Services', [THoldService.Create(NewId, 'HoldService')]);

finalization

UnregisterServices('/Services', ['HoldService']);

end.
