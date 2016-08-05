program msgpackview;

uses
  Forms,
  main in 'main.pas' {frmMain},
  find in 'find.pas' {frmFind},
  valueeditor in 'valueeditor.pas' {frmNodeValueEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmFind, frmFind);
  Application.CreateForm(TfrmNodeValueEditor, frmNodeValueEditor);
  Application.Run;
end.
