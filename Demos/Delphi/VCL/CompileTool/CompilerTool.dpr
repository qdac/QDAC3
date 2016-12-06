program CompilerTool;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form_Main},
  Unit2 in 'Unit2.pas' {Form_PathSetup},
  Common in 'Common.pas',
  Unit3 in 'Unit3.pas' {Frame_Compiler: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'CompilerTool';
  Application.CreateForm(TForm_Main, Form_Main);
  Application.Run;
end.
