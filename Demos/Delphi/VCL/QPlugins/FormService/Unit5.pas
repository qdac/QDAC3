unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, FMX.Objects, QPlugins,
  qplugins_fmx_messages, qplugins_formsvc, qplugins_fmx_formsvc, FMX.Edit;

type
  TForm5 = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    FloatAnimation1: TFloatAnimation;
    Edit1: TEdit;
    Edit2: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure ShowFMXForm;
begin
  Form5 := TForm5.Create(nil);
//  Form5.ShowModal;
  FreeAndNil(Form5);
end;
exports ShowFMXForm;

initialization

RegisterFormService('/Services/Docks/Forms', 'FMXForm', TForm5, True);
RegisterFormService('/Services/Docks/Forms', 'FMXDock', TForm5, False).Align :=
  faLeftBottom;

finalization
UnregisterServices('/Services/Docks/Forms', ['FMXForm', 'FMXDock']);

end.
