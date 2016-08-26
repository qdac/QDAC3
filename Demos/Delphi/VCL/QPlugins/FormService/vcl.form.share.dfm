object Form3: TForm3
  Left = 30
  Top = 30
  Caption = 'Form3'
  ClientHeight = 201
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 447
    Height = 201
    Align = alClient
    Brush.Color = clCream
    Pen.Color = clRed
    ExplicitLeft = 200
    ExplicitTop = 88
    ExplicitWidth = 65
    ExplicitHeight = 65
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 142
    Height = 13
    Caption = #36825#26159#20849#20139'BPL'#30340'DLL'#37324#30340#31383#20307
  end
  object Label2: TLabel
    Left = 24
    Top = 104
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Edit1: TEdit
    Left = 24
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 24
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit2'
  end
  object Button1: TButton
    Left = 176
    Top = 40
    Width = 113
    Height = 25
    Caption = #36890#36807#23646#24615#20256#20540
    TabOrder = 2
    OnClick = Button1Click
  end
end
