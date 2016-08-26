object Form4: TForm4
  Left = 0
  Top = 0
  Align = alClient
  Caption = 'Form4'
  ClientHeight = 201
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 447
    Height = 201
    Align = alClient
    Brush.Color = clSkyBlue
    Pen.Color = clRed
    ExplicitLeft = 200
    ExplicitTop = 88
    ExplicitWidth = 65
    ExplicitHeight = 65
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 149
    Height = 13
    Caption = #36825#26159#38745#24577#38142#25509#30340'DLL'#37324#30340#31383#20307
  end
  object Edit1: TEdit
    Left = 24
    Top = 40
    Width = 121
    Height = 21
    Hint = 'This is static DLL hint'
    ParentShowHint = False
    ShowHint = True
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
    Left = 208
    Top = 56
    Width = 105
    Height = 25
    Caption = #20851#38381#24182#37322#25918
    TabOrder = 2
    OnClick = Button1Click
  end
end
