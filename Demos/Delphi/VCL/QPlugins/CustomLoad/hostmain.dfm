object Form2: TForm2
  Left = 0
  Top = 0
  Caption = #21160#24577#21152#36733#25554#20214#31034#20363
  ClientHeight = 465
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 134
    Height = 13
    Caption = #21152#36733#25554#20214#65288#25903#25345'DLL/BPL'#65289
  end
  object edtPluginsFile: TEdit
    Left = 8
    Top = 32
    Width = 521
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 535
    Top = 30
    Width = 75
    Height = 25
    Caption = #21152#36733#25554#20214
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 59
    Width = 619
    Height = 398
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.dll'
    Filter = 'DLL|*.dll|BPL|*.bpl'
    Left = 312
    Top = 240
  end
end
