object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 299
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
  object Button1: TButton
    Left = 40
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Simple Post'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 40
    Top = 72
    Width = 529
    Height = 201
    TabOrder = 1
  end
  object Button2: TButton
    Left = 136
    Top = 24
    Width = 89
    Height = 25
    Caption = 'Wrap for Delphi'
    TabOrder = 2
    OnClick = Button2Click
  end
end
