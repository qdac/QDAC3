object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Enum PHP Vars'
  ClientHeight = 336
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 440
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Decode'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 417
    Height = 169
    Lines.Strings = (
      '<html>'
      '<body>'
      '<?php'
      '$text="Hello,world";'
      '$sum=0;'
      '$val1=100;'
      '$val2=200;'
      '$sum=$val1+$val2;'
      '?>'
      '</body>'
      '</html>')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 8
    Top = 192
    Width = 521
    Height = 129
    TabOrder = 2
  end
end
