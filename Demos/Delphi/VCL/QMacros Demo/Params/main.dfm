object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21442#25968#21270#23439'(Paramed macros)'
  ClientHeight = 285
  ClientWidth = 565
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
    Left = 16
    Top = 24
    Width = 70
    Height = 13
    Caption = #34920#36798#24335'(Expr):'
  end
  object edtExpr: TEdit
    Left = 92
    Top = 21
    Width = 333
    Height = 21
    TabOrder = 0
    Text = '1860%Rand(1000,9999)%1531'
  end
  object Button1: TButton
    Left = 440
    Top = 19
    Width = 97
    Height = 25
    Caption = #29983#25104'(Generate)'
    TabOrder = 1
    OnClick = Button1Click
  end
  object mmResults: TMemo
    Left = 8
    Top = 56
    Width = 537
    Height = 209
    TabOrder = 2
  end
end
