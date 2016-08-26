object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 201
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 112
    Width = 275
    Height = 21
    Caption = #25105#26159'DLL'#25554#20214#37324#30340#36890#30693#25509#25910#32773
    Font.Charset = GB2312_CHARSET
    Font.Color = clMaroon
    Font.Height = -21
    Font.Name = #38582#20070
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 152
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 16
    Width = 97
    Height = 17
    Caption = #25509#25910#36890#30693
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 56
    Width = 297
    Height = 17
    TabOrder = 1
  end
end
