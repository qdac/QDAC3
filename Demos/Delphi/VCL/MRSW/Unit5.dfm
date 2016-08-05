object Form5: TForm5
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Form5'
  ClientHeight = 283
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = #26411#27425#32467#26524':'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 52
    Height = 13
    Caption = #27979#35797#27425#25968':'
  end
  object Label3: TLabel
    Left = 168
    Top = 32
    Width = 52
    Height = 13
    Caption = #35835#21462#21344#27604':'
  end
  object Button1: TButton
    Left = 8
    Top = 61
    Width = 75
    Height = 25
    Caption = 'MRSW'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 94
    Top = 61
    Width = 75
    Height = 25
    Caption = 'CS'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 103
    Width = 431
    Height = 162
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 2
  end
  object WinMRSW: TButton
    Left = 180
    Top = 62
    Width = 75
    Height = 25
    Caption = 'WinMRSW'
    TabOrder = 3
    OnClick = WinMRSWClick
  end
  object SpinEdit1: TSpinEdit
    Left = 226
    Top = 29
    Width = 47
    Height = 22
    MaxValue = 9
    MinValue = 1
    TabOrder = 4
    Value = 8
  end
  object Edit1: TEdit
    Left = 66
    Top = 29
    Width = 96
    Height = 21
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 5
    Text = '1000000'
  end
  object Button3: TButton
    Left = 267
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Sysutils'
    TabOrder = 6
    OnClick = Button3Click
  end
end
