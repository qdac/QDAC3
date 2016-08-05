object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 201
  ClientWidth = 447
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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open Server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OpenClient'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 344
    Top = 40
    Width = 95
    Height = 25
    Caption = 'Send To Client'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 44
    Width = 330
    Height = 21
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 3
  end
  object Button4: TButton
    Left = 344
    Top = 67
    Width = 95
    Height = 25
    Caption = 'Send To Server'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Edit2: TEdit
    Left = 8
    Top = 71
    Width = 330
    Height = 21
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 5
  end
  object Memo1: TMemo
    Left = 8
    Top = 98
    Width = 431
    Height = 95
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 6
  end
  object Button5: TButton
    Left = 170
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 7
    OnClick = Button5Click
  end
end
