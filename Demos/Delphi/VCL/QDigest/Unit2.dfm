object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
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
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 447
    Height = 160
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 41
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = 'MD5'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 112
      Top = 10
      Width = 75
      Height = 25
      Caption = 'SHA'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 272
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 353
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Button4'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 216
    Top = 104
  end
end
