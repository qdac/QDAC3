object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 379
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 49
    Width = 787
    Height = 330
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 787
    Height = 49
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 13
      Width = 129
      Height = 25
      Caption = 'Create Pages'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 147
      Top = 13
      Width = 129
      Height = 25
      Caption = 'MultiInstance'
      TabOrder = 1
    end
    object Button3: TButton
      Left = 279
      Top = 13
      Width = 129
      Height = 25
      Caption = 'ShowModal'
      TabOrder = 2
    end
    object Button4: TButton
      Left = 414
      Top = 13
      Width = 91
      Height = 25
      Caption = 'Custom Action'
      TabOrder = 3
    end
    object Button5: TButton
      Left = 511
      Top = 13
      Width = 90
      Height = 25
      Caption = 'FMX Form'
      TabOrder = 4
    end
    object Button6: TButton
      Left = 607
      Top = 13
      Width = 75
      Height = 25
      Caption = 'ByExecute'
      TabOrder = 5
    end
  end
end
