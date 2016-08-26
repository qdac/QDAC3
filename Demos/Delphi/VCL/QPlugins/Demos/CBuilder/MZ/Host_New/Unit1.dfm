object Form_Host: TForm_Host
  Left = 0
  Top = 0
  Caption = 'Form_Host'
  ClientHeight = 368
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 698
    Height = 327
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 698
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Button1: TButton
      Left = 24
      Top = 8
      Width = 75
      Height = 25
      Caption = 'DockTo'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 112
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Show'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 200
      Top = 8
      Width = 75
      Height = 25
      Caption = 'ShowModal'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
end
