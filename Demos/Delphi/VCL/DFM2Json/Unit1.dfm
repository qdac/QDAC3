object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DFM2Json'
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
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 40
    ExplicitWidth = 417
    ExplicitHeight = 153
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitLeft = 128
    ExplicitTop = 8
    ExplicitWidth = 185
    object Button1: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Hint = 'Open|Opens an existing file'
      Caption = '&Open...'
      ImageIndex = 7
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object ActionList1: TActionList
    Left = 352
    Top = 8
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
    end
  end
end
