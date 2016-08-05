object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 144
    ExplicitTop = 24
    ExplicitWidth = 185
    object Button1: TButton
      Left = 360
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 16
      Top = 14
      Width = 305
      Height = 21
      TabOrder = 1
      Text = 'This is your <%RealName - $NickName$%> replace'
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 447
    Height = 295
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    ExplicitLeft = 184
    ExplicitTop = 144
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
