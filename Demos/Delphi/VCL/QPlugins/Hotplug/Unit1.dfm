object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #26381#21153#28909#25554#25320#31034#20363
  ClientHeight = 421
  ClientWidth = 594
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 594
    Height = 380
    Align = alClient
    Lines.Strings = (
      #26412#31034#20363#28436#31034#30340#26159#36890#36807#25193#23637#25509#21475#26367#25442#26381#21153#26412#36523#30340#21151#33021#26469#23454#29616#19994#21153#30340#28909#25554#25320#12290
      #28857#20987#35843#29992#26381#21153#25353#38062#21487#20197#35843#29992#26381#21153#24182#22312#19979#38754#36755#20986#19968#26465#26085#24535#12290
      #22914#26524#26410#26367#25442#26381#21153#65292#21017#26174#31034#30340#26159' TEchoService:Service hotplug demo'
      #22914#26524#36890#36807#25193#23637#26367#25442#26381#21153#65292#21017#26174#31034#30340#26159'TEchoReplacement:Service hotplug demo')
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 47
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 594
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitLeft = 208
    ExplicitTop = 208
    ExplicitWidth = 185
    object Button1: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = #35843#29992#26381#21153
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = #26367#25442#26381#21153
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
