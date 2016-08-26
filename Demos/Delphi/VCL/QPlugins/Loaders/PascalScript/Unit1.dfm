object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #33050#26412#21152#36733#22120#28436#31034#31034#20363'(Demo)'
  ClientHeight = 320
  ClientWidth = 626
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 626
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 24
      Top = 9
      Width = 75
      Height = 25
      Caption = #26522#20030#26381#21153
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 112
      Top = 10
      Width = 75
      Height = 25
      Caption = #27714#21644
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 626
    Height = 279
    Align = alClient
    Lines.Strings = (
      #26412#31034#20363#31243#24207#28436#31034#20102#22914#20309#20351#29992' ldr_pascal.pas '#26469#21152#36733#30001#33050#26412#25552#20379#30340#26381#21153#12290
      #26412#21333#20803#38656#35201' JCL '#30340#33050#26412#35299#37322#22120#25903#25345#65292#20320#21487#20197#26367#25442' ldr_pascal '#20013#30340#33050#26412#35299#37322#22120#20026#33258#24049#30340#20854#23427#35299#37322#22120#12290)
    TabOrder = 1
  end
end
