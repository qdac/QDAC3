object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #25968#25454#38598#36895#24230#27604#36739
  ClientHeight = 372
  ClientWidth = 685
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
    Left = 584
    Top = 0
    Width = 101
    Height = 372
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = #24320#22987#27979#35797
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object mmLog: TMemo
    Left = 0
    Top = 0
    Width = 584
    Height = 372
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 336
    Top = 192
  end
end
