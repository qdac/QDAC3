object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Plan Masks'
  ClientHeight = 495
  ClientWidth = 709
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
    Width = 709
    Height = 454
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 709
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 305
      Height = 35
      Align = alLeft
      Caption = 
        #25513#30721#26684#24335#20026' '#31186' '#20998' '#26102' '#26085' '#26376' '#21608' ['#24180'] '#38468#21152#20869#23481#13#10'Mask format is Second Minute Hour Da' +
        'y Month WeekDay [Year]'
      Layout = tlCenter
      ExplicitHeight = 26
    end
    object Button1: TButton
      Left = 392
      Top = 10
      Width = 150
      Height = 25
      Caption = #27979#35797#29992#20363'(Test case)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 548
      Top = 10
      Width = 150
      Height = 25
      Caption = #33258#23450#20041'(Custom)'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
