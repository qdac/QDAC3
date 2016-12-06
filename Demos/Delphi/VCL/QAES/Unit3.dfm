object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'AES'#21152#23494#28436#31034
  ClientHeight = 356
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 345
    Top = 105
    Height = 251
    ExplicitLeft = 440
    ExplicitTop = 73
    ExplicitHeight = 307
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 655
    Height = 105
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 13
      Width = 75
      Height = 25
      Caption = 'ECB'#21152#23494
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 112
      Top = 13
      Width = 75
      Height = 25
      Caption = #35299#23494
      TabOrder = 1
      OnClick = Button2Click
    end
    object CBC: TButton
      Left = 200
      Top = 13
      Width = 75
      Height = 25
      Caption = 'CBC'
      TabOrder = 2
      OnClick = CBCClick
    end
    object Button3: TButton
      Left = 288
      Top = 13
      Width = 75
      Height = 25
      Caption = #27969#21152#23494
      TabOrder = 3
      OnClick = Button3Click
    end
    object rgAlog: TRadioGroup
      Left = 16
      Top = 47
      Width = 129
      Height = 52
      Caption = #21152#23494#31639#27861
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'ECB'
        'CBC')
      TabOrder = 4
    end
    object rgKeyLen: TRadioGroup
      Left = 151
      Top = 47
      Width = 178
      Height = 52
      Caption = #23494#38053#24378#24230
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        '128 '#20301
        '192 '#20301
        '256 '#20301)
      TabOrder = 5
    end
    object GroupBox1: TGroupBox
      Left = 335
      Top = 47
      Width = 290
      Height = 52
      Caption = #23494#38053
      TabOrder = 6
      object edtKey: TEdit
        Left = 6
        Top = 21
        Width = 279
        Height = 21
        TabOrder = 0
        Text = 'QDAC.Demo'
      end
    end
    object ProgressBar1: TProgressBar
      Left = 369
      Top = 18
      Width = 256
      Height = 17
      TabOrder = 7
      Visible = False
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 105
    Width = 345
    Height = 251
    Align = alLeft
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    Lines.Strings = (
      #21069#38754#26377#20010#22823#22351#34507#65292#25235#20303#20182#65281
      #21861#65292#25235#19981#20303#65311#20320#21643#37027#31528#21602#65292#25105#35201#19981#26159#23475#24597#65292#25105#26089#19978#20102#65281
      #21861#65292#20320#20063#23475#24597#65311#24971#29322#23376#29609#24847'~~~')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 348
    Top = 105
    Width = 307
    Height = 251
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 2
  end
end
