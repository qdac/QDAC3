object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 410
  ClientWidth = 757
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 73
    Width = 757
    Height = 337
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 757
    Height = 73
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 13
      Width = 129
      Height = 25
      Caption = #21019#24314#21333#23454#20363#39029#31614
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 148
      Top = 13
      Width = 129
      Height = 25
      Caption = #26174#31034#22810#23454#20363#31383#20307
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 281
      Top = 13
      Width = 129
      Height = 25
      Caption = #27169#24577#26174#31034
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 414
      Top = 13
      Width = 129
      Height = 25
      Caption = #33258#23450#20041#21160#20316
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 16
      Top = 44
      Width = 129
      Height = 25
      Caption = 'FMX '#31383#20307
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 281
      Top = 44
      Width = 129
      Height = 25
      Caption = #36890#36807'Execute'#25191#34892
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 148
      Top = 44
      Width = 129
      Height = 25
      Caption = #22810#23454#20363#39029#31614
      TabOrder = 6
      OnClick = Button7Click
    end
    object ComboBox1: TComboBox
      Left = 549
      Top = 14
      Width = 145
      Height = 21
      ItemIndex = 0
      TabOrder = 7
      Text = 'faNone'
      OnChange = ComboBox1Change
      Items.Strings = (
        'faNone'
        'faDefault'
        'faLeftTop'
        'faCenterTop'
        'faTop'
        'faRightTop'
        'faRightCenter'
        'faRight'
        'faRightBottom,'
        'faCenterBottom,'
        'faBottom'
        'faLeftBottom'
        'faLeft'
        'faLeftCenter'
        'faContent'
        'faCenter'
        'faHoriz'
        'faVert')
    end
    object Button8: TButton
      Left = 416
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Button8'
      TabOrder = 8
      OnClick = Button8Click
    end
  end
end
