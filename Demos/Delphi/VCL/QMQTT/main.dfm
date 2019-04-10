object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MQTT Client '
  ClientHeight = 519
  ClientWidth = 934
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 934
    Height = 68
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    OnClick = Panel1Click
    object Label6: TLabel
      Left = 8
      Top = 45
      Width = 72
      Height = 16
      Caption = #23458#25143#31471'ID'#65306
    end
    object leServerHost: TLabeledEdit
      Left = 88
      Top = 11
      Width = 121
      Height = 24
      EditLabel.Width = 80
      EditLabel.Height = 16
      EditLabel.Caption = #26381#21153#22120#22320#22336':'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = '16e09j0.mqtt.iot.gz.baidubce.com'
    end
    object leServerPort: TLabeledEdit
      Left = 290
      Top = 11
      Width = 55
      Height = 24
      EditLabel.Width = 50
      EditLabel.Height = 16
      EditLabel.Caption = #31471#21475#21495':'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = '1883'
    end
    object leUserName: TLabeledEdit
      Left = 416
      Top = 11
      Width = 129
      Height = 24
      EditLabel.Width = 50
      EditLabel.Height = 16
      EditLabel.Caption = #29992#25143#21517':'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = '16e09j0/guest'
    end
    object lePassword: TLabeledEdit
      Left = 599
      Top = 11
      Width = 138
      Height = 24
      EditLabel.Width = 35
      EditLabel.Height = 16
      EditLabel.Caption = #23494#30721':'
      LabelPosition = lpLeft
      PasswordChar = '*'
      TabOrder = 3
      Text = 'ZLZPvc0i9Tavm2EL'
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 824
      Top = 8
      Width = 107
      Height = 52
      Margins.Top = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = #36830#25509
      TabOrder = 4
      OnClick = Button1Click
    end
    object chkAutoSend: TCheckBox
      Left = 469
      Top = 45
      Width = 116
      Height = 17
      Caption = #33258#21160#21457#36865#27979#35797
      TabOrder = 5
      OnClick = chkAutoSendClick
    end
    object chkAutoClearLog: TCheckBox
      Left = 608
      Top = 45
      Width = 185
      Height = 17
      Caption = #27599#38548'15'#31186#28165#31354#19968#27425#26085#24535
      TabOrder = 6
    end
    object edtClientId: TEdit
      Left = 86
      Top = 41
      Width = 198
      Height = 24
      TabOrder = 7
      Text = 'qdac_test'
    end
    object chkSSL: TCheckBox
      Left = 368
      Top = 45
      Width = 97
      Height = 17
      Caption = 'SSL'
      TabOrder = 8
      OnClick = chkSSLClick
    end
    object cbxVersion: TComboBox
      Left = 743
      Top = 11
      Width = 65
      Height = 24
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 9
      Text = '3.1.1'
      Items.Strings = (
        '3.1.1'
        '5.0')
    end
    object Button2: TButton
      Left = 287
      Top = 40
      Width = 75
      Height = 25
      Caption = #25442#19968#20010
      TabOrder = 10
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 68
    Width = 934
    Height = 428
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 490
      Top = 0
      Height = 428
      ExplicitLeft = 464
      ExplicitTop = 192
      ExplicitHeight = 100
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 490
      Height = 428
      Align = alLeft
      BevelOuter = bvLowered
      ShowCaption = False
      TabOrder = 0
      object Panel5: TPanel
        Left = 1
        Top = 1
        Width = 488
        Height = 32
        Align = alTop
        TabOrder = 0
        object Label1: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 65
          Height = 24
          Align = alLeft
          Caption = #25509#25910#20027#39064':'
          Layout = tlCenter
          ExplicitHeight = 16
        end
        object btnSubscribe: TButton
          AlignWithMargins = True
          Left = 224
          Top = 4
          Width = 75
          Height = 24
          Align = alRight
          Caption = #35746#38405
          TabOrder = 0
          OnClick = btnSubscribeClick
        end
        object edtSubscribeTopic: TEdit
          AlignWithMargins = True
          Left = 75
          Top = 4
          Width = 143
          Height = 24
          Align = alClient
          TabOrder = 1
          Text = '/Topic1,/Topic2'
        end
        object Panel9: TPanel
          Left = 302
          Top = 1
          Width = 185
          Height = 30
          Align = alRight
          BevelOuter = bvNone
          ShowCaption = False
          TabOrder = 2
          object Label5: TLabel
            Left = 8
            Top = 8
            Width = 63
            Height = 16
            Caption = 'QoS '#32423#21035':'
          end
          object cbxRecvQoSLevel: TComboBox
            Left = 77
            Top = 3
            Width = 100
            Height = 22
            Style = csOwnerDrawFixed
            ItemIndex = 0
            TabOrder = 0
            Text = #26368#22810#19968#27425
            Items.Strings = (
              #26368#22810#19968#27425
              #33267#23569#19968#27425
              #21482#21457#19968#27425)
          end
        end
      end
      object Memo1: TMemo
        Left = 1
        Top = 33
        Width = 488
        Height = 394
        Align = alClient
        TabOrder = 1
      end
    end
    object Panel4: TPanel
      Left = 493
      Top = 0
      Width = 441
      Height = 428
      Align = alClient
      BevelOuter = bvLowered
      ShowCaption = False
      TabOrder = 1
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 439
        Height = 32
        Align = alTop
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 65
          Height = 24
          Align = alLeft
          Caption = #21457#24067#20027#39064':'
          Layout = tlCenter
          ExplicitHeight = 16
        end
        object edtPublishTopic: TEdit
          AlignWithMargins = True
          Left = 75
          Top = 4
          Width = 175
          Height = 24
          Align = alClient
          TabOrder = 0
          Text = '/Topic1'
        end
        object Panel8: TPanel
          Left = 253
          Top = 1
          Width = 185
          Height = 30
          Align = alRight
          BevelOuter = bvNone
          ShowCaption = False
          TabOrder = 1
          object Label4: TLabel
            Left = 8
            Top = 8
            Width = 63
            Height = 16
            Caption = 'QoS '#32423#21035':'
          end
          object cbxQoSLevel: TComboBox
            Left = 77
            Top = 3
            Width = 100
            Height = 22
            Style = csOwnerDrawFixed
            ItemIndex = 0
            TabOrder = 0
            Text = #26368#22810#19968#27425
            Items.Strings = (
              #26368#22810#19968#27425
              #33267#23569#19968#27425
              #21482#21457#19968#27425)
          end
        end
      end
      object Panel7: TPanel
        Left = 1
        Top = 395
        Width = 439
        Height = 32
        Align = alBottom
        TabOrder = 1
        object Label3: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 65
          Height = 24
          Align = alLeft
          Caption = #21457#24067#20869#23481':'
          Layout = tlCenter
          ExplicitHeight = 16
        end
        object btnPublish: TButton
          AlignWithMargins = True
          Left = 360
          Top = 4
          Width = 75
          Height = 24
          Align = alRight
          Caption = #21457#24067
          TabOrder = 0
          OnClick = btnPublishClick
        end
        object edtMessage: TEdit
          AlignWithMargins = True
          Left = 75
          Top = 4
          Width = 279
          Height = 24
          Align = alClient
          TabOrder = 1
          Text = 'Hello,world'
        end
      end
      object Memo2: TMemo
        Left = 1
        Top = 33
        Width = 439
        Height = 362
        Align = alClient
        TabOrder = 2
      end
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 496
    Width = 934
    Height = 23
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    TabOrder = 2
  end
  object tmSend: TTimer
    Enabled = False
    Interval = 20
    OnTimer = tmSendTimer
    Left = 592
    Top = 304
  end
  object tmStatics: TTimer
    OnTimer = tmStaticsTimer
    Left = 680
    Top = 304
  end
end
