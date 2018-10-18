object frmSftpBrowser: TfrmSftpBrowser
  Left = 0
  Top = 0
  ActiveControl = edtServer
  Caption = #20351#29992' SFTP '#25171#24320
  ClientHeight = 591
  ClientWidth = 964
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Height = 591
    ExplicitLeft = 488
    ExplicitTop = 264
    ExplicitHeight = 100
  end
  object pcSession: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 591
    ActivePage = tsSessions
    Align = alLeft
    TabOrder = 0
    object tsSessions: TTabSheet
      Caption = #20250#35805
      object lbxSessions: TListBox
        Left = 0
        Top = 201
        Width = 281
        Height = 362
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = lbxSessionsDblClick
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 201
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 14
          Width = 40
          Height = 13
          Caption = #26381#21153#22120':'
        end
        object Label2: TLabel
          Left = 8
          Top = 63
          Width = 40
          Height = 13
          Caption = #29992#25143#21517':'
        end
        object Label3: TLabel
          Left = 8
          Top = 113
          Width = 28
          Height = 13
          Caption = #23494#30721':'
        end
        object edtUser: TEdit
          Left = 24
          Top = 84
          Width = 241
          Height = 21
          TabOrder = 1
        end
        object edtPass: TEdit
          Left = 24
          Top = 134
          Width = 241
          Height = 21
          PasswordChar = '*'
          TabOrder = 2
        end
        object btnConnect: TButton
          Left = 190
          Top = 161
          Width = 75
          Height = 25
          Caption = #36830#25509
          TabOrder = 3
          OnClick = btnConnectClick
        end
        object edtServer: TEdit
          Left = 24
          Top = 36
          Width = 241
          Height = 21
          TabOrder = 0
        end
      end
    end
    object tsDirectory: TTabSheet
      Caption = #30446#24405
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 41
        Align = alTop
        Caption = 'Panel2'
        TabOrder = 0
        object sbSearch: TSpeedButton
          Left = 257
          Top = 1
          Width = 23
          Height = 39
          Hint = #25628#32034#26085#24535#20869#23481
          Align = alRight
          Flat = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFAB75E4924BDCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAB75E47A23D4AC75E4FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAB75
            E47A23D4AC75E4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F1FDDDC7F4BC90E9DD
            C7F4EEE3FAFFFFFFFFFFFFAB75E47A23D4AC75E4FFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFBC90E98A3DD9AC75E4BD91EAB483E78A3ED9AB75E4AB75E47A23D4AC75
            E4FFFFFFFFFFFFFFFFFFFFFFFFF7F1FD924BDCB483E7F7F1FDFFFFFFFFFFFFFF
            FFFFFFFFFFC49EEC7A23D4A368E1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB482E7
            B482E7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCDACEF9A59DEFFFF
            FFFFFFFFFFFFFFFFFFFFEEE3FA8A3DD9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFF9A59DECDACEFFFFFFFFFFFFFFFFFFFC49EECB482E7
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD5BAF2B482
            E7FFFFFFFFFFFFFFFFFFBC90E9C49EECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFDDC7F49A59DEFFFFFFFFFFFFFFFFFFBC90E9BC90E9
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDDC7F49A59
            DEFFFFFFFFFFFFFFFFFFD5BAF2AB75E4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFC49EECBC90E9FFFFFFFFFFFFFFFFFFFFFFFF8A3DD9
            E6D5F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F1FD8A3DD9EEE3
            FAFFFFFFFFFFFFFFFFFFFFFFFFD5BAF2924BDCF7F1FDFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFAB75E4BC90E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            CDACEF812FD6CDACEFF7F1FDFFFFFFFFFFFFD5BAF2924BDCB483E7FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEE3FAAC75E48A3ED97A23D482
            30D6A368E1DDC7F4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = sbSearchClick
          ExplicitLeft = 4
          ExplicitTop = 2
          ExplicitHeight = 22
        end
        object cbxCurrentDir: TComboBox
          AlignWithMargins = True
          Left = 4
          Top = 9
          Width = 250
          Height = 21
          Margins.Top = 8
          Align = alClient
          TabOrder = 0
          OnKeyPress = cbxCurrentDirKeyPress
        end
      end
      object tvDirList: TTreeView
        Left = 0
        Top = 41
        Width = 281
        Height = 522
        Align = alClient
        Indent = 19
        TabOrder = 1
        OnExpanding = tvDirListExpanding
      end
    end
  end
  object Panel1: TPanel
    Left = 292
    Top = 0
    Width = 672
    Height = 591
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object lvFiles: TListView
      Left = 0
      Top = 0
      Width = 672
      Height = 591
      Align = alClient
      Columns = <
        item
          Caption = #25991#20214#21517
          Width = 200
        end
        item
          Alignment = taCenter
          Caption = #23646#24615
          Width = 75
        end
        item
          Alignment = taCenter
          Caption = #25152#26377#32773
          Width = 85
        end
        item
          Alignment = taCenter
          Caption = #38582#23646#32452
          Width = 85
        end
        item
          Alignment = taCenter
          Caption = #25991#20214#22823#23567
          Width = 85
        end
        item
          Alignment = taCenter
          Caption = #20462#25913#26102#38388
          Width = 120
        end>
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = lvFilesDblClick
    end
  end
  object sftpClient: TScSFTPClient
    Left = 456
    Top = 96
  end
  object sshClient: TScSSHClient
    Left = 520
    Top = 96
  end
end
