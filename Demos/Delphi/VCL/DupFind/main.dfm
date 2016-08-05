object Form6: TForm6
  Left = 0
  Top = 0
  Caption = #37325#22797#25991#20214#26597#25214#24037#20855
  ClientHeight = 410
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Splitter1: TSplitter
    Left = 241
    Top = 0
    Height = 328
    Align = alRight
    ExplicitLeft = 296
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 241
    Height = 328
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 239
      Height = 24
      Align = alTop
      Caption = #37325#22797#25991#20214
      TabOrder = 0
    end
    object vstDupFiles: TVirtualStringTree
      Left = 1
      Top = 25
      Width = 239
      Height = 302
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Colors.UnfocusedColor = clMedGray
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.MainColumn = -1
      PopupMenu = PopupMenu1
      TabOrder = 1
      OnGetText = vstDupFilesGetText
      OnInitChildren = vstDupFilesInitChildren
      OnInitNode = vstDupFilesInitNode
      Columns = <>
    end
  end
  object Panel3: TPanel
    Left = 244
    Top = 0
    Width = 491
    Height = 328
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 485
      Height = 246
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 479
        Height = 24
        Align = alTop
        Caption = #26412#24037#20855#23558#25628#32034#25351#23450#39537#21160#22120#19979#30340#25152#26377#37325#22797#25991#20214#65292#20351#29992#20102'QDAC'#30340'QDigest'#12289'QWorker'#12289'QRBTree'#19977#20010#21333#20803#65292#20197#21152#24555#22788#29702#36895#24230'.'
        WordWrap = True
        ExplicitWidth = 474
      end
      object lblLastResults: TLabel
        Left = 6
        Top = 198
        Width = 6
        Height = 12
      end
      object Label3: TLabel
        Left = 8
        Top = 225
        Width = 78
        Height = 12
        Caption = #26368#23567#25991#20214#22823#23567':'
      end
      object clbDrives: TCheckListBox
        Left = 6
        Top = 62
        Width = 475
        Height = 72
        Columns = 6
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        ItemHeight = 13
        TabOrder = 0
        OnClick = clbDrivesClick
      end
      object rbByDrive: TRadioButton
        Left = 6
        Top = 39
        Width = 113
        Height = 17
        Caption = #25353#39537#21160#22120#26597#25214
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = rbByDriveClick
      end
      object rbByDir: TRadioButton
        Left = 3
        Top = 141
        Width = 113
        Height = 17
        Caption = #21482#26816#32034#29305#23450#30446#24405
        TabOrder = 2
        OnClick = rbByDriveClick
      end
      object edtSearchDir: TEdit
        Left = 6
        Top = 164
        Width = 451
        Height = 20
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 3
      end
      object btnBrowse: TButton
        Left = 455
        Top = 162
        Width = 26
        Height = 25
        Caption = '...'
        TabOrder = 4
        OnClick = btnBrowseClick
      end
      object btnStart: TButton
        Left = 394
        Top = 193
        Width = 75
        Height = 25
        Caption = #25195#25551'(&S)'
        TabOrder = 5
        OnClick = btnStartClick
      end
      object btnSelectAll: TButton
        Left = 325
        Top = 33
        Width = 75
        Height = 25
        Caption = #20840#36873'(&A)'
        TabOrder = 6
        OnClick = btnSelectAllClick
      end
      object btnDeselectAll: TButton
        Left = 406
        Top = 33
        Width = 75
        Height = 25
        Caption = #20840#21542'(&A)'
        TabOrder = 7
        OnClick = btnDeselectAllClick
      end
      object chkByQuickHash: TCheckBox
        Left = 272
        Top = 197
        Width = 97
        Height = 17
        Alignment = taLeftJustify
        Caption = #20351#29992#24555#36895#27604#36739
        TabOrder = 8
      end
      object edtMinFileSize: TEdit
        Left = 86
        Top = 223
        Width = 91
        Height = 20
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 9
        Text = '0B'
      end
    end
    object sbxProgress: TScrollBox
      Left = 0
      Top = 252
      Width = 491
      Height = 76
      HorzScrollBar.Smooth = True
      HorzScrollBar.Style = ssHotTrack
      VertScrollBar.Smooth = True
      VertScrollBar.Style = ssHotTrack
      Align = alClient
      BevelKind = bkTile
      BorderStyle = bsNone
      TabOrder = 1
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 328
    Width = 735
    Height = 82
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 2
    object mmLogs: TMemo
      Left = 25
      Top = 1
      Width = 709
      Height = 80
      Align = alClient
      BorderStyle = bsNone
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 0
    end
    object Panel6: TPanel
      Left = 1
      Top = 1
      Width = 24
      Height = 80
      Align = alLeft
      TabOrder = 1
      object Label2: TLabel
        Left = 1
        Top = 23
        Width = 22
        Height = 56
        Align = alClient
        Alignment = taCenter
        Caption = #26085#24535
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 12
        ExplicitHeight = 24
      end
      object sbCloseLog: TSpeedButton
        Left = 1
        Top = 1
        Width = 22
        Height = 22
        Align = alTop
        Caption = 'r'
        Flat = True
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Marlett'
        Font.Style = []
        ParentFont = False
        OnClick = sbCloseLogClick
        ExplicitLeft = 8
        ExplicitTop = 16
        ExplicitWidth = 23
      end
    end
  end
  object tmProgress: TTimer
    OnTimer = tmProgressTimer
    Left = 416
    Top = 136
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 144
    Top = 200
    object miOpenFolder: TMenuItem
      Caption = #25171#24320#25991#20214#20301#32622'(&L)'
      OnClick = miOpenFolderClick
    end
    object miOpenFile: TMenuItem
      Caption = #25171#24320#25991#20214'(&O)'
      OnClick = miOpenFileClick
    end
    object N1: TMenuItem
      Caption = #20108#36827#21046#27604#36739'(&C)'
      OnClick = N1Click
    end
    object miBinaryCmpAll: TMenuItem
      Caption = #20840#37096#25353#20108#36827#21046#27604#36739
      OnClick = miBinaryCmpAllClick
    end
  end
end
