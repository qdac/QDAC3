object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QLog'#36319#36394#22120'(syslog'#21327#35758')'
  ClientHeight = 405
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 200
    Top = 41
    Height = 329
    ExplicitLeft = 368
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object vstHosts: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 200
    Height = 329
    Align = alLeft
    Colors.UnfocusedColor = clMedGray
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 17
    Header.MainColumn = -1
    TabOrder = 0
    OnFocusChanged = vstHostsFocusChanged
    OnGetText = vstHostsGetText
    OnKeyDown = vstHostsKeyDown
    Columns = <>
  end
  object Panel1: TPanel
    Left = 203
    Top = 41
    Width = 524
    Height = 329
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 177
      Width = 524
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 364
    end
    object pnlLogDetail: TPanel
      Left = 0
      Top = 180
      Width = 524
      Height = 149
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = pnlLogDetailResize
      object mmLogText: TMemo
        Left = 0
        Top = 25
        Width = 524
        Height = 124
        Align = alClient
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 0
      end
      object pnlDetailTitle: TPanel
        Left = 0
        Top = 0
        Width = 524
        Height = 25
        Align = alTop
        BevelKind = bkFlat
        BevelOuter = bvNone
        TabOrder = 1
        object sbSize: TSpeedButton
          AlignWithMargins = True
          Left = 499
          Top = 1
          Width = 18
          Height = 19
          Margins.Top = 1
          Margins.Bottom = 1
          Align = alRight
          Caption = '0'
          Flat = True
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Marlett'
          Font.Style = []
          ParentFont = False
          OnClick = sbSizeClick
          ExplicitTop = 2
        end
        object sbClear: TSpeedButton
          AlignWithMargins = True
          Left = 475
          Top = 1
          Width = 18
          Height = 19
          Margins.Top = 1
          Margins.Bottom = 1
          Align = alRight
          Caption = 'r'
          Flat = True
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Marlett'
          Font.Style = []
          ParentFont = False
          OnClick = sbClearClick
          ExplicitTop = 0
        end
        object chkAutoScroll: TCheckBox
          Left = 4
          Top = 2
          Width = 97
          Height = 17
          Caption = #33258#21160#28378#21160#26085#24535
          TabOrder = 0
        end
      end
    end
    object vstLogs: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 524
      Height = 177
      Align = alClient
      Colors.UnfocusedColor = clMedGray
      Header.AutoSizeIndex = 5
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Height = 17
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 1
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnBeforeItemErase = vstLogsBeforeItemErase
      OnDrawText = vstLogsDrawText
      OnFocusChanged = vstLogsFocusChanged
      OnGetText = vstLogsGetText
      ExplicitLeft = 3
      ExplicitTop = 1
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
          Position = 0
          WideText = #24207#21495
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
          Position = 1
          Width = 65
          WideText = #32423#21035
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
          Position = 2
          Width = 120
          WideText = #26102#38388
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
          Position = 3
          Width = 120
          WideText = #20027#26426#21517#31216
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
          Position = 4
          Width = 85
          WideText = #32447#31243
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
          Position = 5
          Width = 80
          WideText = #28040#24687
        end>
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 370
    Width = 727
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 47
      Top = 0
      Width = 680
      Height = 35
      Align = alRight
      Caption = 
        'QLog'#26085#24535#36828#31243#36319#36394#22120#65292#22522#20110'QDAC.QLog'#35774#35745#65292#20860#23481'syslog'#21327#35758#65292#20351#29992'UDP 514'#31471#21475#12290'QDAC '#24320#21457#32452'@swish'#20316 +
        #21697#65292#23448#26041'QQ'#32676':250530692'
      Layout = tlCenter
      ExplicitHeight = 13
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 727
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 721
      Height = 35
      Align = alClient
      Caption = 'QLogServer '#25903#25345'QLog'#21450#26222#36890#30340'syslog'#21327#35758#65292#27880#24847#26085#24535#30446#21069#29256#26412#20840#37096#20445#23384#22312#20869#23384#65292#19981#35201#29992#20110#26080#26080#38480#26399#36319#36394#12290
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 547
      ExplicitHeight = 13
    end
  end
  object usLogServer: TIdUDPServer
    Bindings = <>
    DefaultPort = 514
    OnUDPRead = usLogServerUDPRead
    Left = 640
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.log'
    Filter = #26085#24535#25991#20214'|*.log'
    Left = 152
    Top = 48
  end
end
