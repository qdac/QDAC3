object frmMemStatics: TfrmMemStatics
  Left = 0
  Top = 0
  Caption = #24212#29992#20869#23384#20351#29992#24773#20917#32479#35745#25253#21578
  ClientHeight = 422
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 12
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblSortBy: TLabel
      Left = 14
      Top = 16
      Width = 48
      Height = 12
      Caption = #25490#24207#20381#25454
    end
    object btnStart: TButton
      Left = 219
      Top = 10
      Width = 131
      Height = 25
      Caption = #32479#35745#20869#23384#20998#37197#29366#24577
      TabOrder = 0
      OnClick = btnStartClick
    end
    object cbxChartValueType: TComboBox
      Left = 68
      Top = 12
      Width = 145
      Height = 20
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 1
      Text = #20998#37197#27425#25968
      OnChange = cbxChartValueTypeChange
      Items.Strings = (
        #20998#37197#27425#25968
        #37322#25918#27425#25968
        #20998#37197#22359#25968
        #26368#22823#20998#37197#22359#25968
        #20998#37197#23610#23544)
    end
    object chkIgnoreZeros: TCheckBox
      Left = 368
      Top = 16
      Width = 161
      Height = 17
      Caption = #32472#22270#26102#24573#30053#20540#20026'0'#30340#39033#30446
      TabOrder = 2
      OnClick = chkIgnoreZerosClick
    end
  end
  object tsStatics: TPageControl
    Left = 0
    Top = 41
    Width = 680
    Height = 381
    ActivePage = tsSummary
    Align = alClient
    TabOrder = 1
    object tsSummary: TTabSheet
      Caption = #25253#21578
      object mmSummary: TMemo
        Left = 0
        Top = 0
        Width = 672
        Height = 353
        Align = alClient
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 0
      end
    end
    object tsChart: TTabSheet
      Caption = #22270#34920
      ImageIndex = 1
      OnResize = tsChartResize
      object imgChart: TImage
        Left = 0
        Top = 0
        Width = 672
        Height = 353
        Align = alClient
        Stretch = True
        ExplicitLeft = 200
        ExplicitTop = -32
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
  end
end
