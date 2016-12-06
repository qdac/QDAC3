object Frame_Compiler: TFrame_Compiler
  Left = 0
  Top = 0
  Width = 500
  Height = 440
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = #23435#20307
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    500
    440)
  object L: TLabel
    Left = 6
    Top = 10
    Width = 7
    Height = 14
  end
  object PC: TPageControl
    Left = 0
    Top = 36
    Width = 500
    Height = 404
    ActivePage = TabInstall
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabInstall: TTabSheet
      Caption = 'Install Info'
      DesignSize = (
        492
        374)
      object RootDir: TLabeledEdit
        Left = 4
        Top = 26
        Width = 482
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 56
        EditLabel.Height = 14
        EditLabel.Caption = 'RootDir:'
        LabelSpacing = 6
        ReadOnly = True
        TabOrder = 0
      end
      object App: TLabeledEdit
        Left = 4
        Top = 76
        Width = 482
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 28
        EditLabel.Height = 14
        EditLabel.Caption = 'App:'
        LabelSpacing = 6
        ReadOnly = True
        TabOrder = 1
      end
      object BrowsingPath: TLabeledEdit
        Left = 4
        Top = 126
        Width = 482
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 91
        EditLabel.Height = 14
        EditLabel.Caption = 'BrowsingPath:'
        LabelSpacing = 6
        ReadOnly = True
        TabOrder = 2
      end
    end
    object TabEnv: TTabSheet
      Caption = 'Environment'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Env: TValueListEditor
        Left = 0
        Top = 0
        Width = 492
        Height = 374
        Align = alClient
        TabOrder = 0
        TitleCaptions.Strings = (
          'EnvName'
          'EnvValue')
        ColWidths = (
          150
          336)
      end
    end
  end
end
