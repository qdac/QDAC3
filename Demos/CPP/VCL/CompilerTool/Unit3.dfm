object Frame_Compiler: TFrame_Compiler
  Left = 0
  Top = 0
  Width = 521
  Height = 431
  TabOrder = 0
  DesignSize = (
    521
    431)
  object L: TLabel
    Left = 6
    Top = 10
    Width = 3
    Height = 13
  end
  object PC: TPageControl
    Left = 0
    Top = 36
    Width = 521
    Height = 395
    ActivePage = TabInstall
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabInstall: TTabSheet
      Caption = #23433#35013#20449#24687
      ExplicitTop = 26
      ExplicitHeight = 365
      DesignSize = (
        513
        367)
      object RootDir: TLabeledEdit
        Left = 4
        Top = 26
        Width = 503
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 48
        EditLabel.Height = 13
        EditLabel.Caption = #20027#30446#24405#65306
        LabelSpacing = 6
        ReadOnly = True
        TabOrder = 0
      end
      object App: TLabeledEdit
        Left = 4
        Top = 76
        Width = 503
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 48
        EditLabel.Height = 13
        EditLabel.Caption = #20027#31243#24207#65306
        LabelSpacing = 6
        ReadOnly = True
        TabOrder = 1
      end
      object BrowsingPath: TLabeledEdit
        Left = 4
        Top = 126
        Width = 503
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 60
        EditLabel.Height = 13
        EditLabel.Caption = #27983#35272#36335#24452#65306
        LabelSpacing = 6
        ReadOnly = True
        TabOrder = 2
      end
    end
    object TabEnv: TTabSheet
      Caption = #29615#22659#21464#37327
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 652
      ExplicitHeight = 353
      object Env: TValueListEditor
        Left = 0
        Top = 0
        Width = 513
        Height = 365
        Align = alClient
        TabOrder = 0
        TitleCaptions.Strings = (
          #21464#37327#21517
          #20540)
        ExplicitLeft = 24
        ExplicitTop = 48
        ExplicitWidth = 306
        ExplicitHeight = 300
        ColWidths = (
          150
          357)
      end
    end
  end
end
