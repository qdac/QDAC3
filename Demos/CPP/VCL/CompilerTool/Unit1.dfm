object Form_Main: TForm_Main
  Left = 0
  Top = 0
  Caption = #21629#20196#34892#32534#35793#24037#20855
  ClientHeight = 389
  ClientWidth = 744
  Color = clBtnFace
  Constraints.MinHeight = 428
  Constraints.MinWidth = 760
  DragMode = dmAutomatic
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object P_Work: TPanel
    Left = 0
    Top = 0
    Width = 744
    Height = 389
    Align = alClient
    BevelOuter = bvNone
    Caption = 'P_Work'
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      744
      389)
    object PC: TPageControl
      Left = 0
      Top = 0
      Width = 744
      Height = 389
      ActivePage = TabProject
      Align = alClient
      TabOrder = 0
      object TabReadme: TTabSheet
        Caption = #31243#24207#21151#33021#35828#26126
        OnShow = TabReadmeShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object M: TMemo
          Left = 0
          Top = 0
          Width = 736
          Height = 319
          Align = alClient
          Font.Charset = GB2312_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = #23435#20307
          Font.Style = []
          Lines.Strings = (
            
              #26412#31243#24207#19981#33021#22312#35064#26426#19978#36816#34892#65292#22240#20026#26159#36890#36807#21629#20196#34892#32534#35793'Delphi'#12289'C++Builder'#25991#20214#30340#65292#22240#27492#30005#33041#19978#24517#39035#26377#30456#24212#30340#31243#24207#65288#21487#20197#26159#20174#21035#20154#37027 +
              #37324'Copy'#36807
            #26469#30340#65292#20294#24517#39035#23436#25972#65292'BDS'#24037#31243#24517#39035#35201'.NET'#25165#33021#32534#35793#65281#65289#12290
            ''
            #25903#25345#25991#20214#26684#24335#22914#19979#65306
            'Pas'#9#9'(Delphi'#21333#20803#25991#20214')'
            'Dpr/Dpk'#9#9'(Delphi'#24037#31243#25991#20214#65292#31532#19977#26041#25511#20214#36335#24452#35201#22312#38468#21152#36335#24452#37324#35774#32622#65281')'
            'DProj'#9#9'(BDS Delphi'#24037#31243#25991#20214#65292#31532#19977#26041#25511#20214#36335#24452#35201#22312#38468#21152#36335#24452#37324#35774#32622#65281')'
            'Cpp'#9#9'(C++Builder'#21333#20803#25991#20214')'
            'Bpr/Bpk'#9#9'(C++Builder'#24037#31243#25991#20214#65292#26410#27979#35797')'
            'CBProj'#9#9'(BDS C++Builder'#24037#31243#25991#20214#65292#31532#19977#26041#25511#20214#36335#24452#35201#22312#38468#21152#36335#24452#37324#35774#32622#65292#36755#20986#30446#24405#19981#25903#25345#20013#25991#30446#24405#21517#65281')'
            ''
            #31243#24207#22312#21551#21160#26102#20250#35835#21462#27880#20876#34920#37324#30340#23433#35013#20449#24687#65292#20877#35835#21462#37197#32622#25991#20214#20013#30340#20449#24687#65288#22914#26524#19982#27880#20876#34920#30340#20914#31361#65292#21017#24573#30053#65289#12290
            ''
            #31243#24207#19981#20250#22312'64'#20301#12289'Android'#12289'IOS'#12289'OSX'#19979#32534#35793#35774#35745#24211#65281#65281#65281
            ''
            'Android'#26242#26102#19981#25903#25345#29983#25104'APK'#65281
            ''
            #22810#29256#26412#25511#20214#32534#35793#26041#27861#65288' '#20165'dpk'#25903#25345#36825#31181#26041#27861#65289#65306
            
              '1'#12289#20462#25913'dpk'#25991#20214#21517#65292#21435#38500#29256#26412#20449#24687#65288#22914'FastReport'#30340#65306'frx21.dpk'#21644'dclfrx21.dpk'#65292#25913#21517#20026'frx.dpk' +
              #21644'dclfrx.dpk'#65289#65307
            
              '2'#12289#20462#25913'dpk'#25991#20214#20869#23481#65292#21435#38500#29256#26412#20449#24687#65288#22914'FastReport'#30340#65306'frx.dpk'#21644'dclfrx.dpk'#65292'frx.dpk'#20013#25152#26377#30340'fr' +
              'x21'#25913#20026'frx'#65292'dclfrx.dpk'#20013#25152#26377#30340
            'frx21'#25913#20026'frx'#65292#25152#26377'dclfrx21'#25913#20026'dclfrx'#65289#65307
            
              '3'#12289#23558'dpk'#25991#20214#21152#20837#26412#24037#20855#65292#22312#25991#20214#20013#21491#38190#65292#36873#25321#8220#33258#21160#25490#24207#8221#65288#23558#25353#20381#36182#20851#31995#33258#21160#35843#25972#32534#35793#39034#24207#65289#65292#21246#36873#26412#24037#20855#20013#30340#8220'DPK'#28155#21152'IDE'#29256#26412 +
              #21495
            #8221#65292#32534#35793#21518#23558#33258#21160#21152#19978#29256#26412#21495#65288#22914#65306'frx.dpk'#22312'2010'#19979#29983#25104'frx140.bpl'#65292#22312'XE8'#19979#29983#25104'frx220.bpl'#65289#12290)
          ParentFont = False
          TabOrder = 0
        end
        object P3: TPanel
          Left = 0
          Top = 319
          Width = 736
          Height = 40
          Align = alBottom
          BevelOuter = bvNone
          ShowCaption = False
          TabOrder = 1
          object BtnRegType: TButton
            Left = 317
            Top = 7
            Width = 103
            Height = 25
            Caption = #27880#20876#25193#23637#21517
            TabOrder = 0
            OnClick = BtnRegTypeClick
          end
        end
      end
      object TabProject: TTabSheet
        Caption = #32534#35793#39033#30446
        ImageIndex = 1
        OnShow = TabProjectShow
        object P1: TPanel
          Left = 0
          Top = 319
          Width = 736
          Height = 40
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'P1'
          ShowCaption = False
          TabOrder = 0
          DesignSize = (
            736
            40)
          object BtnAdd: TButton
            Left = 20
            Top = 8
            Width = 70
            Height = 25
            Caption = #28155#21152
            TabOrder = 0
            OnClick = BtnAddClick
          end
          object BtnDelete: TButton
            Left = 120
            Top = 8
            Width = 70
            Height = 25
            Caption = #21024#38500
            TabOrder = 1
            OnClick = BtnDeleteClick
          end
          object BtnSave: TButton
            Left = 322
            Top = 8
            Width = 70
            Height = 25
            Caption = #20445#23384
            TabOrder = 2
            OnClick = BtnSaveClick
          end
          object BtnSaveAs: TButton
            Left = 423
            Top = 8
            Width = 70
            Height = 25
            Caption = #21478#23384#20026
            TabOrder = 3
            OnClick = BtnSaveAsClick
          end
          object ShowCmd: TCheckBox
            Left = 632
            Top = 3
            Width = 111
            Height = 17
            Anchors = [akTop, akRight]
            Caption = #26174#31034#21629#20196#34892
            TabOrder = 4
          end
          object ShowAllLog: TCheckBox
            Left = 632
            Top = 21
            Width = 111
            Height = 17
            Anchors = [akTop, akRight]
            Caption = #23436#25972#26085#24535
            TabOrder = 5
          end
          object BtnOpen: TButton
            Left = 221
            Top = 8
            Width = 70
            Height = 25
            Caption = #25171#24320
            TabOrder = 6
            OnClick = BtnOpenClick
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 736
          Height = 319
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Panel2'
          ShowCaption = False
          TabOrder = 1
          object Splitter1: TSplitter
            Left = 415
            Top = 0
            Height = 319
            Align = alRight
            ExplicitLeft = 534
            ExplicitHeight = 419
          end
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 415
            Height = 319
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Panel3'
            ShowCaption = False
            TabOrder = 0
            object Panel5: TPanel
              Left = 0
              Top = 141
              Width = 415
              Height = 178
              Align = alBottom
              BevelOuter = bvNone
              Caption = 'Panel5'
              ShowCaption = False
              TabOrder = 0
              object ProjectSet: TPageControl
                Left = 0
                Top = 0
                Width = 415
                Height = 178
                ActivePage = TabOutPath
                Align = alClient
                TabOrder = 0
                object TabOutPath: TTabSheet
                  Caption = #36755#20986#36335#24452
                  DesignSize = (
                    407
                    148)
                  object L_Dir: TLabel
                    Left = 6
                    Top = 126
                    Width = 396
                    Height = 12
                    Caption = #36335#24452#23558#23384#25918'Win32'#25991#20214#65292#20854#20182#24179#21488#23558#25353#24179#21488#29983#25104#23376#30446#24405#65306'Win64/Android'#31561#65281
                    Color = clBtnFace
                    Font.Charset = GB2312_CHARSET
                    Font.Color = clRed
                    Font.Height = -12
                    Font.Name = #23435#20307
                    Font.Style = []
                    ParentColor = False
                    ParentFont = False
                  end
                  object OutFinalDir: TLabeledEdit
                    Left = 102
                    Top = 6
                    Width = 280
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 91
                    EditLabel.Height = 14
                    EditLabel.Caption = 'BPL/EXE'#36335#24452#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    ReadOnly = True
                    TabOrder = 0
                  end
                  object Button1: TButton
                    Left = 382
                    Top = 6
                    Width = 22
                    Height = 22
                    Anchors = [akTop, akRight]
                    Caption = '...'
                    TabOrder = 1
                    OnClick = Button1Click
                  end
                  object OutIncludeDir: TLabeledEdit
                    Left = 102
                    Top = 36
                    Width = 280
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 91
                    EditLabel.Height = 14
                    EditLabel.Caption = 'Include'#36335#24452#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    ReadOnly = True
                    TabOrder = 2
                  end
                  object Button2: TButton
                    Left = 382
                    Top = 36
                    Width = 22
                    Height = 22
                    Anchors = [akTop, akRight]
                    Caption = '...'
                    TabOrder = 3
                    OnClick = Button2Click
                  end
                  object OutLibDir: TLabeledEdit
                    Left = 102
                    Top = 66
                    Width = 280
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 63
                    EditLabel.Height = 14
                    EditLabel.Caption = 'Lib'#36335#24452#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    ReadOnly = True
                    TabOrder = 4
                  end
                  object Button3: TButton
                    Left = 382
                    Top = 66
                    Width = 22
                    Height = 22
                    Anchors = [akTop, akRight]
                    Caption = '...'
                    TabOrder = 5
                    OnClick = Button3Click
                  end
                  object BuildRelease: TRadioButton
                    Left = 44
                    Top = 101
                    Width = 89
                    Height = 17
                    Caption = 'Release'#29256
                    Checked = True
                    TabOrder = 6
                    TabStop = True
                  end
                  object BuildDebug: TRadioButton
                    Left = 151
                    Top = 101
                    Width = 89
                    Height = 17
                    Caption = 'Debug'#29256
                    TabOrder = 7
                  end
                  object DpkAddIDEVersion: TCheckBox
                    Left = 250
                    Top = 101
                    Width = 160
                    Height = 17
                    Caption = 'DPK'#28155#21152'IDE'#29256#26412#21495
                    TabOrder = 8
                  end
                end
                object TabProjectSet: TTabSheet
                  Caption = #39033#30446#38468#21152#36335#24452#21644#35774#32622
                  ImageIndex = 1
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  DesignSize = (
                    407
                    148)
                  object ExtraAlias: TLabeledEdit
                    Left = 102
                    Top = 3
                    Width = 302
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 42
                    EditLabel.Height = 14
                    EditLabel.Caption = #21035#21517#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    TabOrder = 0
                  end
                  object ExtraIncludeDir: TLabeledEdit
                    Left = 102
                    Top = 54
                    Width = 280
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 91
                    EditLabel.Height = 14
                    EditLabel.Caption = 'Include'#36335#24452#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    ReadOnly = True
                    TabOrder = 1
                  end
                  object ExtraLibDir: TLabeledEdit
                    Left = 102
                    Top = 79
                    Width = 280
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 63
                    EditLabel.Height = 14
                    EditLabel.Caption = 'Lib'#36335#24452#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    ReadOnly = True
                    TabOrder = 2
                  end
                  object Button4: TButton
                    Left = 382
                    Top = 79
                    Width = 22
                    Height = 22
                    Anchors = [akTop, akRight]
                    Caption = '...'
                    TabOrder = 3
                    OnClick = Button4Click
                  end
                  object Button5: TButton
                    Left = 382
                    Top = 54
                    Width = 22
                    Height = 22
                    Anchors = [akTop, akRight]
                    Caption = '...'
                    TabOrder = 4
                    OnClick = Button5Click
                  end
                  object ExtraSearchDir: TLabeledEdit
                    Left = 102
                    Top = 105
                    Width = 280
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 84
                    EditLabel.Height = 14
                    EditLabel.Caption = 'Search'#36335#24452#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    ReadOnly = True
                    TabOrder = 5
                  end
                  object Button6: TButton
                    Left = 382
                    Top = 105
                    Width = 22
                    Height = 22
                    Anchors = [akTop, akRight]
                    Caption = '...'
                    TabOrder = 6
                    OnClick = Button6Click
                  end
                  object ExtraNameSpaces: TCheckBox
                    Left = 18
                    Top = 131
                    Width = 177
                    Height = 17
                    Caption = #29983#25104#30340'Hpp'#20013#21253#21547#21629#21517#31354#38388
                    Checked = True
                    Color = clBtnFace
                    Font.Charset = GB2312_CHARSET
                    Font.Color = clRed
                    Font.Height = -12
                    Font.Name = #23435#20307
                    Font.Style = []
                    ParentColor = False
                    ParentFont = False
                    State = cbChecked
                    TabOrder = 7
                  end
                  object ExtraNameSpaceSearch: TLabeledEdit
                    Left = 102
                    Top = 28
                    Width = 302
                    Height = 22
                    Anchors = [akLeft, akTop, akRight]
                    EditLabel.Width = 70
                    EditLabel.Height = 14
                    EditLabel.Caption = #21629#21517#31354#38388#65306
                    LabelPosition = lpLeft
                    LabelSpacing = 0
                    TabOrder = 8
                  end
                  object ExtraBuildAllUnits: TCheckBox
                    Left = 240
                    Top = 131
                    Width = 97
                    Height = 17
                    Caption = #32534#35793#20840#37096#21333#20803
                    Color = clBtnFace
                    Font.Charset = GB2312_CHARSET
                    Font.Color = clRed
                    Font.Height = -12
                    Font.Name = #23435#20307
                    Font.Style = []
                    ParentColor = False
                    ParentFont = False
                    TabOrder = 9
                  end
                end
              end
            end
            object Panel6: TPanel
              Left = 0
              Top = 0
              Width = 415
              Height = 141
              Align = alClient
              BevelOuter = bvNone
              Caption = 'Panel6'
              ShowCaption = False
              TabOrder = 1
              object Panel7: TPanel
                Left = 386
                Top = 0
                Width = 29
                Height = 141
                Align = alRight
                BevelOuter = bvNone
                Caption = 'Panel7'
                ShowCaption = False
                TabOrder = 0
                DesignSize = (
                  29
                  141)
                object BtnDown: TButton
                  Left = 5
                  Top = 47
                  Width = 18
                  Height = 25
                  Caption = #8595
                  TabOrder = 0
                  OnClick = BtnDownClick
                end
                object BtnUp: TButton
                  Left = 5
                  Top = 69
                  Width = 18
                  Height = 25
                  Anchors = [akLeft, akBottom]
                  Caption = #8593
                  TabOrder = 1
                  OnClick = BtnUpClick
                end
              end
            end
          end
          object Panel4: TPanel
            Left = 418
            Top = 0
            Width = 318
            Height = 319
            Align = alRight
            BevelOuter = bvNone
            Caption = 'Panel4'
            Constraints.MinWidth = 240
            ShowCaption = False
            TabOrder = 1
            object CompilerPC: TPageControl
              Left = 0
              Top = 0
              Width = 318
              Height = 319
              ActivePage = TabNew
              Align = alClient
              TabOrder = 0
              object TabNew: TTabSheet
                Caption = #33258#23450#20041
                DesignSize = (
                  310
                  289)
                object L: TLabel
                  Left = 6
                  Top = 54
                  Width = 7
                  Height = 14
                end
                object CustomRootDir: TLabeledEdit
                  Left = 6
                  Top = 26
                  Width = 278
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 56
                  EditLabel.Height = 14
                  EditLabel.Caption = #20027#30446#24405#65306
                  TabOrder = 0
                end
                object Button7: TButton
                  Left = 285
                  Top = 26
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 1
                  OnClick = Button7Click
                end
                object AddIDE: TButton
                  Left = 117
                  Top = 98
                  Width = 75
                  Height = 25
                  Caption = #30830#23450#28155#21152
                  TabOrder = 2
                  OnClick = AddIDEClick
                end
              end
            end
          end
        end
      end
      object TabLog: TTabSheet
        Caption = #32534#35793#26085#24535
        ImageIndex = 2
        OnShow = TabLogShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object P2: TPanel
          Left = 0
          Top = 319
          Width = 736
          Height = 40
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'P2'
          ShowCaption = False
          TabOrder = 0
          object BtnSaveLog: TButton
            Left = 210
            Top = 8
            Width = 60
            Height = 25
            Caption = #20445#23384
            TabOrder = 0
            OnClick = BtnSaveLogClick
          end
          object BtnClearLog: TButton
            Left = 466
            Top = 8
            Width = 60
            Height = 25
            Caption = #28165#31354
            TabOrder = 1
            OnClick = BtnClearLogClick
          end
        end
        object Log: TMemo
          Left = 0
          Top = 0
          Width = 736
          Height = 319
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
        end
      end
    end
    object BtnCompiler: TButton
      Left = 557
      Top = 353
      Width = 70
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #32534#35793
      TabOrder = 1
      OnClick = BtnCompilerClick
    end
    object ChangeLangID: TComboBox
      Left = 258
      Top = 0
      Width = 132
      Height = 22
      Style = csDropDownList
      TabOrder = 2
      OnChange = ChangeLangIDChange
    end
  end
  object OD: TOpenDialog
    Filter = 
      #25152#26377#21487#29992#25991#20214'|*.cpp;*.pas;*.bpr;*.dpr;*.bpk;*.dpk;*.cbproj;*.dproj|Delp' +
      'hi Unit|*.Pas|Delphi Project|*.dpr;*.dpk;*.dproj|C++ Builder Uni' +
      't|*.cpp|C++ Builder Project|*.bpr;*.bpk;*.cbproj'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = #35831#36873#25321#35201#25171#24320#30340#25991#20214
    Left = 68
    Top = 242
  end
  object SD: TSaveDialog
    Left = 100
    Top = 242
  end
  object PM: TPopupMenu
    Left = 136
    Top = 242
    object PM_All: TMenuItem
      Caption = #25152#26377#34892#21015
      object PM_SelectAll: TMenuItem
        Caption = #20840#36873
        OnClick = PM_SelectAllClick
      end
      object PM_ClearAll: TMenuItem
        Caption = #20840#28165#38500
        OnClick = PM_ClearAllClick
      end
      object PM_DeSelectAll: TMenuItem
        Caption = #21453#36873
        OnClick = PM_DeSelectAllClick
      end
    end
    object PM_Row: TMenuItem
      Caption = #24403#21069#34892
      object PM_SelectRow: TMenuItem
        Caption = #20840#36873
        OnClick = PM_SelectRowClick
      end
      object PM_ClearRow: TMenuItem
        Caption = #20840#28165#38500
        OnClick = PM_ClearRowClick
      end
      object PM_DeSelectRow: TMenuItem
        Caption = #21453#36873
        OnClick = PM_DeSelectRowClick
      end
    end
    object PM_Column: TMenuItem
      Caption = #24403#21069#21015
      object PM_SelectColumn: TMenuItem
        Caption = #20840#36873
        OnClick = PM_SelectColumnClick
      end
      object PM_ClearColumn: TMenuItem
        Caption = #20840#28165#38500
        OnClick = PM_ClearColumnClick
      end
      object PM_DeSelectColumn: TMenuItem
        Caption = #21453#36873
        OnClick = PM_DeSelectColumnClick
      end
    end
    object PM_CompilerSort: TMenuItem
      Caption = #33258#21160#25490#24207'('#20165'Pas/Dpk)'
      OnClick = PM_CompilerSortClick
    end
    object PM_NoCompilerByRed: TMenuItem
      Caption = #19981#20877#32534#35793#25253#38169#30340#25991#20214
      OnClick = PM_NoCompilerByRedClick
    end
  end
end
