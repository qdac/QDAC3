object Form_Main: TForm_Main
  Left = 0
  Top = 0
  Caption = 'CompilerTool'
  ClientHeight = 389
  ClientWidth = 744
  Color = clBtnFace
  Constraints.MinHeight = 428
  Constraints.MinWidth = 760
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
  DesignSize = (
    744
    389)
  PixelsPerInch = 96
  TextHeight = 14
  object L_Language: TLabel
    Left = 252
    Top = 4
    Width = 63
    Height = 14
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'Language:'
    ParentBiDiMode = False
  end
  object L_Style: TLabel
    Left = 510
    Top = 4
    Width = 42
    Height = 14
    Alignment = taRightJustify
    Caption = 'Style:'
    Visible = False
  end
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 744
    Height = 389
    ActivePage = TabProject
    Align = alClient
    MultiLine = True
    TabOrder = 0
    OnChange = PCChange
    object TabReadme: TTabSheet
      Caption = 'Description'
      object P3: TPanel
        Left = 0
        Top = 319
        Width = 736
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        Caption = 'P3'
        ShowCaption = False
        TabOrder = 0
        object BtnRegType: TButton
          Left = 320
          Top = 8
          Width = 97
          Height = 25
          Caption = 'Sign ExtName'
          TabOrder = 0
          OnClick = BtnRegTypeClick
        end
      end
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
          
            'This program will not run on the bare metal, because it is throu' +
            'gh the command-line compiler Delphi, C ++ Builder '
          
            'files, so you must have the appropriate program on your computer' +
            ' (which can be over Copy from others, but must be '
          'complete, BDS project must be. NET to compile!).'
          ''
          'Supported file formats are as follows:'
          'Pas (Delphi unit file)'
          
            'Dpr / Dpk (Delphi project files, third-party control path to be ' +
            'set in an additional path inside!)'
          
            'DProj (BDS Delphi project files, third-party control path to be ' +
            'set in an additional path inside!)'
          'C (C ++ Builder unit files)'
          'Cpp (C ++ Builder unit files)'
          'Bpr / Bpk (C ++ Builder project files, not tested)'
          
            'CBProj (BDS C ++ Builder project files, third-party control path' +
            ' in the additional path where you want to set the '
          'output directory does not support the Chinese directory name!)'
          ''
          
            'Program at startup will read the registry information in the ins' +
            'tallation, and then reads the configuration file (if '
          'the conflict with the registry is ignored).'
          ''
          
            'Win64, Android, IOS, OS X does not support generated under the D' +
            'esign Library! ! !'
          ''
          'Android does not support generate APK!'
          ''
          
            'Multi-compiled version of the control method (dpk support only t' +
            'his method):'
          
            '1, modify dpk file name, removing the version information (such ' +
            'as the FastReport: frx21.dpk and dclfrx21.dpk, renamed '
          'frx.dpk and dclfrx.dpk);'
          
            '2, modify dpk file contents, removing version information (such ' +
            'as the FastReport: frx.dpk and dclfrx.dpk, frx.dpk all '
          
            'frx21 to frx, dclfrx.dpk all frx21 to frx, all dclfrx21 to dclfr' +
            'x );'
          
            '3, the dpk file on this tool, automatically add the right, selec' +
            't "automatic sorting" (will be adjusted automatically '
          
            'compile dependencies in order), check this tool "DPK add IDE ver' +
            'sion number" in the document compiled after The '
          
            'version number (eg: frx.dpk generate frx140.bpl in 2010, the gen' +
            'eration frx220.bpl at XE8).')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object TabProject: TTabSheet
      Caption = 'Project'
      ImageIndex = 1
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
          Left = 24
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Add'
          TabOrder = 0
          OnClick = BtnAddClick
        end
        object BtnDel: TButton
          Left = 126
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 1
          OnClick = BtnDelClick
        end
        object BtnOpen: TButton
          Left = 228
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Open'
          TabOrder = 2
          OnClick = BtnOpenClick
        end
        object BtnSave: TButton
          Left = 330
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Save'
          TabOrder = 3
          OnClick = BtnSaveClick
        end
        object BtnSaveAs: TButton
          Left = 432
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Save As'
          TabOrder = 4
          OnClick = BtnSaveAsClick
        end
        object ShowCmd: TCheckBox
          Left = 637
          Top = 2
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'ShowCommand'
          TabOrder = 5
        end
        object ShowAllLog: TCheckBox
          Left = 637
          Top = 20
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'CompleteLog'
          TabOrder = 6
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 736
        Height = 319
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel1'
        ShowCaption = False
        TabOrder = 1
        object Splitter1: TSplitter
          Left = 433
          Top = 0
          Height = 319
          Align = alRight
          ExplicitLeft = 456
          ExplicitTop = 144
          ExplicitHeight = 100
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 433
          Height = 319
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Panel2'
          ShowCaption = False
          TabOrder = 0
          object Panel4: TPanel
            Left = 0
            Top = 141
            Width = 433
            Height = 178
            Align = alBottom
            BevelOuter = bvNone
            Caption = 'Panel4'
            ShowCaption = False
            TabOrder = 0
            object ProjectSet: TPageControl
              Left = 0
              Top = 0
              Width = 433
              Height = 178
              ActivePage = TabOutPath
              Align = alClient
              TabOrder = 0
              object TabOutPath: TTabSheet
                Caption = 'Output Path'
                DesignSize = (
                  425
                  148)
                object OutFinalDir: TLabeledEdit
                  Left = 102
                  Top = 18
                  Width = 296
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 91
                  EditLabel.Height = 14
                  EditLabel.Caption = 'BPL/EXE Path:'
                  LabelPosition = lpLeft
                  ReadOnly = True
                  TabOrder = 0
                end
                object OutIncludeDir: TLabeledEdit
                  Left = 102
                  Top = 48
                  Width = 296
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 91
                  EditLabel.Height = 14
                  EditLabel.Caption = 'Include Path:'
                  LabelPosition = lpLeft
                  ReadOnly = True
                  TabOrder = 1
                end
                object OutLibDir: TLabeledEdit
                  Left = 102
                  Top = 78
                  Width = 296
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 63
                  EditLabel.Height = 14
                  EditLabel.Caption = 'Lib Path:'
                  LabelPosition = lpLeft
                  ReadOnly = True
                  TabOrder = 2
                end
                object Button3: TButton
                  Left = 400
                  Top = 78
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 3
                  OnClick = Button3Click
                end
                object Button2: TButton
                  Left = 400
                  Top = 48
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 4
                  OnClick = Button2Click
                end
                object Button1: TButton
                  Left = 400
                  Top = 18
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 5
                  OnClick = Button1Click
                end
                object DpkAddIDEVersion: TCheckBox
                  Left = 184
                  Top = 106
                  Width = 174
                  Height = 17
                  Caption = 'DPK add IDE Ver Number'
                  TabOrder = 6
                end
                object BuildRelease: TRadioButton
                  Left = 15
                  Top = 106
                  Width = 70
                  Height = 17
                  Caption = 'Release'
                  Checked = True
                  TabOrder = 7
                  TabStop = True
                end
                object BuildDebug: TRadioButton
                  Left = 15
                  Top = 128
                  Width = 70
                  Height = 17
                  Caption = 'Debug'
                  TabOrder = 8
                end
                object ClangWin32: TCheckBox
                  Left = 184
                  Top = 128
                  Width = 174
                  Height = 17
                  Caption = 'Clang C++ for Win32'
                  Checked = True
                  State = cbChecked
                  TabOrder = 9
                end
              end
              object TabProjectSet: TTabSheet
                Caption = 'Additional Paths and Settings'
                ImageIndex = 1
                DesignSize = (
                  425
                  148)
                object ExtraAlias: TLabeledEdit
                  Left = 102
                  Top = 3
                  Width = 320
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 42
                  EditLabel.Height = 14
                  EditLabel.Caption = 'Alias:'
                  LabelPosition = lpLeft
                  TabOrder = 0
                end
                object ExtraNameSpaceSearch: TLabeledEdit
                  Left = 102
                  Top = 28
                  Width = 320
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 70
                  EditLabel.Height = 14
                  EditLabel.Caption = 'NS Search:'
                  LabelPosition = lpLeft
                  TabOrder = 1
                end
                object ExtraIncludeDir: TLabeledEdit
                  Left = 102
                  Top = 54
                  Width = 296
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 91
                  EditLabel.Height = 14
                  EditLabel.Caption = 'Include Path:'
                  LabelPosition = lpLeft
                  ReadOnly = True
                  TabOrder = 2
                end
                object ExtraLibDir: TLabeledEdit
                  Left = 102
                  Top = 79
                  Width = 296
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 63
                  EditLabel.Height = 14
                  EditLabel.Caption = 'Lib Path:'
                  LabelPosition = lpLeft
                  ReadOnly = True
                  TabOrder = 3
                end
                object ExtraSearchDir: TLabeledEdit
                  Left = 102
                  Top = 105
                  Width = 296
                  Height = 22
                  Anchors = [akLeft, akTop, akRight]
                  EditLabel.Width = 84
                  EditLabel.Height = 14
                  EditLabel.Caption = 'Search Path:'
                  LabelPosition = lpLeft
                  ReadOnly = True
                  TabOrder = 4
                end
                object Button6: TButton
                  Left = 400
                  Top = 105
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 5
                  OnClick = Button6Click
                end
                object Button4: TButton
                  Left = 400
                  Top = 79
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 6
                  OnClick = Button4Click
                end
                object Button5: TButton
                  Left = 400
                  Top = 54
                  Width = 22
                  Height = 22
                  Anchors = [akTop, akRight]
                  Caption = '...'
                  TabOrder = 7
                  OnClick = Button5Click
                end
                object ExtraBuildAllUnits: TCheckBox
                  Left = 240
                  Top = 131
                  Width = 121
                  Height = 17
                  Caption = 'Build all units'
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
                  TabOrder = 8
                end
                object ExtraNameSpaces: TCheckBox
                  Left = 18
                  Top = 131
                  Width = 199
                  Height = 17
                  Caption = 'Resulting in a namespace Hpp'
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
                  TabOrder = 9
                end
              end
            end
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 433
            Height = 141
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Panel5'
            ShowCaption = False
            TabOrder = 1
            object Panel6: TPanel
              Left = 404
              Top = 0
              Width = 29
              Height = 141
              Align = alRight
              BevelOuter = bvNone
              Caption = 'Panel6'
              ShowCaption = False
              TabOrder = 0
              DesignSize = (
                29
                141)
              object BtnUp: TButton
                Left = 5
                Top = 81
                Width = 18
                Height = 25
                Anchors = [akLeft, akBottom]
                Caption = #8593
                TabOrder = 0
                OnClick = BtnUpClick
              end
              object BtnDown: TButton
                Left = 5
                Top = 35
                Width = 18
                Height = 25
                Caption = #8595
                TabOrder = 1
                OnClick = BtnDownClick
              end
            end
            object VDT: TVirtualDrawTree
              Left = 0
              Top = 0
              Width = 404
              Height = 141
              Align = alClient
              Header.AutoSizeIndex = -1
              Header.Font.Charset = GB2312_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -12
              Header.Font.Name = #23435#20307
              Header.Font.Style = []
              Header.Height = 32
              Header.MainColumn = 1
              Header.Options = [hoColumnResize, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible]
              Indent = 0
              LineStyle = lsSolid
              TabOrder = 1
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
              OnColumnClick = VDTColumnClick
              OnDrawNode = VDTDrawNode
              OnInitNode = VDTInitNode
              OnMouseMove = VDTMouseMove
              OnMouseUp = VDTMouseUp
              Columns = <
                item
                  Alignment = taCenter
                  MaxWidth = 40
                  MinWidth = 40
                  Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible, coFixed, coAllowFocus]
                  Position = 0
                  Width = 40
                  WideText = 'No'
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  MaxWidth = 40
                  MinWidth = 40
                  Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                  Position = 1
                  Width = 40
                  WideText = 'Use'
                end
                item
                  CaptionAlignment = taCenter
                  MinWidth = 150
                  Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                  Position = 2
                  Width = 150
                  WideText = 'FileName'
                end>
            end
          end
        end
        object Panel3: TPanel
          Left = 436
          Top = 0
          Width = 300
          Height = 319
          Align = alRight
          BevelOuter = bvNone
          Caption = 'Panel3'
          Constraints.MinWidth = 240
          ParentBackground = False
          ShowCaption = False
          TabOrder = 1
          object CompilerPC: TPageControl
            Left = 0
            Top = 0
            Width = 300
            Height = 319
            ActivePage = TabNew
            Align = alClient
            TabOrder = 0
            object TabNew: TTabSheet
              Caption = 'Custom'
              DesignSize = (
                292
                289)
              object L: TLabel
                Left = 6
                Top = 56
                Width = 7
                Height = 14
                ParentShowHint = False
                ShowHint = False
              end
              object CustomRootDir: TLabeledEdit
                Left = 6
                Top = 26
                Width = 261
                Height = 22
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 70
                EditLabel.Height = 14
                EditLabel.Caption = 'Root Path:'
                ReadOnly = True
                TabOrder = 0
              end
              object Button7: TButton
                Left = 269
                Top = 26
                Width = 22
                Height = 22
                Anchors = [akTop, akRight]
                Caption = '...'
                TabOrder = 1
                OnClick = Button7Click
              end
              object AddIDE: TButton
                Left = 109
                Top = 98
                Width = 75
                Height = 25
                Caption = 'Add IDE'
                TabOrder = 2
                OnClick = AddIDEClick
              end
            end
          end
        end
      end
    end
    object TabLog: TTabSheet
      Caption = 'CompilerLog'
      ImageIndex = 2
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
        DesignSize = (
          736
          40)
        object BtnSaveLog: TButton
          Left = 179
          Top = 8
          Width = 75
          Height = 25
          Caption = 'SaveLog'
          TabOrder = 0
          OnClick = BtnSaveLogClick
        end
        object BtnClearLog: TButton
          Left = 483
          Top = 8
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'ClearLog'
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
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object BtnCompiler: TButton
    Left = 553
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 1
    OnClick = BtnCompilerClick
  end
  object ChangeLangID: TComboBox
    Left = 322
    Top = 0
    Width = 150
    Height = 22
    Style = csDropDownList
    TabOrder = 2
    OnChange = ChangeLangIDChange
  end
  object ChangeStyle: TComboBox
    Left = 558
    Top = 0
    Width = 150
    Height = 22
    Style = csDropDownList
    Sorted = True
    TabOrder = 3
    Visible = False
    OnChange = ChangeStyleChange
  end
  object OD: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofForceShowHidden]
    Left = 208
    Top = 64
  end
  object SD: TSaveDialog
    Left = 248
    Top = 64
  end
  object PM: TPopupMenu
    Left = 168
    Top = 64
    object PM_All: TMenuItem
      Tag = 1
      Caption = 'All Ranks'
    end
    object PM_Row: TMenuItem
      Tag = 2
      Caption = 'Current Row'
    end
    object PM_Column: TMenuItem
      Tag = 3
      Caption = 'Current Column'
    end
    object PM_Ver: TMenuItem
      Tag = 4
      Caption = 'IDE Version'
    end
    object PM_Platform: TMenuItem
      Tag = 5
      Caption = 'Platform'
    end
    object PM_Sort: TMenuItem
      Tag = 6
      Caption = 'Sort by dependency(Only Delphi Files)'
      OnClick = PM_SortClick
    end
    object PM_PM_NoCompilerByRed: TMenuItem
      Tag = 7
      Caption = 'No compilation error file'
      OnClick = PM_PM_NoCompilerByRedClick
    end
  end
end
