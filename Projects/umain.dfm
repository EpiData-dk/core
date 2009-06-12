object MainForm: TMainForm
  Left = 474
  Top = 205
  Width = 881
  Height = 745
  Caption = 'Core Test Project'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 873
    Height = 177
    Align = alTop
    TabOrder = 0
    DesignSize = (
      873
      177)
    object SpeedButton1: TSpeedButton
      Left = 320
      Top = 32
      Width = 23
      Height = 22
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
        333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
        0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
        07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
        07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
        0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
        33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
        B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
        3BB33773333773333773B333333B3333333B7333333733333337}
      NumGlyphs = 2
      OnClick = SpeedButton1Click
    end
    object Label3: TLabel
      Left = 16
      Top = 136
      Width = 55
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Processing:'
    end
    object pgLabel: TLabel
      Left = 96
      Top = 136
      Width = 66
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'progressLabel'
    end
    object SpeedButton2: TSpeedButton
      Left = 704
      Top = 32
      Width = 23
      Height = 22
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      OnClick = SpeedButton2Click
    end
    object Label4: TLabel
      Left = 144
      Top = 104
      Width = 46
      Height = 13
      Caption = 'Log level:'
    end
    object edInputFile: TLabeledEdit
      Left = 24
      Top = 32
      Width = 289
      Height = 21
      EditLabel.Width = 43
      EditLabel.Height = 13
      EditLabel.Caption = 'Input file:'
      TabOrder = 0
    end
    object readBtn: TButton
      Left = 24
      Top = 64
      Width = 105
      Height = 25
      Caption = '&Read File'
      TabOrder = 1
      OnClick = readBtnClick
    end
    object pgBar: TProgressBar
      Left = 16
      Top = 152
      Width = 617
      Height = 16
      Anchors = [akLeft, akBottom]
      Smooth = True
      TabOrder = 2
    end
    object CheckBox1: TCheckBox
      Left = 144
      Top = 72
      Width = 169
      Height = 17
      Caption = 'Read CHK file'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object edOutputFile: TLabeledEdit
      Left = 432
      Top = 32
      Width = 265
      Height = 21
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.Caption = 'Output file:'
      TabOrder = 4
    end
    object saveBtn: TButton
      Left = 432
      Top = 64
      Width = 105
      Height = 25
      Caption = '&Save File'
      Enabled = False
      TabOrder = 5
      OnClick = saveBtnClick
    end
    object CheckBox2: TCheckBox
      Left = 560
      Top = 72
      Width = 169
      Height = 17
      Caption = 'Write CHK file'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object stataCombo: TComboBox
      Left = 584
      Top = 104
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 7
      Text = 'Stata 8/9'
      Visible = False
      Items.Strings = (
        'Stata 4/5'
        'Stata 6'
        'Stata 7'
        'Stata 8/9'
        'Stata 10')
    end
    object closeBtn: TButton
      Left = 24
      Top = 96
      Width = 105
      Height = 25
      Caption = '&Close'
      Enabled = False
      TabOrder = 8
      OnClick = closeBtnClick
    end
    object cbDebug: TComboBox
      Left = 200
      Top = 100
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 9
      Text = '2 - Normal'
      OnChange = cbDebugChange
      Items.Strings = (
        '0 - None'
        '1 - Errors/Warnings'
        '2 - Normal'
        '3 - Verbose'
        '4 - Very Verbose')
    end
    object filetypeCombo: TComboBox
      Left = 432
      Top = 104
      Width = 145
      Height = 21
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 10
      Text = 'EpiData'
      OnChange = filetypeComboChange
      Items.Strings = (
        'EpiData'
        'Stata'
        'DBase'
        'CSV'
        'Excel')
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 177
    Width = 873
    Height = 541
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'File Info:'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 865
        Height = 513
        Align = alClient
        Lines.Strings = (
          '')
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Documentation:'
      ImageIndex = 1
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 865
        Height = 513
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'ChkFiles'
      ImageIndex = 2
      object Splitter1: TSplitter
        Left = 433
        Top = 0
        Height = 513
        Beveled = True
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 433
        Height = 513
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 433
          Height = 41
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 0
          object Label1: TLabel
            Left = 16
            Top = 10
            Width = 137
            Height = 20
            Caption = 'Original Chk File:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
          end
        end
        object orgChkMemo: TMemo
          Left = 0
          Top = 41
          Width = 433
          Height = 472
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Fixedsys'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
      object Panel4: TPanel
        Left = 436
        Top = 0
        Width = 429
        Height = 513
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 429
          Height = 41
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 0
          object Label2: TLabel
            Left = 16
            Top = 10
            Width = 132
            Height = 20
            Caption = 'Parsed Chk File:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object parsedChkMemo: TMemo
          Left = 0
          Top = 41
          Width = 429
          Height = 472
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Fixedsys'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Data'
      ImageIndex = 2
      object sg: TStringGrid
        Left = 0
        Top = 41
        Width = 865
        Height = 472
        Align = alClient
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 865
        Height = 41
        Align = alTop
        TabOrder = 1
        object Button2: TButton
          Left = 16
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Show Values'
          TabOrder = 0
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 112
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Show Labels'
          TabOrder = 1
          OnClick = Button3Click
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Log:'
      ImageIndex = 4
      object Memo3: TMemo
        Left = 0
        Top = 41
        Width = 865
        Height = 472
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Fixedsys'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 865
        Height = 41
        Align = alTop
        TabOrder = 1
        object Button6: TButton
          Left = 8
          Top = 8
          Width = 97
          Height = 25
          Caption = 'Clear Log'
          TabOrder = 0
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 120
          Top = 8
          Width = 89
          Height = 25
          Caption = 'Save Log'
          TabOrder = 1
          OnClick = Button7Click
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 352
    Top = 32
  end
  object SaveDialog1: TSaveDialog
    FilterIndex = 0
    Left = 744
    Top = 32
  end
end
