object Form1: TForm1
  Left = 240
  Top = 118
  Width = 827
  Height = 565
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 819
    Height = 97
    Align = alTop
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 352
      Top = 32
      Width = 23
      Height = 22
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        333FFFFFFFFFFFFFF3333380000000000000333333888888888888883F333300
        7B7B7B7B7B7B033333883F33333333338F33330F07B7B7B7B7B70333338F8F33
        3333333383F3330B0B7B7B7B7B7B7033338F83F33333333338F3330FB0B7B7B7
        B7B7B033338F38F333333333383F330BF07B7B7B7B7B7B03338F383FFFFF3333
        338F330FBF000007B7B7B703338F33888883FFFFFF83330BFBFBFBF000000033
        338F3333333888888833330FBFBFBFBFBFB03333338F333333333338F333330B
        FBFBFBFBFBF03333338F33333FFFFFF83333330FBFBF0000000333333387FFFF
        8888888333333330000033333333333333388888333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        33333333333333333333}
      NumGlyphs = 2
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 768
      Top = 32
      Width = 23
      Height = 22
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        333FFFFFFFFFFFFFF3333380000000000000333333888888888888883F333300
        7B7B7B7B7B7B033333883F33333333338F33330F07B7B7B7B7B70333338F8F33
        3333333383F3330B0B7B7B7B7B7B7033338F83F33333333338F3330FB0B7B7B7
        B7B7B033338F38F333333333383F330BF07B7B7B7B7B7B03338F383FFFFF3333
        338F330FBF000007B7B7B703338F33888883FFFFFF83330BFBFBFBF000000033
        338F3333333888888833330FBFBFBFBFBFB03333338F333333333338F333330B
        FBFBFBFBFBF03333338F33333FFFFFF83333330FBFBF0000000333333387FFFF
        8888888333333330000033333333333333388888333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        33333333333333333333}
      NumGlyphs = 2
      OnClick = SpeedButton1Click
    end
    object edInputFilename: TLabeledEdit
      Left = 24
      Top = 32
      Width = 321
      Height = 21
      EditLabel.Width = 40
      EditLabel.Height = 13
      EditLabel.Caption = '&Input file'
      TabOrder = 0
      Text = 'C:\pas\EpiDataProjekt\test\test.rec'
    end
    object Button1: TButton
      Left = 24
      Top = 64
      Width = 75
      Height = 25
      Caption = '&Read File'
      TabOrder = 2
      OnClick = Button1Click
    end
    object edOutputFilename: TLabeledEdit
      Left = 448
      Top = 32
      Width = 313
      Height = 21
      EditLabel.Width = 48
      EditLabel.Height = 13
      EditLabel.Caption = '&Output file'
      TabOrder = 1
      Text = 'C:\pas\EpiDataProjekt\test\outtest.rec'
    end
    object Button2: TButton
      Left = 448
      Top = 64
      Width = 75
      Height = 25
      Caption = '&Export'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 97
    Width = 819
    Height = 441
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'File info'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 811
        Height = 413
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'File structure'
      ImageIndex = 2
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 811
        Height = 413
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Orig CheckFile'
      ImageIndex = 3
      object memoOrigCheckFile: TMemo
        Left = 0
        Top = 0
        Width = 811
        Height = 413
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Intepreted CheckFile'
      ImageIndex = 4
      object memoIntepredCheck: TMemo
        Left = 0
        Top = 0
        Width = 811
        Height = 413
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      object sg: TStringGrid
        Left = 0
        Top = 33
        Width = 811
        Height = 380
        Align = alClient
        ColCount = 30
        RowCount = 30
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 811
        Height = 33
        Align = alTop
        TabOrder = 1
        object checkShowLabels: TCheckBox
          Left = 8
          Top = 8
          Width = 249
          Height = 17
          Caption = 'Show labels insted of values'
          TabOrder = 0
          OnClick = checkShowLabelsClick
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'EpiData data file (*.rec)|*.rec|Stata file (*.dta)|*.dta|Text fi' +
      'le (*.txt)|*.txt|dBase file (*.dbf)|*.dbf|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 176
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'EpiData data file (*.rec)|*.rec|Stata file (*.dta)|*.dta|Text fi' +
      'le (*.txt)|*.txt|dBase file (*.dbf)|*.dbf|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 208
    Top = 64
  end
end
