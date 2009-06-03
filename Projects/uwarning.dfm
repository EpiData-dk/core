object WarningForm: TWarningForm
  Left = 168
  Top = 120
  Width = 480
  Height = 300
  Caption = 'WARNING - EpiData Core module test application - WARNING'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 154
    Top = 40
    Width = 164
    Height = 37
    Caption = 'WARNING'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 53
    Top = 88
    Width = 384
    Height = 16
    Caption = 'Application for testing of EpiData Software Core module'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 78
    Top = 160
    Width = 311
    Height = 16
    Caption = 'Press OK to accept that this is only for testing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 48
    Top = 120
    Width = 387
    Height = 16
    Caption = 
      'Data quality problems can Occur. Use only for test of core modul' +
      'e'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Button1: TButton
    Left = 296
    Top = 200
    Width = 90
    Height = 25
    Caption = 'OK - start test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
  end
  object Button2: TButton
    Left = 71
    Top = 200
    Width = 75
    Height = 25
    Caption = 'E&xit'
    ModalResult = 2
    TabOrder = 1
  end
end
