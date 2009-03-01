object formPW: TformPW
  Left = 310
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Password'
  ClientHeight = 188
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 56
    Width = 73
    Height = 13
    Caption = 'Enter password'
  end
  object labelRepeatPW: TLabel
    Left = 32
    Top = 88
    Width = 83
    Height = 13
    Caption = 'Repeat password'
  end
  object labelFilename: TLabel
    Left = 32
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object editPW1: TEdit
    Left = 136
    Top = 56
    Width = 177
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object editPW2: TEdit
    Left = 136
    Top = 88
    Width = 177
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 64
    Top = 136
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 208
    Top = 136
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
end
