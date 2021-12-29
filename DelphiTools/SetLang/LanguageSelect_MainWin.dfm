object Main: TMain
  Left = 241
  Top = 500
  Width = 328
  Height = 245
  BorderStyle = bsSizeToolWin
  Caption = 'Language select tool for MoonShell2 (General)'
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label2: TLabel
    Left = 8
    Top = 192
    Width = 100
    Height = 12
    Caption = 'LanguageSelect.exe'
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 176
    Height = 12
    Caption = 'Only Japanese corresponds. sorry.'
  end
  object SaveBtn: TBitBtn
    Left = 144
    Top = 184
    Width = 81
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = SaveBtnClick
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 232
    Top = 184
    Width = 81
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
    Kind = bkCancel
  end
  object LangSetsLst: TListBox
    Left = 8
    Top = 24
    Width = 305
    Height = 153
    ItemHeight = 12
    TabOrder = 0
  end
  object StartupTimer: TTimer
    Interval = 1
    OnTimer = StartupTimerTimer
    Left = 16
    Top = 64
  end
end
