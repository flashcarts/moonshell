object Main: TMain
  Left = -582
  Top = 438
  BorderStyle = bsDialog
  Caption = 'CreateProFont'
  ClientHeight = 132
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS UI Gothic'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object PrevAImg: TImage
    Left = 0
    Top = 40
    Width = 561
    Height = 41
  end
  object PrevWImg: TImage
    Left = 0
    Top = 80
    Width = 561
    Height = 41
  end
  object NoJPUnicodeLabel: TLabel
    Left = 24
    Top = 88
    Width = 107
    Height = 16
    Caption = 'NoJPUnicodeLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial Unicode MS'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object AnkJPLabel: TLabel
    Left = 35
    Top = 40
    Width = 65
    Height = 15
    Caption = 'AnkJPLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object JPUnicodeLabel: TLabel
    Left = 24
    Top = 64
    Width = 82
    Height = 12
    Caption = 'JPUnicodeLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object AnkNoJPLabel: TLabel
    Left = 36
    Top = 8
    Width = 101
    Height = 19
    Caption = 'AnkNoJPLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object CP950ZHO_HKLabel: TLabel
    Left = 272
    Top = 8
    Width = 103
    Height = 12
    Caption = 'CP950ZHO_HKLabel'
    Font.Charset = CHINESEBIG5_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'PMingLiU'
    Font.Style = []
    ParentFont = False
  end
  object CP936zhoLabel: TLabel
    Left = 256
    Top = 40
    Width = 78
    Height = 12
    Caption = 'CP936zhoLabel'
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'SimSun'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object CP949korLabel: TLabel
    Left = 248
    Top = 64
    Width = 78
    Height = 12
    Caption = 'CP949korLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'GulimChe'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object CP998Label: TLabel
    Left = 184
    Top = 96
    Width = 63
    Height = 14
    Caption = 'CP998Label'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object AnkJPFixLabel: TLabel
    Left = 147
    Top = 40
    Width = 78
    Height = 12
    Caption = 'AnkJPFixLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object JPUnicodeFixLabel: TLabel
    Left = 144
    Top = 56
    Width = 102
    Height = 12
    Caption = 'JPUnicodeFixLabel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object prgbar: TProgressBar
    Left = 0
    Top = 24
    Width = 561
    Height = 15
    TabOrder = 0
  end
  object iniOpenDlg: TOpenDialog
    Filter = 'font setting file (*.ini)|*.ini'
    Title = 'select font setting file.'
    Left = 104
    Top = 48
  end
end
