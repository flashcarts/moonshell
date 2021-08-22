object Form1: TForm1
  Left = -569
  Top = 366
  Width = 266
  Height = 510
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object PrevImg: TImage
    Left = 0
    Top = 96
    Width = 256
    Height = 384
  end
  object LogLst: TListBox
    Left = 0
    Top = 0
    Width = 257
    Height = 97
    ItemHeight = 12
    TabOrder = 0
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 16
    OnTimer = Timer1Timer
    Left = 160
    Top = 24
  end
  object OpenDlg: TOpenDialog
    FileName = 'splash.ini'
    Filter = 'MoonShell splash setting file (splash.ini)|splash.ini'
    Title = 'Select MoonShell splash setting file (splash.ini)'
    Left = 120
    Top = 240
  end
end
