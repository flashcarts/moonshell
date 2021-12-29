object Main: TMain
  Left = 244
  Top = 190
  BorderStyle = bsDialog
  Caption = 'Main'
  ClientHeight = 236
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object LogLst: TListBox
    Left = 0
    Top = 0
    Width = 397
    Height = 236
    Align = alClient
    ItemHeight = 12
    TabOrder = 0
  end
  object StartupTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = StartupTimerTimer
    Left = 16
    Top = 32
  end
end
