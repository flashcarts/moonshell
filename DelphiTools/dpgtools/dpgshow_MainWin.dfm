object Main: TMain
  Left = -555
  Top = 366
  Width = 264
  Height = 219
  Caption = 'Main'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object StatusBar1: TStatusBar
    Left = 0
    Top = 173
    Width = 256
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object DPGOpenDlg: TOpenDialog
    DefaultExt = 'dpg'
    Filter = 'nDs-mPeG VideoFile (*.dpg)|*.dpg'
    Title = #20877#29983#12377#12427'DPG'#12501#12449#12452#12523#12434#25351#23450#12375#12390#19979#12373#12356#12290
    Left = 8
    Top = 32
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 40
    Top = 32
  end
  object IntervalTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = IntervalTimerTimer
    Left = 72
    Top = 32
  end
end
