object Main: TMain
  Left = -519
  Top = 388
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsToolWindow
  Caption = 'Main'
  ClientHeight = 45
  ClientWidth = 184
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
  object DPGOpenDlg: TOpenDialog
    DefaultExt = 'dpg'
    Filter = 'nDs-mPeG VideoFile (*.dpg)|*.dpg'
    Title = #22793#25563#12377#12427'DPG'#12501#12449#12452#12523#12434#25351#23450#12375#12390#19979#12373#12356#12290
    Left = 8
    Top = 8
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 40
    Top = 8
  end
end
