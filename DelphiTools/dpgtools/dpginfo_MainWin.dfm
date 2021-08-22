object Main: TMain
  Left = -502
  Top = 405
  Width = 340
  Height = 342
  Caption = 'Main'
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object InfoMemo: TMemo
    Left = 0
    Top = 0
    Width = 332
    Height = 315
    Align = alClient
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Terminal'
    Font.Style = []
    Lines.Strings = (
      'InfoMemo')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object dpgOpenDlg: TOpenDialog
    DefaultExt = 'dpg'
    Filter = 'nDs-mPeG VideoFile (*.dpg)|*.dpg'
    Left = 8
    Top = 8
  end
end
