object enclog: Tenclog
  Left = 32
  Top = 407
  AutoScroll = False
  BorderStyle = bsSizeToolWin
  Caption = 'enclog'
  ClientHeight = 381
  ClientWidth = 426
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
  object loglst: TMemo
    Left = 0
    Top = 0
    Width = 426
    Height = 381
    Align = alClient
    Lines.Strings = (
      'loglst')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
end
