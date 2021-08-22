object Form1: TForm1
  Left = 855
  Top = 25
  Width = 397
  Height = 409
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Img: TImage
    Left = 0
    Top = 0
    Width = 256
    Height = 48
  end
  object Label1: TLabel
    Left = 168
    Top = 200
    Width = 72
    Height = 18
    Caption = 'MoonShell'
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 120
    Top = 280
    Width = 33
    Height = 12
    Caption = 'Label2'
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 24
    Top = 88
  end
end
