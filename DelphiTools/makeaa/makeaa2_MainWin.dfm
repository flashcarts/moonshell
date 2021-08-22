object Main: TMain
  Left = 247
  Top = 165
  BorderStyle = bsSingle
  Caption = 'Main'
  ClientHeight = 274
  ClientWidth = 689
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
  object PrvImg: TImage
    Left = 160
    Top = 8
    Width = 521
    Height = 257
  end
  object Image1: TImage
    Left = 176
    Top = 72
    Width = 32
    Height = 32
    Visible = False
  end
  object Label1: TLabel
    Left = 168
    Top = 16
    Width = 104
    Height = 24
    Caption = 'Label1 '#12354#12356#12358
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = #12513#12452#12522#12458
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object Label2: TLabel
    Left = 168
    Top = 40
    Width = 84
    Height = 24
    Caption = 'Label2 cp0'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = #12513#12452#12522#12458
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 145
    Height = 257
    ItemHeight = 12
    TabOrder = 0
  end
end
