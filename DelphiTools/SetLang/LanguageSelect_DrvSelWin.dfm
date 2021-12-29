object DrvSel: TDrvSel
  Left = 35
  Top = 302
  BorderStyle = bsToolWindow
  Caption = 'DrvSel'
  ClientHeight = 120
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 292
    Height = 12
    Caption = 'MoonShell2'#12434#12452#12531#12473#12488#12540#12523#12375#12383#12487#12451#12473#12463#12434#36984#25246#12375#12390#12367#12384#12373#12356#12290
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 287
    Height = 12
    Caption = 'Please select the disk where MoonShell2 was installed. '
  end
  object BitBtn1: TBitBtn
    Left = 136
    Top = 88
    Width = 97
    Height = 25
    TabOrder = 0
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 248
    Top = 88
    Width = 99
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object DriveLst: TComboBox
    Left = 8
    Top = 48
    Width = 337
    Height = 26
    Style = csDropDownList
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Terminal'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 2
  end
end
