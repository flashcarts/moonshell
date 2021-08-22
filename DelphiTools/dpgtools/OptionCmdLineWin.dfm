object OptionCmdLine: TOptionCmdLine
  Left = -561
  Top = 366
  Width = 632
  Height = 243
  BorderStyle = bsSizeToolWin
  Caption = 'mencoder edit command line format.'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 33
    Height = 12
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 33
    Height = 12
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 184
    Top = 192
    Width = 288
    Height = 12
    Caption = 'The command line parameter doesn'#39't do the error check.'
  end
  object CmdLineEdt: TEdit
    Left = 8
    Top = 48
    Width = 609
    Height = 20
    TabOrder = 0
    Text = 'CmdLineEdt'
    OnChange = CmdLineEdtChange
  end
  object PreviewMemo: TMemo
    Left = 8
    Top = 72
    Width = 609
    Height = 105
    Lines.Strings = (
      'PreviewMemo')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 96
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    Kind = bkCancel
  end
end
