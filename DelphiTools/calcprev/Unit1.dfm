object Form1: TForm1
  Left = 239
  Top = 124
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 447
  ClientWidth = 288
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 433
    Caption = '256x192 screen preview'
    TabOrder = 0
    object TopImg: TImage
      Left = 8
      Top = 16
      Width = 256
      Height = 192
    end
    object BtmImg: TImage
      Left = 8
      Top = 232
      Width = 256
      Height = 192
      PopupMenu = PopupMenu1
    end
    object Button1: TButton
      Left = 16
      Top = 344
      Width = 32
      Height = 32
      Caption = '1'
      TabOrder = 0
    end
    object Button2: TButton
      Left = 16
      Top = 384
      Width = 32
      Height = 32
      Caption = '0'
      TabOrder = 1
    end
    object Button3: TButton
      Left = 56
      Top = 344
      Width = 32
      Height = 32
      Caption = '2'
      TabOrder = 2
    end
    object Button4: TButton
      Left = 96
      Top = 344
      Width = 32
      Height = 32
      Caption = '3'
      TabOrder = 3
    end
    object Button16: TButton
      Left = 144
      Top = 344
      Width = 32
      Height = 32
      Caption = '+'
      TabOrder = 4
    end
    object Button20: TButton
      Left = 144
      Top = 384
      Width = 72
      Height = 32
      Caption = #65309
      TabOrder = 5
    end
    object Button6: TButton
      Left = 56
      Top = 304
      Width = 32
      Height = 32
      Caption = '5'
      TabOrder = 6
    end
    object Button7: TButton
      Left = 96
      Top = 304
      Width = 32
      Height = 32
      Caption = '6'
      TabOrder = 7
    end
    object Button9: TButton
      Left = 56
      Top = 264
      Width = 32
      Height = 32
      Caption = '8'
      TabOrder = 8
    end
    object Button10: TButton
      Left = 96
      Top = 264
      Width = 32
      Height = 32
      Caption = '9'
      TabOrder = 9
    end
    object Button13: TButton
      Left = 224
      Top = 384
      Width = 32
      Height = 32
      Caption = #247
      TabOrder = 10
    end
    object Button14: TButton
      Left = 224
      Top = 344
      Width = 32
      Height = 32
      Caption = #215
      TabOrder = 11
    end
    object Button17: TButton
      Left = 144
      Top = 264
      Width = 32
      Height = 32
      Caption = #65288
      TabOrder = 12
    end
    object Button18: TButton
      Left = 144
      Top = 304
      Width = 32
      Height = 32
      Caption = #65289
      TabOrder = 13
    end
    object Button11: TButton
      Left = 56
      Top = 384
      Width = 32
      Height = 32
      Caption = #12539
      TabOrder = 14
    end
    object Button12: TButton
      Left = 96
      Top = 384
      Width = 32
      Height = 32
      Caption = 'DEL'
      TabOrder = 15
    end
    object Button15: TButton
      Left = 184
      Top = 344
      Width = 32
      Height = 32
      Caption = '-'
      TabOrder = 16
    end
    object Button8: TButton
      Left = 16
      Top = 264
      Width = 32
      Height = 32
      Caption = '7'
      TabOrder = 17
    end
    object Button5: TButton
      Left = 16
      Top = 304
      Width = 32
      Height = 32
      Caption = '4'
      TabOrder = 18
    end
    object Button19: TButton
      Left = 184
      Top = 264
      Width = 72
      Height = 32
      Caption = #21069#31572#12434#12467#12500#12540
      PopupMenu = PopupMenu1
      TabOrder = 19
    end
    object Button21: TButton
      Left = 184
      Top = 304
      Width = 72
      Height = 32
      Caption = #26908#31639#30011#38754
      TabOrder = 20
    end
    object StaticText1: TStaticText
      Left = 16
      Top = 190
      Width = 116
      Height = 16
      Caption = #20837#21147#24335#12398#36884#20013#32080#26524#65306' 5'
      Color = clWhite
      ParentColor = False
      TabOrder = 21
    end
    object ListBox1: TListBox
      Left = 8
      Top = 48
      Width = 257
      Height = 137
      Ctl3D = False
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Terminal'
      Font.Style = []
      ItemHeight = 13
      Items.Strings = (
        ''
        #24335'1: 8*42/5 = 67.2'
        ''
        #24335'2: 67.2/0 = 0'#38500#31639#12456#12521#12540
        ''
        #24335'3: 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15'
        '     +16+17+18+19+20 = 210'
        #24335'4: 52+8 = 60')
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 22
    end
    object StaticText2: TStaticText
      Left = 16
      Top = 24
      Width = 56
      Height = 16
      Caption = 'Calculator'
      Color = clWhite
      ParentColor = False
      TabOrder = 23
    end
    object Edit1: TEdit
      Left = 16
      Top = 238
      Width = 241
      Height = 21
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Terminal'
      Font.Style = []
      ParentFont = False
      TabOrder = 24
      Text = 'Edit1'
    end
  end
  object PopupMenu1: TPopupMenu
    AutoHotkeys = maManual
    Left = 24
    Top = 224
    object N16721: TMenuItem
      Caption = #31572'1: 67.2'
    end
    object N21: TMenuItem
      Caption = #31572'2: ---'
    end
    object N32101: TMenuItem
      Caption = #31572'3: 210'
    end
    object N4601: TMenuItem
      Caption = #31572'4: 60'
    end
    object N51: TMenuItem
      Caption = #31572'5: ---'
    end
  end
end
