object Main: TMain
  Left = 9
  Top = 117
  BorderStyle = bsToolWindow
  Caption = 'Main'
  ClientHeight = 568
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object FileSelGrpBox: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 553
    Caption = #12501#12449#12452#12523#12522#12473#12488#30011#38754#29992' '#32972#26223#30011#20687
    TabOrder = 0
    object FileSelImg: TImage
      Left = 8
      Top = 96
      Width = 256
      Height = 448
      OnMouseDown = FileSelImgMouseDown
      OnMouseMove = FileSelImgMouseMove
      OnMouseUp = FileSelImgMouseUp
    end
    object FileSelZoomLbl: TLabel
      Left = 240
      Top = 48
      Width = 24
      Height = 12
      Alignment = taRightJustify
      Caption = '100%'
    end
    object FileSelOpenBtn: TBitBtn
      Left = 8
      Top = 24
      Width = 81
      Height = 25
      Caption = #30011#20687#12434#38283#12367
      Default = True
      TabOrder = 0
      OnClick = FileSelOpenBtnClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object FileSel_AutoPaddingChk: TCheckBox
      Left = 8
      Top = 64
      Width = 161
      Height = 17
      Caption = #19978#19979#30011#38754#12398#38553#38291#12434#35036#27491#12377#12427
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = FileSel_AutoPaddingChkClick
    end
    object FileList_ZoomBar: TTrackBar
      Left = 89
      Top = 24
      Width = 182
      Height = 25
      Max = 200
      Min = 25
      Position = 100
      TabOrder = 2
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = FileList_ZoomBarChange
    end
    object FileSel_PastelChk: TCheckBox
      Left = 176
      Top = 64
      Width = 89
      Height = 17
      Caption = #12497#12473#12486#12523#21152#24037
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = FileSel_PastelChkClick
    end
  end
  object LunchGrpBox: TGroupBox
    Left = 288
    Top = 8
    Width = 273
    Height = 553
    Caption = #12501#12449#12452#12523#12521#12531#12481#12515#12540#30011#38754#29992' '#32972#26223#30011#20687
    TabOrder = 1
    object LunchImg: TImage
      Left = 8
      Top = 96
      Width = 256
      Height = 448
      OnMouseDown = LunchImgMouseDown
      OnMouseMove = LunchImgMouseMove
      OnMouseUp = LunchImgMouseUp
    end
    object LunchZoomLbl: TLabel
      Left = 240
      Top = 48
      Width = 24
      Height = 12
      Alignment = taRightJustify
      Caption = '100%'
    end
    object LunchOpenBtn: TBitBtn
      Left = 8
      Top = 24
      Width = 81
      Height = 25
      Caption = #30011#20687#12434#38283#12367
      Default = True
      TabOrder = 0
      OnClick = LunchOpenBtnClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object Lunch_AutoPaddingChk: TCheckBox
      Left = 8
      Top = 64
      Width = 161
      Height = 17
      Caption = #19978#19979#30011#38754#12398#38553#38291#12434#35036#27491#12377#12427
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = Lunch_AutoPaddingChkClick
    end
    object Lunch_ZoomBar: TTrackBar
      Left = 89
      Top = 24
      Width = 182
      Height = 25
      Max = 200
      Min = 25
      Position = 100
      TabOrder = 2
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = Lunch_ZoomBarChange
    end
    object Lunch_PastelChk: TCheckBox
      Left = 176
      Top = 64
      Width = 89
      Height = 17
      Caption = #12497#12473#12486#12523#21152#24037
      TabOrder = 3
      OnClick = Lunch_PastelChkClick
    end
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 32
    Top = 120
    object F1: TMenuItem
      Caption = #12501#12449#12452#12523' (&F)'
      object MainMenu_Create: TMenuItem
        Caption = #29694#22312#12398#35373#23450#12391'BGBMP.DAT'#12434#20316#25104' (&C)'
        OnClick = MainMenu_CreateClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MainMenu_Exit: TMenuItem
        Caption = #38281#12376#12427' (&X)'
        OnClick = MainMenu_ExitClick
      end
    end
    object Q1: TMenuItem
      Caption = #30011#36074#12496#12521#12531#12473' (&Q)'
      object MainMenu_Mode8bit: TMenuItem
        Caption = #20302#30011#36074' 256'#33394#12514#12540#12489' 192kbyte'
        Checked = True
        OnClick = MainMenu_Mode8bitClick
      end
      object MainMenu_Mode15bit: TMenuItem
        Caption = #39640#30011#36074' 32768'#33394#12514#12540#12489' 384kbyte'
        OnClick = MainMenu_Mode15bitClick
      end
    end
    object MainMenu_About: TMenuItem
      Caption = #12496#12540#12472#12519#12531#24773#22577' (&A)'
      OnClick = MainMenu_AboutClick
    end
  end
  object OpenPicDlg: TOpenPictureDialog
    Filter = #12377#12409#12390#12398#12501#12449#12452#12523' (*.jpg;*.jpeg;*.bmp;*.png)|*.jpg;*.jpeg;*.bmp;*.png'
    Left = 72
    Top = 120
  end
end
