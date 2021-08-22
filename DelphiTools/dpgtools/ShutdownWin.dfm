object Shutdown: TShutdown
  Left = 91
  Top = 83
  BorderStyle = bsDialog
  Caption = 'Shutdown...'
  ClientHeight = 117
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 193
    Height = 12
    Caption = #20840#12390#12398'DPG'#12456#12531#12467#12540#12489#12364#32066#20102#12375#12414#12375#12383#12290
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 193
    Height = 12
    Caption = #12354#12392'%d'#31186#12391#33258#21205#12471#12515#12483#12488#12480#12454#12531#12375#12414#12377#8230
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 104
    Height = 12
    Caption = #12456#12521#12540#12364#12354#12426#12414#12375#12383#12290
  end
  object Label4: TLabel
    Left = 8
    Top = 96
    Width = 144
    Height = 12
    Caption = #12471#12515#12483#12488#12480#12454#12531#12434#32154#34892#12375#12414#12377#12290
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 56
    Width = 241
    Height = 15
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 160
    Top = 81
    Width = 89
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object TimeoutTimer: TTimer
    Enabled = False
    OnTimer = TimeoutTimerTimer
    Left = 16
    Top = 16
  end
  object BeepTimer: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = BeepTimerTimer
    Left = 56
    Top = 16
  end
end
