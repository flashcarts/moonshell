object Network: TNetwork
  Left = 277
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Get from YouTube'
  ClientHeight = 192
  ClientWidth = 576
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
    Top = 11
    Width = 24
    Height = 12
    Caption = 'URL:'
  end
  object StatusLbl: TLabel
    Left = 8
    Top = 156
    Width = 48
    Height = 12
    Caption = 'StatusLbl'
  end
  object LogMemo: TMemo
    Left = 168
    Top = 40
    Width = 401
    Height = 129
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS UI Gothic'
    Font.Style = []
    Lines.Strings = (
      'LogMemo')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object PrgBar: TProgressBar
    Left = 8
    Top = 173
    Width = 553
    Height = 15
    TabOrder = 3
  end
  object URLEdt: TEdit
    Left = 40
    Top = 8
    Width = 353
    Height = 20
    TabOrder = 0
    Text = 'URLEdt'
    OnClick = URLEdtClick
    OnKeyDown = URLEdtKeyDown
    OnMouseDown = URLEdtMouseDown
  end
  object StartBtn: TBitBtn
    Left = 400
    Top = 6
    Width = 75
    Height = 24
    Caption = 'START'
    TabOrder = 1
    OnClick = StartBtnClick
    Kind = bkOK
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 153
    Height = 113
    Caption = 'Source formats'
    TabOrder = 4
    object fmt22Chk: TCheckBox
      Left = 8
      Top = 16
      Width = 137
      Height = 17
      Caption = 'fmt=22 1280x720 MP4'
      TabOrder = 0
    end
    object fmt18Chk: TCheckBox
      Left = 8
      Top = 40
      Width = 137
      Height = 17
      Caption = 'fmt=18 480x360 MP4'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object fmt6Chk: TCheckBox
      Left = 8
      Top = 64
      Width = 137
      Height = 17
      Caption = 'fmt=6 448x336 FLV'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object fmt0Chk: TCheckBox
      Left = 8
      Top = 88
      Width = 137
      Height = 17
      Caption = 'default 320x240 FLV'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 3
    end
  end
  object CancelBtn: TBitBtn
    Left = 496
    Top = 6
    Width = 75
    Height = 24
    Caption = 'Cancel'
    TabOrder = 5
    Kind = bkCancel
  end
  object IdHTTP1: TIdHTTP
    MaxLineAction = maException
    ReadTimeout = 5000
    RecvBufferSize = 65536
    SendBufferSize = 65536
    OnWork = IdHTTP1Work
    OnWorkBegin = IdHTTP1WorkBegin
    OnWorkEnd = IdHTTP1WorkEnd
    AllowCookies = True
    HandleRedirects = True
    ProtocolVersion = pv1_0
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.ContentType = 'text/html'
    Request.Accept = '*/*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 184
    Top = 64
  end
end
