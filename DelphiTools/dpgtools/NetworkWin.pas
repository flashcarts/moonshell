unit NetworkWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP;

type
  TNetwork = class(TForm)
    LogMemo: TMemo;
    PrgBar: TProgressBar;
    URLEdt: TEdit;
    Label1: TLabel;
    StartBtn: TBitBtn;
    GroupBox1: TGroupBox;
    StatusLbl: TLabel;
    fmt22Chk: TCheckBox;
    fmt18Chk: TCheckBox;
    fmt6Chk: TCheckBox;
    fmt0Chk: TCheckBox;
    IdHTTP1: TIdHTTP;
    CancelBtn: TBitBtn;
    procedure URLEdtClick(Sender: TObject);
    procedure URLEdtKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure URLEdtMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StartBtnClick(Sender: TObject);
    procedure IdHTTP1WorkBegin(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCountMax: Integer);
    procedure IdHTTP1Work(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
    procedure IdHTTP1WorkEnd(Sender: TObject; AWorkMode: TWorkMode);
  private
    { Private 宣言 }
    procedure FormDisabled;
    procedure FormEnabled;
  public
    { Public 宣言 }
    OptOutputPath:string;
    VideoFilename:string;
    procedure Start(_OptOutputPath:string);
  end;

var
  Network: TNetwork;

implementation

{$R *.dfm}

const EditMessage='Please input YouTube address.';

var
  fsize:integer;
  showhttplog:boolean;

procedure TNetwork.IdHTTP1WorkBegin(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCountMax: Integer);
begin
  if showhttplog=False then exit;
  fsize:=AWorkCountMax;
  if fsize<4096 then exit;
  LogMemo.Lines.Add('HTTP: Download. ('+inttostr(fsize div 1024)+'kbyte)');
  PrgBar.Position:=0;
  PrgBar.Max:=fsize;
end;

procedure TNetwork.IdHTTP1Work(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
var
  fpos:integer;
begin
  Application.ProcessMessages;
  if showhttplog=False then exit;
  if fsize<4096 then exit;
  fpos:=AWorkCount;
  PrgBar.Position:=fpos;
  StatusLbl.Caption:=format('Download. %d%% (%d/%dk)',[trunc(fpos/fsize*100),fpos div 1024,fsize div 1024]);
end;

procedure TNetwork.IdHTTP1WorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  if showhttplog=False then exit;
  if fsize<4096 then exit;
  Caption:='Downloaded.';
  PrgBar.Position:=0;
end;

// ----------------------------------------------------

procedure TNetwork.FormDisabled;
begin
  Network.Enabled:=False;
  StartBtn.Enabled:=False;
  CancelBtn.Enabled:=False;
  StatusLbl.Caption:='Standby...';
end;

procedure TNetwork.FormEnabled;
begin
  Network.Enabled:=True;
  StartBtn.Enabled:=True;
  CancelBtn.Enabled:=True;
  StatusLbl.Caption:='Standby...';
end;

procedure TNetwork.Start(_OptOutputPath:string);
begin
  FormEnabled;
  URLEdt.Text:=EditMessage;

  LogMemo.Clear;

  LogMemo.Lines.Add('STARTボタンを押した後に、ダウンロード処理を中断することはできません。');
  LogMemo.Lines.Add('左枠の設定で高画質の動画を元にすると、作成される動画も少し綺麗になりますが、エンコード時間も長くなります。');
  LogMemo.Lines.Add('');
  LogMemo.Lines.Add('After the START button is pushed, the download processing cannot be interrupted.');
  LogMemo.Lines.Add('');
  LogMemo.Lines.Add('When you specify the video of a high resolution by setting the left.');
  LogMemo.Lines.Add('The output DPG video becomes a little beautiful, and the encode time becomes long.');

  fsize:=0;
  showhttplog:=False;

  OptOutputPath:=_OptOutputPath;
  VideoFilename:='';
end;

// ----------------------------------------------------

procedure TNetwork.URLEdtClick(Sender: TObject);
begin
  if URLEdt.Text=EditMessage then URLEdt.Text:='';
end;

procedure TNetwork.URLEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if URLEdt.Text=EditMessage then URLEdt.Text:='';
end;

procedure TNetwork.URLEdtMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if URLEdt.Text=EditMessage then URLEdt.Text:='';
end;

// ----------------------------------------------------

procedure TNetwork.StartBtnClick(Sender: TObject);
var
  htmlstr:string;
  whtmlstr:widestring;
  wfs:TFileStream;
  Title,Description,Keywords:string;
  VideoID,OneShotID:string;
  DownloadURL:string;
  flvdata:string;
  function GetMetaData(ID:string):string;
  var
    pos:integer;
  begin
    pos:=ansipos(ID,htmlstr);
    if pos=0 then begin
      ID:=ansilowercase(ID);
      pos:=ansipos(ID,htmlstr);
      if pos=0 then begin
        Result:='not found meta data.';
        exit;
      end;
    end;
    Result:=copy(htmlstr,pos+length(ID),length(htmlstr));
    pos:=ansipos('"',Result);
    Result:=copy(Result,1,pos-1);
  end;
begin
  ModalResult:=mrNone;

  FormDisabled;

  LogMemo.Clear;

  fsize:=0;
  showhttplog:=False;

  StatusLbl.Caption:='Try connect YouTube server.';

  try
    htmlstr:=IdHTTP1.Get(URLEdt.Text);
    whtmlstr:=UTF8Decode(htmlstr);
    htmlstr:=whtmlstr;
    except else begin
      LogMemo.Lines.Add('Network error.');
      FormEnabled;
      exit;
    end;
  end;

  Title:=GetMetaData('<meta name="Title" content="');
  Description:=GetMetaData('<meta name="Description" content="');
  Keywords:=GetMetaData('<meta name="Keywords" content="');

  VideoID:=GetMetaData('"video_id": "');
  OneShotID:=GetMetaData('"t": "');

  DownloadURL:=format('http://youtube.com/get_video.php?video_id=%s&t=%s',[VideoID,OneShotID]);

  VideoFilename:=OptOutputPath+formatDateTime('yyyymmdd-hhnnss',now)+' '+Title;

  LogMemo.Lines.Add('--- YouTube information.');
  LogMemo.Lines.Add('Title: '+Title);
  LogMemo.Lines.Add('Description: '+Description);
  LogMemo.Lines.Add('Keywords: '+Keywords);
  LogMemo.Lines.Add('Video ID: '+VideoID);
  LogMemo.Lines.Add('OneShot ID: '+OneShotID);
  LogMemo.Lines.Add('Download URL: '+DownloadURL);

  flvdata:='';

  showhttplog:=True;

  if (flvdata='') and (fmt22Chk.Checked=True) then begin
    try
      StatusLbl.Caption:='Try fmt=22 1280x720 MP4...';
      flvdata:=IdHTTP1.Get(DownloadURL+'&fmt=22');
      VideoFilename:=VideoFilename+'.mp4';
      except else begin
        LogMemo.Lines.Add('Not found this format data.');
      end;
    end;
  end;

  if (flvdata='') and (fmt18Chk.Checked=True) then begin
    try
      StatusLbl.Caption:='Try fmt=18 480x360 MP4...';
      flvdata:=IdHTTP1.Get(DownloadURL+'&fmt=18');
      VideoFilename:=VideoFilename+'.mp4';
      except else begin
        LogMemo.Lines.Add('Not found this format data.');
      end;
    end;
  end;

  if (flvdata='') and (fmt6Chk.Checked=True) then begin
    try
      StatusLbl.Caption:='Try fmt=6 448x336 FLV...';
      flvdata:=IdHTTP1.Get(DownloadURL+'&fmt=6');
      VideoFilename:=VideoFilename+'.flv';
      except else begin
        LogMemo.Lines.Add('Not found this format data.');
      end;
    end;
  end;

  if (flvdata='') and (fmt0Chk.Checked=True) then begin
    try
      StatusLbl.Caption:='Try default format 320x240 FLV...';
      flvdata:=IdHTTP1.Get(DownloadURL);
      VideoFilename:=VideoFilename+'.flv';
      except else begin
        LogMemo.Lines.Add('Not found this format data.');
      end;
    end;
  end;

  StatusLbl.Caption:='';

  showhttplog:=False;

  if flvdata='' then begin
    LogMemo.Lines.Add('Fatal error: Not found video file.');
    FormEnabled;
    exit;
  end;

  LogMemo.Lines.Add('Video filename: '+VideoFilename);

  wfs:=TFileStream.Create(VideoFilename,fmCreate);
  wfs.WriteBuffer(flvdata[1],length(flvdata));
  wfs.Free;

  LogMemo.Lines.Add('Download successed. Regist to main queue.');
  
  LogMemo.Lines.SaveToFile(changefileext(Application.ExeName,'')+'_YouTubeDownloader.log');

  FormEnabled;

  ModalResult:=mrOk;
end;

end.
