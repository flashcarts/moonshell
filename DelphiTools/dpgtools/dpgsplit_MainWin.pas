unit dpgsplit_MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _dpg_const, ExtCtrls;

type
  TMain = class(TForm)
    DPGOpenDlg: TOpenDialog;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure CopyData(itemname:string;srcfn,dstfn:string;pos,size:integer);
var
  rfs,wfs:TFileStream;
  buf:array of byte;
begin
  setlength(buf,size);

  Application.Title:=itemname+' ReadData.';
  Main.Caption:=Application.Title;

  rfs:=TFileStream.Create(srcfn,fmOpenRead or fmShareDenyWrite);
  rfs.Position:=pos;
  rfs.ReadBuffer(buf[0],size);
  rfs.Free;

  Application.Title:=itemname+' WriteData.';
  Main.Caption:=Application.Title;

  wfs:=TFileStream.Create(dstfn,fmCreate);
  wfs.Position:=0;
  wfs.WriteBuffer(buf[0],size);
  wfs.Free;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Timer1.Enabled:=True;

  Main.ClientHeight:=0;

  Application.Title:='dpgdec.exe for DPG0/1/2/3';
  Main.Caption:=Application.Title;
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  DPGINFO:TDPGINFO;
  fn:string;
  fsize:integer;
begin
  Timer1.Enabled:=False;

  fn:=ParamStr(1);
  if FileExists(fn)=False then fn:='';

  if fn='' then begin
    if DPGOpenDlg.Execute=True then fn:=DPGOpenDlg.FileName;
  end;

  if fn='' then begin
    Application.Terminate;
    exit;
  end;

  if LoadDPGINFO(DPGINFO,fn,fsize)=0 then begin
    ShowMessage('対応していないバージョンです。');
    Application.Terminate;
    exit;
  end;

  if DPGINFO.SndCh=0 then begin
    CopyData('Audio',fn,ChangeFileExt(fn,'.mp2'),DPGINFO.AudioPos,DPGINFO.AudioSize);
  end;
  if (DPGINFO.SndCh=1) or (DPGINFO.SndCh=2) then begin
    CopyData('Audio',fn,ChangeFileExt(fn,'.wav'),DPGINFO.AudioPos,DPGINFO.AudioSize);
  end;
  if DPGINFO.SndCh=3 then begin
    CopyData('Audio',fn,ChangeFileExt(fn,'.ogg'),DPGINFO.AudioPos,DPGINFO.AudioSize);
  end;
  CopyData('Movie',fn,ChangeFileExt(fn,'.m1v'),DPGINFO.MoviePos,DPGINFO.MovieSize);

  Application.Terminate;
end;

end.
