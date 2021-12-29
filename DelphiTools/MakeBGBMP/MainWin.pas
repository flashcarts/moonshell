unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, Buttons, ExtCtrls, ExtDlgs, ComCtrls,Jpeg,gldpng;

type
  TMain = class(TForm)
    FileSelGrpBox: TGroupBox;
    FileSelImg: TImage;
    LunchGrpBox: TGroupBox;
    LunchImg: TImage;
    FileSelOpenBtn: TBitBtn;
    LunchOpenBtn: TBitBtn;
    FileSel_AutoPaddingChk: TCheckBox;
    Lunch_AutoPaddingChk: TCheckBox;
    MainMenu1: TMainMenu;
    F1: TMenuItem;
    MainMenu_About: TMenuItem;
    MainMenu_Create: TMenuItem;
    N1: TMenuItem;
    MainMenu_Exit: TMenuItem;
    Q1: TMenuItem;
    MainMenu_Mode8bit: TMenuItem;
    MainMenu_Mode15bit: TMenuItem;
    OpenPicDlg: TOpenPictureDialog;
    FileList_ZoomBar: TTrackBar;
    Lunch_ZoomBar: TTrackBar;
    FileSelZoomLbl: TLabel;
    LunchZoomLbl: TLabel;
    FileSel_PastelChk: TCheckBox;
    Lunch_PastelChk: TCheckBox;
    procedure MainMenu_ExitClick(Sender: TObject);
    procedure MainMenu_CreateClick(Sender: TObject);
    procedure MainMenu_AboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileSel_AutoPaddingChkClick(Sender: TObject);
    procedure Lunch_AutoPaddingChkClick(Sender: TObject);
    procedure FileSelImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FileSelImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FileSelImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LunchImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LunchImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LunchImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FileList_ZoomBarChange(Sender: TObject);
    procedure Lunch_ZoomBarChange(Sender: TObject);
    procedure FileSelOpenBtnClick(Sender: TObject);
    procedure LunchOpenBtnClick(Sender: TObject);
    procedure MainMenu_Mode8bitClick(Sender: TObject);
    procedure MainMenu_Mode15bitClick(Sender: TObject);
    procedure FileSel_PastelChkClick(Sender: TObject);
    procedure Lunch_PastelChkClick(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses _PicTools,_m_Tools,BGBMP;

var
  BGBMPType:EBGBMPType;

const BGBMPFilename='BGBMP.DAT';

const ScreenWidth=256;
const ScreenHeight=192;
const ScreenPadding=64;

type
  TPicInfo=record
    PosX,PosY:integer;
    Zoom:double;
    AutoPadding:boolean;
    Pastel:boolean;
  end;

var
  FileSelMasterBM,LunchMasterBM:TBitmap;
  FileSelPicInfo,LunchPicInfo:TPicInfo;

function LoadFromFile_bmp(var bm:TBitmap;fn:string):boolean;
begin
  bm:=TBitmap.Create;
  try
    bm.LoadFromFile(fn);
    bm.PixelFormat:=pf24bit;
    except else begin
      Result:=False;
      exit;
    end;
  end;

  if (bm.Height=0) or (bm.Width=0) then begin
    bm.Free;
    Result:=False;
    exit;
  end;

  Result:=True;
end;

function LoadFromFile_jpg(var bm:TBitmap;fn:string):boolean;
var
  JpegImg:TJpegImage;
begin
  JpegImg:=TJpegImage.Create;

  JpegImg.PixelFormat:=jf24bit;
  JpegImg.Grayscale:=False;

  JpegImg.ProgressiveDisplay:=False;
  JpegImg.LoadFromFile(fn);

  if (JpegImg.Height=0) or (JpegImg.Width=0) then begin
    JpegImg.Free;
    Result:=False;
    exit;
  end;

  bm:=TBitmap.Create;
  MakeBlankBM(bm,JpegImg.Width,JpegImg.Height,pf24bit);

  bm.Canvas.Draw(0,0,JpegImg);

  JpegImg.Free;

  Result:=True;
end;

function LoadFromFile_png(var bm:TBitmap;fn:string):boolean;
var
  png:TGLDPNG;
  abm:TBitmap;
  x,y,w,h:integer;
  srcpb,alphapb:PByteArray;
  transcol,alpha,ialpha:dword;
  tr,tg,tb:dword;
begin
  bm:=TBitmap.Create;

  png:=TGLDPNG.Create;
  png.Image:=bm;
  png.LoadFromFile(fn);

  bm.PixelFormat:=pf24bit;

  abm:=TBitmap.Create;
  if png.AlphaBitmapAssignTo(abm)=True then begin
    abm.PixelFormat:=pf8bit;
    transcol:=png.BGColor;
    if transcol=GLD_NONECOLOR then transcol:=RGB($80,$80,$80); // 背景色が未定義のときは灰色を使う
    if (transcol and $1000000)<>0 then transcol:=RGB($80,$80,$80); // 背景色がパレットカラーのときは灰色を使う
    tr:=(transcol shr 0) and $ff;
    tg:=(transcol shr 8) and $ff;
    tb:=(transcol shr 16) and $ff;
    w:=abm.Width;
    h:=abm.Height;
    for y:=0 to h-1 do begin
      srcpb:=bm.ScanLine[y];
      alphapb:=abm.ScanLine[y];
      for x:=0 to w-1 do begin
        alpha:=alphapb[x*1+0];
        ialpha:=$ff-alpha;
        srcpb[x*3+0]:=((srcpb[x*3+0]*ialpha) div $100)+((tb*alpha) div $100);
        srcpb[x*3+1]:=((srcpb[x*3+1]*ialpha) div $100)+((tg*alpha) div $100);
        srcpb[x*3+2]:=((srcpb[x*3+2]*ialpha) div $100)+((tr*alpha) div $100);
      end;
    end;
  end;
  abm.Free;

  png.Free;

  Result:=True;
end;

function LoadImageFromFile(var bm:TBitmap;fn:string):boolean;
var
  ext:string;
begin
  ext:=ansilowercase(extractfileext(fn));

  if ext='.bmp' then begin
    Result:=LoadFromFile_bmp(bm,fn);
    exit;
  end;

  if ext='.jpg' then begin
    Result:=LoadFromFile_jpg(bm,fn);
    exit;
  end;

  if ext='.png' then begin
    Result:=LoadFromFile_png(bm,fn);
    exit;
  end;

  Result:=False;
end;

procedure RedrawPreviewImage(dstbm:TBitmap;var srcbm:TBitmap;PicInfo:TPicInfo);
var
  z:double;
  sx,sy,sw,sh:integer;
  x,y:integer;
  pb1,pb2:pByteArray;
  c:integer;
begin
  z:=1/PicInfo.Zoom;
  if PicInfo.PosX<0 then PicInfo.PosX:=0;
  if PicInfo.PosY<0 then PicInfo.PosY:=0;

  SetStretchBltMode(dstbm.Canvas.Handle,HALFTONE);

  dstbm.Canvas.Brush.Color:=0;
  dstbm.Canvas.Brush.Style:=bsSolid;
  dstbm.Canvas.FillRect(Rect(0,0,dstbm.Width,dstbm.Height));

  sx:=PicInfo.PosX;
  sy:=PicInfo.PosY;
  sw:=trunc(ScreenWidth*z);
  sh:=trunc(ScreenHeight*z);

  StretchBlt(dstbm.Canvas.Handle,0,0,ScreenWidth,ScreenHeight,srcbm.Canvas.Handle,sx,sy,sw,sh,SRCCOPY);

  sh:=trunc(ScreenHeight*z);
  inc(sy,sh);

  if PicInfo.AutoPadding=True then begin
    sh:=trunc(ScreenPadding*z);
    inc(sy,sh);
  end;

  sh:=trunc(ScreenHeight*z);

  StretchBlt(dstbm.Canvas.Handle,0,ScreenHeight+ScreenPadding,ScreenWidth,ScreenHeight,srcbm.Canvas.Handle,sx,sy,sw,sh,SRCCOPY);

  if PicInfo.Pastel=True then begin
    for y:=0 to ScreenHeight-1 do begin
      pb1:=dstbm.ScanLine[y];
      pb2:=dstbm.ScanLine[ScreenHeight+ScreenPadding+y];
      for x:=0 to (ScreenWidth*3)-1 do begin
        c:=((255*3) div 4)+(pb1[x] div 4);
        if c<0 then c:=0;
        if 255<c then c:=255;
        pb1[x]:=c;
        c:=((255*3) div 4)+(pb2[x] div 4);
        if c<0 then c:=0;
        if 255<c then c:=255;
        pb2[x]:=c;
      end;
    end;
  end;
end;

procedure TMain.MainMenu_ExitClick(Sender: TObject);
begin
  Main.Close;
end;

procedure DrawToBitmap(dstbm:TBitmap);
var
  tmpbm:TBitmap;
begin
  tmpbm:=TBitmap.Create;
  MakeBlankBM(tmpbm,ScreenWidth,ScreenHeight+ScreenPadding+ScreenHeight,pf24bit);

  RedrawPreviewImage(tmpbm,FileSelMasterBM,FileSelPicInfo);
  BitBlt(dstbm.Canvas.Handle,0,0,ScreenWidth,ScreenHeight,tmpbm.Canvas.Handle,0,0,SRCCOPY);
  BitBlt(dstbm.Canvas.Handle,0,ScreenHeight,ScreenWidth,ScreenHeight,tmpbm.Canvas.Handle,0,ScreenHeight+ScreenPadding,SRCCOPY);

  RedrawPreviewImage(tmpbm,LunchMasterBM,LunchPicInfo);
  BitBlt(dstbm.Canvas.Handle,ScreenWidth,0,ScreenWidth,ScreenHeight,tmpbm.Canvas.Handle,0,0,SRCCOPY);
  BitBlt(dstbm.Canvas.Handle,ScreenWidth,ScreenHeight,ScreenWidth,ScreenHeight,tmpbm.Canvas.Handle,0,ScreenHeight+ScreenPadding,SRCCOPY);

  tmpbm.Free;
end;

procedure TMain.MainMenu_CreateClick(Sender: TObject);
var
  dstbm:TBitmap;
  wfn:string;
begin
  dstbm:=TBitmap.Create;
  MakeBlankBM(dstbm,ScreenWidth*2,ScreenHeight*2,pf24bit);
  DrawToBitmap(dstbm);

  wfn:=CreateBGBMP(dstbm,ExtractFilePath(OpenPicDlg.FileName),BGBMPType);

  dstbm.Free;

  ShowMessage('背景画像用ファイルを作成しました。 ['+BGBMPFilename+']'+CRLF+'ファームウェアと同じフォルダ（m3sakuraフォルダ）にコピーしてから起動して下さい。'+CRLF+CRLF+'作成したファイルの詳細な場所：'+CRLF+wfn);
end;

procedure TMain.MainMenu_AboutClick(Sender: TObject);
begin
  ShowMessage('MakeBGBMP.exe for M3Sakura');
end;

procedure TMain.FormCreate(Sender: TObject);
var
  fn:string;
begin
  Application.Title:='M3Sakura 背景画像コンバータ BetaTest.3';
  Main.Caption:=Application.Title;
  
  BGBMPType:=EBGBT_8bit;

  MakeBlankImg(FileSelImg,pf24bit);
  MakeBlankImg(LunchImg,pf24bit);

  if OpenPicDlg.Execute=False then begin
    Application.Terminate;
    exit;
  end;
  fn:=OpenPicDlg.FileName;
//  fn:='testbg.bmp';

  FileSelMasterBM:=TBitmap.Create;
  LoadImageFromFile(FileSelMasterBM,fn);
  LunchMasterBM:=TBitmap.Create;
  LoadImageFromFile(LunchMasterBM,fn);

  with FileSelPicInfo do begin
    PosX:=0;
    PosY:=0;
    Zoom:=1;
    AutoPadding:=True;
    Pastel:=True;
  end;

  with LunchPicInfo do begin
    PosX:=0;
    PosY:=0;
    Zoom:=1;
    AutoPadding:=True;
    Pastel:=False;
  end;

  RedrawPreviewImage(FileSelImg.Picture.Bitmap,FileSelMasterBM,FileSelPicInfo);
  FileSelImg.Refresh;
  RedrawPreviewImage(LunchImg.Picture.Bitmap,LunchMasterBM,LunchPicInfo);
  LunchImg.Refresh;
end;

procedure TMain.FileSel_AutoPaddingChkClick(Sender: TObject);
begin
  FileSelPicInfo.AutoPadding:=not FileSelPicInfo.AutoPadding;
  RedrawPreviewImage(FileSelImg.Picture.Bitmap,FileSelMasterBM,FileSelPicInfo);
  FileSelImg.Refresh;
end;

procedure TMain.Lunch_AutoPaddingChkClick(Sender: TObject);
begin
  LunchPicInfo.AutoPadding:=not LunchPicInfo.AutoPadding;
  RedrawPreviewImage(LunchImg.Picture.Bitmap,LunchMasterBM,LunchPicInfo);
  LunchImg.Refresh;
end;

procedure TMain.FileSel_PastelChkClick(Sender: TObject);
begin
  FileSelPicInfo.Pastel:=not FileSelPicInfo.Pastel;
  RedrawPreviewImage(FileSelImg.Picture.Bitmap,FileSelMasterBM,FileSelPicInfo);
  FileSelImg.Refresh;
end;

procedure TMain.Lunch_PastelChkClick(Sender: TObject);
begin
  LunchPicInfo.Pastel:=not LunchPicInfo.Pastel;
  RedrawPreviewImage(LunchImg.Picture.Bitmap,LunchMasterBM,LunchPicInfo);
  LunchImg.Refresh;
end;

var
  fmf:boolean;
  fmx,fmy:integer;
  lmf:boolean;
  lmx,lmy:integer;

procedure TMain.FileSelImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fmf:=True;
  fmx:=x;
  fmy:=y;
end;

procedure TMain.FileSelImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  sx,sy:integer;
begin
  if fmf=False then exit;

  sx:=fmx-x;
  sy:=fmy-y;
  if FileSelPicInfo.Zoom<1 then begin
    sx:=trunc(sx*(1/FileSelPicInfo.Zoom));
    sy:=trunc(sy*(1/FileSelPicInfo.Zoom));
  end;

  inc(FileSelPicInfo.PosX,sx);
  inc(FileSelPicInfo.PosY,sy);
  fmx:=x;
  fmy:=y;

  RedrawPreviewImage(FileSelImg.Picture.Bitmap,FileSelMasterBM,FileSelPicInfo);
  FileSelImg.Refresh;
end;

procedure TMain.FileSelImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fmf:=False;

  with FileSelPicInfo do begin
    if PosX<0 then PosX:=0;
    if PosY<0 then PosY:=0;
  end;
end;

procedure TMain.LunchImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  lmf:=True;
  lmx:=x;
  lmy:=y;
end;

procedure TMain.LunchImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  sx,sy:integer;
begin
  if lmf=False then exit;

  sx:=lmx-x;
  sy:=lmy-y;
  if LunchPicInfo.Zoom<1 then begin
    sx:=trunc(sx*(1/LunchPicInfo.Zoom));
    sy:=trunc(sy*(1/LunchPicInfo.Zoom));
  end;

  inc(LunchPicInfo.PosX,sx);
  inc(LunchPicInfo.PosY,sy);
  lmx:=x;
  lmy:=y;

  RedrawPreviewImage(LunchImg.Picture.Bitmap,LunchMasterBM,LunchPicInfo);
  LunchImg.Refresh;
end;

procedure TMain.LunchImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  lmf:=False;

  with LunchPicInfo do begin
    if PosX<0 then PosX:=0;
    if PosY<0 then PosY:=0;
  end;
end;

procedure TMain.FileList_ZoomBarChange(Sender: TObject);
begin
  FileSelZoomLbl.Caption:=format('%d%%',[FileList_ZoomBar.Position]);
  FileSelPicInfo.Zoom:=FileList_ZoomBar.Position/100;
  RedrawPreviewImage(FileSelImg.Picture.Bitmap,FileSelMasterBM,FileSelPicInfo);
  FileSelImg.Refresh;
end;

procedure TMain.Lunch_ZoomBarChange(Sender: TObject);
begin
  LunchZoomLbl.Caption:=format('%d%%',[Lunch_ZoomBar.Position]);
  LunchPicInfo.Zoom:=Lunch_ZoomBar.Position/100;
  RedrawPreviewImage(LunchImg.Picture.Bitmap,LunchMasterBM,LunchPicInfo);
  LunchImg.Refresh;
end;

procedure TMain.FileSelOpenBtnClick(Sender: TObject);
var
  fn:string;
begin
  if OpenPicDlg.Execute=False then begin
    Application.Terminate;
    exit;
  end;
  fn:=OpenPicDlg.FileName;

  LoadImageFromFile(FileSelMasterBM,fn);

  with FileSelPicInfo do begin
    PosX:=0;
    PosY:=0;
  end;

  RedrawPreviewImage(FileSelImg.Picture.Bitmap,FileSelMasterBM,FileSelPicInfo);
  FileSelImg.Refresh;
end;

procedure TMain.LunchOpenBtnClick(Sender: TObject);
var
  fn:string;
begin
  if OpenPicDlg.Execute=False then begin
    Application.Terminate;
    exit;
  end;
  fn:=OpenPicDlg.FileName;

  LoadImageFromFile(LunchMasterBM,fn);

  with LunchPicInfo do begin
    PosX:=0;
    PosY:=0;
  end;

  RedrawPreviewImage(LunchImg.Picture.Bitmap,LunchMasterBM,LunchPicInfo);
  LunchImg.Refresh;
end;

procedure TMain.MainMenu_Mode8bitClick(Sender: TObject);
begin
  BGBMPType:=EBGBT_8bit;

  MainMenu_Mode8bit.Checked:=True;
  MainMenu_Mode15bit.Checked:=False;
end;

procedure TMain.MainMenu_Mode15bitClick(Sender: TObject);
begin
  BGBMPType:=EBGBT_15bit;

  MainMenu_Mode8bit.Checked:=False;
  MainMenu_Mode15bit.Checked:=True;
end;

end.
