unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,_PicTools, StdCtrls, Menus;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    TopImg: TImage;
    BtmImg: TImage;
    IntervalTimer: TTimer;
    Label2: TLabel;
    Label1: TLabel;
    ThumbImg: TImage;
    Label3: TLabel;
    ThumbTextLbl: TLabel;
    Label4: TLabel;
    Pen16chk: TRadioButton;
    Pen8chk: TRadioButton;
    Pen4chk: TRadioButton;
    Label5: TLabel;
    SmoothLineChk: TCheckBox;
    PenStarChk: TRadioButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure BtmImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtmImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BtmImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IntervalTimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  fnstr:string;

var
  mf:boolean;
  mx,my:integer;

var
  EraseMode:boolean;

var
  curbm:TBitmap;

type
  TMouseRec=record
    x,y:integer;
  end;

var
  MouseRec:array of TMouseRec;
  MouseRecCount:integer;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fnstr:='Today '+formatdatetime('hh:nn',now);

  MakeBlankImg(TopImg,pf8bit);
  CreateGrayscalePalette(TopImg.Picture.Bitmap);
  with TopImg.Canvas do begin
    Brush.Color:=$ffffff;
    FillRect(Rect(0,0,256,192));
  end;

  MakeBlankImg(BtmImg,pf8bit);
  CreateGrayscalePalette(BtmImg.Picture.Bitmap);
  with BtmImg.Canvas do begin
    Brush.Color:=$ffffff;
    FillRect(Rect(0,0,256,192));
    Pen.Color:=$000000;
  end;

  CurBM:=TBitmap.Create;
  MakeBlankBM(CurBM,256,192,pf8bit);
  CreateGrayscalePalette(CurBM);
  with CurBM.Canvas do begin
    Brush.Color:=$ffffff;
    FillRect(Rect(0,0,256,192));
    Pen.Color:=$000000;
  end;

  mf:=False;

  MouseRecCount:=0;
  setlength(MouseRec,MouseRecCount);

  EraseMode:=False;
end;

procedure TForm1.BtmImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mf:=True;
  mx:=x;
  my:=y;

  MouseRecCount:=0;
  setlength(MouseRec,MouseRecCount);
end;

procedure TForm1.BtmImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if mf=False then exit;

  mx:=x;
  my:=y;
end;

const pen16bm:array[0..16-1,0..16-1] of byte=(
(  $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00),
(  $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00),
(  $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00),
(  $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00),
(  $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00),
(  $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00)
);

const pen8bm:array[0..16-1,0..16-1] of byte=(
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $00, $00)
);

const pen4bm:array[0..16-1,0..16-1] of byte=(
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $00),
(  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $00)
);

const penstarbm:array[0..16-1,0..16-1] of byte=(
(  $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $00, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00, $00),
(  $00, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $00),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff),
(  $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00),
(  $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00),
(  $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00),
(  $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00),
(  $00, $00, $00, $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00, $00, $00, $00),
(  $00, $00, $00, $ff, $ff, $ff, $ff, $00, $00, $ff, $ff, $ff, $ff, $00, $00, $00),
(  $00, $00, $ff, $ff, $ff, $ff, $00, $00, $00, $00, $ff, $ff, $ff, $ff, $00, $00),
(  $00, $00, $ff, $ff, $ff, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $00, $00),
(  $00, $ff, $ff, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $ff, $ff, $00)
);

procedure TForm1.BtmImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  penbm:array[0..16-1,0..16-1] of byte;
  linebm:TBitmap;
  tmpbm:array[0..768-1,0..1024-1] of boolean;
  pbsrc,pbdst:PByteArray;
  srcalpha,dstalpha:integer;
  sx,sy:integer;
  col:dword;
  dx,dy:integer;
  tmpdepth:integer;
  procedure drawline_polyline;
  var
    idx,cnt:integer;
    fidx,fra:double;
    depth:integer;
    ax,ay:array of integer;
    ofs:double;
    lev:integer;
    tx,ty:double;
    x0,y0,x1,y1:integer;
    x,y:double;
  begin
    cnt:=MouseRecCount;
    setlength(ax,cnt);
    setlength(ay,cnt);
    linebm.Canvas.MoveTo(MouseRec[0].x*4,MouseRec[0].y*4);
    for idx:=0 to cnt*1-1 do begin
      fidx:=idx/1;
      lev:=0;
      tx:=0;
      ty:=0;
      tmpdepth:=0;
      for depth:=0 to 0 do begin
        ofs:=fidx+depth;
        if ofs<0 then ofs:=0;
        if (cnt-2)<ofs then ofs:=cnt-2;
        x0:=MouseRec[trunc(ofs)+0].x;
        y0:=MouseRec[trunc(ofs)+0].y;
        x1:=MouseRec[trunc(ofs)+1].x;
        y1:=MouseRec[trunc(ofs)+1].y;
        fra:=frac(ofs);
        x:=(x0*(1-fra))+(x1*fra);
        y:=(y0*(1-fra))+(y1*fra);
        tmpdepth:=1-abs(depth);
        tmpdepth:=tmpdepth*tmpdepth;
        tx:=tx+(x*tmpdepth);
        ty:=ty+(y*tmpdepth);
        inc(lev,tmpdepth);
      end;
      tx:=tx/lev;
      ty:=ty/lev;
      linebm.Canvas.LineTo(trunc(tx*4),trunc(ty*4));
    end;
  end;
  procedure drawline_smooth;
  var
    idx,cnt:integer;
    fidx,fra:double;
    depth:integer;
    ax,ay:array of integer;
    ofs:double;
    lev:integer;
    tx,ty:double;
    x0,y0,x1,y1:integer;
    x,y:double;
    lx,ly:double;
  begin
    cnt:=MouseRecCount;
    setlength(ax,cnt);
    setlength(ay,cnt);
    linebm.Canvas.MoveTo(MouseRec[0].x*4,MouseRec[0].y*4);
    lx:=0;ly:=0;
    for idx:=0 to cnt*16-1 do begin
      fidx:=idx/16;
      lev:=0;
      tx:=0;
      ty:=0;
      for depth:=-1 to 1 do begin
        ofs:=fidx+depth;
        if ofs<0 then ofs:=0;
        if (cnt-2)<ofs then ofs:=cnt-2;
        x0:=MouseRec[trunc(ofs)+0].x;
        y0:=MouseRec[trunc(ofs)+0].y;
        x1:=MouseRec[trunc(ofs)+1].x;
        y1:=MouseRec[trunc(ofs)+1].y;
        fra:=frac(ofs);
        x:=(x0*(1-fra))+(x1*(fra));
        y:=(y0*(1-fra))+(y1*(fra));
        tmpdepth:=2-abs(depth);
        tmpdepth:=tmpdepth*0+1;
        tx:=tx+(x*tmpdepth);
        ty:=ty+(y*tmpdepth);
        inc(lev,tmpdepth);
      end;
      tx:=tx/lev;
      ty:=ty/lev;
      linebm.Canvas.LineTo(trunc(tx*4),trunc(ty*4));
      listbox1.Items.Add(format('%f %.5f %.5f',[idx/16,tx-lx,ty-ly]));
      lx:=tx;
      ly:=ty;
    end;
  end;
  procedure DrawBoundBox;
  var
    pb:PByteArray;
    x,y:integer;
    x0,y0,x1,y1:integer;
    thumbbm:TBitmap;
    sw,sh:integer;
    dx,dy,dw,dh:integer;
    ratio:double;
    fir:double;
    ir:integer;
    pbdst,pbsrc:pbytearray;
    cx,cy:integer;
    c:byte;
  begin
    x0:=256;
    y0:=192;
    x1:=0;
    y1:=0;
    for y:=0 to 192-1 do begin
      pb:=TopImg.Picture.Bitmap.ScanLine[y];
      for x:=0 to 256-1 do begin
        if pb[x]<>$ff then begin
          if x<x0 then x0:=x;
          if y<y0 then y0:=y;
          if x1<x then x1:=x;
          if y1<y then y1:=y;
        end;
      end;
    end;

    dec(x0,4);
    if x0<0 then x0:=0;
    dec(y0,4);
    if y0<0 then y0:=0;
    inc(x1,4);
    if 256<x1 then x1:=256;
    inc(y1,4);
    if 192<y1 then y1:=192;

    sw:=x1-x0;
    sh:=y1-y0;

    if (sw<=0) or (sh<=0) then exit;

    dw:=64;
    dh:=48;
    dx:=8;
    dy:=118;

    with ThumbImg.Canvas do begin
      Brush.Color:=$808080;
      FillRect(Bounds(dx,dy,dw,dh));
    end;

    if (dh/sh)<(dw/sw) then begin
      ratio:=dh/sh;
      dw:=trunc(sw*ratio);
      end else begin
      ratio:=dw/sw;
      dh:=trunc(sh*ratio);
    end;

    inc(dx,(64-dw) div 2);
    inc(dy,(48-dh) div 2);

    thumbbm:=TBitmap.Create;
    MakeBlankBM(thumbbm,dw,dh,pf8bit);

    fir:=1/ratio;
    if fir<1 then fir:=1;
    ir:=trunc(fir);

    for y:=0 to dh-1 do begin
      pbdst:=ThumbImg.Picture.Bitmap.ScanLine[dy+y];
      for x:=0 to dw-1 do begin
        c:=$ff;
        for cy:=0 to ir-1 do begin
          pbsrc:=TopImg.Picture.Bitmap.ScanLine[y0+trunc(y*fir)+cy];
          for cx:=0 to ir-1 do begin
            if pbsrc[x0+trunc(x*fir)+cx]<>$ff then c:=$00;
          end;
        end;
        pbdst[(dx+x)*3+0]:=c;
        pbdst[(dx+x)*3+1]:=c;
        pbdst[(dx+x)*3+2]:=c;
      end;
    end;

    with ThumbImg.Canvas do begin
      Font:=Form1.Font;
      Font.Color:=$ffffff;
      Brush.Style:=bsClear;
      TextOut(8,166,fnstr);
    end;

    ThumbImg.Refresh;

    with TopImg.Canvas do begin
      Pen.Color:=$d0d0d0;
      MoveTo(x0,y0);
      LineTo(x1,y0);
      LineTo(x1,y1);
      LineTo(x0,y1);
      LineTo(x0,y0);
    end;
  end;
begin
  mf:=False;

  if EraseMode=True then begin
    BitBlt(TopImg.Canvas.Handle,0,0,256,192,CurBM.Canvas.Handle,0,0,SRCCOPY);
    DrawBoundBox;

    TopImg.Refresh;
    
    exit;
  end;

  linebm:=TBitmap.Create;
  MakeBlankBM(linebm,1024,768,pf8bit);
  CreateGrayscalePalette(linebm);

  with linebm.Canvas do begin
    Brush.Color:=$ffffff;
    FillRect(Rect(0,0,1024,768));
    Pen.Color:=$000000;
  end;

  if SmoothLineChk.Checked=False then begin
    drawline_polyline;
    end else begin
    drawline_smooth;
  end;

  for y:=0 to 768-1 do begin
    for x:=0 to 1024-1 do begin
      tmpbm[y,x]:=False;
    end;
  end;

  if Form1.Pen16chk.Checked=True then begin
    for sy:=0 to 16-1 do begin
      for sx:=0 to 16-1 do begin
        penbm[sy,sx]:=pen16bm[sy,sx];
      end;
    end;
  end;

  if Form1.Pen8chk.Checked=True then begin
    for sy:=0 to 16-1 do begin
      for sx:=0 to 16-1 do begin
        penbm[sy,sx]:=pen8bm[sy,sx];
      end;
    end;
  end;

  if Form1.Pen4chk.Checked=True then begin
    for sy:=0 to 16-1 do begin
      for sx:=0 to 16-1 do begin
        penbm[sy,sx]:=pen4bm[sy,sx];
      end;
    end;
  end;

  if Form1.PenStarchk.Checked=True then begin
    for sy:=0 to 16-1 do begin
      for sx:=0 to 16-1 do begin
        penbm[sy,sx]:=penstarbm[sy,sx];
      end;
    end;
  end;

  for sy:=16 to 768-1 do begin
    pbsrc:=linebm.ScanLine[sy];
    for sx:=16 to 1024-1 do begin
      col:=pbsrc[sx];
      if col<>$ff then begin
        for dy:=0 to 16-1 do begin
          for dx:=0 to 16-1 do begin
            if penbm[dy,dx]=$ff then tmpbm[sy-16+dy,sx-16+dx]:=True;
          end;
        end;
      end;
    end;
  end;

  linebm.Free;

  for sy:=0 to 192-1 do begin
    pbdst:=CurBM.ScanLine[sy];
    for sx:=0 to 256-1 do begin
      srcalpha:=0;
      for dy:=0 to 4-1 do begin
        for dx:=0 to 4-1 do begin
          if tmpbm[sy*4+dy,sx*4+dx]=True then inc(srcalpha);
        end;
      end;

      if srcalpha<>0 then begin
        dstalpha:=($100 div 16)*srcalpha;
        col:=pbdst[sx];
        col:=(col*($100-dstalpha)) div $100;
        pbdst[sx]:=col;
      end;
    end;
  end;

  BitBlt(TopImg.Canvas.Handle,0,0,256,192,CurBM.Canvas.Handle,0,0,SRCCOPY);
  DrawBoundBox;

  TopImg.Refresh;

  BitBlt(BtmImg.Canvas.Handle,0,0,256,192,CurBM.Canvas.Handle,0,0,SRCCOPY);
  BtmImg.Refresh;
end;

procedure TForm1.IntervalTimerTimer(Sender: TObject);
begin
  if mf=True then begin
    if EraseMode=False then begin
      if 0<MouseRecCount then begin
        with BtmImg.Canvas do begin
          with MouseRec[MouseRecCount-1] do begin
            MoveTo(x,y);
          end;
          LineTo(mx,my);
        end;
      end;
      setlength(MouseRec,MouseRecCount+1);
      MouseRec[MouseRecCount].x:=mx;
      MouseRec[MouseRecCount].y:=my;
      inc(MouseRecCount);
      end else begin
      with BtmImg.Canvas do begin
        Brush.Color:=$ffffff;
        FillRect(Bounds(mx-8,my-8,16,16));
      end;
      with TopImg.Canvas do begin
        Brush.Color:=$ffffff;
        FillRect(Bounds(mx-8,my-8,16,16));
      end;
      with CurBM.Canvas do begin
        Brush.Color:=$ffffff;
        FillRect(Bounds(mx-8,my-8,16,16));
      end;
    end;
  end;

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=word('X') then EraseMode:=True;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=word('X') then EraseMode:=False;
end;

end.
