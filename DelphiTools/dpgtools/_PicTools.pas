// このユニットに含まれる関数は、インターフェース変更しないで下さい。

unit _PicTools;

interface

uses
  Windows,ExtCtrls,Graphics,Classes,Controls;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

type
  TWindowPos=record
    Left,Width:integer;
    Top,Height:integer;
  end;

type
  TRGBPack=packed record
    rgb:dword;
    b,g,r:byte;
  end;

type
  TRGB=packed record
    b,g,r:byte;
  end;
  PRGB=^TRGB;
  TRGBArray=array[0..65535*10] of TRGB;
  PRGBArray=^TRGBArray;

type
  TMono=byte;
  PMono=^TMono;
  TMonoArray=array[0..65535*10] of TMono;
  PMonoArray=^TMonoArray;

type
  TGray=byte;
  PGray=^TGray;
  TGrayArray=array[0..65535*10] of TGray;
  PGrayArray=^TGrayArray;

type
  TPal=byte;
  PPal=^TPal;
  TPalArray=array[0..65535*10] of TPal;
  PPalArray=^TPalArray;

procedure AdjustWindowSize(const mx,my:integer;var pos:TWindowPos);
function CheckWindowInside(var pos:TWindowPos;x,y:integer):boolean;

procedure MakeBlankImg(img:TImage;const pf:TPixelFormat);
procedure MakeBlankPaintBox(Paint:TPaintBox);
procedure MakeBlankBM(var bm:TBitmap;const x,y:integer;const pf:TPixelFormat);

function RGB2RGB(r,g,b:byte):TRGB;

function dword2RGB(dw:dword):TRGB;
function RGB2dword(var rgb:TRGB):dword;
function ColorAlpha(col1:dword;Alpha:integer):dword;
function RGBAlpha(var rgb1:TRGB;Alpha:integer):TRGB;
function ColorModulate(col1,col2:dword;Alpha:integer):dword;
function RGBModulate(var rgb1,rgb2:TRGB;Alpha:integer):TRGB;
procedure RGBModulateVar(var rgb1,rgb2:TRGB;Alpha:integer);
function AlphaModulate(Alpha1,Alpha2:dword):dword;
function AlphaModulateLiner(Alpha1,Alpha2:dword;blend:integer):dword;
function RGBItoRGB(rgbi:word):dword;
function RGBtoRGBI(rgb:dword):word;
procedure SetRGBPack(var RGBPack:TRGBPack;const _rgb:dword);overload;
procedure SetRGBPack(var RGBPack:TRGBPack;const _r,_g,_b:byte);overload;

procedure CreateGrayscalePalette(bm:TBitmap);
procedure FillAlphaBMPos(bm:TBitmap;var pos:TWindowPos; var rgb:TRGB;Alpha:byte);
procedure FillAlphaBM(bm:TBitmap;var rgb:TRGB;Alpha:byte);
procedure DrawFlameBitmap(bm:TBitmap;var Bright,Mask:TRGB);
procedure DrawFlameCanvas(Canvas:TCanvas;Width,Height:integer;var Bright,Mask:TRGB);

implementation

uses _const,_SimpleDialog;

procedure AdjustWindowSize(const mx,my:integer;var pos:TWindowPos);
begin
  with pos do begin
    if Width>mx then Width:=mx;
    if Height>(my-24) then Height:=my-24;

    if (Left+Width)>mx then
      Left:=mx-Width;
    if Left<0 then
      Left:=0;

    if (Top+Height)>(my-24) then
      Top:=(my-24)-Height;
    if Top<0 then
      Top:=0;
  end;
end;

function CheckWindowInside(var pos:TWindowPos;x,y:integer):boolean;
begin
  with pos do begin
    if ((Left<=x) and (x<=(Left+Width)) and (Top<=y) and (y<=(Top+Height))) then begin
      Result:=True;
      end else begin
      Result:=False;
    end;
  end;
end;

procedure MakeBlankImg(img:TImage;const pf:TPixelFormat);
begin
  with img do begin
    Picture.Graphic:=Picture.Bitmap;
    Picture.Bitmap.PixelFormat:=pf;
    Picture.Bitmap.Width:=Width;
    Picture.Bitmap.Height:=Height;
    with Canvas do begin
      Brush.Color:=$000000;
      FillRect(Rect(0,0,Width,Height));
    end;
    ControlStyle:=ControlStyle+[csOpaque];
  end;
end;

procedure MakeBlankPaintBox(Paint:TPaintBox);
begin
  with Paint do begin
    Width:=Width;
    Height:=Height;
    with Canvas do begin
      Brush.Color:=$000000;
      FillRect(Rect(0,0,Width,Height));
    end;
    ControlStyle:=ControlStyle+[csOpaque];
  end;
end;

procedure MakeBlankBM(var bm:TBitmap;const x,y:integer;const pf:TPixelFormat);
begin
  if bm=nil then bm:=TBitmap.Create;
  with bm do begin
    Width:=0;
    Height:=0;
    PixelFormat:=pf;
    Width:=x;
    Height:=y;
    with Canvas do begin
      Brush.Style:=bsSolid;
      Brush.Color:=$000000;
      FillRect(Rect(0,0,x,y));
    end;
  end;
end;

function RGB2RGB(r,g,b:byte):TRGB;
begin
  Result.b:=b;
  Result.g:=g;
  Result.r:=r;
end;

function dword2RGB(dw:dword):TRGB;
begin
  Result.b:=byte(dw shr 16);
  Result.g:=byte(dw shr 8);
  Result.r:=byte(dw);
end;

function RGB2dword(var rgb:TRGB):dword;
begin
  Result:=(rgb.r or (rgb.g shl 8) or (rgb.b shl 16));
end;

function ColorAlpha(col1:dword;Alpha:integer):dword;
var
  rgb0,rgb1:TRGB;
begin
  rgb1:=dword2rgb(col1);
  rgb0:=RGBAlpha(rgb1,Alpha);
  Result:=rgb2dword(rgb0);
end;

function RGBAlpha(var rgb1:TRGB;Alpha:integer):TRGB;
begin
  case Alpha of
    $00: begin
      Result.b:=$00;
      Result.g:=$00;
      Result.r:=$00;
    end;
    $80: begin
      Result.b:=rgb1.b div 2;
      Result.g:=rgb1.g div 2;
      Result.r:=rgb1.r div 2;
    end;
    $ff: Result:=rgb1;
    else begin
      Result.b:=(rgb1.b*Alpha) div $ff;
      Result.g:=(rgb1.g*Alpha) div $ff;
      Result.r:=(rgb1.r*Alpha) div $ff;
    end;
  end;
end;

function ColorModulate(col1,col2:dword;Alpha:integer):dword;
var
  rgb0,rgb1,rgb2:TRGB;
begin
  rgb1:=dword2rgb(col1);
  rgb2:=dword2rgb(col2);
  rgb0:=RGBModulate(rgb1,rgb2,Alpha);
  Result:=rgb2dword(rgb0);
end;

function RGBModulate(var rgb1,rgb2:TRGB;Alpha:integer):TRGB;
var
  iAlpha:integer;
begin
  iAlpha:=$ff-Alpha;

  case Alpha of
    $00: Result:=rgb1;
    $80: begin
      Result.b:=(rgb1.b div 2)+(rgb2.b div 2);
      Result.g:=(rgb1.g div 2)+(rgb2.g div 2);
      Result.r:=(rgb1.r div 2)+(rgb2.r div 2);
    end;
    $ff: Result:=rgb2;
    else begin
      Result.b:=((rgb1.b*iAlpha)+(rgb2.b*Alpha)) div $ff;
      Result.g:=((rgb1.g*iAlpha)+(rgb2.g*Alpha)) div $ff;
      Result.r:=((rgb1.r*iAlpha)+(rgb2.r*Alpha)) div $ff;
    end;
  end;
end;

procedure RGBModulateVar(var rgb1,rgb2:TRGB;Alpha:integer);
var
  iAlpha:integer;
begin
  iAlpha:=$ff-Alpha;

  case Alpha of
    $00: begin end;
    $80: begin
      with rgb1 do begin
        b:=(b div 2)+(rgb2.b div 2);
        g:=(g div 2)+(rgb2.g div 2);
        r:=(r div 2)+(rgb2.r div 2);
      end;
    end;
    $ff: rgb1:=rgb2;
    else begin
      with rgb1 do begin
        b:=((b*iAlpha)+(rgb2.b*Alpha)) div $ff;
        g:=((g*iAlpha)+(rgb2.g*Alpha)) div $ff;
        r:=((r*iAlpha)+(rgb2.r*Alpha)) div $ff;
      end;
    end;
  end;
end;

function AlphaModulateLiner(Alpha1,Alpha2:dword;blend:integer):dword;
begin
  Result:=((Alpha1*dword($ff-blend))+(Alpha2*dword(blend))) div $ff;
end;

function AlphaModulate(Alpha1,Alpha2:dword):dword;
begin
  Result:=(Alpha1*Alpha2) div $ff;
end;

function RGBItoRGB(rgbi:word):dword;
var
  r,g,b,i:byte;
begin
  r:=0;
  g:=0;
  b:=0;
  i:=0;

  if (rgbi and (1 shl  0))<>0 then i:=1;
  if (rgbi and (1 shl  1))<>0 then inc(b,1 shl 0);
  if (rgbi and (1 shl  2))<>0 then inc(b,1 shl 1);
  if (rgbi and (1 shl  3))<>0 then inc(b,1 shl 2);
  if (rgbi and (1 shl  4))<>0 then inc(b,1 shl 3);
  if (rgbi and (1 shl  5))<>0 then inc(b,1 shl 4);
  if (rgbi and (1 shl  6))<>0 then inc(r,1 shl 0);
  if (rgbi and (1 shl  7))<>0 then inc(r,1 shl 1);
  if (rgbi and (1 shl  8))<>0 then inc(r,1 shl 2);
  if (rgbi and (1 shl  9))<>0 then inc(r,1 shl 3);
  if (rgbi and (1 shl 10))<>0 then inc(r,1 shl 4);
  if (rgbi and (1 shl 11))<>0 then inc(g,1 shl 0);
  if (rgbi and (1 shl 12))<>0 then inc(g,1 shl 1);
  if (rgbi and (1 shl 13))<>0 then inc(g,1 shl 2);
  if (rgbi and (1 shl 14))<>0 then inc(g,1 shl 3);
  if (rgbi and (1 shl 15))<>0 then inc(g,1 shl 4);

  if i=0 then begin
    r:=(r shl 3);
    g:=(g shl 3);
    b:=(b shl 3);
    end else begin
    r:=(r shl 3)+4;
    g:=(g shl 3)+4;
    b:=(b shl 3)+4;
  end;

  Result:=(b shl 16)+(g shl 8)+r;
end;

function RGBtoRGBI(rgb:dword):word;
begin
  Result:=0;
end;

procedure SetRGBPack(var RGBPack:TRGBPack;const _rgb:dword);overload;
begin
  with RGBPack do begin
    rgb:=_rgb;
    r:=rgb and $ff;
    g:=(rgb shr 8) and $ff;
    b:=(rgb shr 16) and $ff;
  end;
end;

procedure SetRGBPack(var RGBPack:TRGBPack;const _r,_g,_b:byte);overload;
begin
  with RGBPack do begin
    rgb:=dword(_b)*$10000+dword(_g)*$100+dword(_r);
    r:=_r;
    g:=_g;
    b:=_b;
  end;
end;

procedure CreateGrayscalePalette(bm:TBitmap);
var
  cnt:integer;
  LogPalette:TMaxLogPalette;
begin
  FillChar(LogPalette,SizeOf(LogPalette),0);
  LogPalette.palVersion:=$300;

  LogPalette.palNumEntries:=$100;
  for cnt:=$00 to $ff do begin
    with LogPalette.palPalEntry[cnt] do begin
      peBlue:=cnt;
      peGreen:=cnt;
      peRed:=cnt;
    end;
  end;
  bm.Palette:=CreatePalette(PLogPalette(@LogPalette)^);
end;

procedure FillAlphaBMPos(bm:TBitmap;var pos:TWindowPos; var rgb:TRGB;Alpha:byte);
var
  x,y:integer;
  pBM:PRGB;
begin
  if pos.Width=0 then exit;
  if pos.Height=0 then exit;

  case Alpha of
    $00: begin
    end;
    $ff: begin
      with pos do begin
        for y:=0 to Height-1 do begin
          pBM:=bm.ScanLine[Top+y];
          inc(pBM,Left);
          for x:=0 to Width-1 do begin
            pBM^:=rgb;
            inc(pBM);
          end;
        end;
      end;
    end;
    else begin
      with pos do begin
        for y:=0 to Height-1 do begin
          pBM:=bm.ScanLine[Top+y];
          inc(pBM,Left);
          for x:=0 to Width-1 do begin
            RGBModulateVar(pBM^,rgb,Alpha);
            inc(pBM);
          end;
        end;
      end;
    end;
  end;
end;

procedure FillAlphaBM(bm:TBitmap;var rgb:TRGB;Alpha:byte);
var
  pos:TWindowPos;
begin
  pos.Left:=0;
  pos.Top:=0;
  pos.Width:=bm.Width;
  pos.Height:=bm.Height;
  FillAlphaBMPos(bm,pos,rgb,Alpha);
end;

procedure DrawFlameBitmap(bm:TBitmap;var Bright,Mask:TRGB);
begin
  DrawFlameCanvas(bm.Canvas,bm.Width,bm.Height,Bright,Mask);
end;

procedure DrawFlameCanvas(Canvas:TCanvas;Width,Height:integer;var Bright,Mask:TRGB);
var
  x,y:integer;
begin
  with Canvas do begin
    Pen.Mode:=pmCopy;
    Pen.Style:=psSolid;

    Pen.Color:=rgb2dword(Mask);
    x:=Width-2;
    y:=Height-2;
    MoveTo(0,0);
    LineTo(x,0);
    LineTo(x,y);
    LineTo(0,y);
    LineTo(0,0);
    Pen.Color:=rgb2dword(Bright);
    x:=Width-1;
    y:=Height-1;
    MoveTo(1,1);
    LineTo(x,1);
    LineTo(x,y);
    LineTo(1,y);
    LineTo(1,1);
  end;
end;

end.

