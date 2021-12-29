unit BGBMP;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, _PicTools;

const BGBMPFilename='BGBMP.DAT';

type EBGBMPType=(EBGBT_None=0,EBGBT_8bit=1,EBGBT_15bit=2);

function CreateBGBMP(var bm:TBitmap;writepath:string;BGBMPType:EBGBMPType):string;

implementation

uses NkDIB,MedianCut,Octree;

const DitherTable4Data8:array[0..(4*4)-1] of integer=(0,4,1,5, 6,2,7,3, 1,5,0,4, 7,3,6,2);

procedure DitherTable4(var bm:TBitmap);
var
  x,y,w,h:integer;
  psbm:PByteArray;
  procedure RGB8to5(var c:byte);
  var
    sx,sy:integer;
    ic:integer;
  begin
    sx:=x and 3;
    sy:=y and 3;
    ic:=c+DitherTable4Data8[sx*4+sy];
    if $ff<ic then ic:=$ff;
//    c:=ic and not $7;
    c:=ic;
  end;
begin
  w:=bm.Width;
  h:=bm.Height;

  for y:=0 to h-1 do begin
    psbm:=bm.ScanLine[y];
    for x:=0 to w-1 do begin
      RGB8to5(psbm[x*3+0]);
      RGB8to5(psbm[x*3+1]);
      RGB8to5(psbm[x*3+2]);
    end;
  end;
end;

procedure Palette8to5(var bm:TBitmap);
var
  idx:integer;
  nkbm:TnkDIB;
begin
  nkbm:=TNkDIB.Create;
  nkbm.Assign(bm);
  for idx:=0 to nkbm.PaletteSize-1-1 do begin
    nkbm.Colors[idx]:=(nkbm.Colors[idx] and $f8f8f8) shr 3;
  end;
  bm.Free;
  bm:=TBitmap.Create;
  bm.Assign(nkbm);
  nkbm.Free;
end;

procedure BitmapReduce256colors_DHGL(var bm:TBitmap);
var
  tmpbm:TBitmap;
begin
  DitherTable4(bm);

  tmpbm:=ReduceColorsByMedianCutV(bm,8);
  bm.Free;
  bm:=tmpbm;

//  Palette8to5(bm);
end;

procedure BitmapReduce15bit(var bm:TBitmap);
var
  x,y,w,h:integer;
  gr,gg,gb:integer;
  psbm:PByteArray;
  procedure RGB8to5(var c:byte;var g:integer);
  var
    ic:integer;
  begin
    ic:=c+g;
    if $ff<ic then ic:=$ff;
    g:=ic and $7;
    c:=ic and not $7;
  end;
begin
  w:=bm.Width;
  h:=bm.Height;

  gr:=0;
  gg:=0;
  gb:=0;

  for y:=0 to h-1 do begin
    psbm:=bm.ScanLine[y];
    for x:=0 to w-1 do begin
      RGB8to5(psbm[x*3+0],gr);
      RGB8to5(psbm[x*3+1],gg);
      RGB8to5(psbm[x*3+2],gb);
    end;
  end;
end;

function CreateBGBMP(var bm:TBitmap;writepath:string;BGBMPType:EBGBMPType):string;
var
  w,h:integer;
  wbuf:array of word;
  wfn:string;
  wfs:TFileStream;
  x,y:integer;
  tmp32:dword;
  pbuf24,pbuf8:PByteArray;
  r,g,b,col15:dword;
  rgb:dword;
  pal15:array[0..256-1] of word;
  nkbm:TNkDIB;
  idx:integer;
begin
  w:=256*2;
  h:=192*2;

  wfn:=writepath+BGBMPFilename;
  wfs:=TFileStream.Create(wfn,fmCreate);

  tmp32:=dword(BGBMPType);
  wfs.WriteBuffer(tmp32,4);

  case BGBMPType of
    EBGBT_None: begin
    end;
    EBGBT_8bit: begin
      BitmapReduce256colors_DHGL(bm);

      nkbm:=TNkDIB.Create;
      nkbm.Assign(bm);

      for idx:=0 to 256-1 do begin
        rgb:=nkbm.Colors[idx];
        r:=((rgb shr 16) and $ff) shr 3;
        g:=((rgb shr 8) and $ff) shr 3;
        b:=((rgb shr 0) and $ff) shr 3;
        pal15[idx]:=(r shl 10) or (g shl 5) or (b shl 0) or (1 shl 15);
      end;
      wfs.WriteBuffer(pal15[0],256*2);

      for y:=0 to h-1 do begin
        pbuf8:=bm.ScanLine[y];
        wfs.WriteBuffer(pbuf8[0],w);
      end;
    end;
    EBGBT_15bit: begin
      BitmapReduce15bit(bm);

      setlength(wbuf,w);
      for y:=0 to h-1 do begin
        pbuf24:=bm.ScanLine[y];
        for x:=0 to w-1 do begin
          r:=pbuf24[x*3+0] shr 3;
          g:=pbuf24[x*3+1] shr 3;
          b:=pbuf24[x*3+2] shr 3;
          wbuf[x]:=(r shl 10) or (g shl 5) or (b shl 0) or (1 shl 15);
        end;
        wfs.WriteBuffer(wbuf[0],w*2);
      end;
    end;
  end;

  wfs.Free;

  Result:=wfn;
end;

end.
