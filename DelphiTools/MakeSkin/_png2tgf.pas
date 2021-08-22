unit _png2tgf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, FileCtrl,GLDPNG, _PicTools,MainWin;

function png2tgf(var pFile:TFile;path:string;CheckWidth,CheckHeight:integer):boolean;

implementation

var
  sw,sh:integer;

procedure LoadPNG(fn:string;var bm,abm:TBitmap);
var
  png:TGLDPNG;
begin
  png:=TGLDPNG.Create;

  png.Image:=bm;
  png.AlphaBitmap:=abm;
  png.LoadFromFile(fn);

  png.Free;
end;

procedure SaveTGF(var pFile:TFile;var cbm,abm:TBitmap);
var
  x,y:integer;
  len:integer;
  pcbm,pabm:PByteArray;
  alpha:byte;
  cnt:integer;
  procedure Write8(d:byte);
  begin
    pFile.DecompData[pFile.DecompSize+0]:=(d shr 0) and $ff;
    inc(pFile.DecompSize,1);
  end;
  procedure Write16(d:word);
  begin
    pFile.DecompData[pFile.DecompSize+0]:=(d shr 0) and $ff;
    pFile.DecompData[pFile.DecompSize+1]:=(d shr 8) and $ff;
    inc(pFile.DecompSize,2);
  end;
  procedure Write32(d:dword);
  begin
    pFile.DecompData[pFile.DecompSize+0]:=(d shr 0) and $ff;
    pFile.DecompData[pFile.DecompSize+1]:=(d shr 8) and $ff;
    pFile.DecompData[pFile.DecompSize+2]:=(d shr 16) and $ff;
    pFile.DecompData[pFile.DecompSize+3]:=(d shr 24) and $ff;
    inc(pFile.DecompSize,4);
  end;
  procedure Write15(pbm:PByteArray;alpha:byte;x:integer);
  var
    r,g,b:integer;
  begin
    alpha:=31-alpha;
    r:=pbm[x*3+2];
    r:=(r*alpha) div 31;
    g:=pbm[x*3+1];
    g:=(g*alpha) div 31;
    b:=pbm[x*3+0];
    b:=(b*alpha) div 31;
    Write16((b shl 10)+(g shl 5)+(r shl 0)+(1 shl 15));
  end;
  function getlength(pbm:PByteArray;alpha:byte;x:integer):integer;
  begin
    Result:=0;
    while(alpha=pbm[x]) do begin
      inc(Result);
      inc(x);
      if x=sw then exit;
      if Result=$ff then exit;
    end;
  end;
begin
  setlength(pFile.DecompData,(2*2)+(sw*sh*2*2));
  pFile.DecompSize:=0;

  Write16(sw);
  Write16(sh);

  for y:=0 to sh-1 do begin
    pcbm:=cbm.ScanLine[y];
    pabm:=abm.ScanLine[y];
    x:=0;
    while(x<sw) do begin
      alpha:=pabm[x];
      len:=getlength(pabm,alpha,x);
      Write8(alpha);
      Write8(len);
      if alpha<>31 then begin
        for cnt:=x to x+len-1 do begin
          Write15(pcbm,alpha,cnt);
        end;
      end;
      inc(x,len);
    end;
  end;

  setlength(pFile.DecompData,pFile.DecompSize);
  
  while((pFile.DecompSize and 3)<>0) do begin
    setlength(pFile.DecompData,pFile.DecompSize+1);
    pFile.DecompData[pFile.DecompSize]:=0;
    inc(pFile.DecompSize);
  end;
end;

procedure conv8to5(var bm:TBitmap);
var
  x,y,w:integer;
  pbm:PByteArray;
begin
  w:=bm.Width;
  if bm.PixelFormat=pf24bit then w:=w*3;

  for y:=0 to bm.Height-1 do begin
    pbm:=bm.ScanLine[y];
    for x:=0 to w-1 do begin
      pbm[x]:=pbm[x] shr 3;
    end;
  end;
end;

function png2tgf(var pFile:TFile;path:string;CheckWidth,CheckHeight:integer):boolean;
var
  pngcbm,pngabm:TBitmap;
begin
  pngcbm:=TBitmap.Create;
  pngabm:=TBitmap.Create;

  LoadPNG(path+'\'+pFile.Filename,pngcbm,pngabm);

  sw:=pngcbm.Width;
  sh:=pngcbm.Height;

  if (sw<>CheckWidth) or (sh<>CheckHeight) then begin
    pngcbm.Free;
    pngabm.Free;
    Result:=False;
    exit;
  end;

  if (assigned(pngabm)=False) or (pngabm.Width<>sw) or (pngabm.Height<>sh) then begin
    MakeBlankBM(pngabm,sw,sh,pf24bit);
    pngabm.Canvas.Brush.Color:=0;
    pngabm.Canvas.FillRect(Rect(0,0,sw,sh));
  end;

  conv8to5(pngcbm);
  conv8to5(pngabm);

  SaveTGF(pFile,pngcbm,pngabm);

  pngcbm.Free;
  pngabm.Free;

  Result:=True;
end;

end.
