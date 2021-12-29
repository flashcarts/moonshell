unit _bmp2b15;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,GLDPNG, ExtCtrls,_PicTools, StdCtrls, FileCtrl, Buttons,MainWin;

function bmp2b15(var pFile:TFile;path:string;CheckWidth,CheckHeight:integer):boolean;

implementation

const DitherTable4:array[0..(4*4)-1] of integer=(0,4,1,5, 6,2,7,3, 1,5,0,4, 7,3,6,2);

var
  B15TransFlag:boolean;
  B15Data:array of word;
  B15DataCount:integer;

procedure dither(srcbm:TBitmap);
var
  x,y,w,h:integer;
  psbm:PByteArray;
  r,g,b:integer;
  dr,dg,db:integer;
  function d(c:integer):integer;
  var
    sx,sy,a:integer;
  begin
    sx:=x and 3;
    sy:=y and 3;
    a:=DitherTable4[sx*4+sy];
    Result:=(c+a) shr 3;
    if Result>31 then Result:=31;
  end;
  function d2(c:integer;var dc:integer):integer;
  var
    a:integer;
  begin
    a:=c+dc;
    dc:=a and 7;
    Result:=a shr 3;
    if Result>31 then Result:=31;
  end;
  function RGB15(r,g,b:integer):word;
  begin
    Result:=(b shl 10)+(g shl 5)+(r shl 0)+(1 shl 15);
  end;
begin
  w:=srcbm.Width;
  h:=srcbm.Height;

  dr:=0;
  dg:=0;
  db:=0;

  B15DataCount:=w*h;
  setlength(B15Data,B15DataCount);
  B15TransFlag:=False;

  for y:=0 to h-1 do begin
    psbm:=srcbm.ScanLine[y];
    for x:=0 to w-1 do begin
      b:=psbm[x*3+0];
      g:=psbm[x*3+1];
      r:=psbm[x*3+2];
      B15Data[x+(y*w)]:=RGB15(d(r),d(g),d(b));
    end;
  end;
end;

procedure saveb15(var pFile:TFile;w,h:integer);
var
  idx:integer;
  procedure write16(d:word);
  begin
    pFile.DecompData[pFile.DecompSize+0]:=(d shr 0) and $ff;
    pFile.DecompData[pFile.DecompSize+1]:=(d shr 8) and $ff;
    inc(pFile.DecompSize,2);
  end;
  procedure writebool(d:Boolean);
  begin
    if d=False then begin
      write16(0);
      end else begin
      write16(1);
    end;
  end;
begin
  setlength(pFile.DecompData,(2*4)+(B15DataCount*2));
  pFile.DecompSize:=0;

  write16(w);
  write16(h);
  writebool(B15TransFlag);
  write16(0);

  for idx:=0 to B15DataCount-1 do begin
    write16(B15Data[idx]);
  end;

  setlength(pFile.DecompData,pFile.DecompSize);
  
  while((pFile.DecompSize and 3)<>0) do begin
    setlength(pFile.DecompData,pFile.DecompSize+1);
    pFile.DecompData[pFile.DecompSize]:=0;
    inc(pFile.DecompSize);
  end;
end;

function bmp2b15(var pFile:TFile;path:string;CheckWidth,CheckHeight:integer):boolean;
var
  srcbm:TBitmap;
begin
  srcbm:=TBitmap.Create;
  srcbm.LoadFromFile(path+'\'+pFile.Filename);

  if (srcbm.Width<>CheckWidth) or (srcbm.Height<>CheckHeight) then begin
    srcbm.Free;
    Result:=False;
    exit;
  end;

  dither(srcbm);
  saveb15(pFile,srcbm.Width,srcbm.Height);

  srcbm.Free;

  Result:=True;
end;

end.
