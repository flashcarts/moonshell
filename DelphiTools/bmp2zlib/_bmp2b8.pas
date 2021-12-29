unit _bmp2b8;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,GLDPNG, ExtCtrls,_PicTools, StdCtrls, FileCtrl, Buttons,MainWin;

function bmp2b8(var pBMPFile:TBMPFile):boolean;

implementation

const DitherTable4:array[0..(4*4)-1] of integer=(0,4,1,5, 6,2,7,3, 1,5,0,4, 7,3,6,2);

procedure saveb8(var pBMPFile:TBMPFile;var bm:TBitmap;w,h:integer);
var
  x,y:integer;
  pb:PByteArray;
  procedure write8(d:byte);
  begin
    setlength(pBMPFile.DecompData,pBMPFile.DecompSize+1);
    pBMPFile.DecompData[pBMPFile.DecompSize]:=d;
    inc(pBMPFile.DecompSize,1);
  end;
begin
  for y:=0 to h-1 do begin
    pb:=bm.ScanLine[y];
    for x:=0 to w-1 do begin
      write8(pb[x]);
    end;
  end;
end;

function bmp2b8(var pBMPFile:TBMPFile):boolean;
var
  srcbm:TBitmap;
begin
  srcbm:=TBitmap.Create;
  srcbm.LoadFromFile(pBMPFile.Filename);

  saveb8(pBMPFile,srcbm,srcbm.Width,srcbm.Height);

  srcbm.Free;

  Result:=True;
end;

end.
