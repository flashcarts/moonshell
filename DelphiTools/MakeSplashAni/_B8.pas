unit _B8;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _m_Tools, _PicTools, StdCtrls, ExtCtrls,NkDIB;

function GetB8Size(srcbm:TBitmap):integer;
procedure MakeB8(bm:TBitmap;var b8pal:array of word;var b8palCount:integer;var b8buf:array of byte;var b8bufCount:integer);

function GetB15Size(srcbm:TBitmap):integer;
procedure MakeB15Beta(bm:TBitmap;var b15buf:array of word;var b15bufCount:integer);
procedure MakeB15Diff(bm:TBitmap;var b15buf:array of word;var b15bufCount:integer;lastbm:TBitmap);

implementation

function GetB8Size(srcbm:TBitmap):integer;
begin
  Result:=srcbm.Width*srcbm.Height;
end;

procedure MakeB8(bm:TBitmap;var b8pal:array of word;var b8palCount:integer;var b8buf:array of byte;var b8bufCount:integer);
var
  nkbm:TNkDIB;
  idx:integer;
  x,y,w,h:integer;
  yofs:integer;
  psbm:PByteArray;
  RGB:TRGB;
  function RGB15(r,g,b:integer):word;
  begin
{
    r:=r shr 3;
    g:=g shr 3;
    b:=b shr 3;
}
    Result:=(b shl 10)+(g shl 5)+(r shl 0)+(1 shl 15);
  end;
begin
  if bm.PixelFormat<>pf8bit then showmessage('can not convert b8 from not 8bit.');

  nkbm:=TNkDIB.Create;
  nkbm.Assign(bm);

  for idx:=0 to 256-1 do begin
    RGB:=dword2RGB(nkbm.Colors[idx]);
    b8pal[idx]:=RGB15(RGB.r,RGB.g,RGB.b);
  end;

  w:=nkbm.Width;
  h:=nkbm.Height;

  b8palCount:=1;

  for y:=0 to h-1 do begin
    psbm:=nkbm.ScanLine[y];
    yofs:=y*w;
    for x:=0 to w-1 do begin
      if psbm[x]=$ff then begin
        b8buf[yofs+x]:=0;
        end else begin
        b8buf[yofs+x]:=psbm[x];
        if b8palCount<=psbm[x] then b8palCount:=psbm[x]+1;
      end;
    end;
  end;

  nkbm.Free;
end;

function GetB15Size(srcbm:TBitmap):integer;
begin
  Result:=srcbm.Width*srcbm.Height;
end;

procedure MakeB15Beta(bm:TBitmap;var b15buf:array of word;var b15bufCount:integer);
var
  x,y,w,h:integer;
  yofs:integer;
  psbm:PByteArray;
  function RGB15(r,g,b:integer):word;
  begin
    r:=r shr 3;
    g:=g shr 3;
    b:=b shr 3;
    Result:=(b shl 10)+(g shl 5)+(r shl 0)+(1 shl 15);
  end;
begin
  if bm.PixelFormat<>pf24bit then showmessage('can not convert b15 from not 24bit.');

  w:=bm.Width;
  h:=bm.Height;

  for y:=0 to h-1 do begin
    psbm:=bm.ScanLine[y];
    yofs:=y*w;
    for x:=0 to w-1 do begin
      b15buf[yofs+x]:=RGB15(psbm[x*3+2],psbm[x*3+1],psbm[x*3+0]);
    end;
  end;
end;

procedure MakeB15Diff(bm:TBitmap;var b15buf:array of word;var b15bufCount:integer;lastbm:TBitmap);
var
  x,y,w,h:integer;
  b15idx:integer;
  psbm,plbm:PByteArray;
  nullcnt:integer;
  function RGB15(r,g,b:integer):word;
  begin
    r:=r shr 3;
    g:=g shr 3;
    b:=b shr 3;
    Result:=(b shl 10)+(g shl 5)+(r shl 0)+(1 shl 15);
  end;
begin
  if bm.PixelFormat<>pf24bit then showmessage('can not convert b15 from not 24bit.');

  w:=bm.Width;
  h:=bm.Height;

  nullcnt:=0;
  b15idx:=0;

  for y:=0 to h-1 do begin
    psbm:=bm.ScanLine[y];
    plbm:=lastbm.ScanLine[y];
    for x:=0 to w-1 do begin
      if (psbm[x*3+2]<>plbm[x*3+2]) or (psbm[x*3+1]<>plbm[x*3+1]) or (psbm[x*3+0]<>plbm[x*3+0]) then begin
        if nullcnt<>0 then begin
          b15buf[b15idx]:=nullcnt;
          inc(b15idx);
          nullcnt:=0;
        end;
        b15buf[b15idx]:=RGB15(psbm[x*3+2],psbm[x*3+1],psbm[x*3+0]);
        inc(b15idx);
        end else begin
        inc(nullcnt);
      end;
    end;
  end;

  if nullcnt<>0 then begin
    b15buf[b15idx]:=nullcnt;
    inc(b15idx);
  end;

  b15bufcount:=b15idx;
end;

end.
