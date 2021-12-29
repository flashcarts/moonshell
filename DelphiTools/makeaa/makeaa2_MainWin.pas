unit makeaa2_MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,_PicTools;

type
  TMain = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    ListBox1: TListBox;
    PrvImg: TImage;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private êÈåæ }
    procedure createfontfile(_FontHeight:integer;wfn:string);
  public
    { Public êÈåæ }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

var
  uniusetbl:array[0..$10000-1] of boolean;

procedure LoadUniTbl(fn:string);
var
  tblstr:TStringList;
  idx,cnt,pos:integer;
  str:string;
begin
  tblstr:=TStringList.Create;
  tblstr.LoadFromFile(fn);
  cnt:=tblstr.Count;

  for idx:=0 to cnt-1 do begin
    str:=tblstr[idx];
    pos:=ansipos(chr(9)+'0x',str);
    if pos<>0 then begin
      str:='$'+copy(str,pos+3,4);
      uniusetbl[strtoint(str)]:=true;
    end;
  end;

  tblstr.Free;
end;

var
  wfs:TFileStream;
  postbl:array[0..$10000-1] of dword;
  widthstbl:array[0..$10000-1] of byte;

var
  levels:array[0..$100-1] of integer;

procedure savedata(widx:integer;w,h:integer);
var
  x,y:integer;
  pb:pbytearray;
  r,g,b:integer;
  curbits:dword;
  curbitscount:integer;
  function reduce(c:integer):integer;
  begin
    // lev8->index8
    if $ff<=c then begin
      Result:=6;
      exit;
    end;
    if $df<=c then begin
      Result:=5;
      exit;
    end;
    if $bf<=c then begin
      Result:=4;
      exit;
    end;
    if $9c<=c then begin
      Result:=3;
      exit;
    end;
    if $74<=c then begin
      Result:=2;
      exit;
    end;
    if $48<=c then begin
      Result:=1;
      exit;
    end;
    Result:=0;
  end;
  procedure w3bit(data:dword);
  begin
    if 16<=curbitscount then begin
      wfs.WriteBuffer(curbits,2);
      curbits:=curbits shr 16;
      dec(curbitscount,16);
    end;
    curbits:=curbits or (data shl curbitscount);
    inc(curbitscount,3);
  end;
begin
  curbits:=0;
  curbitscount:=0;

  for y:=0 to h-1 do begin
    pb:=Main.Image1.Picture.Bitmap.ScanLine[y];
    for x:=0 to w-1 do begin
      b:=pb[x*3+0];
      g:=pb[x*3+1];
      r:=pb[x*3+2];
      inc(levels[b]);
      inc(levels[g]);
      inc(levels[r]);
      b:=reduce(b);
      g:=reduce(g);
      r:=reduce(r);
      if (b=0) and (g=0) and (r=0) then begin
        w3bit(7);
        end else begin
        w3bit(r);
        w3bit(g);
        w3bit(b);
      end;
    end;
  end;

  if curbitscount<>0 then wfs.WriteBuffer(curbits,2);
end;

var
  maxsizebyte:integer;

procedure TMain.createfontfile(_FontHeight:integer;wfn:string);
var
  headersize:integer;
  wstr:widestring;
  idx:integer;
  x,y:integer;
  w,h,dw:integer;
  fontcnt:integer;
  totalpixels:integer;
  startpos,lastpos,datasize:integer;
  tmp:byte;
  MaxWidth,MaxHeight:integer;
  prvx,prvy:integer;
  FontHeight:integer;
  procedure wid;
  var
    s:string;
  begin
    s:='MoonShell2 ClearTypeFont V3'+char(0)+char(3)+char(0)+char(0)+char(0);
    wfs.WriteBuffer(s[1],headersize);
  end;
  function GetHeight:integer;
  var
    x,y:integer;
    pb:pbytearray;
  begin
    Result:=0;
    for y:=0 to Main.Image1.Picture.Bitmap.Height-1 do begin
      pb:=Main.Image1.Picture.Bitmap.ScanLine[y];
      for x:=0 to (MaxWidth*3)-1 do begin
        if pb[x]<>0 then Result:=y+1;
      end;
    end;
  end;
begin

  MaxWidth:=32;
  MaxHeight:=_FontHeight;

  wfn:=format(wfn,[MaxHeight]);
  ListBox1.Items.Add('Create ['+wfn+']');
  
  wfs:=TFileStream.Create(wfn,fmCreate);
  headersize:=32;
  wid;

  wfs.Position:=headersize;
  for idx:=0 to $10000-1 do begin
    postbl[idx]:=0;
    widthstbl[idx]:=0;
  end;
  wfs.WriteBuffer(postbl[0],4*$10000);
  wfs.WriteBuffer(widthstbl[0],1*$10000);

  MakeBlankImg(PrvImg,pf24bit);

  for idx:=0 to $100-1 do begin
    levels[idx]:=0;
  end;

  fontcnt:=0;

  h:=MaxHeight;

  Image1.Canvas.Font.Color:=$ffffff;
  Image1.Canvas.Brush.Style:=bsSolid;
  Image1.Canvas.Brush.Color:=$000000;

  maxsizebyte:=0;
  startpos:=wfs.Position;

  prvx:=0;
  prvy:=0;

  for idx:=0 to $10000-1 do begin
    if uniusetbl[idx]=true then begin
      FontHeight:=h-1;

      wstr:=widechar(idx);
      x:=0;
      y:=-2;
      dw:=0;
      if wstr='/' then inc(x);
      if ('ÇO'<=wstr) and (wstr<='ÅG') then begin
        inc(FontHeight,2);
        wstr:=widechar(integer('0')+(idx-$ff10));
        dec(y);
      end;
      if ('Ç`'<=wstr) and (wstr<='Çy') then begin
        inc(FontHeight,1);
        wstr:=widechar(integer('A')+(idx-$ff21));
        dec(y);
        dw:=2;
      end;
      if ('ÇÅ'<=wstr) and (wstr<='Çö') then begin
        inc(FontHeight,1);
        wstr:=widechar(integer('a')+(idx-$ff41));
        dec(y);
        dw:=1;
      end;
      if wstr='_' then dec(y);
      if wstr=';' then dec(y);

      Image1.Canvas.Font.Height:=-FontHeight;

      Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Image1.Height));
      TextOutW(Image1.Canvas.Handle,x,y,pWideChar(@wstr[1]),length(wstr));
      if MaxHeight<GetHeight then begin
        Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Image1.Height));
        TextOutW(Image1.Canvas.Handle,x,y-1,pWideChar(@wstr[1]),length(wstr));
        if MaxHeight<GetHeight then begin
          Image1.Canvas.Font.Height:=-(FontHeight-1);
          Image1.Canvas.FillRect(Rect(0,0,Image1.Width,Image1.Height));
          TextOutW(Image1.Canvas.Handle,x,y,pWideChar(@wstr[1]),length(wstr));
        end;
      end;

      w:=x+Image1.Canvas.TextWidth(wstr)+dw+1;
      if wstr='Å@' then w:=(h div 2)+1;

      if ($829f<=idx) and (idx<=$8396) then dec(w); // #HIRAGANA LETTER SMALL A Å` #KATAKANA LETTER SMALL KE

      if MaxWidth<w then ListBox1.Items.Add(format('over width 0x%4x %2d',[idx,w]));
      if MaxHeight<GetHeight then begin
        ListBox1.Items.Add(format('over height 0x%4x %2d',[idx,GetHeight]));
        if prvimg.Width<=(prvx+24) then begin
          prvx:=0;
          inc(prvy,24);
        end;
        BitBlt(prvimg.Canvas.Handle,prvx,prvy,24,24,Image1.Canvas.Handle,0,0,SRCCOPY);
        inc(prvx,24);
        prvimg.Refresh;
      end;

      lastpos:=wfs.Position;
      savedata(idx,w,h+1);
      while((wfs.Position and 3)<>0) do begin
        tmp:=0;
        wfs.WriteBuffer(tmp,1);
      end;
      datasize:=wfs.Position-lastpos;
      if maxsizebyte<datasize then maxsizebyte:=datasize;

      if $ffffff<lastpos-startpos then begin
        showmessage('lastpos overflow $'+inttohex(lastpos,8));
      end;

      if False then begin // preview
        if prvimg.Width<=(prvx+24) then begin
          prvx:=0;
          inc(prvy,24);
        end;
        BitBlt(prvimg.Canvas.Handle,prvx,prvy,24,24,Image1.Canvas.Handle,0,0,SRCCOPY);
        inc(prvx,24);
      end;

      postbl[idx]:=(datasize shl 24) or ((lastpos-startpos) and $ffffff);
      widthstbl[idx]:=w;

      inc(fontcnt);
    end;
    if (idx and $ff)=0 then begin
      Image1.Refresh;
      ListBox1.Refresh;
      PrvImg.Refresh;
      caption:=inttostr(fontcnt)+' '+inttostr(idx);
    end;
  end;

  prvimg.Canvas.Pen.Color:=$808080;
  for y:=0 to (prvimg.Height div 24)-1 do begin
    prvimg.Canvas.MoveTo(0,y*24+MaxHeight);
    prvimg.Canvas.LineTo(prvimg.Width,y*24+MaxHeight);
    prvimg.Canvas.MoveTo(0,y*24+24);
    prvimg.Canvas.LineTo(prvimg.Width,y*24+24);
  end;

  caption:='fontcnt:'+inttostr(fontcnt)+' maxsizebyte:'+inttostr(maxsizebyte);;

  wfs.Position:=headersize;
  wfs.WriteBuffer(postbl[0],4*$10000);
  wfs.WriteBuffer(widthstbl[0],1*$10000);

  wfs.Free;

  totalpixels:=0;
  for idx:=0 to $100-1 do begin
    inc(totalpixels,levels[idx]);
  end;

  for idx:=0 to $100-1 do begin
    if levels[idx]<>0 then begin
      ListBox1.Items.Add(format('%3d %x %dpix %f%%',[idx,idx,levels[idx],levels[idx]/totalpixels*100]));
    end;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  wfs:TFileStream;
  idx:integer;
  wfn:string;
  tmp:integer;
  procedure _LoadUniTbl(fn:string);
  var
    idx:integer;
  begin
    for idx:=0 to $10000-1 do begin
      uniusetbl[idx]:=false;
    end;

    LoadUniTbl(fn);

    for idx:=$00 to $20-1 do begin
      uniusetbl[idx]:=false;
    end;

    for idx:=$20 to $80-1 do begin
      uniusetbl[idx]:=true;
    end;
  end;
begin
  Show;

{
  wfs:=TFileStream.Create('a',fmCreate);
  for idx:=0 to 512-1 do begin
    tmp:=0;
    wfs.WriteBuffer(tmp,1);
  end;
  for idx:=0 to 256-1 do begin
    wfs.WriteBuffer(idx,1);
    tmp:=255-idx;
    wfs.WriteBuffer(tmp,1);
  end;
  wfs.Free;
  exit;
}

  maxsizebyte:=0;

  MakeBlankImg(Image1,pf24bit);

  _LoadUniTbl('CP932.TXT');
  Image1.Canvas.Font:=Label1.Font;

  wfn:='tfontjpn\font%d.ctf';
  createfontfile(12,wfn);
  createfontfile(14,wfn);
  createfontfile(16,wfn);

  _LoadUniTbl('CP0.TXT');
  Image1.Canvas.Font:=Label2.Font;

  wfn:='tfonteng\font%d.ctf';
  createfontfile(12,wfn);
  createfontfile(14,wfn);
  createfontfile(16,wfn);

end;

end.
