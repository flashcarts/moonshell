unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _m_Tools, _PicTools, StdCtrls, FileCtrl, ExtCtrls,INIFiles,zlib;

type
  TForm1 = class(TForm)
    PrevImg: TImage;
    Timer1: TTimer;
    LogLst: TListBox;
    OpenDlg: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses _B8,NkDIB,MedianCut,Octree;

const CRLF:string=char($0d)+char($0a);

type
  TSplashScreen=record
    SrcFilename:string;
    VSyncCount:integer;
    FileOffset,ImageSize:integer;
    DecompSize:integer;
{
    b8pal:array[0..256-1] of word;
    b8palCount:integer;
    b8buf:array of byte;
    b8bufCount:integer;
}
    b15buf:array of word;
    b15bufCount:integer;
    CompBufCount:integer;
    CompBuf:array of byte;
  end;

type
  TSplash=record
    DataPath:string;
    SourceFileMask:string;
    SourceFPS:integer;
    UseDither24to15bit:boolean;
    Flags:dword;
    ScreenCount:integer;
    Screen:array of TSplashScreen;
  end;

var
  SplashINIFilename:string;
  Splash:TSplash;

const Flags_WaitForTerminate=1 shl 0;
const Flags_AlreadyAllDraw=1 shl 1;

procedure GetFileList(Path:string;Mask:string;var FilesLst:TStringList);
var
  SearchRec: TSearchRec;
  res:integer;
  fn:string;
begin
  FilesLst.Clear;

  res:=FindFirst(Path+Mask, (FaAnyFile), SearchRec);
  if res=0 then begin
    repeat
      if (SearchRec.Attr and faDirectory)=0 then begin
        fn:=SearchRec.Name;
        FilesLst.Add(fn);
      end;
      res:=FindNext(SearchRec);
    until (res<>0);
  end;
  FindClose(SearchRec);

  FilesLst.Sort;
end;

procedure LoadINI(INIFilename:string);
var
  ini:TINIFile;
  Section:string;
begin
  ini:=TINIFile.Create(INIFilename);

  Section:='ConvertSetting';

  Splash.SourceFileMask:=ini.ReadString(Section,'SourceFileMask','');
  Splash.SourceFPS:=ini.ReadInteger(Section,'SourceFPS',0);

  Splash.Flags:=0;
  if ini.ReadBool(Section,'WaitForTerminate',False)=True then Splash.Flags:=Splash.Flags or Flags_WaitForTerminate;
  if ini.ReadBool(Section,'AlreadyAllDraw',False)=True then Splash.Flags:=Splash.Flags or Flags_AlreadyAllDraw;

  Splash.UseDither24to15bit:=ini.ReadBool(Section,'UseDither24to15bit',True);

  ini.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  idx:integer;
  FilesLst:TStringList;
  fn:string;
  bm:TBitmap;
  ErrorStr:string;
begin
  ErrorStr:='';

  SplashINIFilename:=ParamStr(1);

  if FileExists(SplashINIFilename)=False then begin
    if OpenDlg.Execute=True then SplashINIFilename:=OpenDlg.FileName;
  end;

  if FileExists(SplashINIFilename)=False then begin
    Application.Terminate;
    exit;
  end;

  LoadINI(SplashINIFilename);

  Splash.DataPath:=ExtractFilePath(SplashINIFilename);

  FilesLst:=TStringList.Create;

  GetFileList(Splash.DataPath,Splash.SourceFileMask,FilesLst);

  Splash.ScreenCount:=FilesLst.Count;
  setlength(Splash.Screen,Splash.ScreenCount);
  for idx:=0 to Splash.ScreenCount-1 do begin
    fn:=FilesLst[idx];
    Splash.Screen[idx].SrcFilename:=fn;
    bm:=TBitmap.Create;
    bm.LoadFromFile(Splash.DataPath+fn);
    if bm.Width<>256 then ErrorStr:=ErrorStr+fn+' Width is not 256pixels.'+CRLF;
    if bm.Height=0 then ErrorStr:=ErrorStr+fn+' Height is 0pixel.'+CRLF;
    if 192<bm.Height then ErrorStr:=ErrorStr+fn+' Height is over 192pixels.'+CRLF;
    bm.Free;
  end;

  FilesLst.Free;

  if Splash.ScreenCount=0 then ErrorStr:=ErrorStr+'Image file not found.'+CRLF;
  if Splash.SourceFPS=0 then ErrorStr:=ErrorStr+'SourceFPS is zero.'+CRLF;

  if ErrorStr<>'' then begin
    ShowMessage(ErrorStr);
    Application.Terminate;
    exit;
  end;

  MakeBlankImg(PrevImg,pf24bit);

  Timer1.Enabled:=True;
end;

const DitherTable4Data8:array[0..(4*4)-1] of integer=(0,4,1,5, 6,2,7,3, 1,5,0,4, 7,3,6,2);
const DitherTable4Data16:array[0..(4*4)-1] of integer=(4,12,6,14, 10,0,8,2, 7,15,5,13, 9,3,11,1);

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
    c:=ic and not $7;
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

procedure MaskUnder3bit(var bm:TBitmap);
var
  x,y,w,h:integer;
  psbm:PByteArray;
begin
  w:=bm.Width;
  h:=bm.Height;

  for y:=0 to h-1 do begin
    psbm:=bm.ScanLine[y];
    for x:=0 to (w*3)-1 do begin
      psbm[x]:=psbm[x] and not 3;
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

procedure Palette5to8(var bm:TBitmap);
var
  idx:integer;
  nkbm:TnkDIB;
begin
  nkbm:=TNkDIB.Create;
  nkbm.Assign(bm);
  for idx:=0 to nkbm.PaletteSize-1-1 do begin
    nkbm.Colors[idx]:=nkbm.Colors[idx] shl 3;
  end;
  bm.Free;
  bm:=TBitmap.Create;
  bm.Assign(nkbm);
  nkbm.Free;
end;

procedure CheckDuplicatePalettes(var bm:TBitmap);
var
  nkbm:TNkDIB;
  palcnt:integer;
  paluse:array[0..256-1] of boolean;
  idx:integer;
  x,y:integer;
  pbm:PByteArray;
  palidx:integer;
  procedure setpaluse;
  var
    idx:integer;
    x,y:integer;
    pbm:PByteArray;
  begin
    for idx:=0 to 256-1 do begin
      paluse[idx]:=False;
    end;
    for y:=0 to nkbm.Height-1 do begin
      pbm:=nkbm.ScanLine[y];
      for x:=0 to nkbm.Width-1 do begin
        paluse[pbm[x]]:=True;
      end;
    end;
    for idx:=0 to 256-1 do begin
      if paluse[idx]=True then palcnt:=idx+1;
    end;
  end;
begin
  nkbm:=TNkDIB.Create;
  nkbm.Assign(bm);

  palcnt:=0;
  setpaluse;
//  form1.loglst.Items.Add(format('palcnt=%d',[palcnt]));

  for idx:=palcnt-1 downto 1 do begin
    if paluse[idx]=False then begin
//      form1.loglst.Items.Add(format('delete palette=%d',[idx]));
      for y:=0 to nkbm.Height-1 do begin
        pbm:=nkbm.ScanLine[y];
        for x:=0 to nkbm.Width-1 do begin
          if idx<=pbm[x] then pbm[x]:=pbm[x]-1;
        end;
      end;
      for palidx:=idx to 256-1-1 do begin
        nkbm.Colors[palidx]:=nkbm.Colors[palidx+1];
      end;
    end;
  end;

  bm.Free;
  bm:=TBitmap.Create;
  bm.Assign(nkbm);
  nkbm.Free;
end;

function BitmapReduce254colors_DHGL(var bm:TBitmap):boolean;
var
  x,y,w,h:integer;
  pb:PByteArray;
  tmpbm:TBitmap;
begin
//  DitherTable4(bm);
//  DitherTable4And8to5bit(bm);
  MaskUnder3bit(bm);

  bm.PixelFormat:=pf24bit;
  tmpbm:=ReduceColorsByMedianCutV(bm,8);
  bm.Free;
  bm:=tmpbm;

  w:=bm.Width;
  h:=bm.Height;

  Palette8to5(bm);

  CheckDuplicatePalettes(bm);

  for y:=0 to h-1 do begin
    pb:=bm.ScanLine[y];
    for x:=0 to w-1 do begin
      if 255=pb[x] then ShowMessage('palette limit error');
    end;
  end;

//  Palette5to8(bm);

  Result:=True;
end;

{$L gbalzss16enc_static.obj}

function _gbalzss16enc_static_ExecuteEncode(_inbuf:PWordArray;_insize:dword;_outbuf:PByteArray):integer; cdecl; external;

procedure lzss16Compress_Internal(srcbuf:Pointer;srcbufCount:integer;var zlibbuf:array of byte;var zlibbufCount:integer);
begin
  zlibbufCount:=_gbalzss16enc_static_ExecuteEncode(pWordArray(srcbuf),srcbufCount,pbytearray(@zlibbuf[0]));
  zlibbufCount:=(zlibbufCount+3) and not 3;
end;

procedure zlibCompressBuf(const InBuf: Pointer; InBytes: Integer;
                      out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TZStreamRec;
  P: Pointer;
  function CCheck(code: Integer): Integer;
  begin
    Result := code;
    if code < 0 then raise ECompressionError.Create('ZLIB compress error!!'); //!!
  end;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.zalloc := zlibAllocMem;
  strm.zfree := zlibFreeMem;
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    CCheck(deflateInit_(strm, 1, zlib_version, sizeof(strm)));
    try
      while CCheck(deflate(strm, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PChar(Integer(OutBuf) + (Integer(strm.next_out) - Integer(P)));
        strm.avail_out := 256;
      end;
    finally
      CCheck(deflateEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

type
  TZLIB=record
    DecompSize:integer;
    DecompData:array of byte;
    CompSize:integer;
    CompData:array of byte;
  end;

procedure zlibCompress(var pZLIB:TZLIB);
var
  _EncData:PByteArray;
  _EncDataSize:integer;
begin
  pZLIB.CompSize:=0;

  if pZLIB.DecompSize<=0 then exit;

  zlibCompressBuf(pointer(pZLIB.DecompData),pZLIB.DecompSize,pointer(_EncData),_EncDataSize);

  if (_EncData=nil) or (_EncDataSize=0) then exit;

  pZLIB.CompSize:=_EncDataSize;
  setlength(pZLIB.CompData,pZLIB.CompSize);
  MoveMemory(@pZLIB.CompData[0],@_EncData[0],pZLIB.CompSize);

  pZLIB.CompSize:=(pZLIB.CompSize+3) and not 3;
  setlength(pZLIB.CompData,pZLIB.CompSize);

  FreeMem(_EncData,_EncDataSize);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  firstbm:TBitmap;
  idx:integer;
  wfs:TFileStream;
  dwtmp:dword;
  srcbm:TBitmap;
  z:TZLIB;
begin
  Timer1.Enabled:=False;

  LogLst.Clear;

  firstbm:=TBitmap.Create;

  for idx:=0 to Splash.ScreenCount-1 do begin
    with Splash.Screen[idx] do begin
      VSyncCount:=trunc(60/Splash.SourceFPS*idx);

      srcbm:=TBitmap.Create;

      srcbm.LoadFromFile(Splash.DataPath+SrcFilename);
      srcbm.PixelFormat:=pf24bit;

      BitBlt(PrevImg.Canvas.Handle,0,0,srcbm.Width,srcbm.Height,srcbm.Canvas.Handle,0,0,SRCCOPY);

//      BitmapReduce254colors_DHGL(srcbm);

      DitherTable4(srcbm);

      BitBlt(PrevImg.Canvas.Handle,0,srcbm.Height,srcbm.Width,srcbm.Height,srcbm.Canvas.Handle,0,0,SRCCOPY);

      PrevImg.Refresh;

{
      b8bufCount:=GetB8Size(srcbm);
      setlength(b8buf,b8bufCount);
      MakeB8(srcbm,b8pal,b8palCount,b8buf,b8bufCount);
      b8palCount:=(b8palCount+1) and not 1;
}

      ImageSize:=srcbm.Width*srcbm.Height;

      b15bufCount:=GetB15Size(srcbm);
      setlength(b15buf,b15bufCount);
      if idx=0 then begin
        MakeBlankBM(firstbm,srcbm.Width,srcbm.Height,pf24bit);
        BitBlt(firstbm.Canvas.Handle,0,0,srcbm.Width,srcbm.Height,srcbm.Canvas.Handle,0,0,SRCCOPY);
        MakeB15Beta(srcbm,b15buf,b15bufCount);

        z.DecompSize:=b15bufCount*2;
        z.DecompData:=pointer(b15buf);
        zlibCompress(z);
        setlength(CompBuf,z.CompSize);
        MoveMemory(@CompBuf[0],@z.CompData[0],z.CompSize);
        CompBufCount:=z.CompSize;
        end else begin
        MakeB15Diff(srcbm,b15buf,b15bufCount,firstbm);
      end;

      srcbm.Free;

{
      DecompSize:=b15bufCount;
      setlength(CompBuf,DecompSize*2);
      lzss16Compress_Internal(Pointer(addr(b15buf[0])),DecompSize,CompBuf,CompBufCount);
      setlength(CompBuf,CompBufCount);
}

//      LogLst.Items.Add(format('Frame:%d %s VSync:%d CompressedSize:%dbyte PalleteCount:%d',[idx,SrcFilename,VSyncCount,CompBufCount,b8palCount]));
      LogLst.Items.Add(format('Frame:%d %s VSync:%d CompressedSize:%dbyte DecompressedSize:%d',[idx,SrcFilename,VSyncCount,CompBufCount,b15bufcount*2]));
      LogLst.ItemIndex:=idx;
      LogLst.Refresh;
    end;
  end;

  wfs:=TFileStream.Create(Splash.DataPath+'splash.ani',fmCreate);

  wfs.Position:=0;

  dwtmp:=0;
  wfs.WriteBuffer(dwtmp,4);
  wfs.WriteBuffer(dwtmp,4);
  for idx:=0 to Splash.ScreenCount-1 do begin
    wfs.WriteBuffer(dwtmp,4);
    wfs.WriteBuffer(dwtmp,4);
    wfs.WriteBuffer(dwtmp,4);
    wfs.WriteBuffer(dwtmp,4);
  end;

  for idx:=0 to Splash.ScreenCount-1 do begin
    with Splash.Screen[idx] do begin
      FileOffset:=wfs.Position;
{
      FileSize:=4+(b8palCount*2)+CompBufCount;
      wfs.WriteBuffer(b8palCount,4);
      wfs.WriteBuffer(b8pal[0],b8palCount*2);
      wfs.WriteBuffer(CompBuf[0],CompBufCount);
}
{
      FileSize:=CompBufCount;
      wfs.WriteBuffer(CompBuf[0],CompBufCount);
}
      if idx=0 then begin
        wfs.WriteBuffer(CompBuf[0],CompBufCount);
        end else begin
        wfs.WriteBuffer(b15Buf[0],b15BufCount*2);
      end;
    end;
  end;

  wfs.Position:=0;

  wfs.WriteBuffer(Splash.Flags,4);
  wfs.WriteBuffer(Splash.ScreenCount,4);

  for idx:=0 to Splash.ScreenCount-1 do begin
    with Splash.Screen[idx] do begin
      wfs.WriteBuffer(VSyncCount,4);
      wfs.WriteBuffer(FileOffset,4);
      wfs.WriteBuffer(ImageSize,4);
      if idx=0 then begin
        wfs.WriteBuffer(CompBufCount,4);
        end else begin
        wfs.WriteBuffer(b15BufCount,4);
      end;
    end;
  end;

  wfs.Free;

  LogLst.Items.SaveToFile(ChangeFileExt(SplashINIFilename,'.log'));

  ShowMessage(Splash.DataPath+'splash.ani'+CRLF+'Created.');

  Close;
end;

end.
