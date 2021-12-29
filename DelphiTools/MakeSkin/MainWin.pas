unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,_const, StdCtrls,_m_Tools,
  ExtCtrls,zlib;

const AppTitle='MakeSkin.exe';
const AppVersion='ver 0.4';

type
  TMain = class(TForm)
    StartupTimer: TTimer;
    LogLst: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure StartupTimerTimer(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Main: TMain;

type
  TFile=record
    Filename:string;
    Width,Height:integer;
    FilenameOffset:integer;
    DecompSize:integer;
    DecompData:array of byte;
    CompSize:integer;
    CompData:array of byte;
    CompDataOffset:integer;
  end;

implementation

{$R *.dfm}

uses _bmp2b15,_png2tgf;

procedure AddLog(str:string);
begin
  Main.LogLst.Items.Add(str);
end;

var
  FilesCount:integer;
  Files:array of TFile;

procedure AddFileList(path,_Filename:string;_Width,_Height:integer);
begin
  setlength(Files,FilesCount+1);
  with Files[FilesCount] do begin
    Filename:=_Filename;
    Width:=_Width;
    Height:=_Height;
    FilenameOffset:=0;
    DecompSize:=0;
    setlength(DecompData,DecompSize);
    CompSize:=0;
    setlength(CompData,CompSize);
    CompDataOffset:=0;
  end;
  inc(FilesCount);
end;

function LoadFileList(lstfn,path:string):boolean;
var
  lst:TStringList;
  idx:integer;
  s:string;
  pos:integer;
  fn:string;
  w,h:integer;
begin
  Result:=True;

  FilesCount:=0;

  lst:=TStringList.Create;
  lst.LoadFromFile(lstfn);

  for idx:=0 to lst.Count-1 do begin
    s:=lst[idx];
    if s<>'' then begin
      pos:=ansipos(',',s);
      fn:=copy(s,1,pos-1);
      s:=copy(s,pos+1,length(s));
      pos:=ansipos(',',s);
      w:=strtointdef(copy(s,1,pos-1),0);
      s:=copy(s,pos+1,length(s));
      h:=strtointdef(s,0);

      if FileExists(path+'\'+fn)=False then begin
        AddLog('File not found. ['+fn+']');
        Result:=False;
        end else begin
        AddFileList(path,fn,w,h);
      end;

    end;
  end;

  lst.Free;
end;

function Convert(TagPath:string):boolean;
var
  idx:integer;
  fn,ext:string;
  totalsize:integer;
  function ini2ini(var pFile:TFile;path:string;CheckWidth,CheckHeight:integer):boolean;
  var
    rfs:TFileStream;
  begin
    rfs:=TFileStream.Create(path+'\'+pFile.Filename,fmOpenRead);
    pFile.DecompSize:=rfs.Size;
    setlength(pFile.DecompData,pFile.DecompSize);
    rfs.ReadBuffer(pFile.DecompData[0],pFile.DecompSize);
    rfs.Free;

    while((pFile.DecompSize and 3)<>0) do begin
      setlength(pFile.DecompData,pFile.DecompSize+1);
      pFile.DecompData[pFile.DecompSize]:=0;
      inc(pFile.DecompSize);
    end;

    Result:=True;
  end;
begin
  Result:=True;

  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      fn:=Filename;
      ext:=ExtractFileExt(fn);
      if ext='.ini' then begin
        if ini2ini(Files[idx],TagPath,Width,Height)=False then begin
          AddLog('Load error. request=('+inttostr(Width)+','+inttostr(Height)+')pixels. ['+fn+']');
          Result:=False;
          end else begin
//          Main.Memo1.Lines.Add('Loaded. ini2ini ['+fn+']');
          Filename:=ChangeFileExt(Filename,'.ini');
        end;
        fn:='';
      end;
      if ext='.bmp' then begin
        if bmp2b15(Files[idx],TagPath,Width,Height)=False then begin
          AddLog('Illigal image size error. request=('+inttostr(Width)+','+inttostr(Height)+')pixels. ['+fn+']');
          Result:=False;
          end else begin
//          AddLog('Converted. bmp2b15 ['+fn+']');
          Filename:=ChangeFileExt(Filename,'.b15');
        end;
        fn:='';
      end;
      if ext='.png' then begin
        if png2tgf(Files[idx],TagPath,Width,Height)=False then begin
          AddLog('Illigal image size error. request=('+inttostr(Width)+','+inttostr(Height)+')pixels. ['+fn+']');
          Result:=False;
          end else begin
//          AddLog('Converted. png2tgf ['+fn+']');
          Filename:=ChangeFileExt(Filename,'.tgf');
        end;
        fn:='';
      end;
      if (DecompSize and 3)<>0 then begin
        AddLog('Binary size is not 4byte alignmented. ['+fn+'] '+inttostr(DecompSize)+'byte.');
        Result:=False;
      end;
      if fn<>'' then begin
        AddLog('Unknown file format. ['+fn+']');
        Result:=False;
      end;
    end;
  end;

  totalsize:=0;
  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      inc(totalsize,DecompSize);
    end;
  end;

  AddLog(format('Total decompressed binary size = %dbyte',[totalsize]));
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

procedure zlibCompress(var pFile:TFile);
var
  _EncData:PByteArray;
  _EncDataSize:integer;
begin
  pFile.CompSize:=0;

  if pFile.DecompSize<=0 then exit;

  zlibCompressBuf(pointer(pFile.DecompData),pFile.DecompSize,pointer(_EncData),_EncDataSize);

  if (_EncData=nil) or (_EncDataSize=0) then exit;

  pFile.CompSize:=_EncDataSize;
  setlength(pFile.CompData,pFile.CompSize);
  MoveMemory(@pFile.CompData[0],@_EncData[0],pFile.CompSize);

  pFile.CompSize:=(pFile.CompSize+3) and not 3;
  setlength(pFile.CompData,pFile.CompSize);

  FreeMem(_EncData,_EncDataSize);
end;

procedure Compress_ZLIB(TagPath:string);
var
  idx:integer;
begin
  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      zlibCompress(Files[idx]);
      AddLog('Compress ZLIB '+Filename+' '+format('%d->%dbyte',[DecompSize,CompSize]));
    end;
  end;
end;

procedure Compress_Flat(TagPath:string);
var
  idx:integer;
begin
  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      CompSize:=DecompSize;
      setlength(CompData,CompSize);
      movememory(CompData,DecompData,CompSize);
      AddLog('Flat. '+Filename+' '+format('%d->%dbyte',[DecompSize,CompSize]));
    end;
  end;
end;

procedure WriteFile(SkinFilename:string);
var
  ID:string;
  idx:integer;
  wfs:TFileStream;
  HeaderOffset:integer;
  HeaderSize:dword;
  procedure wfs8bit(d:byte);
  begin
    wfs.WriteBuffer(d,1);
  end;
  procedure wfs16bit(d:word);
  begin
    wfs.WriteBuffer(d,2);
  end;
  procedure wfs32bit(d:dword);
  begin
    wfs.WriteBuffer(d,4);
  end;
  procedure wfsStr(str:string);
  var
    len:byte;
  begin
    str:=str+char(0);
    len:=length(str);
    wfs8bit(len-1);
    wfs.WriteBuffer(str[1],len);
  end;
begin
  wfs:=TFileStream.Create(SkinFilename,fmCreate);

  ID:='Skin files package for MoonShell2 type.3'+char(0)+char(0);
  wfsStr(ID);

  wfs32bit(FilesCount);

  HeaderSize:=0;
  wfs32bit(HeaderSize);

  HeaderOffset:=wfs.Position;

  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      wfs32bit(FilenameOffset);
      wfs32bit(DecompSize);
      wfs32bit(CompSize);
      wfs32bit(CompDataOffset);
    end;
  end;

  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      FilenameOffset:=wfs.Position-HeaderOffset;
      wfsStr(Filename);
    end;
  end;

  while((wfs.Position and 3)<>0) do begin
    wfs8bit(0);
  end;

  HeaderSize:=wfs.Position-HeaderOffset;

  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      CompDataOffset:=wfs.Position;
      if CompSize<>0 then wfs.WriteBuffer(CompData[0],CompSize);
      while((wfs.Position and 3)<>0) do begin
        wfs8bit(0);
      end;
    end;
  end;

  wfs.Position:=HeaderOffset;

  for idx:=0 to FilesCount-1 do begin
    with Files[idx] do begin
      wfs32bit(FilenameOffset);
      wfs32bit(DecompSize);
      wfs32bit(CompSize);
      wfs32bit(CompDataOffset);
    end;
  end;

  wfs.Position:=HeaderOffset-4;
  wfs32bit(HeaderSize);

  wfs.Free;
end;

// ----------------------------------------------------------------

procedure TMain.FormCreate(Sender: TObject);
begin
  Application.Title:=AppTitle+' '+AppVersion;
  Main.Caption:=Application.Title;

  StartPath:=ExtractFilePath(Application.ExeName);

  LogLst.Clear;

  AddLog(AppTitle+' '+AppVersion);
  AddLog('');

  StartupTimer.Enabled:=True;
end;

procedure TMain.StartupTimerTimer(Sender: TObject);
var
  logfn:string;
  SkinFilename:string;
  TagPath:string;
  procedure SaveLog;
  begin
    LogLst.Items.SaveToFile(logfn);
  end;
begin
  StartupTimer.Enabled:=False;

  if paramcount<2 then begin
    ShowMessage('Command line: MakeSkin.exe SkinFilename.skn SourceFilePath'+CRLF+CRLF+'See attached batch file.');
    Application.Terminate;
    exit;
  end;

  logfn:=ChangeFileExt(Application.ExeName,'.log');

  SkinFilename:=StartPath+paramstr(1);
  TagPath:=StartPath+paramstr(2);

  if DirectoryExists(TagPath)=False then begin
    ShowMessage('Target path not found.'+CRLF+TagPath);
    Application.Terminate;
    exit;
  end;

  if LoadFileList(StartPath+'MakeSkin.lst',TagPath)=False then begin
    SaveLog;
    ShowMessage('File not found error.'+CRLF+CRLF+'Refer log file.'+CRLF+logfn);
    Application.Terminate;
    exit;
  end;

  if Convert(TagPath)=False then begin
    SaveLog;
    ShowMessage('Convert error.'+CRLF+CRLF+'Refer log file.'+CRLF+logfn);
    Application.Terminate;
    exit;
  end;

  Compress_ZLIB(TagPath);

  Main.Refresh;

  WriteFile(SkinFilename);

  AddLog('Terminate.');

  SaveLog;

  Application.Terminate;
end;

end.
