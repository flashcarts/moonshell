unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,_const, StdCtrls,_m_Tools,
  ExtCtrls,zlib;

type
  TMain = class(TForm)
    StartupTimer: TTimer;
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
  TBMPFile=record
    Filename:string;
    DecompSize:integer;
    DecompData:array of byte;
    CompSize:integer;
    CompData:array of byte;
  end;

implementation

{$R *.dfm}

uses _bmp2b15,_bmp2b8;

var
  BMPFile:TBMPFile;

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

procedure zlibCompress(var pBMPFile:TBMPFile);
var
  _EncData:PByteArray;
  _EncDataSize:integer;
begin
  pBMPFile.CompSize:=0;

  if pBMPFile.DecompSize<=0 then exit;

  zlibCompressBuf(pointer(pBMPFile.DecompData),pBMPFile.DecompSize,pointer(_EncData),_EncDataSize);

  if (_EncData=nil) or (_EncDataSize=0) then exit;

  pBMPFile.CompSize:=_EncDataSize;
  setlength(pBMPFile.CompData,pBMPFile.CompSize);
  MoveMemory(@pBMPFile.CompData[0],@_EncData[0],pBMPFile.CompSize);

  pBMPFile.CompSize:=(pBMPFile.CompSize+3) and not 3;
  setlength(pBMPFile.CompData,pBMPFile.CompSize);

  FreeMem(_EncData,_EncDataSize);
end;

// ----------------------------------------------------------------

procedure TMain.FormCreate(Sender: TObject);
begin
  StartPath:=ExtractFilePath(Application.ExeName);

  StartupTimer.Enabled:=True;
end;

procedure TMain.StartupTimerTimer(Sender: TObject);
var
  wfs:TFileStream;
begin
  StartupTimer.Enabled:=False;

  BMPFile.Filename:='langerr.bmp';

//  if bmp2b15(BMPFile)=False then ShowMessage('bmp2b15 error.');
//  BMPFile.Filename:=ChangeFileExt(BMPFile.Filename,'.b15zlib');

  if bmp2b8(BMPFile)=False then ShowMessage('bmp2b8 error.');
  BMPFile.Filename:=ChangeFileExt(BMPFile.Filename,'.b8zlib');

  zlibCompress(BMPFile);

  wfs:=TFileStream.Create(BMPFile.Filename,fmCreate);
  wfs.WriteBuffer(BMPFile.CompData[0],BMPFile.CompSize);

  wfs.Free;

  Application.Terminate;
end;

end.
