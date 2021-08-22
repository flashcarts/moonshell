// Ç±ÇÃÉÜÉjÉbÉgÇ…ä‹Ç‹ÇÍÇÈä÷êîÇÕÅAÉCÉìÉ^Å[ÉtÉFÅ[ÉXïœçXÇµÇ»Ç¢Ç≈â∫Ç≥Ç¢ÅB

unit _m_Tools;

interface

uses
  Windows,ShlObj,Messages,Clipbrd,ActiveX,SysUtils,Controls,ComObj,Classes,Graphics,ExtCtrls,ShellAPI;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

type
  TDriveInfo=record
    Enabled:boolean;
    Drive:string;
    VolumeLabel:string;
    VolumeSerial,MaxFNameLen,FileSys:dword;
  end;

const BELOW_NORMAL_PRIORITY_CLASS=$00004000;
const ABOVE_NORMAL_PRIORITY_CLASS=$00008000;

function GetMDXTitle(MDXFilename:string):string;
function GetMDXTitleBuf(buf:PByteArray;bufcount:integer;Filename:string):string;
function GetPDXFilename(MDXFilename:string):string;

procedure SetPriorityLevel(Level:integer);

function GetCRC16(var src:array of Byte;const len:integer):word;
function GetStrCRC16(var src:string):word;
function GetStrHash(var src:string):word;

procedure DeleteFolder(Path:string);
procedure CopyFolder(SrcPath,DstPath:string);
function GetFileSize(Filename:string):integer;
function GetDriveInfomation(Path:string):TDriveInfo;
function GetDriveTypeName(drv:string):string;

function GetDesktopPath:string;
function isAnkChar(c:byte):boolean;

procedure SetX68kPlusCode;
function strCnvX68kPlusSJIS(mststr:string):string;
function strClearESC(mststr:string):string;
function strCnvAnk2to1byte(mststr:string):string;
function strGetCompact(Master:string):string;

function MChangeFileExt(Path,ext:string):string;
function thExpandFileName(Filename:string):string;
function thExtractFilePath(Filename:string):string;
function thExtractFilename(Filename:string):string;
function thExtractFileExt(Filename:string):string;
function thChangeFileExt(Filename,ext:string):string;

var
  X68kPlusCodeStr:string;

implementation

uses _const,_SimpleDialog;

const WM_CLOSE=$0010;

function GetMDXTitle(MDXFilename:string):string;
var
  tf:TextFile;
  ttl:string;
begin
  if FileExists(MDXFilename)=False then begin
    Result:='';
    exit;
  end;

  AssignFile(tf, MDXFilename);
  Reset(tf);
  Readln(tf, ttl);
  CloseFile(tf);

  Result:=ttl;
end;

function GetMDXTitleBuf(buf:PByteArray;bufcount:integer;Filename:string):string;
var
  bufpos:integer;
  c:char;
  function GetBufChar:char;
  begin
    Result:=char(buf[bufpos]);
    inc(bufpos);
  end;
begin
  Result:='';
  bufpos:=0;
  c:=GetBufChar;
  while ((c<>CR) and (c<>LF) and (c<>char($00))) do begin
    Result:=Result+c;
    c:=GetBufChar;
  end;

  if trim(Result)='' then Result:='['+ExtractFilename(Filename)+']';
end;

function GetPDXFilename(MDXFilename:string):string;
var
  fs:TFileStream;
  res:byte;
  pos:integer;
  endflag:boolean;
  function GetByte(var res:byte):boolean;
  begin
    fs.ReadBuffer(res,1);
    if fs.Position>=fs.Size then begin
      fs.Free;
      Result:=False;
      end else begin
      Result:=True;
    end;
  end;
begin
  Result:='';

  fs:=TFileStream.Create(MDXFilename,fmOpenRead or fmShareDenyNone);
  res:=$00;
  endflag:=False;
  pos:=0;
  while (endflag=False) do begin
    endflag:=True;
    fs.Position:=pos;
    if GetByte(res)=False then exit;
    if res<>$0d then endflag:=False;
    if GetByte(res)=False then exit;
    if res<>$0a then endflag:=False;
    if GetByte(res)=False then exit;
    if res<>$1a then endflag:=False;
    inc(pos);
  end;

  if GetByte(res)=False then exit;
  while (res<>$00) do begin
    Result:=Result+char(res);
    if length(Result)>=127 then begin
      fs.Free;
      Result:='';
      exit;
    end;
    if GetByte(res)=False then begin
      fs.Free;
      Result:='';
      exit;
    end;
  end;
  fs.Free;

  if copy(Result,1,1)='\' then Result:=copy(Result,2,length(Result));

{$ifdef mdxwin}
  if Result='' then Result:='bos.pdx';
{$endif}
end;

procedure SetPriorityLevel(Level:integer);
begin
  case Level of
    0: SetPriorityClass(GetCurrentProcess,IDLE_PRIORITY_CLASS);
    1: SetPriorityClass(GetCurrentProcess,BELOW_NORMAL_PRIORITY_CLASS);
    2: SetPriorityClass(GetCurrentProcess,NORMAL_PRIORITY_CLASS);
    3: SetPriorityClass(GetCurrentProcess,ABOVE_NORMAL_PRIORITY_CLASS);
    4: SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
    5: SetPriorityClass(GetCurrentProcess,REALTIME_PRIORITY_CLASS);
    else SetPriorityClass(GetCurrentProcess,NORMAL_PRIORITY_CLASS);
  end;
end;

// Table of CRC values for high order byte
const TableCRCHi: array[0..$ff] of Byte=(
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81,
$40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0,
$80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01,
$C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41,
$00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81,
$40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0,
$80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01,
$C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40,
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81,
$40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0,
$80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01,
$C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41,
$00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81,
$40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0,
$80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01,
$C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81,
$40
);

// Table of CRC values for low order byte
const TableCRCLo: array[0..$ff] of Byte=(
$00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05, $C5, $C4,
$04, $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA, $CB, $0B, $C9, $09,
$08, $C8, $D8, $18, $19, $D9, $1B, $DB, $DA, $1A, $1E, $DE, $DF, $1F, $DD,
$1D, $1C, $DC, $14, $D4, $D5, $15, $D7, $17, $16, $D6, $D2, $12, $13, $D3,
$11, $D1, $D0, $10, $F0, $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7,
$37, $F5, $35, $34, $F4, $3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A,
$3B, $FB, $39, $F9, $F8, $38, $28, $E8, $E9, $29, $EB, $2B, $2A, $EA, $EE,
$2E, $2F, $EF, $2D, $ED, $EC, $2C, $E4, $24, $25, $E5, $27, $E7, $E6, $26,
$22, $E2, $E3, $23, $E1, $21, $20, $E0, $A0, $60, $61, $A1, $63, $A3, $A2,
$62, $66, $A6, $A7, $67, $A5, $65, $64, $A4, $6C, $AC, $AD, $6D, $AF, $6F,
$6E, $AE, $AA, $6A, $6B, $AB, $69, $A9, $A8, $68, $78, $B8, $B9, $79, $BB,
$7B, $7A, $BA, $BE, $7E, $7F, $BF, $7D, $BD, $BC, $7C, $B4, $74, $75, $B5,
$77, $B7, $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0, $50, $90, $91,
$51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95, $94, $54, $9C, $5C,
$5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99, $59, $58, $98, $88,
$48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E, $8F, $4F, $8D, $4D, $4C, $8C,
$44, $84, $85, $45, $87, $47, $46, $86, $82, $42, $43, $83, $41, $81, $80,
$40
);

function GetCRC16(var src:array of Byte;const len:integer):word;
var
  CRCHi,CRCLo:Byte;
  CRCIndex:byte;
  cnt:integer;
begin
  CRCHi:=$FF;
  CRCLo:=$FF;
//  CRCIndex:=0;

  for cnt:=0 to len-1 do begin
    CRCIndex:=CRCHi xor src[cnt];
    CRCHi:=CRCLo xor TableCRCHi[CRCIndex];
    CRCLo:=TableCRCLo[CRCIndex];
  end;

  Result:=(word(CRCHi) shl 8)+CRCLo;
end;

function GetStrCRC16(var src:string):word;
var
  CRCHi,CRCLo:Byte;
  CRCIndex:byte;
  cnt:integer;
begin
  CRCHi:=$FF;
  CRCLo:=$FF;

  for cnt:=1 to Length(src) do begin
    CRCIndex:=CRCHi xor byte(src[cnt]);
    CRCHi:=CRCLo xor TableCRCHi[CRCIndex];
    CRCLo:=TableCRCLo[CRCIndex];
  end;

  Result:=(word(CRCHi) shl 8)+CRCLo;
end;

function GetStrHash(var src:string):word;
begin
  Result:=GetStrCRC16(src);
end;

procedure DeleteFolder(Path:string);
var
  FileLst:array of string;
  FileLstMax:integer;
  cnt:integer;
  res:integer;
  SearchRec: TSearchRec;
begin
  if DirectoryExists(Path)=False then exit;
  
  res:=FindFirst(Path+'*.*', (faReadOnly or faHidden or faSysFile or faArchive), SearchRec);
  if res=0 then begin
    FileLstMax:=0;
    repeat
      SetLength(FileLst,FileLstMax+1);
      FileLst[FileLstMax]:=SearchRec.Name;
      inc(FileLstMax);
      res:=FindNext(SearchRec);
    until (res<>0);

    for cnt:=0 to FileLstMax-1 do begin
      DeleteFile(Path+FileLst[cnt]);
    end;
  end;
  FindClose(SearchRec);

  res:=FindFirst(Path+'*.*', (faDirectory or faReadOnly or faHidden or faSysFile or faArchive), SearchRec);
  if res=0 then begin
    FileLstMax:=0;
    repeat
      SetLength(FileLst,FileLstMax+1);
      if (SearchRec.Attr and faDirectory)<>0 then begin
        if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then begin
          FileLst[FileLstMax]:=SearchRec.Name;
          inc(FileLstMax);
        end;
      end;
      res:=FindNext(SearchRec);
    until (res<>0);

    for cnt:=0 to FileLstMax-1 do begin
      DeleteFolder(Path+FileLst[cnt]+'\');
    end;
  end;
  FindClose(SearchRec);

  rmdir(Path);
end;

procedure CopyFolder(SrcPath,DstPath:string);
var
  FileLst:array of string;
  FileLstMax:integer;
  cnt:integer;
  res:integer;
  SearchRec: TSearchRec;
begin
  res:=FindFirst(SrcPath+'*.*', (faReadOnly or faHidden or faSysFile or faArchive), SearchRec);
  if res=0 then begin
    FileLstMax:=0;
    repeat
      SetLength(FileLst,FileLstMax+1);
      FileLst[FileLstMax]:=SearchRec.Name;
      inc(FileLstMax);
      res:=FindNext(SearchRec);
    until (res<>0);

    if DirectoryExists(DstPath)=False then ForceDirectories(DstPath);
    for cnt:=0 to FileLstMax-1 do begin
      CopyFile(pchar(SrcPath+FileLst[cnt]),pchar(DstPath+FileLst[cnt]),False);
    end;
  end;
  FindClose(SearchRec);

  res:=FindFirst(SrcPath+'*.*', (faDirectory or faReadOnly or faHidden or faSysFile or faArchive), SearchRec);
  if res=0 then begin
    FileLstMax:=0;
    repeat
      SetLength(FileLst,FileLstMax+1);
      if (SearchRec.Attr and faDirectory)<>0 then begin
        if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then begin
          FileLst[FileLstMax]:=SearchRec.Name;
          inc(FileLstMax);
        end;
      end;
      res:=FindNext(SearchRec);
    until (res<>0);

    for cnt:=0 to FileLstMax-1 do begin
      CopyFolder(SrcPath+FileLst[cnt]+'\',DstPath+FileLst[cnt]+'\');
    end;
  end;
  FindClose(SearchRec);
end;

function GetFileSize(Filename:string):integer;
var
  fs:TFileStream;
begin
  if FileExists(Filename)=False then begin
    Result:=0;
    exit;
  end;
  try
    fs:=TFileStream.Create(Filename,fmOpenRead);
    Result:=fs.Size;
    fs.Free;
    except else Result:=0;
  end;
end;

function GetDriveInfomation(Path:string):TDriveInfo;
var
  Buf: array [0..MAX_PATH] of Char;
begin
  Buf[0]:=char($00);
  Result.Drive:=char(Path[1])+':\';

  if GetVolumeInformation(PChar(Result.Drive), Buf, DWORD(sizeof(Buf)), @Result.VolumeSerial, Result.MaxFNameLen, Result.FileSys, nil, 0)=True then begin
    Result.VolumeLabel:=Buf;
    Result.Enabled:=True;
    end else begin
    Result.VolumeLabel:='';
    Result.Enabled:=False;
  end;
end;

function GetDriveTypeName(drv:string):string;
begin
  if drv='' then begin
    Result:='unknown! ';
    end else begin
    case GetDriveType(addr(drv[1])) of
      0:               Result:='unknown! '; // ÉhÉâÉCÉuÇÃéÌóﬁÇ™îªífÇ≈Ç´Ç‹ÇπÇÒÅB
      1:               Result:='No Root! '; // ÉãÅ[ÉgÉfÉBÉåÉNÉgÉäÇ™ë∂ç›ÇµÇ‹ÇπÇÒÅB
      DRIVE_REMOVABLE: Result:='Removable';
      DRIVE_FIXED:     Result:='Fixed-HDD';
      DRIVE_REMOTE:    Result:='Network  ';
      DRIVE_CDROM:     Result:='CD-ROM   ';
      DRIVE_RAMDISK:   Result:='RAM-Disk ';
      else             Result:='noDefine!';
    end;
  end;
end;

function GetDesktopPath:string;
var
  pidl:PItemIDList;
  buf:array [0..MAX_PATH] of Char;
  m:IMalloc;
begin
  OleCheck(CoGetMalloc(1, m));
  OleCheck(SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, pidl));
  try
    Assert(SHGetPathFromIDList(pidl, buf));
    Result := buf;
  finally
    m.Free(pidl);
  end;
end;

function isAnkChar(c:byte):boolean;
begin
  if (c<=$7f) then begin
    Result:=True;
    exit;
  end;
  if (c<=$9f) then begin
    Result:=False;
    exit;
  end;
  if (c<=$df) then begin
    Result:=True;
    exit;
  end;

  case c of
    $E0: Result:=False;
    $E1: Result:=False;
    $E2: Result:=False;
    $E3: Result:=False;
    $E4: Result:=False;
    $E5: Result:=False;
    $E6: Result:=False;
    $E7: Result:=False;
    $E8: Result:=False;
    $E9: Result:=False;
    $EA: Result:=False;
    $EB: Result:=True;
    $EC: Result:=True;
    $ED: Result:=False;
    $EE: Result:=False;
    $EF: Result:=True;
    $F0: Result:=False;
    $F1: Result:=False;
    $F2: Result:=False;
    $F3: Result:=False;
    $F4: Result:=True;
    $F5: Result:=True;
    $F6: Result:=True;
    $F7: Result:=True;
    $F8: Result:=True;
    $F9: Result:=True;
    $FA: Result:=False;
    $FB: Result:=False;
    $FC: Result:=False;
    $FD: Result:=True;
    $FE: Result:=True;
    $FF: Result:=True;
    else Result:=True;
  end;
end;

procedure SetX68kPlusCode;
begin
  if X68kPlusCodeStr='' then begin
    X68kPlusCodeStr:='';
    X68kPlusCodeStr:=X68kPlusCodeStr+'  SHSXEXETEQAKBLBSHTLFUTFFCRSOSI';
    X68kPlusCodeStr:=X68kPlusCodeStr+'DED1D2D3D4NKSNFBCNEMSBECÅ®Å©Å™Å´';
    X68kPlusCodeStr:=X68kPlusCodeStr+'  ÅIÅhÅîÅêÅìÅïÅfÅiÅjÅñÅ{ÅCÅ|ÅDÅ^';
    X68kPlusCodeStr:=X68kPlusCodeStr+'ÇOÇPÇQÇRÇSÇTÇUÇVÇWÇXÅFÅGÅÉÅÅÅÑÅH';
    X68kPlusCodeStr:=X68kPlusCodeStr+'ÅóÇ`ÇaÇbÇcÇdÇeÇfÇgÇhÇiÇjÇkÇlÇmÇn';
    X68kPlusCodeStr:=X68kPlusCodeStr+'ÇoÇpÇqÇrÇsÇtÇuÇvÇwÇxÇyÅmÅ_ÅnÅOÅQ';
    X68kPlusCodeStr:=X68kPlusCodeStr+'ÅeÇÅÇÇÇÉÇÑÇÖÇÜÇáÇàÇâÇäÇãÇåÇçÇéÇè';
    X68kPlusCodeStr:=X68kPlusCodeStr+'ÇêÇëÇíÇìÇîÇïÇñÇóÇòÇôÇöÅoÅbÅpÅ`Åè';
    X68kPlusCodeStr:=X68kPlusCodeStr+'Å_Å`ÅbÅ¢Å°ÅQÇÇüÇ°Ç£Ç•ÇßÇ·Ç„ÇÂÇ¡';
    X68kPlusCodeStr:=X68kPlusCodeStr+'  Ç†Ç¢Ç§Ç¶Ç®Ç©Ç´Ç≠ÇØÇ±Ç≥ÇµÇ∑ÇπÇª';
    X68kPlusCodeStr:=X68kPlusCodeStr+'  ÅBÅuÅvÅAÅEÉíÉ@ÉBÉDÉFÉHÉÉÉÖÉáÉb';
    X68kPlusCodeStr:=X68kPlusCodeStr+'Å[ÉAÉCÉEÉGÉIÉJÉLÉNÉPÉRÉTÉVÉXÉZÉ\';
    X68kPlusCodeStr:=X68kPlusCodeStr+'É^É`ÉcÉeÉgÉiÉjÉkÉlÉmÉnÉqÉtÉwÉzÉ}';
    X68kPlusCodeStr:=X68kPlusCodeStr+'É~ÉÄÉÅÉÇÉÑÉÜÉàÉâÉäÉãÉåÉçÉèÉìÅJÅK';
    X68kPlusCodeStr:=X68kPlusCodeStr+'ÇΩÇøÇ¬ÇƒÇ∆Ç»Ç…Ç ÇÀÇÃÇÕÇ–Ç”Ç÷ÇŸÇ‹';
    X68kPlusCodeStr:=X68kPlusCodeStr+'Ç›ÇﬁÇﬂÇ‡Ç‚Ç‰ÇÊÇÁÇËÇÈÇÍÇÎÇÌÇÒfeff';
  end;
end;

function strCnvX68kPlusSJIS(mststr:string):string;
var
  cnt:integer;
  cc0,cc1:byte;
begin
  if X68kPlusCodeStr='' then SetX68kPlusCode;

  if mststr='' then begin
    Result:='';
    exit;
  end;

  cnt:=1;
  while (cnt<=length(mststr)) do begin
    if isAnkChar(byte(mststr[cnt]))=True then begin
      inc(cnt);
      end else begin
      if (cnt+1)<=length(mststr) then begin
        cc0:=byte(mststr[cnt+0]);
        if (cc0=$80) or (($f0<=cc0) and (cc0<=$f3)) then begin
          cc1:=byte(mststr[cnt+1]);
          mststr[cnt+0]:=X68kPlusCodeStr[cc1*2+1];
          mststr[cnt+1]:=X68kPlusCodeStr[cc1*2+2];
        end;
      end;
      inc(cnt,2);
    end;
  end;

  Result:=mststr;
end;

function strClearESC(mststr:string):string;
var
  cnt:integer;
  ready:boolean;
  scnt:integer;
  nowESC:boolean;
  cc:char;
begin
  ready:=False;
  for cnt:=1 to Length(mststr)-1 do begin
    if byte(mststr[cnt])=$1b then ready:=True;
  end;
  if ready=False then begin
    Result:=mststr;
    exit;
  end;

  Result:='';
  nowESC:=False;
  scnt:=1;
  while (scnt<=length(mststr)) do begin
    cc:=mststr[scnt];
    if isAnkChar(byte(cc))=False then begin
      if (scnt+1)<=length(mststr) then Result:=Result+cc+mststr[scnt+1];
      inc(scnt,2);
      end else begin
      if nowESC=False then begin
        if cc=char($1b) then begin
          nowESC:=True;
          end else begin
          Result:=Result+cc;
        end;
        end else begin
        if ('a'<=cc) and (cc<='z') then nowESC:=False;
        if ('A'<=cc) and (cc<='Z') then nowESC:=False;
        if ('@'=cc) or (cc='*') then nowESC:=False;
      end;
      inc(scnt,1);
    end;
  end;
end;

const DoubleAnk='Å@ÇOÇPÇQÇRÇSÇTÇUÇVÇWÇXÇ`ÇaÇbÇcÇdÇeÇfÇgÇhÇiÇjÇkÇlÇmÇnÇoÇpÇqÇrÇsÇtÇuÇvÇwÇxÇyÇÅÇÇÇÉÇÑÇÖÇÜÇáÇàÇâÇäÇãÇåÇçÇéÇèÇêÇëÇíÇìÇîÇïÇñÇóÇòÇôÇö';
const SingleAnk=' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

function strCnvAnk2to1byte(mststr:string):string;
var
  ready:boolean;
  cnt:integer;
  ccp:integer;
  cc1,cc2:char;
begin
  if mststr='' then begin
    Result:='';
    exit;
  end;

  ready:=False;
  for cnt:=1 to Length(mststr)-1 do begin
    if (byte(mststr[cnt])=$82) and (byte(mststr[cnt+1])<=$9e) then ready:=True;
  end;
  if ready=False then begin
    Result:=mststr;
    exit;
  end;

  cnt:=1;
  Result:='';

  // ÇQÉoÉCÉgANK -> ÇPÉoÉCÉgANKïœä∑
  while (cnt<=length(mststr)) do begin
    cc1:=mststr[cnt];
    if isAnkChar(byte(cc1))=True then begin
      Result:=Result+cc1;
      inc(cnt);
      end else begin
      if (cnt+1)<=length(mststr) then begin
        cc2:=mststr[cnt+1];
        if (byte(cc1)<>$82) or ($9f<=byte(cc2)) then begin
          Result:=Result+cc1+cc2;
          end else begin
          ccp:=AnsiPos(cc1+cc2,DoubleAnk);
          if ccp=0 then begin
            Result:=Result+cc1+cc2;
            end else begin
            Result:=Result+SingleAnk[((ccp-1) div 2)+1];
          end;
        end;
      end;
      inc(cnt,2);
    end;
  end;
end;

function strGetCompact(Master:string):string;
var
  GetTitle:string;
  GetTitleCount:integer;
  cnt:integer;
  testchar:char;
  SpaceFlag:boolean;
  WideCharFlag:boolean;
  WideTopChar:char;
begin
  if Master='' then begin
    Result:='';
    exit;
  end;

  SpaceFlag:=False;
  WideCharFlag:=False;
  WideTopChar:=char($00);
  GetTitle:=StringofChar(#0,length(Master)+1024);
  GetTitleCount:=0;
  cnt:=1;

  while (cnt<=length(Master)) do begin
    testchar:=Master[cnt];
    if WideCharFlag=True then begin
      WideCharFlag:=False;
      if ((WideTopChar=char($81)) and (testchar=char($40))) then begin
        if SpaceFlag=False then begin
          WideTopChar:=char($00);
          testchar:=char($20);
          SpaceFlag:=True;
          end else begin
          WideTopChar:=char($00);
          testchar:=char($00);
        end;
        end else begin
        SpaceFlag:=False;
      end;
      if WideTopChar<>char($00) then begin
        inc(GetTitleCount);
        GetTitle[GetTitleCount]:=WideTopChar;
      end;
      end else begin
      if isAnkChar(byte(testchar))=False then begin
        WideCharFlag:=True;
        WideTopChar:=testchar;
        testchar:=char($00);
        end else begin
        if (testchar=char(13)) or (testchar=char(10)) or (testchar=char($09)) or (testchar=' ') then begin
          if SpaceFlag=False then begin
            testchar:=char($20);
            SpaceFlag:=True;
            end else begin
            testchar:=char($00);
          end;
          end else begin
          if testchar='|' then begin
            if SpaceFlag=False then SpaceFlag:=True;
            end else begin
            SpaceFlag:=False;
          end;
        end;
      end;
    end;
    if testchar<>char($00) then begin
      inc(GetTitleCount);
      GetTitle[GetTitleCount]:=testchar;
    end;
    inc(cnt);
  end;
  if WideCharFlag=True then dec(GetTitleCount);

  if GetTitleCount=0 then begin
    Result:='';
    end else begin
    if GetTitle[1]<>' ' then begin
      Result:=copy(GetTitle,1,GetTitleCount);
      end else begin
      Result:=copy(GetTitle,2,GetTitleCount-1);
    end;
  end;
end;

function MChangeFileExt(Path,ext:string):string;
var
  cnt:integer;
  dotcnt:integer;
begin
  if Path='' then exit;

  dotcnt:=-1;
  for cnt:=1 to length(Path) do begin
    if Path[cnt]='.' then dotcnt:=cnt;
  end;

  if dotcnt=-1 then begin
    Result:=ext;
    end else begin
    Result:=copy(Path,1,dotcnt-1)+ext;
  end;
end;

function thExpandFileName(Filename:string):string;
var
  tmpdrv:char;
begin
  Result:=Filename;
  if Result='' then exit;
  tmpdrv:=Result[1];

  if ('0'<=tmpdrv) and (tmpdrv<='9') then begin
    ShowMessage('DebugMessage','thExpandFileNameÇÕÉÜÅ[ÉUÅ[íËã`ÉpÉXÇÉTÉ|Å[ÉgÇµÇƒÇ¢Ç‹ÇπÇÒÅB');
    Result:='';
    exit;
  end;

  ShowMessage('DebugMessage','åƒÇ—èoÇµãKñÒà·îΩÇ≈Ç∑ÅBthExpandFileNameÇÕégÇÌÇÍÇ»Ç≠Ç»ÇËÇ‹ÇµÇΩÅB');

  if (tmpdrv='#') then begin
    Result[1]:='A';
    Result:=ExpandUNCFileName(Result);
    Result[1]:=tmpdrv;
    end else begin
    Result:=ExpandUNCFileName(Result);
  end;
end;

function thExtractFilePath(Filename:string):string;
var
  tmpdrv:char;
begin
  Result:=Filename;
  if Result='' then exit;
  tmpdrv:=Result[1];

  if (tmpdrv='#') or (('0'<=tmpdrv) and (tmpdrv<='9')) then begin
    Result[1]:='A';
    Result:=ExtractFilePath(Result);
    Result[1]:=tmpdrv;
    end else begin
    Result:=ExtractFilePath(Result);
  end;
end;

function thExtractFilename(Filename:string):string;
var
  tmpdrv:char;
begin
  Result:=Filename;
  if Result='' then exit;

  tmpdrv:=Result[1];
  if (tmpdrv='#') or (('0'<=tmpdrv) and (tmpdrv<='9')) then begin
    Result[1]:='A';
    Result:=ExtractFilename(Result);
    end else begin
    Result:=ExtractFilename(Result);
  end;
end;

function thExtractFileExt(Filename:string):string;
var
  tmpdrv:char;
begin
  Result:=Filename;
  if Result='' then exit;

  tmpdrv:=Result[1];
  if (tmpdrv='#') or (('0'<=tmpdrv) and (tmpdrv<='9')) then begin
    Result[1]:='A';
    Result:=ExtractFileExt(Result);
    end else begin
    Result:=ExtractFileExt(Result);
  end;
end;

function thChangeFileExt(Filename,ext:string):string;
var
  tmpdrv:char;
begin
  Result:=Filename;
  if Result='' then exit;

  tmpdrv:=Result[1];
  if (tmpdrv='#') or (('0'<=tmpdrv) and (tmpdrv<='9')) then begin
    Result[1]:='A';
    Result:=ChangeFileExt(Result,ext);
    Result[1]:=tmpdrv;
    end else begin
    Result:=ChangeFileExt(Result,ext);
  end;
end;

end.

