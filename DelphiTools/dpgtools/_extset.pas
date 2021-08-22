unit _extset;

interface

uses
  Windows, SysUtils, Classes, Graphics;

var
  EXTSET_SourceFilename:string;

type
  TExtendSetting=record
    srcx,srcy,srcw,srch:integer;
    dstw,dsth:integer;
    Bright:integer;
    StartTimeSec,EndTimeSec:double;
  end;

function ExtendSetting_CheckFormat(fn:string):string;
function ExtendSetting_GetVideoFilename(fn:string):string;
procedure ExtendSetting_Load(fn:string;var ExtendSetting:TExtendSetting);

implementation

uses inifiles;

const CRLF:string=char($0d)+char($0a);

function ExtendSetting_CheckFormat(fn:string):string;
var
  ini:TINIFile;
  sec:string;
  id,videofn:string;
begin
  ini:=TINIFile.Create(fn);

  sec:='System';
  id:=ini.ReadString(sec,'ID','');
  videofn:=ini.ReadString(sec,'VideoFilename','');

  ini.Free;

  if id<>'dpgenc ExtendSetting format0' then begin
    Result:='未対応かバージョンの違う拡張設定ファイルです。';
    exit;
  end;

  if FileExists(ExtractFilePath(fn)+videofn)=False then begin
    Result:='リンクしている元ビデオファイルが見つかりませんでした。'+CRLF+'リンクビデオファイル名：'+videofn;
    exit;
  end;

  Result:='';
end;

function ExtendSetting_GetVideoFilename(fn:string):string;
var
  ini:TINIFile;
  sec:string;
  videofn:string;
begin
  ini:=TINIFile.Create(fn);

  sec:='System';
  videofn:=ini.ReadString(sec,'VideoFilename','');

  ini.Free;

  Result:=ExtractFilePath(fn)+videofn;
end;

procedure ExtendSetting_Load(fn:string;var ExtendSetting:TExtendSetting);
var
  ini:TINIFile;
  sec:string;
begin
  ini:=TINIFile.Create(fn);

  sec:='System';
  ini.ReadString(sec,'ID','');
  EXTSET_SourceFilename:=ini.ReadString(sec,'VideoFilename','');

  sec:='ExtendSetting';
  with ExtendSetting do begin
    srcx:=ini.ReadInteger(sec,'srcx',srcx);
    srcy:=ini.ReadInteger(sec,'srcy',srcy);
    srcw:=ini.ReadInteger(sec,'srcw',srcw);
    srch:=ini.ReadInteger(sec,'srch',srch);
    dstw:=ini.ReadInteger(sec,'dstw',dstw);
    dsth:=ini.ReadInteger(sec,'dsth',dsth);
    Bright:=ini.ReadInteger(sec,'Bright',Bright);
    StartTimeSec:=ini.ReadFloat(sec,'StartTimeSec',StartTimeSec);
    EndTimeSec:=ini.ReadFloat(sec,'EndTimeSec',EndTimeSec);
  end;

  ini.Free;
end;

end.
