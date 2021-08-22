unit _dpgfs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,_PicTools, StdCtrls;

var
  BaseFilename:string;

procedure SetBaseFilename(srcfn,dstfn:string);
function GetSourceFilename:string;
function GetDPGFilename:string;
function GetDPGMovieFilename:string;
function GetDPGWave1Filename:string;
function GetDPGWave2Filename:string;
function GetDPGMP2Filename:string;
function GetDPGGOPListFilename:string;
function GetExternalWaveFilename:string;
function GetTempFilename:string;
function GetTempffmpegFilename:string;

function GetExtSetFilename:string;

implementation

uses _extset;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

var
  SourceFilename:string;
  ExtSetFilename:string;

procedure SetBaseFilename(srcfn,dstfn:string);
begin
  if lowercase(ExtractFileExt(srcfn))='.ini' then begin
    ExtSetFilename:=srcfn;
    SourceFilename:=ExtendSetting_GetVideoFilename(ExtSetFilename);
    end else begin
    ExtSetFilename:='';
    SourceFilename:=srcfn;
  end;

  BaseFilename:=ChangeFileExt(dstfn,'');
end;

function GetDPGFilename:string;
begin
  Result:=BaseFilename+'.dpg';
end;

function GetDPGMovieFilename:string;
begin
  Result:=BaseFilename+'.dpgm1v';
end;

function GetDPGWave1Filename:string;
begin
  Result:=BaseFilename+'.dpg1.wav';
end;

function GetDPGWave2Filename:string;
begin
  Result:=BaseFilename+'.dpg2.wav';
end;

function GetDPGMP2Filename:string;
begin
//  Result:=BaseFilename+'.dpg.mp2';
  Result:=BaseFilename+'.dpg.ogg';
end;

function GetDPGGOPListFilename:string;
begin
  Result:=BaseFilename+'.dpg.gls';
end;

function GetExternalWaveFilename:string;
begin
  Result:=ChangeFileExt(SourceFilename,'.wav');
end;

function GetSourceFilename:string;
begin
  Result:=SourceFilename;
end;

function GetTempFilename:string;
begin
  Result:=BaseFilename+'.temp$$$';
end;

function GetTempffmpegFilename:string;
begin
  Result:=BaseFilename+'.ffmpeg.mpg';
end;

function GetExtSetFilename:string;
begin
  Result:=ExtSetFilename;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
