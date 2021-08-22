unit dpgenc_language;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _m_Tools, StdCtrls;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

const LI_ID_Title=0;
const LI_ID_FontName=1;
const LI_ID_FontSize=2;
const LI_ID_FontCodepage=3;

const LI_FormSourceLabel=0;
const LI_FormSelect=1;
const LI_FormCancel=2;
const LI_FormStart=3;
const LI_FormShowOptions=4;
const LI_FormAudioOptions=5;
const LI_FormAudioBPS=6;
const LI_FormAudioBPSUnit=7;
const LI_FormAudioVolume=8;
const LI_FormVideoOptions=9;
const LI_FormVideoAspect=10;
const LI_FormVideoAspectItem0=11;
const LI_FormVideoAspectItem1=12;
const LI_FormVideoAspectItem2=13;
const LI_FormVideoAspectItem3=14;
const LI_FormVideoBPS=15;
const LI_FormVideoBPSUnit=16;
const LI_FormVideoFPS=17;
const LI_FormVideoFPSUnit=18;
const LI_FormVideoFPSAuto=19;
const LI_FormVideoBright=20;
const LI_FormVideoBlur=21;
const LI_FormVideoBlurDeep=22;
const LI_FormVideoBlurLight=23;
const LI_FormVideoFlip=24;

const LI_AboutFrameRate=25;
const LI_PipeErrorCreate=26;
const LI_PipeErrorAttribute=27;
const LI_GetAudioError=28;

const LI_Preenc_DS=29;
const LI_Preenc_ffmpeg=30;

const LI_OutputPathTag=31;
const LI_OutputPathChange=32;
const LI_OutputPathDlgTitle=33;
const LI_OutputPathDlgFilename=34;

const LI_MainMenu_Option=35;
const LI_MainMenu_ShowLog=36;
const LI_MainMenu_About=37;
const LI_MainMenu_AutoShutdown=38;

const LI_QueueMessage=39;

const LI_CancelPopup=40;
const LI_Canceling=41;
const LI_Canceled=42;

const LI_DetectErrorFPS=43;
const LI_DetectErrorWMV3=44;

const LI_OptionAdvance=45;

const LI_VideoEncodeProgressFormat=46;

const LI_SetAutoShutdownMsg=47;

procedure LoadLngFile(fn:string);
function lngID(LI_ID:integer):string;
function lng(LI:integer):string;

implementation

const CRLF:string=char($0d)+char($0a);

var
  lngstr:TStringList;

procedure LoadLngFile(fn:string);
begin
  if assigned(lngstr)=true then begin
    lngstr.Free;
    lngstr:=NIL;
  end;

  if FileExists(fn)=False then begin
    ShowMessage('FatalError:not found '+fn);
    exit;
  end;

  lngstr:=TStringList.Create;
  lngstr.LoadFromFile(fn);
end;

function lngID(LI_ID:integer):string;
begin
  if assigned(lngstr)=false then begin
    Result:='';
    exit;
  end;
  if (LI_ID<0) or (lngstr.Count<=LI_ID) then begin
    Result:='';
    exit;
  end;

  Result:=lngstr[LI_ID];
end;

function lng(LI:integer):string;
var
  pos:integer;
begin
  inc(LI,4);

  if assigned(lngstr)=false then begin
    Result:='load not .lng file';
    exit;
  end;

  if (LI<0) and (lngstr.Count<=LI) then begin
    Result:='nodef';
    exit;
  end;

  Result:=lngstr[LI];

  pos:=ansipos('\n',Result);
  while(pos<>0) do begin
    Result[pos+0]:=char($0d);
    Result[pos+1]:=char($0a);
    pos:=ansipos('\n',Result);
  end;

end;

end.
