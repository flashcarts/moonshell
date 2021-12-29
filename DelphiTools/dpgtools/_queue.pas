unit _queue;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _m_Tools, StdCtrls,dpgenc_language,_dosbox;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

procedure SetMainTitle(msg:string);

procedure QueueInit;
function QueueGetQueueCount:integer;
function QueueGetQueueIdx:integer;
procedure QueueDecQueueCount;
procedure QueueRefreshStatus;
function isQueueLast:boolean;
procedure QueueSetResult(resmsg,errormsg:string);
procedure QueueNext;
procedure QueueAdd(fn:string);
function QueueNowEncoding:boolean;

function QueueExistsError:boolean;

procedure Current_Init;
procedure Current_SetSrcFilename(fn:string);
procedure Current_SetDstFilename(fn:string);
procedure Current_SetError(msg:string;fn:string);
function Current_GetSrcFilename:string;
function Current_GetDstFilename:string;
function Current_isError:boolean;
function Current_GetErrorMsg:string;
procedure Current_RequestCancel;
function Current_GetRequestCancel:boolean;

procedure Current_SetProgress(msg:string);

implementation

uses MainWin,_dpgfs;

const CRLF:string=char($0d)+char($0a);

procedure SetMainTitle(msg:string);
begin
  Main.Caption:=msg;
end;

var
  QueueCount,QueueIdx:integer;
  ErrorFlag:boolean;

type
  TCurrent=record
    SrcFilename,DstFilename:string;
    ErrorStr:string;
    RequestCancel:boolean;
  end;

var
  Current:TCurrent;

procedure QueueInit;
begin
  QueueCount:=0;
  QueueIdx:=0;
  ErrorFlag:=False;
end;

function QueueGetQueueCount:integer;
begin
  Result:=QueueCount;
end;

function QueueGetQueueIdx:integer;
begin
  Result:=QueueIdx;
end;

procedure QueueDecQueueCount;
begin
  dec(QueueCount);
end;

procedure QueueRefreshStatus;
begin
  Main.StatusBar1.SimpleText:=format('Total:%d Completed:%d Incomplete:%d',[QueueCount,QueueIdx,QueueCount-QueueIdx]);
end;

function isQueueLast:boolean;
begin
  if QueueIdx=(QueueCount-1) then begin
    Result:=True;
    end else begin
    Result:=False;
  end;
end;

procedure QueueSetResult(resmsg,errormsg:string);
begin
  Main.QueueGrid.Cells[0,QueueIdx]:=resmsg;
  if errormsg<>'' then Main.QueueGrid.Cells[1,QueueIdx]:=errormsg+CRLF+Current.SrcFilename;
  QueueRefreshStatus;
end;

procedure QueueNext;
begin
  inc(QueueIdx);
  Main.StandbyTimer.Enabled:=True;

  QueueRefreshStatus;
end;

procedure QueueAdd(fn:string);
begin
  if FileExists(fn)=False then exit;

  Main.QueueGrid.RowCount:=QueueCount+1;
  Main.QueueGrid.Cells[0,QueueCount]:='';
  Main.QueueGrid.Cells[1,QueueCount]:=fn;
  inc(QueueCount);

  QueueRefreshStatus;
end;

function QueueNowEncoding:boolean;
begin
  if QueueIdx<QueueCount then begin
    Result:=True;
    end else begin
    Result:=False;
  end;
end;

function QueueExistsError:boolean;
begin
  Result:=ErrorFlag;
end;

procedure Current_Init;
begin
  with Current do begin
    SrcFilename:='';
    DstFilename:='';
    ErrorStr:='';
    RequestCancel:=False;
  end;
  CreateDOSBOX_SetRequestCancel(False);
end;

procedure Current_SetSrcFilename(fn:string);
begin
  with Current do begin
    SrcFilename:=fn;
    if (SrcFilename<>'') and (DstFilename<>'') then begin
      SetBaseFilename(SrcFilename,DstFilename);
    end;
  end;
end;

procedure Current_SetDstFilename(fn:string);
begin
  with Current do begin
    DstFilename:=fn;
    if (SrcFilename<>'') and (DstFilename<>'') then begin
      SetBaseFilename(SrcFilename,DstFilename);
    end;
  end;
end;

procedure Current_SetError(msg:string;fn:string);
begin
  if fn<>'' then msg:=msg+' ['+fn+']';
  Current.ErrorStr:=msg;
  ErrorFlag:=True;
end;

function Current_GetSrcFilename:string;
begin
  Result:=Current.SrcFilename;
end;

function Current_GetDstFilename:string;
begin
  Result:=Current.DstFilename;
end;

function Current_isError:boolean;
begin
  if Current.ErrorStr='' then begin
    Result:=False;
    end else begin
    Result:=True;
  end;
end;

function Current_GetErrorMsg:string;
begin
  Result:=Current.ErrorStr;
end;

procedure Current_RequestCancel;
begin
  with Current do begin
    Main.QueueGrid.Cells[1,QueueIdx]:=lng(LI_Canceling)+CRLF+SrcFilename;
    RequestCancel:=True;
  end;
  CreateDOSBOX_SetRequestCancel(True);
end;

function Current_GetRequestCancel:boolean;
begin
  Result:=Current.RequestCancel;
end;

procedure Current_SetProgress(msg:string);
begin
  if msg='' then begin
    Main.QueueGrid.Cells[1,QueueIdx]:=Current.SrcFilename;
    end else begin
    Main.QueueGrid.Cells[1,QueueIdx]:=msg+CRLF+ExtractFilename(Current.SrcFilename);
  end;
end;

end.
