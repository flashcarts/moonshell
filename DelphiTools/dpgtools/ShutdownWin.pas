unit ShutdownWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls,_dosbox,MainWin;

type
  TShutdown = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    BitBtn1: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    TimeoutTimer: TTimer;
    BeepTimer: TTimer;
    procedure TimeoutTimerTimer(Sender: TObject);
    procedure BeepTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private 宣言 }
    TimeoutSec:integer;
    ErrorFlag:boolean;
  public
    { Public 宣言 }
    procedure ShutdownStart(_ErrorFlag:boolean);
  end;

var
  Shutdown: TShutdown;

implementation

{$R *.dfm}

procedure TShutdown.ShutdownStart(_ErrorFlag:boolean);
begin
  ErrorFlag:=_ErrorFlag;

  if ErrorFlag=False then begin
    TimeoutSec:=30;
    end else begin
    TimeoutSec:=60;
  end;
  TimeoutTimer.Enabled:=True;
  TimeoutTimerTimer(nil);

  Label3.Visible:=ErrorFlag;
  Label4.Visible:=ErrorFlag;

  ProgressBar1.Max:=TimeoutSec;
  ProgressBar1.Position:=TimeoutSec;

  if ErrorFlag=True then BeepTimer.Interval:=1000;
  BeepTimer.Enabled:=True;
end;

procedure TShutdown.TimeoutTimerTimer(Sender: TObject);
begin
  if 0<TimeoutSec then begin
    dec(TimeoutSec);
    ProgressBar1.Position:=TimeoutSec;
    Label2.Caption:=format('あと%d秒で自動シャットダウンします…',[TimeoutSec]);
  end;

  if TimeoutSec=0 then begin
    CreateDOSBOX_UseCMD(StartPath,'shutdown.exe','-s -t 1 -c "DPGエンコードが終了したので自動シャットダウンをスケジュールしました。" -f');
    ModalResult:=mrOk;
  end;
end;

function Win32APIBeep(dwFreq, dwDuration: DWORD): BOOL; stdcall; external kernel32 name 'Beep';

function ExecBeep(Freq,Time:dword):boolean;
begin
  result:=Win32APIBeep(Freq,Time);
end;

procedure TShutdown.BeepTimerTimer(Sender: TObject);
begin
  if ErrorFlag=False then begin
    ExecBeep(440,1);
    end else begin
    ExecBeep(440,50);
    sleep(50);
    ExecBeep(880,50);
  end;
end;

procedure TShutdown.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TimeoutTimer.Enabled:=False;
  BeepTimer.Enabled:=False;
end;

end.
