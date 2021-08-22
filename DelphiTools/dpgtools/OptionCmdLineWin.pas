unit OptionCmdLineWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TOptionCmdLine = class(TForm)
    CmdLineEdt: TEdit;
    PreviewMemo: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure CmdLineEdtChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private éŒ¾ }
  public
    { Public éŒ¾ }
    CmdLineFormat:string;
    procedure CmdLineRefresh;
  end;

var
  OptionCmdLine: TOptionCmdLine;

implementation

uses OptionWin;

{$R *.dfm}

procedure TOptionCmdLine.CmdLineRefresh;
var
  cmdline:string;
begin
  try
    cmdline:=format(CmdLineFormat,[4000,256,192,256*3*192,160]);
    except else
    cmdline:='Perse error.';
  end;
  PreviewMemo.Text:='Preview:>"mencoder.exe" -v - -o "output.m1v" '+cmdline;
end;

procedure TOptionCmdLine.CmdLineEdtChange(Sender: TObject);
begin
  CmdLineFormat:=CmdLineEdt.Text;
  CmdLineRefresh;
end;

procedure TOptionCmdLine.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight:=Height;
  Constraints.MaxHeight:=Height;

  Caption:='mencoder edit command line format';
  Label1.Caption:='%0:d TotalFrameCount , %1:d ScreenWidth , %2:d ScreenHeight';
  Label2.Caption:='%3:d ScreenSize(Byte) , %4:d kbit/sec(kbps)';
end;

procedure TOptionCmdLine.FormResize(Sender: TObject);
begin
  with CmdLineEdt do begin
    Width:=OptionCmdLine.ClientWidth-Left-8;
  end;
  with PreviewMemo do begin
    Width:=OptionCmdLine.ClientWidth-Left-8;
  end;

end;

end.
