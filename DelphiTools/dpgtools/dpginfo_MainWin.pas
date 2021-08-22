unit dpginfo_MainWin;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls,
  Dialogs,_dpg_const;

type
  TMain = class(TForm)
    dpgOpenDlg: TOpenDialog;
    InfoMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
var
  dpgfn:string;
begin
  Application.Title:='dpginfo.exe for DPG0/1/2/3';
  Main.Caption:=Application.Title;

  dpgfn:=ParamStr(1);

  if FileExists(dpgfn)=False then begin
    if dpgOpenDlg.Execute=True then begin
      dpgfn:=dpgOpenDlg.FileName;
    end;
  end;

  if FileExists(dpgfn)=False then begin
    Application.Terminate;
    exit;
  end;

  InfoMemo.Text:=LoadDPGINFOString(dpgfn);
end;

end.
