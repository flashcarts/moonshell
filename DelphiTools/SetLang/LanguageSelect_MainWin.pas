unit LanguageSelect_MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,_m_Tools;

type
  TMain = class(TForm)
    StartupTimer: TTimer;
    SaveBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label2: TLabel;
    LangSetsLst: TListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure StartupTimerTimer(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Main: TMain;

implementation

uses LanguageSelect_DrvSelWin;

{$R *.dfm}

const LANGSETFilename='moonshl2\language.set';

var
  CodePage:integer;

// ----------------------

type
  TLangSet=record
    CodePage:integer;
    Name:string;
  end;

var
  LangSetsCount:integer;
  LangSets:array of TLangSet;

procedure LangSetInitGEN;
  procedure add(CodePage:integer;Name:string);
  begin
    setlength(LangSets,LangSetsCount+1);
    LangSets[LangSetsCount].CodePage:=CodePage;
    LangSets[LangSetsCount].Name:=Name;
    inc(LangSetsCount);
  end;
begin
  LangSetsCount:=0;
  add(932,'Japanese / 日本語');
  add(933,'Japanese (Fixed) / 日本語（固定幅フォント）');
  add(0,'English / 英語');
  add(1,'French / フランス語');
  add(2,'German / ドイツ語');
  add(3,'Italian / イタリア語');
  add(4,'Spanish / スペイン語');
  add(5,'Portuguese / ポルトガル語');
  add(6,'Dutch / オランダ語');
end;

// ----------------------

procedure TMain.FormCreate(Sender: TObject);
begin
  StartupTimer.Enabled:=True;
end;

procedure TMain.StartupTimerTimer(Sender: TObject);
var
  ModalResult:TModalResult;
  rfs:TFileStream;
  str:string;
  idx:integer;
begin
  StartupTimer.Enabled:=False;

  if DrvSel.CheckInstalledDrives=False then begin
    ShowMessage('The disk where MoonShell2 had been installed was not found.'+CRLF+CRLF+'MoonShell2がインストールされたディスクが見つかりませんでした。');
    Application.Terminate;
    exit;
  end;

  DrvSel.Caption:=Main.Caption;
  if DrvSel.ShowModal<>mrOk then begin
    Application.Terminate;
    exit;
  end;

  rfs:=TFileStream.Create(DrvSel.TargetDrive+LANGSETFilename,fmOpenRead);
  str:='000';
  rfs.ReadBuffer(str[1],3);
  CodePage:=strtoint(str);
  rfs.Free;

  LangSetInitGEN;

  LangSetsLst.Clear;
  for idx:=0 to LangSetsCount-1 do begin
    LangSetsLst.Items.Add(format('%3.3d %s',[LangSets[idx].CodePage,LangSets[idx].Name]));
    if CodePage=LangSets[idx].CodePage then LangSetsLst.ItemIndex:=idx;
  end;

end;

procedure TMain.CancelBtnClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMain.SaveBtnClick(Sender: TObject);
var
  wfs:TFileStream;
  str:string;
begin
  wfs:=TFileStream.Create(DrvSel.TargetDrive+LANGSETFilename,fmCreate);
  str:=copy(LangSetsLst.Items[LangSetsLst.ItemIndex],1,3);
  wfs.WriteBuffer(str[1],3);
  wfs.Free;

  ShowMessage('The default language was set. ['+LangSetsLst.Items[LangSetsLst.ItemIndex]+']'+CRLF+CRLF+'言語設定を保存しました。');
  Main.Close;
end;

end.
