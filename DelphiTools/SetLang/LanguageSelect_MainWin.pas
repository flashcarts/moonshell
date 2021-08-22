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
    { Private �錾 }
  public
    { Public �錾 }
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
  add(932,'Japanese / ���{��');
  add(933,'Japanese (Fixed) / ���{��i�Œ蕝�t�H���g�j');
  add(0,'English / �p��');
  add(1,'French / �t�����X��');
  add(2,'German / �h�C�c��');
  add(3,'Italian / �C�^���A��');
  add(4,'Spanish / �X�y�C����');
  add(5,'Portuguese / �|���g�K����');
  add(6,'Dutch / �I�����_��');
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
    ShowMessage('The disk where MoonShell2 had been installed was not found.'+CRLF+CRLF+'MoonShell2���C���X�g�[�����ꂽ�f�B�X�N��������܂���ł����B');
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

  ShowMessage('The default language was set. ['+LangSetsLst.Items[LangSetsLst.ItemIndex]+']'+CRLF+CRLF+'����ݒ��ۑ����܂����B');
  Main.Close;
end;

end.
