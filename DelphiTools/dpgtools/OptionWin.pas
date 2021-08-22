unit OptionWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, INIFiles, Menus;

type
  TOption = class(TForm)
    CancelBtn: TBitBtn;
    StartBtn: TBitBtn;
    ReencOptGroup: TGroupBox;
    ReencKBPSLst: TComboBox;
    VideoBPSLbl: TLabel;
    VideoBPSUnitLbl: TLabel;
    ReencFPSLst: TComboBox;
    VideoFPSLbl: TLabel;
    VideoFPSUnitLbl: TLabel;
    SoundGrp: TGroupBox;
    SndKBPSLst: TComboBox;
    SoundVolumeLbl: TLabel;
    SndVolumeBar: TTrackBar;
    sndVolumeLbl: TLabel;
    VideoBrightLbl: TLabel;
    ReencBrightnessBar: TTrackBar;
    ReencBrightLbl: TLabel;
    VideoBlurLbl: TLabel;
    ReencBlurLbl: TLabel;
    ReencBlurBar: TTrackBar;
    VideoBlurLightLbl: TLabel;
    ReencVerticalSwapChk: TCheckBox;
    VideoAspectLbl: TLabel;
    ReencAspectLst: TComboBox;
    SoundBPSUnitLbl: TLabel;
    SoundBPSLbl: TLabel;
    VideoBlurDeepLbl: TLabel;
    PreencLst: TComboBox;
    AdvanceChk: TCheckBox;
    PriorityLevelLbl: TLabel;
    PriorityLevelLst: TComboBox;
    ReencSmoothFrameBlendingChk: TCheckBox;
    ReencFPSAutoChk: TCheckBox;
    procedure StartBtnClick(Sender: TObject);
    procedure SndFreqOptionsChange(Sender: TObject);
    procedure ReencBarChange(Sender: TObject);
    procedure PreencLstChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AdvanceChkClick(Sender: TObject);
    procedure SndFreqLstChange(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
    procedure Init;
    procedure LoadINI;
    procedure SaveINI;
    function GetParam_SndKBPS:integer;
    function GetParam_ReencCmdLine:string;
  end;

var
  Option: TOption;

var
  ReencCmdLine_LowProfile:string;
  ReencCmdLine_HighProfile:string;

implementation

uses OptionCmdLineWin, MainWin;

{$R *.dfm}

procedure TOption.LoadINI;
var
  ini:TINIFile;
  Section:string;
  i:integer;
  procedure SetListStr(Section:string;ID:string;Lst:TComboBox;def:string);
  var
    idx:integer;
    str:string;
  begin
    str:=ini.ReadString(Section,ID,def);
    for idx:=0 to Lst.Items.Count-1 do begin
      if Lst.Items[idx]=str then Lst.ItemIndex:=idx;
    end;
  end;
  procedure SetListStr_SplitSpace(Section:string;ID:string;Lst:TComboBox;def:string);
  var
    idx:integer;
    str:string;
    tag:string;
    pos:integer;
  begin
    str:=ini.ReadString(Section,ID,def);
    for idx:=0 to Lst.Items.Count-1 do begin
      tag:=Lst.Items[idx];
      pos:=ansipos(' ',tag);
      if pos<>0 then tag:=copy(tag,1,pos-1);
      if tag=str then Lst.ItemIndex:=idx;
    end;
  end;
  procedure SetLang(lang:string);
  var
    t:TMenuItem;
    cnt,count:integer;
    tlang:string;
  begin
    lang:=ansilowercase(lang);
    t:=Main.MainMenu_Language;
    count:=t.Count;
    for cnt:=0 to count-1 do begin
      t.Items[cnt].Checked:=False;
      tlang:=t.Items[cnt].Caption;
      tlang:=copy(tlang,1,ansipos(' ',tlang)-1);
      tlang:=ansilowercase(tlang);
      if lang=tlang then t.Items[cnt].Checked:=True;
    end;
  end;
begin
  ini:=TINIFile.Create(ChangeFileExt(Application.ExeName,'.ini'));

  Section:='global';

  SetLang(ini.ReadString(Section,'Language','.JPN'));
  Main.MainFormWidth:=ini.ReadInteger(Section,'MainFormWidth',Main.MainFormWidth);
  Main.MainFormHeight:=ini.ReadInteger(Section,'MainFormHeight',Main.MainFormHeight);

  PriorityLevelLst.ItemIndex:=ini.ReadInteger(Section,'PriorityLevel',PriorityLevelLst.ItemIndex);
  AdvanceChk.Checked:=ini.ReadBool(Section,'Advance',AdvanceChk.Checked);

  Section:='option';

  Main.OptOutputPath:=ini.ReadString(Section,'OutputPath',Main.OptOutputPath);

  PreencLst.ItemIndex:=ini.ReadInteger(Section,'Preenc',PreencLst.ItemIndex);

  SetListStr_SplitSpace(Section,'SndKBPS',SndKBPSLst,'');

  i:=ini.ReadInteger(Section,'SndVolume',SndVolumeBar.Position);
  if (i<SndVolumeBar.Min) or (SndVolumeBar.Max<i) then i:=0;
  SndVolumeBar.Position:=i;

  Section:='reencoption';

  SetListStr(Section,'Aspect',ReencAspectLst,'');
  SetListStr(Section,'KBPS',ReencKBPSLst,'');
  SetListStr(Section,'FPS',ReencFPSLst,'');
  ReencFPSAutoChk.Checked:=ini.ReadBool(Section,'FPSAuto',ReencFPSAutoChk.Checked);
  ReencBrightnessBar.Position:=ini.ReadInteger(Section,'Brightness',ReencBrightnessBar.Position);
  ReencBlurBar.Position:=ini.ReadInteger(Section,'Blur',ReencBlurBar.Position);
  ReencVerticalSwapChk.Checked:=ini.ReadBool(Section,'VerticalSwap',ReencVerticalSwapChk.Checked);
  ReencSmoothFrameBlendingChk.Checked:=ini.ReadBool(Section,'SmoothFrameBlending',ReencSmoothFrameBlendingChk.Checked);

  ini.Free;
end;

procedure TOption.SaveINI;
var
  ini:TINIFile;
  Section:string;
  function GetInt_SplitSpace(str:string):integer;
  var
    pos:integer;
  begin
    pos:=ansipos(' ',str);
    if pos<>0 then str:=copy(str,1,pos-1);
    Result:=strtointdef(str,0);
  end;
  function GetLang:string;
  var
    t:TMenuItem;
    cnt,count:integer;
    tlang:string;
  begin
    t:=Main.MainMenu_Language;
    count:=t.Count;
    for cnt:=0 to count-1 do begin
      if t.Items[cnt].Checked=True then begin
        tlang:=t.Items[cnt].Caption;
        tlang:=copy(tlang,1,ansipos(' ',tlang)-1);
        Result:=tlang;
        exit;
      end;
    end;
    Result:='.$$$';
  end;
begin
  ini:=TINIFile.Create(ChangeFileExt(Application.ExeName,'.ini'));

  Section:='global';

  ini.WriteString(Section,'Language',GetLang);
  ini.WriteInteger(Section,'MainFormWidth',Main.MainFormWidth);
  ini.WriteInteger(Section,'MainFormHeight',Main.MainFormHeight);

  ini.WriteInteger(Section,'PriorityLevel',PriorityLevelLst.ItemIndex);
  ini.WriteBool(Section,'Advance',AdvanceChk.Checked);

  Section:='option';

  ini.WriteString(Section,'OutputPath',Main.OptOutputPath);

  ini.WriteInteger(Section,'Preenc',PreencLst.ItemIndex);

  ini.WriteInteger(Section,'SndKBPS',GetInt_SplitSpace(SndKBPSLst.Text));
  ini.WriteInteger(Section,'SndVolume',SndVolumeBar.Position);

  Section:='reencoption';

  ini.WriteString(Section,'Aspect',ReencAspectLst.Text);
  ini.WriteString(Section,'KBPS',ReencKBPSLst.Text);
  ini.WriteString(Section,'FPS',ReencFPSLst.Text);
  ini.WriteBool(Section,'FPSAuto',ReencFPSAutoChk.Checked);
  ini.WriteInteger(Section,'Brightness',ReencBrightnessBar.Position);
  ini.WriteInteger(Section,'Blur',ReencBlurBar.Position);
  ini.WriteBool(Section,'VerticalSwap',ReencVerticalSwapChk.Checked);
  ini.WriteBool(Section,'SmoothFrameBlending',ReencSmoothFrameBlendingChk.Checked);

  ini.Free;
end;

procedure TOption.Init;
var
  f:boolean;
begin
  SndFreqOptionsChange(nil);
  ReencBarChange(nil);

  f:=AdvanceChk.Checked;

  PreencLst.Enabled:=f;

  SoundVolumeLbl.Enabled:=f;
  SndVolumeLbl.Enabled:=f;
  SndVolumeBar.Enabled:=f;
  
  ReencFPSAutoChk.Enabled:=f;
  
  VideoBlurLbl.Enabled:=f;
  VideoBlurDeepLbl.Enabled:=f;
  VideoBlurLightLbl.Enabled:=f;
  ReencBlurLbl.Enabled:=f;
  ReencBlurBar.Enabled:=f;
  ReencVerticalSwapChk.Enabled:=f;
  ReencSmoothFrameBlendingChk.Enabled:=f;

  PriorityLevelLbl.Enabled:=f;
  PriorityLevelLst.Enabled:=f;
end;

procedure TOption.StartBtnClick(Sender: TObject);
begin
  SaveINI;
end;

procedure TOption.SndFreqOptionsChange(Sender: TObject);
begin
  sndVolumeLbl.Caption:=inttostr(sndVolumeBar.Position)+'db';
end;

procedure TOption.ReencBarChange(Sender: TObject);
begin
  ReencBrightLbl.Caption:=inttostr(ReencBrightnessBar.Position)+'%';

  if ReencBlurBar.Position=100 then begin
    ReencBlurLbl.Caption:='off';
    end else begin
    ReencBlurLbl.Caption:=inttostr(ReencBlurBar.Position)+'%';
  end;
end;

procedure TOption.PreencLstChange(Sender: TObject);
begin
  if PreencLst.ItemIndex=2 then begin
    ShowMessage('not support pre-encode by mencoder.');
    PreencLst.ItemIndex:=1;
  end;
end;

procedure TOption.CancelBtnClick(Sender: TObject);
begin
  LoadINI;
  Init;
end;

procedure TOption.FormCreate(Sender: TObject);
var
  idx:integer;
  kbps:integer;
begin
  Option.Caption:='dpgenc option';

  ReencKBPSLst.Clear;
  for idx:=1 to 1024 div 64 do begin
    kbps:=idx*64;
    ReencKBPSLst.Items.Add(inttostr(kbps));
    if kbps=512 then ReencKBPSLst.ItemIndex:=ReencKBPSLst.Items.Count-1;
  end;
//  ReencKBPSLst.ItemIndex:=10;
end;

procedure TOption.AdvanceChkClick(Sender: TObject);
begin
  Init;
end;

procedure TOption.SndFreqLstChange(Sender: TObject);
begin
  Init;
end;

function TOption.GetParam_SndKBPS:integer;
var
  str:string;
  pos:integer;
begin
  str:=SndKBPSLst.Text;
  pos:=ansipos(' ',str);
  if pos<>0 then str:=copy(str,1,pos-1);
  Result:=strtointdef(str,0);
end;

function TOption.GetParam_ReencCmdLine:string;
var
  s:string;
begin
  s:='-noautosub -nosound -frames %0:d -demuxer rawvideo -rawvideo fps=24:w=%1:d:h=%2:d:format=bgr24:size=%3:d';
  s:=s+' -ovc lavc -lavcopts vcodec=mpeg1video:vbitrate=%4:d:keyint=60';
  s:=s+':intra_matrix=8,9,12,22,26,27,29,34,9,10,14,26,27,29,34,37,12,14,18,27,29,34,37,38,22,26,27,31,36,37,38,40,26,27,29,36,39,38,40,48,27,29,34,37,38,40,48,58,29,34,37,38,40,48,58,69,34,37,38,40,48,58,69,79';
  s:=s+':inter_matrix=16,18,20,22,24,26,28,30,18,20,22,24,26,28,30,32,20,22,24,26,28,30,32,34,22,24,26,30,32,32,34,36,24,26,28,32,34,34,36,38,26,28,30,32,34,36,38,40,28,30,32,34,36,38,42,42,30,32,34,36,38,40,42,44';
  s:=s+' -ofps 24 -of rawvideo';

  Result:=s;
end;

end.
