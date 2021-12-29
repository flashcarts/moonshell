unit LanguageSelect_DrvSelWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,_m_Tools;

type
  TDrvSel = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DriveLst: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
    TargetDrive:string;
    function CheckInstalledDrives:boolean;
  end;

var
  DrvSel: TDrvSel;

implementation

{$R *.dfm}

const LANGSETFilename='moonshl2\language.set';

function TDrvSel.CheckInstalledDrives:boolean;
var
  idx:integer;
  fn:string;
  rfs:TFileStream;
begin
  Result:=False;
{
  DrvSel.DriveLst.Items.Add('C:\ [direct]');
  DrvSel.DriveLst.ItemIndex:=0;
  Result:=True;
  exit;
}

  DrvSel.DriveLst.Clear;

  for idx:=2 to 26-1 do begin
    fn:=char($41+idx)+':\';
    if GetDriveInfomation(fn).Enabled=True then begin
      try
        rfs:=TFileStream.Create(fn+LANGSETFilename,fmOpenRead);
        rfs.Free;
        DrvSel.DriveLst.Items.Add(fn+' ['+GetDriveInfomation(fn).VolumeLabel+']');
        except else begin
        end;
      end;
    end;
  end;

  if DrvSel.DriveLst.Items.Count<>0 then begin
    DrvSel.DriveLst.ItemIndex:=DrvSel.DriveLst.Items.Count-1;
    Result:=True;
  end;
end;

procedure TDrvSel.BitBtn1Click(Sender: TObject);
begin
  TargetDrive:=copy(DriveLst.Text,1,3);
end;

end.
