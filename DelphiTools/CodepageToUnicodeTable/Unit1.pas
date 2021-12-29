unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  lc2uctbl:array[$00..$100-1] of word;

procedure LoadUnicodeTable(codepage:integer);
var
  tblstrs:TStringList;
  cnt,cnt2:integer;
  lc,uc:word;
  str:string;
  ustr,lstr:string;
  p:integer;
  wfs:TFileStream;
begin
  for cnt:=0 to $100-1 do begin
    lc2uctbl[cnt]:=$ffff;
  end;

  tblstrs:=TStringList.Create;
  tblstrs.LoadFromFile(format('CP%d.TXT',[codepage]));

  for cnt:=0 to tblstrs.Count-1 do begin
    str:=tblstrs[cnt];
    if copy(str,1,2)='0x' then begin
      str:=copy(str,1,ansipos('#',str));

      p:=ansipos(char(9),str);

      lstr:=copy(str,1,p-1);
      lstr:=copy(lstr,3,4);
      lstr:=trim(lstr);

      ustr:=copy(str,p+1,6);
      ustr:=copy(ustr,3,4);
      ustr:=trim(ustr);

      if (copy(str,p+1+7,1)=chr($09)) or (copy(str,p+1+8,1)=chr($09)) then begin
        ustr:='';
      end;

      if (ustr<>'') and (lstr<>'') then begin
        uc:=strtointdef('$'+ustr,0);
        lc:=strtointdef('$'+lstr,0);
        if (uc<>0) and (lc<>0) then begin
          lc2uctbl[lc]:=uc;
        end;
      end;
    end;
  end;

  tblstrs.Free;

  wfs:=TFileStream.Create(format('cp%d.tbl',[codepage]),fmCreate);
  for cnt:=0 to $100-1 do begin
    wfs.WriteBuffer(lc2uctbl[cnt],2);
  end;
  wfs.free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadUnicodeTable(437);
  LoadUnicodeTable(850);
  LoadUnicodeTable(1252);

end;

end.
 