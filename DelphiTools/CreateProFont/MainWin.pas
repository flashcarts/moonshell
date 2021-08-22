unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,_PicTools, ExtCtrls, StdCtrls, FileCtrl,INIFiles, ComCtrls;

type
  TMain = class(TForm)
    AnkJPLabel: TLabel;
    JPUnicodeLabel: TLabel;
    PrevAImg: TImage;
    PrevWImg: TImage;
    NoJPUnicodeLabel: TLabel;
    prgbar: TProgressBar;
    iniOpenDlg: TOpenDialog;
    AnkNoJPLabel: TLabel;
    CP950ZHO_HKLabel: TLabel;
    CP936zhoLabel: TLabel;
    CP949korLabel: TLabel;
    CP998Label: TLabel;
    AnkJPFixLabel: TLabel;
    JPUnicodeFixLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

const FontHeight=12;

type
  TINI=record
    CodePageTextFilename:string;
    AnkFontHeight:integer;
    AnkYOffset:integer;
    WideFontHeight:integer;
    WideYOffset:integer;
    AutoClose:integer;
  end;

var
  INI:TINI;

var
  CodePage:integer;
  useunitbl:array[$0000..$10000-1] of boolean;

type
  TFontPro=record
    Offset:dword;
    Width:byte;
    BitStream:array of byte;
    BitSize:integer;
  end;

var
  FontPro:array[$0000..$10000] of TFontPro;

procedure LoadUnicodeTable(tblfn:string);
var
  tblstrs:TStringList;
  cnt,cnt2:integer;
  lc,uc:word;
  str:string;
  ustr,lstr:string;
  p:integer;
begin
  for cnt:=0 to $10000-1 do begin
    useunitbl[cnt]:=False;
  end;

  tblstrs:=TStringList.Create;
  tblstrs.LoadFromFile(tblfn);

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
        if (uc<>0) and (lc<>0) then useunitbl[uc]:=True;
      end;
    end;
  end;

  useunitbl[$0000]:=False;

  tblstrs.Free;
end;

function GetWidth(uc:word;var tmpbm:TBitmap):integer;
var
  x,y:integer;
  pba:array[0..16] of PByteArray;
  pb:PByteArray;
begin
  if CodePage=933 then begin
    if uc<$80 then begin
      Result:=FontHeight div 2;
      end else begin
      Result:=FontHeight div 1;
    end;
    exit;
  end;
  
//  Main.Caption:=inttohex(uc,4);

  if uc=$00 then begin
    Result:=trunc(FontHeight*0.5);
    exit;
  end;

  if uc<$20 then begin
    Result:=0;
    exit;
  end;

  if uc=$20 then begin
    Result:=3;//trunc(INI.FontHeight*0.5);
    exit;
  end;

  if uc=$3000 then begin
    Result:=6;//trunc(INI.FontHeight*0.5);
    exit;
  end;

  for y:=0 to tmpbm.Height-1 do begin
    pba[y]:=tmpbm.ScanLine[y];
  end;

  for x:=tmpbm.Width-1 downto 0 do begin
    for y:=0 to tmpbm.Height-1 do begin
      pb:=pba[y];
      if pb[x]<>0 then begin
        Result:=x+1;

        case CodePage of
          932: begin
            if uc=$3000 then Result:=trunc(FontHeight*0.8); // double-byte space
            if uc=$3001 then Result:=Result*2; // [、]
            if uc=$3002 then Result:=Result*2; // [。]
            if uc=$FF0C then Result:=Result*2; // [，]
            if uc=$FF0E then Result:=Result*2; // [．]
          end;
          933: begin
            if uc=$3000 then Result:=trunc(FontHeight*0.8); // double-byte space
            if uc=$3001 then Result:=Result*2; // [、]
            if uc=$3002 then Result:=Result*2; // [。]
            if uc=$FF0C then Result:=Result*2; // [，]
            if uc=$FF0E then Result:=Result*2; // [．]
          end;
        end;

        exit;
      end;
    end;
  end;


  Result:=0;
end;

procedure SetFontPro(ccnt:integer;var tmpbm:TBitmap;w:integer);
var
  idx:integer;
  x,y:integer;
  pb:PByteArray;
begin
  with FontPro[ccnt] do begin
    Offset:=0;

    Width:=w;
    BitSize:=16*FontHeight;
    setlength(BitStream,BitSize);
    for idx:=0 to BitSize-1 do begin
      BitStream[idx]:=0;
    end;

    for y:=0 to FontHeight-1 do begin
      pb:=tmpbm.ScanLine[y];
      for x:=0 to w-1 do begin
        BitStream[(y*16)+x]:=pb[x];
      end;
    end;
  end;

end;

const WidthsCountsMax=32;
var
  WidthsCounts:array[0..WidthsCountsMax] of integer;

procedure WriteFontPro(binfn:string);
var
  wfs:TFileStream;
  ccnt:integer;
  lastpos:integer;
  str:string;
  cnt:integer;
  procedure wu8(b:integer);
  begin
    wfs.WriteBuffer(b,1);
  end;
  procedure wu16(w:integer);
  begin
    wfs.WriteBuffer(w,2);
  end;
  procedure wu32(dw:dword);
  begin
    wfs.WriteBuffer(dw,4);
  end;
  procedure wbs1bpp(Width:integer);
  var
    bcnt:integer;
    bitimage:word;
    bitcount:integer;
  begin
    if FontPro[ccnt].BitSize=0 then exit;

    bitimage:=0;
    bitcount:=0;

    with FontPro[ccnt] do begin
      for bcnt:=0 to BitSize-1 do begin
        if BitStream[bcnt]<>0 then begin
          bitimage:=bitimage or (1 shl bitcount);
        end;
        inc(bitcount);
        if bitcount=16 then begin
          if Width<=8 then begin
            wu8(bitimage);
            end else begin
            wu16(bitimage);
          end;
          bitimage:=0;
          bitcount:=0;
        end;
      end;
    end;

    if bitcount<>0 then begin
      ShowMessage('wbs1bpp: unknown bitsize?');
      wu16(bitimage);
    end;
  end;
begin
  wfs:=TFileStream.Create(binfn,fmCreate);

  wfs.Position:=0;

  wfs.Size:=$10000;
  wfs.Position:=wfs.Size;

  wu16(FontHeight);

  for ccnt:=0 to $10000-1 do begin
    with FontPro[ccnt] do begin
      if BitSize=0 then begin
        Offset:=0;
        end else begin
        Offset:=wfs.Position-$10000;
        if 16<Width then begin
          ShowMessage('Width overflow. '+inttohex(ccnt,4));
        end;
        wu16(Width);
        wbs1bpp(Width);
      end;
    end;
  end;

  wfs.Position:=0;

  lastpos:=0;

  if CodePage<>933 then begin // Padding width
    wu8(1);
    end else begin
    wu8(0);
  end;

  for ccnt:=1 to $10000-1 do begin
    with FontPro[ccnt] do begin
      if Offset=0 then begin
        wu8(0);
        end else begin
        wu8(Offset-lastpos);
        lastpos:=Offset;
      end;
    end;
  end;

  wfs.Free;

  wfs:=TFileStream.Create('chrglyph.log',fmCreate);

  for cnt:=0 to WidthsCountsMax-1 do begin
    WidthsCounts[cnt]:=0;
  end;

  for ccnt:=0 to $10000-1 do begin
    with FontPro[ccnt] do begin
      inc(WidthsCounts[Width]);
    end;
  end;

  for cnt:=0 to WidthsCountsMax-1 do begin
    str:=inttostr(cnt)+' '+inttostr(WidthsCounts[cnt])+chr(13)+chr(10);
    wfs.WriteBuffer(str[1],length(str));
  end;

  for ccnt:=0 to $10000-1 do begin
    if useunitbl[ccnt]=True then begin
      str:='0x'+inttohex(ccnt,4)+' '+inttostr(FontPro[ccnt].Width)+chr(13)+chr(10);
      wfs.WriteBuffer(str[1],length(str));
    end;
  end;

  wfs.Free;

end;

procedure CreateUnicodeFont;
var
  ucnt:integer;

  tmpbm:TBitmap;
  w,h:integer;
  t:integer;
  isAnk:boolean;
  prevxa,prevxw:integer;

  tick:dword;

  TotalCount:integer;
  CurrentCount:integer;

  ShowUniPreview:boolean;

  procedure DrawFont1bpp;
  var
    astr:string;
    unistr:WideString;
  begin
    if isAnk=True then begin
      astr:=char(ucnt);
      TextOutA(tmpbm.Canvas.Handle,0,t,pAnsiChar(@astr[1]),length(astr));
      end else begin
      unistr:=WideChar(ucnt);
      TextOutW(tmpbm.Canvas.Handle,0,t,pWideChar(@unistr[1]),length(unistr));
    end;
  end;
  procedure DrawFont;
  begin
    if isAnk=True then begin
      t:=INI.AnkYOffset;
      case CodePage of
        932: begin
          tmpbm.Canvas.Font:=Main.AnkJPLabel.Font;
          if ucnt=integer('_') then t:=t-1;
        end;
        933: begin
          tmpbm.Canvas.Font:=Main.AnkJPFixLabel.Font;
          if ucnt=integer('_') then t:=t-1;
        end;
        else begin
          tmpbm.Canvas.Font:=Main.AnkNoJPLabel.Font;
        end;
      end;
      end else begin
      t:=INI.WideYOffset;
      case CodePage of
        932: tmpbm.Canvas.Font:=Main.JPUnicodeLabel.Font;
        933: tmpbm.Canvas.Font:=Main.JPUnicodeFixLabel.Font;
        936: begin
          tmpbm.Canvas.Font:=Main.CP936zhoLabel.Font;
          if ucnt=$3001 then t:=t+1;
          if ucnt=$3002 then t:=t+1;
          if ucnt=$ff0c then t:=t+1;
          if ucnt=$ff0e then t:=t+1;
        end;
        950: begin
          tmpbm.Canvas.Font:=Main.CP950ZHO_HKLabel.Font;
          if ucnt=$3001 then t:=t+4;
          if ucnt=$3002 then t:=t+4;
          if ucnt=$ff0c then t:=t+4;
          if ucnt=$ff0e then t:=t+4;
        end;
        949: tmpbm.Canvas.Font:=Main.CP949korLabel.Font;
        else tmpbm.Canvas.Font:=Main.NoJPUnicodeLabel.Font;
      end;
    end;
    DrawFont1bpp;
  end;
begin
  for ucnt:=$0 to $10000-1 do begin
    FontPro[ucnt].BitSize:=0;
  end;

  prevxa:=0;
  prevxw:=0;

  TotalCount:=$10000;
  CurrentCount:=0;

  main.prgbar.Position:=CurrentCount;
  main.prgbar.Max:=TotalCount;

  tick:=0;

  for ucnt:=$0000 to $10000-1 do begin
    if 250<(GetTickCount-tick) then begin
      tick:=GetTickCount;
      main.prgbar.Position:=CurrentCount;
      main.Refresh;
    end;

    if useunitbl[ucnt]=True then begin
      inc(CurrentCount);

      w:=FontHeight*4;
      h:=FontHeight;
      tmpbm:=TBitmap.Create;
      MakeBlankBM(tmpbm,w,h,pf8bit);
      CreateGrayscalePalette(tmpbm);

      if ucnt<$100 then begin
        isAnk:=True;
        end else begin
        isAnk:=False;
      end;

      DrawFont;

      w:=GetWidth(ucnt,tmpbm);
      if w<>0 then begin
        SetFontPro(ucnt,tmpbm,w);

        if ucnt<$0100 then begin
          if prevxa<Main.PrevAImg.Width then begin
            BitBlt(Main.PrevAImg.Canvas.Handle,prevxa,0,w,h,tmpbm.Canvas.Handle,0,0,SRCCOPY);
            Main.PrevAImg.Refresh;
            inc(prevxa,w);
          end;
        end;

        if $0100<=ucnt then begin
          ShowUniPreview:=False;
{
          if CodePage=0 then ShowUniPreview:=True;
          if CodePage=874 then ShowUniPreview:=True;
          if (900<=CodePage) and (CodePage<=999) then begin
            if (ucnt and $001f)=0 then ShowUniPreview:=True;
          end;
}
          if ucnt=$8072 then ShowUniPreview:=True;
          if ucnt=$ff0c then ShowUniPreview:=True;
          if ucnt=$ff0e then ShowUniPreview:=True;
          if ucnt=$3002 then ShowUniPreview:=True;
          if ucnt=$3001 then ShowUniPreview:=True;

          if ShowUniPreview=True then begin
            if prevxw<Main.PrevWImg.Width then begin
              Main.PrevWImg.Canvas.Pen.Color:=$808080;
              Main.PrevWImg.Canvas.MoveTo(prevxw,0);
              Main.PrevWImg.Canvas.LineTo(prevxw,12);
              inc(prevxw,1);
              BitBlt(Main.PrevWImg.Canvas.Handle,prevxw,0,w,h,tmpbm.Canvas.Handle,0,0,SRCCOPY);
              Main.PrevWImg.Refresh;
              inc(prevxw,w);
            end;
          end;
        end;
      end;

      tmpbm.Free;
    end;

  end;

  main.prgbar.Position:=0;
end;

procedure LoadINI(inifn:string);
var
  INIFile:TINIFile;
  Section:String;
begin
  INIFile:=TINIFile.Create(inifn);

  Section:='setting';

  with INI do begin
    CodePageTextFilename:=INIFile.ReadString(Section,'CodePageTextFilename','CP0.TXT');
    AnkFontHeight:=INIFile.ReadInteger(Section,'AnkFontHeight',12);
    AnkYOffset:=INIFile.ReadInteger(Section,'AnkYOffset',-3);
    WideFontHeight:=INIFile.ReadInteger(Section,'WideFontHeight',12);
    WideYOffset:=INIFile.ReadInteger(Section,'WideYOffset',0);
    AutoClose:=INIFile.ReadInteger(Section,'AutoClose',0);
  end;

  INIFile.Free;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  inifn:string;
  cnt:integer;
begin
  Show;

  if ParamCount=1 then begin
    inifn:=ParamStr(1);
    end else begin
    if iniOpenDlg.Execute=False then begin
      Application.Terminate;
      exit;
    end;
    inifn:=iniOpenDlg.FileName;
  end;
  caption:=inifn;

  LoadINI(inifn);

  AnkJPLabel.Font.Height:=-INI.AnkFontHeight;
  AnkJPLabel.Font.Color:=$ffffff;
  AnkNoJPLabel.Font.Height:=-INI.AnkFontHeight;
  AnkNoJPLabel.Font.Color:=$ffffff;
  JPUnicodeLabel.Font.Height:=-INI.WideFontHeight;
  JPUnicodeLabel.Font.Color:=$ffffff;
  NoJPUnicodeLabel.Font.Height:=-INI.WideFontHeight;
  NoJPUnicodeLabel.Font.Color:=$ffffff;
  CP950ZHO_HKLabel.Font.Height:=-INI.WideFontHeight;
  CP950ZHO_HKLabel.Font.Color:=$ffffff;
  CP936zhoLabel.Font.Height:=-INI.WideFontHeight;
  CP936zhoLabel.Font.Color:=$ffffff;
  CP949korLabel.Font.Height:=-INI.WideFontHeight;
  CP949korLabel.Font.Color:=$ffffff;

  AnkJPFixLabel.Font.Height:=-INI.AnkFontHeight;
  AnkJPFixLabel.Font.Color:=$ffffff;
  JPUnicodeFixLabel.Font.Height:=-INI.WideFontHeight;
  JPUnicodeFixLabel.Font.Color:=$ffffff;

  PrevAImg.Height:=FontHeight;
  MakeBlankImg(PrevAImg,pf8bit);
  CreateGrayscalePalette(PrevAImg.Picture.Bitmap);

  PrevWImg.Height:=FontHeight;
  MakeBlankImg(PrevWImg,pf8bit);
  CreateGrayscalePalette(PrevWImg.Picture.Bitmap);

  CodePage:=strtoint(copy(changefileext(extractfilename(INI.CodePageTextFilename),''),3,255));

  LoadUnicodeTable(ExtractFilePath(inifn)+INI.CodePageTextFilename);

  CreateUnicodeFont;
  Caption:='save font';
  WriteFontPro(format('chrglyph.%3.3d',[CodePage]));

  Caption:='terminate';

  if INI.AutoClose<>0 then Application.Terminate;
end;


end.
