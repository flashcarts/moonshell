unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,_PicTools, StdCtrls;

type
  TForm1 = class(TForm)
    Img: TImage;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
  curidx:integer;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MakeBlankImg(Img,pf24bit);

  curidx:=0;

//  curidx:=90;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  x,y:integer;
  limx:integer;
  idx:integer;
  ofsy:integer;
  msg:widestring;
  procedure DrawVerFatPixel(x,y:integer;alpha:integer);
  var
    col:dword;
  begin
    col:=$ffe0e0;
    Img.Canvas.Pixels[x,y-1]:=ColorModulate(Img.Canvas.Pixels[x,y-1],col,alpha div 4);
    Img.Canvas.Pixels[x,y+0]:=ColorModulate(Img.Canvas.Pixels[x,y+0],col,alpha);
    Img.Canvas.Pixels[x,y+1]:=ColorModulate(Img.Canvas.Pixels[x,y+1],col,alpha div 4);
  end;                    
  function DrawChar(x,y,ofsy:integer;wch:widechar):integer;
  var
    bm:TBitmap;
    procedure cptrans(ox,oy:integer;alpha:integer;adc:dword);
    var
      px,py:integer;
      c:dword;
      rgb:TRGB;
    begin
      for px:=0 to 16-1 do begin
        for py:=0 to 16-1 do begin
          if bm.Canvas.Pixels[px,py]<>0 then begin
            c:=ColorModulate(Img.Canvas.Pixels[x+ox+px,y+oy+py],$000000,alpha);
            Img.Canvas.Pixels[x+ox+px,y+oy+py]:=c+adc;
          end;
        end;
      end;
    end;
    procedure cpfill(ox,oy:integer);
    var
      px,py:integer;
    begin
      for px:=0 to 16-1 do begin
        for py:=0 to 16-1 do begin
          if bm.Canvas.Pixels[px,py]<>0 then begin
            Img.Canvas.Pixels[x+ox+px,y+oy+py]:=$ffffff;
          end;
        end;
      end;
    end;
  begin
    bm:=TBitmap.Create;
    MakeBlankBM(bm,16,16,pf24bit);
    bm.Canvas.Font:=Label1.Font;
    bm.Canvas.Font.Color:=$ffffff;
    bm.Canvas.TextOut(0,0+ofsy,wch);
    bm.Canvas.TextOut(1,0+ofsy,wch);
    Result:=bm.Canvas.TextWidth(wch);

    cptrans(-1,0,96,$404040);
    cptrans(0,-1,96,$404040);
    cptrans(+1,0,96,$000000);
    cptrans(+2,0,48,$000000);
    cptrans(0,+1,96,$000000);
    cptrans(0,+2,48,$000000);
    cptrans(+1,+1,32,$000000);
    cpfill(0,0);

    bm.Free;

  end;
begin
  MakeBlankImg(Img,pf24bit);

  Img.Picture.Bitmap.LoadFromFile('BaseBG.bmp');

  limx:=curidx*2;
  msg:='ÇlÇèÇèÇéÇrÇàÇÖÇåÇå';
  x:=8;
  y:=16-4;
  for idx:=0 to 9-1 do begin
    ofsy:=16-((limx-(8+idx*12)) div 3);
    if ofsy<16 then begin
      if ofsy<0 then ofsy:=0;
      x:=x+DrawChar(x,y,ofsy,msg[idx+1]);
    end;
  end;

  y:=32-2;
  limx:=curidx*2;
  if 160<limx then limx:=160;
  for x:=0 to limx-1 do begin
    DrawVerFatPixel(x,y,256);
  end;
  DrawVerFatPixel(limx+0,y,192);
  DrawVerFatPixel(limx+1,y,128);
  DrawVerFatPixel(limx+2,y,96);
  DrawVerFatPixel(limx+3,y,64);

  Img.Canvas.Font:=Label2.Font;
  Img.Canvas.Brush.Style:=bsClear;
  msg:='ver 2.00 beta.1';
  x:=176;
  y:=32;
  Img.Canvas.Font.Color:=$808080;
  Img.Canvas.TextOut(x-1,y-1,msg);
  Img.Canvas.Font.Color:=$ffffff;
  Img.Canvas.TextOut(x+0,y+0,msg);

  Img.Refresh;

  if curidx<>0 then Img.Picture.Bitmap.Height:=32;
  Img.Picture.Bitmap.SaveToFile(format('Splash%3.3d.bmp',[curidx]));

//  Timer1.Enabled:=False;
//  exit;

  inc(curidx);
  if curidx=trunc(60*1.333) then begin
    Timer1.Enabled:=False;
    for idx:=0 to 0-1 do begin
      Img.Picture.Bitmap.SaveToFile(format('Splash%3.3d.bmp',[curidx]));
      inc(curidx);
    end;
    Close;
  end;
end;

end.
