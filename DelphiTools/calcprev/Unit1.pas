unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,_PicTools, StdCtrls, Menus;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    TopImg: TImage;
    BtmImg: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button16: TButton;
    Button20: TButton;
    Button6: TButton;
    Button7: TButton;
    Button9: TButton;
    Button10: TButton;
    Button13: TButton;
    Button14: TButton;
    Button17: TButton;
    Button18: TButton;
    Button11: TButton;
    Button12: TButton;
    Button15: TButton;
    Button8: TButton;
    Button5: TButton;
    Button19: TButton;
    Button21: TButton;
    StaticText1: TStaticText;
    ListBox1: TListBox;
    StaticText2: TStaticText;
    Edit1: TEdit;
    PopupMenu1: TPopupMenu;
    N16721: TMenuItem;
    N21: TMenuItem;
    N32101: TMenuItem;
    N4601: TMenuItem;
    N51: TMenuItem;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  MakeBlankImg(TopImg,pf24bit);
  with TopImg.Canvas do begin
    Brush.Color:=$ffffff;
    FillRect(Rect(0,0,256,192));
  end;

  MakeBlankImg(BtmImg,pf24bit);
  with BtmImg.Canvas do begin
    Brush.Color:=$808080;
    FillRect(Rect(0,0,256,192));
  end;

  Edit1.Text:='(5*8)-5*7/';
end;

end.
