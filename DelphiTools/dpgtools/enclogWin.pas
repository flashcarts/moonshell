unit enclogWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  Tenclog = class(TForm)
    loglst: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private éŒ¾ }
  public
    { Public éŒ¾ }
  end;

var
  enclog: Tenclog;

implementation

{$R *.dfm}

procedure Tenclog.FormCreate(Sender: TObject);
begin
  loglst.Clear;
end;

end.
