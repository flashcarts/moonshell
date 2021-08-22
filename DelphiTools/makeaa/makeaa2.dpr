program makeaa2;

uses
  Forms,
  makeaa2_MainWin in 'makeaa2_MainWin.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
