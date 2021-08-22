program dpgsplit;

uses
  Forms,
  dpgsplit_MainWin in 'dpgsplit_MainWin.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
