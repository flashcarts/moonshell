program dpginfo;

uses
  Forms,
  dpginfo_MainWin in 'dpginfo_MainWin.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
