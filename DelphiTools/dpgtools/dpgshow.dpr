program dpgshow;

uses
  Forms,
  dpgshow_MainWin in 'dpgshow_MainWin.pas' {Main},
  dpgshow_DSSupportWin in 'dpgshow_DSSupportWin.pas' {DSSupport},
  dpgshow_info in 'dpgshow_info.pas' {Info};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TDSSupport, DSSupport);
  Application.CreateForm(TInfo, Info);
  Application.Run;
end.
