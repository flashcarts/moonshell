program dpgenc;

uses
  Forms,
  MainWin in 'MainWin.pas' {Main},
  OptionWin in 'OptionWin.pas' {Option},
  enclogWin in 'enclogWin.pas' {enclog},
  encprvWin in 'encprvWin.pas' {encprv},
  OptionCmdLineWin in 'OptionCmdLineWin.pas' {OptionCmdLine},
  DSSupportWin in 'DSSupportWin.pas' {DSSupport},
  ShutdownWin in 'ShutdownWin.pas' {Shutdown},
  NetworkWin in 'NetworkWin.pas' {Network};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TOption, Option);
  Application.CreateForm(Tenclog, enclog);
  Application.CreateForm(Tencprv, encprv);
  Application.CreateForm(TOptionCmdLine, OptionCmdLine);
  Application.CreateForm(TDSSupport, DSSupport);
  Application.CreateForm(TShutdown, Shutdown);
  Application.CreateForm(TNetwork, Network);
  Application.Run;
end.
