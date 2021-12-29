program LanguageSelect;

uses
  Forms,
  LanguageSelect_MainWin in 'LanguageSelect_MainWin.pas' {Main},
  LanguageSelect_DrvSelWin in 'LanguageSelect_DrvSelWin.pas' {DrvSel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TDrvSel, DrvSel);
  Application.Run;
end.
