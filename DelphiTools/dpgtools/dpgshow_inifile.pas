unit dpgshow_inifile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, INIFiles, Menus;

procedure LoadINI;
procedure SaveINI;

var
  ShowInfomationWindow:boolean;
  ShowStatusBar:boolean;

implementation

procedure LoadINI;
var
  ini:TINIFile;
  Section:string;
begin
  ini:=TINIFile.Create(ChangeFileExt(Application.ExeName,'.ini'));

  Section:='global';

  ShowInfomationWindow:=ini.ReadBool(Section,'ShowInfomationWindow',True);
  ShowStatusBar:=ini.ReadBool(Section,'ShowStatusBar',True);

  ini.Free;
end;

procedure SaveINI;
var
  ini:TINIFile;
  Section:string;
begin
  ini:=TINIFile.Create(ChangeFileExt(Application.ExeName,'.ini'));

  Section:='global';

  ini.WriteBool(Section,'ShowInfomationWindow',ShowInfomationWindow);
  ini.WriteBool(Section,'ShowStatusBar',ShowStatusBar);

  ini.Free;
end;

end.
