program gitwhattestproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, gitmanagerTests, gitManager, fileUtilities;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

