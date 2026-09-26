program gitwhat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, fileUtilities, config_xml_doc_handler, repo, gitManager, git_api,
  gitResponseInterface, gitResponse, xml_doc_handler, pvproject, branch,
  httpClient, pivotalApi, header, settingsForm, setting;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TfSettings, fSettings);
  Application.Run;
end.

