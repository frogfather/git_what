unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,FileUtilities, config;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCodeDirectory: TButton;
    cbCurrentRepo: TComboBox;
    eCodeDirectory: TEdit;
    lCurrentRepo: TLabel;
    lCodeDirectory: TLabel;
    pTree: TPanel;
    pDirectory: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure bCodeDirectoryClick(Sender: TObject);
    procedure eCodeDirectoryChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fConfig: TConfig;
    function loadConfig(configFileName: string):TConfig;
    procedure saveConfig(configFileName: String; config_ :TConfig);
    procedure rescanRepos(codeDir: string);
    property config: TConfig read fConfig write fConfig;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bCodeDirectoryClick(Sender: TObject);
begin
  If selectDirectoryDialog1.Execute then eCodeDirectory.Text:= selectDirectoryDialog1.FileName;
  if ((eCodeDirectory.Text <> config.codeDirectory)
     and (messageDlg('','Code directory has changed. Rescan?',mtConfirmation,[mbYes, mbNo],'') = mrYes))
     then
       begin
       config.clearRepos;
       rescanRepos(eCodeDirectory.Text);
       end;
     eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.eCodeDirectoryChange(Sender: TObject);
begin
  if (eCodeDirectory.Text <> config.codeDirectory)
     then eCodeDirectory.Font.Color:=clRed
     else eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //Read a config file containing the code directory and all git repositories
  config:= loadConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
end;

function TForm1.loadConfig(configFileName: string): TConfig;
begin
  if (fileExists(configFileName))
     then result:= TConfig.Create(openFileAsArray(configFileName, #$0A))
     else result:= TConfig.Create;
end;

procedure TForm1.saveConfig(configFileName: String; config_: TConfig);
var
  configArray:TStringArray;
  fileContents:string;
  index:integer;
begin
  fileContents:='';
  configArray:=config.toStringArray;
  for index:=0 to pred(length(configArray)) do
    begin
    fileContents:=fileContents+configArray[index];
    if (index < pred(length(configArray))) then
    fileContents:=fileContents+ #$0A;
    end;
  writeStream(configFileName, fileContents);
end;

procedure TForm1.rescanRepos(codeDir: string);
var
directoryList:TStringlist;
index:integer;
directoryName,repoName:string;
repoNameParts:TStringArray;
begin
  //starting with the specified codedirectory
  //we look in all subdirectories and check if it has
  //a .git directory
  //if it does, we don't go any deeper into that directory
  //sounds kind of recursive doesn't it?
  chdir(codeDir);
  if directoryExists('.git') then
    begin
      //add to the list of repos and don't go any deeper
      repoNameParts:=codeDir.Split('/');
      repoName:= repoNameParts[length(repoNameParts)-1];

      config.addRepo(repoName,codeDir);
    end else
    begin
    directoryList:=findDirectories(codeDir+'/');
    for index:= 0 to pred(directoryList.Count) do
      begin
      directoryName:= directoryList[index];
      rescanRepos(codeDir+'/'+directoryName);

      end;
    end;

end;

end.

