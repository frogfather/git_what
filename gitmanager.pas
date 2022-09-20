unit gitManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,config,process,fileUtilities;
type
  
  { TGitWhat }

  TGitWhat = class(TInterfacedObject)
    private
    fProcess:TProcess;
    fConfig: TConfig;
    fCodeDirectoryChanged:TNotifyEvent;
    fReposChanged:TNotifyEvent;
    fCurrentRepoChanged:TNotifyEvent;
    fCurrentBranchChanged:TNotifyEvent;
    function loadConfig(configFilename:string):TConfig;
    procedure saveConfig(configFileName: String);
    function getCodeDirectory:string;
    function getCurrentRepoName:string;
    procedure setCodeDirectory(codeDirectory:string);
    procedure setCurrentRepoName(currentRepoName_:string);
    procedure directoryChanged(sender:TObject);
    procedure repoListChanged(sender:TObject);
    procedure currentRepoChanged(sender:TObject);
    procedure currentBranchChanged(sender:TObject);
    property config: TConfig read fConfig;
    public
    constructor create(configFilename:string;onCodeDirectoryChanged,onReposChanged,onCurrentRepoChanged,onCurrentBranchChanged:TNotifyEvent);
    procedure rescanRepos;
    function getRepoNames:TStringlist;
    function gitLog: TStringList;
    function executeCommand(repo,command:string):TStringlist;
    property codeDirectory: string read getCodeDirectory write setCodeDirectory;
    property currentRepoName: string read getCurrentRepoName write setCurrentRepoName;
  end;

implementation

{ TGitWhat }

constructor TGitWhat.create(configFilename:string;onCodeDirectoryChanged,onReposChanged,onCurrentRepoChanged,onCurrentBranchChanged:TNotifyEvent);
begin
  fConfig:=loadConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fReposChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fCurrentBranchChanged:=onCurrentBranchChanged;
end;

procedure TGitWhat.rescanRepos;
begin
  config.rescanRepos;
end;

function TGitWhat.getRepoNames: TStringlist;
begin
  result:=config.repoNames;
end;

function TGitWhat.gitLog: TStringList;
begin
  result:=executeCommand('/Users/cloudsoft/Code/housekeeper-app','git checkout account-view');
end;

function TGitWhat.executeCommand(repo, command: string): TStringlist;
  begin
  result:=TStringlist.Create;
  if fProcess = nil then fProcess:=TProcess.Create(Nil);
  if not directoryExists('.git') then exit;
  fProcess.Executable := '/bin/sh';
  fProcess.Parameters.Add('-c');
  fProcess.Parameters.Add(command);
  fProcess.Options := fProcess.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
  fProcess.Execute;
  result.LoadFromStream(fProcess.Output);
  end;

function TGitWhat.loadConfig(configFilename: string): TConfig;
begin
  if (fileExists(configFileName))
     then result:= TConfig.Create(@directoryChanged,@repoListChanged,@currentRepoChanged,@currentBranchChanged,openFileAsArray(configFileName, #$0A))
     else result:= TConfig.Create(@directoryChanged,@repoListChanged,@currentRepoChanged,@currentBranchChanged);
end;

procedure TGitWhat.saveConfig(configFileName: String);
var
  configArray:TStringArray;
  fileContents:string;
  index:integer;
begin
  fileContents:='';
  configArray:=config.toStringArray; //need config to xmlDocument method as well
  for index:=0 to pred(length(configArray)) do
    begin
    fileContents:=fileContents+configArray[index];
    if (index < pred(length(configArray))) then
    fileContents:=fileContents+ #$0A;
    end;
  writeStream(configFileName, fileContents);
end;

function TGitWhat.getCodeDirectory: string;
begin
  result:=config.codeDirectory;
end;

function TGitWhat.getCurrentRepoName: string;
begin
  result:=config.currentRepoName;
end;

procedure TGitWhat.setCodeDirectory(codeDirectory: string);
begin
  config.codeDirectory:=codeDirectory;
end;

procedure TGitWhat.setCurrentRepoName(currentRepoName_: string);
begin
  config.currentRepoName:=currentRepoName_;
end;

procedure TGitWhat.directoryChanged(sender: TObject);
begin
  fCodeDirectoryChanged(self);
end;

procedure TGitWhat.repoListChanged(sender: TObject);
begin
  fReposChanged(self);
end;

procedure TGitWhat.currentRepoChanged(sender: TObject);
var
  branches:TStringlist;
  index:integer;
begin
  //get branches
  branches:= executeCommand(config.currentRepoName, 'git branch');
  for index:= 0 to pred(branches.Count) do
    begin
    if (branches[index].Substring(0,1) = '*') then
       config.currentBranchName:=branches[index].Substring(1);
    end;
  fCurrentRepoChanged(self);
end;

procedure TGitWhat.currentBranchChanged(sender: TObject);
begin
  fcurrentBranchChanged(self);
end;



end.

