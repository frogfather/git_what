unit gitManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,config,process,fileUtilities,repo;
type
  
  { TGitWhat }

  TGitWhat = class(TInterfacedObject)
    private
    fProcess:TProcess;
    fConfig: TConfig;
    fBranches: TStringlist;
    fCodeDirectoryChanged:TNotifyEvent;
    fReposChanged:TNotifyEvent;
    fCurrentRepoChanged:TNotifyEvent;
    fBranchesChanged:TNotifyEvent;
    fCurrentBranchChanged:TNotifyEvent;
    function loadConfig(configFilename:string):TConfig;
    procedure saveConfig(configFileName: String);
    function getCodeDirectory:string;
    function getCurrentRepoName:string;
    function getCurrentBranchName: string;
    procedure setCodeDirectory(codeDirectory:string);
    procedure setCurrentRepoName(currentRepoName_:string);
    procedure setCurrentBranchName(branchName_: string);
    function getCurrentRepo:TRepo;
    function getBranches:TStringlist;
    procedure directoryChanged(sender:TObject);
    procedure repoListChanged(sender:TObject);
    procedure currentRepoChanged(sender:TObject);
    procedure currentBranchChanged(sender:TObject);
    property config: TConfig read fConfig;
    public
    constructor create(
      configFilename:string;
      onCodeDirectoryChanged,
      onReposChanged,
      onCurrentRepoChanged,
      onBranchesChanged,
      onCurrentBranchChanged:TNotifyEvent);
    procedure rescanRepos;
    function getRepoNames:TStringlist;
    function executeCommand(repo,command:string):TStringlist;
    property codeDirectory: string read getCodeDirectory write setCodeDirectory;
    property currentRepoName: string read getCurrentRepoName write setCurrentRepoName;
    property currentrepo: TRepo read getCurrentrepo;
    property branches: TStringlist read Getbranches;
    property currentBranchName: string read getCurrentBranchName write setCurrentBranchName;
  end;

implementation

{ TGitWhat }

constructor TGitWhat.create(
  configFilename:string;
  onCodeDirectoryChanged,
  onReposChanged,
  onCurrentRepoChanged,
  onBranchesChanged,
  onCurrentBranchChanged:TNotifyEvent);
begin
  fConfig:=loadConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fReposChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fBranchesChanged:=onBranchesChanged;
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

function TGitWhat.executeCommand(repo, command: string): TStringlist;
  begin
  result:=TStringlist.Create;
  chdir(repo);
  if fProcess = nil then fProcess:=TProcess.Create(Nil);
  if not directoryExists('.git') then exit;
  fProcess.Parameters.Clear;
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

function TGitWhat.getCurrentBranchName: string;
begin
  result:=config.currentBranchName;
end;

procedure TGitWhat.setCodeDirectory(codeDirectory: string);
begin
  config.codeDirectory:=codeDirectory;
end;

procedure TGitWhat.setCurrentRepoName(currentRepoName_: string);
begin
  config.currentRepoName:=currentRepoName_;
end;

procedure TGitWhat.setCurrentBranchName(branchName_: string);
var
  currentBranches,branchSwitchResult :TStringlist;
  currentBranchIndex:integer;
begin
  //first check if the branch still exists and if we're on it
  currentBranches:=executeCommand(config.currentRepo.path, 'git branch --sort=-committerdate');
  currentBranchIndex:=currentBranches.IndexOf(branchName_);
  if (currentBranchIndex> -1) then
    begin
    //switch to this branch
    if (currentBranches[currentBranchIndex].Substring(1,1) = '*') then exit; //already on branch
    branchSwitchResult:=executeCommand(config.currentRepo.path, 'git checkout '+branchName_.Substring(1));
    //if result is ok
    config.currentBranchName:=branchName_;
    end;
end;

function TGitWhat.getCurrentRepo: TRepo;
begin
  result:=fConfig.currentRepo;
end;

function TGitWhat.getBranches: TStringlist;
begin

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
begin
  fBranchesChanged(self);
end;

procedure TGitWhat.currentBranchChanged(sender: TObject);
begin
  fcurrentBranchChanged(self);
end;



end.

