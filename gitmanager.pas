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
    fCodeDirectoryChanged:TNotifyEvent;
    fReposChanged:TNotifyEvent;
    fCurrentRepoChanged:TNotifyEvent;
    fBranchesChanged:TNotifyEvent;
    fCurrentBranchChanged:TNotifyEvent;
    function getCodeDirectory:string;
    function getCurrentRepoName:string;
    procedure setCodeDirectory(codeDirectory:string);
    procedure setCurrentRepoName(currentRepoName_:string);
    procedure setCurrentBranch(branchName_: string);
    function getCurrentRepo:TRepo;
    function getBranches:TStringlist;
    function getCurrentBranch:string;
    function executeCommand(repo,command:string):TStringlist;
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
      onCurrentBranchChanged:TNotifyEvent);
    procedure rescanRepos;
    function getRepoNames:TStringlist;
    procedure saveConfig(configFileName: String);
    property codeDirectory: string read getCodeDirectory write setCodeDirectory;
    property currentRepoName: string read getCurrentRepoName write setCurrentRepoName;
    property currentrepo: TRepo read getCurrentrepo;
    property branches: TStringlist read Getbranches;
    property currentBranch: string read getCurrentBranch write setCurrentBranch;
  end;

implementation

{ TGitWhat }

constructor TGitWhat.create(
  configFilename:string;
  onCodeDirectoryChanged,
  onReposChanged,
  onCurrentRepoChanged,
  onCurrentBranchChanged:TNotifyEvent);
begin
  fConfig:=TConfig.create(@directoryChanged,@repolistChanged,@currentRepoChanged,@currentBranchChanged);
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fReposChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fCurrentBranchChanged:=onCurrentBranchChanged;
  fConfig.loadConfig(openFileAsArray(configFilename,#$0A));
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

procedure TGitWhat.setCurrentBranch(branchName_: string);
var
  currentBranches :TStringlist;
begin
  //first check if the branch still exists and if we're on it
  currentBranches:=executeCommand(config.currentRepo.path, 'git branch --sort=-committerdate');
  if (currentBranches.IndexOf(branchName_) > -1) then
     executeCommand(config.currentRepo.path, 'git checkout '+branchName_.Substring(1));

end;

function TGitWhat.getCurrentRepo: TRepo;
begin
  result:=fConfig.currentRepo;
end;

function TGitWhat.getBranches: TStringlist;
begin
  result:=executeCommand(config.currentRepo.path, 'git branch --sort=-committerdate');
end;

function TGitWhat.getCurrentBranch: string;
var
  branchList:TStringlist;
  index:integer;
begin
  branchList:=executeCommand(config.currentRepo.path, 'git branch --sort=-committerdate');
  for index:=0 to pred(branchList.Count) do
    if branchList[index].Substring(0,1) = '*' then
      begin
      result:=branchList[index];
      exit;
      end;
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
  repoBranches:TStringlist;
begin
  repoBranches:=branches;
  fCurrentRepoChanged(self);
end;

procedure TGitWhat.currentBranchChanged(sender: TObject);
begin
  fcurrentBranchChanged(self);
end;



end.

