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
    function loadConfig(configFilename:string):TConfig;
    procedure saveConfig(configFileName: String);
    function getCodeDirectory:string;
    procedure setCodeDirectory(codeDirectory:string);
    function getCurrentRepo:string;
    procedure setCurrentRepo(currentRepo_:string);
    procedure directoryChanged(sender:TObject);
    procedure repoListChanged(sender:TObject);
    procedure currentRepoChanged(sender:TObject);
    property config: TConfig read fConfig;
    public
    constructor create(configFilename:string;onCodeDirectoryChanged,onReposChanged,onCurrentRepoChanged:TNotifyEvent);
    procedure rescanRepos;
    function getRepoNames:TStringlist;
    function gitLog: TStringList;
    function executeCommand(repo,command:string):TStringlist;
    property codeDirectory: string read getCodeDirectory write setCodeDirectory;
    property currentRepo: string read getCurrentRepo write setCurrentRepo;
  end;

implementation

{ TGitWhat }

constructor TGitWhat.create(configFilename:string;onCodeDirectoryChanged,onReposChanged,onCurrentRepoChanged:TNotifyEvent);
begin
  fConfig:=loadConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fReposChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
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
  //should we do this in a different thread?
  if config.switchToRepo(repo) then
    begin
    if fProcess = nil then fProcess:=TProcess.Create(Nil);
    if not directoryExists('.git') then exit;
    fProcess.Executable := '/bin/sh';
    fProcess.Parameters.Add('-c');
    fProcess.Parameters.Add(command);
    fProcess.Options := fProcess.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
    fProcess.Execute;
    result:=TStringlist.Create;
    result.LoadFromStream(fProcess.Output);
    end;
end;

function TGitWhat.loadConfig(configFilename: string): TConfig;
begin
  if (fileExists(configFileName))
     then result:= TConfig.Create(@directoryChanged,@repoListChanged,@currentRepoChanged,openFileAsArray(configFileName, #$0A))
     else result:= TConfig.Create(@directoryChanged,@repoListChanged,@currentRepoChanged);
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

procedure TGitWhat.setCodeDirectory(codeDirectory: string);
begin
  config.codeDirectory:=codeDirectory;
end;

function TGitWhat.getCurrentRepo: string;
begin
  result:=config.currentRepo;
end;

procedure TGitWhat.setCurrentRepo(currentRepo_: string);
begin
  config.currentRepo:=currentRepo_;
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
  fCurrentRepoChanged(self);
end;



end.

