unit gitManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,fileUtil,repo,fgl,dateUtils,git_api;
type
  
  { TGitWhat }

  TGitWhat = class(TInterfacedObject)
    private
    fCodeDirectory:String;
    fCurrentRepoName:string;
    fNewRepositories: specialize TFPGMap<string,TRepo>;
    fRepositories: specialize TFPGMap<string,TRepo>;
    fExclusions:TStringlist;
    fCodeDirectoryChanged:TNotifyEvent;
    fRepositoriesChanged:TNotifyEvent;
    fCurrentRepoChanged:TNotifyEvent;
    fCurrentBranchChanged:TNotifyEvent;
    function updateRepositories:integer;
    function repoMissingOrUpdated(repoName: string; repo_: TRepo): boolean;
    procedure loadConfig(lines: TStringArray);
    procedure clearNewRepos;
    procedure addNewRepo(repoName: string; repo:TRepo);
    procedure addRepo(repoName: string; repo_: TRepo);
    procedure deleteRepo(repoName: string);
    procedure setCodeDirectory(codeDirectory:string);
    procedure setCurrentRepoName(currentRepoName_:string);
    procedure setCurrentBranch(branchName_: string);
    function getCurrentRepo:TRepo;
    function getBranches:TStringlist;
    function getCurrentBranch:string;
    function toStringArray:TStringArray;
    procedure directoryChanged(sender:TObject);
    procedure repoListChanged(sender:TObject);
    procedure currentRepoChanged(sender:TObject);
    procedure currentBranchChanged(sender:TObject);
    property exclusions: TStringlist read fExclusions;
    public
    constructor create(
      configFilename:string;
      onCodeDirectoryChanged,
      onReposChanged,
      onCurrentRepoChanged,
      onCurrentBranchChanged:TNotifyEvent);
    procedure rescanRepos;
    procedure doRescanRepos(codeDir: String);
    function getRepoNames:TStringlist;
    procedure saveConfig(configFileName: String);
    property codeDirectory: string read fCodeDirectory write setCodeDirectory;
    property currentRepoName: string read fCurrentRepoName write setCurrentRepoName;
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
  fCodeDirectory:='';
  fRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fNewRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fRepositories.Sorted:=true;
  fExclusions:=TStringlist.Create;
  fExclusions.Add('node_modules');
  fExclusions.Add('lib');
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fRepositoriesChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fCurrentBranchChanged:=onCurrentBranchChanged;
  loadConfig(openFileAsArray(configFilename,#$0A));
end;

procedure TGitWhat.loadConfig(lines: TStringArray);
var
  index:integer;
  parts:TStringArray;
begin
  for index:= 0 to pred(length(lines)) do
  begin
    //split each line on ,
    parts:=lines[index].Split(',');
    if (length(parts)=3) then
       addRepo(parts[0],TRepo.create(parts[1],Iso8601ToDate(parts[2])))
    else
      if (length(parts) = 2) then
      begin
      if (parts[0] = 'code_directory') then fCodeDirectory := parts[1]
        else if (parts[0] = 'current_repo') then fCurrentRepoName := parts[1]
      end;
  end;
end;

procedure TGitWhat.saveConfig(configFileName: String);
var
  configArray:TStringArray;
  fileContents:string;
  index:integer;
begin
  fileContents:='';
  configArray:= toStringArray; //need config to xmlDocument method as well
  for index:=0 to pred(length(configArray)) do
    begin
    fileContents:=fileContents+configArray[index];
    if (index < pred(length(configArray))) then
    fileContents:=fileContents+ #$0A;
    end;
  writeStream(configFileName, fileContents);
end;

procedure TGitWhat.rescanRepos;
begin
  clearNewRepos;
  doRescanRepos(codeDirectory);
  if (updateRepositories > 0) then fRepositoriesChanged(self);
end;

procedure TGitWhat.doRescanRepos(codeDir: String);
  var
  directoryList:TStringlist;
  index:integer;
  directoryName,repoName:string;
  repoNameParts:TStringArray;
  newRepo:TRepo;
  fileModified:longint;
  begin
    chdir(codeDir);
    repoNameParts:=codeDir.Split('/');
    repoName:= repoNameParts[pred(length(repoNameParts))];
    if directoryExists('.git') then
      begin
        if fileExists('.git/index') then fileModified:= FileAge('.git/index')
        else fileModified:= FileAge('.git/config');
        newRepo:=TRepo.create(codeDir,FileDateToDateTime(fileModified));
        addNewRepo(repoName,newRepo);
      end else if exclusions.IndexOf(repoName) = -1 then
      begin
      directoryList:=findAllDirectories(codeDir+'/',false);
      for index:= 0 to pred(directoryList.Count) do
        begin
        directoryName:= directoryList[index];
        doRescanRepos(directoryName);
        end;
      end;
  end;

function TGitWhat.getRepoNames: TStringlist;
var
  index:integer;
begin
  result:=TStringlist.Create;
  for index:= 0 to pred(fRepositories.Count) do
      result.add(fRepositories.Keys[index]);
end;

function TGitWhat.toStringArray: TStringArray;
var
  configLength,index:integer;
begin
  result:= TStringArray.create;
  configLength:=fRepositories.Count + 2;
  setLength(Result, configLength);
  result[0]:='code_directory,'+fCodeDirectory;
  result[1]:='current_repo,'+fCurrentRepoName;
  for index:= 0 to pred(fRepositories.Count) do
    begin
      result[index+2]:=fRepositories.Keys[index]+','+(fRepositories.Data[index]).path+','+DateToISO8601((fRepositories.Data[index]).lastUsed);
    end;
end;

function TGitWhat.updateRepositories: integer;
var
index:integer;
begin
  result:=0;
  //first remove any repos that are in fRepositories and not in fNewRepositories
  index:= 0;
    while index < fRepositories.Count do
    begin
    if (fNewRepositories.IndexOf(fRepositories.Keys[index]) = -1) then
         begin
         deleteRepo(fRepositories.Keys[index]);
         result:=result+1;
         end
       else index:=index+1;
    end;
  //now add/update other repos
  for index:= 0 to pred(fNewRepositories.Count) do
    begin
    if repoMissingOrUpdated(fNewRepositories.Keys[index],fNewRepositories.Data[index])
       then
          begin
          addRepo(fNewRepositories.Keys[index],fNewRepositories.Data[index]);
          result:=result+1;
          end;
    end;
end;

function TGitWhat.repoMissingOrUpdated(repoName: string; repo_: TRepo): boolean;
var
  foundRepo:TRepo;
begin
  result:=false;
  if (fRepositories.IndexOf(repoName) = -1) then result:=true else
    begin
    foundRepo:= fRepositories.Data[fRepositories.IndexOf(repoName)];
    result:= (foundRepo.lastUsed <> repo_.lastUsed) or (foundRepo.path <> repo_.path);
    end;
end;

procedure TGitWhat.clearNewRepos;
begin
  fNewRepositories.Clear;
end;

procedure TGitWhat.addNewRepo(repoName: string; repo: TRepo);
begin
  fNewRepositories.AddOrSetData(repoName, repo);
end;

procedure TGitWhat.addRepo(repoName: string; repo_: TRepo);
begin
  fRepositories.AddOrSetData(repoName, repo_);
end;

procedure TGitWhat.deleteRepo(repoName: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex > -1) then
     fRepositories.Delete(repoIndex);
end;

procedure TGitWhat.setCodeDirectory(codeDirectory: string);
begin
  if (directoryExists(codeDirectory)) and (fCodeDirectory <> codeDirectory) then
  begin
    fCodeDirectory:=codeDirectory;
    fCodeDirectoryChanged(self);
  end;
//else raise an error?
end;

procedure TGitWhat.setCurrentRepoName(currentRepoName_: string);
begin
  if (fCurrentRepoName <> currentRepoName_) then
  fCurrentRepoName:=currentRepoName_;
end;

procedure TGitWhat.setCurrentBranch(branchName_: string);
var
  currentBranches :TStringlist;
  gitApi: TGitApi;
begin
  currentBranches:= getBranches;
  if (currentBranches.IndexOf(branchName_) > -1) then
    begin
     gitApi:=TGitApi.create(currentRepo);
     gitApi.changeBranch(branchName_.Substring(1));
     fCurrentBranchChanged(self);
    end;
end;

function TGitWhat.getCurrentRepo: TRepo;
var
  currentRepoIndex:integer;
begin
  result:=nil;
  fRepositories.Find(fCurrentRepoName, currentRepoIndex);
  if (currentRepoIndex > - 1) then result:= fRepositories.Data[currentRepoIndex];
end;

function TGitWhat.getBranches: TStringlist;
var
  gitApi: TGitApi;
begin
  gitApi:=TGitApi.create(currentRepo);
  result:=gitApi.getBranches;
end;

function TGitWhat.getCurrentBranch: string;
var
  gitApi: TGitApi;
  branchList:TStringlist;
  index:integer;
begin
  gitApi:=TGitApi.create(currentRepo);
  branchList:=gitApi.getBranches;
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
  fRepositoriesChanged(self);
end;

procedure TGitWhat.currentRepoChanged(sender: TObject);
begin
  fCurrentRepoChanged(self);
end;

procedure TGitWhat.currentBranchChanged(sender: TObject);
begin
  fcurrentBranchChanged(self);
end;



end.

