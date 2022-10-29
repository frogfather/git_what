unit gitManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,fileUtil,repo,fgl,dateUtils,git_api,gitResponse;
type
  
  { TGitWhat }

  TGitWhat = class(TInterfacedObject)
    private
    fCodeDirectory:String;
    fCurrentRepoName:string;
    fCurrentBranch: string;
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
    procedure setCodeDirectory(codeDirectory_:string);
    procedure setCurrentRepoName(repoName_:string);
    procedure setCurrentBranch(branchName_: string);
    procedure rescanRepos;
    procedure doRescanRepos(codeDir: String);
    function getCurrentRepo:TRepo;
    function getBranches:TStringlist;
    function onCurrentBranch(branch:string):Boolean;
    function toStringArray:TStringArray;
    property exclusions: TStringlist read fExclusions;
    public
    constructor create(
      configFilename:string;
      onCodeDirectoryChanged,
      onReposChanged,
      onCurrentRepoChanged,
      onCurrentBranchChanged:TNotifyEvent);
    function getRepoNames:TStringlist;
    procedure saveConfig(configFileName: String);
    property codeDirectory: string read fCodeDirectory write setCodeDirectory;
    property currentRepoName: string read fCurrentRepoName write setCurrentRepoName;
    property currentrepo: TRepo read getCurrentrepo;
    property branches: TStringList read Getbranches;
    property currentBranch: string read fCurrentBranch write setCurrentBranch;
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
  fCodeDirectory:='';
  fCurrentRepoName:='';
  fCurrentBranch:='';
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fRepositoriesChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fCurrentBranchChanged:=onCurrentBranchChanged;
  if (fileExists(configFileName)) then
     loadConfig(openFileAsArray(configFilename,#$0A));
end;

procedure TGitWhat.loadConfig(lines: TStringArray);
var
  index:integer;
  parts:TStringArray;
begin
  //Needs rewritten to use XML file
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
{ Actions that change state }

procedure TGitWhat.setCodeDirectory(codeDirectory_: string);
begin
  if (directoryExists(codeDirectory_)) and (fCodeDirectory <> codeDirectory_) then
  begin
    chDir(codeDirectory_);
    fCodeDirectory:=codeDirectory_;
    rescanRepos;
    fCodeDirectoryChanged(self);
  end else raise Exception.Create('Directory does not exist');
end;

procedure TGitWhat.setCurrentRepoName(repoName_: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName_, repoIndex);
  if (repoIndex > 0) and (fCurrentRepoName <> repoName_) then
    begin
    chDir(fRepositories.Data[repoIndex].path);
    fCurrentRepoName:=repoName_;
    fCurrentRepoChanged(self);
    end;
end;

procedure TGitWhat.setCurrentBranch(branchName_: string);
var
  branchResponse,changeResponse :TGitResponse;
  gitApi: TGitApi;
  branchIndex:integer;
begin
  gitApi:=TGitApi.create(currentRepo);
  branchResponse:= gitApi.getBranches;
  if not branchResponse.success then
     begin
     fCurrentBranchChanged(branchResponse);
     exit;
     end;
  branchIndex:=branchResponse.results.IndexOf(branchName_);
  if (branchIndex > -1) then
    begin
    //try to change the branch
    if not onCurrentBranch(branchResponse.results[branchIndex]) then
      begin
      changeResponse:=gitApi.changeBranch(branchName_.Substring(1));
      if not changeResponse.success then
        begin
        fCurrentBranchChanged(changeResponse);
        exit;
        end;
      end;
    fCurrentBranchChanged(gitApi.logWithDecoration);
    end;
end;
{ Commands that don't change state }

function TGitWhat.getCurrentRepo: TRepo;
var
  currentRepoIndex:integer;
begin
  result:=nil;
  fRepositories.Find(fCurrentRepoName, currentRepoIndex);
  if (currentRepoIndex > 0) then result:= fRepositories.Data[currentRepoIndex];
end;

function TGitWhat.getBranches: TStringlist;
var
  gitApi: TGitApi;
  branchResponse:TgitResponse;
begin
  gitApi:=TGitApi.create(currentRepo);
  branchResponse:=gitApi.getBranches;
  if branchResponse.success
     then result:= branchResponse.results
     else result:= TStringlist.Create;
end;

function TGitWhat.onCurrentBranch(branch: string): Boolean;
begin
  result:= branch.Substring(0,1) = '*';
end;

end.

