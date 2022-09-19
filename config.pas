unit config;

{$mode ObjFPC}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,fgl,fileUtil,repo,dateUtils,branch;
type
  
  { TConfig }
  //TODO repositories should be an array of repository objects (TBA)
  TConfig = class(TInterfacedObject)
    private
      fCodeDirectory:String;
      fCurrentRepo:string;
      fNewRepositories: specialize TFPGMap<string,TRepo>;
      fRepositories: specialize TFPGMap<string,TRepo>;
      fRepositoriesChanged:TNotifyEvent;
      fCodeDirectoryChanged:TNotifyEvent;
      fCurrentRepoChanged:TNotifyEvent;
      fExclusions:TStringlist;
      procedure setCodeDirectory(codeDirectory_:string);
      procedure setCurrentRepo(currentRepo_:string);
      function getRepoPath(repoName:string):string;
      function getRepoNames:TStringlist;
      function repoMissingOrUpdated(repoName: string; repo_: TRepo):boolean;
      procedure deleteRepo(repoName:string);
      procedure clearNewRepos;
      procedure addRepo(repoName:string;repo_:TRepo);
      procedure doRescanRepos(codeDir:String);
      function updateRepositories:integer;
      property exclusions: TStringlist read fExclusions;
    public
    constructor create(onCodeDirectoryChanged,onRepositoriesChanged,onCurrentRepoChanged:TNotifyEvent);
    constructor create(onCodeDirectoryChanged,onRepositoriesChanged,onCurrentRepoChanged:TNotifyEvent;lines: TStringArray);
    destructor destroy;
    function toStringArray: TStringArray;
    procedure addNewRepo(repoName:string;repo: TRepo);
    procedure rescanRepos(codeDirChanged:boolean = false);
    property codeDirectory:string read fCodeDirectory write setCodeDirectory;
    property repoNames: TStringlist read getRepoNames;
    property currentRepo: string read fCurrentRepo write setCurrentRepo;
  end;

implementation

{ TConfig }

constructor TConfig.create(onCodeDirectoryChanged,onRepositoriesChanged,onCurrentRepoChanged:TNotifyEvent);
begin
  //create an empty object
  fCodeDirectory:='';
  fRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fNewRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fRepositories.Sorted:=true;
  fRepositoriesChanged:=onRepositoriesChanged;
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fExclusions:=TStringlist.Create;
  fExclusions.Add('node_modules');
  fExclusions.Add('lib');
end;

constructor TConfig.create(onCodeDirectoryChanged,onRepositoriesChanged,onCurrentRepoChanged:TNotifyEvent; lines: TStringArray);
var
  index:integer;
  parts:TStringArray;
begin
  fRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fNewRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fRepositories.Sorted:=true;
  fRepositoriesChanged:=onRepositoriesChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fExclusions:=TStringlist.Create;
  fExclusions.Add('node_modules');
  fExclusions.Add('lib');
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
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
      else if (parts[0] = 'current_repo') then fCurrentRepo := parts[1]
      end;
  end;
end;

destructor TConfig.destroy;
begin
  if fRepositories <> nil then fRepositories.Free;
  if fNewRepositories <> nil then fNewRepositories.Free;
end;

procedure TConfig.setCodeDirectory(codeDirectory_: string);
begin
if (directoryExists(codeDirectory_)) and (fCodeDirectory <> codeDirectory_) then
  begin
    fCodeDirectory:=codeDirectory_;
    fCodeDirectoryChanged(self);
  end;
//else raise an error?
end;

procedure TConfig.setCurrentRepo(currentRepo_: string);
begin
if (fCurrentRepo <> currentRepo_) then
  begin
  fCurrentRepo:=currentRepo_;
  fCurrentRepoChanged(self);
  end;
end;

//Adds new repo to NewRepositories map
//This way we can check if anything's changed
//before firing event handlers
procedure TConfig.addNewRepo(repoName: string; repo:TRepo);
begin
  fNewRepositories.AddOrSetData(repoName, repo);
end;

function TConfig.getRepoPath(repoName: string): string;
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex > 0) then result:= (fRepositories.Data[repoIndex]).path
  else result:='';
end;

function TConfig.getRepoNames: TStringlist;
var
  index:integer;
begin
  result:=TStringlist.Create;
  for index:= 0 to pred(fRepositories.Count) do
      result.add(fRepositories.Keys[index]);
end;

function TConfig.repoMissingOrUpdated(repoName: string; repo_: TRepo
  ): boolean;
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

procedure TConfig.deleteRepo(repoName: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex > -1) then
     fRepositories.Delete(repoIndex);
end;

procedure TConfig.clearNewRepos;
begin
  fNewRepositories.Clear;
end;

procedure TConfig.addRepo(repoName: string; repo_: TRepo);
begin
  fRepositories.AddOrSetData(repoName, repo_);
end;

procedure TConfig.rescanRepos(codeDirChanged:boolean = false);
begin
  clearNewRepos;
  doRescanRepos(codeDirectory);
  if (updateRepositories > 0) then fRepositoriesChanged(self);
end;

procedure TConfig.doRescanRepos(codeDir: String);
var
directoryList:TStringlist;
index:integer;
directoryName,repoName:string;
repoNameParts:TStringArray;
branchFileNameParts:TStringArray;
newRepo:TRepo;
newBranch:TBranch;
fileModified,branchModified:longint;
branchFiles:TStringlist;
bFIndex:Integer;
begin
  chdir(codeDir);
  repoNameParts:=codeDir.Split('/');
  repoName:= repoNameParts[pred(length(repoNameParts))];
  if directoryExists('.git') then
    begin
      //add to the list of repos and don't go any deeper
      //if index doesn't exist look at config which will be
      if fileExists('.git/index') then fileModified:= FileAge('.git/index')
      else fileModified:= FileAge('.git/config');
      newRepo:=TRepo.create(codeDir,FileDateToDateTime(fileModified));
      if (directoryExists('.git/refs')) and (directoryExists('.git/refs/heads')) then
        //get branches
        begin
        chDir(codeDir+'/.git/refs/heads');
        branchFiles:=findAllFiles(codeDir+'/.git/refs/heads','',false);
        if (branchFiles.Count > 0) then
          begin
          for bfIndex:=0 to pred(branchFiles.Count) do
            begin
            branchFileNameParts:=branchFiles[bfIndex].Split('/');
            branchModified:= FileAge(branchFiles[bfIndex]);
            newBranch.branch_name:=branchFileNameParts[pred(length(branchFileNameParts))];
            newBranch.branch_lastModified:=FileDateToDateTime(branchModified);
            newRepo.addBranch(newBranch);
            end;
          end;
        chDir(codeDir);
        end;
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

function TConfig.updateRepositories:integer;
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

function TConfig.toStringArray: TStringArray;
var
  configLength,index:integer;
begin
  result:= TStringArray.create;
  configLength:=fRepositories.Count + 1;
  setLength(Result, configLength);
  result[0]:='code_directory,'+fCodeDirectory;
  for index:= 0 to pred(fRepositories.Count) do
    begin
      result[index+1]:=fRepositories.Keys[index]+','+(fRepositories.Data[index]).path+','+DateToISO8601((fRepositories.Data[index]).lastUsed);
    end;
end;

end.

