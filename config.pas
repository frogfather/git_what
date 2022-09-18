unit config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,fgl,fileUtil,repo,dateUtils;
type
  
  { TConfig }
  //TODO repositories should be an array of repository objects (TBA)
  TConfig = class(TInterfacedObject)
    private
      fCodeDirectory:String;
      fCurrentRepo:string;
      fRepositories: specialize TFPGMap<string,TRepo>;
      fRepositoriesChanged:TNotifyEvent;
      fCodeDirectoryChanged:TNotifyEvent;
      fCurrentRepoChanged:TNotifyEvent;
      fNoNotify:boolean;
      fExclusions:TStringlist;
      procedure setCodeDirectory(codeDirectory_:string);
      procedure setCurrentRepo(currentRepo_:string);
      function getRepoPath(repoName:string):string;
      function getRepoNames:TStringlist;
      procedure deleteRepo(repoName:string);
      procedure clearRepos;
      procedure doRescanRepos(codeDir:String);
      property exclusions: TStringlist read fExclusions;
      property nonotify: boolean read fNoNotify write fNoNotify;
    public
    constructor create(onRepositoriesChanged,onCodeDirectoryChanged,onCurrentRepoChanged:TNotifyEvent);
    constructor create(onRepositoriesChanged,onCodeDirectoryChanged,onCurrentRepoChanged:TNotifyEvent;lines: TStringArray);
    function toStringArray: TStringArray;
    procedure addRepo(repoName:string;repo: TRepo);
    procedure rescanRepos;
    property codeDirectory:string read fCodeDirectory write setCodeDirectory;
    property repoNames: TStringlist read getRepoNames;
    property currentRepo: string read fCurrentRepo write setCurrentRepo;
  end;

implementation

{ TConfig }

constructor TConfig.create(onRepositoriesChanged,onCodeDirectoryChanged,onCurrentRepoChanged:TNotifyEvent);
begin
  //create an empty object
  fNoNotify:=true;
  fCodeDirectory:='';
  fRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fRepositories.Sorted:=true;
  fRepositoriesChanged:=onRepositoriesChanged;
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fExclusions:=TStringlist.Create;
  fExclusions.Add('node_modules');
  fExclusions.Add('lib');
  fNoNotify:=false;
end;

constructor TConfig.create(onRepositoriesChanged,onCodeDirectoryChanged,onCurrentRepoChanged:TNotifyEvent; lines: TStringArray);
var
  index:integer;
  parts:TStringArray;
  newRepo:TRepo;
begin
  fNoNotify:=true;
  fRepositories:= specialize TFPGMap<string,TRepo>.Create;
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
      begin
      newRepo.path:=parts[1];
      newRepo.lastUsed:=Iso8601ToDate(parts[2]);
      addRepo(parts[0],newRepo);
      end else
      if (length(parts) = 2) then
      begin
      if (parts[0] = 'code_directory') then fCodeDirectory := parts[1]
      else if (parts[0] = 'current_repo') then fCurrentRepo := parts[1]
      end;
  end;
  fNoNotify:=false;
end;

procedure TConfig.setCodeDirectory(codeDirectory_: string);
begin
if (directoryExists(codeDirectory_)) then
  begin
    fCodeDirectory:=codeDirectory_;
    fCodeDirectoryChanged(self);
  end;
//else raise an error?
end;

procedure TConfig.setCurrentRepo(currentRepo_: string);
begin
  fCurrentRepo:=currentRepo_;
  fCurrentRepoChanged(self);
end;

procedure TConfig.addRepo(repoName: string; repo:TRepo);
var
  repoIndex:integer;
begin
  repoIndex:=   fRepositories.IndexOf(repoName);
  if (repoIndex = -1) then
    begin
      fRepositories.Add(repoName, repo);
      //we don't want to fire the event handler while rescanning
      if not nonotify then fRepositoriesChanged(self);
    end;
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

procedure TConfig.deleteRepo(repoName: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex > 0) then
    begin
      fRepositories.Delete(repoIndex);
      //we don't want to fire the event handler while rescanning
      if not nonotify then fRepositoriesChanged(self);
    end;
end;

procedure TConfig.rescanRepos;
begin
  nonotify:= true;
  clearRepos;
  doRescanRepos(codeDirectory);
  nonotify:=false;
  fRepositoriesChanged(self);
end;

procedure TConfig.doRescanRepos(codeDir: String);
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
  repoName:= repoNameParts[length(repoNameParts)-1];
  if directoryExists('.git') then
    begin
      //add to the list of repos and don't go any deeper
      //if index doesn't exist look at config which will be
      if fileExists('.git/index') then fileModified:= FileAge('.git/index')
      else fileModified:= FileAge('.git/config');
      newRepo.path:=codeDir;
      newRepo.lastUsed:=FileDateToDateTime(fileModified);
      addRepo(repoName,newRepo);
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

procedure TConfig.clearRepos;
begin
  fRepositories.Clear;
  if not nonotify then fRepositoriesChanged(self);
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

