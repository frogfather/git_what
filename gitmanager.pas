unit gitManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,fileUtil,repo,fgl,dateUtils,
  git_api, gitResponse,xml_doc_handler,laz2_DOM,branch,pvProject,pivotalApi;
type
  
  { TGitWhat }

  TGitWhat = class(TInterfacedObject)
    private
    fXMLDocumentHandler:TXMLDocumentHandler;
    fCurrentDirectory:String;
    fCurrentRepoName:string;
    fNewRepositories: specialize TFPGMap<string,TRepo>;
    fRepositories: specialize TFPGMap<string,TRepo>;
    fExclusions:TStringlist;
    fCurrentDirectoryChanged:TNotifyEvent;
    fRepositoriesChanged:TNotifyEvent;
    fCurrentRepoChanged:TNotifyEvent;
    fCurrentBranchChanged:TNotifyEvent;
    function updateRepositories:integer;
    function repoMissingOrUpdated(repoName: string; repo_: TRepo): boolean;
    function getRepoName(path:string):string;
    procedure clearNewRepos;
    procedure addNewRepo(repoName: string; repo:TRepo);
    procedure addRepo(repoName: string; repo_: TRepo);
    procedure deleteRepo(repoName: string);
    procedure setCodeDirectory(codeDirectory_:string);
    procedure setCurrentRepoName(repoName_:string);
    procedure setCurrentBranchName(branchName_: string);
    procedure rescanRepos;
    procedure doRescanRepos(codeDir: String);
    procedure toXML;
    procedure fromXML;
    function getCurrentRepo:TRepo;
    function getBranches:TStringlist;
    function onCurrentBranch(branch:string):Boolean;
    function getCurrentBranchName:string;
    property exclusions: TStringlist read fExclusions;
    property xmlDocumentHandler:TXMLDocumentHandler read fXMLDocumentHandler;
    public
    constructor create(
      onCodeDirectoryChanged,
      onReposChanged,
      onCurrentRepoChanged,
      onCurrentBranchChanged:TNotifyEvent);
    function getRepoNames:TStringlist;
    function saveToFile(fileName:string):boolean;
    function loadFromFile(fileName:string):boolean;
    property codeDirectory: string read fCurrentDirectory write setCodeDirectory;
    property currentRepoName: string read fCurrentRepoName write setCurrentRepoName;
    property currentrepo: TRepo read getCurrentrepo;
    property currentBranchName: string read GetcurrentBranchName write SetcurrentBranchName;
    property branches: TStringList read Getbranches;
  end;

implementation

{ TGitWhat }

constructor TGitWhat.create(
  onCodeDirectoryChanged,
  onReposChanged,
  onCurrentRepoChanged,
  onCurrentBranchChanged:TNotifyEvent);
begin
  fCurrentDirectory:='';
  fRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fNewRepositories:= specialize TFPGMap<string,TRepo>.Create;
  fRepositories.Sorted:=true;
  fExclusions:=TStringlist.Create;
  fExclusions.Add('node_modules');
  fExclusions.Add('lib');
  fCurrentRepoName:='';
  fCurrentDirectoryChanged:=onCodeDirectoryChanged;
  fRepositoriesChanged:=onReposChanged;
  fCurrentRepoChanged:=onCurrentRepoChanged;
  fCurrentBranchChanged:=onCurrentBranchChanged;
  fXMLDocumentHandler:=TXMLDocumentHandler.create;
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
  newRepo:TRepo;
  fileModified:longint;
  begin
    chdir(codeDir);
    repoName:= getRepoName(codeDir);
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

procedure TGitWhat.toXML;
var
  index:integer;
  reposNode,repoNode,branchNode:TDOMNode;
  attributes:TStringArray;
  currentBranchForRepo:TBranch;
  currentBranchNameForRepo:string;
begin
  //create an xml document based on the gitManager class
  setLength(attributes,4);
  attributes[0]:='name';
  attributes[2]:='ref';
  with xmlDocumentHandler do
    begin
    initializeDoc;
    addNode('','code-directory',codeDirectory);
    addNode('','current-repo',currentRepoName);
    //Add pivotal projects with id="x"
    //Then in the repo use ref="x" to the project

    //For branches add pivotal stories with id="x"
    //then in branch use ref="x" to the story

    reposNode:=addNode('','repos');
    for index:= 0 to pred(fRepositories.Count) do
      begin
      currentBranchForRepo:=fRepositories.Data[index].currentBranch;
      if (currentBranchForRepo = nil)
        then currentBranchNameForRepo:=''
        else currentBranchNameForRepo:= currentBranchForRepo.name;
      attributes[1]:=fRepositories.Keys[index];
      attributes[3]:=fRepositories.Data[index].pivotalProjectId.ToString;
      repoNode:=createNode('repo','',attributes);
      repoNode.AppendChild(createNode('path',fRepositories.Data[index].path));
      branchNode:=createNode('branch','');
      branchNode.AppendChild(createNode('branch-name', currentBranchNameForRepo));
      repoNode.AppendChild(branchNode);
      repoNode.AppendChild(createNode('last-used',DateToISO8601(fRepositories.Data[index].lastUsed)));
      reposNode.AppendChild(repoNode);
      end;
    end;
end;

procedure TGitWhat.fromXML;
var
  reposNode,childNode,repoCurrentBranchNode:TDOMNode;
  repoEnumerator:TDomNodeEnumerator;
  repoPath:string;
  repoPivotal:TPivotal;
  repoLastUsed:TDateTime;
  repoCurrentBranch:TBranch;
begin
  codeDirectory:= xmlDocumentHandler.getNodeTextValue('code-directory');
  currentRepoName:= xmlDocumentHandler.getNodeTextValue('current-repo');
  //TODO there are probably build in methods on TXMLDocument that do this better
  reposNode:= xmlDocumentHandler.getNode('repos');
  if (reposNode <> Nil) and (reposNode.GetChildCount > 0) then
    begin
    repoEnumerator:= reposNode.GetEnumerator;
    while repoEnumerator.MoveNext do
      begin
      childNode:=repoEnumerator.Current;
      //create a repo from this
      repoPath:=childNode.ChildNodes.Item[0].TextContent;
      //repoPivotal:=childNode.ChildNodes.Item[1].TextContent.ToInteger;
      repoCurrentBranchNode:=childNode.ChildNodes.Item[1];
      repoLastUsed:= ISO8601ToDate(childNode.ChildNodes.Item[2].TextContent);
      if (repoCurrentBranchNode.GetChildCount = 3) then
        begin
        repoCurrentBranch:=TBranch.Create(
          repoCurrentBranchNode.ChildNodes[0].TextContent,
          repoCurrentBranchNode.ChildNodes[1].TextContent,
          repoCurrentBranchNode.ChildNodes[2].TextContent.ToInteger);
        end;
      addRepo(getRepoName(repoPath),TRepo.create(repoPath,repoLastUsed,repoCurrentBranch)) //pivotal project  as last param
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

function TGitWhat.saveToFile(fileName: string): boolean;
begin
  //use the document handler to create the document
  //then call save on it
  toXML;
  xmlDocumentHandler.save(fileName);
  result:=true;
end;

function TGitWhat.loadFromFile(fileName: string): boolean;
begin
  xmlDocumentHandler.load(fileName);
  fromXML;
  result:=true;
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

function TGitWhat.getRepoName(path: string): string;
var
  repoNameParts:TStringArray;
begin
  repoNameParts:=path.Split('/');
  result:= repoNameParts[pred(length(repoNameParts))];
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
  if (directoryExists(codeDirectory_)) and (fCurrentDirectory <> codeDirectory_) then
  begin
    chDir(codeDirectory_);
    fCurrentDirectory:=codeDirectory_;
    rescanRepos;
    fCurrentDirectoryChanged(self);
  end;
end;

procedure TGitWhat.setCurrentRepoName(repoName_: string);
var
  repoIndex:integer;
  branchResponse:TGitResponse;
  gitApi: TGitApi;
  index:integer;
begin
  fRepositories.Find(repoName_, repoIndex);
  if (repoIndex > 0) and (fCurrentRepoName <> repoName_) then
    begin
    chDir(fRepositories.Data[repoIndex].path);
    fCurrentRepoName:=repoName_;
    gitApi:=TGitApi.create(currentRepo);
    branchResponse:= gitApi.getBranches;
    if branchResponse.success
      then currentRepo.updateBranches(branchResponse.results);
    fCurrentRepoChanged(self);
    end;
end;

procedure TGitWhat.setCurrentBranchName(branchName_: string);
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
      currentRepo.setCurrentBranch(branchName_);
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
     then
       begin
       //Update currentRepo with branch information
       currentrepo.updateBranches(branchResponse.results);
       result:= branchResponse.results;
       end
     else result:= TStringlist.Create;
end;

function TGitWhat.onCurrentBranch(branch: string): Boolean;
begin
  result:= branch.Substring(0,1) = '*';
end;

function TGitWhat.getCurrentBranchName: string;
var
  currRepo:TRepo;
begin
  currRepo:=currentRepo;
  if (currRepo = Nil) or (currRepo.currentBranch = Nil) then exit;
  result:=currRepo.currentBranch.name;
end;

end.

