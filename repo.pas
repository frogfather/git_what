unit repo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,arrayUtils,branch,pvProject;
type
  
  { TRepo }

  //represents a git repository
  TRepo = class(TinterfacedObject)
    private
    fpath:string;
    flastUsed:TDateTime;
    fPivotalProjectId: integer;
    fHasPivotalProject: boolean;
    fBranches: TBranches;
    fExclusions:TStringList;
    fMainBranch:TBranch;
    fStatus:TStringList;
    fCurrentBranch: TBranch;
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    procedure addBranch(branch_:TBranch);
    procedure removeBranch(branchName_:string);
    procedure removeBranch(branch:TBranch);
    function findOrCreateBranch(branchName_: string; addToBranchlistIfNotFound:Boolean = false):TBranch;
    public
    constructor create(path_:string; lastUsed_:TDateTime; currentBranch_:TBranch = nil; pivotalProjectId_:integer = -1; mainBranch_:TBranch = nil;exclusions:TStringList = nil);
    procedure setCurrentBranch(branchName:string);
    procedure setMainBranch(branchName: string);
    procedure updateBranches(branchList:TStringList);
    procedure setRepoStatus(status:TStringList);
    procedure addExclusion(exclusion:String);
    procedure removeExclusion(exclusion:String);
    property path: string read fPath write setPath;
    property lastUsed: TDateTime read fLastUsed write setLastUsed;
    property mainBranch: TBranch read fMainBranch;
    property pivotalProjectId: integer read fPivotalProjectId;
    property hasPivotalProject:boolean read fHasPivotalProject;
    property currentBranch: TBranch read fCurrentBranch;
    property status:TStringList read fStatus;
    property exclusions:TStringList read fExclusions;
  end;

implementation

{ TRepo }

procedure TRepo.setLastUsed(lastUsed_: TDateTime);
begin
  fLastUsed:=lastUsed_;
end;

procedure TRepo.setPath(path_: string);
begin
  fPath:=path_;
end;

procedure TRepo.setCurrentBranch(branchName: string);
begin
  fCurrentBranch:=findOrCreateBranch(branchName,true);
end;

procedure TRepo.setMainBranch(branchName: string);
begin
  fMainBranch:=findOrCreateBranch(branchName);
end;

//Treat branches as ephemeral except where they're
//either the main branch or excluded branches
procedure TRepo.updateBranches(branchList: TStringList);
var
  index:integer;
  toRemove:TBranches;
begin
  if branchList.Count = 0 then fBranches.clear;
  for index:=0 to pred(branchList.Count) do
    addBranch(TBranch.create(branchList[index]));
  toRemove:=TBranches.create;
  for index:=0 to pred(fBranches.size) do
    begin
    if (branchlist.IndexOf(fBranches[index].name) = -1)
    then toRemove.push(fBranches[index]);
    end;
  if (toRemove.size > 0) then
    begin
      for index:= 0 to pred(toRemove.size) do
      removeBranch(toRemove[index]);
    end;
end;

procedure TRepo.addBranch(branch_: TBranch);
begin
  if (fBranches.findByName(branch_.name)) = Nil
    then fBranches.push(branch_);
end;

procedure TRepo.removeBranch(branchName_: string);
begin

end;

procedure TRepo.removeBranch(branch: TBranch);
begin
  fBranches.delete(branch);
end;

function TRepo.findOrCreateBranch(branchName_: string; addToBranchlistIfNotFound:Boolean = false): TBranch;
begin
  result:=fBranches.findByName(branchName_);
  if (result = nil) then result:= TBranch.create(branchName_);
  if addToBranchListIfNotFound then addBranch(result);
end;

procedure TRepo.setRepoStatus(status:TStringList);
begin
  fStatus:=status;
end;

procedure TRepo.addExclusion(exclusion: String);
begin
  //Add the supplied item if it isn't already there.
  if (fExclusions.IndexOf(exclusion) = -1) then fExclusions.Add(exclusion);
end;

procedure TRepo.removeExclusion(exclusion: String);
begin
  if (fExclusions.IndexOf(exclusion) > -1) then fExclusions.Delete(fExclusions.IndexOf(exclusion));
end;

constructor TRepo.create(path_:string; lastUsed_:TDateTime; currentBranch_:TBranch;
            pivotalProjectId_:integer; mainBranch_: TBranch;exclusions:TStringList);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
  fPivotalProjectId:=pivotalProjectId_;
  fCurrentBranch:=currentBranch_;
  fStatus:=TStringlist.create;
  if (currentBranch_ <> nil) then
  fBranches.push(currentBranch_);
  if (mainBranch_ <> nil) then
  fMainBranch:=mainBranch_;
  fExclusions:=TStringList.Create;
end;

end.

