unit repo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,arrayUtils,branch;
type
  
  { TRepo }

  //represents a git repository
  TRepo = class(TinterfacedObject)
    private
    fpath:string;
    flastUsed:TDateTime;
    fPivotalProject: integer;
    fBranches: TBranches;
    fCurrentBranch: TBranch;
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    procedure setPivotalProject(projectId:integer);
    procedure addBranch(branch_:TBranch);
    procedure removeBranch(branchName_:string);
    procedure removeBranch(branch:TBranch);
    public
    constructor create(path_:string; lastUsed_:TDateTime; currentBranch_:TBranch = nil; pivotalProject_:integer = -1);
    procedure setCurrentBranch(branchName:string);
    procedure updateBranches(branchList:TStringList);
    property path: string read fPath write setPath;
    property lastUsed: TDateTime read fLastUsed write setLastUsed;
    property pivotalProject: integer read fPivotalProject write setPivotalProject;
    property currentBranch: TBranch read fCurrentBranch;
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

procedure TRepo.setPivotalProject(projectId: integer);
begin
  fPivotalProject := projectId;
end;

procedure TRepo.setCurrentBranch(branchName: string);
var
  foundBranch:TBranch;
begin
  foundBranch:= fBranches.findByName(branchName);
  if foundBranch <> nil then
    fCurrentBranch:= foundBranch
  else
    fCurrentBranch:=TBranch.create(branchName);
    addBranch(fCurrentBranch);
end;

procedure TRepo.updateBranches(branchList: TStringList);
var
  index:integer;
  toRemove:TBranches;
begin
  if branchList.Count = 0 then fBranches.clear;

  for index:=0 to pred(branchList.Count) do
    addBranch(TBranch.create(branchList[index]));

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

constructor TRepo.create(path_:string; lastUsed_:TDateTime; currentBranch_:TBranch; pivotalProject_:integer);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
  fPivotalProject:=pivotalProject_;
  fCurrentBranch:=currentBranch_;
  if (currentBranch_ <> nil) then
  fBranches.push(currentBranch_);
end;

end.

