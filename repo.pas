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
    procedure setCurrentBranch(branchName:string);
    procedure addBranch(branch_:TBranch);
    procedure removeBranch(branchName_:string);
    public
    constructor create(path_:string; lastUsed_:TDateTime; currentBranch_:TBranch = nil; pivotalProject_:integer = -1);
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
    fBranches.push(fCurrentBranch);
end;

procedure TRepo.addBranch(branch_: TBranch);
begin

end;

procedure TRepo.removeBranch(branchName_: string);
begin

end;

constructor TRepo.create(path_:string; lastUsed_:TDateTime; currentBranch_:TBranch; pivotalProject_:integer);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
  fPivotalProject:=pivotalProject_;
  fCurrentBranch:=currentBranch_;
end;

end.

