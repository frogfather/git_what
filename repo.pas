unit repo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,arrayUtils,branch;
type
  
  { TRepo }

  TRepo = class(TinterfacedObject)
    private
    fpath:string;
    flastUsed:TDateTime;
    fBranches:Tbranches;
    fCurrentBranch:string;
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    public
    procedure addBranch(branch_:TBranch);
    procedure deleteBranch(branchName_:string);
    procedure setCurrentBranch(newBranch_:string);
    constructor create(path_:string;lastUsed_:TDateTime;branches_:TBranches);
    constructor create(path_:string;lastUsed_:TDateTime);
    property path: string read fPath write setPath;
    property lastUsed: TDateTime read fLastUsed write setLastUsed;
    property currentBranch:String read fCurrentBranch write setCurrentBranch;
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

procedure TRepo.addBranch(branch_: TBranch);
begin
  if (fBranches.indexOf(branch_.branch_name) = -1) then
  fBranches.push(branch_);
end;

procedure TRepo.deleteBranch(branchName_: string);
begin
  if (fBranches.indexOf(branchName_) = -1) then
    fBranches.delete(branchName_);
end;

procedure TRepo.setCurrentBranch(newBranch_: string);
begin
  if (fBranches.indexOf(newBranch_) > -1)
    then currentBranch:=newBranch_;
end;

constructor TRepo.create(path_: string; lastUsed_: TDateTime;branches_:TBranches);
begin
  fPath:=path;
  fLastUsed:=lastUsed;
  fBranches:=branches_;
end;

constructor TRepo.create(path_: string; lastUsed_: TDateTime);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
  fBranches:= TBranches.create;
end;

end.

