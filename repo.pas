unit repo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,arrayUtils;
type
  
  { TRepo }

  TRepo = class(TinterfacedObject)
    private
    fpath:string;
    flastUsed:TDateTime;
    fBranches:TStringlist;
    fCurrentBranch:string;
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    procedure setBranches(branches_:TStringlist);
    public
    procedure setCurrentBranch(newBranch_:string);
    constructor create(path_:string;lastUsed_:TDateTime);
    property path: string read fPath write setPath;
    property lastUsed: TDateTime read fLastUsed write setLastUsed;
    property branches: TStringlist read fBranches write Setbranches;
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

procedure TRepo.setBranches(branches_: TStringlist);
begin
  fBranches:=branches_;
  if (fBranches.Count > 0) then currentBranch:=fBranches[0];
end;

procedure TRepo.setCurrentBranch(newBranch_: string);
begin
  if (currentBranch <> newBranch_) then
  fcurrentBranch:=newBranch_;
end;

constructor TRepo.create(path_: string; lastUsed_: TDateTime);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
end;

end.

