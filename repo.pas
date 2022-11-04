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
    fPivotalProject: integer;
    fCurrentBranch: string;
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    procedure setPivotalProject(projectId:integer);
    public
    constructor create(path_:string; lastUsed_:TDateTime; pivotalProject_:integer = -1; currentBranch_:string='');
    property path: string read fPath write setPath;
    property lastUsed: TDateTime read fLastUsed write setLastUsed;
    property pivotalProject: integer read fPivotalProject write setPivotalProject;
    property currentBranch: string read fCurrentBranch write fCurrentBranch;
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

constructor TRepo.create(path_:string; lastUsed_:TDateTime; pivotalProject_:integer; currentBranch_:string);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
  fPivotalProject:=pivotalProject_;
  fCurrentBranch:=currentBranch_;
end;

end.

