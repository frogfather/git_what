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
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    public
    constructor create(path_:string;lastUsed_:TDateTime);
    property path: string read fPath write setPath;
    property lastUsed: TDateTime read fLastUsed write setLastUsed;
    property pivotalProject: integer read fPivotalProject write fPivotalProject;
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

constructor TRepo.create(path_: string; lastUsed_: TDateTime);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
end;

end.

