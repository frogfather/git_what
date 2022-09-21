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
    fCurrentBranch:string;
    procedure setLastUsed(lastUsed_:TDateTime);
    procedure setPath(path_:string);
    public
    procedure setCurrentBranch(newBranch_:string);
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



procedure TRepo.setCurrentBranch(newBranch_: string);
begin
  currentBranch:=newBranch_;
end;

constructor TRepo.create(path_: string; lastUsed_: TDateTime);
begin
  fPath:=path_;
  fLastUsed:=lastUsed_;
end;

end.

