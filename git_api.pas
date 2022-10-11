unit git_api;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,process,repo,gitResponse;
type
  //For executing git commands and returning results in a standard form
  
  { TGitApi }

  TGitApi = class(TInterfacedObject)
    private
    fProcess:TProcess;
    fRepo:TRepo;
    fParams:TStringlist;
    function executeCommand:TStringList;
    procedure clearParams;
    procedure resetParams(default:boolean = true);
    property params: TStringlist read fParams;
    public
    constructor create(repo_:TRepo);
    function getBranches:TGitResponse;
    function changeBranch(branchName:string):TGitResponse;
    function logWithDecoration:TGitResponse;
  end;

implementation

{ TGitApi }

constructor TGitApi.create(repo_: TRepo);
begin
  fRepo:=repo_;
  fProcess:=TProcess.Create(Nil);
  fParams:=TStringlist.Create;
end;

function TGitApi.executeCommand: TStringList;
var
  param:integer;
begin
  result:=TStringlist.Create;
  if (fRepo = nil) then exit;
  chdir(fRepo.path);
  if not directoryExists('.git') then exit;
  fProcess.Parameters.Clear;
  fProcess.Executable := '/bin/sh';
  for param:= 0 to pred(params.Count) do
    begin
    fProcess.Parameters.Add(params[param])
    end;
  fProcess.Options := fProcess.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
  fProcess.Execute;
  result.LoadFromStream(fProcess.Output);
end;

procedure TGitApi.clearParams;
begin
  fParams.Clear;
end;

procedure TGitApi.resetParams(default:boolean);
begin
  clearParams;
  if default then fParams.Add('-c');
end;

function TGitApi.getBranches: TGitResponse;
begin
  resetParams;
  params.Add('git branch --sort=-committerdate');
  result:= TGitResponse.Create(executeCommand);
end;

function TGitApi.changeBranch(branchName: string): TGitResponse;
begin
  resetParams;
  params.Add('git checkout '+branchName);
  result:= TGitResponse.Create(executeCommand);
end;

function TGitApi.logWithDecoration: TGitResponse;
begin
  resetParams;
  params.Add('git log --graph --oneline --decorate');
  result:=TGitResponse.create(executeCommand);
end;


end.

