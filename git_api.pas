unit git_api;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,process,repo;
type
  //For executing git commands and returning results in a standard form
  
  { TGitApi }

  TGitApi = class(TInterfacedObject)
    private
    fProcess:TProcess;
    fRepo:TRepo;
    function execute(parameters:TStringList):TStringList;
    public
    constructor create(repo_:TRepo);
    function getBranches:TStringList;
    function changeBranch(branchName:string):TStringList;
  end;

implementation

{ TGitApi }

constructor TGitApi.create(repo_: TRepo);
begin
  fRepo:=repo_;
  fProcess:=TProcess.Create(Nil);
end;

function TGitApi.execute(parameters: TStringList): TStringList;
var
  param:integer;
begin
  result:=TStringlist.Create;
  chdir(fRepo.path);
  if not directoryExists('.git') then exit;
  fProcess.Parameters.Clear;
  fProcess.Executable := '/bin/sh';
  for param:= 0 to pred(parameters.Count) do
    begin
    fProcess.Parameters.Add(parameters[param])
    end;
  //fProcess.Parameters.Add('-c');
  //fProcess.Parameters.Add(command);
  fProcess.Options := fProcess.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
  fProcess.Execute;
  result.LoadFromStream(fProcess.Output);
end;

function TGitApi.getBranches: TStringList;
var
  params:TStringlist;
begin
  params:=TStringlist.Create;
  params.Add('-c');
  params.Add('git branch --sort=-committerdate');
  result:= execute(params);
end;

function TGitApi.changeBranch(branchName: string): TStringList;
var
  params:TStringlist;
begin
  params:=TStringlist.Create;
  params.Add('-c');
  params.Add('git checkout '+branchName);
  result:= execute(params);
end;


end.

