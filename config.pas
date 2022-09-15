unit config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,fgl;
type
  
  { TConfig }

  TConfig = class(TInterfacedObject)
    private
      fCodeDirectory:String;
      fRepositories: specialize TFPGMap<string,string>;
      procedure setCodeDirectory(codeDirectory:string);
      function getRepoPath(repoName:string):string;
      procedure deleteRepo(repoName:string);
    public
    constructor create;
    constructor create(lines: TStringArray);
    function toStringArray: TStringArray;
    procedure addRepo(repoName,repoPath:string);
    procedure clearRepos;
    property codeDirectory:string read fCodeDirectory write setCodeDirectory;
  end;

implementation

{ TConfig }

procedure TConfig.setCodeDirectory(codeDirectory: string);
begin

end;

procedure TConfig.addRepo(repoName, repoPath: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex = -1) then fRepositories.Add(repoName, repoPath);
end;

function TConfig.getRepoPath(repoName: string): string;
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex > -1) then result:= fRepositories.Data[repoIndex]
  else result:='';
end;

procedure TConfig.deleteRepo(repoName: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex > -1) then fRepositories.Delete(repoIndex);
end;

procedure TConfig.clearRepos;
begin
  fRepositories.Clear;
end;

constructor TConfig.create;
begin
  //create an empty object
  fCodeDirectory:='';
  fRepositories:= specialize TFPGMap<string,string>.Create;
end;

constructor TConfig.create(lines: TStringArray);
var
  index:integer;
  parts:TStringArray;
begin
  fRepositories:= specialize TFPGMap<string,string>.Create;
  for index:= 0 to pred(length(lines)) do
  begin
    //split each line on =
    parts:=lines[index].Split('=');
    if (length(parts)=2) then
      begin
        if (parts[0] = 'code_directory') then fCodeDirectory := parts[1]
        else addRepo(parts[0],parts[1]);
      end;
  end;
end;

function TConfig.toStringArray: TStringArray;
var
  configLength,index:integer;
begin
  result:= TStringArray.create;
  configLength:=fRepositories.Count + 1;
  setLength(Result, configLength);
  result[0]:='code_directory='+fCodeDirectory;
  for index:= 0 to pred(fRepositories.Count) do
    begin
      result[index+1]:=fRepositories.Keys[index]+'='+fRepositories.Data[index];
    end;
end;

end.

