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
      fRepositoriesChanged:TNotifyEvent;
      fCodeDirectoryChanged:TNotifyEvent;
      fRescanning:boolean;
      procedure setCodeDirectory(codeDirectory_:string);
      function getRepoPath(repoName:string):string;
      procedure deleteRepo(repoName:string);
      property rescanning: boolean read fRescanning write fRescanning;
    public
    constructor create(onRepositoriesChanged,onCodeDirectoryChanged:TNotifyEvent);
    constructor create(onRepositoriesChanged,onCodeDirectoryChanged:TNotifyEvent;lines: TStringArray);
    function toStringArray: TStringArray;
    procedure addRepo(repoName,repoPath:string);
    procedure clearRepos;
    procedure rescanRepos;
    property codeDirectory:string read fCodeDirectory write setCodeDirectory;
  end;

implementation

{ TConfig }

constructor TConfig.create(onRepositoriesChanged,onCodeDirectoryChanged:TNotifyEvent);
begin
  //create an empty object
  fCodeDirectory:='';
  fRepositories:= specialize TFPGMap<string,string>.Create;
  fRepositories.Sorted:=true;
  fRepositoriesChanged:=onRepositoriesChanged;
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
end;

constructor TConfig.create(onRepositoriesChanged,onCodeDirectoryChanged:TNotifyEvent; lines: TStringArray);
var
  index:integer;
  parts:TStringArray;
begin
  fRepositories:= specialize TFPGMap<string,string>.Create;
  fRepositories.Sorted:=true;
  fRepositoriesChanged:=onRepositoriesChanged;
  fCodeDirectoryChanged:=onCodeDirectoryChanged;
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

procedure TConfig.setCodeDirectory(codeDirectory_: string);
begin
if (directoryExists(codeDirectory_)) then fCodeDirectory:=codeDirectory_;
//else raise an error?
end;

procedure TConfig.addRepo(repoName, repoPath: string);
var
  repoIndex:integer;
begin
  fRepositories.Find(repoName,repoIndex);
  if (repoIndex = -1) then
    begin
      fRepositories.Add(repoName, repoPath);
      fRepositoriesChanged(self);
    end;
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
  if (repoIndex > -1) then
    begin
      fRepositories.Delete(repoIndex);
      fRepositoriesChanged(self);
    end;
end;

procedure TConfig.clearRepos;
begin
  fRepositories.Clear;
  fRepositoriesChanged(self);
end;

procedure TConfig.rescanRepos;
begin

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

