unit pivotalApi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpClient,fpjson, jsonparser,fileUtilities;
type
  { TPivotalApi }

  TPivotalApi = class(TInterfacedObject)
    private
    fToken:string;
    function getClient: THttpClient;
    public
    constructor create(configFile: string);
    function getProjects: TJSONObject;
    function getProject(projectId:string):TJSONObject;
    function getStories(projectId:string):TJSONObject;
    function getStory(projectId,storyId:string):TJSONObject;
  end;

implementation

{ TPivotalApi }
const baseUrl: string = 'https://www.pivotaltracker.com/services/v5';

function TPivotalApi.getClient: THttpClient;
begin
  result:=THttpClient.Create;
  result.baseUrl:= baseUrl;
  result.addHeader('X-TrackerToken', fToken);
  result.addHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
  result.addHeader('Content-Type', 'application/json');
end;

constructor TPivotalApi.create(configFile: string);
var
  fileLines:TStringArray;
  index:integer;
begin
  if fileexists(configFile) then
  fileLines:=openFileAsArray(configFile, #$0A);
  if (length(fileLines) > 0) then for index:= 0 to pred(length(filelines)) do
    begin
      if (fileLines[index].Split(',')[0] = 'X-TrackerToken')
        then fToken:=fileLines[index].Split(',')[1];
    end;
end;

function TPivotalApi.getProjects: TJSONObject;
begin
  result:=getClient.get('/projects');
end;

function TPivotalApi.getProject(projectId: string): TJSONObject;
begin
  result:=getClient.get('/projects/'+projectId);
end;

function TPivotalApi.getStories(projectId: string): TJSONObject;
begin
  result:=getClient.get('/projects/'+projectId+'/stories');
end;

function TPivotalApi.getStory(projectId, storyId: string): TJSONObject;
begin
  result:=getClient.get('/projects/'+projectId+'/stories/'+storyId);
end;

end.

