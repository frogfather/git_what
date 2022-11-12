unit pivotalApi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, httpClient,fpjson, jsonparser;
type
  { TPivotalApi }

  TPivotalApi = class(TInterfacedObject)
    private
    function getClient: THttpClient;
    public
    function getProjects: TJSONObject;
    function getProject(projectId:string):TJSONObject;
  end;

implementation

{ TPivotalApi }
const baseUrl: string = 'https://www.pivotaltracker.com/services/v5';

function TPivotalApi.getClient: THttpClient;
begin
  result:=THttpClient.Create;
  result.baseUrl:= baseUrl;
  result.addHeader('X-TrackerToken', 'xxxx'); //add to config file
  result.addHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
  result.addHeader('Content-Type', 'application/json');
end;

function TPivotalApi.getProjects: TJSONObject;
begin
  result:=getClient.get('/projects');
end;

function TPivotalApi.getProject(projectId: string): TJSONObject;
begin
  result:=getClient.get('/projects/'+projectId);
end;

end.

