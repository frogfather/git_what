unit gitResponse;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,gitResponseInterface;
type
  
  { TGitResponse }

  TGitResponse = class(TInterfacedObject, IGitResponse)
    private
      fErrors:TStringlist;
      fResults:TStringlist;
      fSuccess:boolean;
      function getErrors:Tstringlist;
      function getResults:TStringlist;
      function getSuccess:boolean;
    public
      constructor create(commandResult:TStringlist);
      property errors:TStringlist read getErrors;
      property results:TStringlist read getResults;
      property success:boolean read getSuccess;
  end;

implementation

{ TGitResponse }

function TGitResponse.getErrors: Tstringlist;
begin
  result:=fErrors;
end;

function TGitResponse.getResults: TStringlist;
begin
  result:=fResults;
end;

function TGitResponse.getSuccess: boolean;
begin
  result:=fSuccess;
end;

constructor TGitResponse.create(commandResult: TStringlist);
begin
  if (commandResult.Count > 0) then
    begin
      //TODO - only look for error at start of response
      if (commandResult[0].IndexOf('error') > -1) then
        begin
          fErrors:=commandResult;
          fResults:=TStringlist.Create;
          fSuccess:=false;
        end else
        begin
          fErrors:=TStringlist.Create;
          fResults:=commandResult;
          fSuccess:=true;
        end;
    end else
    begin
      fResults:=TStringlist.Create;
      fErrors:=TStringlist.Create;
      fErrors.Add('No response');
      fSuccess:=false;
    end;
end;

end.

