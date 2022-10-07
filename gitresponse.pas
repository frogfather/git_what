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
      fResult:TStringlist;
      function getErrors:Tstringlist;
      function getResult:TStringlist;
    public
      function toString:string;
      property errors:TStringlist read getErrors;
      property result:TStringlist read getResult;
  end;

implementation

{ TGitResponse }

function TGitResponse.getErrors: Tstringlist;
begin
  result:=fErrors;
end;

function TGitResponse.getResult: TStringlist;
begin
  result:=fResult;
end;

function TGitResponse.toString: string;
begin
  result:=fResult.CommaText;
end;

end.

