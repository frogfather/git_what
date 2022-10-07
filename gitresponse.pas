unit gitResponse;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  IGitResponse = interface
  ['{b5a0657a-6cf2-4f5b-b95e-1467f901ca26}']
  function getErrors:TStringlist;
  function getResult: String;
  property errors:TStringlist read getErrors;
  property result:string read getResult;
  end;

implementation

end.

