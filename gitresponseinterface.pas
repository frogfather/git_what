unit gitResponseInterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  IGitResponse = interface
  ['{b5a0657a-6cf2-4f5b-b95e-1467f901ca26}']
  function getErrors:TStringlist;
  function getResults: TStringlist;
  property errors:TStringlist read getErrors;
  property results:TStringlist read getResults;
  end;

implementation

end.

