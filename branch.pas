unit branch;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils;
type

  { TBranch }

  TBranch = class(TInterfacedObject)
    private
    fName: string;
    fStory: integer;
    fStoryName: string;
    public
    constructor create(name_:string; storyName_:string = ''; story_: integer = -1);
    property name: string read fName write fName;
  end;

  { TBranches }
  TBranches = array of TBranch;

  { TBranchHelper }

  TBranchHelper = type Helper for TBranches
  function size: integer;
  function push(element:TBranch):integer;
  function indexOf(element:TBranch):integer;
  function findByName(elementName:string):TBranch;
  end;

implementation

generic function GetIndex<T>(aItem:T; aArr: specialize TArray<T>): SizeInt;
begin
  for Result := 0 to High(aArr) do
    if aArr[Result] = aItem then
      Exit;
  Result := -1;
end;

{ TBranchHelper }

function TBranchHelper.size: integer;
begin
  result:=length(self);
end;

function TBranchHelper.push(element: TBranch): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TBranchHelper.indexOf(element: TBranch): integer;
begin
  result:= specialize getIndex<TBranch>(element,self);
end;

function TBranchHelper.findByName(elementName: string): TBranch;
var
  index: integer;
begin
  result:=nil;
  if self.size = 0 then exit;
  for index:= 0 to pred(self.size) do
    begin
      if (Self[index].name = elementName) then
        begin
          result:=self[index];
          exit;
        end;
    end;
end;

{ TBranch }

constructor TBranch.create(name_: string; storyName_: string; story_: integer);
begin
  fName:=name_;
  fStoryName:=storyName_;
  fStory:=story_;
end;

end.

