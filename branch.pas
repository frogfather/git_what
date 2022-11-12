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
  procedure clear;
  procedure delete(branch:TBranch);
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
  if (self.indexOf(element) = -1) then
  setLength(self, succ(self.size));
  self[pred(self.size)] := element;
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
  if (self = nil) or (self.size = 0) then exit; //These are the same thing!
  for index:= 0 to pred(self.size) do
    begin
      if (Self[index].name = elementName) then
        begin
          result:=self[index];
          exit;
        end;
    end;
end;

procedure TBranchHelper.clear;
begin
  setLength(self,0);
end;

procedure TBranchHelper.delete(branch: TBranch);
var
  itemAt, index:integer;
begin
  itemAt:=self.indexOf(branch);
  if (itemAt > -1) then
  begin
    for index:=itemAt to pred(self.size) do
      if (index < pred(self.size)) then self[index]:= self[index+1];
    setLength(Self,length(self)-1);
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

