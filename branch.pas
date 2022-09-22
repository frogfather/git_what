unit branch;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils;
type
  TBranch = record
    branch_name:String;
    branch_lastModified: TDateTime;
  end;
  TBranches = specialize Tarray<TBranch>;

  { TBranchHelper }
  TBranchHelper = type helper for TBranches
  function size: integer;
  function push(branch_:TBranch):integer;
  function indexOf(branchName_: string):integer;
  function delete(branchName_:string):integer;
  end;

implementation

{ TBranchHelper }

function TBranchHelper.size: integer;
begin
  result:= length(self);
end;

function TBranchHelper.push(branch_: TBranch): integer;
begin
  if (self.indexOf(branch_.branch_name) = -1) then
    insert(branch_,self,length(self));
  result:=self.size;
end;

function TBranchHelper.indexOf(branchName_: string): integer;
var
  index:integer;
begin
  result:=-1;
  for index:=0 to pred(self.size) do
    begin
    if self[index].branch_name = branchName_ then
      begin
        result:=index;
        Exit;
      end;
    end;
end;

function TBranchHelper.delete(branchName_:string): integer;
var
  branchIndex:integer;
begin
  result:=self.size;
  branchIndex:= self.indexOf(branchName_);
  if (branchIndex = -1) then exit;
  while (branchIndex < pred(self.size))  do
  self[branchIndex]:=self[branchIndex+1];
  setLength(self, length(self) - 1);
end;



end.

