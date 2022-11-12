unit pvproject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,DateUtils;
type
  //represents a pivotal project - sort of

  { TPivotal }

  TPivotal = class(TInterfacedObject)
    private
      fId:integer;
      fKind: string;
      fName: string;
      fPublic: boolean;
      fProject_type: string;
      fStart_time: TDateTime;
      fCreated_at: TDateTime;
      fUpdated_at: TDateTime;
      fAccount_id: integer;
      fCurrent_iteration_number: integer;
    public
      constructor create(
        id,Account_id,Current_iteration_number:integer;
        kind,name,Project_type: string;
        Public: boolean;
        Start_time,Created_at,Updated_at: string);
  end;

implementation

{ TPivotal }

constructor TPivotal.create(
  id,Account_id, Current_iteration_number: integer;
  kind, name, Project_type: string;
  Public: boolean;
  Start_time, Created_at, Updated_at: string);
begin
  fId:=id;
  fAccount_id:=Account_id;
  fkind:=kind;
  fname:=name;
  fProject_type:=Project_type;
  fPublic:=Public;
  fStart_time:=ISO8601ToDate(Start_time);
  fCreated_at:=ISO8601ToDate(Created_at);
  fUpdated_at:=ISO8601ToDate(Updated_at);
end;

end.

