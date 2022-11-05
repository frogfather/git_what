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
      fVersion: integer;
      fIteration_length: integer;
      fWeek_start_day: string; //should be enum
      fPoint_scale: string;
      fPoint_scale_is_custom: boolean;
      fBugs_and_chores_are_estimatable: boolean;
      fAutomatic_planning: boolean;
      fEnable_tasks: boolean;
      fTime_zone: integer; //actually an object with name and offset. We'll just store the offset
      fVelocity_averaged_over: integer;
      fNumber_of_done_iterations_to_show: integer;
      fHas_google_domain: boolean;
      fEnable_incoming_emails: boolean;
      fInitial_velocity: integer;
      fPublic: boolean;
      fAtom_enabled: boolean;
      fProject_type: string;
      fStart_time: TDateTime;
      fCreated_at: TDateTime;
      fUpdated_at: TDateTime;
      fShow_story_priority: boolean;
      fShow_priority_icon: boolean;
      fShow_priority_icon_in_all_panels: boolean;
      fAccount_id: integer;
      fCurrent_iteration_number: integer;
      fEnable_following: boolean;
    public
      constructor create(
        id,version,iteration_length,
        Time_zone,Velocity_averaged_over,
        Number_of_done_iterations_to_show,
        Initial_velocity,Account_id,
        Current_iteration_number:integer;
        kind,name,Week_start_day,Point_scale,
        Project_type: string;
        Point_scale_is_custom,Bugs_and_chores_are_estimatable,
        Automatic_planning,Enable_tasks,Has_google_domain,
        Enable_incoming_emails,Public,Atom_enabled,
        Show_story_priority,Show_priority_icon,
        Show_priority_icon_in_all_panels,Enable_following: boolean;
        Start_time,Created_at,Updated_at: string);



  end;

implementation

{ TPivotal }

constructor TPivotal.create(id, version, iteration_length, Time_zone,
  Velocity_averaged_over, Number_of_done_iterations_to_show, Initial_velocity,
  Account_id, Current_iteration_number: integer; kind, name, Week_start_day,
  Point_scale, Project_type: string; Point_scale_is_custom,
  Bugs_and_chores_are_estimatable, Automatic_planning, Enable_tasks,
  Has_google_domain, Enable_incoming_emails, Public, Atom_enabled,
  Show_story_priority, Show_priority_icon, Show_priority_icon_in_all_panels,
  Enable_following: boolean; Start_time, Created_at, Updated_at: string);
begin
  fId:=id;
  fVersion:=version;
  fiteration_length:=iteration_length;
  fTime_zone:=Time_zone;
  fVelocity_averaged_over:=Velocity_averaged_over;
  fNumber_of_done_iterations_to_show:=Number_of_done_iterations_to_show;
  fInitial_velocity:=Initial_velocity;
  fAccount_id:=Account_id;
  fCurrent_iteration_number:=Current_iteration_number;
  fkind:=kind;
  fname:=name;
  fWeek_start_day:=Week_start_day;
  fPoint_scale:=Point_scale;
  fProject_type:=Project_type;
  fPoint_scale_is_custom:=Point_scale_is_custom;
  fBugs_and_chores_are_estimatable:=Bugs_and_chores_are_estimatable;
  fAutomatic_planning:=Automatic_planning;
  fEnable_tasks:=Enable_tasks;
  fHas_google_domain:=Has_google_domain;
  fEnable_incoming_emails:=Enable_incoming_emails;
  fPublic:=Public;
  fAtom_enabled:=Atom_enabled;
  fShow_story_priority:=Show_story_priority;
  fShow_priority_icon:=Show_priority_icon;
  fShow_priority_icon_in_all_panels:=Show_priority_icon_in_all_panels;
  fEnable_following:=Enable_following;
  fStart_time:=ISO8601ToDate(Start_time);
  fCreated_at:=ISO8601ToDate(Created_at);
  fUpdated_at:=ISO8601ToDate(Updated_at);
end;

end.

