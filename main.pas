unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,FileUtilities,Fileutil, config,process;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCodeDirectory: TButton;
    Button1: TButton;
    cbCurrentRepo: TComboBox;
    eCodeDirectory: TEdit;
    lCurrentRepo: TLabel;
    lCodeDirectory: TLabel;
    ListBox1: TListBox;
    Process1: TProcess;
    pTree: TPanel;
    pDirectory: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure bCodeDirectoryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure eCodeDirectoryChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function RunCommand(directory,command:string):TStringList;
  private
    fConfig: TConfig;
    function loadConfig(configFileName: string):TConfig;
    procedure repositoriesChanged(sender:TObject);
    procedure codeDirectoryChanged(sender:TObject);
    procedure saveConfig(configFileName: String; config_ :TConfig);
    procedure loadNames(currentRepoName:string);
    property config: TConfig read fConfig write fConfig;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bCodeDirectoryClick(Sender: TObject);
begin
  If selectDirectoryDialog1.Execute then config.codeDirectory:= selectDirectoryDialog1.FileName;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  listbox1.Items:=runCommand('/Users/cloudsoft/Code/housekeeper-app','git branch');
end;

procedure TForm1.eCodeDirectoryChange(Sender: TObject);
begin
  if (eCodeDirectory.Text <> config.codeDirectory)
     then eCodeDirectory.Font.Color:=clRed
     else eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //Read a config file containing the code directory and all git repositories
  config:= loadConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
  loadNames('');
  eCodeDirectory.Text:=config.codeDirectory;
end;

function TForm1.RunCommand(directory, command: string): TStringList;
begin
  chdir(directory);
  if not directoryExists('.git') then exit;
  Process1.Executable := '/bin/sh';
  Process1.Parameters.Add('-c');
  Process1.Parameters.Add(command);
  Process1.Options := Process1.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
  Process1.Execute;
  result:=TStringlist.Create;
  result.LoadFromStream(Process1.Output);
end;

function TForm1.loadConfig(configFileName: string): TConfig;
begin
  if (fileExists(configFileName))
     then result:= TConfig.Create(@repositoriesChanged,@codeDirectoryChanged,openFileAsArray(configFileName, #$0A))
     else result:= TConfig.Create(@repositoriesChanged,@codeDirectoryChanged);
end;

procedure TForm1.repositoriesChanged(sender: TObject);
var
  currentRepoName:String;
begin
  if (cbCurrentRepo.ItemIndex > -1)
     then currentRepoName:=cbCurrentRepo.Items[cbCurrentRepo.ItemIndex]
     else currentRepoName:='';
     loadNames(currentRepoName);
end;

procedure TForm1.codeDirectoryChanged(sender: TObject);
begin
  if (messageDlg('','Code directory has changed. Rescan?',mtConfirmation,[mbYes, mbNo],'') = mrYes)
     then config.rescanRepos;
     eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.saveConfig(configFileName: String; config_: TConfig);
var
  configArray:TStringArray;
  fileContents:string;
  index:integer;
begin
  fileContents:='';
  configArray:=config.toStringArray; //need config to xmlDocument method as well
  for index:=0 to pred(length(configArray)) do
    begin
    fileContents:=fileContents+configArray[index];
    if (index < pred(length(configArray))) then
    fileContents:=fileContents+ #$0A;
    end;
  writeStream(configFileName, fileContents);
end;

procedure TForm1.loadNames(currentRepoName:string);
var
  currentRepoNameIndex:integer;
begin
  cbCurrentRepo.Clear;
  cbCurrentRepo.Items:=config.repoNames;
  if (currentRepoName <> '') then
    begin
      currentRepoNameIndex:= cbCurrentRepo.Items.IndexOf(currentRepoName);
      cbCurrentRepo.ItemIndex:=currentRepoNameIndex;
    end;
end;

end.

