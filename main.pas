unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,FileUtilities,Fileutil, config,process,gitManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCodeDirectory: TButton;
    Button1: TButton;
    cbCurrentRepo: TComboBox;
    cbCurrentBranch: TComboBox;
    eCodeDirectory: TEdit;
    lCurrentBranch: TLabel;
    lCurrentRepo: TLabel;
    lCodeDirectory: TLabel;
    ListBox1: TListBox;
    pTree: TPanel;
    pDirectory: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure bCodeDirectoryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbCurrentBranchSelect(Sender: TObject);
    procedure cbCurrentRepoSelect(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fGitWhat: TGitWhat;
    procedure onCodeDirectoryChanged(sender:TObject);
    procedure onReposChanged(sender:TObject);
    procedure onCurrentRepoChanged(sender:TObject);
    procedure onCurrentBranchChanged(sender:TObject);
    procedure loadNames(currentRepoName:string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bCodeDirectoryClick(Sender: TObject);
begin
  If selectDirectoryDialog1.Execute then fGitWhat.codeDirectory:= selectDirectoryDialog1.FileName;
  if (eCodeDirectory.Text <> fGitWhat.codeDirectory)
     then eCodeDirectory.Font.Color:=clRed
     else eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.cbCurrentBranchSelect(Sender: TObject);
begin
  fGitWhat.currentBranch:=cbCurrentBranch.Text;
end;

procedure TForm1.cbCurrentRepoSelect(Sender: TObject);
begin
  fGitwhat.currentRepoName:=cbCurrentRepo.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fGitWhat.saveConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  //Create the git manager and its config
  fGitWhat:=TGitWhat.create(
    getUsrDir('cloudsoft')+'/.gitwhat.cfg',
    @onCodeDirectoryChanged,
    @onReposChanged,
    @onCurrentRepoChanged,
    @onCurrentBranchChanged);
  eCodeDirectory.Text:=fGitWhat.codeDirectory;
  cbCurrentRepo.Items:= fGitWhat.getRepoNames;
  cbCurrentRepo.ItemIndex:=cbCurrentRepo.items.indexOf(fGitWhat.currentRepoName);
  onCurrentRepoChanged(self);
end;

procedure TForm1.onCodeDirectoryChanged(sender: TObject);
  begin
  fGitWhat.rescanRepos;
  eCodeDirectory.Text:=fGitWhat.codeDirectory;
  eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.onReposChanged(sender: TObject);
var
  currentRepoName:String;
begin
messagedlg('','repos changed',mtInformation,[mbOK],0);
if (cbCurrentRepo.ItemIndex > -1)
   then currentRepoName:=cbCurrentRepo.Items[cbCurrentRepo.ItemIndex]
   else currentRepoName:='';
   loadNames(currentRepoName);
end;

procedure TForm1.onCurrentRepoChanged(sender: TObject);
begin
  cbCurrentBranch.Items:=fGitWhat.branches;
  cbCurrentBranch.ItemIndex:=cbCurrentBranch.items.indexOf(fGitWhat.currentBranch);
end;

procedure TForm1.onCurrentBranchChanged(sender: TObject);
begin
  messagedlg('','current branch changed',mtInformation,[mbOK],0);
  //currently no action
end;

procedure TForm1.loadNames(currentRepoName:string);
var
  currentRepoNameIndex:integer;
begin
  cbCurrentRepo.Clear;
  cbCurrentRepo.Items:=fGitWhat.getRepoNames;
  if (currentRepoName <> '') then
    begin
      currentRepoNameIndex:= cbCurrentRepo.Items.IndexOf(currentRepoName);
      cbCurrentRepo.ItemIndex:=currentRepoNameIndex;
    end;
end;

end.

