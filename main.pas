unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  FileUtilities, Fileutil, SynEdit,SynHighlighterPosition, SynEditHighlighter, config, process, gitManager, gitResponse;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCodeDirectory: TButton;
    cbCurrentRepo: TComboBox;
    cbCurrentBranch: TComboBox;
    eCodeDirectory: TEdit;
    eStory: TEdit;
    ePivotal: TEdit;
    lProject: TLabel;
    lStory: TLabel;
    lCurrentBranch: TLabel;
    lCurrentRepo: TLabel;
    lCodeDirectory: TLabel;
    lbLog: TListBox;
    pLog: TPanel;
    pTree: TPanel;
    pDirectory: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    spMain: TSplitter;
    gitBranchView: TSynEdit;
    spLog: TSplitter;
    procedure cbCurrentBranchSelect(Sender: TObject);
    procedure cbCurrentRepoSelect(Sender: TObject);
    procedure eCodeDirectoryDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gitBranchViewChange(Sender: TObject);
  private
    fGitWhat: TGitWhat;
    fHighlighter:TSynPositionHighlighter;
    fAttrTest:TtkTokenKind;
    procedure onCodeDirectoryChanged(sender:TObject);
    procedure onReposChanged(sender:TObject);
    procedure onCurrentRepoChanged(sender:TObject);
    procedure onCurrentBranchChanged(sender:TObject);
    procedure loadNames(currentRepoName:string);
    procedure updateBranchList;
    function getCurrentBranchIndex(branchList:TStrings):Integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbCurrentBranchSelect(Sender: TObject);
begin
  if fGitWhat.currentrepo = nil then exit;
  fGitWhat.currentRepo.currentBranch:=cbCurrentBranch.Text;
end;

procedure TForm1.cbCurrentRepoSelect(Sender: TObject);
begin
  fGitwhat.currentRepoName:=cbCurrentRepo.Text;
end;

procedure TForm1.eCodeDirectoryDblClick(Sender: TObject);
begin
  If selectDirectoryDialog1.Execute then fGitWhat.codeDirectory:= selectDirectoryDialog1.FileName;
  if (eCodeDirectory.Text <> fGitWhat.codeDirectory)
     then eCodeDirectory.Font.Color:=clRed
     else eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fGitWhat.saveToFile(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  fHighlighter:=TSynPositionHighlighter.Create(Self);
  fAttrTest:=fHighlighter.CreateTokenID('AttrTest',clGreen,clNone,[]);
  gitBranchView.Highlighter:= fHighlighter;
  //Create the git manager and its config
  fGitWhat:=TGitWhat.create(
    @onCodeDirectoryChanged,
    @onReposChanged,
    @onCurrentRepoChanged,
    @onCurrentBranchChanged);
  fGitWhat.loadFromFile(getUsrDir('cloudsoft')+'/.gitwhat.cfg');
  eCodeDirectory.Text:=fGitWhat.codeDirectory;
  cbCurrentRepo.Items:= fGitWhat.getRepoNames;
  cbCurrentRepo.ItemIndex:=cbCurrentRepo.items.indexOf(fGitWhat.currentRepoName);
  updateBranchList;
  cbCurrentBranchSelect(self);
end;

procedure TForm1.gitBranchViewChange(Sender: TObject);
var
  index:integer;
begin
  for index:= 0 to pred(gitBranchView.Lines.Count) do
    begin
    fHighlighter.AddToken(index,gitBranchView.Lines[index].Length,fAttrTest);
    end;
end;

procedure TForm1.onCodeDirectoryChanged(sender: TObject);
  begin
  eCodeDirectory.Text:=fGitWhat.codeDirectory;
  eCodeDirectory.Font.Color:=clBlack;
end;

procedure TForm1.onReposChanged(sender: TObject);
var
  currentRepoName:String;
begin
if (cbCurrentRepo.ItemIndex > -1)
   then currentRepoName:=cbCurrentRepo.Items[cbCurrentRepo.ItemIndex]
   else currentRepoName:='';
   loadNames(currentRepoName);
end;

procedure TForm1.onCurrentRepoChanged(sender: TObject);
begin
  updateBranchList;
  cbCurrentBranchSelect(self);
  lbLog.items.add('Switched to repo '+cbCurrentRepo.Text+' - branch '+cbCurrentBranch.Text);
end;

procedure TForm1.onCurrentBranchChanged(sender: TObject);
var
  index:integer;
begin
  //sender here will be a TGitResponse containing the result of the operation
  if sender is TGitResponse then with sender as TGitResponse do
    begin
    if success then
      begin
      gitBranchView.ClearAll;
      updateBranchList;
      for index:=0 to pred(results.Count) do
      gitBranchView.Lines.Add(results[index]);
      gitBranchViewChange(nil);
      lbLog.items.add('Switched to branch '+cbCurrentBranch.Text);
      end
    else
      begin
      lbLog.items.add('Couldn''t switch branch. Response was: '+errors[0]);
      updateBranchList;
      end;
    end;
end;

procedure TForm1.loadNames(currentRepoName:string);
var
  currentRepoNameIndex:integer;
begin
  cbCurrentRepo.Clear;
  cbCurrentBranch.Clear;
  cbCurrentRepo.Items:=fGitWhat.getRepoNames;
  if (currentRepoName <> '') then
    begin
      currentRepoNameIndex:= cbCurrentRepo.Items.IndexOf(currentRepoName);
      cbCurrentRepo.ItemIndex:=currentRepoNameIndex;
    end;
end;

procedure TForm1.updateBranchList;
begin
  cbCurrentBranch.Items:=fGitWhat.branches;
  cbCurrentBranch.ItemIndex:= getCurrentBranchIndex(cbCurrentBranch.Items);
end;

function TForm1.getCurrentBranchIndex(branchList: TStrings): Integer;
begin
  for result:=0 to pred(branchList.Count) do
    if branchList[result].Substring(0,1) = '*' then exit;
  result:=-1;
end;
end.

