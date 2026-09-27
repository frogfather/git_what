unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ValEdit, FileUtilities, Fileutil, SynEdit, SynHighlighterPosition,
  SynEditHighlighter, process, gitManager, gitResponse, fpjson,
  jsonparser, TypInfo,pivotalApi,setting, Types;

type

  { TmainForm }

  TmainForm = class(TForm)
    bCodeDirectory: TButton;
    bSave: TButton;
    cbCurrentRepo: TComboBox;
    cbCurrentBranch: TComboBox;
    eCodeDirectory: TEdit;
    lbRepoStatus: TListBox;
    lCurrentBranch: TLabel;
    lCurrentRepo: TLabel;
    lCodeDirectory: TLabel;
    lbLog: TListBox;
    PageControl1: TPageControl;
    pLog: TPanel;
    pTree: TPanel;
    pDirectory: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    spMain: TSplitter;
    gitBranchView: TSynEdit;
    spLog: TSplitter;
    tsMain: TTabSheet;
    tsSettings: TTabSheet;
    vleSettings: TValueListEditor;
    procedure bSaveClick(Sender: TObject);
    procedure cbCurrentBranchSelect(Sender: TObject);
    procedure cbCurrentRepoSelect(Sender: TObject);
    procedure eCodeDirectoryDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gitBranchViewChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
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
    function extractJSON(inputData: TJSONData; objectName: string; outputList: TStringlist; whitespace: string = ''): TStringlist;
    procedure setInitialSettings;
  public

  end;

var
  mainForm: TmainForm;

implementation

{$R *.lfm}
const configFileName = '/.gitwhat/config.csv';
const dataFileName = '/.gitwhat/data.xml';

{ TmainForm }

//Sets the requested branch name on the GitManager
//Once the requested branch has been successfully selected
//The onCurrentBranchChanged event is fired.
procedure TmainForm.cbCurrentBranchSelect(Sender: TObject);
begin
  if (fGitWhat.currentrepo = nil) or (cbCurrentBranch.Text = '') then exit;
  if (fGitWhat.currentBranchName <> cbCurrentBranch.Text) then
     fGitWhat.currentBranchName:= cbCurrentBranch.Text;
end;

procedure TmainForm.bSaveClick(Sender: TObject);
begin
  //filename hard coded for the moment

end;

//Sets the requested repo name on the gitManager.
//Once the repo has been selected the onCurrentRepoChanged event
//is fired.
procedure TmainForm.cbCurrentRepoSelect(Sender: TObject);
begin
  fGitwhat.currentRepoName:=cbCurrentRepo.Text;
end;

procedure TmainForm.eCodeDirectoryDblClick(Sender: TObject);
begin
  If selectDirectoryDialog1.Execute then fGitWhat.codeDirectory:= selectDirectoryDialog1.FileName;
  if (eCodeDirectory.Text <> fGitWhat.codeDirectory)
     then eCodeDirectory.Font.Color:=clRed
     else eCodeDirectory.Font.Color:=clBlack;
end;

procedure TmainForm.FormDestroy(Sender: TObject);
begin
  fGitWhat.saveToFile(getUsrDir('johncampbell')+dataFileName);
end;


procedure TmainForm.FormShow(Sender: TObject);
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
  fGitWhat.loadFromFile(getUsrDir('johncampbell')+dataFileName);
  eCodeDirectory.Text:=fGitWhat.codeDirectory;
  cbCurrentRepo.Items:= fGitWhat.getRepoNames;
  cbCurrentRepo.ItemIndex:=cbCurrentRepo.items.indexOf(fGitWhat.currentRepoName);
  updateBranchList;
  cbCurrentBranchSelect(self);
end;

procedure TmainForm.gitBranchViewChange(Sender: TObject);
var
  index:integer;
begin
  for index:= 0 to pred(gitBranchView.Lines.Count) do
    begin
    fHighlighter.AddToken(index,gitBranchView.Lines[index].Length,fAttrTest);
    end;
end;

procedure TmainForm.PageControl1Change(Sender: TObject);
var
  index: integer;
begin
  If PageControl1.ActivePageIndex = 0 then exit;
  if (fGitWhat.settings.size <> 6) then setInitialSettings;
  //Now populate the list with the settings
  vleSettings.Clear;
  for index:=0 to pred(fGitWhat.settings.size) do
    begin
    vleSettings.InsertRow(fGitWhat.settings[index].name,fGitWhat.settings[index].value,true);
    end;
end;

//Event fired if the code directory is changed.
procedure TmainForm.onCodeDirectoryChanged(sender: TObject);
  begin
  eCodeDirectory.Text:=fGitWhat.codeDirectory;
  eCodeDirectory.Font.Color:=clBlack;
end;

//Event fired if the list of repos has changed
procedure TmainForm.onReposChanged(sender: TObject);
var
  currentRepoName:String;
begin
if (cbCurrentRepo.ItemIndex > -1)
   then currentRepoName:=cbCurrentRepo.Items[cbCurrentRepo.ItemIndex]
   else currentRepoName:='';
   loadNames(currentRepoName);
end;

//Event fired if the gitManager has successfully switched to a new repo.
procedure TmainForm.onCurrentRepoChanged(sender: TObject);
begin
  updateBranchList;
  cbCurrentBranchSelect(self);
  lbRepoStatus.items:=fGitWhat.currentrepo.status;
  lbLog.items.add('Switched to repo '+cbCurrentRepo.Text+' - branch '+cbCurrentBranch.Text);
end;

//Event fired if the gitManager has successfully switched to a new branch.
procedure TmainForm.onCurrentBranchChanged(sender: TObject);
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
      //Could have setting to automatically stash before switch and apply any
      //stashed changes when switching back
      begin
      lbLog.items.add('Couldn''t switch branch. Response was: '+errors[0]);
      updateBranchList;
      end;
    end;
end;

procedure TmainForm.loadNames(currentRepoName:string);
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

procedure TmainForm.updateBranchList;
begin
  cbCurrentBranch.Items:=fGitWhat.branches;
  cbCurrentBranch.ItemIndex:= getCurrentBranchIndex(cbCurrentBranch.Items);
end;

function TmainForm.getCurrentBranchIndex(branchList: TStrings): Integer;
begin
  for result:=0 to pred(branchList.Count) do
    if branchList[result].Substring(0,1) = '*' then exit;
  result:=-1;
end;

function TmainForm.extractJSON(inputData: TJSONData; objectName: string;
  outputList: TStringlist; whitespace: string): TStringlist;
var
  object_type: string;
  itemNo:Integer;
  jItem:TJSONData;
  bracket: char;
begin
  object_type := GetEnumName(TypeInfo(TJSONtype), Ord(inputData.JSONType));
  case object_type of
    'jtObject', 'jtArray':
      begin
      if (object_type = 'jtObject') then bracket := '{' else bracket := '[';
      if length(objectName) > 0 then
      outputList.Add(whitespace+objectName+': '+bracket) else
        outputList.Add(whitespace+bracket);
      whitespace:= whitespace + '    ';
      for itemNo:=0 to inputData.Count - 1 do
        begin
          jItem := inputData.Items[itemNo];
          //if it's an array the items won't have names
          if (object_type = 'jtObject') then
          extractJSON(jItem, TJSONObject(inputData).Names[itemNo], outputList, whitespace)
          else extractJSON(jItem, '', outputList, whitespace);
        end;
      whitespace:=whitespace.Substring(0, length(whitespace)-4);
      if (object_type = 'jtObject') then bracket := '}' else bracket := ']';
      outputList.Add(whitespace+bracket);
      end;
    'jtNumber': outputList.Add(whitespace+objectName+': '+inttostr(inputData.AsInteger));
    'jtString': outputList.Add(whitespace+objectName+': '+inputData.AsString);
    'jtBoolean': outputList.Add(whitespace+objectName+': '+booltostr(inputData.AsBoolean));
  end;
  result:=outputList;
end;

procedure TmainForm.setInitialSettings;
begin
  fGitWhat.settings.clear;
  fGitWhat.settings.push(TSetting.create('Rebase all repos on load','true',TDataType.booleanType));
  fGitWhat.settings.push(TSetting.create('Rebase all branches when switching repo','true',TDataType.booleanType));
  fGitWhat.settings.push(TSetting.create('Stash before rebasing','true',TDataType.booleanType));
  fGitWhat.settings.push(TSetting.create('Unstash after rebasing','true',TDataType.booleanType));
  fGitWhat.settings.push(TSetting.create('Stash before switching branches','true',TDataType.booleanType));
  fGitWhat.settings.push(TSetting.create('Unstash after switching branches','true',TDataType.booleanType));
end;

end.

