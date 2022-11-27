unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, FileUtilities, Fileutil, SynEdit, SynHighlighterPosition,
  SynEditHighlighter, process, gitManager, gitResponse, fpjson,
  jsonparser, TypInfo;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCodeDirectory: TButton;
    bSave: TButton;
    cbCurrentRepo: TComboBox;
    cbCurrentBranch: TComboBox;
    eCodeDirectory: TEdit;
    eTrackerToken: TEdit;
    eStory: TEdit;
    ePivotal: TEdit;
    lTrackerToken: TLabel;
    lProject: TLabel;
    lStory: TLabel;
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
    procedure bSaveClick(Sender: TObject);
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
    function extractJSON(inputData: TJSONData; objectName: string; outputList: TStringlist; whitespace: string = ''): TStringlist;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbCurrentBranchSelect(Sender: TObject);
begin
  if (fGitWhat.currentrepo = nil) or (cbCurrentBranch.Text = '') then exit;
  fGitWhat.currentBranchName:= cbCurrentBranch.Text;
end;

procedure TForm1.bSaveClick(Sender: TObject);
begin
  //filename hard coded for the moment
  writeStream(getUsrDir('cloudsoft')+'/.gitwhatpivotal.cfg', 'X-TrackerToken,'+eTrackerToken.Text);
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

function TForm1.extractJSON(inputData: TJSONData; objectName: string;
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

end.

