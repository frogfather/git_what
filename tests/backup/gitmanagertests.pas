unit gitmanagerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,gitmanager;

type

  { TGitWhatTestCase }

  TGitWhatTestCase= class(TTestCase)
  protected
    fGitWhat: TGitWhat;
    fCodeDirectoryChangedHandlerCalled:boolean;
    fReposChangedHandlerCalled:boolean;
    fCurrentRepoChangedHandlerCalled:boolean;
    fCurrentBranchChangedHandlerCalled:boolean;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure codeDirectoryChanged(sender:TObject);
    procedure reposChanged(sender:TObject);
    procedure currentRepoChanged(sender:TObject);
    procedure currentBranchChanged(sender:TObject);
  published
    procedure ManagerCreatedWithNoConfigFile;
  end;

implementation

procedure TGitWhatTestCase.ManagerCreatedWithNoConfigFile;
begin
  AssertEquals(fGitWhat.branches.Count,0);
  AssertEquals(fGitWhat.currentBranch,'');
  AssertEquals(fGitWhat.currentRepoName,'');
end;

procedure TGitWhatTestCase.SetUp;
begin
  fGitWhat:=TGitWhat.create('none',@codeDirectoryChanged,@reposChanged,@currentRepoChanged,@currentBranchChanged);
  fCodeDirectoryChangedHandlerCalled:=false;
  fReposChangedHandlerCalled:=false;
  fCurrentRepoChangedHandlerCalled:=false;
  fCurrentBranchChangedHandlerCalled:=false;
end;

procedure TGitWhatTestCase.TearDown;
begin

end;

procedure TGitWhatTestCase.codeDirectoryChanged(sender: TObject);
begin
  fCodeDirectoryChangedHandlerCalled:=true;
end;

procedure TGitWhatTestCase.reposChanged(sender: TObject);
begin
  fReposChangedHandlerCalled:=true;
end;

procedure TGitWhatTestCase.currentRepoChanged(sender: TObject);
begin
  fCurrentRepoChangedHandlerCalled:=true;
end;

procedure TGitWhatTestCase.currentBranchChanged(sender: TObject);
begin
  fCurrentBranchChangedHandlerCalled:=true;
end;

initialization
  RegisterTest(TGitWhatTestCase);
end.

