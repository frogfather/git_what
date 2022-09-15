unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,FileUtilities, config;

type

  { TForm1 }

  TForm1 = class(TForm)
    bCodeDirectory: TButton;
    cbCurrentRepo: TComboBox;
    eCodeDirectory: TEdit;
    lCurrentRepo: TLabel;
    lCodeDirectory: TLabel;
    pTree: TPanel;
    pDirectory: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure bCodeDirectoryClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fConfig: TConfig;
    function loadConfig(configFileName: string):TConfig;
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
  If selectDirectoryDialog1.Execute then eCodeDirectory.Text:= selectDirectoryDialog1.FileName;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //Read a config file containing the code directory and all git repositories
  config:= loadConfig(getUsrDir('cloudsoft')+'/.gitwhat.cfg');

end;

function TForm1.loadConfig(configFileName: string): TConfig;
begin
  if (fileExists(configFileName))
     then result:= TConfig.Create(openFileAsArray(configFileName, #$0A))
     else result:= TConfig.Create;
end;

end.

