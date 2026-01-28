unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList;

type
  TFormParserCSV = class(TForm)
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miHelp: TMenuItem;
    miOpen: TMenuItem;
    miDelimiter_1: TMenuItem;
    miExit: TMenuItem;
    miAbout: TMenuItem;
    alMain: TActionList;
    aExit: TAction;
    aAbout: TAction;
    procedure aExitExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormParserCSV: TFormParserCSV;

implementation

{$R *.dfm}

uses
  frmAbout;

procedure TFormParserCSV.aAboutExecute(Sender: TObject);
begin
  with TFormAbout.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormParserCSV.aExitExecute(Sender: TObject);
begin
  Close;
end;

end.
