unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.ExtCtrls;

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
    sbMain: TStatusBar;
    tmTime: TTimer;
    pnlCSVData: TPanel;
    pnlJSONData: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pnlGridData: TPanel;
    procedure aExitExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmTimeTimer(Sender: TObject);
  private
    procedure SetMainData;
    function GetFormTime: String;
  public
    property FormTime : String read GetFormTime;
  end;

var
  FormParserCSV: TFormParserCSV;

implementation

{$R *.dfm}

uses
  uUtils,
  frmAbout;

const
  cViewOffset = '   ';

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

procedure TFormParserCSV.FormShow(Sender: TObject);
begin
  SetMainData;
end;

function TFormParserCSV.GetFormTime: String;
begin
  Result := Format(cViewOffset + 'Time: %s', [FormatDateTime('HH:nn:ss', Now)]);
end;

procedure TFormParserCSV.SetMainData;
begin
  sbMain.Panels[0].Text := Format(cViewOffset + 'Version: %s', [GetAppVersion]);
  sbMain.Panels[1].Text := FormTime;
end;

procedure TFormParserCSV.tmTimeTimer(Sender: TObject);
begin
  if not FormTime.Equals(sbMain.Panels[1].Text) then
    sbMain.Panels[1].Text := FormTime;
end;

end.
