unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw, Vcl.Grids, MSHTML;

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
    pnlCSVCaption: TPanel;
    Shape1: TShape;
    pnlGridCaption: TPanel;
    Shape2: TShape;
    pnlJSONCaption: TPanel;
    Shape3: TShape;
    pnlGridManager: TPanel;
    pnlJSONManager: TPanel;
    wbJSONView: TWebBrowser;
    edJSONPath: TEdit;
    btnJSONSave: TButton;
    sgGridView: TStringGrid;
    btnCSVAnalysis: TButton;
    pnlCSVParseData: TPanel;
    pnlCSVManager: TPanel;
    btnCSVLoad: TButton;
    btnCSVOpen: TButton;
    lbCSVPathBrowse: TLabel;
    edCSVPath: TEdit;
    pnlCSVProcess: TPanel;
    pnlCSVProcessView: TPanel;
    pnlCSVProcessBar: TPanel;
    lbCSVProcess: TLabel;
    pbCSVProgress: TProgressBar;
    mmCSVProcessInfo: TMemo;
    cbParseType: TComboBox;
    odCSVOpen: TOpenDialog;
    aCSVOpen: TAction;
    aCSVLoad: TAction;
    miLoad: TMenuItem;
    aJSONSave: TAction;
    aAnalysis: TAction;
    procedure aExitExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmTimeTimer(Sender: TObject);
    procedure aCSVOpenExecute(Sender: TObject);
    procedure aCSVLoadExecute(Sender: TObject);
    procedure aJSONSaveExecute(Sender: TObject);
    procedure aAnalysisExecute(Sender: TObject);
  private
    FIsRunning: Boolean;
    FJSONContent: String;
    FSelectedFile: String;

    procedure LogClear;
    procedure LogMessage(const aMessage: string);

    procedure SetControls(aEnabled: Boolean);
    procedure UpdateProgress(aPercent: Integer);
    procedure OnComplete(aSuccess: Boolean; aElapsedMs: Int64; const aJsonResult: string; const aMessage: string);

    procedure DisplayGrid(const aJson: string);
    procedure DisplayJson(const aJson: string);
  end;

var
  FormParserCSV: TFormParserCSV;

implementation

{$R *.dfm}

uses
  uUtils,
  uConstants,
  uParser.Core,
  uParser.Fast,
  uParser.Fire,
  uParser.Standard,
  System.IOUtils,
  System.JSON,
  frmAbout,
  frmAnalysis;

procedure TFormParserCSV.aAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TFormParserCSV.aAnalysisExecute(Sender: TObject);
begin
  ShowAnalysis(FSelectedFile);
end;

procedure TFormParserCSV.aCSVLoadExecute(Sender: TObject);
var
  Parser: ICsvParser;
begin
  LogMessage('');
  aCSVOpen.Enabled := False;

  case TParseType(cbParseType.ItemIndex) of
    ptFast : begin
      LogMessage('=== Starting Fast Conversion ===');
      LogMessage('Method: Low-level pointer parsing with TTask');
      Parser := TFastCsvParser.Create;
    end;
    ptFire : begin
      LogMessage('=== Starting Fire Conversion ===');
      LogMessage('Method: Fire parsing with TTask');
      Parser := TFireCsvParser.Create;
    end
    else
    begin //ptStandard
      LogMessage('=== Starting Standard Conversion ===');
      LogMessage('Method: TStringList with TThread');
      Parser := TStandardCsvParser.Create;
    end;
  end;

  FIsRunning := True;
  SetControls(False);
  Parser.Convert(FSelectedFile, UpdateProgress, OnComplete);
end;

procedure TFormParserCSV.aCSVOpenExecute(Sender: TObject);
begin
   if odCSVOpen.Execute then
  begin
    FSelectedFile := odCSVOpen.FileName;

    aCSVLoad.Enabled := True;
    pnlCSVProcess.Visible := True;
    edCSVPath.Text := FSelectedFile;

    LogClear;
    LogMessage('Selected file: ' + ExtractFileName(FSelectedFile));
    LogMessage('File size: ' + FormatFloat(cFormatFileSize, TFile.GetSize(FSelectedFile)) + ' bytes');
  end;
end;

procedure TFormParserCSV.aExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormParserCSV.aJSONSaveExecute(Sender: TObject);
begin
  var jsonContent := TStringList.Create;
  try
    jsonContent.Text := FJSONContent;
    jsonContent.SaveToFile(edJSONPath.Text, TEncoding.UTF8);

    LogMessage('');
    LogMessage('JSON saved to: ' + ExtractFileName(edJSONPath.Text));
    ShowMessage('JSON file saved successfully!');
  finally
    jsonContent.Free;
  end;
end;

procedure TFormParserCSV.DisplayGrid(const aJson: string);
begin
  var gridData := JsonToGridData(aJson);
  if not gridData.IsValid then
    raise Exception.Create(gridData.ErrorMessage);

  sgGridView.ColCount := Length(gridData.Headers);
  sgGridView.RowCount := Length(gridData.Rows) + 1;
  sgGridView.FixedRows := 1;

  // Set headers
  for var colIndex := 0 to High(gridData.Headers) do
    sgGridView.Cells[colIndex, 0] := gridData.Headers[colIndex];

  // Set rows
  for var rowIndex := 0 to High(gridData.Rows) do
    for var colIndex := 0 to High(gridData.Rows[rowIndex]) do
      sgGridView.Cells[colIndex, rowIndex + 1] := gridData.Rows[rowIndex][colIndex];
end;

procedure TFormParserCSV.DisplayJson(const aJson: string);
begin
  wbJSONView.Navigate('about:blank');
  while wbJSONView.ReadyState <> READYSTATE_COMPLETE do
    Application.ProcessMessages;

  if Assigned(wbJSONView.Document) then
  begin
    var htmlDoc := wbJSONView.Document as IHTMLDocument2;
    var Data := VarArrayCreate([0, 0], varVariant);
    Data[0] := JsonToHtml(aJson);

    htmlDoc.write(PSafeArray(TVarData(Data).VArray));
    htmlDoc.close;
  end;
end;

procedure TFormParserCSV.FormShow(Sender: TObject);
begin
  FJSONContent := '';
  FSelectedFile := '';
  FIsRunning := False;

  SetControls(False);
  edJSONPath.Enabled := False;
  pnlCSVProcess.Visible := False;

  odCSVOpen.Filter := cCsvFileFilter;
  odCSVOpen.Title := cCsvDialogTitle;

  sbMain.Panels[0].Text := GetAppVersion;
  sbMain.Panels[1].Text := GetAppTimeText;

  cbParseType.ItemIndex := ptStandard.ToInteger;
end;

procedure TFormParserCSV.LogClear;
begin
  mmCSVProcessInfo.Clear;
  with mmCSVProcessInfo.Lines do
  begin
    Add('===  CSV to JSON Converter  ===');
    Add('');
  end;
end;

procedure TFormParserCSV.LogMessage(const aMessage: string);
begin
  mmCSVProcessInfo.Lines.Add(aMessage);
  //Auto-scroll to bottom
  SendMessage(mmCSVProcessInfo.Handle, EM_LINESCROLL, 0, mmCSVProcessInfo.Lines.Count);
end;

procedure TFormParserCSV.OnComplete(aSuccess: Boolean; aElapsedMs: Int64; const aJsonResult: string; const aMessage: string);
begin
  FIsRunning := False;

  if aSuccess then
  begin
    FJSONContent := aJsonResult;

    DisplayGrid(FJSONContent);
    DisplayJson(FJSONContent);
    edJSONPath.Text := ChangeFileExt(FSelectedFile, cExtJson);

    LogMessage('Completed successfully!');
    LogMessage(Format(cFormatElapsedTime, [aElapsedMs, aElapsedMs / cMillisecondsPerSecond]));
    LogMessage(aMessage);
  end
  else
  begin
    LogMessage('Failed: ' + aMessage);
    ShowMessage('Conversion failed: ' + aMessage);
  end;

  SetControls(aSuccess);
  aCSVOpen.Enabled := True;
end;

procedure TFormParserCSV.SetControls(aEnabled: Boolean);
begin
  aCSVLoad.Enabled := aEnabled;
  aJSONSave.Enabled := aEnabled;
  aAnalysis.Enabled := aEnabled;

  sgGridView.Enabled := aEnabled;
  wbJSONView.Enabled := aEnabled;

  if not aEnabled then
    pbCSVProgress.Position := 0;
end;

procedure TFormParserCSV.tmTimeTimer(Sender: TObject);
begin
  var FormTime := GetAppTimeText;
  if not FormTime.Equals(sbMain.Panels[1].Text) then
    sbMain.Panels[1].Text := FormTime;
end;

procedure TFormParserCSV.UpdateProgress(aPercent: Integer);
begin
  pbCSVProgress.Position := aPercent;
end;

end.
