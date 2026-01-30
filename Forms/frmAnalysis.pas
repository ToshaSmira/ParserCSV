unit frmAnalysis;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormAnalysis = class(TForm)
    AboutShapeBtm: TShape;
    pnlManager: TPanel;
    btnOk: TButton;
    mAnalysis: TMemo;
    procedure FormShow(Sender: TObject);
  private
    FFilePath: String;
  public
    property FilePath : String read FFilePath write FFilePath;
  end;

procedure ShowAnalysis(const aPath: String);

implementation

{$R *.dfm}

uses
  uParser.Analysis;

procedure ShowAnalysis(const aPath: String);
begin
  with TFormAnalysis.Create(nil) do
  try
    FilePath := aPath;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormAnalysis.FormShow(Sender: TObject);
begin
  var analyzer := TDataQualityAnalyzer.Create;
  try
    mAnalysis.Clear;
    mAnalysis.Lines.Text := analyzer.AnalyzeFile(FilePath);
  finally
    analyzer.Free;
  end;
end;

end.
