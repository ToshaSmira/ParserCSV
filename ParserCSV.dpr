program ParserCSV;

uses
  Vcl.Forms,
  frmMain in 'Forms\frmMain.pas' {FormParserCSV},
  frmAbout in 'Forms\frmAbout.pas' {FormAbout},
  uUtils in 'Core\uUtils.pas',
  uConstants in 'Core\uConstants.pas',
  uParser.Core in 'Core\uParser.Core.pas',
  uParser.Standard in 'Core\uParser.Standard.pas',
  uParser.Fast in 'Core\uParser.Fast.pas',
  uParser.Analysis in 'Core\uParser.Analysis.pas',
  frmAnalysis in 'Forms\frmAnalysis.pas' {FormAnalysis},
  uParser.Fire in 'Core\uParser.Fire.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormParserCSV, FormParserCSV);
  Application.Run;
end.
