program ParserCSV;

uses
  Vcl.Forms,
  frmMain in 'Forms\frmMain.pas' {FormParserCSV},
  frmAbout in 'Forms\frmAbout.pas' {FormAbout},
  uUtils in 'Core\uUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormParserCSV, FormParserCSV);
  Application.Run;
end.
