unit frmAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFormAbout = class(TForm)
    pnlManager: TPanel;
    btnOk: TButton;
    AboutShapeBtm: TShape;
    pnlProgramName: TPanel;
    lbParserCSV: TLabel;
    pnlVersionInfo: TPanel;
    lbVersion: TLabel;
    lbVersionInfo: TLabel;
    pnlCopyright: TPanel;
    lbCopyright: TLabel;
    AboutShapeTp: TShape;
    procedure FormShow(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  uUtils;

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);
begin
  lbVersionInfo.Caption := GetAppVersion;
end;

end.
