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
    pnlVersionInfo: TPanel;
    pnlCopyright: TPanel;
    lbCopyright: TLabel;
    AboutShapeTp: TShape;
    lbGitHubLink: TLabel;
    procedure FormShow(Sender: TObject);
    procedure lbGitHubLinkClick(Sender: TObject);
  end;

procedure ShowAbout;

implementation

{$R *.dfm}

uses
  uUtils,
  Winapi.ShellAPI;

{ TFormAbout }

procedure ShowAbout;
begin
  with TFormAbout.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  pnlVersionInfo.Caption := GetAppVersion;
end;

procedure TFormAbout.lbGitHubLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(lbGitHubLink.Caption), nil, nil, SW_SHOWNORMAL);
end;

end.
