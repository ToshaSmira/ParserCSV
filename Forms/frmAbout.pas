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
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Position := poMainFormCenter;
end;

procedure TFormAbout.FormShow(Sender: TObject);
  function GetAppVersion: string;
  var
    Size, Dummy: DWORD;
    Buffer: TBytes;
    PVerInfo: PVSFixedFileInfo;
    VerLen: UINT;
  begin
    Result := '0.0.0.0';
    var FileName := ParamStr(0);

    Size := GetFileVersionInfoSize(PChar(FileName), Dummy);
    if Size > 0 then
    begin
      SetLength(Buffer, Size);
      if GetFileVersionInfo(PChar(FileName), 0, Size, Buffer) then
      begin
        if VerQueryValue(Buffer, '\', Pointer(PVerInfo), VerLen) then
        begin
          Result := Format('%d.%d.%d.%d', [
            HiWord(PVerInfo^.dwFileVersionMS), // Major
            LoWord(PVerInfo^.dwFileVersionMS), // Minor
            HiWord(PVerInfo^.dwFileVersionLS), // Release
            LoWord(PVerInfo^.dwFileVersionLS)  // Build
          ]);
        end;
      end;
    end;
  end;
begin
  lbVersionInfo.Caption := GetAppVersion;
end;

end.
