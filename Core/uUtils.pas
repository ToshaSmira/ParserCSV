unit uUtils;

interface

uses
  System.SysUtils, Winapi.Windows;

function GetAppVersion: string;

implementation

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

end.
