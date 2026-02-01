unit uParser.Core;

interface

uses
  System.SysUtils, uConstants, uUtils;

type
  TParseType = (ptStandard, ptFast, ptFire);
  TParseTypeHelper = record helper for TParseType
    function ToInteger: Integer;
  end;

  TProgressCallback = reference to procedure(aPercent: Integer);
  TCompletionCallback = reference to procedure(aSuccess: Boolean; aElapsedMs: Int64; const aJsonResult: string; const aMessage: string);

  ICsvParser = interface
    ['{B8E7F1A2-3C4D-4E5F-A6B7-C8D9E0F1A2B3}']
    procedure Convert(const aFileName: string; onProgress: TProgressCallback;
      onComplete: TCompletionCallback);
  end;

implementation

{ TParseTypeHelper }

function TParseTypeHelper.ToInteger: Integer;
begin
  Result := Ord(Self);
end;

end.
