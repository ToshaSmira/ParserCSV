unit uParser.Core;

interface

uses
  System.SysUtils, uConstants;

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

  TBaseCsvParser = class(TInterfacedObject, ICsvParser)
  protected
    function DetectDelimiter(const aLine: string): Char;
    function CleanField(const aField: string): string;
  public
    procedure Convert(const aFileName: string; onProgress: TProgressCallback;
      onComplete: TCompletionCallback); virtual; abstract;
  end;

implementation

function TBaseCsvParser.DetectDelimiter(const aLine: string): Char;
begin
  Result := cDelimiterComma;
  if Pos(cDelimiterSemicolon, aLine) > 0 then
    Result := cDelimiterSemicolon
end;

function TBaseCsvParser.CleanField(const aField: string): string;
begin
  Result := Trim(aField);
  if (Length(Result) >= 2) and (Result[1] = cQuoteChar) and (Result[Length(Result)] = cQuoteChar) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ TParseTypeHelper }

function TParseTypeHelper.ToInteger: Integer;
begin
  Result := Ord(Self);
end;

end.
