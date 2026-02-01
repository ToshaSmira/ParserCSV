unit uParser.Fire;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Diagnostics,
  System.IOUtils, FireDAC.Comp.BatchMove, FireDAC.Comp.BatchMove.Text,
  FireDAC.Comp.BatchMove.JSON, FireDAC.Stan.Intf, uParser.Core, uConstants, uUtils;

type
  TFireCsvParser = class(TInterfacedObject, ICsvParser)
  private
    procedure ParseCsvToJson(const aFileName: string;
      onProgress: TProgressCallback; onComplete: TCompletionCallback);
  public
    procedure Convert(const aFileName: string; onProgress: TProgressCallback;
      onComplete: TCompletionCallback);
  end;

implementation

procedure TFireCsvParser.Convert(const aFileName: string;
  onProgress: TProgressCallback; onComplete: TCompletionCallback);
begin
  TTask.Run(
    procedure
    begin
      ParseCsvToJson(aFileName, onProgress, onComplete);
    end);
end;

procedure TFireCsvParser.ParseCsvToJson(const aFileName: string;
  onProgress: TProgressCallback; onComplete: TCompletionCallback);
var
  batchMove: TFDBatchMove;
  textReader: TFDBatchMoveTextReader;
  jsonWriter: TFDBatchMoveJSONWriter;
  stopwatch: TStopwatch;
  memStream: TMemoryStream;
begin
  if not FileExists(aFileName) then
    raise Exception.Create('File not found: ' + aFileName);

  batchMove := TFDBatchMove.Create(nil);
  textReader := TFDBatchMoveTextReader.Create(nil);
  jsonWriter := TFDBatchMoveJSONWriter.Create(nil);
  memStream := TMemoryStream.Create;

  try
    stopwatch := TStopwatch.StartNew;
    try
      var delimiter := cDelimiterComma;
      var lines := TStringList.Create;
      try
        lines.LoadFromFile(aFileName);
        if lines.Count > 0 then
          delimiter := DetectCsvDelimiter(lines[0]);
      finally
        lines.Free;
      end;

      jsonWriter.Stream := memStream;
      textReader.FileName := aFileName;
      textReader.DataDef.Separator := delimiter;
      textReader.DataDef.WithFieldNames := True;

      batchMove.Reader := textReader;
      batchMove.Writer := jsonWriter;
      batchMove.GuessFormat([taDelimSep, taHeader, taFields]);
      batchMove.Mappings.Clear;
      batchMove.Mode := dmAlwaysInsert;

      for var i := 0 to textReader.DataDef.Fields.Count - 1 do
        textReader.DataDef.Fields[i].DataType := atString;

      batchMove.Execute;
      if Assigned(onProgress) then
        TThread.Queue(nil,
          procedure
          begin
            onProgress(cProgressComplete);
          end);

      stopwatch.Stop;
      memStream.Position := 0;

      var jsonResult := '';
      var stringStream := TStringStream.Create('', TEncoding.UTF8);
      try
        stringStream.CopyFrom(memStream, memStream.Size);
        jsonResult := stringStream.DataString;
      finally
        stringStream.Free;
      end;

      if Assigned(onComplete) then
        TThread.Synchronize(nil,
          procedure
          begin
            onComplete(True, stopwatch.ElapsedMilliseconds, jsonResult,
              Format(cFormatProcessedRows, [batchMove.ReadCount]));
          end);

    except
      on E: Exception do
      begin
        if Assigned(onComplete) then
          TThread.Synchronize(nil,
            procedure
            begin
              onComplete(False, 0, '', E.Message);
            end);
      end;
    end;
  finally
    memStream.Free;
    jsonWriter.Free;
    textReader.Free;
    batchMove.Free;
  end;
end;

end.
