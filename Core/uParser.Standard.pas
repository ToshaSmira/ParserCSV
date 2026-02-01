unit uParser.Standard;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Threading, 
  System.Diagnostics, System.Math, uParser.Core, uConstants, uUtils;

type
  TStandardCsvParser = class(TInterfacedObject, ICsvParser)
  private
    procedure ParseCsvToJson(const aFileName: string; 
      onProgress: TProgressCallback; onComplete: TCompletionCallback);
  public
    procedure Convert(const aFileName: string; onProgress: TProgressCallback; 
      onComplete: TCompletionCallback);
  end;

implementation

procedure TStandardCsvParser.Convert(const aFileName: string; 
  onProgress: TProgressCallback; onComplete: TCompletionCallback);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      ParseCsvToJson(aFileName, onProgress, onComplete);
    end
  ).Start;
end;

procedure TStandardCsvParser.ParseCsvToJson(const aFileName: string; 
  onProgress: TProgressCallback; onComplete: TCompletionCallback);
var
  jsonArray: TJSONArray;
  stopwatch: TStopwatch;

  csvLines: TStringList;
  headers: TStringList;
  fields: TStringList;

begin
  if not FileExists(aFileName) then
    raise Exception.Create('File not found: ' + aFileName);

  csvLines := TStringList.Create;
  headers := TStringList.Create;
  fields := TStringList.Create;
  jsonArray := TJSONArray.Create;

  try
    stopwatch := TStopwatch.StartNew;
    try
      csvLines.LoadFromFile(aFileName);

      if csvLines.Count = 0 then
      begin
        stopwatch.Stop;
        if Assigned(onComplete) then
          TThread.Synchronize(nil, 
            procedure
            begin
              onComplete(True, stopwatch.ElapsedMilliseconds, cEmptyJsonArray, 'Empty file');
            end);
        Exit;
      end;

      // Parse headers
      headers.StrictDelimiter := True;
      headers.Delimiter := DetectCsvDelimiter(csvLines[0]);
      headers.DelimitedText := csvLines[0];

      // Clean headers
      for var i := 0 to headers.Count - 1 do
        headers[i] := CleanCsvField(headers[i]);

      if csvLines.Count = 1 then
      begin
        stopwatch.Stop;
        if Assigned(onComplete) then
          TThread.Synchronize(nil,
            procedure
            begin
              onComplete(True, stopwatch.ElapsedMilliseconds, cEmptyJsonArray, 'Only headers');
            end);
        Exit;
      end;

      // Parse data rows
      fields.StrictDelimiter := True;
      fields.Delimiter := headers.Delimiter;

      var lastProgress := cProgressInitial;
      for var i := 1 to csvLines.Count - 1 do
      begin
        if Trim(csvLines[i]).IsEmpty then
          Continue;

        fields.DelimitedText := csvLines[i];
        
        if fields.Count = 0 then
          Continue;

        var jsonObj := TJSONObject.Create;
        try
          for var j := 0 to Min(headers.Count - 1, fields.Count - 1) do
            jsonObj.AddPair(headers[j], CleanCsvField(fields[j]));
          
          jsonArray.AddElement(jsonObj);
        except
          jsonObj.Free;
          raise;
        end;

        // Update progress only when percentage changes
        var currentProgress := Trunc((i / (csvLines.Count - 1)) * cProgressComplete);
        if (currentProgress <> lastProgress) and Assigned(onProgress) then
        begin
          lastProgress := currentProgress;
          TThread.Queue(nil,
            procedure
            begin
              onProgress(currentProgress);
            end);
        end;
      end;

      // Final progress update to 100% (only if not already at 100)
      if (lastProgress < cProgressComplete) and Assigned(onProgress) then
        TThread.Queue(nil,
          procedure
          begin
            onProgress(cProgressComplete);
          end);

      stopwatch.Stop;

      if Assigned(onComplete) then
        TThread.Synchronize(nil,
          procedure
          begin
            onComplete(True, stopwatch.ElapsedMilliseconds, jsonArray.ToJSON,
              Format(cFormatProcessedRows, [jsonArray.Count]));
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
    csvLines.Free;
    headers.Free;
    fields.Free;
    jsonArray.Free;
  end;
end;

end.
