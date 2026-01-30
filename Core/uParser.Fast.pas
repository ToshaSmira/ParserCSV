unit uParser.Fast;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  System.Threading, System.Diagnostics, System.Math, uParser.Core, uConstants;

type
  TFastCsvParser = class(TBaseCsvParser)
  private
    procedure ParseCsvToJson(const aFileName: string; onProgress: TProgressCallback; onComplete: TCompletionCallback);
  public
    procedure Convert(const aFileName: string; onProgress: TProgressCallback; onComplete: TCompletionCallback); override;
  end;

implementation

procedure TFastCsvParser.Convert(const aFileName: string; 
  onProgress: TProgressCallback; onComplete: TCompletionCallback);
begin
  TTask.Run(
    procedure 
    begin 
      ParseCsvToJson(aFileName, onProgress, onComplete); 
    end);
end;

procedure TFastCsvParser.ParseCsvToJson(const aFileName: string; 
  onProgress: TProgressCallback; onComplete: TCompletionCallback);
var
  fileStream: TFileStream;
  buffer: TBytes;
  stopwatch: TStopwatch;
  
  headers: TList<string>;
  dataRows: TList<TArray<string>>;
  jsonArray: TJSONArray;
  
  textContent: string;
  delimiter: Char;
  pStart, pCurrent, pEnd, pFieldStart: PChar;
  
begin
  if not FileExists(aFileName) then
    raise Exception.Create('File not found: ' + aFileName);

  fileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  headers := TList<string>.Create;
  dataRows := TList<TArray<string>>.Create;
  jsonArray := TJSONArray.Create;

  try
    stopwatch := TStopwatch.StartNew;
    try
      if fileStream.Size = 0 then
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

      // Load and prepare content
      SetLength(buffer, fileStream.Size);
      fileStream.ReadBuffer(buffer[0], fileStream.Size);
      textContent := TEncoding.UTF8.GetString(buffer);
      delimiter := DetectDelimiter(textContent);
      
      pStart := PChar(textContent);
      pCurrent := pStart;
      pEnd := pStart + Length(textContent);
      pFieldStart := pStart;

      // Parse headers
      while pCurrent < pEnd do
      begin
        if pCurrent^ = delimiter then
        begin
          var fieldValue: string;
          SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
          headers.Add(CleanField(fieldValue));
          Inc(pCurrent);
          pFieldStart := pCurrent;
        end
        else if (pCurrent^ = #13) and ((pCurrent + 1) < pEnd) and ((pCurrent + 1)^ = #10) then
        begin
          var fieldValue: string;
          SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
          headers.Add(CleanField(fieldValue));
          Inc(pCurrent, 2);
          pFieldStart := pCurrent;
          Break;
        end
        else if pCurrent^ = #10 then
        begin
          var fieldValue: string;
          SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
          headers.Add(CleanField(fieldValue));
          Inc(pCurrent);
          pFieldStart := pCurrent;
          Break;
        end
        else
          Inc(pCurrent);
      end;

      if pCurrent >= pEnd then
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
      var rowCount := 0;
      while pCurrent < pEnd do
      begin
        var currentRow: TArray<string>;
        SetLength(currentRow, headers.Count);
        var fieldIndex := 0;

        while (pCurrent < pEnd) and (fieldIndex < headers.Count) do
        begin
          if pCurrent^ = delimiter then
          begin
            var fieldValue: string;
            SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
            currentRow[fieldIndex] := CleanField(fieldValue);
            Inc(fieldIndex);
            Inc(pCurrent);
            pFieldStart := pCurrent;
          end
          else if (pCurrent^ = #13) and ((pCurrent + 1) < pEnd) and ((pCurrent + 1)^ = #10) then
          begin
            var fieldValue: string;
            SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
            currentRow[fieldIndex] := CleanField(fieldValue);
            Inc(fieldIndex);
            Inc(pCurrent, 2);
            pFieldStart := pCurrent;
            Break;
          end
          else if pCurrent^ = #10 then
          begin
            var fieldValue: string;
            SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
            currentRow[fieldIndex] := CleanField(fieldValue);
            Inc(fieldIndex);
            Inc(pCurrent);
            pFieldStart := pCurrent;
            Break;
          end
          else
            Inc(pCurrent);
        end;

        // Handle last field if at end of file
        if (pCurrent >= pEnd) and (fieldIndex < headers.Count) and (pFieldStart < pEnd) then
        begin
          var fieldValue: string;
          SetString(fieldValue, pFieldStart, pCurrent - pFieldStart);
          currentRow[fieldIndex] := CleanField(fieldValue);
          Inc(fieldIndex);
        end;

        if fieldIndex > 0 then
        begin
          dataRows.Add(currentRow);
          Inc(rowCount);
          
          // Update progress periodically
          if rowCount mod cProgressUpdateInterval = 0 then
          begin
            var currentProgress := Trunc(((pCurrent - pStart) / (pEnd - pStart)) * cProgressComplete);
            if Assigned(onProgress) then
              TThread.Queue(nil, 
                procedure 
                begin
                  onProgress(currentProgress);
                end);
          end;
        end;
      end;

      // Final progress update to 100%
      if Assigned(onProgress) then
        TThread.Queue(nil, 
          procedure 
          begin
            onProgress(cProgressComplete);
          end);

      stopwatch.Stop;

      // Build JSON from parsed data
      for var i := 0 to dataRows.Count - 1 do
      begin
        var jsonObj := TJSONObject.Create;
        try
          var row := dataRows[i];
          for var j := 0 to Min(headers.Count - 1, Length(row) - 1) do
            jsonObj.AddPair(headers[j], row[j]);
          
          jsonArray.AddElement(jsonObj);
        except
          jsonObj.Free;
          raise;
        end;
      end;

      if Assigned(onComplete) then
        TThread.Synchronize(nil, 
          procedure 
          begin
            onComplete(True, stopwatch.ElapsedMilliseconds, jsonArray.ToJSON, 
              Format(cFormatProcessedRows, [dataRows.Count]));
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
    fileStream.Free;
    headers.Free;
    dataRows.Free;
    jsonArray.Free;
  end;
end;

end.
