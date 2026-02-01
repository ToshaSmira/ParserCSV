unit uParser.Analysis;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.DateUtils,
  System.RegularExpressions, System.StrUtils, System.Math, uUtils;

type
  TFormatInfo = record
    Pattern: string;
    Count: Integer;
    ExampleRow: Integer;
    ExampleColumn: string;
    ExampleValue: string;
  end;

  TLogicalError = record
    RowNumber: Integer;
    Description: string;
    Details: string;
  end;

  TPatternIdentifier = reference to function(const value: string): string;

  TDataQualityAnalyzer = class
  private
    FFileName: string;
    FTotalRows: Integer;
    FHeaders: TStringList;
    FDataRows: TList<TStringList>;
    FDateFormats, FTimeFormats, FIntervalFormats: TDictionary<string, TFormatInfo>;
    FLogicalErrors: TList<TLogicalError>;
    
    procedure LoadCsvFile;
    procedure AnalyzeColumnFormats(const columnNames: TArray<string>; 
      patternFunc: TPatternIdentifier; formatDict: TDictionary<string, TFormatInfo>);
    procedure AddOrUpdateFormat(const pattern, columnName, value: string; 
      rowIdx: Integer; formatDict: TDictionary<string, TFormatInfo>);
    procedure AnalyzeLogicalErrors;
    function IdentifyDatePattern(const value: string): string;
    function IdentifyTimePattern(const value: string): string;
    function IdentifyIntervalPattern(const value: string): string;
    function ParseDateTime(const dateStr, timeStr: string): TDateTime;
    function GenerateReport: string;
    function GetOverallStatus: string;
    function GetColIdx(const name: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AnalyzeFile(const fileName: string): string;
  end;

implementation

constructor TDataQualityAnalyzer.Create;
begin
  inherited;
  FDateFormats := TDictionary<string, TFormatInfo>.Create;
  FTimeFormats := TDictionary<string, TFormatInfo>.Create;
  FIntervalFormats := TDictionary<string, TFormatInfo>.Create;
  FLogicalErrors := TList<TLogicalError>.Create;
  FHeaders := TStringList.Create;
  FDataRows := TList<TStringList>.Create;
end;

destructor TDataQualityAnalyzer.Destroy;
var
  i: Integer;
begin
  FDateFormats.Free;
  FTimeFormats.Free;
  FIntervalFormats.Free;
  FLogicalErrors.Free;
  FHeaders.Free;
  for i := 0 to FDataRows.Count - 1 do
    FDataRows[i].Free;
  FDataRows.Free;
  inherited;
end;

function TDataQualityAnalyzer.GetColIdx(const name: string): Integer;
begin
  Result := FHeaders.IndexOf(name);
end;

procedure TDataQualityAnalyzer.LoadCsvFile;
var
  csvLines: TStringList;
  i: Integer;
  row: TStringList;
  delimiter: Char;
begin
  csvLines := TStringList.Create;
  try
    csvLines.LoadFromFile(FFileName);
    if csvLines.Count = 0 then
      raise Exception.Create('CSV file is empty');
    
    delimiter := DetectCsvDelimiter(csvLines[0]);
    ParseCsvLine(csvLines[0], delimiter, FHeaders);
    
    for i := 1 to csvLines.Count - 1 do
    begin
      if Trim(csvLines[i]) = '' then
        Continue;
      
      row := TStringList.Create;
      ParseCsvLine(csvLines[i], delimiter, row);
      if row.Count > 0 then
        FDataRows.Add(row)
      else
        row.Free;
    end;
    
    FTotalRows := FDataRows.Count;
  finally
    csvLines.Free;
  end;
end;

function TDataQualityAnalyzer.IdentifyDatePattern(const value: string): string;
begin
  if TRegEx.IsMatch(value, '^\d{8}$') then
    Result := 'YYYYMMDD (no separator)'
  else if TRegEx.IsMatch(value, '^\d{4}-\d{2}-\d{2}$') then
    Result := 'YYYY-MM-DD (dash separator)'
  else if TRegEx.IsMatch(value, '^\d{4}/\d{2}/\d{2}$') then
    Result := 'YYYY/MM/DD (slash separator)'
  else if TRegEx.IsMatch(value, '^\d{4}:\d{2}:\d{2}$') then
    Result := 'YYYY:MM:DD (colon separator)'
  else
    Result := 'Unknown format';
end;

function TDataQualityAnalyzer.IdentifyTimePattern(const value: string): string;
begin
  if TRegEx.IsMatch(value, '^\d{4}$') then
    Result := 'HHMM (continuous, no sep)'
  else if TRegEx.IsMatch(value, '^\d{2}:\d{2}$') then
    Result := 'HH:MM (colon separator)'
  else if TRegEx.IsMatch(value, '^\d{2}-\d{2}$') then
    Result := 'HH-MM (dash separator)'
  else if TRegEx.IsMatch(value, '^\d{2}/\d{2}$') then
    Result := 'HH/MM (slash separator)'
  else
    Result := 'Unknown format';
end;

function TDataQualityAnalyzer.IdentifyIntervalPattern(const value: string): string;
var
  hasColon, hasDash, hasSlash: Boolean;
begin
  hasColon := Pos(':', value) > 0;
  hasDash := Pos('-', value) > 0;
  hasSlash := Pos('/', value) > 0;
  
  if hasColon and hasSlash and hasDash then
    Result := 'Mixed (colon+slash+dash)'
  else if hasColon and hasSlash then
    Result := 'Mixed (colon in time + slash)'
  else if hasColon and hasDash then
    Result := 'Mixed (colon in time + dash)'
  else if hasSlash then
    Result := '/ (slash) separator'
  else if hasDash then
    Result := '- (dash) separator'
  else if hasColon then
    Result := ': (colon) separator'
  else
    Result := 'Unknown separator';
end;

procedure TDataQualityAnalyzer.AddOrUpdateFormat(const pattern, columnName, value: string; 
  rowIdx: Integer; formatDict: TDictionary<string, TFormatInfo>);
var
  info: TFormatInfo;
begin
  if formatDict.TryGetValue(pattern, info) then
  begin
    Inc(info.Count);
    formatDict[pattern] := info;
  end
  else
  begin
    info.Pattern := pattern;
    info.Count := 1;
    info.ExampleRow := rowIdx + 2;  // +2 because row 1 is header
    info.ExampleColumn := columnName;
    info.ExampleValue := value;
    formatDict.Add(pattern, info);
  end;
end;

procedure TDataQualityAnalyzer.AnalyzeColumnFormats(const columnNames: TArray<string>; 
  patternFunc: TPatternIdentifier; formatDict: TDictionary<string, TFormatInfo>);
var
  i, j, colIdx: Integer;
  value, pattern: string;
begin
  for i := 0 to FDataRows.Count - 1 do
  begin
    for j := 0 to High(columnNames) do
    begin
      colIdx := GetColIdx(columnNames[j]);
      if (colIdx >= 0) and (colIdx < FDataRows[i].Count) then
      begin
        value := FDataRows[i][colIdx];
        if value <> '' then
        begin
          pattern := patternFunc(value);
          AddOrUpdateFormat(pattern, columnNames[j], value, i, formatDict);
        end;
      end;
    end;
  end;
end;

function TDataQualityAnalyzer.ParseDateTime(const dateStr, timeStr: string): TDateTime;
var
  cleanDate, cleanTime: string;
begin
  Result := 0;
  try
    cleanDate := StringReplace(StringReplace(StringReplace(dateStr, '-', '', [rfReplaceAll]), 
      '/', '', [rfReplaceAll]), ':', '', [rfReplaceAll]);
    cleanTime := StringReplace(StringReplace(StringReplace(timeStr, ':', '', [rfReplaceAll]), 
      '-', '', [rfReplaceAll]), '/', '', [rfReplaceAll]);
    
    if (Length(cleanDate) = 8) and (Length(cleanTime) >= 4) then
      Result := EncodeDateTime(StrToInt(Copy(cleanDate, 1, 4)), StrToInt(Copy(cleanDate, 5, 2)),
        StrToInt(Copy(cleanDate, 7, 2)), StrToInt(Copy(cleanTime, 1, 2)), 
        StrToInt(Copy(cleanTime, 3, 2)), 0, 0);
  except
    Result := 0;
  end;
end;

procedure TDataQualityAnalyzer.AnalyzeLogicalErrors;
var
  i, bdIdx, btIdx, edIdx, etIdx: Integer;
  beginDT, endDT: TDateTime;
  error: TLogicalError;
begin
  bdIdx := GetColIdx('Begin_Date');
  btIdx := GetColIdx('Begin_Time');
  edIdx := GetColIdx('End_Date');
  etIdx := GetColIdx('End_Time');
  
  if (bdIdx < 0) or (btIdx < 0) or (edIdx < 0) or (etIdx < 0) then
    Exit;
  
  for i := 0 to FDataRows.Count - 1 do
  begin
    if (Max(bdIdx, Max(btIdx, Max(edIdx, etIdx))) >= FDataRows[i].Count) then
      Continue;
    
    beginDT := ParseDateTime(FDataRows[i][bdIdx], FDataRows[i][btIdx]);
    endDT := ParseDateTime(FDataRows[i][edIdx], FDataRows[i][etIdx]);
    
    if (beginDT > 0) and (endDT > 0) and (endDT < beginDT) then
    begin
      error.RowNumber := i + 2;
      error.Description := 'End date/time is before begin date/time';
      error.Details := Format('Begin: %s %s, End: %s %s', 
        [FDataRows[i][bdIdx], FDataRows[i][btIdx], FDataRows[i][edIdx], FDataRows[i][etIdx]]);
      FLogicalErrors.Add(error);
    end;
  end;
end;

function TDataQualityAnalyzer.GetOverallStatus: string;
var
  issues: Integer;
begin
  issues := Ord(FDateFormats.Count > 1) + Ord(FTimeFormats.Count > 1) + Ord(FLogicalErrors.Count > 0);
  
  if issues >= 3 then
    Result := 'CRITICAL'
  else if issues >= 1 then
    Result := 'WARNING'
  else if FIntervalFormats.Count > 1 then
    Result := 'ATTENTION NEEDED'
  else
    Result := 'GOOD';
end;

function TDataQualityAnalyzer.GenerateReport: string;
var
  sb: TStringList;
  key: string;
  info: TFormatInfo;
  i: Integer;
  error: TLogicalError;
  status: string;
  
  procedure AddSection(const title, priority: string; dict: TDictionary<string, TFormatInfo>; 
    const hasValue: Boolean);
  var
    key: string;
    info: TFormatInfo;
  begin
    if dict.Count > 1 then
    begin
      sb.Add(Format('%s (%s)', [title, priority]));
      sb.Add('   Found ' + IntToStr(dict.Count) + ' different formats:');
      sb.Add('');
      sb.Add('   Format Pattern                 | Count | Example Row | Column      | Value');
      sb.Add('   ------------------------------ | ----- | ----------- | ----------- | ----------');
      for key in dict.Keys do
      begin
        info := dict[key];
        sb.Add(Format('   %-30s | %-5d | Row %-7d | %-11s | %s',
          [info.Pattern, info.Count, info.ExampleRow, info.ExampleColumn, info.ExampleValue]));
      end;
      sb.Add('');
    end
    else if dict.Count = 1 then
    begin
      sb.Add(title + ': OK');
      sb.Add('   All values use consistent format.');
      sb.Add('');
    end;
  end;
  
begin
  sb := TStringList.Create;
  try
    status := GetOverallStatus;
    
    sb.Add('======================== DATA QUALITY ANALYSIS REPORT ========================');
    sb.Add('File: ' + ExtractFileName(FFileName));
    sb.Add('Analysis Date: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    sb.Add('Total Rows Analyzed: ' + IntToStr(FTotalRows));
    sb.Add('');
    sb.Add('OVERALL STATUS: ' + status);
    sb.Add(IfThen(status = 'CRITICAL', 'The dataset contains multiple critical inconsistencies that require immediate',
      IfThen(status = 'WARNING', 'The dataset contains inconsistencies that should be addressed before processing.',
      IfThen(status = 'ATTENTION NEEDED', 'The dataset has minor formatting inconsistencies that should be reviewed.',
      'The dataset appears to be in good quality with consistent formatting.'))));
    if status <> 'GOOD' then
      sb.Add('attention before further processing.');
    sb.Add('');
    sb.Add('===============================================================================');
    sb.Add('DETECTED ANOMALIES');
    sb.Add('===============================================================================');
    sb.Add('');
    
    AddSection('1. DATE FORMAT INCONSISTENCIES', 'HIGH PRIORITY', FDateFormats, True);
    AddSection('2. TIME FORMAT INCONSISTENCIES', 'HIGH PRIORITY', FTimeFormats, True);
    
    if FIntervalFormats.Count > 1 then
    begin
      sb.Add('3. INTERVAL SEPARATOR INCONSISTENCIES (MEDIUM PRIORITY)');
      sb.Add('   Found ' + IntToStr(FIntervalFormats.Count) + ' different separators:');
      sb.Add('');
      sb.Add('   Separator Pattern              | Count | Example Row | Value');
      sb.Add('   ------------------------------ | ----- | ----------- | -----------------');
      for key in FIntervalFormats.Keys do
      begin
        info := FIntervalFormats[key];
        sb.Add(Format('   %-30s | %-5d | Row %-7d | %s',
          [info.Pattern, info.Count, info.ExampleRow, info.ExampleValue]));
      end;
      sb.Add('');
    end
    else if FIntervalFormats.Count = 1 then
    begin
      sb.Add('3. INTERVAL FORMAT: OK');
      sb.Add('   All intervals use consistent separator.');
      sb.Add('');
    end;
    
    if FLogicalErrors.Count > 0 then
    begin
      sb.Add('4. LOGICAL ERRORS (CRITICAL)');
      sb.Add('   Found ' + IntToStr(FLogicalErrors.Count) + ' logical inconsistencies:');
      sb.Add('');
      sb.Add('   Row | Issue Description                          | Details');
      sb.Add('   --- | ------------------------------------------ | ---------------------------');
      for i := 0 to FLogicalErrors.Count - 1 do
      begin
        error := FLogicalErrors[i];
        sb.Add(Format('   %-3d | %-42s | %s', [error.RowNumber, error.Description, error.Details]));
      end;
      sb.Add('');
    end
    else
    begin
      sb.Add('4. LOGICAL CONSISTENCY: OK');
      sb.Add('   No logical errors detected (all end dates/times are after begin dates/times).');
      sb.Add('');
    end;

    Result := sb.Text;
  finally
    sb.Free;
  end;
end;

function TDataQualityAnalyzer.AnalyzeFile(const fileName: string): string;
begin
  FFileName := fileName;
  
  try
    LoadCsvFile;
    
    // Analyze date formats
    AnalyzeColumnFormats(['Begin_Date', 'End_Date'], IdentifyDatePattern, FDateFormats);
    
    // Analyze time formats
    AnalyzeColumnFormats(['Begin_Time', 'End_Time'], IdentifyTimePattern, FTimeFormats);
    
    // Analyze interval formats
    AnalyzeColumnFormats(['Begin_And_End_Time'], IdentifyIntervalPattern, FIntervalFormats);
    
    AnalyzeLogicalErrors;
    Result := GenerateReport;
  except
    on E: Exception do
      Result := 'ERROR: Failed to analyze file.' + sLineBreak + E.Message;
  end;
end;

end.
