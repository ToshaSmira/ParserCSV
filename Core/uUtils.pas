unit uUtils;

interface

uses
  System.SysUtils, Winapi.Windows, System.JSON, System.Classes;

type
  TGridData = record
    Headers: TArray<string>;
    Rows: TArray<TArray<string>>;
    IsValid: Boolean;
    ErrorMessage: string;
  end;

function GetAppVersion: string;
function GetAppTimeText: string;
function ParseJsonToGridData(const aJson: string): TGridData;
function GenerateJsonHtml(const aJson: string): string;

implementation

uses
  uConstants;

function GetAppTimeText: string;
begin
  Result := Format(cViewOffset + 'Time: %s', [FormatDateTime('HH:nn:ss', Now)]);
end;

function GetAppVersion: string;
var
  Size, Dummy: DWORD;
  Buffer: TBytes;
  PVerInfo: PVSFixedFileInfo;
  VerLen: UINT;
begin
  Result := cViewOffset + 'Version: 0.0.0.0';
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

    Result := Format(cViewOffset + 'Version: %s', [Result]);
  end;
end;

function ParseJsonToGridData(const aJson: string): TGridData;
var
  rowData: TArray<string>;
begin
  Result.IsValid := False;
  Result.ErrorMessage := '';
  SetLength(Result.Headers, 0);
  SetLength(Result.Rows, 0);

  if aJson.IsEmpty then
  begin
    Result.ErrorMessage := 'JSON is empty';
    Exit;
  end;

  var headers := TStringList.Create;
  var jsonValue := TJSONObject.ParseJSONValue(aJson);
  try
    var jsonArray := jsonValue as TJSONArray;
    if jsonArray.Count = 0 then
    begin
      Result.ErrorMessage := 'JSON array is empty';
      Exit;
    end;

    try
      if not (jsonValue is TJSONArray) then
      begin
        Result.ErrorMessage := 'JSON is not an array';
        Exit;
      end;

      // Extract headers from first object
      var jsonObj := jsonArray.Items[0] as TJSONObject;
      for var colIndex := 0 to jsonObj.Count - 1 do
        headers.Add(jsonObj.Pairs[colIndex].JsonString.Value);

      // Convert headers to array
      SetLength(Result.Headers, headers.Count);
      for var colIndex := 0 to headers.Count - 1 do
        Result.Headers[colIndex] := headers[colIndex];

      // Extract row data
      SetLength(Result.Rows, jsonArray.Count);
      for var rowIndex := 0 to jsonArray.Count - 1 do
      begin
        jsonObj := jsonArray.Items[rowIndex] as TJSONObject;
        SetLength(rowData, headers.Count);

        for var colIndex := 0 to headers.Count - 1 do
        begin
          var fieldValue := jsonObj.GetValue(headers[colIndex]);
          if Assigned(fieldValue) then
            rowData[colIndex] := fieldValue.Value
          else
            rowData[colIndex] := '';
        end;
      
        Result.Rows[rowIndex] := rowData;
      end;

      Result.IsValid := True;
    except
      on E: Exception do
      begin
        Result.ErrorMessage := 'Error processing JSON: ' + E.Message;
        Result.IsValid := False;
      end;
    end;
  finally
    headers.Free;
    jsonValue.Free;
  end;
end;

function GenerateJsonHtml(const aJson: string): string;
  function EscapeHtml(const aText: string): string;
  begin
    Result := aText;
    Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  end;

  function BuildJsonTree(aValue: TJSONValue; aIndentLevel: Integer): string;
  begin
    Result := '';
    var indent := StringOfChar(' ', aIndentLevel * 2);

    if aValue is TJSONObject then
    begin
      var jsonObj := aValue as TJSONObject;
      if jsonObj.Count = 0 then
        Result := '<span class="json-bracket">{}</span>'
      else
      begin
        Result := '<span class="json-bracket">{</span>' +
                  '<span class="toggle" onclick="toggleNode(this)">[−]</span>' +
                  '<div class="json-content">';

        for var i := 0 to jsonObj.Count - 1 do
        begin
          var pair := jsonObj.Pairs[i];
          var needsComma := i < jsonObj.Count - 1;

          Result := Result + '<div class="json-line" style="margin-left: 20px;">' +
                    '<span class="json-key">"' + EscapeHtml(pair.JsonString.Value) + '"</span>' +
                    '<span class="json-colon">: </span>';

          Result := Result + BuildJsonTree(pair.JsonValue, aIndentLevel + 1);

          if needsComma then
            Result := Result + '<span class="json-comma">,</span>';

          Result := Result + '</div>';
        end;

        Result := Result + '</div>' +
                  '<div class="json-line"><span class="json-bracket">}</span></div>';
      end;
    end
    else if aValue is TJSONArray then
    begin
      var jsonArr := aValue as TJSONArray;
      if jsonArr.Count = 0 then
        Result := '<span class="json-bracket">[]</span>'
      else
      begin
        Result := '<span class="json-bracket">[</span>' +
                  '<span class="toggle" onclick="toggleNode(this)">[−]</span>' +
                  '<div class="json-content">';

        for var i := 0 to jsonArr.Count - 1 do
        begin
          var itemValue := jsonArr.Items[i];
          var needsComma := i < jsonArr.Count - 1;

          Result := Result + '<div class="json-line" style="margin-left: 20px;">';
          Result := Result + BuildJsonTree(itemValue, aIndentLevel + 1);

          if needsComma then
            Result := Result + '<span class="json-comma">,</span>';

          Result := Result + '</div>';
        end;

        Result := Result + '</div>' +
                  '<div class="json-line"><span class="json-bracket">]</span></div>';
      end;
    end
    else if aValue is TJSONString then
      Result := '<span class="json-string">"' + EscapeHtml(aValue.Value) + '"</span>'
    else if aValue is TJSONNumber then
      Result := '<span class="json-number">' + aValue.Value + '</span>'
    else if aValue is TJSONBool then
      Result := '<span class="json-boolean">' + aValue.Value + '</span>'
    else if aValue is TJSONNull then
      Result := '<span class="json-null">null</span>'
    else
      Result := '<span class="json-string">"' + EscapeHtml(aValue.Value) + '"</span>';
  end;

begin
  Result := '';

  if aJson.IsEmpty then
  begin
    Result := '<html><body style="background: #ffffff;"><p style="color: #666; font-family: Arial;">No JSON data to display</p></body></html>';
    Exit;
  end;

  var jsonValue := TJSONObject.ParseJSONValue(aJson);
  try
    try
      if not Assigned(jsonValue) then
      begin
        Result := '<html><body style="background: #ffffff;"><p style="color: #c00; font-family: Arial;">Invalid JSON format</p></body></html>';
        Exit;
      end;

      // Build interactive JSON tree
      var jsonTree := BuildJsonTree(jsonValue, 0);

      // Build HTML with interactive tree
      Result :=
        '<!DOCTYPE html>' +
        '<html>' +
        '<head>' +
        '<meta charset="UTF-8">' +
        '<style>' +
        '  body {' +
        '    margin: 0;' +
        '    padding: 20px;' +
        '    font-family: ''Consolas'', ''Monaco'', ''Courier New'', monospace;' +
        '    font-size: 14px;' +
        '    background: #ffffff;' +
        '    color: #000000;' +
        '    line-height: 1.5;' +
        '  }' +
        '  .json-container {' +
        '    background: #ffffff;' +
        '    border: 1px solid #d0d0d0;' +
        '    border-radius: 4px;' +
        '    padding: 15px;' +
        '    overflow: auto;' +
        '  }' +
        '  .json-line {' +
        '    white-space: nowrap;' +
        '  }' +
        '  .json-content {' +
        '    display: block;' +
        '  }' +
        '  .json-content.collapsed {' +
        '    display: none;' +
        '  }' +
        '  .toggle {' +
        '    display: inline-block;' +
        '    width: 20px;' +
        '    cursor: pointer;' +
        '    color: #666;' +
        '    user-select: none;' +
        '    margin-right: 5px;' +
        '    font-weight: bold;' +
        '  }' +
        '  .toggle:hover {' +
        '    color: #333;' +
        '    background-color: #f0f0f0;' +
        '  }' +
        '  .json-key {' +
        '    color: #0451a5;' +
        '    font-weight: bold;' +
        '  }' +
        '  .json-string {' +
        '    color: #a31515;' +
        '  }' +
        '  .json-number {' +
        '    color: #098658;' +
        '  }' +
        '  .json-boolean {' +
        '    color: #0000ff;' +
        '  }' +
        '  .json-null {' +
        '    color: #0000ff;' +
        '  }' +
        '  .json-bracket {' +
        '    color: #000000;' +
        '    font-weight: bold;' +
        '  }' +
        '  .json-colon {' +
        '    color: #000000;' +
        '  }' +
        '  .json-comma {' +
        '    color: #000000;' +
        '  }' +
        '</style>' +
        '<script>' +
        '  function toggleNode(element) {' +
        '    var content = element.nextElementSibling;' +
        '    while (content && !content.classList.contains(''json-content'')) {' +
        '      content = content.nextElementSibling;' +
        '    }' +
        '    if (content) {' +
        '      if (content.classList.contains(''collapsed'')) {' +
        '        content.classList.remove(''collapsed'');' +
        '        element.textContent = ''[−]'';' +
        '      } else {' +
        '        content.classList.add(''collapsed'');' +
        '        element.textContent = ''[+]'';' +
        '      }' +
        '    }' +
        '  }' +
        '</script>' +
        '</head>' +
        '<body>' +
        '<div class="json-container">' +
        jsonTree +
        '</div>' +
        '</body>' +
        '</html>';
    except
      on E: Exception do
        Result := '<html><body style="background: #ffffff;"><p style="color: #c00; font-family: Arial;">Error: ' + E.Message + '</p></body></html>';
    end;
  finally
    jsonValue.Free;
  end;
end;

end.
