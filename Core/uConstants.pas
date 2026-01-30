unit uConstants;

interface

const
  // View
  cViewOffset = '   ';

  // CSV Delimiters
  cDelimiterComma = ',';
  cDelimiterSemicolon = ';';
  cQuoteChar = '"';

  // File Extensions
  cExtJson = '.json';
  cExtCsv = '.csv';

  // File Dialog Constants
  cCsvFileFilter = 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
  cCsvDialogTitle = 'Select CSV File';

  // Progress Constants
  cProgressComplete = 100;
  cProgressUpdateInterval = 10; // Update every N rows
  cProgressInitial = -1;

  // Time Conversion
  cMillisecondsPerSecond = 1000;

  // Format Strings
  cFormatFileSize = '#,##0';
  cFormatTime = 'HH:nn:ss';
  cFormatElapsedTime = 'Time: %d ms (%.2f sec)';
  cFormatProcessedRows = 'Processed %d rows';

  // JSON Constants
  cEmptyJsonArray = '[]';

implementation

end.
