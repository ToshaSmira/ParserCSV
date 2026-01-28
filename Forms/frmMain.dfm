object FormParserCSV: TFormParserCSV
  Left = 0
  Top = 0
  Caption = 'CSV Data Processor'
  ClientHeight = 327
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mmMain
  Position = poScreenCenter
  TextHeight = 15
  object mmMain: TMainMenu
    Left = 208
    Top = 16
    object miFile: TMenuItem
      Caption = 'File'
      object miOpen: TMenuItem
        Caption = 'Open'
      end
      object miDelimiter_1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = aExit
      end
    end
    object miHelp: TMenuItem
      Action = aAbout
      object miAbout: TMenuItem
        Caption = 'About...'
      end
    end
  end
  object alMain: TActionList
    Left = 272
    Top = 16
    object aExit: TAction
      Caption = 'Exit'
      OnExecute = aExitExecute
    end
    object aAbout: TAction
      Caption = 'About'
      OnExecute = aAboutExecute
    end
  end
end
