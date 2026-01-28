object FormParserCSV: TFormParserCSV
  Left = 0
  Top = 0
  Caption = 'CSV to JSON Convertor'
  ClientHeight = 327
  ClientWidth = 624
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mmMain
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 200
    Top = 0
    Height = 308
    Color = clScrollBar
    ParentColor = False
    ExplicitLeft = 256
    ExplicitTop = 144
    ExplicitHeight = 100
  end
  object Splitter2: TSplitter
    Left = 421
    Top = 0
    Height = 308
    Align = alRight
    Color = clScrollBar
    ParentColor = False
    ExplicitLeft = 336
    ExplicitTop = 152
    ExplicitHeight = 100
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 308
    Width = 624
    Height = 19
    Panels = <
      item
        Text = 'Version:'
        Width = 150
      end
      item
        Text = 'Time:'
        Width = 50
      end>
    ExplicitTop = 300
    ExplicitWidth = 622
  end
  object pnlCSVData: TPanel
    Left = 0
    Top = 0
    Width = 200
    Height = 308
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlJSONData: TPanel
    Left = 424
    Top = 0
    Width = 200
    Height = 308
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 430
  end
  object pnlGridData: TPanel
    Left = 203
    Top = 0
    Width = 218
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitLeft = 188
    ExplicitWidth = 229
  end
  object mmMain: TMainMenu
    Left = 24
    Top = 8
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
      Caption = 'Help'
      object miAbout: TMenuItem
        Action = aAbout
      end
    end
  end
  object alMain: TActionList
    Left = 72
    Top = 8
    object aExit: TAction
      Caption = 'Exit'
      OnExecute = aExitExecute
    end
    object aAbout: TAction
      Caption = 'About'
      OnExecute = aAboutExecute
    end
  end
  object tmTime: TTimer
    OnTimer = tmTimeTimer
    Left = 208
    Top = 272
  end
end
