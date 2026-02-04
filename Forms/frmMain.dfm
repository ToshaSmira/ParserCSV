object FormParserCSV: TFormParserCSV
  Left = 0
  Top = 0
  Caption = 'CSV to JSON Convertor'
  ClientHeight = 423
  ClientWidth = 818
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
    Left = 250
    Top = 0
    Height = 404
    Color = clScrollBar
    ParentColor = False
    ExplicitLeft = 256
    ExplicitTop = 144
    ExplicitHeight = 100
  end
  object Splitter2: TSplitter
    Left = 565
    Top = 0
    Height = 404
    Align = alRight
    Color = clScrollBar
    ParentColor = False
    ExplicitLeft = 336
    ExplicitTop = 152
    ExplicitHeight = 100
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 404
    Width = 818
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
    ExplicitTop = 396
    ExplicitWidth = 816
  end
  object pnlCSVData: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 404
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 396
    object pnlCSVCaption: TPanel
      Left = 0
      Top = 0
      Width = 250
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      Caption = '1. Load CSV'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object Shape1: TShape
        AlignWithMargins = True
        Left = 5
        Top = 46
        Width = 240
        Height = 1
        Margins.Left = 5
        Margins.Right = 5
        Align = alBottom
        Pen.Color = clGray
        ExplicitLeft = 0
        ExplicitTop = -15
        ExplicitWidth = 200
      end
    end
    object pnlCSVParseData: TPanel
      Left = 0
      Top = 50
      Width = 250
      Height = 85
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        250
        85)
      object lbCSVPathBrowse: TLabel
        Left = 8
        Top = 2
        Width = 41
        Height = 15
        Caption = 'Browse:'
      end
      object edCSVPath: TEdit
        Left = 5
        Top = 23
        Width = 240
        Height = 23
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
      object cbParseType: TComboBox
        Left = 5
        Top = 56
        Width = 239
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'Standard'
        Items.Strings = (
          'Standard'
          'Fast'
          'Fire')
      end
    end
    object pnlCSVManager: TPanel
      Left = 0
      Top = 135
      Width = 250
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        250
        35)
      object btnCSVLoad: TButton
        Left = 170
        Top = 4
        Width = 75
        Height = 25
        Action = aCSVLoad
        Anchors = [akRight, akBottom]
        TabOrder = 0
      end
      object btnCSVOpen: TButton
        Left = 86
        Top = 4
        Width = 75
        Height = 25
        Action = aCSVOpen
        Anchors = [akRight, akBottom]
        TabOrder = 1
      end
    end
    object pnlCSVProcess: TPanel
      Left = 0
      Top = 170
      Width = 250
      Height = 231
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object pnlCSVProcessView: TPanel
        Left = 0
        Top = 0
        Width = 250
        Height = 231
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnlCSVProcessBar: TPanel
          Left = 0
          Top = 0
          Width = 250
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            250
            41)
          object lbCSVProcess: TLabel
            Left = 8
            Top = 10
            Width = 43
            Height = 15
            Caption = 'Process:'
          end
          object pbCSVProgress: TProgressBar
            Left = 57
            Top = 8
            Width = 188
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
        end
        object mmCSVProcessInfo: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 44
          Width = 244
          Height = 184
          Align = alClient
          Lines.Strings = (
            '')
          TabOrder = 1
        end
      end
    end
  end
  object pnlJSONData: TPanel
    Left = 568
    Top = 0
    Width = 250
    Height = 404
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 566
    ExplicitHeight = 396
    object pnlJSONCaption: TPanel
      Left = 0
      Top = 0
      Width = 250
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      Caption = '3. Export JSON'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object Shape3: TShape
        AlignWithMargins = True
        Left = 5
        Top = 46
        Width = 240
        Height = 1
        Margins.Left = 5
        Margins.Right = 5
        Align = alBottom
        Pen.Color = clGray
        ExplicitLeft = 0
        ExplicitTop = -15
        ExplicitWidth = 200
      end
    end
    object pnlJSONManager: TPanel
      Left = 0
      Top = 339
      Width = 250
      Height = 65
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 331
      DesignSize = (
        250
        65)
      object edJSONPath: TEdit
        Left = 7
        Top = 6
        Width = 238
        Height = 23
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
      object btnJSONSave: TButton
        Left = 170
        Top = 34
        Width = 75
        Height = 25
        Action = aJSONSave
        Anchors = [akRight, akBottom]
        TabOrder = 1
      end
    end
    object wbJSONView: TWebBrowser
      AlignWithMargins = True
      Left = 5
      Top = 50
      Width = 240
      Height = 289
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 2
      ExplicitHeight = 281
      ControlData = {
        4C000000D8130000E51700000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object pnlGridData: TPanel
    Left = 253
    Top = 0
    Width = 312
    Height = 404
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 310
    ExplicitHeight = 396
    object pnlGridCaption: TPanel
      Left = 0
      Top = 0
      Width = 312
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      Caption = '2. View && Edit Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      ExplicitWidth = 310
      object Shape2: TShape
        AlignWithMargins = True
        Left = 5
        Top = 46
        Width = 302
        Height = 1
        Margins.Left = 5
        Margins.Right = 5
        Align = alBottom
        Pen.Color = clGray
        ExplicitLeft = 0
        ExplicitTop = -15
        ExplicitWidth = 200
      end
    end
    object pnlGridManager: TPanel
      Left = 0
      Top = 368
      Width = 312
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 360
      ExplicitWidth = 310
      DesignSize = (
        312
        36)
      object btnCSVAnalysis: TButton
        Left = 232
        Top = 5
        Width = 75
        Height = 25
        Action = aAnalysis
        Anchors = [akRight, akBottom]
        TabOrder = 0
        ExplicitLeft = 230
      end
    end
    object sgGridView: TStringGrid
      AlignWithMargins = True
      Left = 5
      Top = 50
      Width = 302
      Height = 318
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 2
      ExplicitWidth = 300
      ExplicitHeight = 310
    end
  end
  object mmMain: TMainMenu
    Left = 136
    Top = 328
    object miFile: TMenuItem
      Caption = 'File'
      object miOpen: TMenuItem
        Action = aCSVOpen
      end
      object miLoad: TMenuItem
        Action = aCSVLoad
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
    Left = 80
    Top = 328
    object aExit: TAction
      Caption = 'Exit'
      OnExecute = aExitExecute
    end
    object aAbout: TAction
      Caption = 'About'
      OnExecute = aAboutExecute
    end
    object aCSVOpen: TAction
      Caption = 'Open'
      OnExecute = aCSVOpenExecute
    end
    object aCSVLoad: TAction
      Caption = 'Load'
      OnExecute = aCSVLoadExecute
    end
    object aJSONSave: TAction
      Caption = 'Save'
      OnExecute = aJSONSaveExecute
    end
    object aAnalysis: TAction
      Caption = 'Analysis'
      OnExecute = aAnalysisExecute
    end
  end
  object tmTime: TTimer
    OnTimer = tmTimeTimer
    Left = 24
    Top = 328
  end
  object odCSVOpen: TOpenDialog
    Filter = 'CSV (*.csv)|*.csv'
    Left = 56
    Top = 258
  end
end
