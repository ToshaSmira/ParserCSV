object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 198
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  TextHeight = 15
  object AboutShapeBtm: TShape
    Left = 0
    Top = 146
    Width = 350
    Height = 1
    Align = alBottom
    ExplicitTop = 127
  end
  object AboutShapeTp: TShape
    Left = 0
    Top = 0
    Width = 350
    Height = 1
    Align = alTop
  end
  object pnlManager: TPanel
    Left = 0
    Top = 147
    Width = 350
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 139
    ExplicitWidth = 348
    object btnOk: TButton
      Left = 248
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 8
      TabOrder = 0
    end
  end
  object pnlProgramName: TPanel
    Left = 0
    Top = 1
    Width = 350
    Height = 75
    Align = alTop
    BevelOuter = bvNone
    Caption = 'ParserCSV'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object pnlVersionInfo: TPanel
    Left = 0
    Top = 76
    Width = 350
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    Caption = '  Version: 0.0.0.0'
    TabOrder = 2
    ExplicitTop = 82
    ExplicitWidth = 348
  end
  object pnlCopyright: TPanel
    Left = 0
    Top = 100
    Width = 350
    Height = 46
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 106
    ExplicitWidth = 348
    ExplicitHeight = 32
    object lbCopyright: TLabel
      Left = 103
      Top = -2
      Width = 146
      Height = 15
      Caption = 'Copyright (c) 2026 by Smira'
    end
    object lbGitHubLink: TLabel
      Left = 66
      Top = 15
      Width = 226
      Height = 15
      Cursor = crHandPoint
      Caption = 'https://github.com/ToshaSmira/ParserCSV'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lbGitHubLinkClick
    end
  end
end
