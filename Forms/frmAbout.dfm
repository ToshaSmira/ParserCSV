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
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 348
    object lbParserCSV: TLabel
      Left = 114
      Top = 33
      Width = 115
      Height = 32
      Alignment = taCenter
      Caption = 'ParserCSV'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlVersionInfo: TPanel
    Left = 0
    Top = 82
    Width = 350
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 348
    object lbVersion: TLabel
      Left = 129
      Top = 1
      Width = 44
      Height = 15
      Caption = 'Version :'
    end
    object lbVersionInfo: TLabel
      Left = 179
      Top = 1
      Width = 33
      Height = 15
      Caption = '0.0.0.0'
    end
  end
  object pnlCopyright: TPanel
    Left = 0
    Top = 106
    Width = 350
    Height = 40
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 348
    ExplicitHeight = 32
    object lbCopyright: TLabel
      Left = 98
      Top = -2
      Width = 146
      Height = 15
      Caption = 'Copyright (c) 2026 by Smira'
    end
  end
end
