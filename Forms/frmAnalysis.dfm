object FormAnalysis: TFormAnalysis
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'CSV Analysis'
  ClientHeight = 400
  ClientWidth = 576
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
    Top = 348
    Width = 576
    Height = 1
    Align = alBottom
    ExplicitTop = 127
    ExplicitWidth = 350
  end
  object pnlManager: TPanel
    Left = 0
    Top = 349
    Width = 576
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 139
    ExplicitWidth = 348
    object btnOk: TButton
      Left = 488
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 8
      TabOrder = 0
    end
  end
  object mAnalysis: TMemo
    Left = 0
    Top = 0
    Width = 576
    Height = 348
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
