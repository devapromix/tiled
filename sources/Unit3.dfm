object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Messages'
  ClientHeight = 83
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormClick
  OnClick = FormClick
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MsgLog: TRichEdit
    Left = 0
    Top = 0
    Width = 447
    Height = 83
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Zoom = 100
    ExplicitLeft = 216
    ExplicitTop = 40
  end
end
