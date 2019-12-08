object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 557
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    681
    557)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 530
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 665
    Height = 510
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 536
    Top = 524
    Width = 137
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create 100'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 120
    Top = 32
  end
end
