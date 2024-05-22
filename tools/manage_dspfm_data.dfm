object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 609
  ClientWidth = 955
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object FlowPanel1: TFlowPanel
    Left = 0
    Top = 0
    Width = 97
    Height = 609
    Align = alLeft
    Caption = 'FlowPanel1'
    TabOrder = 0
    object Button1: TButton
      Left = 1
      Top = 1
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetInsert1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 1
      Top = 26
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetEdit1
      TabOrder = 1
    end
    object Button3: TButton
      Left = 1
      Top = 51
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetCancel1
      TabOrder = 2
    end
    object Button4: TButton
      Left = 1
      Top = 76
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetDelete1
      TabOrder = 3
    end
    object Button5: TButton
      Left = 1
      Top = 101
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetRefresh1
      TabOrder = 4
    end
    object Button6: TButton
      Left = 1
      Top = 126
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetFirst1
      TabOrder = 5
    end
    object Button7: TButton
      Left = 1
      Top = 151
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetNext1
      TabOrder = 6
    end
    object Button8: TButton
      Left = 1
      Top = 176
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetPrior1
      TabOrder = 7
    end
    object Button9: TButton
      Left = 1
      Top = 201
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetLast1
      TabOrder = 8
    end
    object Button10: TButton
      Left = 1
      Top = 226
      Width = 75
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Action = DatasetPost1
      TabOrder = 9
    end
  end
  object DBGrid1: TDBGrid
    Left = 96
    Top = 296
    Width = 857
    Height = 313
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBEdit1: TDBEdit
    Left = 304
    Top = 792
    Width = 134
    Height = 21
    DataField = 'exe_num'
    DataSource = DataSource1
    TabOrder = 2
  end
  object ActionManager1: TActionManager
    Left = 688
    Top = 64
    StyleName = 'Platform Default'
    object DatasetFirst1: TDataSetFirst
      Category = 'Dataset'
      Caption = '&First'
      Hint = 'First'
      ImageIndex = 0
    end
    object DatasetPrior1: TDataSetPrior
      Category = 'Dataset'
      Caption = '&Prior'
      Hint = 'Prior'
      ImageIndex = 1
    end
    object DatasetNext1: TDataSetNext
      Category = 'Dataset'
      Caption = '&Next'
      Hint = 'Next'
      ImageIndex = 2
    end
    object DatasetLast1: TDataSetLast
      Category = 'Dataset'
      Caption = '&Last'
      Hint = 'Last'
      ImageIndex = 3
    end
    object DatasetInsert1: TDataSetInsert
      Category = 'Dataset'
      Caption = '&Insert'
      Hint = 'Insert'
      ImageIndex = 4
    end
    object DatasetDelete1: TDataSetDelete
      Category = 'Dataset'
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 5
    end
    object DatasetEdit1: TDataSetEdit
      Category = 'Dataset'
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 6
    end
    object DatasetPost1: TDataSetPost
      Category = 'Dataset'
      Caption = 'P&ost'
      Hint = 'Post'
      ImageIndex = 7
    end
    object DatasetCancel1: TDataSetCancel
      Category = 'Dataset'
      Caption = '&Cancel'
      Hint = 'Cancel'
      ImageIndex = 8
    end
    object DatasetRefresh1: TDataSetRefresh
      Category = 'Dataset'
      Caption = '&Refresh'
      Hint = 'Refresh'
      ImageIndex = 9
    end
  end
  object FDConn: TFDConnection
    Params.Strings = (
      'Database=G:\my_projects\exe\debug\DSP_FM\data\data_dspfm.db'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 848
    Top = 208
  end
  object FDTable1: TFDTable
    Active = True
    IndexFieldNames = 'num'
    Connection = FDConn
    TableName = 'arcade'
    Left = 832
    Top = 56
    object FDTable1num: TFDAutoIncField
      DisplayWidth = 10
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
    end
    object FDTable1rom: TWideMemoField
      DisplayWidth = 26
      FieldName = 'rom'
      Origin = 'rom'
      Required = True
      BlobType = ftWideMemo
    end
    object FDTable1name: TWideMemoField
      DisplayWidth = 10
      FieldName = 'name'
      Origin = 'name'
      Required = True
      BlobType = ftWideMemo
    end
    object FDTable1year: TWideMemoField
      DisplayWidth = 10
      FieldName = 'year'
      Origin = 'year'
      BlobType = ftWideMemo
    end
    object FDTable1manufactor: TWideMemoField
      DisplayWidth = 10
      FieldName = 'manufactor'
      Origin = 'manufactor'
      BlobType = ftWideMemo
    end
    object FDTable1rom_path: TWideMemoField
      DisplayWidth = 10
      FieldName = 'rom_path'
      Origin = 'rom_path'
      BlobType = ftWideMemo
    end
    object FDTable1rom_global_path: TWideMemoField
      DisplayWidth = 13
      FieldName = 'rom_global_path'
      Origin = 'rom_global_path'
      BlobType = ftWideMemo
    end
    object FDTable1exe_num: TIntegerField
      DisplayWidth = 10
      FieldName = 'exe_num'
      Origin = 'exe_num'
    end
  end
  object DataSource1: TDataSource
    DataSet = FDTable1
    Left = 824
    Top = 136
  end
end
