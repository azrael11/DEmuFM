object dm: Tdm
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 811
  Width = 1345
  object fdconn: TFDConnection
    Params.Strings = (
      'Database=G:\my_projects\exe\debug\DEmuFM\data\data_dspfm.db'
      'DriverID=SQLite')
    UpdateOptions.AssignedValues = [uvLockWait, uvAutoCommitUpdates]
    UpdateOptions.AutoCommitUpdates = True
    Connected = True
    LoginPrompt = False
    Left = 32
    Top = 16
  end
  object query: TFDQuery
    Connection = fdconn
    Left = 32
    Top = 80
  end
  object tConfig: TFDTable
    CachedUpdates = True
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'config'
    Left = 104
    Top = 16
    object tConfigfirstrun: TIntegerField
      FieldName = 'firstrun'
      Origin = 'firstrun'
      Required = True
    end
    object tConfigcurrent_emu: TWideMemoField
      FieldName = 'current_emu'
      Origin = 'current_emu'
      BlobType = ftWideMemo
    end
    object tConfigprj_name: TWideMemoField
      FieldName = 'prj_name'
      Origin = 'prj_name'
      BlobType = ftWideMemo
    end
    object tConfigprj_path: TWideMemoField
      FieldName = 'prj_path'
      Origin = 'prj_path'
      BlobType = ftWideMemo
    end
    object tConfigprj_version: TWideMemoField
      FieldName = 'prj_version'
      Origin = 'prj_version'
      BlobType = ftWideMemo
    end
    object tConfigprj_images_main: TWideMemoField
      FieldName = 'prj_images_main'
      Origin = 'prj_images_main'
      BlobType = ftWideMemo
    end
    object tConfigprj_images_bar: TWideMemoField
      FieldName = 'prj_images_bar'
      Origin = 'prj_images_bar'
      BlobType = ftWideMemo
    end
    object tConfigprj_images_config: TWideMemoField
      FieldName = 'prj_images_config'
      Origin = 'prj_images_config'
      BlobType = ftWideMemo
    end
    object tConfigprj_images_flags: TWideMemoField
      FieldName = 'prj_images_flags'
      Origin = 'prj_images_flags'
      BlobType = ftWideMemo
    end
    object tConfigprj_images_controls: TWideMemoField
      FieldName = 'prj_images_controls'
      Origin = 'prj_images_controls'
      BlobType = ftWideMemo
    end
    object tConfigprj_media: TWideMemoField
      FieldName = 'prj_media'
      Origin = 'prj_media'
      BlobType = ftWideMemo
    end
    object tConfigprj_export: TWideMemoField
      FieldName = 'prj_export'
      Origin = 'prj_export'
      BlobType = ftWideMemo
    end
    object tConfigprj_themes: TWideMemoField
      FieldName = 'prj_themes'
      Origin = 'prj_themes'
      BlobType = ftWideMemo
    end
    object tConfigprj_sounds: TWideMemoField
      FieldName = 'prj_sounds'
      Origin = 'prj_sounds'
      BlobType = ftWideMemo
    end
    object tConfigprj_fonts: TWideMemoField
      FieldName = 'prj_fonts'
      Origin = 'prj_fonts'
      BlobType = ftWideMemo
    end
    object tConfigprj_temp: TWideMemoField
      FieldName = 'prj_temp'
      Origin = 'prj_temp'
      BlobType = ftWideMemo
    end
    object tConfigprj_kind: TWideMemoField
      FieldName = 'prj_kind'
      Origin = 'prj_kind'
      BlobType = ftWideMemo
    end
    object tConfiglang: TIntegerField
      FieldName = 'lang'
      Origin = 'lang'
    end
    object tConfighiscore: TWideMemoField
      FieldName = 'hiscore'
      Origin = 'hiscore'
      BlobType = ftWideMemo
    end
    object tConfignvram: TWideMemoField
      FieldName = 'nvram'
      Origin = 'nvram'
      BlobType = ftWideMemo
    end
    object tConfigqsnapshot: TWideMemoField
      FieldName = 'qsnapshot'
      Origin = 'qsnapshot'
      BlobType = ftWideMemo
    end
    object tConfigsamples: TWideMemoField
      FieldName = 'samples'
      Origin = 'samples'
      BlobType = ftWideMemo
    end
    object tConfigsound: TIntegerField
      FieldName = 'sound'
      Origin = 'sound'
    end
    object tConfigvideo: TIntegerField
      FieldName = 'video'
      Origin = 'video'
    end
    object tConfigscraper: TWideMemoField
      FieldName = 'scraper'
      Origin = 'scraper'
      BlobType = ftWideMemo
    end
  end
  object tArcade: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'arcade'
    Left = 672
    Top = 16
    object tArcadenum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tArcaderom: TWideMemoField
      Alignment = taCenter
      FieldName = 'rom'
      Origin = 'rom'
      Required = True
      BlobType = ftWideMemo
    end
    object tArcadename: TWideMemoField
      Alignment = taCenter
      FieldName = 'name'
      Origin = 'name'
      Required = True
      BlobType = ftWideMemo
    end
    object tArcadeyear: TWideMemoField
      Alignment = taCenter
      FieldName = 'year'
      Origin = 'year'
      BlobType = ftWideMemo
    end
    object tArcademanufactor: TWideMemoField
      FieldName = 'manufactor'
      Origin = 'manufactor'
      BlobType = ftWideMemo
    end
    object tArcaderom_path: TWideMemoField
      FieldName = 'rom_path'
      Origin = 'rom_path'
      BlobType = ftWideMemo
    end
    object tArcaderom_global_path: TWideMemoField
      FieldName = 'rom_global_path'
      Origin = 'rom_global_path'
      BlobType = ftWideMemo
    end
    object tArcadeexe_num: TIntegerField
      FieldName = 'exe_num'
      Origin = 'exe_num'
    end
    object tArcadestate: TWideMemoField
      FieldName = 'state'
      Origin = 'state'
      BlobType = ftWideMemo
    end
    object tArcadestate_icon: TIntegerField
      FieldName = 'state_icon'
      Origin = 'state_icon'
    end
    object tArcadestate_desc: TWideMemoField
      FieldName = 'state_desc'
      Origin = 'state_desc'
      BlobType = ftWideMemo
    end
    object tArcadestate_date: TWideMemoField
      FieldName = 'state_date'
      Origin = 'state_date'
      BlobType = ftWideMemo
    end
    object tArcadehiscore: TIntegerField
      Alignment = taCenter
      FieldName = 'hiscore'
      Origin = 'hiscore'
    end
    object tArcadetotal_time: TIntegerField
      FieldName = 'total_time'
      Origin = 'total_time'
    end
  end
  object tArcadeConfig: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'arcade_config'
    Left = 672
    Top = 80
    object tArcadeConfignum: TIntegerField
      FieldName = 'num'
      Required = True
    end
    object tArcadeConfigfullscreen: TIntegerField
      FieldName = 'fullscreen'
    end
    object tArcadeConfigfull_x: TIntegerField
      FieldName = 'full_x'
    end
    object tArcadeConfigfull_y: TIntegerField
      FieldName = 'full_y'
    end
    object tArcadeConfigbezels: TIntegerField
      FieldName = 'bezels'
    end
    object tArcadeConfigwin_center: TIntegerField
      FieldName = 'win_center'
    end
    object tArcadeConfigwin_size: TIntegerField
      FieldName = 'win_size'
    end
    object tArcadeConfigsound: TIntegerField
      FieldName = 'sound'
    end
  end
  object tArcadeTGDB: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'arcade_tgdb'
    Left = 672
    Top = 144
    object tArcadeTGDBnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tArcadeTGDBid: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      Required = True
    end
    object tArcadeTGDBtitle: TWideMemoField
      FieldName = 'title'
      Origin = 'title'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBrelease_date: TWideMemoField
      FieldName = 'release_date'
      Origin = 'release_date'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBplatform_id: TWideMemoField
      FieldName = 'platform_id'
      Origin = 'platform_id'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBplayers: TWideMemoField
      Alignment = taCenter
      FieldName = 'players'
      Origin = 'players'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBoverview: TWideMemoField
      Alignment = taCenter
      FieldName = 'overview'
      Origin = 'overview'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBlast_updated: TWideMemoField
      FieldName = 'last_updated'
      Origin = 'last_updated'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBrating: TWideMemoField
      FieldName = 'rating'
      Origin = 'rating'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBcoop: TWideMemoField
      Alignment = taCenter
      FieldName = 'coop'
      Origin = 'coop'
      BlobType = ftWideMemo
    end
    object tArcadeTGDByoutube: TWideMemoField
      FieldName = 'youtube'
      Origin = 'youtube'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBos: TWideMemoField
      FieldName = 'os'
      Origin = 'os'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBprocessor: TWideMemoField
      FieldName = 'processor'
      Origin = 'processor'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBram: TWideMemoField
      FieldName = 'ram'
      Origin = 'ram'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBhdd: TWideMemoField
      FieldName = 'hdd'
      Origin = 'hdd'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBvideo: TWideMemoField
      FieldName = 'video'
      Origin = 'video'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBsound: TWideMemoField
      FieldName = 'sound'
      Origin = 'sound'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBdevelopers: TWideMemoField
      FieldName = 'developers'
      Origin = 'developers'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBgenres: TWideMemoField
      FieldName = 'genres'
      Origin = 'genres'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBpublishers: TWideMemoField
      FieldName = 'publishers'
      Origin = 'publishers'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBalternates: TWideMemoField
      FieldName = 'alternates'
      Origin = 'alternates'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBrom: TWideMemoField
      FieldName = 'rom'
      Origin = 'rom'
      BlobType = ftWideMemo
    end
  end
  object tArcadeTGDBImages: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'id'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'arcade_tgdb_images'
    Left = 672
    Top = 208
    object tArcadeTGDBImagesid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object tArcadeTGDBImagesimg_id: TIntegerField
      FieldName = 'img_id'
    end
    object tArcadeTGDBImagesrom: TWideMemoField
      FieldName = 'rom'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBImagesimg_type: TWideMemoField
      FieldName = 'img_type'
      Required = True
      BlobType = ftWideMemo
    end
    object tArcadeTGDBImagesside: TWideMemoField
      FieldName = 'side'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBImagesfilename: TWideMemoField
      FieldName = 'filename'
      Required = True
      BlobType = ftWideMemo
    end
    object tArcadeTGDBImagesresolution: TWideMemoField
      FieldName = 'resolution'
      BlobType = ftWideMemo
    end
    object tArcadeTGDBImagespath: TWideMemoField
      FieldName = 'path'
      Required = True
      BlobType = ftWideMemo
    end
  end
  object tTGDBDevelopers: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'tgdb_developers'
    Left = 376
    Top = 16
    object tTGDBDevelopersnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tTGDBDevelopersid: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      Required = True
    end
    object tTGDBDevelopersname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      BlobType = ftWideMemo
    end
  end
  object tTGDBGenres: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'tgdb_genres'
    Left = 376
    Top = 80
    object tTGDBGenresnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tTGDBGenresid: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      Required = True
    end
    object tTGDBGenresname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      BlobType = ftWideMemo
    end
  end
  object tTGDBPlatforms: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'tgdb_platforms'
    Left = 376
    Top = 208
    object tTGDBPlatformsnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tTGDBPlatformsid: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      Required = True
    end
    object tTGDBPlatformsname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsalias: TWideMemoField
      FieldName = 'alias'
      Origin = 'alias'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsicon: TBlobField
      FieldName = 'icon'
      Origin = 'icon'
    end
    object tTGDBPlatformsconsole: TWideMemoField
      FieldName = 'console'
      Origin = 'console'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformscontroller: TWideMemoField
      FieldName = 'controller'
      Origin = 'controller'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsdeveloper: TWideMemoField
      FieldName = 'developer'
      Origin = 'developer'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsmanufactor: TWideMemoField
      FieldName = 'manufactor'
      Origin = 'manufactor'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsmedia: TWideMemoField
      FieldName = 'media'
      Origin = 'media'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformscpu: TWideMemoField
      FieldName = 'cpu'
      Origin = 'cpu'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsmemory: TWideMemoField
      FieldName = 'memory'
      Origin = 'memory'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsgraphics: TWideMemoField
      FieldName = 'graphics'
      Origin = 'graphics'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformssound: TWideMemoField
      FieldName = 'sound'
      Origin = 'sound'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsmax_controllers: TWideMemoField
      FieldName = 'max_controllers'
      Origin = 'max_controllers'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsdisplay: TWideMemoField
      FieldName = 'display'
      Origin = 'display'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsoverview: TWideMemoField
      FieldName = 'overview'
      Origin = 'overview'
      BlobType = ftWideMemo
    end
    object tTGDBPlatformsyoutube: TWideMemoField
      FieldName = 'youtube'
      Origin = 'youtube'
      BlobType = ftWideMemo
    end
  end
  object tTGDBPublishers: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'tgdb_publishers'
    Left = 376
    Top = 144
    object tTGDBPublishersnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tTGDBPublishersid: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      Required = True
    end
    object tTGDBPublishersname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      BlobType = ftWideMemo
    end
  end
  object tConfigEmus: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'emulators_config_paths'
    Left = 104
    Top = 80
    object tConfigEmusnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tConfigEmusemulator: TWideMemoField
      FieldName = 'emulator'
      Origin = 'emulator'
      BlobType = ftWideMemo
    end
    object tConfigEmusbox_art: TWideMemoField
      FieldName = 'box_art'
      Origin = 'box_art'
      BlobType = ftWideMemo
    end
    object tConfigEmussnapshot: TWideMemoField
      FieldName = 'snapshot'
      Origin = 'snapshot'
      BlobType = ftWideMemo
    end
    object tConfigEmusvideo: TWideMemoField
      FieldName = 'video'
      Origin = 'video'
      BlobType = ftWideMemo
    end
    object tConfigEmusmanual: TWideMemoField
      FieldName = 'manual'
      Origin = 'manual'
      BlobType = ftWideMemo
    end
    object tConfigEmusbezels: TWideMemoField
      FieldName = 'bezels'
      Origin = 'bezels'
      BlobType = ftWideMemo
    end
  end
  object tGamepad: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'gpd_map'
    Left = 32
    Top = 584
    object tGamepadnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tGamepadgname: TWideMemoField
      FieldName = 'gname'
      Origin = 'gname'
      BlobType = ftWideMemo
    end
    object tGamepadname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      BlobType = ftWideMemo
    end
    object tGamepadgtype: TWideMemoField
      FieldName = 'gtype'
      Origin = 'gtype'
      BlobType = ftWideMemo
    end
    object tGamepadserial: TWideMemoField
      FieldName = 'serial'
      Origin = 'serial'
      BlobType = ftWideMemo
    end
    object tGamepadbuttons: TWideMemoField
      FieldName = 'buttons'
      Origin = 'buttons'
      BlobType = ftWideMemo
    end
    object tGamepadguid: TWideMemoField
      FieldName = 'guid'
      Origin = 'guid'
      BlobType = ftWideMemo
    end
    object tGamepadvendor: TWideMemoField
      FieldName = 'vendor'
      Origin = 'vendor'
      BlobType = ftWideMemo
    end
    object tGamepadproduct: TWideMemoField
      FieldName = 'product'
      Origin = 'product'
      BlobType = ftWideMemo
    end
    object tGamepadproduct_version: TWideMemoField
      FieldName = 'product_version'
      Origin = 'product_version'
      BlobType = ftWideMemo
    end
    object tGamepadhat_up: TIntegerField
      FieldName = 'hat_up'
      Origin = 'hat_up'
    end
    object tGamepadhat_down: TIntegerField
      FieldName = 'hat_down'
      Origin = 'hat_down'
    end
    object tGamepadhat_left: TIntegerField
      FieldName = 'hat_left'
      Origin = 'hat_left'
    end
    object tGamepadhat_right: TIntegerField
      FieldName = 'hat_right'
      Origin = 'hat_right'
    end
    object tGamepadleft_x_plus: TIntegerField
      FieldName = 'left_x_plus'
      Origin = 'left_x_plus'
    end
    object tGamepadleft_x_minus: TIntegerField
      FieldName = 'left_x_minus'
      Origin = 'left_x_minus'
    end
    object tGamepadleft_y_plus: TIntegerField
      FieldName = 'left_y_plus'
      Origin = 'left_y_plus'
    end
    object tGamepadleft_y_minus: TIntegerField
      FieldName = 'left_y_minus'
      Origin = 'left_y_minus'
    end
    object tGamepadright_x_plus: TIntegerField
      FieldName = 'right_x_plus'
      Origin = 'right_x_plus'
    end
    object tGamepadright_x_minus: TIntegerField
      FieldName = 'right_x_minus'
      Origin = 'right_x_minus'
    end
    object tGamepadright_y_plus: TIntegerField
      FieldName = 'right_y_plus'
      Origin = 'right_y_plus'
    end
    object tGamepadright_y_minus: TIntegerField
      FieldName = 'right_y_minus'
      Origin = 'right_y_minus'
    end
    object tGamepadleft_deadzone: TIntegerField
      FieldName = 'left_deadzone'
      Origin = 'left_deadzone'
    end
    object tGamepadright_deadzone: TIntegerField
      FieldName = 'right_deadzone'
      Origin = 'right_deadzone'
    end
    object tGamepadb0: TIntegerField
      FieldName = 'b0'
      Origin = 'b0'
    end
    object tGamepadb1: TIntegerField
      FieldName = 'b1'
      Origin = 'b1'
    end
    object tGamepadb2: TIntegerField
      FieldName = 'b2'
      Origin = 'b2'
    end
    object tGamepadb3: TIntegerField
      FieldName = 'b3'
      Origin = 'b3'
    end
    object tGamepadb4: TIntegerField
      FieldName = 'b4'
      Origin = 'b4'
    end
    object tGamepadb5: TIntegerField
      FieldName = 'b5'
      Origin = 'b5'
    end
    object tGamepadb6: TIntegerField
      FieldName = 'b6'
      Origin = 'b6'
    end
    object tGamepadb7: TIntegerField
      FieldName = 'b7'
      Origin = 'b7'
    end
    object tGamepadb8: TIntegerField
      FieldName = 'b8'
      Origin = 'b8'
    end
    object tGamepadb9: TIntegerField
      FieldName = 'b9'
      Origin = 'b9'
    end
    object tGamepadb10: TIntegerField
      FieldName = 'b10'
      Origin = 'b10'
    end
    object tGamepadb11: TIntegerField
      FieldName = 'b11'
      Origin = 'b11'
    end
    object tGamepadb12: TIntegerField
      FieldName = 'b12'
      Origin = 'b12'
    end
    object tGamepadb13: TIntegerField
      FieldName = 'b13'
      Origin = 'b13'
    end
    object tGamepadb14: TIntegerField
      FieldName = 'b14'
      Origin = 'b14'
    end
    object tGamepadb15: TIntegerField
      FieldName = 'b15'
      Origin = 'b15'
    end
    object tGamepadcoin: TIntegerField
      FieldName = 'coin'
      Origin = 'coin'
    end
    object tGamepadstart: TIntegerField
      FieldName = 'start'
      Origin = 'start'
    end
    object tGamepadtrigger_left: TIntegerField
      FieldName = 'trigger_left'
      Origin = 'trigger_left'
    end
    object tGamepadtrigger_right: TIntegerField
      FieldName = 'trigger_right'
      Origin = 'trigger_right'
    end
    object tGamepadleft: TIntegerField
      FieldName = 'left'
      Origin = '"left"'
    end
    object tGamepadright: TIntegerField
      FieldName = 'right'
      Origin = '"right"'
    end
  end
  object tJoystick: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'joy_map'
    Left = 32
    Top = 648
    object tJoysticknum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tJoystickjname: TIntegerField
      FieldName = 'jname'
      Origin = 'jname'
    end
    object tJoystickname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      BlobType = ftWideMemo
    end
    object tJoystickplayer: TIntegerField
      FieldName = 'player'
      Origin = 'player'
    end
    object tJoystickjtype: TWideMemoField
      FieldName = 'jtype'
      Origin = 'jtype'
      BlobType = ftWideMemo
    end
    object tJoystickbuttons: TIntegerField
      FieldName = 'buttons'
      Origin = 'buttons'
    end
    object tJoystickguid: TWideMemoField
      FieldName = 'guid'
      Origin = 'guid'
      BlobType = ftWideMemo
    end
    object tJoystickvendor: TWideMemoField
      FieldName = 'vendor'
      Origin = 'vendor'
      BlobType = ftWideMemo
    end
    object tJoystickproduct: TWideMemoField
      FieldName = 'product'
      Origin = 'product'
      BlobType = ftWideMemo
    end
    object tJoystickproduct_version: TWideMemoField
      FieldName = 'product_version'
      Origin = 'product_version'
      BlobType = ftWideMemo
    end
    object tJoystickserial: TWideMemoField
      FieldName = 'serial'
      Origin = 'serial'
      BlobType = ftWideMemo
    end
    object tJoystickaxis_x_plus: TIntegerField
      FieldName = 'axis_x_plus'
      Origin = 'axis_x_plus'
    end
    object tJoystickaxis_x_minus: TIntegerField
      FieldName = 'axis_x_minus'
      Origin = 'axis_x_minus'
    end
    object tJoystickaxis_y_plus: TIntegerField
      FieldName = 'axis_y_plus'
      Origin = 'axis_y_plus'
    end
    object tJoystickaxis_y_minus: TIntegerField
      FieldName = 'axis_y_minus'
      Origin = 'axis_y_minus'
    end
    object tJoystickdeadzone_x: TIntegerField
      FieldName = 'deadzone_x'
      Origin = 'deadzone_x'
    end
    object tJoystickdeadzone_y: TIntegerField
      FieldName = 'deadzone_y'
      Origin = 'deadzone_y'
    end
    object tJoystickbutton0: TIntegerField
      FieldName = 'button0'
      Origin = 'button0'
    end
    object tJoystickbutton1: TIntegerField
      FieldName = 'button1'
      Origin = 'button1'
    end
    object tJoystickbutton2: TIntegerField
      FieldName = 'button2'
      Origin = 'button2'
    end
    object tJoystickbutton3: TIntegerField
      FieldName = 'button3'
      Origin = 'button3'
    end
    object tJoystickbutton4: TIntegerField
      FieldName = 'button4'
      Origin = 'button4'
    end
    object tJoystickbutton5: TIntegerField
      FieldName = 'button5'
      Origin = 'button5'
    end
    object tJoystickbutton6: TIntegerField
      FieldName = 'button6'
      Origin = 'button6'
    end
    object tJoystickbutton7: TIntegerField
      FieldName = 'button7'
      Origin = 'button7'
    end
    object tJoystickbutton8: TIntegerField
      FieldName = 'button8'
      Origin = 'button8'
    end
    object tJoystickbutton9: TIntegerField
      FieldName = 'button9'
      Origin = 'button9'
    end
    object tJoystickbutton10: TIntegerField
      FieldName = 'button10'
      Origin = 'button10'
    end
    object tJoystickbutton11: TIntegerField
      FieldName = 'button11'
      Origin = 'button11'
    end
    object tJoystickbutton12: TIntegerField
      FieldName = 'button12'
      Origin = 'button12'
    end
    object tJoystickbutton13: TIntegerField
      FieldName = 'button13'
      Origin = 'button13'
    end
    object tJoystickbutton14: TIntegerField
      FieldName = 'button14'
      Origin = 'button14'
    end
    object tJoystickbutton15: TIntegerField
      FieldName = 'button15'
      Origin = 'button15'
    end
    object tJoystickstart: TIntegerField
      FieldName = 'start'
      Origin = 'start'
    end
    object tJoystickcoin: TIntegerField
      FieldName = 'coin'
      Origin = 'coin'
    end
  end
  object tKeyboard: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'key_map'
    Left = 32
    Top = 520
    object tKeyboardnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tKeyboardemulator: TWideMemoField
      FieldName = 'emulator'
      BlobType = ftWideMemo
    end
    object tKeyboardplayer: TIntegerField
      FieldName = 'player'
      Origin = 'player'
    end
    object tKeyboardkey_up: TIntegerField
      FieldName = 'key_up'
      Origin = 'key_up'
    end
    object tKeyboardkey_down: TIntegerField
      FieldName = 'key_down'
      Origin = 'key_down'
    end
    object tKeyboardkey_left: TIntegerField
      FieldName = 'key_left'
      Origin = 'key_left'
    end
    object tKeyboardkey_right: TIntegerField
      FieldName = 'key_right'
      Origin = 'key_right'
    end
    object tKeyboardkey_b0: TIntegerField
      FieldName = 'key_b0'
      Origin = 'key_b0'
    end
    object tKeyboardkey_b1: TIntegerField
      FieldName = 'key_b1'
      Origin = 'key_b1'
    end
    object tKeyboardkey_b2: TIntegerField
      FieldName = 'key_b2'
      Origin = 'key_b2'
    end
    object tKeyboardkey_b3: TIntegerField
      FieldName = 'key_b3'
      Origin = 'key_b3'
    end
    object tKeyboardkey_b4: TIntegerField
      FieldName = 'key_b4'
      Origin = 'key_b4'
    end
    object tKeyboardkey_b5: TIntegerField
      FieldName = 'key_b5'
      Origin = 'key_b5'
    end
    object tKeyboardkey_coin: TIntegerField
      FieldName = 'key_coin'
      Origin = 'key_coin'
    end
    object tKeyboardkey_start: TIntegerField
      FieldName = 'key_start'
      Origin = 'key_start'
    end
  end
  object tKeyboardFrontend: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'key_map_frontend'
    Left = 120
    Top = 520
    object tKeyboardFrontendnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tKeyboardFrontendname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      Required = True
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendquit_dspfm: TWideMemoField
      FieldName = 'quit_dspfm'
      Origin = 'quit_dspfm'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendplay: TWideMemoField
      FieldName = 'play'
      Origin = 'play'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendmove_up: TWideMemoField
      FieldName = 'move_up'
      Origin = 'move_up'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendmove_down: TWideMemoField
      FieldName = 'move_down'
      Origin = 'move_down'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendmove_left: TWideMemoField
      FieldName = 'move_left'
      Origin = 'move_left'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendmove_right: TWideMemoField
      FieldName = 'move_right'
      Origin = 'move_right'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendshow_configuration: TWideMemoField
      FieldName = 'show_configuration'
      Origin = 'show_configuration'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendshow_display: TWideMemoField
      FieldName = 'show_display'
      Origin = 'show_display'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendshow_controls: TWideMemoField
      FieldName = 'show_controls'
      Origin = 'show_controls'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendshow_information: TWideMemoField
      FieldName = 'show_information'
      Origin = 'show_information'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendshow_hide_time_game: TWideMemoField
      FieldName = 'show_hide_time_game'
      Origin = 'show_hide_time_game'
      BlobType = ftWideMemo
    end
    object tKeyboardFrontendplatform_emulators: TWideMemoField
      FieldName = 'platform_emulators'
      Origin = 'platform_emulators'
      BlobType = ftWideMemo
    end
  end
  object tKeyboardInGame: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'key_map_ingame'
    Left = 224
    Top = 520
    object tKeyboardInGamenum: TFDAutoIncField
      FieldName = 'num'
      ReadOnly = False
    end
    object tKeyboardInGamename: TWideMemoField
      FieldName = 'name'
      BlobType = ftWideMemo
    end
    object tKeyboardInGameleave_game: TIntegerField
      FieldName = 'leave_game'
    end
    object tKeyboardInGamepause_game: TIntegerField
      FieldName = 'pause_game'
    end
    object tKeyboardInGamefullscreen_game: TIntegerField
      FieldName = 'fullscreen_game'
    end
    object tKeyboardInGameservice: TIntegerField
      FieldName = 'service'
    end
    object tKeyboardInGamefastest: TIntegerField
      FieldName = 'fastest'
    end
    object tKeyboardInGameslow: TIntegerField
      FieldName = 'slow'
    end
    object tKeyboardInGamereset: TIntegerField
      FieldName = 'reset'
    end
    object tKeyboardInGamesave_snap_player_1: TIntegerField
      FieldName = 'save_snap_player_1'
    end
    object tKeyboardInGamesave_snap_player_2: TIntegerField
      FieldName = 'save_snap_player_2'
    end
    object tKeyboardInGameload_snap_player_1: TIntegerField
      FieldName = 'load_snap_player_1'
    end
    object tKeyboardInGameload_snap_player_2: TIntegerField
      FieldName = 'load_snap_player_2'
    end
    object tKeyboardInGamesnapshot: TIntegerField
      FieldName = 'snapshot'
    end
    object tKeyboardInGameshow_info: TIntegerField
      FieldName = 'show_info'
    end
  end
  object tLanguage: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'lang'
    Left = 192
    Top = 16
    object tLanguagenum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tLanguageellinika: TWideMemoField
      FieldName = 'ellinika'
      Origin = 'ellinika'
      BlobType = ftWideMemo
    end
    object tLanguagespain: TWideMemoField
      FieldName = 'spain'
      Origin = 'spain'
      BlobType = ftWideMemo
    end
    object tLanguagerussian: TWideMemoField
      FieldName = 'russian'
      Origin = 'russian'
      BlobType = ftWideMemo
    end
    object tLanguageenglish: TWideMemoField
      FieldName = 'english'
      Origin = 'english'
      BlobType = ftWideMemo
    end
    object tLanguagefrancais: TWideMemoField
      FieldName = 'francais'
      Origin = 'francais'
      BlobType = ftWideMemo
    end
    object tLanguagegerman: TWideMemoField
      FieldName = 'german'
      Origin = 'german'
      BlobType = ftWideMemo
    end
    object tLanguagebrazil: TWideMemoField
      FieldName = 'brazil'
      Origin = 'brazil'
      BlobType = ftWideMemo
    end
    object tLanguageitalian: TWideMemoField
      FieldName = 'italian'
      Origin = 'italian'
      BlobType = ftWideMemo
    end
  end
  object tLanguagePop: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'lang_pop'
    Left = 192
    Top = 80
    object tLanguagePopnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tLanguagePopellinika: TWideMemoField
      FieldName = 'ellinika'
      Origin = 'ellinika'
      BlobType = ftWideMemo
    end
    object tLanguagePopcatallan: TWideMemoField
      FieldName = 'catallan'
      Origin = 'catallan'
      BlobType = ftWideMemo
    end
    object tLanguagePopcastellano: TWideMemoField
      FieldName = 'castellano'
      Origin = 'castellano'
      BlobType = ftWideMemo
    end
    object tLanguagePopenglish: TWideMemoField
      FieldName = 'english'
      Origin = 'english'
      BlobType = ftWideMemo
    end
    object tLanguagePopfrancais: TWideMemoField
      FieldName = 'francais'
      Origin = 'francais'
      BlobType = ftWideMemo
    end
    object tLanguagePopgerman: TWideMemoField
      FieldName = 'german'
      Origin = 'german'
      BlobType = ftWideMemo
    end
    object tLanguagePopbrazil: TWideMemoField
      FieldName = 'brazil'
      Origin = 'brazil'
      BlobType = ftWideMemo
    end
    object tLanguagePopitalian: TWideMemoField
      FieldName = 'italian'
      Origin = 'italian'
      BlobType = ftWideMemo
    end
  end
  object tNes: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'nes'
    Left = 1000
    Top = 16
    object tNesnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tNesrom: TWideMemoField
      FieldName = 'rom'
      Origin = 'rom'
      Required = True
      BlobType = ftWideMemo
    end
    object tNesrom_path: TWideMemoField
      FieldName = 'rom_path'
      Origin = 'rom_path'
      BlobType = ftWideMemo
    end
    object tNesstate: TWideMemoField
      FieldName = 'state'
      Origin = 'state'
      BlobType = ftWideMemo
    end
    object tNesstate_icon: TIntegerField
      FieldName = 'state_icon'
      Origin = 'state_icon'
    end
    object tNesstate_desc: TWideMemoField
      FieldName = 'state_desc'
      Origin = 'state_desc'
      BlobType = ftWideMemo
    end
    object tNestotal_time: TIntegerField
      FieldName = 'total_time'
      Origin = 'total_time'
    end
  end
  object tNes_Config: TFDTable
    Filtered = True
    CachedUpdates = True
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    Left = 1000
    Top = 80
    object IntegerField5: TIntegerField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IntegerField6: TIntegerField
      FieldName = 'fullscreen'
      Origin = 'fullscreen'
    end
    object IntegerField7: TIntegerField
      FieldName = 'full_x'
      Origin = 'full_x'
    end
    object IntegerField8: TIntegerField
      FieldName = 'full_y'
      Origin = 'full_y'
    end
    object IntegerField9: TIntegerField
      FieldName = 'bezels'
      Origin = 'bezels'
    end
    object IntegerField10: TIntegerField
      FieldName = 'win_center'
      Origin = 'win_center'
    end
    object IntegerField11: TIntegerField
      FieldName = 'win_size'
      Origin = 'win_size'
    end
    object IntegerField12: TIntegerField
      FieldName = 'sound'
      Origin = 'sound'
    end
  end
  object tNesMedia: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'nes_media'
    Left = 1000
    Top = 144
    object tNesMedianum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tNesMediarom: TWideMemoField
      FieldName = 'rom'
      Origin = 'rom'
      Required = True
      BlobType = ftWideMemo
    end
    object tNesMediabox_art: TWideMemoField
      FieldName = 'box_art'
      Origin = 'box_art'
      BlobType = ftWideMemo
    end
    object tNesMediasnapshots: TWideMemoField
      FieldName = 'snapshots'
      Origin = 'snapshots'
      BlobType = ftWideMemo
    end
    object tNesMediamanuals: TWideMemoField
      FieldName = 'manuals'
      Origin = 'manuals'
      BlobType = ftWideMemo
    end
    object tNesMediavideos: TWideMemoField
      FieldName = 'videos'
      Origin = 'videos'
      BlobType = ftWideMemo
    end
  end
  object tNesTGDB: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'nes_tgdb'
    Left = 1000
    Top = 208
    object tNesTGDBnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tNesTGDBid: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      Required = True
    end
    object tNesTGDBtitle: TWideMemoField
      FieldName = 'title'
      Origin = 'title'
      BlobType = ftWideMemo
    end
    object tNesTGDBrelease_date: TWideMemoField
      FieldName = 'release_date'
      Origin = 'release_date'
      BlobType = ftWideMemo
    end
    object tNesTGDBplatform_id: TWideMemoField
      FieldName = 'platform_id'
      Origin = 'platform_id'
      BlobType = ftWideMemo
    end
    object tNesTGDBplayers: TWideMemoField
      FieldName = 'players'
      Origin = 'players'
      BlobType = ftWideMemo
    end
    object tNesTGDBoverview: TWideMemoField
      FieldName = 'overview'
      Origin = 'overview'
      BlobType = ftWideMemo
    end
    object tNesTGDBlast_updated: TWideMemoField
      FieldName = 'last_updated'
      Origin = 'last_updated'
      BlobType = ftWideMemo
    end
    object tNesTGDBrating: TWideMemoField
      FieldName = 'rating'
      Origin = 'rating'
      BlobType = ftWideMemo
    end
    object tNesTGDBcoop: TWideMemoField
      FieldName = 'coop'
      Origin = 'coop'
      BlobType = ftWideMemo
    end
    object tNesTGDByoutube: TWideMemoField
      FieldName = 'youtube'
      Origin = 'youtube'
      BlobType = ftWideMemo
    end
    object tNesTGDBos: TWideMemoField
      FieldName = 'os'
      Origin = 'os'
      BlobType = ftWideMemo
    end
    object tNesTGDBprocessor: TWideMemoField
      FieldName = 'processor'
      Origin = 'processor'
      BlobType = ftWideMemo
    end
    object tNesTGDBram: TWideMemoField
      FieldName = 'ram'
      Origin = 'ram'
      BlobType = ftWideMemo
    end
    object tNesTGDBhdd: TWideMemoField
      FieldName = 'hdd'
      Origin = 'hdd'
      BlobType = ftWideMemo
    end
    object tNesTGDBvideo: TWideMemoField
      FieldName = 'video'
      Origin = 'video'
      BlobType = ftWideMemo
    end
    object tNesTGDBsound: TWideMemoField
      FieldName = 'sound'
      Origin = 'sound'
      BlobType = ftWideMemo
    end
    object tNesTGDBdevelopers: TWideMemoField
      FieldName = 'developers'
      Origin = 'developers'
      BlobType = ftWideMemo
    end
    object tNesTGDBgenres: TWideMemoField
      FieldName = 'genres'
      Origin = 'genres'
      BlobType = ftWideMemo
    end
    object tNesTGDBpublishers: TWideMemoField
      FieldName = 'publishers'
      Origin = 'publishers'
      BlobType = ftWideMemo
    end
    object tNesTGDBalternates: TWideMemoField
      FieldName = 'alternates'
      Origin = 'alternates'
      BlobType = ftWideMemo
    end
    object tNesTGDBbox_art_original: TWideMemoField
      FieldName = 'box_art_original'
      Origin = 'box_art_original'
      BlobType = ftWideMemo
    end
    object tNesTGDBbox_art_small: TWideMemoField
      FieldName = 'box_art_small'
      Origin = 'box_art_small'
      BlobType = ftWideMemo
    end
    object tNesTGDBbox_art_thumb: TWideMemoField
      FieldName = 'box_art_thumb'
      Origin = 'box_art_thumb'
      BlobType = ftWideMemo
    end
    object tNesTGDBbox_art_cropped: TWideMemoField
      FieldName = 'box_art_cropped'
      Origin = 'box_art_cropped'
      BlobType = ftWideMemo
    end
    object tNesTGDBbox_art_medium: TWideMemoField
      FieldName = 'box_art_medium'
      Origin = 'box_art_medium'
      BlobType = ftWideMemo
    end
    object tNesTGDBbox_art_large: TWideMemoField
      FieldName = 'box_art_large'
      Origin = 'box_art_large'
      BlobType = ftWideMemo
    end
    object tNesTGDBpic_1: TWideMemoField
      FieldName = 'pic_1'
      Origin = 'pic_1'
      BlobType = ftWideMemo
    end
    object tNesTGDBpic_1_ext: TWideMemoField
      FieldName = 'pic_1_ext'
      Origin = 'pic_1_ext'
      BlobType = ftWideMemo
    end
    object tNesTGDBrom: TWideMemoField
      FieldName = 'rom'
      Origin = 'rom'
      BlobType = ftWideMemo
    end
  end
  object tNesTGDBImages: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'id'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    Left = 1000
    Top = 272
    object IntegerField14: TIntegerField
      FieldName = 'id'
      Origin = 'id'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IntegerField15: TIntegerField
      FieldName = 'game_id'
      Origin = 'game_id'
    end
    object WideMemoField35: TWideMemoField
      FieldName = 'rom'
      Origin = 'rom'
      BlobType = ftWideMemo
    end
    object WideMemoField36: TWideMemoField
      FieldName = 'img_type'
      Origin = 'img_type'
      Required = True
      BlobType = ftWideMemo
    end
    object WideMemoField37: TWideMemoField
      FieldName = 'side'
      Origin = 'side'
      BlobType = ftWideMemo
    end
    object WideMemoField38: TWideMemoField
      FieldName = 'filename'
      Origin = 'filename'
      Required = True
      BlobType = ftWideMemo
    end
    object WideMemoField39: TWideMemoField
      FieldName = 'resolution'
      Origin = 'resolution'
      BlobType = ftWideMemo
    end
  end
  object tPlayers: TFDTable
    CachedUpdates = True
    IndexFieldNames = 'num'
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'players'
    Left = 328
    Top = 520
    object tPlayersnum: TFDAutoIncField
      FieldName = 'num'
      Origin = 'num'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
    end
    object tPlayersname: TWideMemoField
      FieldName = 'name'
      Origin = 'name'
      Required = True
      BlobType = ftWideMemo
    end
    object tPlayersplayer: TIntegerField
      FieldName = 'player'
      Origin = 'player'
      Required = True
    end
    object tPlayerskey: TIntegerField
      FieldName = 'key'
      Origin = '"key"'
    end
    object tPlayersjoy: TIntegerField
      FieldName = 'joy'
      Origin = 'joy'
    end
    object tPlayersgpd: TIntegerField
      FieldName = 'gpd'
      Origin = 'gpd'
    end
    object tPlayersplatform: TWideMemoField
      FieldName = 'platform'
      Origin = 'platform'
      Required = True
      BlobType = ftWideMemo
    end
    object tPlayerskey_map_num: TIntegerField
      FieldName = 'key_map_num'
      Origin = 'key_map_num'
    end
    object tPlayersselected: TIntegerField
      FieldName = 'selected'
      Origin = 'selected'
    end
    object tPlayersactive: TIntegerField
      FieldName = 'active'
      Origin = 'active'
    end
  end
  object bsDBArcade: TBindSourceDB
    DataSet = tArcade
    ScopeMappings = <>
    Left = 804
    Top = 16
  end
  object bsDBArcadeConfig: TBindSourceDB
    DataSet = tArcadeConfig
    ScopeMappings = <>
    Left = 808
    Top = 80
  end
  object bsDBArcadeTGDB: TBindSourceDB
    DataSet = tArcadeTGDB
    ScopeMappings = <>
    Left = 808
    Top = 144
  end
  object bsDBArcadeTGDBImages: TBindSourceDB
    DataSet = tArcadeTGDBImages
    ScopeMappings = <>
    Left = 808
    Top = 208
  end
  object bsDBTGDBDevelopers: TBindSourceDB
    DataSet = tTGDBDevelopers
    ScopeMappings = <>
    Left = 496
    Top = 16
  end
  object bsDBTGDBGenres: TBindSourceDB
    DataSet = tTGDBGenres
    ScopeMappings = <>
    Left = 496
    Top = 80
  end
  object bsDBTGDBPublishers: TBindSourceDB
    DataSet = tTGDBPublishers
    ScopeMappings = <>
    Left = 496
    Top = 144
  end
  object ingame: TFDTable
    Connection = fdconn
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'ingame'
    Left = 32
    Top = 248
    object ingameshowFPS: TIntegerField
      FieldName = 'showFPS'
      Origin = 'showFPS'
    end
  end
end
