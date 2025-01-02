unit f_arcade;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.TabControl,
  FMX.Objects,
  FMX.Layouts,
  FMX.Filter.Effects,
  FMX.Effects,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.ListBox,
  FMX.ComboEdit,
  FireDAC.Stan.Param,
  Winapi.Windows,
  vars_consts;

type
  TSaveDialogTypes = (sd_export_markup, sd_export_html, sd_export_json, sd_export_xml);

type
  Tarcade = class(TFrame)
    tc_arcade: TTabControl;
    ti_arcade_general: TTabItem;
    ti_arcade_dirs: TTabItem;
    verts_arcade_dirs: TVertScrollBox;
    edt_arcade_dirs_const_roms: TEdit;
    txt_arcade_dirs_const_roms: TText;
    spb_arcade_dirs_const_roms: TSpeedButton;
    img_arcade_dirs_const_roms: TImage;
    rect_arcade_dirs: TRectangle;
    od_arcade_config: TOpenDialog;
    pb_arcade_dirs: TProgressBar;
    txt_arcade_dirs: TText;
    spb_arcade_dirs_const_roms_refresh: TSpeedButton;
    img_arcade_dirs_const_roms_refresh: TImage;
    txt_arcade_dirs_const_hiscore: TText;
    edt_arcade_dirs_const_hiscore: TEdit;
    spb_arcade_dirs_const_hiscore: TSpeedButton;
    img_arcade_dirs_const_hiscore: TImage;
    txt_arcade_dirs_const_nvram: TText;
    edt_arcade_dirs_const_nvram: TEdit;
    spb_arcade_dirs_const_nvram: TSpeedButton;
    img_arcade_dirs_const_nvram: TImage;
    txt_arcade_dirs_const_samples: TText;
    edt_arcade_dirs_const_samples: TEdit;
    spb_arcade_dirs_const_samples: TSpeedButton;
    img_arcade_dirs_const_samples: TImage;
    ti_arcade_media_dirs: TTabItem;
    vsb_arcade_dirs: TVertScrollBox;
    ti_arcade_graphics: TTabItem;
    cb_arcade_graphics_driver: TComboBox;
    grb_arcade_graphics_windowed: TGroupBox;
    grb_arcade_graphics_fullscreen: TGroupBox;
    lbl_arcade_graphics_driver: TLabel;
    lbl_arcade_graphics_driver_selected: TLabel;
    chb_arcade_graphics_center_window: TCheckBox;
    grb_arcade_graphics_window_size: TGroupBox;
    rb_arcade_graphics_window_original: TRadioButton;
    rb_arcade_graphics_window_2x: TRadioButton;
    rb_arcade_graphics_window_3x: TRadioButton;
    edt_arcade_graphics_full_width: TEdit;
    edt_arcade_graphics_full_height: TEdit;
    lbl_arcade_graphics_full_height: TLabel;
    lbl_arcade_graphics_full_width: TLabel;
    chb_arcade_graphics_bezels: TCheckBox;
    rb_arcade_graphics_windowed: TRadioButton;
    rb_arcade_graphics_fullscreen: TRadioButton;
    grb_arcade_graphics_window_state: TGroupBox;
    ti_arcade_sound: TTabItem;
    chb_arcade_sound_enable: TCheckBox;
    grb_arcade_sound_options: TGroupBox;
    grb_arcade_general_export: TGroupBox;
    lbl_arcade_general_export: TLabel;
    cb_arcade_general_export: TComboBox;
    btn_arcade_general_export: TButton;
    sd_arcade_config: TSaveDialog;
    prbar_arcade_general_export: TProgressBar;
    grbArcadeGeneralFrontend: TGroupBox;
    lbArcadeGeneralFrontendType: TListBox;
    lbArcadeGeneralFrontendView: TListBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    S: TTabItem;
    edt_arcade_dirs_media_bezels: TEdit;
    edt_arcade_dirs_media_images: TEdit;
    edt_arcade_dirs_media_manuals: TEdit;
    edt_arcade_dirs_media_video: TEdit;
    spb_arcade_dirs_media_bezels: TSpeedButton;
    img_arcade_dirs_media_bezels: TImage;
    spb_arcade_dirs_media_manuals: TSpeedButton;
    img_arcade_dirs_media_manuals: TImage;
    spb_arcade_dirs_media_snapshot: TSpeedButton;
    img_arcade_dirs_media_snapshot: TImage;
    spb_arcade_dirs_media_video: TSpeedButton;
    img_arcade_dirs_media_video: TImage;
    txt_arcade_dirs_media_bezels: TText;
    txt_arcade_dirs_media_images: TText;
    txt_arcade_dirs_media_manuals: TText;
    txt_arcade_dirs_media_video: TText;
    procedure chb_arcade_graphics_center_windowClick(Sender: TObject);
    procedure OnShow;
    procedure OnClose;
    procedure rb_arcade_graphics_fullscreenClick(Sender: TObject);
    procedure rb_arcade_graphics_windowedClick(Sender: TObject);
    procedure rb_arcade_graphics_window_2xClick(Sender: TObject);
    procedure rb_arcade_graphics_window_3xClick(Sender: TObject);
    procedure rb_arcade_graphics_window_originalClick(Sender: TObject);
    procedure spb_arcade_dirs_const_hiscoreClick(Sender: TObject);
    procedure spb_arcade_dirs_const_nvramClick(Sender: TObject);
    procedure spb_arcade_dirs_const_romsClick(Sender: TObject);
    procedure spb_arcade_dirs_const_roms_refreshClick(Sender: TObject);
    procedure spb_arcade_dirs_const_samplesClick(Sender: TObject);
    procedure spb_arcade_dirs_media_manualsClick(Sender: TObject);
    procedure spb_arcade_dirs_media_snapshotClick(Sender: TObject);
    procedure spb_arcade_dirs_media_videoClick(Sender: TObject);
    procedure set_win_size(num: integer);
    procedure chb_arcade_sound_enableClick(Sender: TObject);
    procedure btn_arcade_general_exportClick(Sender: TObject);
    procedure sd_arcade_configCanClose(Sender: TObject; var CanClose: Boolean);
    procedure lbArcadeGeneralFrontendViewChange(Sender: TObject);

  private
    { Private declarations }
    beforeView, beforeImgType: string;
    save_dialog_type: TSaveDialogTypes;

    function scan_dir_roms(dir: string; refresh: Boolean): integer;
    procedure fields_active(enable: Boolean);
    procedure reload_main;

  public
    { Public declarations }
    arcadeAnyChanges: boolean;
  end;

implementation

{$R *.fmx}

uses
  uarcade_actions,
  umain_config,
  uTheGamesDatabase,
  prj_functions,
  sound_engine,
  files_export,
  uDataModule,
  language,
  capsdefs, ulang;

procedure Tarcade.btn_arcade_general_exportClick(Sender: TObject);
begin
  case cb_arcade_general_export.ItemIndex of
    0:
      begin
        if SelectDirectory('Markup Git single file directory...', '', vPath) then
          CreateAndSave_Markup(vPath, false);
      end;
    1:
      begin
        if SelectDirectory('Html files directory...', '', vPath) then
          CreateAndSave_Html(vPath, false, prbar_arcade_general_export);
      end;
    2:
      ShowMessage('Work In Progress');
    3:
      ShowMessage('Work In Progress');
  end;
end;

procedure Tarcade.chb_arcade_graphics_center_windowClick(Sender: TObject);
begin
  dm.tArcadeConfigwin_center.AsInteger := (Sender as TCheckBox).IsChecked.ToInteger;
end;

{ Tarcade }

procedure Tarcade.chb_arcade_sound_enableClick(Sender: TObject);
begin
  dm.tArcadeConfigsound.AsInteger := (Sender as TCheckBox).IsChecked.ToInteger;
end;

procedure Tarcade.fields_active(enable: Boolean);
begin
  edt_arcade_dirs_const_roms.Enabled := enable;
  edt_arcade_dirs_const_samples.Enabled := enable;
  edt_arcade_dirs_const_nvram.Enabled := enable;
  edt_arcade_dirs_const_hiscore.Enabled := enable;
  spb_arcade_dirs_const_roms.Enabled := enable;
  spb_arcade_dirs_const_roms_refresh.Enabled := enable;
  spb_arcade_dirs_const_samples.Enabled := enable;
  spb_arcade_dirs_const_nvram.Enabled := enable;
  spb_arcade_dirs_const_hiscore.Enabled := enable;
end;

procedure Tarcade.lbArcadeGeneralFrontendViewChange(Sender: TObject);
begin
  if lbArcadeGeneralFrontendView.ListItems[0].IsSelected then
    dm.tArcadeConfigselect_cover.AsString := 'boxart'
  else if lbArcadeGeneralFrontendView.ListItems[1].IsSelected then
    dm.tArcadeConfigselect_cover.AsString := 'boxart_back'
  else if lbArcadeGeneralFrontendView.ListItems[2].IsSelected then
    dm.tArcadeConfigselect_cover.AsString := 'banner'
  else if lbArcadeGeneralFrontendView.ListItems[3].IsSelected then
    dm.tArcadeConfigselect_cover.AsString := 'clearlogo'
  else if lbArcadeGeneralFrontendView.ListItems[4].IsSelected then
    dm.tArcadeConfigselect_cover.AsString := 'screenshot'
  else if lbArcadeGeneralFrontendView.ListItems[5].IsSelected then
    dm.tArcadeConfigselect_cover.AsString := 'fanart';
end;

procedure Tarcade.OnClose;
begin
//
  dm.tArcadeConfig.Post;
  dm.tArcadeConfig.ApplyUpdates;
end;

procedure Tarcade.OnShow;
begin
  dm.tArcadeConfig.Edit;

  // <Translations
  // General
  ti_arcade_general.Text := lang.getTransString(GENERAL, dm.tConfiglang.AsInteger);
  grb_arcade_general_export.Text := lang.getTransString(mEXPORT, dm.tConfiglang.AsInteger);
  btn_arcade_general_export.Text := lang.getTransString(mEXPORT, dm.tConfiglang.AsInteger);
  lbl_arcade_general_export.Text := lang.getTransString(EXPORT_COMPATIBILITY_LIST, dm.tConfiglang.AsInteger);
  // Graphics
  ti_arcade_graphics.Text := lang.getTransString(Graphics, dm.tConfiglang.AsInteger);
  lbl_arcade_graphics_driver.Text := lang.getTransString(SELECT_GRAPHICS_DRIVER, dm.tConfiglang.AsInteger);
  lbl_arcade_graphics_driver_selected.Text := lang.getTransString(SELECTED_GRAPHICS_DRIVER, dm.tConfiglang.AsInteger);
  grb_arcade_graphics_window_state.Text := lang.getTransString(WINDOW_STATE, dm.tConfiglang.AsInteger);
  rb_arcade_graphics_windowed.Text := lang.getTransString(WINDOW, dm.tConfiglang.AsInteger);
  rb_arcade_graphics_fullscreen.Text := lang.getTransString(FULLSCREEN, dm.tConfiglang.AsInteger);
  grb_arcade_graphics_windowed.Text := lang.getTransString(WINDOW_MODE, dm.tConfiglang.AsInteger);
  chb_arcade_graphics_center_window.Text := lang.getTransString(CENTER_IN_DESKTOP, dm.tConfiglang.AsInteger);
  grb_arcade_graphics_window_size.Text := lang.getTransString(WINDOW_SIZE, dm.tConfiglang.AsInteger);
  rb_arcade_graphics_window_original.Text := lang.getTransString(ORIGINAL, dm.tConfiglang.AsInteger);
  grb_arcade_graphics_fullscreen.Text := lang.getTransString(FULLSCREEN_MODE, dm.tConfiglang.AsInteger);
  lbl_arcade_graphics_full_width.Text := lang.getTransString(mWIDTH, dm.tConfiglang.AsInteger);
  lbl_arcade_graphics_full_height.Text := lang.getTransString(mHEIGHT, dm.tConfiglang.AsInteger);
  chb_arcade_graphics_bezels.Text := lang.getTransString(SHOW_BEZELS, dm.tConfiglang.AsInteger);
  // Sound
  ti_arcade_sound.Text := lang.getTransString(SOUND, dm.tConfiglang.AsInteger);
  chb_arcade_sound_enable.Text := lang.getTransString(ENABLE_SOUND, dm.tConfiglang.AsInteger);
  // Directories
  ti_arcade_dirs.Text := lang.getTransString(DIRECTORIES, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_const_roms.Text := 'Roms ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_const_roms.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Arcade Roms ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_const_samples.Text := 'Samples ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_const_samples.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Samples ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_const_nvram.Text := 'NvRam ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_const_nvram.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' NvRam ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_const_hiscore.Text := 'Hi Score ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_const_hiscore.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Hi Score ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  // Media Directories
  ti_arcade_media_dirs.Text := lang.getTransString(MEDIA, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_media_images.Text := 'Snapshots ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_media_images.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Snapshots ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_media_video.Text := 'Video ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_media_video.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Video ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_media_manuals.Text := 'Manuals ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_media_manuals.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Manuals ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  txt_arcade_dirs_media_bezels.Text := 'Bezels ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);
  edt_arcade_dirs_media_bezels.TextPrompt := lang.getTransString(ADD, dm.tConfiglang.AsInteger) + ' Bezels ' + lang.getTransString(DIRECTORY, dm.tConfiglang.AsInteger);

  // Translations>

  // General
  // Frontend type

  beforeView := dm.tArcadeConfigfrontend_type.AsString;
  beforeImgType := dm.tArcadeConfigselect_cover.AsString;

  if dm.tArcadeConfigfrontend_type.AsString = 'tiled' then
    lbArcadeGeneralFrontendType.ListItems[0].IsSelected := true
  else if dm.tArcadeConfigfrontend_type.AsString = 'list_view' then
    lbArcadeGeneralFrontendType.ListItems[1].IsSelected := true;
  // Frontend view
  if dm.tArcadeConfigselect_cover.AsString = 'boxart' then
    lbArcadeGeneralFrontendView.ListItems[0].IsSelected := true
  else if dm.tArcadeConfigselect_cover.AsString = 'boxart_back' then
    lbArcadeGeneralFrontendView.ListItems[1].IsSelected := true
  else if dm.tArcadeConfigselect_cover.AsString = 'banner' then
    lbArcadeGeneralFrontendView.ListItems[2].IsSelected := true
  else if dm.tArcadeConfigselect_cover.AsString = 'clearlogo' then
    lbArcadeGeneralFrontendView.ListItems[3].IsSelected := true
  else if dm.tArcadeConfigselect_cover.AsString = 'screenshot' then
    lbArcadeGeneralFrontendView.ListItems[4].IsSelected := true
  else if dm.tArcadeConfigselect_cover.AsString = 'fanart' then
    lbArcadeGeneralFrontendView.ListItems[5].IsSelected := true;

  // Graphics
  edt_arcade_graphics_full_width.Text := dm.tArcadeConfigfull_x.AsString;
  edt_arcade_graphics_full_height.Text := dm.tArcadeConfigfull_y.AsString;
  chb_arcade_graphics_bezels.IsChecked := dm.tArcadeConfigbezels.AsInteger.ToBoolean;
  chb_arcade_graphics_center_window.IsChecked := dm.tArcadeConfigwin_center.AsInteger.ToBoolean;
  case dm.tArcadeConfigwin_size.AsInteger of
    0:
      rb_arcade_graphics_window_original.OnClick(rb_arcade_graphics_window_original);
    1:
      rb_arcade_graphics_window_2x.OnClick(rb_arcade_graphics_window_2x);
    2:
      rb_arcade_graphics_window_3x.OnClick(rb_arcade_graphics_window_3x);
  end;
  // Sound
  chb_arcade_sound_enable.IsChecked := dm.tArcadeConfigsound.AsInteger.ToBoolean;
  // Directories
  edt_arcade_dirs_const_roms.Text := dm.tArcaderom_global_path.AsString;
  edt_arcade_dirs_const_samples.Text := dm.tConfigsamples.AsString;
  edt_arcade_dirs_const_nvram.Text := dm.tConfignvram.AsString;
  edt_arcade_dirs_const_hiscore.Text := dm.tConfighiscore.AsString;
  // Media
  // edt_arcade_dirs_media_images.Text := config.emu_path[0].snapshots;
  // edt_arcade_dirs_media_video.Text := config.emu_path[0].video;
  // edt_arcade_dirs_media_manuals.Text := config.emu_path[0].manuals;
  // edt_arcade_dirs_media_bezels.Text := config.emu_path[0].bezels;
  if dm.tArcadeConfigfullscreen.AsInteger.ToBoolean then
    rb_arcade_graphics_fullscreen.OnClick(nil)
  else
    rb_arcade_graphics_windowed.OnClick(nil);
end;

procedure Tarcade.rb_arcade_graphics_fullscreenClick(Sender: TObject);
begin
  rb_arcade_graphics_fullscreen.IsChecked := true;
  grb_arcade_graphics_windowed.Enabled := false;
  grb_arcade_graphics_fullscreen.Enabled := true;
  if dm.tArcadeConfigfullscreen.AsInteger <> 1 then
    arcadeAction.setFullscreen;
end;

procedure Tarcade.rb_arcade_graphics_windowedClick(Sender: TObject);
begin
  rb_arcade_graphics_windowed.IsChecked := true;
  grb_arcade_graphics_fullscreen.Enabled := false;
  grb_arcade_graphics_windowed.Enabled := true;
  if dm.tArcadeConfigfullscreen.AsInteger <> 0 then
    arcadeAction.setFullscreen;
end;

procedure Tarcade.rb_arcade_graphics_window_2xClick(Sender: TObject);
begin
  rb_arcade_graphics_window_2x.IsChecked := true;
  set_win_size(1);
end;

procedure Tarcade.rb_arcade_graphics_window_3xClick(Sender: TObject);
begin
  rb_arcade_graphics_window_3x.IsChecked := true;
  set_win_size(2);
end;

procedure Tarcade.rb_arcade_graphics_window_originalClick(Sender: TObject);
begin
  rb_arcade_graphics_window_original.IsChecked := true;
  set_win_size(0);
end;

procedure Tarcade.reload_main;
var
  vi: integer;
begin
  vi := 0;
  with dm.Tarcade do
  begin
    first;
    while not eof do
    begin
      if arcadeAction.grid_rect[vi].TagString = dm.tArcaderom.AsString then
        arcadeAction.grid_img_gray[vi].Enabled := false;
      arcadeAction.roms_to_run[vi] := false;
      inc(vi);
      next;
    end;
  end;
end;

function Tarcade.scan_dir_roms(dir: string; refresh: Boolean): integer;
var
  games_list: TStringList;
  dir_list: TArray<System.String>;
  vi, vk: integer;
  rom_name, rom_update_name: string;
  full, only: string;
begin
  games_list := TStringList.Create;

  dm.Tarcade.first;
  while not dm.Tarcade.eof do
  begin
    games_list.ADD(dm.tArcaderom.AsString);
    dm.Tarcade.next;
  end;
  // if refresh = false then
  // dir_list := System.IOUtils.TDirectory.GetFiles(dir, '*.zip')
  // else
  // dir_list := System.IOUtils.TDirectory.GetFiles(config.emu_path[0].roms, '*.zip');

  pb_arcade_dirs.Visible := true;
  pb_arcade_dirs.Max := games_list.Count;
  pb_arcade_dirs.Min := 0;
  pb_arcade_dirs.Value := 0;
  txt_arcade_dirs.Text := '';
  for vk := 0 to games_list.Count - 1 do
  begin
    rom_update_name := '';
    txt_arcade_dirs.Text := 'Serching for (' + games_list[vk] + ')';
    pb_arcade_dirs.Value := pb_arcade_dirs.Value + 1;
    application.ProcessMessages;
    for vi := 0 to High(dir_list) do
    begin
      rom_name := ExtractFileName(dir_list[vi]);
      Delete(rom_name, length(rom_name) - 3, 4);
      if games_list[vk] = rom_name then
      begin
        rom_update_name := games_list[vk];
        Break;
      end;
    end;

    if rom_update_name <> '' then
    begin
      if refresh then
      begin
        // full := config.emu_path[0].roms + rom_update_name + '.zip';
        // only := config.emu_path[0].roms;
      end
      else
      begin
        full := dir + PathDelim + rom_update_name + '.zip';
        only := dir + PathDelim;
      end;
    end
    else
    begin
      full := '';
      only := '';
    end;

    dm.Tarcade.Edit;
    dm.tArcaderom_path.AsString := full;
    dm.tArcaderom_global_path.AsString := only;
    dm.Tarcade.Post;
  end;
end;

procedure Tarcade.sd_arcade_configCanClose(Sender: TObject; var CanClose: Boolean);
begin
  case save_dialog_type of
    sd_export_markup:
      ;
    sd_export_html:
      ;
    sd_export_json:
      CreateAndSave_json(sd_arcade_config.filename);
    sd_export_xml:
      CreateAndSave_xml(sd_arcade_config.filename);
  end;
end;

procedure Tarcade.set_win_size(num: integer);
begin
  dm.tArcadeConfigwin_size.Value := num;
end;

procedure Tarcade.spb_arcade_dirs_const_hiscoreClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select HiScore Directory...', '', dir) then
    dm.tConfighiscore.AsString := dir;
end;

procedure Tarcade.spb_arcade_dirs_const_nvramClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select NvRam Directory...', '', dir) then
    dm.tConfignvram.AsString := dir;
end;

procedure Tarcade.spb_arcade_dirs_const_romsClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select Roms Directory...', '', dir) then
  begin
    scan_dir_roms(dir, false);
    fields_active(false);
    edt_arcade_dirs_const_roms.Text := dir;
    // config.emu_path[0].roms := dir;
    reload_main;
    fields_active(true);
    pb_arcade_dirs.Visible := false;
  end;
end;

procedure Tarcade.spb_arcade_dirs_const_roms_refreshClick(Sender: TObject);
begin
  scan_dir_roms('', true);
  fields_active(false);
  reload_main;
  fields_active(true);
  pb_arcade_dirs.Visible := false;
end;

procedure Tarcade.spb_arcade_dirs_const_samplesClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select Samples Directory...', '', dir) then
    dm.tConfigsamples.AsString := dir;
end;

procedure Tarcade.spb_arcade_dirs_media_manualsClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select Manuals Directory...', '', dir) then
  begin
    // Not set yet
  end;
end;

procedure Tarcade.spb_arcade_dirs_media_snapshotClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select Snapshots Directory...', '', dir) then
  begin
    // Not set yet
  end;
end;

procedure Tarcade.spb_arcade_dirs_media_videoClick(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select Videos Directory...', '', dir) then
  begin
    // Not Set Yet
  end;

end;

end.
