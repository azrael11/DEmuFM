unit umain_actions;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Platform.Win,
  Winapi.Windows,
  SDL2,
  SDL2_TTF,
  SDL2_IMAGE,
  SDL2_Mixer,
  FMX.Edit,
  FMX.Forms,
  System.DateUtils,
  System.Types,
  FMX.Types,
  FMX.StdCtrls,
  System.UITypes,
  front_main,
  FMX.Dialogs,
  vars_consts;

type
  TMAIN_ACTIONS = class

  private
    procedure show_kind_type(kind_type: byte);

  public
    constructor Create;
    destructor Destroy;

    procedure save_and_display_total_play_time(start_time, stop_time: TTime; pause_secs_between: Int64);

    procedure main_form_create;
    procedure main_form_show;
    procedure main_form_close;

    procedure main_form_play;
    procedure main_form_step_before_run_game_timer;
    procedure main_form_run_game;
    procedure main_form_reset_game;
    procedure main_form_full_fps;
    procedure main_form_reduce_fps;
    procedure main_form_stop;

    procedure main_form_search(Sender: TObject);

    procedure main_form_sb_mouse_enter(Sender: TObject);
    procedure main_form_sb_mouse_leave(Sender: TObject);
    procedure key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure main_form_lang_strings;

    procedure main_working_games;
    procedure main_working_minor_games;
    procedure main_working_major_games;
    procedure main_not_working_games;
    // Scraper
    procedure main_form_set_scraper(Sender: TObject);
    // Grid Info
    procedure main_form_grid_show;
    procedure main_form_grid_edit;
    procedure main_form_grid_list_create(Sender: TObject);
    procedure main_form_grid_list_apply(Sender: TObject);
    procedure main_form_grid_list_cancel;
    procedure main_form_grid_image_DClick;
    procedure main_form_grid_image_DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF;
      var Operation: TDragOperation);
    procedure main_form_grid_image_InfoDropped(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure grid_rect_OnMouseEnter(Sender: TObject);
    procedure grid_rect_OnMouseLeave(Sender: TObject);
    procedure grid_rect_OnMouseClick(Sender: TObject);
  end;

var
  main_actions: TMAIN_ACTIONS;

implementation

uses
  main,
  umain_config,
  prj_functions,
  config_controls,
  controls_engine,
  configuration,
  main_info,
  emulators,
  uarcade_actions,
  ulang,
  timer_engine,
  main_engine,
  init_games,
  uscraper_tgdb,
  scraper_tgdb,
  splash,
  unes_actions,
  emu_functions,
  multi_platform, uDataModule;

{ TMAIN_ACTIONS }

constructor TMAIN_ACTIONS.Create;
begin
  front_action := TFRONTEND.Create;
end;

destructor TMAIN_ACTIONS.Destroy;
begin

end;

procedure TMAIN_ACTIONS.grid_rect_OnMouseClick(Sender: TObject);
begin
  front_action.Rect_OnMouseClick(Sender);
end;

procedure TMAIN_ACTIONS.grid_rect_OnMouseEnter(Sender: TObject);
begin
  front_action.Rect_OnMouseEnter(Sender);
end;

procedure TMAIN_ACTIONS.grid_rect_OnMouseLeave(Sender: TObject);
begin
  front_action.Rect_OnMouseLeave(Sender);
end;

procedure TMAIN_ACTIONS.key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  front_action.key_down(Key, KeyChar, Shift);
end;

procedure TMAIN_ACTIONS.main_working_major_games;
begin
  front_action.filter(fil_Working_With_Major);
end;

procedure TMAIN_ACTIONS.main_working_minor_games;
begin
  if dm.tConfigprj_kind.AsString = 'mediumSnapshots' then
    front_action.filter(fil_Working_With_Minor);
end;

procedure TMAIN_ACTIONS.main_form_close;
begin
  timers.Free;
  front_action.Free;
  lang.Free;
  if window_render <> nil then
  begin
    // if cinta_tzx.cargada then
    // vaciar_cintas;
    if ((@machine_calls.Close <> nil) and main_vars.driver_ok) then
      machine_calls.Close;
    reset_DSP_FM;
    if joystick_def[0] <> nil then
      close_joystick(p_contrls.map_arcade.num_joystick[0]);
    if joystick_def[1] <> nil then
      close_joystick(p_contrls.map_arcade.num_joystick[1]);
    SDL_DestroyWindow(window_render);
    SDL_VideoQuit;
    SDL_Quit;
    Halt(0);
  end;
  Halt(0);
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TMAIN_ACTIONS.main_form_create;
begin
{$IFDEF WIN64}
  SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);
{$ENDIF}
  config := TMAIN_CONFIG_VARS.Create;
  timers := timer_eng.Create;
  EmuStatus := EsStoped;

  if SDL_Init(SDL_INIT_EVERYTHING) < 0 then
    exit;
  if TTF_Init = -1 then
    exit;
  if Mix_Init(MIX_INIT_MP3 or MIX_INIT_OGG) < 0 then
    exit;
  Mix_OpenAudio(44100, AUDIO_S32MSB, 2, 1024);

  // Sounds
  pause_sound := Mix_LoadWav(PAnsiChar(AnsiString(dm.tConfigprj_path.Value + 'pause.wav')));
  unpause_sound := Mix_LoadWav(PAnsiChar(AnsiString(dm.tConfigprj_path.Value + 'unpause.wav')));

  //Font
  fps_fnt := TTF_OpenFont('fonts\Yeasty_Flavors.ttf', 24);
  if fps_fnt = nil then
    ShowMessage('Font not Loading');
  pause_fnt := TTF_OpenFont('fonts\Yeasty_Flavors.ttf', 48);
  if pause_fnt = nil then
    ShowMessage('Font not Loading');
  fps_font_color.r := 255;
  fps_font_color.g := 255;
  fps_font_color.b := 255;
end;

procedure TMAIN_ACTIONS.main_form_full_fps;
begin
  main_screen.Fast := not main_screen.Fast;
  QueryPerformanceCounter(cont_sincroniza);
  // if not(main_screen.fullscreen) then
  // Winapi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
end;

procedure TMAIN_ACTIONS.main_form_grid_edit;
begin
  frm_main.eff_fillRGB_grid_info_edit.Enabled := not frm_main.eff_fillRGB_grid_info_edit.Enabled;
  front_action.edit_info(frm_main.eff_fillRGB_grid_info_edit.Enabled);
end;

procedure TMAIN_ACTIONS.main_form_grid_image_DClick;
begin
  front_action.edit_img_doubleclick_info;
end;

procedure TMAIN_ACTIONS.main_form_grid_image_DragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  front_action.edit_dt_info_DragOver(Sender, Data, Point, Operation);
end;

procedure TMAIN_ACTIONS.main_form_grid_image_InfoDropped(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  front_action.edit_dt_info_Dropped(Sender, Data, Point);
end;

procedure TMAIN_ACTIONS.main_form_grid_list_apply(Sender: TObject);
begin
  case (Sender as TSpeedButton).Tag of
    10:
      scrape_tgdb.replace_list_item(TTL_Developers);
    20:
      scrape_tgdb.replace_list_item(TTL_Publishers);
    30:
      scrape_tgdb.replace_list_item(TTL_Genres);
  end;
end;

procedure TMAIN_ACTIONS.main_form_grid_list_cancel;
begin
  frm_main.layInfoList.Visible := False;
  frm_main.eff_blur_grid_info_list.Enabled := False;
end;

procedure TMAIN_ACTIONS.main_form_grid_list_create(Sender: TObject);
begin
  with frm_main do
  begin
    eff_blur_grid_info_list.Enabled := True;
    layInfoList.Visible := True;
    case (Sender as TSpeedButton).Tag of
      10:
        begin
          txtInfoListHeader.Text := 'List Of Developers';
          lbInfoList.Items := scrape_tgdb.get_tgdb_list_of(TTL_Developers);
        end;
      20:
        begin
          txtInfoListHeader.Text := 'List Of Publishers';
          lbInfoList.Items := scrape_tgdb.get_tgdb_list_of(TTL_Publishers);
        end;
      30:
        begin
          txtInfoListHeader.Text := 'List Of Genres';
          lbInfoList.Items := scrape_tgdb.get_tgdb_list_of(TTL_Genres);
        end;
    end;
  end;
end;

procedure TMAIN_ACTIONS.main_form_grid_show;
begin
  frm_main.lay_game.Visible := not frm_main.lay_game.Visible;
  frm_main.eff_blur_main.Enabled := frm_main.lay_game.Visible;
end;

procedure TMAIN_ACTIONS.main_form_lang_strings;
begin
  // frm_main.edt_search.TextPrompt := lang.load_lang_string('Find game...');
  // frm_main.lbl_selected_info_value.Text := lang.load_lang_string('None');

  frm_main.edt_search.TextPrompt := lang.getTransString(frm_main.edt_search.Tag, 0);
  frm_main.lbl_selected_info_value.Text := lang.getTransString(frm_main.lbl_selected_info_value.Tag, 0)
end;

procedure TMAIN_ACTIONS.main_form_play;
begin
  if EmuStatus = EsRunning then
    pause_click
  else if EmuStatus = EsStoped then
  begin
    if emu_active = emus_Arcade then
    begin
      main_vars.machine_type := dm.tArcadeexe_num.AsInteger;
      main_screen.fullscreen := dm.tArcadeConfigfullscreen.AsInteger.ToBoolean;
    end
    else if emu_active = emus_Nes then
    begin
      main_vars.machine_type := 1000;
      load_machine(1000);
      // machine_calls.cartridges := front_actio
    end;
    frm_main.tmr_machine.Enabled := True;
  end;
end;

procedure TMAIN_ACTIONS.main_form_reset_game;
begin
  if @machine_calls.Reset <> nil then
    machine_calls.Reset;
  // if not(main_screen.fullscreen) then
  // Winapi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
end;

procedure TMAIN_ACTIONS.main_form_run_game;
begin
  EmuStatus := EsRunning;
  pause_between := 0;
  start_gt := now;
  if @machine_calls.general_loop <> nil then
    machine_calls.general_loop;
end;

procedure TMAIN_ACTIONS.main_form_sb_mouse_enter(Sender: TObject);
begin
  frm_main.eff_glow_main.Parent := (Sender as TSpeedButton);
  if (Sender as TSpeedButton).Hint = 'stop' then
    frm_main.eff_glow_main.GlowColor := TAlphaColorRec.Red
  else if (Sender as TSpeedButton).Hint = 'play' then
    frm_main.eff_glow_main.GlowColor := TAlphaColorRec.Lightgreen
  else
    frm_main.eff_glow_main.GlowColor := TAlphaColorRec.Deepskyblue;

  frm_main.eff_glow_main.Enabled := True;
end;

procedure TMAIN_ACTIONS.main_form_sb_mouse_leave(Sender: TObject);
begin
  frm_main.eff_glow_main.Enabled := False;
end;

procedure TMAIN_ACTIONS.main_form_search(Sender: TObject);
var
  vi: Integer;
begin
  if dm.tConfigprj_kind.AsString = 'medium_thumps' then
    front_action.search_grid(st_search)
  else if dm.tConfigprj_kind.AsString = '' then
    // case  of
    // KT_ListView:
    // begin
    // for vi := 0 to frm_main.lv_main_list.ItemCount - 1 do
    // begin
    // if frm_main.lv_main_list.Items[vi].Text.ToUpper = (Sender as TEdit).Text.ToUpper then
    // begin
    // frm_main.lv_main_list.Selected := frm_main.lv_main_list.Items[vi + 1];
    // break;
    // end;
    // end;
    // end;
    // :
    //
    // end;
end;

procedure TMAIN_ACTIONS.main_form_set_scraper(Sender: TObject);
begin
  if (Sender As TSpeedButton).Tag = 0 then
    scrape_tgdb := TSCRAPER_TGDB.Create(frm_main, emu_active, '', '', False)
  else
    scrape_tgdb := TSCRAPER_TGDB.Create(frm_main, emu_active, dm.tArcadename.AsString,
      dm.tArcaderom.AsString, True);
  frm_scraper.ShowModal;
end;

procedure TMAIN_ACTIONS.main_form_show;
begin
  frm_main.stylebook_main.LoadFromFile(dm.tConfigprj_themes.AsString + 'PuertoRico_Win.style');
  frm_main.StyleBook := frm_main.stylebook_main;
  frm_main.img_platform_change.Bitmap.LoadFromFile(dm.tConfigprj_images_config.AsString + 'arcade.png');

  frm_config_controls.get_ingame_controls;
  frm_config_controls.get_players_controls;

  emu_active := emus_Arcade;
  main_form_lang_strings;
  frm_emu.show_emulator_selected(emu_active);
  emu_in_game.fps_show := False;

  // Set the layout of game config
  frm_main.lay_game.Visible := False;
  frm_main.lay_game.Position.X := 60;
  frm_main.lay_game.Position.Y := 90;
end;

procedure TMAIN_ACTIONS.main_form_step_before_run_game_timer;
begin
  frm_main.tmr_machine.Enabled := False;
  frm_main.lbl_selected_info.Text := 'Now playing :';
  if ((@machine_calls.Close <> nil) and main_vars.driver_ok) then
    machine_calls.Close;
  main_engine.reset_DSP_FM;
  load_machine(main_vars.machine_type);
  main_vars.driver_ok := False;
  if @machine_calls.Start <> nil then
    main_vars.driver_ok := machine_calls.Start;
  timers.autofire_init;
  QueryPerformanceFrequency(cont_micro);
  valor_sync := (1 / machine_calls.fps_max) * cont_micro;
  change_video;
  if not(main_vars.driver_ok) then
    EmuStatus := EsStoped
  else
  begin
    QueryPerformanceCounter(cont_sincroniza);
    main_form_run_game;
  end;
end;

procedure TMAIN_ACTIONS.main_form_stop;
begin
  save_and_display_total_play_time(start_gt, stop_gt, pause_between);
  exit_game;
end;

procedure TMAIN_ACTIONS.main_not_working_games;
begin
  front_action.filter(fil_Not_Working);
end;

procedure TMAIN_ACTIONS.main_working_games;
begin
  front_action.filter(fil_Working);
end;

procedure TMAIN_ACTIONS.main_form_reduce_fps;
begin
  main_vars.current := (main_vars.current + 1) and 3;
  valor_sync := (1 / (machine_calls.fps_max / (main_vars.current + 1))) * cont_micro;
  // if not(main_screen.fullscreen) then
  // Winapi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
end;

procedure TMAIN_ACTIONS.save_and_display_total_play_time(start_time, stop_time: TTime;
  pause_secs_between: Int64);
var
  total: Int64;
begin
  total := SecondsBetween(start_time, stop_time);
  total := total - pause_secs_between;
  total := total + dm.tArcadetotal_time.AsInteger;

  dm.tArcade.Edit;
  dm.tArcadetotal_time.Value := total;
  dm.tArcade.Post;
  dm.tArcade.ApplyUpdates();

  main.frm_main.txt_stb_main_total.Text := 'Total play time : ' + multi_platform.int_to_time(total);
end;

procedure TMAIN_ACTIONS.show_kind_type(kind_type: byte);
begin
  case kind_type of
    0:
      begin
        frm_main.lv_main_list.Visible := True;
        frm_main.lay_main_grid.Visible := False;
      end;
    1:
      begin
        frm_main.lv_main_list.Visible := False;
        frm_main.lay_main_grid.Visible := True;
      end;
  end;
end;

end.
