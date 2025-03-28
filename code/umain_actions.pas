unit umain_actions;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  FMX.Platform.Win,
  FMX.Objects,
  Winapi.Windows,
  SDL2,
  // SDL2_TTF,
  SDL2_IMAGE,
  // SDL2_Mixer,
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
    fav_state: integer;
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

    procedure main_working_games;
    procedure main_working_minor_games;
    procedure main_working_major_games;
    procedure main_not_working_games;
    procedure main_favorites;
    // Scraper
    procedure startScraping(Sender: TObject);
    // Grid Info
    procedure infoExit;
    procedure infoShow;
    procedure infoEdit;
    procedure infoImgDClick;
    procedure infoUnlockContent;
    procedure infoRemoveContent;
    procedure infoRemoveImages;
    procedure infoImgDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure infoImgDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure infoOnMouseEnter(Sender: TObject);
    procedure infoOnMouseLeave(Sender: TObject);
    procedure infoOnMouseClick(Sender: TObject);
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
  // main_engine_ogl,
  init_games,
  uscraper_tgdb,
  scraper_tgdb,
  splash,
  unes_actions,
  emu_functions,
  multi_platform,
  uDataModule;

{ TMAIN_ACTIONS }

constructor TMAIN_ACTIONS.Create;
begin
  front_action := TFRONTEND.Create;
  front_action.CreateInfoBindings;
  fav_state := 0;
end;

destructor TMAIN_ACTIONS.Destroy;
begin

end;

procedure TMAIN_ACTIONS.infoOnMouseClick(Sender: TObject);
begin
  front_action.Rect_OnMouseClick(Sender);
end;

procedure TMAIN_ACTIONS.infoOnMouseEnter(Sender: TObject);
begin
  front_action.Rect_OnMouseEnter(Sender);
end;

procedure TMAIN_ACTIONS.infoOnMouseLeave(Sender: TObject);
begin
  front_action.Rect_OnMouseLeave(Sender);
end;

procedure TMAIN_ACTIONS.infoRemoveContent;
var
  UserResponse: integer;
begin
  UserResponse := MessageDlg('This action will delete all the content except the rom, game name, year and leave the state in working rom. Do you want to continue?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
  if UserResponse = mrYes then
  begin
    dm.tArcade.Edit;
    dm.tArcadeTGDB.Edit;
    dm.tTGDBDevelopers.Edit;
    dm.tTGDBPublishers.Edit;
    dm.tTGDBGenres.Edit;
    dm.tTGDBDevelopersname.Value := '';
    dm.tTGDBPublishersname.Value := '';
    dm.tTGDBGenresname.Value := '';
    dm.tArcadeTGDBplayers.Value := '';
    dm.tArcadeTGDBcoop.Value := '';
    dm.tArcadehiscore.Value := 0;
    dm.tArcadeTGDBoverview.Value := '';
    dm.tArcadestate_icon.Value := 0;
    frm_main.geInfoProgressIconPlayable.Enabled := true;
    dm.tArcadestate_desc.Value := 'Everything seems ok.';
    front_action.editInfoSave;
    front_action.grid_state[front_action.grid_selected].Fill.Color := $FF43A22C;
  end;
end;

procedure TMAIN_ACTIONS.infoRemoveImages;
var
  vi, UserResponse: integer;
begin
  UserResponse := MessageDlg('This action will delete all the images. Do you want to continue?', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
  if UserResponse = mrYes then
  begin
    dm.tArcadeTGDBImages.Edit;
    frm_main.imgGameInfoBoxart.Bitmap := frm_main.imgNotFound;
    if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'boxart', '']), []) then
      dm.tArcadeTGDBImages.Delete;
    frm_main.imgGameInfoLogo.Bitmap := frm_main.imgNotFound;
    if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'clearlogo', '']), []) then
      dm.tArcadeTGDBImages.Delete;
    frm_main.imgGameInfoFanart.Bitmap := frm_main.imgNotFound;
    if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'fanart', '']), []) then
      dm.tArcadeTGDBImages.Delete;
    frm_main.imgGameInfoBanner.Bitmap := frm_main.imgNotFound;
    if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'banner', '']), []) then
      dm.tArcadeTGDBImages.Delete;
    frm_main.imgGameInfoBoxartFront.Bitmap := frm_main.imgNotFound;
    if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'boxart', 'front']), []) then
      dm.tArcadeTGDBImages.Delete;
    frm_main.imgGameInfoBoxartBack.Bitmap := frm_main.imgNotFound;
    if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'boxart', 'back']), []) then
      dm.tArcadeTGDBImages.Delete;

    for vi := High(front_action.imgSnap) downto 0 do
    begin
      FreeAndNil(front_action.imgSnapGlow[vi]);
      FreeAndNil(front_action.imgSnap[vi]);
      if dm.tArcadeTGDBImages.Locate('rom;img_type;side', VarArrayOf([front_action.tmpTable.FieldByName('rom').AsString, 'screenshot', '']), []) then
        dm.tArcadeTGDBImages.Delete;
    end;
    setLength(front_action.imgSnap, 0);
    setLength(front_action.imgSnapGlow, 0);
    front_action.editInfoSave;
    front_action.grid_img[front_action.grid_selected].Bitmap := frm_main.imgNotFound;
  end;
end;

procedure TMAIN_ACTIONS.key_down(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  front_action.key_down(Key, KeyChar, Shift);
end;

procedure TMAIN_ACTIONS.main_working_major_games;
begin
  if frm_main.skaiEmuWorkingMajor.Animation.Running = false then
  begin
    front_action.filter('2');
    if frm_main.skaiEmuWorkingMajor.Animation.Progress = 0 then
    begin
      frm_main.skaiEmuWorkingMajor.Animation.StartProgress := 0;
      frm_main.skaiEmuWorkingMajor.Animation.StopProgress := 1;
      frm_main.skaiEmuWorkingMajor.Animation.Start;
    end
    else
      frm_main.skaiEmuWorkingMajor.Animation.Progress := 0;
  end;
end;

procedure TMAIN_ACTIONS.main_working_minor_games;
begin
  if frm_main.skaiEmuWorkingMinor.Animation.Running = false then
  begin
    front_action.filter('1');
    if frm_main.skaiEmuWorkingMinor.Animation.Progress = 0 then
    begin
      frm_main.skaiEmuWorkingMinor.Animation.StartProgress := 0;
      frm_main.skaiEmuWorkingMinor.Animation.StopProgress := 1;
      frm_main.skaiEmuWorkingMinor.Animation.Start;
    end
    else
      frm_main.skaiEmuWorkingMinor.Animation.Progress := 0;
  end;
end;

procedure TMAIN_ACTIONS.main_favorites;
begin
  case fav_state of
    0:
      begin
        fav_state := 1;
        frm_main.img_emu_favorites.Bitmap := frm_main.imgFavEnable;
        front_action.filter('4');
      end;
    1:
      begin
        fav_state := 2;
        frm_main.img_emu_favorites.Bitmap := frm_main.imgFavNo;
        front_action.filter('5');
      end;
    2:
      begin
        fav_state := 0;
        frm_main.img_emu_favorites.Bitmap := frm_main.imgFavDisable;
        front_action.filter('6');
      end;
  end;
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
  ReportMemoryLeaksOnShutdown := true;
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
end;

procedure TMAIN_ACTIONS.main_form_full_fps;
begin
  main_screen.Fast := not main_screen.Fast;
  QueryPerformanceCounter(cont_sincroniza);
  // if not(main_screen.fullscreen) then
  // Winapi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
end;

procedure TMAIN_ACTIONS.infoEdit;
begin
  frm_main.eff_fillRGB_grid_info_edit.Enabled := not frm_main.eff_fillRGB_grid_info_edit.Enabled;
  front_action.editInfo(frm_main.eff_fillRGB_grid_info_edit.Enabled);
end;

procedure TMAIN_ACTIONS.infoExit;
begin
  if front_action.is_edited = false then
  begin
    front_action.editInfoFree;
    infoShow;
  end;
end;

procedure TMAIN_ACTIONS.infoImgDClick;
begin
  front_action.edit_img_doubleclick_info;
end;

procedure TMAIN_ACTIONS.infoImgDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  front_action.imgDragOver(Sender, Data, Point, Operation);
end;

procedure TMAIN_ACTIONS.infoImgDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  front_action.imgDropped(Sender, Data, Point);
end;

procedure TMAIN_ACTIONS.infoShow;
begin
  frm_main.lay_game.Visible := not frm_main.lay_game.Visible;
  frm_main.eff_blur_main.Enabled := frm_main.lay_game.Visible;
  if frm_main.lay_game.Visible then
  begin
    frm_main.lay_game.Position.X := (screen.Width / 2) - 900;
    frm_main.lay_game.Position.Y := (screen.Height / 2) - 450;
  end
end;

procedure TMAIN_ACTIONS.infoUnlockContent;
begin
  front_action.is_contentlocked := not front_action.is_contentlocked;
  frm_main.spbInfoRemoveContent.Enabled := not front_action.is_contentlocked;
  frm_main.spbInfoRemoveImages.Enabled := not front_action.is_contentlocked;
  if front_action.is_contentlocked then
    frm_main.imgInfoUnlockContent.Bitmap := frm_main.imgLock
  else
    frm_main.imgInfoUnlockContent.Bitmap := frm_main.imgUnLock;
end;

procedure TMAIN_ACTIONS.main_form_play;
begin
  if EmuStatus = EsStoped then
  begin
    if dm.tConfigcurrent_emu.AsString = 'arcade' then
    begin
      main_vars.machine_type := dm.tArcadeexe_num.AsInteger;
      main_screen.fullscreen := dm.tArcadeConfigfullscreen.AsInteger.ToBoolean;
    end
    else if dm.tConfigcurrent_emu.AsString = 'nes' then
    begin
      main_vars.machine_type := 1000;
      load_machine(1000);
      // machine_calls.cartridges := front_actio
    end;
    frm_main.tmr_machine.Enabled := true;
  end;
end;

procedure TMAIN_ACTIONS.main_form_reset_game;
begin
  if @machine_calls.Reset <> nil then
    machine_calls.Reset;
end;

procedure TMAIN_ACTIONS.main_form_run_game;
begin
  EmuStatus := EsRunning;
  pause_between := 0;
  start_gt := now;
  pause_offgt_stopped := true;
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

  frm_main.eff_glow_main.Enabled := true;
end;

procedure TMAIN_ACTIONS.main_form_sb_mouse_leave(Sender: TObject);
begin
  frm_main.eff_glow_main.Enabled := false;
end;

procedure TMAIN_ACTIONS.main_form_search(Sender: TObject);
// var
// vi: Integer;
begin
  // if dm.tConfigprj_kind.AsString = 'medium_thumps' then
  front_action.searchGame;
  // else if dm.tConfigprj_kind.AsString = '' then
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

procedure TMAIN_ACTIONS.startScraping(Sender: TObject);
begin
  if (Sender is TSpeedButton) then
    scrape_tgdb := TSCRAPER_TGDB.Create(frm_main, dm.tConfigcurrent_emu.AsString, '', '', false)
  else if (Sender is TRectangle) then
    scrape_tgdb := TSCRAPER_TGDB.Create(frm_main, dm.tConfigcurrent_emu.AsString, dm.tArcadename.AsString, dm.tArcaderom.AsString, true);
  frm_scraper.ShowModal;
end;

procedure TMAIN_ACTIONS.main_form_show;
begin
  frm_main.stylebook_main.LoadFromFile(dm.tConfigprj_themes.AsString + 'PuertoRico_Win.style');
  frm_main.StyleBook := frm_main.stylebook_main;
  frm_main.img_platform_change.Bitmap.LoadFromFile(dm.tConfigprj_images_config.AsString + 'arcade.png');

  frm_emu.show_emulator_selected;
  front_action.FilterRunFirst;

  frm_main.lay_game.Visible := false;
end;

procedure TMAIN_ACTIONS.main_form_step_before_run_game_timer;
begin
  machine_calls.video_rend := main_engine.getVideoPipeline;
  frm_main.tmr_machine.Enabled := false;
  frm_main.lbl_selected_info.Text := 'Now playing :';
  if ((@machine_calls.Close <> nil) and main_vars.driver_ok) then
    machine_calls.Close;
  main_engine.reset_DSP_FM;
  load_machine(main_vars.machine_type);
  main_vars.driver_ok := false;
  if @machine_calls.Start <> nil then
    main_vars.driver_ok := machine_calls.Start;
  timers.autofire_init;
  QueryPerformanceFrequency(cont_micro);
  value_sync := (1 / machine_calls.fps_max) * cont_micro;
  QueryPerformanceCounter(cont_sincroniza);
  change_video_rendering;
  if not(main_vars.driver_ok) then
    EmuStatus := EsStoped
  else
    main_form_run_game;
end;

procedure TMAIN_ACTIONS.main_form_stop;
begin
  // save_and_display_total_play_time(start_gt, stop_gt, pause_between);
  exit_game;
end;

procedure TMAIN_ACTIONS.main_not_working_games;
begin
  if frm_main.skaiEmuNotWorking.Animation.Running = false then
  begin
    front_action.filter('3');
    if frm_main.skaiEmuNotWorking.Animation.Progress = 0 then
    begin
      frm_main.skaiEmuNotWorking.Animation.StartProgress := 0;
      frm_main.skaiEmuNotWorking.Animation.StopProgress := 1;
      frm_main.skaiEmuNotWorking.Animation.Start;
    end
    else
      frm_main.skaiEmuNotWorking.Animation.Progress := 0;
  end;
end;

procedure TMAIN_ACTIONS.main_working_games;
begin
  if frm_main.skaiEmuWorking.Animation.Running = false then
  begin
    front_action.filter('0');
    if frm_main.skaiEmuWorking.Animation.Progress = 0 then
    begin
      frm_main.skaiEmuWorking.Animation.StartProgress := 0;
      frm_main.skaiEmuWorking.Animation.StopProgress := 1;
      frm_main.skaiEmuWorking.Animation.Start;
    end
    else
      frm_main.skaiEmuWorking.Animation.Progress := 0;
  end;
end;

procedure TMAIN_ACTIONS.main_form_reduce_fps;
begin
  main_vars.current := (main_vars.current + 1) and 3;
  value_sync := (1 / (machine_calls.fps_max / (main_vars.current + 1))) * cont_micro;
end;

procedure TMAIN_ACTIONS.save_and_display_total_play_time(start_time, stop_time: TTime; pause_secs_between: Int64);
var
  total: Int64;
begin
  front_action.tmpTable.Edit;
  total := SecondsBetween(start_time, stop_time);
  total := total - pause_secs_between;
  total := total + front_action.tmpTable.FieldByName('total_time').AsInteger;
  front_action.tmpTable.FieldByName('total_time').AsInteger := total;
  front_action.tmpTable.FieldByName('play_times').AsInteger := front_action.tmpTable.FieldByName('play_times').AsInteger + 1;
  front_action.tmpTable.FieldByName('last_played').AsString := DateTimeToStr(now);
  front_action.tmpTable.Post;
  front_action.tmpTable.ApplyUpdates();

  frm_main.txtSTBLastPlayed.Text := 'Last Played: ' + front_action.tmpTable.FieldByName('last_played').AsString;
  frm_main.txtSTBPlayCounts.Text := 'Game Played: ' + front_action.tmpTable.FieldByName('play_times').AsString + ' times';
  frm_main.txtSTBPlayTime.Text := 'Total Time Played: ' + front_action.tmpTable.FieldByName('total_time').AsString;
end;

procedure TMAIN_ACTIONS.show_kind_type(kind_type: byte);
begin
  case kind_type of
    0:
      begin
        frm_main.lv_main_list.Visible := true;
        frm_main.lay_main_grid.Visible := false;
      end;
    1:
      begin
        frm_main.lv_main_list.Visible := false;
        frm_main.lay_main_grid.Visible := true;
      end;
  end;
end;

end.
