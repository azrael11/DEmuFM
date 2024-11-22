unit main_engine;

interface

uses
  System.Types,
  WinApi.Windows,
  FMX.Forms,
  FMX.Controls,
  FMX.Platform.Win,
  FMX.Graphics,
  System.SysUtils,
  System.Math,
  misc_functions,
  pal_engine,
  sound_engine,
  gfx_engine,
  vars_hide,
  device_functions,
  timer_engine,
  FMX.Platform,
  SDL2,
  SDL2_TTF,
  SDL2_IMAGE,
  SDL2_Mixer,
  FMX.Dialogs,
  System.UITypes,
  System.DateUtils,
  vars_consts,
  System.Math.Vectors;

const
  DSPFM_NAME = 'DEmuFM';
  DSPFM_VERSION = 'WIP0.2';
  PANT_SPRITES = 20;
  PANT_DOBLE = 21;
  PANT_AUX = 22;
  PANT_TEMP = 23;
  PANT_SPRITES_ALPHA = 24;
  MAX_PANT_VISIBLE = 19;
  MAX_PANT_SPRITES = 256;

  MAX_DIP_VALUES = $F;
  MAX_PUNBUF = 768;
  SCREEN_DIF = 20;
  FULL_SCREEN_X = 1920;
  FULL_SCREEN_Y = 1080;
  // Cpu lines
  CLEAR_LINE = 0;
  ASSERT_LINE = 1;
  HOLD_LINE = 2;
  PULSE_LINE = 3;
  ALREADY_RESET = 4;
  IRQ_DELAY = 5;
  INPUT_LINE_NMI = $20;
  M_PI = 3.1415926535;

const
  targetWidth = 1920;
  targetHeight = 1080;

type
  TMAIN_VARS = record
    mainmessage, caption: string;
    frames_sec, machine_type: word;
    language: integer;
    current: byte;
    service1, driver_ok, auto_exec, show_crc_error, center_screen, console_init: boolean;
    sort: word;
    system_type: byte;
  end;

  TDirectory = record
    Base: string;
    Nes: string;
    GameBoy: string;
    Chip8: string;
    sms: string;
    sg1000: string;
    gg: string;
    scv: string;
    // Coleco
    coleco: string;
    // Dirs Arcade
    Arcade_hi: string;
    Arcade_samples: string;
    Arcade_nvram: string;
    arcade_list_roms: array [0 .. $FF] of string;
    // Dirs spectrum
    spectrum_48: string;
    spectrum_128: string;
    spectrum_3: string;
    spectrum_tap_snap: string;
    spectrum_disk: string;
    spectrum_image: string;
    // Dirs amstrad
    amstrad_tap: string;
    amstrad_disk: string;
    amstrad_snap: string;
    amstrad_rom: string;
    // Dirs C64
    c64_tap: string;
    c64_disk: string;
    // Oric
    oric_tap: string;
    // PV1000
    pv1000: string;
    // PV2000
    pv2000: string;
    // Misc
    Preview: string;
    qsnapshot: string;
  end;

  TGLOBAL_CALLS = record
    start: function: boolean;
    num: integer;
    general_loop, reset, close, take_snapshot, setup, accept_config, cartridges, tapes: procedure;
    caption, open_file: string;
    fps_max: single;
    save_qsnap, load_qsnap: procedure(name: string);
  end;

  tmain_screen = record
    fullscreen: boolean;
    video_mode: byte;
    flip_main_screen, flip_main_x, flip_main_y, rot90_screen, rol90_screen, fast: boolean;
    rot180_screen, rot270_screen, pantalla_completa: boolean;
    clear: boolean;
    pause: boolean;
    mouse_x, mouse_y: single;
  end;

  def_dip_value = record
    dip_val: word;
    dip_name: string;
  end;

  def_dip = record
    mask: word;
    name: string;
    number: byte;
    dip: array [0 .. MAX_DIP_VALUES] of def_dip_value;
  end;

  def_dip2 = record
    mask: word;
    name: string;
    case number: byte of
      2:
        (val2: array [0 .. 1] of word; name2: array [0 .. 1] of string[30];);
      4:
        (val4: array [0 .. 3] of word; name4: array [0 .. 3] of string[30];);
      8:
        (val8: array [0 .. 7] of word; name8: array [0 .. 7] of string[30];);
      16:
        (val16: array [0 .. 15] of word; name16: array [0 .. 15] of string[30];);
      32:
        (val32: array [0 .. 31] of word; name32: array [0 .. 31] of string[30];);
  end;

  pdef_dip = ^def_dip;
  pdef_dip2 = ^def_dip2;
  TEmuStatus = (EsPause, EsRunning, EsStoped, EsClosed);

  // Video
procedure start_video(x, y: word; alpha: boolean = false);
procedure close_video;
procedure change_video;
procedure change_video_size(x, y: word);
procedure change_video_clock(fps: single);
procedure fullscreen;
procedure screen_init(num: byte; x, y: word; trans: boolean = false; final_mix: boolean = false; alpha: boolean = false);
procedure screen_mod_scroll(num: byte; long_x, max_x, mask_x, long_y, max_y, mask_y: word);
procedure screen_mod_sprites(num: byte; sprite_end_x, sprite_end_y, sprite_mask_x, sprite_mask_y: word);
procedure update_video;
procedure check_dimensions(x, y: word);
// Update final screen
procedure update_region(o_x1, o_y1, o_x2, o_y2: word; src_site: byte; d_x1, d_y1, d_x2, d_y2: word; dest_site: byte);
procedure update_final_piece(o_x1, o_y1, o_x2, o_y2: word; site: byte);
procedure flip_surface(pant: byte; flipx, flipy: boolean);
procedure video_sync;
// misc
procedure change_caption;
procedure show_info(visible: boolean; x, y: integer);
procedure reset_DSP_FM;
// Multidirs
function find_rom_multiple_dirs(rom_name: string): byte;
procedure split_dirs(dir: string);
function get_all_dirs: string;

procedure pause(sdl_event: TSDL_Event);
procedure pause_click;
procedure pause_action;

// game
procedure exit_game;

{$IFNDEF MSWINDOS}
// linux misc
procedure copymemory(dest, source: pointer; size: integer);
{$ENDIF}

var
  // video
  gscreen: array [0 .. max_screens] of PSDL_Surface;
  window_render: PSDL_Window;
  punbuf: pword;
  punbuf_alpha: pdword;
  main_screen: tmain_screen;
  dest: PSDL_Rect;
  // Misc
  machine_calls: TGLOBAL_CALLS;
  main_vars: TMAIN_VARS;
  Directory: TDirectory;
  cont_sincroniza: int64;
  cont_micro: int64;
  valor_sync: single;
  EmuStatus, EmuStatusTemp: TEmuStatus;
  // surface
  surfaceRect: TSDL_Rect;
  // Bezels
  bezel_surface, bezel_img_surface: PSDL_Surface;
  bezel_img_rect: TSDL_Rect;
  bezel_loading: boolean = false;
  bezel_texture: PSDL_Texture;
  bezel_renderer: PSDL_Renderer;
  // Pause
  pause_surface: PSDL_Surface;
  pause_fnt: PTTF_Font;
  pause_fnt_color: TSDL_Color;
  pause_fnt_texture: PSDL_Texture;
  pause_fnt_renderer: PSDL_Renderer;
  pause_fnt_rect: TSDL_Rect;
  EmulationPaused: boolean;
  pause_between: int64;
  start_gt, stop_gt, pause_ongt, pause_offgt: TTime;
  pause_offgt_stopped: boolean;
  // Sound Effects
  pause_sound, unpause_sound: PMix_Chunk;
  // Game start Info
  // Font
  fps_surface: PSDL_Surface;
  fps_fnt: PTTF_Font;
  fps_font_color, gui_font_color2: TSDL_Color;
  fps_texture: PSDL_Texture;
  fps_renderer: PSDL_Renderer;
  fps_m_rect, fps_t_rect: TSDL_Rect;
  // Scaled surface
  scaledSurface: PSDL_Surface;
  scaled_dimensions: integer;
        frame_main,frame_sub,frame_snd,frame_snd2,frame_mcu:single;
  // Basic memory...
  memory, mem_snd, mem_misc: array [0 .. $FFFF] of byte;
  // Game
  // GameName: String;

implementation

uses
  main,
  front_main,
  controls_engine,
  cpu_misc,
  tap_tzx,
  spectrum_misc,
  umain_config,
  emu_functions,
  uarcade_actions,
  f_arcade,
  configuration,
  multi_platform,
  umain_actions, uDataModule;

procedure exit_game;
begin
  frm_main.lbl_selected_info.Text := 'Just stop playing :';
  if ((@machine_calls.close <> nil) and main_vars.driver_ok) then
    machine_calls.close;
  main_vars.driver_ok := false;
  EmuStatus := EsStoped;
  main_engine.reset_DSP_FM;
  // main.frm_main.img_action_play.Bitmap.LoadFromFile
  // (config.main.prj_images_path.bar + 'play.png');
  // main.frm_main.img_action_play.Margins.Top := 5;
  // main.frm_main.img_action_play.Margins.Bottom := 5;
  // if config.main.prj_kind = KT_ListView then
  // frm_main.lv_main_list.Selected.Index := -1
  // else
  // front_action.stop_game_playing;
  stop_gt := now;
  if pause_offgt_stopped = false then
    pause_between := SecondsBetween(pause_ongt, stop_gt);
  main_actions.save_and_display_total_play_time(start_gt, stop_gt, pause_between);
//  emu_in_game.fps_show := false;
//  emu_in_game.fps_count := false;
  frm_main.tmr_fps.Enabled := false;
  SDL_DestroyWindow(window_render);
end;

procedure pause_click;
begin
  if EmulationPaused then
  begin
    EmulationPaused := false;
    Mix_PlayChannel(-1, pause_sound, 0);
    frm_main.tmr_pause.Enabled := false;
    SDL_PauseAudio(0);
    frm_main.lbl_selected_info.Text := 'Now playing :';
    pause_offgt_stopped := True;
    pause_offgt := now;
    pause_between := pause_between + SecondsBetween(pause_ongt, pause_offgt);
    Application.ProcessMessages;
  end
  else
  begin
    EmulationPaused := True;
    Mix_PlayChannel(-1, unpause_sound, 0);
    frm_main.tmr_pause.Enabled := True;
    SDL_PauseAudio(1);
    frm_main.lbl_selected_info.Text := 'Paused :';
    pause_offgt_stopped := false;
    pause_ongt := now;
    Application.ProcessMessages;
  end;
end;

procedure pause(sdl_event: TSDL_Event);
begin
  if (sdl_event.type_ = SDL_KEYDOWN) and (sdl_event.key.keysym.sym = SDLK_p) then
    pause_click;
end;

procedure pause_action;
var
  blackPixel: UInt32;
begin
  controls_pause;
  blackPixel := SDL_MapRGBA(gscreen[0]^.format, 0, 0, 0, 128);
  SDL_FillRect(gscreen[0], nil, blackPixel);
  pause_fnt_rect.w := pause_surface^.w;
//  if emu_in_game.pause = false then
//  begin
//    pause_fnt_rect.x := (SDL_GetWindowSurface(window_render).w div 2) - (pause_fnt_rect.w div 2);
//    frm_main.tmr_pause.Interval := 3000;
//  end
//  else
//  begin
//    pause_fnt_rect.x := -(pause_fnt_rect.w + 10);
//    frm_main.tmr_pause.Interval := 1000;
//  end;
//  pause_fnt_rect.h := pause_surface^.h;

  pause_fnt_rect.y := (SDL_GetWindowSurface(window_render).h div 2);
  SDL_BlitSurface(pause_surface, nil, SDL_GetWindowSurface(window_render), @pause_fnt_rect);
  SDL_UpdateWindowSurface(window_render);
end;

function find_rom_multiple_dirs(rom_name: string): byte;
var
  f, long: byte;
begin
  for f := 0 to $FF do
  begin
    if Directory.arcade_list_roms[f] = '' then
    begin
      long := f - 1;
      break;
    end;
  end;
  for f := 0 to long do
  begin
    if fileexists(Directory.arcade_list_roms[f] + rom_name) then
    begin
      find_rom_multiple_dirs := f;
      exit;
    end;
  end;
  // Not found
  find_rom_multiple_dirs := 0;
end;

function test_dir(cadena: string): string;
var
  f: word;
begin
  for f := length(cadena) downto 1 do
    if cadena[f] <> PathDelim then
      break;
  test_dir := System.copy(cadena, 1, f);
end;

procedure split_dirs(dir: string);
var
  f, long, old_pos: word;
  total: byte;
begin
  // Limpio todos los directorios
  for f := 0 to $FF do
    if Directory.arcade_list_roms[f] <> '' then
      Directory.arcade_list_roms[f] := '';
  // Check de vacio...
  if dir = '' then
  begin
    Directory.arcade_list_roms[0] := Directory.Base + 'roms' + PathDelim;
    exit;
  end;
  // Divido el directorio
  long := 1;
  total := 0;
  old_pos := 1;
  for f := 1 to length(dir) do
  begin
    if dir[f] = ';' then
    begin
      Directory.arcade_list_roms[total] := test_dir(copy(dir, old_pos, long - 1)) + PathDelim;
      long := 1;
      total := total + 1;
      old_pos := f + 1;
    end
    else
      long := long + 1;
  end;
  long := long - 1;
  // Comprobar si he llegado al final y debo pasarlo
  if long <> 0 then
    Directory.arcade_list_roms[total] := test_dir(copy(dir, old_pos, long)) + PathDelim;
end;

function get_all_dirs: string;
var
  f: byte;
  res: string;
begin
  res := '';
  for f := 0 to $FF do
  begin
    if Directory.arcade_list_roms[f] = '' then
      break;
    res := res + test_dir(Directory.arcade_list_roms[f]) + ';';
  end;
  get_all_dirs := res;
end;

procedure change_video;
var
  x, y: word;
  temps: single;
begin
  // Handle fullscreen or windowed mode setup
  if dm.tArcadeConfigfullscreen.AsInteger.ToBoolean then
  begin
    main_screen.video_mode := 5;
  end
  else
  begin
    // Calculate aspect ratio and destination size
    temps := p_final[0].x / p_final[0].y;
    dest.w := trunc(FULL_SCREEN_Y * temps);

    // If the width exceeds the full-screen limit, cap it
    if dest.w > FULL_SCREEN_X then
      dest.w := FULL_SCREEN_X;

    dest.h := FULL_SCREEN_Y;
    dest.x := (FULL_SCREEN_X - dest.w) shr 1;
    dest.y := 0;

    // Set mouse scaling factors based on the video mode
    case main_screen.video_mode of
      1, 3:
        begin
          main_screen.mouse_x := 1;
          main_screen.mouse_y := 1;
        end;
      2, 4:
        begin
          main_screen.mouse_x := 2;
          main_screen.mouse_y := 2;
        end;
      5:
        begin
          main_screen.mouse_x := 3;
          main_screen.mouse_y := 3;
        end;
      6:
        begin
          main_screen.mouse_x := dest.w / p_final[0].x;
          main_screen.mouse_y := FULL_SCREEN_Y / p_final[0].y;
        end;
    end;

    // Set the video mode based on window size configuration
    case dm.tArcadeConfigwin_size.AsInteger of
      0:
        main_screen.video_mode := 1;
      1:
        main_screen.video_mode := 2;
      2:
        main_screen.video_mode := 6;
    end;
  end;

  // Calculate the scaled resolution based on the multiplier
  x := p_final[0].x * mul_video;
  y := p_final[0].y * mul_video;

  // Update the window caption
  change_caption;

  // Handle screen surface memory management
  if gscreen[0] <> nil then
  begin
    SDL_FreeSurface(gscreen[0]);
  end;
  gscreen[0] := SDL_GetWindowSurface(window_render);

  // Update temporary surface (for rendering)
  if gscreen[PANT_TEMP] <> nil then
  begin
    SDL_FreeSurface(gscreen[PANT_TEMP]);
  end;
  gscreen[PANT_TEMP] := SDL_CreateRGBSurface(0, p_final[0].x, p_final[0].y, 16, 0, 0, 0, 0);

  // Handle double resolution surface
  if gscreen[PANT_DOBLE] <> nil then
  begin
    SDL_FreeSurface(gscreen[PANT_DOBLE]);
  end;
  gscreen[PANT_DOBLE] := SDL_CreateRGBSurface(0, x * 3, y * 3, 16, 0, 0, 0, 0);
end;

procedure check_dimensions(x, y: word);
begin
  if main_screen.rot90_screen or main_screen.rot270_screen then
  begin
    p_final[0].x := y;
    p_final[0].y := x;
  end
  else
  begin
    p_final[0].x := x;
    p_final[0].y := y;
  end;
end;

procedure change_video_size(x, y: word);
begin
  check_dimensions(x, y);
  change_video;
end;

procedure change_video_clock(fps: single);
begin
  valor_sync := (1 / fps) * cont_micro;
end;

procedure start_video(x, y: word; alpha: boolean = false);
var
  f: word;
  temp_width, temp_height: word;
  temp_x, temp_y: integer;
  window_flags: TSDL_WindowFlags;

begin
  // Allocate general pixel buffer memory
  getmem(punbuf, MAX_PUNBUF);

  // Set final screen dimensions based on rotation
  check_dimensions(x, y);

  // Destroy the previous window if it exists
  if window_render <> nil then
    SDL_DestroyWindow(window_render);

  // Arcade emulator video setup
  if dm.tConfigcurrent_emu.AsString = 'arcade' then
  begin
    if dm.tArcadeConfigfullscreen.AsInteger.ToBoolean then
    begin
      // Create fullscreen window
      window_render := SDL_CreateWindow('', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1920, 1080, SDL_WINDOW_SHOWN or SDL_WINDOW_OPENGL);
      if window_render = nil then
        exit; // Error handling, could log an error message here

      SDL_SetWindowFullscreen(window_render, SDL_WINDOW_FULLSCREEN_DESKTOP);
      fps_renderer := SDL_CreateRenderer(window_render, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
      fps_texture := SDL_CreateTextureFromSurface(fps_renderer, fps_surface);

      // Load bezel if configured
      if dm.tArcadeConfigbezels.AsInteger.ToBoolean then
        arcadeAction.loadBezel;
    end
    else
    begin
      // Create windowed mode window
      window_render := SDL_CreateWindow('', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, p_final[0].x, p_final[0].y, SDL_WINDOW_OPENGL or SDL_WINDOW_ALWAYS_ON_TOP);
      if window_render = nil then
        exit; // Error handling, could log an error message here

      // Adjust window size based on user configuration
      case dm.tArcadeConfigwin_size.AsInteger of
        0:
          begin
            temp_width := p_final[0].x;
            temp_height := p_final[0].y;
          end;
        1:
          begin
            SDL_SetWindowSize(window_render, p_final[0].x * 2, p_final[0].y * 2);
            temp_width := p_final[0].x * 2;
            temp_height := p_final[0].y * 2;
          end;
        2:
          begin
            SDL_SetWindowSize(window_render, p_final[0].x * 3, p_final[0].y * 3);
            temp_width := p_final[0].x * 3;
            temp_height := p_final[0].y * 3;
          end;
      end;

      // Center the window if configured
      if dm.tArcadeConfigwin_center.AsInteger.ToBoolean then
      begin
        temp_x := (1920 div 2) - (temp_width div 2);
        temp_y := (1080 div 2) - (temp_height div 2);
        SDL_SetWindowPosition(window_render, temp_x, temp_y);
      end;
    end;
  end;

  // Create pause screen text
  TTF_SetFontOutline(pause_fnt, 1);
  pause_surface := TTF_RenderText_Solid(pause_fnt, 'PAUSE', fps_font_color);
  pause_fnt_renderer := SDL_CreateRenderer(window_render, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  pause_fnt_texture := SDL_CreateTextureFromSurface(pause_fnt_renderer, pause_surface);

  // Change video settings
  change_video;

  // Create general screen buffer for temporary rendering
  gscreen[PANT_TEMP] := SDL_CreateRGBSurface(0, p_final[0].x, p_final[0].y, 16, 0, 0, 0, 0);

  // Create sprite screen buffer (with or without alpha)
  if alpha then
  begin
    gscreen[PANT_SPRITES_ALPHA] := SDL_CreateRGBSurface(0, MAX_PANT_SPRITES, MAX_PANT_SPRITES, 32, $FF, $FF00, $FF0000, $FF000000);
    getmem(punbuf_alpha, MAX_PUNBUF * 2);
  end;
  gscreen[PANT_SPRITES] := SDL_CreateRGBSurface(0, MAX_PANT_SPRITES, MAX_PANT_SPRITES, 16, 0, 0, 0, 0);
  SDL_SetColorKey(gscreen[PANT_SPRITES], SDL_TRUE, SET_TRANS_COLOR);

  // Set transparent color in palette
  paleta[MAX_COLORS] := SET_TRANS_COLOR;

  // Create additional screens and handle transparency if needed
  for f := 1 to MAX_PANT_VISIBLE do
  begin
    if p_final[f].x <> 0 then
    begin
      if p_final[f].final_mix then
      begin
        // Adjust sprite dimensions for final mix
        if p_final[f].sprite_end_x = 0 then
          p_final[f].sprite_end_x := p_final[f].x;
        if p_final[f].sprite_mask_x = 0 then
          p_final[f].sprite_mask_x := p_final[f].x - 1;
        if p_final[f].sprite_end_y = 0 then
          p_final[f].sprite_end_y := p_final[f].y;
        if p_final[f].sprite_mask_y = 0 then
          p_final[f].sprite_mask_y := p_final[f].y - 1;
        p_final[f].x := p_final[f].x + (ADD_SPRITE * 2);
        p_final[f].y := p_final[f].y + (ADD_SPRITE * 2);
      end;

      // Set scroll masks if not set
      if p_final[f].scroll.mask_x = 0 then
        p_final[f].scroll.mask_x := $FFFF;
      if p_final[f].scroll.mask_y = 0 then
        p_final[f].scroll.mask_y := $FFFF;

      // Create screen buffer for each visible layer
      if p_final[f].alpha then
        gscreen[f] := SDL_CreateRGBSurface(0, p_final[f].x, p_final[f].y, 32, $FF, $FF00, $FF0000, $FF000000)
      else
        gscreen[f] := SDL_CreateRGBSurface(0, p_final[f].x, p_final[f].y, 16, 0, 0, 0, 0);

      // Set color key for transparent layers
      if p_final[f].trans then
        SDL_SetColorKey(gscreen[f], SDL_TRUE, SET_TRANS_COLOR);
    end;
  end;

  // Disable FPS display by default
//  emu_in_game.fps_show := false;
//  emu_in_game.fps_count := false;
//  emu_in_game.fps_temp := '';
end;

// functions for video screen creation
procedure screen_init(num: byte; x, y: word; trans: boolean = false; final_mix: boolean = false; alpha: boolean = false);
begin
  p_final[num].x := x;
  p_final[num].y := y;
  p_final[num].trans := trans;
  p_final[num].final_mix := final_mix;
  p_final[num].alpha := alpha;
end;

procedure screen_mod_scroll(num: byte; long_x, max_x, mask_x, long_y, max_y, mask_y: word);
begin
  p_final[num].scroll.long_x := long_x;
  p_final[num].scroll.max_x := max_x;
  p_final[num].scroll.mask_x := mask_x;
  p_final[num].scroll.long_y := long_y;
  p_final[num].scroll.max_y := max_y;
  p_final[num].scroll.mask_y := mask_y;
end;

procedure screen_mod_sprites(num: byte; sprite_end_x, sprite_end_y, sprite_mask_x, sprite_mask_y: word);
begin
  p_final[num].sprite_end_x := sprite_end_x;
  p_final[num].sprite_end_y := sprite_end_y;
  p_final[num].sprite_mask_x := sprite_mask_x;
  p_final[num].sprite_mask_y := sprite_mask_y;
end;

procedure fullscreen;
var
  temp_height, temp_width: word;
  temp_x, temp_y: integer;
  display_mode: TSDL_DisplayMode;
  screen_width, screen_height: integer;
begin
  SDL_GetCurrentDisplayMode(0, @display_mode);
  screen_width := display_mode.w;
  screen_height := display_mode.h;

  if not(dm.tArcadeConfigfullscreen.AsInteger.ToBoolean) then
  begin
    SDL_DestroyWindow(window_render);
    window_render := SDL_CreateWindow('', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, screen_width, screen_height, SDL_WINDOW_SHOWN or SDL_WINDOW_OPENGL);
    SDL_SetWindowFullscreen(window_render, SDL_WINDOW_FULLSCREEN_DESKTOP);
    fps_renderer := SDL_CreateRenderer(window_render, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
    fps_texture := SDL_CreateTextureFromSurface(fps_renderer, fps_surface);
    main_screen.mouse_x := dest.w / p_final[0].x;
    main_screen.mouse_y := FULL_SCREEN_Y / p_final[0].y;

    if dm.tConfigcurrent_emu.AsString = 'arcade' then
    begin
      if dm.tArcadeConfigbezels.AsInteger.ToBoolean then
        arcadeAction.loadBezel;
    end;
    arcadeAction.setFullscreen;
    main_screen.video_mode := 6;
  end
  else
  begin
    SDL_DestroyWindow(window_render);
    window_render := SDL_CreateWindow('', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, p_final[0].x, p_final[0].y, SDL_WINDOW_SHOWN);

    case dm.tArcadeConfigwin_size.AsInteger of
      0:
        begin
          temp_width := p_final[0].x;
          temp_height := p_final[0].y;
          main_screen.video_mode := 1;
        end;
      1:
        begin
          SDL_SetWindowSize(window_render, p_final[0].x * 2, p_final[0].y * 2);
          temp_width := p_final[0].x * 2;
          temp_height := p_final[0].y * 2;
          main_screen.video_mode := 2;
        end;
      2:
        begin
          SDL_SetWindowSize(window_render, p_final[0].x * 3, p_final[0].y * 3);
          temp_width := p_final[0].x * 3;
          temp_height := p_final[0].y * 3;
          main_screen.video_mode := 6;
        end;
    else
      begin
        // Adjust window size based on user's screen resolution
        if (screen_width >= 3840) and (screen_height >= 2160) then // 4K
        begin
          SDL_SetWindowSize(window_render, p_final[0].x * 4, p_final[0].y * 4);
          temp_width := p_final[0].x * 4;
          temp_height := p_final[0].y * 4;
          main_screen.video_mode := 6;
        end
        else if (screen_width >= 2560) and (screen_height >= 1440) then // 2K
        begin
          SDL_SetWindowSize(window_render, p_final[0].x * 3, p_final[0].y * 3);
          temp_width := p_final[0].x * 3;
          temp_height := p_final[0].y * 3;
          main_screen.video_mode := 6;
        end
        else if (screen_width >= 1920) and (screen_height >= 1080) then // 1080p
        begin
          SDL_SetWindowSize(window_render, p_final[0].x * 2, p_final[0].y * 2);
          temp_width := p_final[0].x * 2;
          temp_height := p_final[0].y * 2;
          main_screen.video_mode := 2;
        end
        else
        begin
          temp_width := p_final[0].x;
          temp_height := p_final[0].y;
          main_screen.video_mode := 1;
        end;
      end;
    end;

    if dm.tConfigcurrent_emu.AsString = 'arcade' then
    begin
      if dm.tArcadeConfigwin_center.AsInteger.ToBoolean then
      begin
        temp_x := (screen_width div 2) - (temp_width div 2);
        temp_y := (screen_height div 2) - (temp_height div 2);
        SDL_SetWindowPosition(window_render, temp_x, temp_y);
      end;
      arcadeAction.setFullscreen;
    end;
    change_caption;
  end;

  main_screen.pantalla_completa := True;
  case main_vars.machine_type of
    0 .. 5:
      if (mouse.tipo = 0) then
        SDL_ShowCursor(0)
      else
        SDL_ShowCursor(1);
    182, 381:
      SDL_ShowCursor(1);
  else
    SDL_ShowCursor(0);
  end;

  main_screen.fullscreen := not main_screen.fullscreen;
  gscreen[0] := SDL_GetWindowSurface(window_render);
end;

procedure close_video;
var
  h: byte;
  f: word;
begin
  // Reset all GFX data
  for h := 0 to (MAX_GFX - 1) do
  begin
    if gfx[h].datos <> nil then
    begin
      freemem(gfx[h].datos); // Free GFX data memory
      gfx[h].datos := nil; // Ensure the pointer is reset to nil
    end;
  end;

  // Free all screen surfaces and reset the screen structures
  for f := 0 to max_screens do
  begin
    if gscreen[f] <> nil then
    begin
      SDL_FreeSurface(gscreen[f]); // Free the SDL surface
      gscreen[f] := nil; // Set the pointer to nil
    end;
    fillchar(p_final[f], sizeof(TSCREEN), 0); // Reset the p_final structure to zero
  end;

  // Free the general pixel buffer if it exists
  if punbuf <> nil then
  begin
    freemem(punbuf); // Free memory allocated for punbuf
    punbuf := nil; // Set pointer to nil after freeing
  end;

  // Free the alpha pixel buffer if it exists
  if punbuf_alpha <> nil then
  begin
    freemem(punbuf_alpha); // Free memory allocated for punbuf_alpha
    punbuf_alpha := nil; // Set pointer to nil after freeing
  end;
end;

procedure update_region(o_x1, o_y1, o_x2, o_y2: word; src_site: byte; d_x1, d_y1, d_x2, d_y2: word; dest_site: byte);
var
  origen, destino: TSDL_Rect;
begin
  // Ορισμός περιοχής προέλευσης
  origen.x := o_x1;
  origen.y := o_y1;
  origen.w := o_x2;
  origen.h := o_y2;

  // Ορισμός περιοχής προορισμού
  destino.x := d_x1;
  destino.y := d_y1;
  destino.w := d_x2;
  destino.h := d_y2;

  // Εάν το final_mix είναι ενεργοποιημένο για το προορισμό, προσαρμόζουμε τις συντεταγμένες
  if p_final[dest_site].final_mix then
  begin
    destino.x := destino.x + ADD_SPRITE;
    destino.y := destino.y + ADD_SPRITE;
  end;

  // Χρήση SDL_UpperBlit για την αντιγραφή της περιοχής από την πηγή στον προορισμό
  SDL_UpperBlit(gscreen[src_site], @origen, gscreen[dest_site], @destino);
end;

procedure update_final_piece(o_x1, o_y1, o_x2, o_y2: word; site: byte);
var
  origen, destino: TSDL_Rect;
  y, x: word;
  porig, pdest: pword;
  orig_p, dest_p: dword;
begin
  if main_screen.rot90_screen then
  begin
    // Μετακίνηση από την κανονική οθόνη στην τελική με περιστροφή 90 μοιρών
    orig_p := gscreen[site].pitch shr 1; // Αριθμός byte ανά γραμμή
    dest_p := gscreen[PANT_TEMP].pitch shr 1; // Αριθμός byte ανά γραμμή στην τελική οθόνη
    for y := 0 to (o_y2 - 1) do
    begin
      // Ορίστε τον δείκτη για τα pixel της πηγής
      porig := gscreen[site].pixels;
      inc(porig, ((y + o_y1 + ADD_SPRITE) * orig_p) + o_x1 + ADD_SPRITE);

      // Ορίστε τον δείκτη για τα pixel του προορισμού
      pdest := gscreen[PANT_TEMP].pixels;
      inc(pdest, dest_p - (y + 1));

      // Αντιγραφή pixel από την πηγή στον προορισμό με περιστροφή
      for x := 0 to (o_x2 - 1) do
      begin
        pdest^ := porig^; // Αντιγραφή pixel
        inc(porig); // Προχωρήστε στην επόμενη στήλη της πηγής
        inc(pdest, dest_p); // Προχωρήστε στην επόμενη γραμμή του προορισμού
      end;
    end;
  end
  else if main_screen.rot270_screen then
  begin
    // Μετακίνηση από την κανονική οθόνη στην τελική με περιστροφή 270 μοιρών
    orig_p := gscreen[site].pitch shr 1; // Αριθμός byte ανά γραμμή
    dest_p := gscreen[PANT_TEMP].pitch shr 1; // Αριθμός byte ανά γραμμή στην τελική οθόνη
    for y := 0 to (o_y2 - 1) do
    begin
      // Ορίστε τον δείκτη για τα pixel της πηγής
      porig := gscreen[site].pixels;
      inc(porig, ((y + o_y1 + ADD_SPRITE) * orig_p) + o_x1 + ADD_SPRITE);

      // Ορίστε τον δείκτη για τα pixel του προορισμού
      pdest := gscreen[PANT_TEMP].pixels;
      inc(pdest, (dest_p * (gscreen[PANT_TEMP].h - 1)) + y);

      // Αντιγραφή pixel από την πηγή στον προορισμό με περιστροφή
      for x := 0 to (o_x2 - 1) do
      begin
        pdest^ := porig^; // Αντιγραφή pixel
        inc(porig); // Προχωρήστε στην επόμενη στήλη της πηγής
        dec(pdest, dest_p); // Προχωρήστε στην επόμενη γραμμή του προορισμού (αντίθετη φορά)
      end;
    end;
  end
  else
  begin
    // Αντιγραφή χωρίς περιστροφή
    origen.x := o_x1 + ADD_SPRITE;
    origen.y := o_y1 + ADD_SPRITE;
    origen.w := o_x2;
    origen.h := o_y2;

    destino.x := 0;
    destino.y := 0;
    destino.w := gscreen[PANT_TEMP].w;
    destino.h := gscreen[PANT_TEMP].h;

    // Χρήση SDL_UpperBlit για αντιγραφή περιοχής χωρίς περιστροφή
    SDL_UpperBlit(gscreen[site], @origen, gscreen[PANT_TEMP], @destino);
  end;
end;

procedure flip_surface(pant: byte; flipx, flipy: boolean);
var
  f, i, h: word;
  punt, punt2: pword;
  origen: TSDL_Rect;
begin
  origen.x := 0;
  origen.y := 0;
  origen.w := p_final[pant].x;
  origen.h := p_final[pant].y;

  if flipx or flipy then
  begin
    h := 0;
    for i := (p_final[pant].y - 1) downto 0 do
    begin
      punt := gscreen[pant].pixels;
      inc(punt, (i * gscreen[pant].pitch) shr 1);

      if flipy then
      begin
        punt2 := gscreen[PANT_DOBLE].pixels;
        inc(punt2, (h * gscreen[PANT_DOBLE].pitch) shr 1);
        inc(h);
      end
      else
      begin
        punt2 := gscreen[PANT_DOBLE].pixels;
        inc(punt2, ((i * gscreen[PANT_DOBLE].pitch) shr 1));
      end;

      if flipx then
        inc(punt2, (p_final[pant].x - 1));

      for f := 0 to p_final[pant].x - 1 do
      begin
        punt2^ := punt^;
        inc(punt);
        if flipx then
          dec(punt2)
        else
          inc(punt2);
      end;
    end;

    SDL_UpperBlit(gscreen[PANT_DOBLE], @origen, gscreen[pant], @origen);
  end;
end;

procedure update_video;
var
  punt, punt2, punt3, punt4: pword;
  pant_final: word;
  origen, surfaceRect: TSDL_Rect;
  x, y, i, f, h, scaleWidth, scaleHeight, f_scale: integer;
  screen_width, screen_height: integer;
  display_mode: TSDL_DisplayMode;
   r:longint;

  procedure BlitFlippedY;
  var
    vi, vk: integer;
  begin
    h := 0;
    for vi := p_final[0].y - 1 downto 0 do
    begin
      punt := gscreen[PANT_TEMP].pixels;
      inc(punt, (vi * gscreen[PANT_TEMP].pitch) shr 1);
      punt2 := gscreen[PANT_DOBLE].pixels;
      inc(punt2, (h * gscreen[PANT_DOBLE].pitch) shr 1);
      inc(h);
      for vk := 0 to p_final[0].x - 1 do
      begin
        punt2^ := punt^;
        inc(punt);
        inc(punt2);
      end;
    end;
    origen.w := p_final[0].x;
    origen.h := p_final[0].y;
    SDL_UpperBlit(gscreen[PANT_DOBLE], @origen, gscreen[PANT_TEMP], @origen);
  end;

  procedure BlitFlippedX;
  var
    vi, vk: integer;
  begin
    for vi := 0 to p_final[0].y - 1 do
    begin
      punt := gscreen[PANT_TEMP].pixels;
      inc(punt, (vi * gscreen[PANT_TEMP].pitch) shr 1);
      punt2 := gscreen[PANT_DOBLE].pixels;
      inc(punt2, ((vi * gscreen[PANT_DOBLE].pitch) shr 1) + (p_final[0].x - 1));
      for vk := p_final[0].x - 1 downto 0 do
      begin
        punt2^ := punt^;
        inc(punt);
        dec(punt2);
      end;
    end;
    SDL_UpperBlit(gscreen[PANT_DOBLE], @origen, gscreen[PANT_TEMP], @origen);
  end;

  procedure BlitScaledWithScanlines(scaleFactor: integer; scanlineHeight: integer);
  var
    vi, vk: integer;
    punt, punt2: pword;
    srcPitch, destPitch: integer;
    srcWidth: integer;
    scaledLine: array of word;
    destLineStart: pword;
    isScanline: boolean;
  begin
    // Ορισμός των pitch για ευκολία
    srcPitch := gscreen[PANT_TEMP].pitch shr 1; // pitch του αρχικού buffer (διαίρεση με 2 επειδή χρησιμοποιούμε PWord)
    destPitch := gscreen[PANT_DOBLE].pitch shr 1; // pitch του τελικού buffer (διαίρεση με 2 για PWord)

    // Το πλάτος της αρχικής εικόνας σε pixels
    srcWidth := p_final[0].x;

    // Προετοιμασία του προσωρινού buffer για την κλιμακωμένη γραμμή
    SetLength(scaledLine, srcWidth * scaleFactor);

    // Βρόχος για κάθε γραμμή του αρχικού buffer
    for vi := 0 to p_final[0].y - 1 do
    begin
      // Ρύθμιση του δείκτη στην τρέχουσα γραμμή του αρχικού buffer
      punt := gscreen[PANT_TEMP].pixels;
      inc(punt, vi * srcPitch);

      // Δημιουργία της κλιμακωμένης γραμμής για το τρέχον επίπεδο
      for var vl := 0 to srcWidth - 1 do
      begin
        // Κάθε pixel αντιγράφεται scaleFactor φορές στην προσωρινή κλιμακωμένη γραμμή
        for var hf := 0 to scaleFactor - 1 do
        begin
          scaledLine[(vl * scaleFactor) + hf] := punt^;
        end;
        inc(punt);
      end;

      // Ρύθμιση του δείκτη στην αρχή του προορισμού και αντιγραφή της κλιμακωμένης γραμμής
      for vk := 0 to scaleFactor - 1 do
      begin
        destLineStart := gscreen[PANT_DOBLE].pixels;
        inc(destLineStart, ((vi * scaleFactor) + vk) * destPitch);

        // Ελέγχουμε αν αυτή είναι γραμμή που θα γίνει scanline
        isScanline := ((vi * scaleFactor) + vk) mod (scanlineHeight * 2) >= scanlineHeight;

        if isScanline then
        begin
          // Αν αυτή είναι γραμμή scanline, τη γεμίζουμε με μαύρο χρώμα (ή πιο σκοτεινό)
          FillWord(destLineStart, srcWidth * scaleFactor, $0000);
        end
        else
        begin
          // Αντιγραφή της κλιμακωμένης γραμμής στον προορισμό χρησιμοποιώντας Move
          Move(scaledLine[0], destLineStart^, length(scaledLine) * sizeof(word));
        end;
      end;
    end;

    // Ανανεώνονται οι διαστάσεις της εικόνας
    origen.w := p_final[0].x * scaleFactor;
    origen.h := p_final[0].y * scaleFactor;
    pant_final := PANT_DOBLE;
  end;

  procedure BlitScaled(scaleFactor: integer);
  var
    vi, vk: integer;
    punt, punt2: pword;
    srcPitch, destPitch: integer;
    srcWidth: integer;
    scaledLine: array of word;
    destLineStart: pword;
  begin
    // Ορισμός των pitch για ευκολία
    srcPitch := gscreen[PANT_TEMP].pitch shr 1; // pitch του αρχικού buffer (διαίρεση με 2 επειδή χρησιμοποιούμε PWord)
    destPitch := gscreen[PANT_DOBLE].pitch shr 1; // pitch του τελικού buffer (διαίρεση με 2 για PWord)

    // Το πλάτος της αρχικής εικόνας σε pixels
    srcWidth := p_final[0].x;

    // Προετοιμασία του προσωρινού buffer για την κλιμακωμένη γραμμή
    SetLength(scaledLine, srcWidth * scaleFactor);

    // Βρόχος για κάθε γραμμή του αρχικού buffer
    for vi := 0 to p_final[0].y - 1 do
    begin
      // Ρύθμιση του δείκτη στην τρέχουσα γραμμή του αρχικού buffer
      punt := gscreen[PANT_TEMP].pixels;
      inc(punt, vi * srcPitch);

      // Δημιουργία της κλιμακωμένης γραμμής για το τρέχον επίπεδο
      for var vl := 0 to srcWidth - 1 do
      begin
        // Κάθε pixel αντιγράφεται scaleFactor φορές στην προσωρινή κλιμακωμένη γραμμή
        for var hf := 0 to scaleFactor - 1 do
        begin
          scaledLine[(vl * scaleFactor) + hf] := punt^;
        end;
        inc(punt);
      end;

      // Ρύθμιση του δείκτη στην αρχή του προορισμού και αντιγραφή της κλιμακωμένης γραμμής
      for vk := 0 to scaleFactor - 1 do
      begin
        destLineStart := gscreen[PANT_DOBLE].pixels;
        inc(destLineStart, ((vi * scaleFactor) + vk) * destPitch);

        // Αντιγραφή της κλιμακωμένης γραμμής στον προορισμό χρησιμοποιώντας Move
        Move(scaledLine[0], destLineStart^, length(scaledLine) * sizeof(word));
      end;
    end;

    // Ανανεώνονται οι διαστάσεις της εικόνας
    origen.w := p_final[0].x * scaleFactor;
    origen.h := p_final[0].y * scaleFactor;
    pant_final := PANT_DOBLE;
  end;

  procedure BlitScaledPerfect(targetWidth, targetHeight: integer);
  var
    vi, vl, srcX, srcY, prevSrcY: integer;
    punt, destLineStart, srcPixelPtr: pword;
    srcPitch, destPitch, srcWidth, srcHeight: integer;
    scaleX, scaleY: single;
    xPosArray: array of integer; // Προκαταχωρημένες θέσεις για τα X pixels
  begin
    // Ορισμός των pitch για ευκολία
    srcPitch := gscreen[PANT_TEMP].pitch shr 1; // pitch του αρχικού buffer (PWord)
    destPitch := gscreen[PANT_DOBLE].pitch shr 1; // pitch του τελικού buffer (PWord)

    // Το πλάτος και το ύψος της αρχικής εικόνας σε pixels
    srcWidth := p_final[0].x;
    srcHeight := p_final[0].y;

    // Υπολογισμός των παραγόντων κλίμακας για πλάτος και ύψος
    scaleX := targetWidth / srcWidth;
    scaleY := targetHeight / srcHeight;

    // Προκαταχώρηση των θέσεων X για κάθε pixel στον προορισμό
    SetLength(xPosArray, targetWidth);
    for vl := 0 to targetWidth - 1 do
      xPosArray[vl] := trunc(vl / scaleX); // Υπολογίζουμε μία φορά τις θέσεις X για κάθε στήλη

    // Βρόχος για κάθε γραμμή του προορισμού
    prevSrcY := -1; // Για να ανιχνεύουμε αλλαγές γραμμής στο source
    for vi := 0 to targetHeight - 1 do
    begin
      // Υπολογισμός της τρέχουσας γραμμής στο αρχικό buffer
      srcY := trunc(vi / scaleY); // Βρίσκουμε την αντίστοιχη γραμμή στο αρχικό buffer

      // Αν η γραμμή δεν έχει αλλάξει, δεν χρειάζεται να ξαναφορτώσουμε το buffer
      if srcY <> prevSrcY then
      begin
        punt := gscreen[PANT_TEMP].pixels;
        inc(punt, srcY * srcPitch); // Μετακινούμε τον δείκτη στη σωστή γραμμή
        prevSrcY := srcY;
      end;

      // Ρύθμιση του δείκτη στον προορισμό για την τρέχουσα γραμμή
      destLineStart := gscreen[PANT_DOBLE].pixels;
      inc(destLineStart, vi * destPitch); // Προχωράμε τη θέση στον προορισμό για την τρέχουσα γραμμή

      // Βρόχος για κάθε pixel στην τρέχουσα γραμμή του προορισμού
      for vl := 0 to targetWidth - 1 do
      begin
        srcX := xPosArray[vl]; // Χρησιμοποιούμε την προκαταχωρημένη θέση X
        srcPixelPtr := punt;
        inc(srcPixelPtr, srcX); // Μετακινούμε τον δείκτη στη σωστή στήλη στο αρχικό buffer

        // Αντιγραφή του pixel από το αρχικό στο προορισμό
        destLineStart^ := srcPixelPtr^; // Αντιγραφή του pixel στη νέα θέση του προορισμού
        inc(destLineStart); // Προχωράμε στον επόμενο προορισμό pixel
      end;
    end;

    // Ανανεώνονται οι διαστάσεις της εικόνας
    origen.w := targetWidth;
    origen.h := targetHeight;
    pant_final := PANT_DOBLE;
  end;

  procedure BlitScaledWithoutAspectRatio(targetWidth, targetHeight: integer);
  var
    vi, vl, srcX, srcY: integer;
    punt, destLineStart, srcPixelPtr: pword;
    srcPitch, destPitch, srcWidth, srcHeight: integer;
    stepX, stepY: single;
    posX, posY: single;
  begin
    // Ορισμός pitch
    srcPitch := gscreen[PANT_TEMP].pitch shr 1;
    destPitch := gscreen[PANT_DOBLE].pitch shr 1;

    // Διαστάσεις αρχικής εικόνας
    srcWidth := p_final[0].x;
    srcHeight := p_final[0].y;

    // Υπολογισμός βημάτων (steps) για την κλίμακα χωρίς aspect ratio
    stepX := srcWidth / targetWidth;
    stepY := srcHeight / targetHeight;

    posY := 0.0; // Ξεκινάμε από το 0 για τη θέση Y

    // Βρόχος για κάθε γραμμή του προορισμού
    for vi := 0 to targetHeight - 1 do
    begin
      // Υπολογισμός της αντίστοιχης γραμμής στο αρχικό buffer
      srcY := trunc(posY); // Χρησιμοποιούμε τη θέση Y από την αρχή
      posY := posY + stepY; // Αυξάνουμε τη θέση Y

      punt := gscreen[PANT_TEMP].pixels;
      inc(punt, srcY * srcPitch); // Μετακινήσου στην αντίστοιχη γραμμή του αρχικού buffer

      destLineStart := gscreen[PANT_DOBLE].pixels;
      inc(destLineStart, vi * destPitch); // Μετακινήσου στη γραμμή του προορισμού

      posX := 0.0; // Ξεκινάμε από το 0 για τη θέση X

      // Βρόχος για κάθε pixel στην τρέχουσα γραμμή του προορισμού
      for vl := 0 to targetWidth - 1 do
      begin
        // Υπολογισμός της στήλης στο αρχικό buffer
        srcX := trunc(posX); // Χρησιμοποιούμε τη θέση X από την αρχή
        posX := posX + stepX; // Αυξάνουμε τη θέση X

        srcPixelPtr := punt;
        inc(srcPixelPtr, srcX); // Μετακινούμε τον δείκτη στη στήλη του αρχικού buffer

        // Αντιγραφή του pixel από το αρχικό στο προορισμό
        destLineStart^ := srcPixelPtr^; // Αντιγραφή pixel
        inc(destLineStart); // Προχωράμε στον επόμενο προορισμό pixel
      end;
    end;

    // Ανανεώνονται οι διαστάσεις της εικόνας
    origen.w := targetWidth;
    origen.h := targetHeight;
    pant_final := PANT_DOBLE;
  end;

begin
  SDL_GetCurrentDisplayMode(0, @display_mode);
  screen_width := display_mode.w;
  screen_height := display_mode.h;

  origen.x := 0;
  origen.y := 0;

  if main_screen.flip_main_y then
    BlitFlippedY
  else if main_screen.flip_main_x then
    BlitFlippedX;

  case main_screen.video_mode of
    0:
      exit;
    1:
      begin
        SDL_UpperBlit(gscreen[PANT_TEMP], @origen, gscreen[0], @origen);
        origen.w := gscreen[PANT_TEMP].w;
        origen.h := gscreen[PANT_TEMP].h;
        pant_final := PANT_TEMP;
      end;
    2:
      BlitScaled(2);
    3:
      BlitScaled(1);
    4:
      BlitScaled(2);
    5:
      begin
        scaleWidth := (screen_width div p_final[0].x);
        scaleHeight := (screen_height div p_final[0].y);
        if scaleWidth > scaleHeight then
          f_scale := scaleHeight
        else
          f_scale := scaleWidth;
        BlitScaledPerfect(bezel_rec.visible_area_x, bezel_rec.visible_area_y);
        // BlitScaled(f_scale);
        // BlitScaledWithScanlines(f_scale, 1);
      end;
    6:
      BlitScaled(3);
  end;

  if dm.tConfigcurrent_emu.AsString = 'arcade' then
    if dm.tArcadeConfigfullscreen.AsInteger.ToBoolean then
    begin
      if dm.tArcadeConfigbezels.AsInteger.ToBoolean then
      begin
        surfaceRect.x := (screen_width div 2) - (origen.w div 2);
        surfaceRect.y := (screen_height div 2) - (origen.h div 2);
        surfaceRect.w := origen.w;
        surfaceRect.h := origen.h;
        SDL_UpperBlit(gscreen[pant_final], @origen, gscreen[0], @surfaceRect);
        if bezel_loading then
        begin
          SDL_BlitSurface(bezel_img_surface, nil, bezel_surface, @bezel_img_rect);
        end;
        if EmuStatus = EsPause then
          SDL_BlitSurface(gscreen[0], nil, gscreen[0], @surfaceRect);
      end
      else
      begin
        surfaceRect.x := (screen_width div 2) - (origen.w div 2);
        surfaceRect.y := (screen_height div 2) - (origen.h div 2);
        surfaceRect.w := origen.w;
        surfaceRect.h := origen.h;
        SDL_UpperBlit(gscreen[pant_final], @origen, gscreen[0], @surfaceRect);
      end;
//      show_info(emu_in_game.fps_show, (surfaceRect.x + surfaceRect.w), surfaceRect.y);
    end
    else
    begin
      surfaceRect.x := origen.x;
      surfaceRect.y := origen.y;
      surfaceRect.w := origen.w;
      surfaceRect.h := origen.h;
      SDL_SetWindowTitle(window_render, PAnsiChar(AnsiString(machine_calls.caption)));
      SDL_UpperBlit(gscreen[pant_final], @origen, gscreen[0], @surfaceRect);
    end;
  SDL_UpdateWindowSurface(window_render);
end;

procedure show_info(visible: boolean; x, y: integer);
var
  ave: integer;
  displayMode: TSDL_DisplayMode;
  windowSize: TSDL_Point;
  displayBounds: TSDL_Rect;
  windowPosition: TSDL_Point;
  dstrect: TSDL_Rect;
begin
//  if visible and emu_in_game.fps_count then
//  begin
//    ave := trunc((main_vars.frames_sec * 100) / machine_calls.fps_max);
//    SDL_FreeSurface(fps_surface);
//    // Count Frames
//    TTF_SetFontOutline(fps_fnt, 1);
//    emu_in_game.fps_temp := 'FPS: ' + main_vars.frames_sec.ToString;
//    fps_surface := TTF_RenderText_Solid(fps_fnt, PAnsiChar(AnsiString(emu_in_game.fps_temp)), fps_font_color);
//    fps_t_rect.y := y;
//    fps_t_rect.w := fps_surface^.w;
//    fps_t_rect.h := fps_surface^.h;
//    fps_t_rect.x := x - fps_t_rect.w;
//    SDL_BlitSurface(fps_surface, nil, SDL_GetWindowSurface(window_render), @fps_t_rect);
//    main_vars.frames_sec := 0;
//    emu_in_game.fps_count := false;
//  end
//  else if visible and (emu_in_game.fps_count = false) then
//  begin
//    SDL_FreeSurface(fps_surface);
//    fps_surface := TTF_RenderText_Solid(fps_fnt, PAnsiChar(AnsiString(emu_in_game.fps_temp)), fps_font_color);
//    SDL_BlitSurface(fps_surface, nil, SDL_GetWindowSurface(window_render), @fps_t_rect);
//  end;
end;

procedure change_caption;
begin
  machine_calls.caption := front_action.gamename;
end;

procedure video_sync;
var
  l2: int64;
  res: single;
  LastFrameTime, CurrentFrameTime, ElapsedTime: UInt32;
begin
  update_video;
  controls_value;
  main_vars.frames_sec := main_vars.frames_sec + 1;
  if main_screen.fast then
    exit;
  QueryPerformanceCounter(l2);
  res := (l2 - cont_sincroniza);
  while (res < valor_sync) do
  begin
    QueryPerformanceCounter(l2);
    res := (l2 - cont_sincroniza);
  end;
  QueryPerformanceCounter(cont_sincroniza);
end;

{$IFNDEF windows}

procedure copymemory(dest, source: pointer; size: integer);
begin
  Move(source^, dest^, size);
end;
{$ENDIF}

procedure reset_DSP_FM;
begin
  fillchar(paleta[0], MAX_COLORS * 2, 0);
  fillchar(memory[0], $10000, 0);
  fillchar(mem_snd[0], $10000, 0);
  fillchar(buffer_paleta[0], MAX_COLORS * 2, 1);
  cpu_main_reset;
  machine_calls.cartridges := nil;
  machine_calls.tapes := nil;
  machine_calls.take_snapshot := nil;
  machine_calls.start := nil;
  machine_calls.reset := nil;
  machine_calls.close := nil;
  machine_calls.general_loop := nil;
  machine_calls.setup := nil;
  machine_calls.accept_config := nil;
  machine_calls.save_qsnap := nil;
  machine_calls.load_qsnap := nil;
  machine_calls.general_loop := nil;
  machine_calls.fps_max := 60;
  machine_calls.open_file := '';
  main_vars.current := 0;
  main_vars.mainmessage := '';
  main_vars.service1 := false;
  main_vars.frames_sec := 0;
  sound_status.used_channels := -1;
  main_screen.rot90_screen := false;
  main_screen.rot270_screen := false;
  main_screen.flip_main_screen := false;
  main_screen.flip_main_x := false;
  main_screen.flip_main_y := false;
  main_screen.fast := false;
  close_all_devices;
  cinta_tzx.tape_stop := nil;
  cinta_tzx.tape_start := nil;
  SDL_ShowCursor(0);
  timers.clear;
  marcade.dswa_val := nil;
  marcade.dswb_val := nil;
  marcade.dswc_val := nil;
  marcade.dswa_val2 := nil;
  marcade.dswb_val2 := nil;
  marcade.dswc_val2 := nil;
  cont_sincroniza := sdl_getticks();
  close_audio;
  close_video;
end;

end.
