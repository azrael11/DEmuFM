unit controls_engine;

interface

uses
  sdl2,
  main_engine,
  timer_engine,
  sound_engine,
  WinApi.Windows,
  System.SysUtils;

const
  NUM_PLAYERS = 3;
  NUM_JOYSTICS = 10;
  KEYBOARD_A = 4;
  KEYBOARD_B = 5;
  KEYBOARD_C = 6;
  KEYBOARD_D = 7;
  KEYBOARD_E = 8;
  KEYBOARD_F = 9;
  KEYBOARD_G = 10;
  KEYBOARD_H = 11;
  KEYBOARD_I = 12;
  KEYBOARD_J = 13;
  KEYBOARD_K = 14;
  KEYBOARD_L = 15;
  KEYBOARD_M = 16;
  KEYBOARD_N = 17;
  KEYBOARD_O = 18;
  KEYBOARD_P = 19;
  KEYBOARD_Q = 20;
  KEYBOARD_R = 21;
  KEYBOARD_S = 22;
  KEYBOARD_T = 23;
  KEYBOARD_U = 24;
  KEYBOARD_V = 25;
  KEYBOARD_W = 26;
  KEYBOARD_X = 27;
  KEYBOARD_Y = 28;
  KEYBOARD_Z = 29;
  KEYBOARD_1 = 30;
  KEYBOARD_2 = 31;
  KEYBOARD_3 = 32;
  KEYBOARD_4 = 33;
  KEYBOARD_5 = 34;
  KEYBOARD_6 = 35;
  KEYBOARD_7 = 36;
  KEYBOARD_8 = 37;
  KEYBOARD_9 = 38;
  KEYBOARD_0 = 39;
  //
  KEYBOARD_RETURN = 40;
  KEYBOARD_ESCAPE = 41;
  KEYBOARD_BACKSPACE = 42;
  KEYBOARD_TAB = 43;
  KEYBOARD_SPACE = 44;
  KEYBOARD_HOME = 74;
  KEYBOARD_END = 77;
  KEYBOARD_CAPSLOCK = 57;
  KEYBOARD_AVPAG = 78;
  // Modificadores
  KEYBOARD_LCTRL = 224;
  KEYBOARD_LSHIFT = 225;
  KEYBOARD_LALT = 226;
  KEYBOARD_LWIN = 227;
  KEYBOARD_RCTRL = 228;
  KEYBOARD_RSHIFT = 229;
  KEYBOARD_RALT = 230;
  KEYBOARD_RWIN = 231;
  // Cursor
  KEYBOARD_RIGHT = 79;
  KEYBOARD_LEFT = 80;
  KEYBOARD_DOWN = 81;
  KEYBOARD_UP = 82;
  // Rest of keys
  KEYBOARD_ROW0_T0 = 53;
  KEYBOARD_ROW0_T1 = 45;
  KEYBOARD_ROW0_T2 = 46;
  KEYBOARD_ROW1_T1 = 47;
  KEYBOARD_ROW1_T2 = 48;
  KEYBOARD_ROW2_T1 = 51;
  KEYBOARD_ROW2_T2 = 52;
  KEYBOARD_ROW2_T3 = 49;
  KEYBOARD_ROW3_T0 = 100;
  KEYBOARD_ROW3_T1 = 54;
  KEYBOARD_ROW3_T2 = 55;
  KEYBOARD_ROW3_T3 = 56;
  // Funcion
  KEYBOARD_F1 = 58;
  KEYBOARD_F2 = 59;
  KEYBOARD_F3 = 60;
  KEYBOARD_F4 = 61;
  KEYBOARD_F5 = 62;
  KEYBOARD_F6 = 63;
  KEYBOARD_F7 = 64;
  KEYBOARD_F8 = 65;
  KEYBOARD_F9 = 66;
  KEYBOARD_F10 = 67;
  KEYBOARD_F11 = 68;
  KEYBOARD_F12 = 69;
  // Teclado Numerico
  KEYBOARD_NRETURN = 88;
  KEYBOARD_N1 = 89;
  KEYBOARD_N2 = 90;
  KEYBOARD_N3 = 91;
  KEYBOARD_N4 = 92;
  KEYBOARD_N5 = 93;
  KEYBOARD_N6 = 94;
  KEYBOARD_N7 = 95;
  KEYBOARD_N8 = 96;
  KEYBOARD_N9 = 97;
  KEYBOARD_N0 = 98;
  KEYBOARD_NDOT = 99;
  // Reservada la ultima para indicar que no hay tecla
  KEYBOARD_NONE = 255;

type
  TMODE_STATE = (MS_Test, MS_Config);

type
  TMODE_STATE_BUTTON = (MSB_Ready, MSB_Wait);

type
  def_mouse = record
    x, y: word;
    button1, button2: boolean;
  end;

  def_event = record
    mouse, keyboard, arcade: boolean;
    emouse, ejoystick, ekeyboard, earcade: boolean;
    num_joysticks: byte;
  end;

  def_ingame_actions = record
    name: string;
    leave_game: word;
    leave_game_col_name: string;
    pause_game: word;
    pause_game_col_name: string;
    fullscreen_game: word;
    fullscreen_game_col_name: string;
    service: word;
    service_col_name: string;
    fastest: word;
    fastest_col_name: string;
    slow: word;
    slow_col_name: string;
    reset: word;
    reset_col_name: string;
    save_state_player_1: word;
    save_state_player_1_col_name: string;
    save_state_player_2: word;
    save_state_player_2_col_name: string;
    load_state_player_1: word;
    load_state_player_1_col_name: string;
    load_state_player_2: word;
    load_state_player_2_col_name: string;
    snapshot: word;
    snapshot_col_name: string;
    show_info: word;
    show_info_col_name: string;
  end;

  def_arcade = record
    // info keyboard, joystick, gamepad
    profile, name, platform_type, player, serial, vendor, guid, product, product_version, control_type,
      buttons: array [0 .. NUM_PLAYERS] of string;
    // Boolean state of actions
    coin, start, up, down, left, right, but0, but1, but2, but3, but4, but5,
      use_key: array [0 .. NUM_PLAYERS] of boolean;
    // Keyboard keys
    ncoin, nstart, nup, ndown, nleft, nright, nbut0, nbut1, nbut2, nbut3, nbut4,
      nbut5: array [0 .. NUM_PLAYERS] of byte;
    // Joystick keys
    deadzone_x, deadzone_y: array [0 .. NUM_PLAYERS] of integer;
    joy_up, joy_down, joy_left, joy_right, joy_coin, joy_start: array [0 .. NUM_PLAYERS] of integer;
    jbut0, jbut1, jbut2, jbut3, jbut4, jbut5, jbut6, jbut7, jbut8, jbut9, jbut10, jbut11, jbut12, jbut13,
      jbut14, jbut15, num_joystick: array [0 .. NUM_PLAYERS] of byte;
    // Game Controller keys
    gc_up, gc_down, gc_left, gc_right, gc_coin, gc_start: array [0 .. NUM_PLAYERS] of integer;
    gcbut0, gcbut1, gcbut2, gcbut3, gcbut4, gcbut5, num_gc: array [0 .. NUM_PLAYERS] of byte;
    // Strings to get and set events actions
    ncoin_col_name, nstart_col_name, nup_col_name, ndown_col_name, nleft_col_name, nright_col_name,
      nbut0_col_name, nbut1_col_name, nbut2_col_name, nbut3_col_name, nbut4_col_name, nbut5_col_name,
      jbut0_col_name, jbut1_col_name, jbut2_col_name, jbut3_col_name, jbut4_col_name, jbut5_col_name,
      num_joystick_col_name, joy_up_col_name, joy_down_col_name, joy_left_col_name, joy_right_col_name,
      joy_coin_col_name, joy_start_col_name: array [0 .. NUM_PLAYERS] of string;
  end;

  def_marcade = record
    in0, in1, in2, in3, in4: word;
    dswa, dswb, dswc: word;
    dswa_val, dswb_val, dswc_val: pdef_dip;
  end;

  def_analog_control = record
    x, y: array [0 .. NUM_PLAYERS] of integer;
    val: array [0 .. NUM_PLAYERS] of integer;
    delta, mid_val, max_val, min_val: integer;
    return_center: boolean;
    circle: boolean;
    inverted_x, inverted_y: boolean;
  end;

  def_analog = record
    c: array [0 .. 4] of def_analog_control;
    cpu: byte;
    clock: dword;
  end;

type
  TP_CONTROLS = record
    profile: string;
    name: string;
    controller: string;
    player: string;
    platform_type: string;
    map_arcade: def_arcade;
  end;

type
  TKEYBOARD_TEMP_KEYS = record
    up, down, left, right: word;
    b0, b1, b2, b3, b4, b5: word;
    start: word;
    coin: word;
  end;

type
  TTEMP_KEY = record
    profile: string;
    player: string;
    platform_type: string;
    key: TKEYBOARD_TEMP_KEYS;
  end;

type
  TJOYSTICK_TEMP_KEYS = record
    up, down, left, right: integer;
    b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15: integer;
    start: integer;
    coin: integer;
    deadzone_x, deadzone_y: integer;
  end;

type
  TTEMP_JOY = record
    profile: string;
    player: string;
    name: string;
    vtype: string;
    serial: string;
    buttons: string;
    guid: string;
    vendor: string;
    product: string;
    product_version: string;
    joy: TJOYSTICK_TEMP_KEYS;
  end;

type
  TGAMEPAD_TEMP_KEYS = record
    hat_up, hat_down, hat_left, hat_right: integer;
    b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15: integer;
    trigger_left, trigger_right: integer;
    left, right: integer;
    lx_plus, lx_minus, ly_plus, ly_minus, rx_plus, rx_minus, ry_plus, ry_minus: integer;
    left_deadzone, right_deadzone: integer;
    start: integer;
    coin: integer;
  end;

type
  TTEMP_GPD = record
    profile: string;
    player: string;
    name: string;
    vtype: string;
    serial: string;
    buttons: string;
    guid: string;
    vendor: string;
    product: string;
    product_version: string;
    gpd: TGAMEPAD_TEMP_KEYS;
  end;

var
  temp_key: array [0 .. NUM_PLAYERS] of TTEMP_KEY;
  temp_joy: array [0 .. NUM_PLAYERS] of TTEMP_JOY;
  temp_gpd: array [0 .. NUM_PLAYERS] of TTEMP_GPD;

  active_controllers: array [0 .. 2] of array [0 .. NUM_PLAYERS] of boolean;

  p_contrls: TP_CONTROLS;
  map_ingame_actions: def_ingame_actions;
  num_state: byte;

  keyboard: array [0 .. 255] of boolean;
  marcade: def_marcade;
  event: def_event;
  analog: def_analog;
  mouse_def: def_mouse;
  joystick_def: array [0 .. NUM_PLAYERS] of PSDL_Joystick;
  game_controller_def: array [0 .. NUM_PLAYERS] of PSDL_GameController;

procedure controls_value;
procedure controls_pause;
procedure init_controls(evaluate_mouse, evaluate_keyboard, evaluate_joystick, evaluate_arcade: boolean);
procedure open_joystick(player: byte);
procedure close_joystick(num: byte);
// Mouse cursor
procedure show_mouse_cursor;
procedure hide_mouse_cursor;
// Analog
procedure init_analog(cpu: byte; cpu_clock: integer);
procedure analog_0(sensitivity, port_delta, mid_val, max_val, min_val: integer; return_center: boolean;
  circle: boolean = false; inverted_x: boolean = false; inverted_y: boolean = false);
procedure analog_1(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
procedure analog_2(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
procedure analog_3(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
procedure analog_4(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);

implementation

uses
  main,
  umain_actions,
  umain_config,
  f_dspfm,
  uarcade_actions,
  System.UITypes,
  front_main;

var
  keystate: pbyte = nil;

procedure show_mouse_cursor;
begin
  //
end;

procedure hide_mouse_cursor;
begin
  //
end;

procedure open_joystick(player: byte);
begin
  // joystick_def[0] := SDL_JoystickOpen(p_contrls[0].map_arcade.num_joystick[2]);
  joystick_def[0] := SDL_JoystickOpen(2);
end;

procedure close_joystick(num: byte);
begin
  SDL_JoystickClose(joystick_def[0]);
  joystick_def[0] := nil;
end;

procedure init_controls(evaluate_mouse, evaluate_keyboard, evaluate_joystick, evaluate_arcade: boolean);
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS do
  begin
    p_contrls.map_arcade.up[f] := false;
    p_contrls.map_arcade.down[f] := false;
    p_contrls.map_arcade.left[f] := false;
    p_contrls.map_arcade.right[f] := false;
    p_contrls.map_arcade.but0[f] := false;
    p_contrls.map_arcade.but1[f] := false;
    p_contrls.map_arcade.but2[f] := false;
    p_contrls.map_arcade.but3[f] := false;
    p_contrls.map_arcade.but4[f] := false;
    p_contrls.map_arcade.but5[f] := false;
    p_contrls.map_arcade.coin[f] := false;
    p_contrls.map_arcade.start[f] := false;
  end;
  mouse_def.button1 := false;
  mouse_def.button2 := false;
  mouse_def.x := 0;
  mouse_def.y := 0;
  event.emouse := evaluate_mouse;
  event.ekeyboard := evaluate_keyboard;
  event.ejoystick := evaluate_joystick;
  event.earcade := evaluate_arcade;
  event.num_joysticks := SDL_NumJoysticks;

  if keystate = nil then
    keystate := pbyte(SDL_GetKeyboardState(nil));

  event.mouse := false;
  event.arcade := false;
  event.keyboard := false;
  fillchar(keyboard[0], 256, 0);
  // for f := 0 to NUM_PLAYERS do
  // if joystick_def[f] <> nil then
  // close_joystick(p_contrls.map_arcade.num_joystick[f]);
  // for f := 0 to NUM_PLAYERS do
  // open_joystick(0);
  // for f := 0 to NUM_PLAYERS do
  // if joystick_def[f] = nil then
  // if (joystick_def[f] = nil) and (p_contrls.controller = 'joystick') then
  // p_contrls.map_arcade.use_key[0] := true;
end;

procedure evaluate_arcade_keyb(player: byte);
begin
  // coin
  if p_contrls.map_arcade.coin[player] <> keyboard[p_contrls.map_arcade.ncoin[player]] then
  begin
    p_contrls.map_arcade.coin[player] := keyboard[p_contrls.map_arcade.ncoin[player]];
    event.arcade := true;
  end;
  // start
  if p_contrls.map_arcade.start[player] <> keyboard[p_contrls.map_arcade.nstart[player]] then
  begin
    p_contrls.map_arcade.start[player] := keyboard[p_contrls.map_arcade.nstart[0]];
    event.arcade := true;
  end;
  // up
  if (p_contrls.map_arcade.up[player] <> (keyboard[p_contrls.map_arcade.nup[player]])) then
  begin
    p_contrls.map_arcade.up[player] := keyboard[p_contrls.map_arcade.nup[player]];
    event.arcade := true;
  end;
  // down
  if (p_contrls.map_arcade.down[player] <> (keyboard[p_contrls.map_arcade.ndown[player]])) then
  begin
    p_contrls.map_arcade.down[player] := keyboard[p_contrls.map_arcade.ndown[player]];
    event.arcade := true;
  end;
  // if (p_contrls.map_arcade.up[player] and p_contrls.map_arcade.down[player]) then
  // p_contrls.map_arcade.up[player] := false;
  // left
  if (p_contrls.map_arcade.left[player] <> (keyboard[p_contrls.map_arcade.nleft[player]])) then
  begin
    p_contrls.map_arcade.left[player] := keyboard[p_contrls.map_arcade.nleft[player]];
    event.arcade := true;
  end;
  // right
  if (p_contrls.map_arcade.right[player] <> (keyboard[p_contrls.map_arcade.nright[player]])) then
  begin
    p_contrls.map_arcade.right[player] := keyboard[p_contrls.map_arcade.nright[player]];
    event.arcade := true;
  end;
  // if (p_contrls.map_arcade.left[player] and p_contrls.map_arcade.right[player]) then
  // p_contrls.map_arcade.left[player] := false;

  // Buttons
  if timers.autofire_enabled[0 + (player * 6)] then
  begin
    timers.autofire_status[0 + (player * 6)] := keyboard[p_contrls.map_arcade.nbut0[player]];
  end
  else
  begin
    if (p_contrls.map_arcade.but0[player] <> (keyboard[p_contrls.map_arcade.nbut0[player]])) then
    begin
      p_contrls.map_arcade.but0[player] := keyboard[p_contrls.map_arcade.nbut0[player]];
      event.arcade := true;
    end;
  end;
  if timers.autofire_enabled[1 + (player * 6)] then
  begin
    timers.autofire_status[1 + (player * 6)] := keyboard[p_contrls.map_arcade.nbut1[player]];
  end
  else
  begin
    if (p_contrls.map_arcade.but1[player] <> (keyboard[p_contrls.map_arcade.nbut1[player]])) then
    begin
      p_contrls.map_arcade.but1[player] := keyboard[p_contrls.map_arcade.nbut1[player]];
      event.arcade := true;
    end;
  end;
  if timers.autofire_enabled[2 + (player * 6)] then
  begin
    timers.autofire_status[2 + (player * 6)] := keyboard[p_contrls.map_arcade.nbut2[player]];
  end
  else
  begin
    if (p_contrls.map_arcade.but2[player] <> (keyboard[p_contrls.map_arcade.nbut2[player]])) then
    begin
      p_contrls.map_arcade.but2[player] := keyboard[p_contrls.map_arcade.nbut2[player]];
      event.arcade := true;
    end;
  end;
  if timers.autofire_enabled[3 + (player * 6)] then
  begin
    timers.autofire_status[3 + (player * 6)] := keyboard[p_contrls.map_arcade.nbut3[player]];
  end
  else
  begin
    if (p_contrls.map_arcade.but3[player] <> (keyboard[p_contrls.map_arcade.nbut3[player]])) then
    begin
      p_contrls.map_arcade.but3[player] := keyboard[p_contrls.map_arcade.nbut3[player]];
      event.arcade := true;
    end;
  end;
  if timers.autofire_enabled[4 + (player * 6)] then
  begin
    timers.autofire_status[4 + (player * 6)] := keyboard[p_contrls.map_arcade.nbut4[player]];
  end
  else
  begin
    if (p_contrls.map_arcade.but4[player] <> (keyboard[p_contrls.map_arcade.nbut4[player]])) then
    begin
      p_contrls.map_arcade.but4[player] := keyboard[p_contrls.map_arcade.nbut4[player]];
      event.arcade := true;
    end;
  end;
  if timers.autofire_enabled[5 + (player * 6)] then
  begin
    timers.autofire_status[5 + (player * 6)] := keyboard[p_contrls.map_arcade.nbut5[player]];
  end
  else
  begin
    if (p_contrls.map_arcade.but5[player] <> (keyboard[p_contrls.map_arcade.nbut5[player]])) then
    begin
      p_contrls.map_arcade.but5[player] := keyboard[p_contrls.map_arcade.nbut5[player]];
      event.arcade := true;
    end;
  end;
end;

procedure evaluate_arcade_joy(tevent: TSDL_Event; player: byte);
var
  axis_num: integer;
  tempb: boolean;
begin
  if event.num_joysticks = 0 then
    exit;
  case tevent.type_ of
    SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP:
      begin
        // System
        tempb := SDL_JoystickGetButton(joystick_def[0], p_contrls.map_arcade.joy_coin[0]) <> 0;
        if (p_contrls.map_arcade.coin[0] <> tempb) then
        begin
          p_contrls.map_arcade.coin[player] := tempb;
          event.arcade := true;
        end;
        tempb := SDL_JoystickGetButton(joystick_def[0], p_contrls.map_arcade.joy_start[0]) <> 0;
        if (p_contrls.map_arcade.start[player] <> tempb) then
        begin
          p_contrls.map_arcade.start[player] := tempb;
          event.arcade := true;
        end;
        if timers.autofire_enabled[0 + (player * 6)] then
        begin
          timers.autofire_status[0 + (player * 6)] := SDL_JoystickGetButton(joystick_def[player],
            p_contrls.map_arcade.jbut0[player]) <> 0;
        end
        else
        begin
          tempb := SDL_JoystickGetButton(joystick_def[player], p_contrls.map_arcade.jbut0[player]) <> 0;
          if p_contrls.map_arcade.but0[player] <> tempb then
          begin
            p_contrls.map_arcade.but0[player] := tempb;
            event.arcade := true;
          end;
        end;
        if timers.autofire_enabled[1 + (player * 6)] then
        begin
          timers.autofire_status[1 + (player * 6)] := SDL_JoystickGetButton(joystick_def[player],
            p_contrls.map_arcade.jbut1[player]) <> 0;
        end
        else
        begin
          tempb := SDL_JoystickGetButton(joystick_def[player], p_contrls.map_arcade.jbut1[player]) <> 0;
          if p_contrls.map_arcade.but1[player] <> tempb then
          begin
            p_contrls.map_arcade.but1[player] := tempb;
            event.arcade := true;
          end;
        end;
        if timers.autofire_enabled[2 + (player * 6)] then
        begin
          timers.autofire_status[2 + (player * 6)] := SDL_JoystickGetButton(joystick_def[player],
            p_contrls.map_arcade.jbut2[player]) <> 0;
        end
        else
        begin
          tempb := SDL_JoystickGetButton(joystick_def[player], p_contrls.map_arcade.jbut2[player]) <> 0;
          if p_contrls.map_arcade.but2[player] <> tempb then
          begin
            p_contrls.map_arcade.but2[player] := tempb;
            event.arcade := true;
          end;
          event.arcade := true;
        end;
        if timers.autofire_enabled[3 + (player * 6)] then
        begin
          timers.autofire_status[3 + (player * 6)] := SDL_JoystickGetButton(joystick_def[player],
            p_contrls.map_arcade.jbut3[player]) <> 0;
        end
        else
        begin
          tempb := SDL_JoystickGetButton(joystick_def[player], p_contrls.map_arcade.jbut3[player]) <> 0;
          if p_contrls.map_arcade.but3[player] <> tempb then
          begin
            p_contrls.map_arcade.but3[player] := tempb;
            event.arcade := true;
          end;
          event.arcade := true;
        end;
        if timers.autofire_enabled[4 + (player * 6)] then
        begin
          timers.autofire_status[4 + (player * 6)] := SDL_JoystickGetButton(joystick_def[player],
            p_contrls.map_arcade.jbut4[player]) <> 0;
        end
        else
        begin
          tempb := SDL_JoystickGetButton(joystick_def[player], p_contrls.map_arcade.jbut4[player]) <> 0;
          if p_contrls.map_arcade.but4[player] <> tempb then
          begin
            p_contrls.map_arcade.but4[player] := tempb;
            event.arcade := true;
          end;
          event.arcade := true;
        end;
        if timers.autofire_enabled[5 + (player * 6)] then
        begin
          timers.autofire_status[5 + (player * 6)] := SDL_JoystickGetButton(joystick_def[player],
            p_contrls.map_arcade.jbut5[player]) <> 0;
        end
        else
        begin
          tempb := SDL_JoystickGetButton(joystick_def[player], p_contrls.map_arcade.jbut5[player]) <> 0;
          if p_contrls.map_arcade.but5[player] <> tempb then
          begin
            p_contrls.map_arcade.but5[player] := tempb;
            event.arcade := true;
          end;
          event.arcade := true;
        end;
      end;
    SDL_JOYAXISMOTION:
      begin
        if tevent.jaxis.axis in [0, 2, 4, 6] then
        begin
          if (tevent.jaxis.value > 8000) or (tevent.jaxis.value < -8000) then
          begin
            if tevent.jaxis.value > 8000 then
              p_contrls.map_arcade.left[0] := true
            else if tevent.jaxis.value < -8000 then
              p_contrls.map_arcade.right[0] := true;
            event.arcade := true;
          end
        end
        else if tevent.jaxis.axis in [1, 3, 5, 7] then
        begin
          if (tevent.jaxis.value > 8000) or (tevent.jaxis.value < -8000) then
          begin
            if tevent.jaxis.value > 8000 then
              p_contrls.map_arcade.up[0] := true
            else if tevent.jaxis.value < -8000 then
              p_contrls.map_arcade.down[0] := true;
            event.arcade := true;
          end
        end;
      end;
  end;
end;

procedure evaluate_mouse(tevent: TSDL_Event);
var
  sc_mul: byte;
  function video_mult: byte;
  begin
    case main_screen.video_mode of
      2, 4, 6:
        video_mult := 2;
      5:
        video_mult := 3;
    else
      video_mult := 1;
    end;
  end;

begin
  case tevent.type_ of
    SDL_MOUSEMOTION:
      begin // Movimiento
        sc_mul := video_mult;
        mouse_def.x := tevent.motion.x div sc_mul;
        mouse_def.y := tevent.motion.y div sc_mul;
        event.mouse := true;
      end;
    SDL_MOUSEBUTTONUP:
      begin // Levantar boton
        event.mouse := true;
        case tevent.button.button of
          SDL_BUTTON_LEFT:
            mouse_def.button1 := false;
          SDL_BUTTON_RIGHT:
            mouse_def.button2 := false;
        end;
      end;
    SDL_MOUSEBUTTONDOWN:
      begin
        event.mouse := true;
        case tevent.button.button of
          SDL_BUTTON_LEFT:
            mouse_def.button1 := true;
          SDL_BUTTON_RIGHT:
            mouse_def.button2 := true;
        end;
      end;
  end;
end;

procedure ingame_pause(tevent: TSDL_Event);
begin
  if keyboard[map_ingame_actions.pause_game] then
    pause(tevent);
end;

procedure evaluate_frontend_or_ingame_keyboard(tevent: TSDL_Event);
begin
  if keyboard[map_ingame_actions.leave_game] then
    exit_game;
  if keyboard[map_ingame_actions.pause_game] then
    pause(tevent);
  if keyboard[map_ingame_actions.service] then
    main_vars.service1 := not(main_vars.service1);
  if keyboard[map_ingame_actions.fastest] then
    main_actions.main_form_full_fps;
  if keyboard[map_ingame_actions.reset] then
    main_actions.main_form_reset_game;
  if (keyboard[map_ingame_actions.snapshot] and not(main_screen.fullscreen)) then
  begin
    if @machine_calls.take_snapshot <> nil then
      machine_calls.take_snapshot;
    keystate[map_ingame_actions.snapshot] := 0;
    // Cuando tiene que poner a 0 la tecla esta en el menu... Tengo que ponerla a 0 yo o cree que esta todo el rato pulsada!
  end;
  if keyboard[map_ingame_actions.fullscreen_game] then
    fullscreen;
  if keyboard[map_ingame_actions.save_state_player_1] then
    if @machine_calls.save_qsnap <> nil then
      machine_calls.save_qsnap('-01');
  if keyboard[map_ingame_actions.save_state_player_2] then
    if @machine_calls.save_qsnap <> nil then
      machine_calls.save_qsnap('-02');
  if keyboard[map_ingame_actions.load_state_player_1] then
    if @machine_calls.load_qsnap <> nil then
      machine_calls.load_qsnap('-01');
  if keyboard[map_ingame_actions.load_state_player_1] then
    if @machine_calls.load_qsnap <> nil then
      machine_calls.load_qsnap('-02');
  if keyboard[map_ingame_actions.slow] then
    main_actions.main_form_reduce_fps;
  if keyboard[map_ingame_actions.show_info] then
  begin
    emu_in_game.fps_show := not emu_in_game.fps_show;
    emu_in_game.fps_count := not emu_in_game.fps_count;
    emu_in_game.fps_temp := '';
    frm_main.tmr_fps.Enabled := not frm_main.tmr_fps.Enabled;
  end;
end;

procedure controls_value;
var
  vi: byte;
  sdl_event: TSDL_Event;

begin
  if SDL_PollEvent(@sdl_event) = 0 then
    exit;

  event.arcade := false;
  event.keyboard := false;
  event.mouse := false;

  for vi := 0 to $FF do
    if keyboard[vi] <> (keystate[vi] <> 0) then
    begin
      event.keyboard := true;
      copymemory(@keyboard[0], keystate, $100);
      break;
    end;

  // Keyboard actions for frontend and ingame services
  if event.keyboard then
  begin
    evaluate_frontend_or_ingame_keyboard(sdl_event);
    for vi := 0 to NUM_PLAYERS do
      evaluate_arcade_keyb(vi);
  end;

  { // Joystick and Keyboard game actions
    if (event.ejoystick or event.earcade) then
    begin
    // for vi := 0 to NUM_PLAYERS do
    // begin
    // if not(p_contrls.map_arcade.use_key[0]) then
    if (event.ejoystick) and (not event.keyboard) then
    evaluate_arcade_joy(sdl_event, 0)
    else if event.keyboard then
    evaluate_arcade_keyb(vi);
    // end;
    end; }
  // Mouse Actions
  if event.emouse then
    evaluate_mouse(sdl_event);
end;

procedure controls_pause;
var
  vi: byte;
  sdl_event: TSDL_Event;

begin
  if SDL_PollEvent(@sdl_event) = 0 then
    exit;

  event.arcade := false;
  event.keyboard := false;
  event.mouse := false;

  for vi := 0 to $FF do
    if keyboard[vi] <> (keystate[vi] <> 0) then
    begin
      event.keyboard := true;
      copymemory(@keyboard[0], keystate, $100);
      break;
    end;

  // Keyboard actions for frontend and ingame services
  if event.keyboard then
  begin

    ingame_pause(sdl_event);
    { for vi := 0 to NUM_PLAYERS do
      evaluate_arcade_keyb(vi); }
  end;

  { // Joystick and Keyboard game actions
    if (event.ejoystick or event.earcade) then
    begin
    // for vi := 0 to NUM_PLAYERS do
    // begin
    // if not(p_contrls.map_arcade.use_key[0]) then
    if (event.ejoystick) and (not event.keyboard) then
    evaluate_arcade_joy(sdl_event, 0)
    else if event.keyboard then
    evaluate_arcade_keyb(vi);
    // end;
    end;
    // Mouse Actions
    if event.emouse then
    evaluate_mouse(sdl_event); }
end;

procedure analog_read_0;
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS do
  begin
    // EJE Y
    if analog.c[0].inverted_y then
    begin
      if p_contrls.map_arcade.down[f] then
      begin
        analog.c[0].y[f] := analog.c[0].y[f] + analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].y[f] > analog.c[0].max_val then
            analog.c[0].y[f] := analog.c[0].y[f] - analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].y[f] > analog.c[0].max_val then
            analog.c[0].y[f] := analog.c[0].max_val;
        end;
      end;
      if p_contrls.map_arcade.up[f] then
      begin
        analog.c[0].y[f] := analog.c[0].y[f] - analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].y[f] < analog.c[0].min_val then
            analog.c[0].y[f] := analog.c[0].y[f] + analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].y[f] < analog.c[0].min_val then
            analog.c[0].y[f] := analog.c[0].min_val;
        end;
      end;
      if analog.c[0].return_center then
      begin
        if not(p_contrls.map_arcade.down[f]) then
        begin
          if analog.c[0].y[f] > analog.c[0].mid_val then
          begin
            analog.c[0].y[f] := analog.c[0].y[f] - analog.c[0].delta;
            if analog.c[0].y[f] < analog.c[0].mid_val then
              analog.c[0].y[f] := analog.c[0].mid_val;
          end;
        end;
        if not(p_contrls.map_arcade.up[f]) then
        begin
          if analog.c[0].y[f] < analog.c[0].mid_val then
          begin
            analog.c[0].y[f] := analog.c[0].y[f] + analog.c[0].delta;
            if analog.c[0].y[f] > analog.c[0].mid_val then
              analog.c[0].y[f] := analog.c[0].mid_val;
          end;
        end;
      end;
    end
    else
    begin
      if p_contrls.map_arcade.up[f] then
      begin
        analog.c[0].y[f] := analog.c[0].y[f] + analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].y[f] > analog.c[0].max_val then
            analog.c[0].y[f] := analog.c[0].y[f] - analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].y[f] > analog.c[0].max_val then
            analog.c[0].y[f] := analog.c[0].max_val;
        end;
      end;
      if p_contrls.map_arcade.down[f] then
      begin
        analog.c[0].y[f] := analog.c[0].y[f] - analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].y[f] < analog.c[0].min_val then
            analog.c[0].y[f] := analog.c[0].y[f] + analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].y[f] < analog.c[0].min_val then
            analog.c[0].y[f] := analog.c[0].min_val;
        end;
      end;
      if analog.c[0].return_center then
      begin
        if not(p_contrls.map_arcade.up[f]) then
        begin
          if analog.c[0].y[f] > analog.c[0].mid_val then
          begin
            analog.c[0].y[f] := analog.c[0].y[f] - analog.c[0].delta;
            if analog.c[0].y[f] < analog.c[0].mid_val then
              analog.c[0].y[f] := analog.c[0].mid_val;
          end;
        end;
        if not(p_contrls.map_arcade.down[f]) then
        begin
          if analog.c[0].y[f] < analog.c[0].mid_val then
          begin
            analog.c[0].y[f] := analog.c[0].y[f] + analog.c[0].delta;
            if analog.c[0].y[f] > analog.c[0].mid_val then
              analog.c[0].y[f] := analog.c[0].mid_val;
          end;
        end;
      end;
    end;
    // EJE X
    if analog.c[0].inverted_x then
    begin
      if p_contrls.map_arcade.right[f] then
      begin
        analog.c[0].x[f] := analog.c[0].x[f] + analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].x[f] > analog.c[0].max_val then
            analog.c[0].x[f] := analog.c[0].x[f] - analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].x[f] > analog.c[0].max_val then
            analog.c[0].x[f] := analog.c[0].max_val;
        end;
      end;
      if p_contrls.map_arcade.left[f] then
      begin
        analog.c[0].x[f] := analog.c[0].x[f] - analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].x[f] < analog.c[0].min_val then
            analog.c[0].x[f] := analog.c[0].x[f] + analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].x[f] < analog.c[0].min_val then
            analog.c[0].x[f] := analog.c[0].min_val;
        end;
      end;
      if analog.c[0].return_center then
      begin
        if not(p_contrls.map_arcade.right[f]) then
          if analog.c[0].x[f] > analog.c[0].mid_val then
          begin
            analog.c[0].x[f] := analog.c[0].x[f] - analog.c[0].delta;
            if analog.c[0].x[f] < analog.c[0].mid_val then
              analog.c[0].x[f] := analog.c[0].mid_val;
          end;
        if not(p_contrls.map_arcade.left[f]) then
          if analog.c[0].x[f] < analog.c[0].mid_val then
          begin
            analog.c[0].x[f] := analog.c[0].x[f] + analog.c[0].delta;
            if analog.c[0].x[f] > analog.c[0].mid_val then
              analog.c[0].x[f] := analog.c[0].mid_val;
          end;
      end;
    end
    else
    begin
      if p_contrls.map_arcade.left[f] then
      begin
        analog.c[0].x[f] := analog.c[0].x[f] + analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].x[f] > analog.c[0].max_val then
            analog.c[0].x[f] := analog.c[0].x[f] - analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].x[f] > analog.c[0].max_val then
            analog.c[0].x[f] := analog.c[0].max_val;
        end;
      end;
      if p_contrls.map_arcade.right[f] then
      begin
        analog.c[0].x[f] := analog.c[0].x[f] - analog.c[0].delta;
        if analog.c[0].circle then
        begin
          if analog.c[0].x[f] > analog.c[0].max_val then
            analog.c[0].x[f] := analog.c[0].x[f] + analog.c[0].max_val;
        end
        else
        begin
          if analog.c[0].x[f] < analog.c[0].min_val then
            analog.c[0].x[f] := analog.c[0].min_val;
        end;
      end;
      if analog.c[0].return_center then
      begin
        if not(p_contrls.map_arcade.left[f]) then
          if analog.c[0].x[f] > analog.c[0].mid_val then
          begin
            analog.c[0].x[f] := analog.c[0].x[f] - analog.c[0].delta;
            if analog.c[0].x[f] < analog.c[0].mid_val then
              analog.c[0].x[f] := analog.c[0].mid_val;
          end;
        if not(p_contrls.map_arcade.right[f]) then
          if analog.c[0].x[f] < analog.c[0].mid_val then
          begin
            analog.c[0].x[f] := analog.c[0].x[f] + analog.c[0].delta;
            if analog.c[0].x[f] > analog.c[0].mid_val then
              analog.c[0].x[f] := analog.c[0].mid_val;
          end;
      end;
    end;
  end;
end;

procedure analog_read_1;
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS do
  begin
    if p_contrls.map_arcade.but0[f] then
    begin
      analog.c[1].val[f] := analog.c[1].val[f] + analog.c[1].delta;
      if analog.c[1].val[f] > analog.c[1].max_val then
        analog.c[1].val[f] := analog.c[1].max_val;
    end;
    if analog.c[1].return_center then
    begin
      if not(p_contrls.map_arcade.but0[f]) then
      begin
        if analog.c[1].val[f] > analog.c[1].min_val then
        begin
          analog.c[1].val[f] := analog.c[1].val[f] - analog.c[1].delta;
          if analog.c[1].val[f] < analog.c[1].min_val then
            analog.c[1].val[f] := analog.c[1].min_val;
        end;
      end;
    end;
  end;
end;

procedure analog_read_2;
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS do
  begin
    if p_contrls.map_arcade.but1[f] then
    begin
      analog.c[2].val[f] := analog.c[2].val[f] + analog.c[2].delta;
      if analog.c[2].val[f] > analog.c[2].max_val then
        analog.c[2].val[f] := analog.c[2].max_val;
    end;
    if analog.c[2].return_center then
    begin
      if not(p_contrls.map_arcade.but1[f]) then
      begin
        if analog.c[2].val[f] > analog.c[2].min_val then
        begin
          analog.c[2].val[f] := analog.c[2].val[f] - analog.c[2].delta;
          if analog.c[2].val[f] < analog.c[2].min_val then
            analog.c[2].val[f] := analog.c[2].min_val;
        end;
      end;
    end;
  end;
end;

procedure analog_read_3;
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS do
  begin
    if p_contrls.map_arcade.but2[f] then
    begin
      analog.c[3].val[f] := analog.c[3].val[f] + analog.c[3].delta;
      if analog.c[3].val[f] > analog.c[3].max_val then
        analog.c[3].val[f] := analog.c[3].max_val;
    end;
    if analog.c[3].return_center then
    begin
      if not(p_contrls.map_arcade.but2[f]) then
      begin
        if analog.c[3].val[f] > analog.c[3].min_val then
        begin
          analog.c[3].val[f] := analog.c[3].val[f] - analog.c[3].delta;
          if analog.c[3].val[f] < analog.c[3].min_val then
            analog.c[3].val[f] := analog.c[3].min_val;
        end;
      end;
    end;
  end;
end;

procedure analog_read_4;
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS do
  begin
    if p_contrls.map_arcade.but3[f] then
    begin
      analog.c[4].val[f] := analog.c[4].val[f] + analog.c[4].delta;
      if analog.c[4].val[f] > analog.c[4].max_val then
        analog.c[4].val[f] := analog.c[4].max_val;
    end;
    if analog.c[4].return_center then
    begin
      if not(p_contrls.map_arcade.but3[f]) then
      begin
        if analog.c[4].val[f] > analog.c[4].min_val then
        begin
          analog.c[4].val[f] := analog.c[4].val[f] - analog.c[4].delta;
          if analog.c[4].val[f] < analog.c[4].min_val then
            analog.c[4].val[f] := analog.c[4].min_val;
        end;
      end;
    end;
  end;
end;

procedure init_analog(cpu: byte; cpu_clock: integer);
begin
  analog.cpu := cpu;
  analog.clock := cpu_clock;
end;

procedure analog_0(sensitivity, port_delta, mid_val, max_val, min_val: integer; return_center: boolean;
  circle: boolean = false; inverted_x: boolean = false; inverted_y: boolean = false);
var
  f: byte;
begin
  timers.init(analog.cpu, analog.clock / ((4250000 / analog.clock) * sensitivity), analog_read_0, nil, true);
  analog.c[0].inverted_x := inverted_x;
  analog.c[0].inverted_y := inverted_y;
  analog.c[0].delta := port_delta;
  analog.c[0].mid_val := mid_val;
  analog.c[0].max_val := max_val;
  analog.c[0].min_val := min_val;
  analog.c[0].circle := circle;
  for f := 0 to NUM_PLAYERS do
  begin
    analog.c[0].x[f] := mid_val;
    analog.c[0].y[f] := mid_val;
  end;
  analog.c[0].return_center := return_center;
end;

procedure analog_1(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
var
  f: byte;
begin
  timers.init(analog.cpu, analog.clock / ((4250000 / analog.clock) * sensitivity), analog_read_1, nil, true);
  analog.c[1].delta := port_delta;
  analog.c[1].max_val := max_val;
  analog.c[1].min_val := min_val;
  for f := 0 to NUM_PLAYERS do
    analog.c[1].val[f] := min_val;
  analog.c[1].return_center := return_center;
end;

procedure analog_2(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
var
  f: byte;
begin
  timers.init(analog.cpu, analog.clock / ((4250000 / analog.clock) * sensitivity), analog_read_2, nil, true);
  analog.c[2].delta := port_delta;
  analog.c[2].max_val := max_val;
  analog.c[2].min_val := min_val;
  for f := 0 to NUM_PLAYERS do
    analog.c[2].val[f] := min_val;
  analog.c[2].return_center := return_center;
end;

procedure analog_3(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
var
  f: byte;
begin
  timers.init(analog.cpu, analog.clock / ((4250000 / analog.clock) * sensitivity), analog_read_3, nil, true);
  analog.c[3].delta := port_delta;
  analog.c[3].max_val := max_val;
  analog.c[3].min_val := min_val;
  for f := 0 to NUM_PLAYERS do
    analog.c[3].val[f] := min_val;
  analog.c[3].return_center := return_center;
end;

procedure analog_4(sensitivity, port_delta, max_val, min_val: integer; return_center: boolean);
var
  f: byte;
begin
  timers.init(analog.cpu, analog.clock / ((4250000 / analog.clock) * sensitivity), analog_read_4, nil, true);
  analog.c[4].delta := port_delta;
  analog.c[4].max_val := max_val;
  analog.c[4].min_val := min_val;
  for f := 0 to NUM_PLAYERS do
    analog.c[4].val[f] := min_val;
  analog.c[4].return_center := return_center;
end;

end.
