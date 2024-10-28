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
  AXIS_THRESHOLD = 8000;
  BUTTONS_PER_PLAYER = 6;

const
  NUM_PLAYERS = 4 - 1;
  NUM_JOYSTICKS = 10 - 1;
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
  // Resto de teclas
  KEYBOARD_FILA0_T0 = 53;
  KEYBOARD_FILA0_T1 = 45;
  KEYBOARD_FILA0_T2 = 46;
  KEYBOARD_FILA1_T1 = 47;
  KEYBOARD_FILA1_T2 = 48;
  KEYBOARD_FILA2_T1 = 51;
  KEYBOARD_FILA2_T2 = 52;
  KEYBOARD_FILA2_T3 = 49;
  KEYBOARD_FILA3_T0 = 100;
  KEYBOARD_FILA3_T1 = 54;
  KEYBOARD_FILA3_T2 = 55;
  KEYBOARD_FILA3_T3 = 56;
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
    NUM_JOYSTICKS: byte;
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
    profile, name, platform_type, player, serial, vendor, guid, product, product_version, control_type, buttons: array [0 .. NUM_PLAYERS] of string;
    // Boolean state of actions
    coin, start, up, down, left, right, but0, but1, but2, but3, but4, but5, use_key: array [0 .. NUM_PLAYERS] of boolean;
    // Keyboard keys
    ncoin, nstart, nup, ndown, nleft, nright, nbut0, nbut1, nbut2, nbut3, nbut4, nbut5: array [0 .. NUM_PLAYERS] of byte;
    // Joystick keys
    joy_y, joy_x: array [0 .. NUM_PLAYERS] of smallint;
    deadzone_x, deadzone_y: array [0 .. NUM_PLAYERS] of integer;
    joy_up, joy_down, joy_left, joy_right, joy_coin, joy_start: array [0 .. NUM_PLAYERS] of integer;
    jbut0, jbut1, jbut2, jbut3, jbut4, jbut5, jbut6, jbut7, jbut8, jbut9, jbut10, jbut11, jbut12, jbut13, jbut14, jbut15, num_joystick: array [0 .. NUM_PLAYERS] of byte;
    // Game Controller keys
    gc_up, gc_down, gc_left, gc_right, gc_coin, gc_start: array [0 .. NUM_PLAYERS] of integer;
    gcbut0, gcbut1, gcbut2, gcbut3, gcbut4, gcbut5, num_gc: array [0 .. NUM_PLAYERS] of byte;
    // Strings to get and set events actions
    ncoin_col_name, nstart_col_name, nup_col_name, ndown_col_name, nleft_col_name, nright_col_name, nbut0_col_name, nbut1_col_name, nbut2_col_name, nbut3_col_name, nbut4_col_name, nbut5_col_name,
      jbut0_col_name, jbut1_col_name, jbut2_col_name, jbut3_col_name, jbut4_col_name, jbut5_col_name, num_joystick_col_name, joy_up_col_name, joy_down_col_name, joy_left_col_name, joy_right_col_name,
      joy_coin_col_name, joy_start_col_name: array [0 .. NUM_PLAYERS] of string;
  end;

  def_marcade = record
    in0, in1, in2, in3, in4: word;
    dswa, dswb, dswc: word;
    dswa_val, dswb_val, dswc_val: pdef_dip;
    dswa_val2, dswb_val2, dswc_val2: pdef_dip2;
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
procedure show_mouse_cursor(visible: boolean);
// Analog
procedure init_analog(cpu: byte; cpu_clock: integer);
procedure analog_0(sensitivity, port_delta, mid_val, max_val, min_val: integer; return_center: boolean; circle: boolean = false; inverted_x: boolean = false; inverted_y: boolean = false);
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

procedure show_mouse_cursor(visible: boolean);
begin
  if visible then
    SDL_ShowCursor(SDL_ENABLE) // Show the cursor
  else
    SDL_ShowCursor(SDL_DISABLE); // Hide the cursor
end;

procedure open_joystick(player: byte);
begin
  if SDL_Init(SDL_INIT_JOYSTICK) <> 0 then
  begin
    WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
    exit;
  end;

  // Check if joysticks are available
  if SDL_NumJoysticks() < 1 then
  begin
    WriteLn('No joysticks connected!');
    exit;
  end;

  // Open the joystick for the given player
  joystick_def[player] := SDL_JoystickOpen(player);
  if joystick_def[player] = nil then
  begin
    // WriteLn('Unable to open joystick for player ', player, '! SDL_Error: ', SDL_GetError);
  end
  else
  begin
    // WriteLn('Joystick ', player, ' opened successfully!');
  end;
end;

procedure close_joystick(num: byte);
begin
  SDL_JoystickClose(joystick_def[0]);
  joystick_def[0] := nil;
  SDL_JoystickEventState(SDL_DISABLE);
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
  event.NUM_JOYSTICKS := SDL_NumJoysticks;

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
  open_joystick(0);
  // for f := 0 to NUM_PLAYERS do
  // if joystick_def[f] = nil then
  // if (joystick_def[f] = nil) and (p_contrls.controller = 'joystick') then
  // p_contrls.map_arcade.use_key[0] := true;
end;

procedure UpdateButtonState(var button: boolean; keystate: boolean; var autofireEnabled: boolean; var autofireStatus: boolean; var eventFlag: boolean);
begin
  if autofireEnabled then
    autofireStatus := keystate
  else if button <> keystate then
  begin
    button := keystate;
    eventFlag := true;
  end;
end;

procedure UpdateDirection(var primary: boolean; primaryKeyState: boolean; var secondary: boolean; var eventFlag: boolean);
begin
  if primary <> primaryKeyState then
  begin
    primary := primaryKeyState;
    secondary := false; // Disable opposite direction
    eventFlag := true;
  end;
end;

procedure evaluate_arcade_keyb(player: byte);
begin
  // Coin
  if p_contrls.map_arcade.coin[player] <> keyboard[p_contrls.map_arcade.ncoin[player]] then
  begin
    p_contrls.map_arcade.coin[player] := keyboard[p_contrls.map_arcade.ncoin[player]];
    event.arcade := true;
  end;

  // Start
  if p_contrls.map_arcade.start[player] <> keyboard[p_contrls.map_arcade.nstart[player]] then
  begin
    p_contrls.map_arcade.start[player] := keyboard[p_contrls.map_arcade.nstart[player]];
    event.arcade := true;
  end;

  // Directions: Up/Down
  UpdateDirection(p_contrls.map_arcade.up[player], keyboard[p_contrls.map_arcade.nup[player]], p_contrls.map_arcade.down[player], event.arcade);
  UpdateDirection(p_contrls.map_arcade.down[player], keyboard[p_contrls.map_arcade.ndown[player]], p_contrls.map_arcade.up[player], event.arcade);

  // Directions: Left/Right
  UpdateDirection(p_contrls.map_arcade.left[player], keyboard[p_contrls.map_arcade.nleft[player]], p_contrls.map_arcade.right[player], event.arcade);
  UpdateDirection(p_contrls.map_arcade.right[player], keyboard[p_contrls.map_arcade.nright[player]], p_contrls.map_arcade.left[player], event.arcade);

  // Buttons with autofire handling
  UpdateButtonState(p_contrls.map_arcade.but0[player], keyboard[p_contrls.map_arcade.nbut0[player]], timers.autofire_enabled[0 + (player * 6)], timers.autofire_status[0 + (player * 6)], event.arcade);
  UpdateButtonState(p_contrls.map_arcade.but1[player], keyboard[p_contrls.map_arcade.nbut1[player]], timers.autofire_enabled[1 + (player * 6)], timers.autofire_status[1 + (player * 6)], event.arcade);
  UpdateButtonState(p_contrls.map_arcade.but2[player], keyboard[p_contrls.map_arcade.nbut2[player]], timers.autofire_enabled[2 + (player * 6)], timers.autofire_status[2 + (player * 6)], event.arcade);
  UpdateButtonState(p_contrls.map_arcade.but3[player], keyboard[p_contrls.map_arcade.nbut3[player]], timers.autofire_enabled[3 + (player * 6)], timers.autofire_status[3 + (player * 6)], event.arcade);
  UpdateButtonState(p_contrls.map_arcade.but4[player], keyboard[p_contrls.map_arcade.nbut4[player]], timers.autofire_enabled[4 + (player * 6)], timers.autofire_status[4 + (player * 6)], event.arcade);
  UpdateButtonState(p_contrls.map_arcade.but5[player], keyboard[p_contrls.map_arcade.nbut5[player]], timers.autofire_enabled[5 + (player * 6)], timers.autofire_status[5 + (player * 6)], event.arcade);
end;

procedure evaluar_arcade_joy_extra(player: byte);
var
  tempb: boolean;
  player_joy: byte;
begin
  SDL_JoystickUpdate;
  player_joy := p_contrls.map_arcade.num_joystick[player];
  // System
  tempb := SDL_JoystickGetButton(joystick_def[player_joy], p_contrls.map_arcade.joy_coin[player]) <> 0;
  if (p_contrls.map_arcade.coin[player] <> tempb) then
  begin
    p_contrls.map_arcade.coin[player] := tempb;
    event.arcade := true;
  end;
  tempb := SDL_JoystickGetButton(joystick_def[player_joy], p_contrls.map_arcade.joy_start[player]) <> 0;
  if (p_contrls.map_arcade.start[player] <> tempb) then
  begin
    p_contrls.map_arcade.start[player] := tempb;
    event.arcade := true;
  end;
end;

procedure UpdateJoystickButtonState(player: byte; buttonIndex: integer; player_joy: byte);
var
  tempb: boolean;
begin
  if timers.autofire_enabled[buttonIndex] then
    timers.autofire_status[buttonIndex] := SDL_JoystickGetButton(joystick_def[player_joy], p_contrls.map_arcade.jbut0[player] + (buttonIndex mod BUTTONS_PER_PLAYER)) <> 0
  else
  begin
    tempb := SDL_JoystickGetButton(joystick_def[player_joy], p_contrls.map_arcade.jbut0[player] + (buttonIndex mod BUTTONS_PER_PLAYER)) <> 0;
    if p_contrls.map_arcade.but0[player + (buttonIndex mod BUTTONS_PER_PLAYER)] <> tempb then
    begin
      p_contrls.map_arcade.but0[player + (buttonIndex mod BUTTONS_PER_PLAYER)] := tempb;
      event.arcade := true;
    end;
  end;
end;

procedure UpdateJoystickAxisState(player: byte; axisValue: integer; var positiveDirection: boolean; var negativeDirection: boolean);
var
  tempb: boolean;
begin
  tempb := axisValue < -AXIS_THRESHOLD;
  if tempb <> negativeDirection then
  begin
    event.arcade := true;
    negativeDirection := tempb;
    positiveDirection := false; // Disable opposite direction
  end;

  tempb := axisValue > AXIS_THRESHOLD;
  if tempb <> positiveDirection then
  begin
    event.arcade := true;
    positiveDirection := tempb;
    negativeDirection := false; // Disable opposite direction
  end;
end;

procedure evaluate_arcade_joy(tevent: TSDL_Event; player: byte);
var
  axis_num: integer;
  player_joy: byte;
  buttonIndex: integer;
begin
  if event.NUM_JOYSTICKS = 0 then
    exit;

  player_joy := p_contrls.map_arcade.num_joystick[player];

  case tevent.type_ of
    SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP:
      begin
        // Handle System buttons (Coin and Start)
        UpdateJoystickButtonState(player, 0, player_joy); // Coin button
        UpdateJoystickButtonState(player, 1, player_joy); // Start button

        // Handle player buttons (but0 - but5)
        for buttonIndex := 0 to BUTTONS_PER_PLAYER - 1 do
        begin
          UpdateJoystickButtonState(player, buttonIndex + (player * BUTTONS_PER_PLAYER), player_joy);
        end;
      end;

    SDL_JOYAXISMOTION:
      begin
        // Handle Axis Movements
        if tevent.jaxis.axis in [0, 2, 4, 6] then // X-axis (left/right)
        begin
          axis_num := tevent.jaxis.value;
          UpdateJoystickAxisState(player, axis_num, p_contrls.map_arcade.right[player], p_contrls.map_arcade.left[player]);
        end
        else if tevent.jaxis.axis in [1, 3, 5, 7] then // Y-axis (up/down)
        begin
          axis_num := tevent.jaxis.value;
          UpdateJoystickAxisState(player, axis_num, p_contrls.map_arcade.down[player], p_contrls.map_arcade.up[player]);
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
  event.ejoystick := true;

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

  // Joystick and Keyboard game actions
  if (event.ejoystick or event.earcade) then
  begin
    for vi := 0 to NUM_PLAYERS do
    begin
      if not(p_contrls.map_arcade.use_key[0]) then
        if (event.ejoystick) and (not event.keyboard) then
          evaluate_arcade_joy(sdl_event, 0)
        else if event.keyboard then
          evaluate_arcade_keyb(vi);
    end;
  end;

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

procedure evalue_controls;
var
  f: byte;
  sdl_event: TSDL_Event;
begin
  event.arcade := false;
  event.keyboard := false;
  event.mouse := false;

  // Handle Joystick Events
  if (event.ejoystick or event.earcade) then
  begin
    for f := 0 to NUM_PLAYERS - 1 do
    begin
      if not p_contrls.map_arcade.use_key[f] then
      begin
        // Evaluate joystick controls
        evaluate_arcade_joy(sdl_event, f);
        if event.earcade then
          evaluar_arcade_joy_extra(f); // Call extra function for arcade-specific logic
      end;
    end;
  end;

  // Poll for new SDL events
  if SDL_PollEvent(@sdl_event) = 0 then
    exit;

  // Keyboard handling
  for f := 0 to $FF do
  begin
    if keyboard[f] <> (keystate[f] <> 0) then
    begin
      event.keyboard := true;
      Move(keystate[0], keyboard[0], SizeOf(keystate)); // Copy entire keyboard state
      break;
    end;
  end;

  // Handle independent keypresses
  if event.keyboard then
  begin
    if keyboard[KEYBOARD_F1] then
      main_vars.service1 := not main_vars.service1;

    if keyboard[KEYBOARD_F7] then
    begin
      if Assigned(machine_calls.save_qsnap) then
        machine_calls.save_qsnap('-01');
      keystate[KEYBOARD_F7] := 0;
    end;

    if keyboard[KEYBOARD_F8] then
    begin
      if Assigned(machine_calls.save_qsnap) then
        machine_calls.save_qsnap('-02');
      keystate[KEYBOARD_F8] := 0;
    end;

    if keyboard[KEYBOARD_F9] then
    begin
      if Assigned(machine_calls.load_qsnap) then
        machine_calls.load_qsnap('-01');
      keystate[KEYBOARD_F9] := 0;
    end;

    if keyboard[KEYBOARD_F10] then
    begin
      if Assigned(machine_calls.load_qsnap) then
        machine_calls.load_qsnap('-02');
      keystate[KEYBOARD_F10] := 0;
    end;
  end;

  // Evaluate joystick input for players using keyboards
  if (event.ejoystick or event.earcade) then
  begin
    for f := 0 to NUM_PLAYERS - 1 do
    begin
      if p_contrls.map_arcade.use_key[f] then
      begin
        if event.keyboard then
        begin
          // evaluate_arcade_keyb(f); // Evaluate keyboard input for arcade
          // if event.earcade then
          // evaluar_arcade_keyb_extra(f); // Extra arcade key logic
        end;
      end;
    end;
  end;

  // Handle Mouse Events
  if event.emouse then
  begin
    case sdl_event.type_ of
      SDL_MOUSEMOTION:
        begin
          mouse_def.x := trunc(sdl_event.motion.x / main_screen.mouse_x);
          mouse_def.y := trunc(sdl_event.motion.y / main_screen.mouse_y);
          event.mouse := true;
        end;

      SDL_MOUSEBUTTONUP:
        begin
          event.mouse := true;
          case sdl_event.button.button of
            SDL_BUTTON_LEFT:
              mouse_def.button1 := false;
            SDL_BUTTON_RIGHT:
              mouse_def.button2 := false;
          end;
        end;

      SDL_MOUSEBUTTONDOWN:
        begin
          event.mouse := true;
          case sdl_event.button.button of
            SDL_BUTTON_LEFT:
              mouse_def.button1 := true;
            SDL_BUTTON_RIGHT:
              mouse_def.button2 := true;
          end;
        end;
    end;
  end;
end;

procedure UpdateAxis(var position: integer; delta, min_val, max_val, mid_val: integer; increase, circle, return_center: boolean);
begin
  if increase then
  begin
    position := position + delta;
    if circle then
    begin
      if position > max_val then
        position := position - max_val;
    end
    else if position > max_val then
      position := max_val;
  end
  else
  begin
    position := position - delta;
    if circle then
    begin
      if position < min_val then
        position := position + max_val;
    end
    else if position < min_val then
      position := min_val;
  end;

  // Handle returning to center
  if return_center then
  begin
    if (increase and (position > mid_val)) or (not increase and (position < mid_val)) then
      position := mid_val;
  end;
end;

procedure analog_read_0;
var
  f: byte;
begin
  for f := 0 to NUM_PLAYERS - 1 do
  begin
    // EJE Y
    if analog.c[0].inverted_y then
    begin
      if p_contrls.map_arcade.down[f] then
        UpdateAxis(analog.c[0].y[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, true, analog.c[0].circle, analog.c[0].return_center);
      if p_contrls.map_arcade.up[f] then
        UpdateAxis(analog.c[0].y[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, false, analog.c[0].circle, analog.c[0].return_center);
    end
    else
    begin
      if p_contrls.map_arcade.up[f] then
        UpdateAxis(analog.c[0].y[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, true, analog.c[0].circle, analog.c[0].return_center);
      if p_contrls.map_arcade.down[f] then
        UpdateAxis(analog.c[0].y[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, false, analog.c[0].circle, analog.c[0].return_center);
    end;

    // EJE X
    if analog.c[0].inverted_x then
    begin
      if p_contrls.map_arcade.right[f] then
        UpdateAxis(analog.c[0].x[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, true, analog.c[0].circle, analog.c[0].return_center);
      if p_contrls.map_arcade.left[f] then
        UpdateAxis(analog.c[0].x[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, false, analog.c[0].circle, analog.c[0].return_center);
    end
    else
    begin
      if p_contrls.map_arcade.left[f] then
        UpdateAxis(analog.c[0].x[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, true, analog.c[0].circle, analog.c[0].return_center);
      if p_contrls.map_arcade.right[f] then
        UpdateAxis(analog.c[0].x[f], analog.c[0].delta, analog.c[0].min_val, analog.c[0].max_val, analog.c[0].mid_val, false, analog.c[0].circle, analog.c[0].return_center);
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

procedure analog_0(sensitivity, port_delta, mid_val, max_val, min_val: integer; return_center: boolean; circle: boolean = false; inverted_x: boolean = false; inverted_y: boolean = false);
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
