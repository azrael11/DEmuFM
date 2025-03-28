unit config_controls;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.StrUtils,
  WinApi.Windows,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.Layouts,
  FMX.TabControl,
  FMX.ListBox,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.Platform.Win,
  umain_config,
  SDL2,
  FMX.Ani,
  SubjectStand,
  FrameStand,
  f_controls_keyboard,
  f_controls_joystick,
  f_controls_gamepad;

type
  TEDIT_MODE_CONTROLLER_TYPE = (EMCT_Keyboard, EMCT_Joystick, EMCT_Gamepad, EMCT_None);

  { Joystick and Gamepads layouts

    Joystick Layouts
    CC_Joy_Aracade_1 : Joystick, 6 Action Buttons, Coin, Start

    Gamepad Layouts
    CC_GP_PS4 : Playstation 4 Controller
  }
type
  TCURRENT_CONTROLLER = (CC_Keyboard, CC_Joy_Arcade_1, CC_GP_PS4);

type
  TEDIT_MODE = record
    color: integer;
    cursor: TCursor;
    Edit: boolean;
  end;

type
  TCONTROLLER_DATA = record
    name: string;
    vtype: string;
    serial: string;
    buttons: string;
    guid: TSDL_JoystickGUID;
    vendor: word;
    product: word;
    product_version: word;
    power: string;
  end;

type
  Tfrm_config_controls = class(TForm)
    rect_cc: TRectangle;
    rect_cc_header: TRectangle;
    lbl_cc_header: TLabel;
    cb_cc_players_platform_type: TComboBox;
    lbl_cc_players_platform_type: TLabel;
    tc_cc_players: TTabControl;
    tbi_cc_players_p1: TTabItem;
    cbe_cc_players_profile: TComboEdit;
    spb_cc_players_profile_load: TSpeedButton;
    spb_cc_players_profile_save: TSpeedButton;
    tbi_cc_players_p2: TTabItem;
    tbi_cc_players_p3: TTabItem;
    tbi_cc_players_p4: TTabItem;
    rect_cc_players_platform: TRectangle;
    rect_cc_players_players: TRectangle;
    rect_cc_footer: TRectangle;
    txt_cc_footer_info: TText;
    rect_cc_players_profile: TRectangle;
    cb_cc_players_controller: TComboBox;
    img_cc_players_controller: TImage;
    tc_cc: TTabControl;
    tbi_cc_frontend: TTabItem;
    tbi_cc_ingame: TTabItem;
    tbi_cc_players: TTabItem;
    lbl_cc_frontend_show_configuration: TLabel;
    lbl_cc_frontend_show_controls: TLabel;
    lbl_cc_frontend_show_display: TLabel;
    lbl_cc_frontend_play: TLabel;
    lbl_cc_frontend_show_information: TLabel;
    spb_cc_frontend_edit: TSpeedButton;
    img_cc_frontend_edit: TImage;
    lbl_cc_frontend_choose_platform: TLabel;
    lbl_cc_frontend_move_up: TLabel;
    lbl_cc_frontend_move_down: TLabel;
    lbl_cc_frontend_move_left: TLabel;
    lbl_cc_frontend_move_right: TLabel;
    lbl_cc_frontend_time_play: TLabel;
    lbl_cc_ingame_leave_game: TLabel;
    lbl_cc_ingame_pause_game: TLabel;
    lbl_cc_ingame_fullscreen: TLabel;
    lbl_cc_ingame_reset_game: TLabel;
    lbl_cc_ingame_service: TLabel;
    lbl_cc_ingame_p1_save_state: TLabel;
    lbl_cc_ingame_snapshot: TLabel;
    lbl_cc_ingame_fast: TLabel;
    lbl_cc_ingame_slow: TLabel;
    lbl_cc_ingame_p1_load_state: TLabel;
    lbl_cc_ingame_p2_save_state: TLabel;
    lbl_cc_ingame_p2_load_state: TLabel;
    spb_cc_ingame_edit: TSpeedButton;
    img_cc_ingame_edit: TImage;
    lbl_cc_frontend_quit: TLabel;
    eff_frgb_cc_frontend_edit: TFillRGBEffect;
    eff_frgb_cc_ingame_edit: TFillRGBEffect;
    rect_cc_frontend_quit: TRectangle;
    txt_cc_frontend_quit: TText;
    rect_cc_frontend_move_up: TRectangle;
    txt_cc_frontend_move_up: TText;
    rect_cc_frontend_show_controls: TRectangle;
    txt_cc_frontend_show_controls: TText;
    rect_cc_frontend_move_left: TRectangle;
    txt_cc_frontend_move_left: TText;
    rect_cc_frontend_show_display: TRectangle;
    txt_cc_frontend_show_display: TText;
    rect_cc_frontend_move_right: TRectangle;
    txt_cc_frontend_move_right: TText;
    rect_cc_frontend_move_down: TRectangle;
    txt_cc_frontend_move_down: TText;
    rect_cc_frontend_show_configuration: TRectangle;
    rect_cc_frontend_play: TRectangle;
    txt_cc_frontend_play: TText;
    rect_cc_frontend_show_information: TRectangle;
    txt_cc_frontend_show_information: TText;
    rect_cc_frontend_time_play: TRectangle;
    txt_cc_frontend_time_play: TText;
    rect_cc_frontend_choose_platform: TRectangle;
    txt_cc_frontend_choose_platform: TText;
    txt_cc_frontend_show_configuration: TText;
    rect_cc_ingame_reset_game: TRectangle;
    txt_cc_ingame_reset_game: TText;
    rect_cc_ingame_service: TRectangle;
    txt_cc_ingame_service: TText;
    rect_cc_ingame_fast: TRectangle;
    txt_cc_ingame_fast: TText;
    rect_cc_ingame_slow: TRectangle;
    txt_cc_ingame_slow: TText;
    rect_cc_ingame_fullscreen: TRectangle;
    txt_cc_ingame_fullscreen: TText;
    rect_cc_ingame_leave_game: TRectangle;
    txt_cc_ingame_leave_game: TText;
    rect_cc_ingame_p1_load_state: TRectangle;
    txt_cc_ingame_p1_load_state: TText;
    rect_cc_ingame_p1_save_state: TRectangle;
    txt_cc_ingame_p1_save_state: TText;
    rect_cc_ingame_p2_load_state: TRectangle;
    txt_cc_ingame_p2_load_state: TText;
    rect_cc_ingame_p2_save_state: TRectangle;
    txt_cc_ingame_p2_save_state: TText;
    rect_cc_ingame_pause_game: TRectangle;
    txt_cc_ingame_pause_game: TText;
    rect_cc_ingame_snapshot: TRectangle;
    txt_cc_ingame_snapshot: TText;
    tmr_cc: TTimer;
    spb_cc_players_joystick_info: TSpeedButton;
    img_cc_players_joystick_info: TImage;
    rect_cc_players_joystick_info: TRectangle;
    rect_cc_players_joystick_battery: TRectangle;
    rect_cc_players_joystick_battery_top: TRectangle;
    rect_cc_players_joystick_battery_100: TRectangle;
    rect_cc_players_joystick_battery_5: TRectangle;
    rect_cc_players_joystick_battery_25: TRectangle;
    rect_cc_players_joystick_battery_50: TRectangle;
    rect_cc_players_joystick_battery_60: TRectangle;
    rect_cc_players_joystick_battery_75: TRectangle;
    rect_cc_players_joystick_battery_90: TRectangle;
    txt_cc_players_joystick_battery_percent: TText;
    lbl_cc_players_joystick_info: TLabel;
    lbl_cc_players_joystick_name: TLabel;
    lbl_cc_players_joystick_buttons: TLabel;
    lbl_cc_players_joystick_type: TLabel;
    lbl_cc_players_joystick_vendor: TLabel;
    lbl_cc_players_joystick_serial: TLabel;
    lbl_cc_players_joystick_product: TLabel;
    lbl_cc_players_joystick_product_version: TLabel;
    lbl_cc_players_joystick_name_value: TLabel;
    lbl_cc_players_joystick_buttons_value: TLabel;
    lbl_cc_players_joystick_type_value: TLabel;
    lbl_cc_players_joystick_vendor_value: TLabel;
    lbl_cc_players_joystick_serial_value: TLabel;
    lbl_cc_players_joystick_product_value: TLabel;
    lbl_cc_players_joystick_product_version_value: TLabel;
    img_cc_players_joystick_usb: TImage;
    fanim_cc_players_joystick_info: TFloatAnimation;
    lbl_cc_players_profile: TLabel;
    lbl_players_arcade_contrl_choose: TLabel;
    framestand_p1_key: TFrameStand;
    framestand_p2_key: TFrameStand;
    framestand_p3_key: TFrameStand;
    framestand_p4_key: TFrameStand;
    lay_cc_players_p1_key: TLayout;
    lay_cc_players_p2_key: TLayout;
    lay_cc_players_p3_key: TLayout;
    lay_cc_players_p4_key: TLayout;
    framestand_p1_joy: TFrameStand;
    framestand_p2_joy: TFrameStand;
    framestand_p3_joy: TFrameStand;
    framestand_p4_joy: TFrameStand;
    framestand_p1_gpd: TFrameStand;
    framestand_p2_gpd: TFrameStand;
    framestand_p3_gpd: TFrameStand;
    framestand_p4_gpd: TFrameStand;
    lay_cc_players_p1_joy: TLayout;
    lay_cc_players_p1_gpd: TLayout;
    lay_cc_players_p2_joy: TLayout;
    lay_cc_players_p2_gpd: TLayout;
    lay_cc_players_p3_joy: TLayout;
    lay_cc_players_p3_gpd: TLayout;
    lay_cc_players_p4_joy: TLayout;
    lay_cc_players_p4_gpd: TLayout;
    lbl_cc_players_controller_layout: TLabel;
    cb_cc_players_controller_layout: TComboBox;
    sbConfigExit: TSpeedButton;
    imgConfigExit: TImage;
    procedure cb_cc_players_controllerChange(Sender: TObject);
    procedure cb_cc_players_controller_layoutChange(Sender: TObject);
    procedure cb_cc_players_platform_typeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edit_onClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: word; var KeyChar: Char; Shift: TShiftState);
    procedure tbi_cc_frontendClick(Sender: TObject);
    procedure tbi_cc_ingameClick(Sender: TObject);
    procedure tbi_cc_playersClick(Sender: TObject);
    procedure tmr_ccTimer(Sender: TObject);
    procedure rect_onClick(Sender: TObject);
    procedure spb_cc_players_joystick_infoClick(Sender: TObject);
    procedure tbi_cc_players_p1Click(Sender: TObject);
    procedure tbi_cc_players_p2Click(Sender: TObject);
    procedure tbi_cc_players_p3Click(Sender: TObject);
    procedure tbi_cc_players_p4Click(Sender: TObject);
    procedure show_pop_rect(Sender: TObject);
    procedure show_pop_spb(Sender: TObject);
    procedure show_pop_tbi(Sender: TObject);
    procedure show_pop_cb(Sender: TObject);
    procedure show_pop_cbe(Sender: TObject);
    procedure clear_pop(Sender: TObject);
    procedure sbConfigExitMouseEnter(Sender: TObject);
    procedure sbConfigExitClick(Sender: TObject);
  private
    { Private declarations }
    edit_mode: boolean;
    edit_mode_controller_type: TEDIT_MODE_CONTROLLER_TYPE;
    key_before: string;
    count_time_dot, count_time: integer;
    active_controls: array [0 .. 15] of array [0 .. 3] of array [0 .. 1] of boolean;
    selected_emulator: string;

    procedure set_frontend_controls;
    procedure set_ingame_controls;

    procedure edit_frontend;
    procedure edit_ingame;
    procedure edit_player(num: byte);

    procedure save_key_to_frontend_data(tag: integer; Key: TText);
    procedure save_key_to_ingame_data(Key: TText);
    procedure save_key_to_players_data(Key: TText);

    function check_key_in_frontend(Key: string): boolean;
    function check_key_in_ingame(Key: string): boolean;
    function check_key_in_players(Key: string): boolean;
    function check_key_in_use(Key: TText): boolean;

    procedure get_set_joysticks;
    procedure get_controller_data;
    procedure enable_sdl_joystick_event;

    procedure display_right_controller(index: integer);

    procedure lang_strings;

    // new way
    function get_key_map(player, emulator: string; control_type: TCURRENT_CONTROLLER): string;
    function emulator_name(index: integer): string;

    procedure show_frame(player: string; controller: TCURRENT_CONTROLLER);
    procedure close_all_frame_stands;
    procedure set_current_controls;

    function get_controllers_profile: TStringList;
    procedure get_temp_active_controllers(emu_name: string; players: byte);
    procedure set_temp_active_controllers(emu_name: string; players: byte);
    procedure define_keys_from_temp_controllers(c_type: TCURRENT_CONTROLLER; player: byte);

  protected
    function get_platform_type: string;
    function set_edit_mode(mode: boolean): TEDIT_MODE;
  public
    { Public declarations }
    player: string;
    profile: string;
    emulator: string;
    joy_selected: integer;
    key_map: string;

    frame_p1_key: TFrameInfo<Tcontrol_keyboard>;
    frame_p1_joy: TFrameInfo<Tcontrol_joystick>;
    frame_p1_gpd: TFrameInfo<Tcontrol_gamepad>;
    frame_p2_key: TFrameInfo<Tcontrol_keyboard>;
    frame_p2_joy: TFrameInfo<Tcontrol_joystick>;
    frame_p2_gpd: TFrameInfo<Tcontrol_gamepad>;
    frame_p3_key: TFrameInfo<Tcontrol_keyboard>;
    frame_p3_joy: TFrameInfo<Tcontrol_joystick>;
    frame_p3_gpd: TFrameInfo<Tcontrol_gamepad>;
    frame_p4_key: TFrameInfo<Tcontrol_keyboard>;
    frame_p4_joy: TFrameInfo<Tcontrol_joystick>;
    frame_p4_gpd: TFrameInfo<Tcontrol_gamepad>;

    procedure get_players_controls;
    procedure get_ingame_controls;
  end;

var
  frm_config_controls: Tfrm_config_controls;

  temp_text: TText;
  temp_rect: TRectangle;

  joy_temp: TJOYSTICK_KEYS;

  joy_data: array [0 .. 3] of TCONTROLLER_DATA;

  c_controller: TCURRENT_CONTROLLER;

implementation

uses
  main,
  controls_engine,
  prj_functions,
  udata_controllers,
  ulang,
  ulang_consts,
  main_engine,
  uDataModule;

{$R *.fmx}

procedure Tfrm_config_controls.cb_cc_players_controllerChange(Sender: TObject);
begin
  frame_p1_gpd.Frame.gpd_active := False;
  frame_p2_gpd.Frame.gpd_active := False;
  frame_p3_gpd.Frame.gpd_active := False;
  frame_p4_gpd.Frame.gpd_active := False;
  frame_p1_joy.Frame.joy_active := False;
  frame_p2_joy.Frame.joy_active := False;
  frame_p3_joy.Frame.joy_active := False;
  frame_p4_joy.Frame.joy_active := False;
  cb_cc_players_platform_type.OnChange(cb_cc_players_platform_type);
end;

procedure Tfrm_config_controls.cb_cc_players_controller_layoutChange(Sender: TObject);
var
  joy_selected: integer;
begin
  joy_selected := cb_cc_players_controller.ItemIndex;
  get_controller_data;
  spb_cc_players_joystick_info.Visible := True;
  // img_cc_players_controller.Bitmap.LoadFromFile(config.main.prj_images_path.main + 'joystick.png');
  if joy_data[0].vtype <> 'Game Controller' then
  begin
    c_controller := CC_Joy_Arcade_1
  end
  else
  begin
    case cb_cc_players_controller_layout.ItemIndex of
      0:
        c_controller := CC_Joy_Arcade_1;
      1:
        c_controller := CC_GP_PS4
    end;
  end;
  define_keys_from_temp_controllers(c_controller, tc_cc_players.TabIndex);
  show_frame(tc_cc_players.TabIndex.ToString, c_controller);
  tc_cc_players.SetFocus;
end;

procedure Tfrm_config_controls.cb_cc_players_platform_typeChange(Sender: TObject);
var
  num: integer;

  procedure show_hide_players(p2, p3, p4: boolean);
  begin
    tbi_cc_players_p2.Visible := p2;
    tbi_cc_players_p3.Visible := p3;
    tbi_cc_players_p4.Visible := p4;

    if selected_emulator <> (Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex] then
    begin
      selected_emulator := (Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex];
      tc_cc_players.TabIndex := 0;
    end;
  end;

begin
  if (Sender as TComboBox).ItemIndex > 0 then
  begin
    tc_cc_players.Enabled := True;
    show_hide_players(True, True, True);
    case (Sender as TComboBox).ItemIndex of
      1, 13:
        num := 3;
      2, 3, 4, 5, 8, 9, 11, 12:
        begin
          show_hide_players(True, False, False);
          num := 1;
        end;
      6, 7, 10, 14, 15:
        begin
          show_hide_players(False, False, False);
          num := 0;
        end;
    end;
  end
  else
  begin
    show_hide_players(True, True, True);
    tc_cc_players.Enabled := False;
  end;
  Inc(num);
  if num = 0 then
    num := 1;
  if cb_cc_players_controller.ItemIndex > 0 then
  begin
    lbl_cc_players_controller_layout.Visible := True;
    cb_cc_players_controller_layout.Visible := True;
    cb_cc_players_controller_layout.ItemIndex := 0;
  end
  else
  begin
    lbl_cc_players_controller_layout.Visible := False;
    cb_cc_players_controller_layout.Visible := False;
    spb_cc_players_joystick_info.Visible := False;
    rect_cc_players_joystick_info.Visible := False;
    // img_cc_players_controller.Bitmap.LoadFromFile(config.main.prj_images_path.main + 'keyboard.png');
    c_controller := CC_Keyboard;
  end;

  emulator := emulator_name(cb_cc_players_platform_type.ItemIndex);
  profile := cbe_cc_players_profile.Text;
  // player := tc_cc_players.TabIndex.ToString;
  define_keys_from_temp_controllers(c_controller, tc_cc_players.TabIndex);
  show_frame(tc_cc_players.TabIndex.ToString, c_controller);
  tc_cc_players.SetFocus;
end;

function Tfrm_config_controls.check_key_in_frontend(Key: string): boolean;
var
  keys_front: array [0 .. 11] of string;
  vi: integer;
begin
  Result := False;
  dm.tKeyboardFrontend.Locate('name', 'quest');
  keys_front[0] := dm.tKeyboardFrontendquit_dspfm.AsString;
  keys_front[1] := dm.tKeyboardFrontendplay.AsString;
  keys_front[2] := dm.tKeyboardFrontendmove_up.AsString;
  keys_front[3] := dm.tKeyboardFrontendmove_down.AsString;
  keys_front[4] := dm.tKeyboardFrontendmove_left.AsString;
  keys_front[5] := dm.tKeyboardFrontendmove_right.AsString;
  keys_front[6] := dm.tKeyboardFrontendshow_configuration.AsString;
  keys_front[7] := dm.tKeyboardFrontendshow_display.AsString;
  keys_front[8] := dm.tKeyboardFrontendshow_controls.AsString;
  keys_front[9] := dm.tKeyboardFrontendshow_information.AsString;
  keys_front[10] := dm.tKeyboardFrontendshow_hide_time_game.AsString;
  keys_front[11] := dm.tKeyboardFrontendplatform_emulators.AsString;

  for vi := 0 to 11 do
  begin
    if keys_front[vi] = Key then
    begin
      Result := True;
      break;
    end;
  end;
end;

function Tfrm_config_controls.check_key_in_ingame(Key: string): boolean;
var
  keys_ingame: array [0 .. 11] of string;
  vi: integer;
begin
  dm.tKeyboardInGame.Locate('name', 'Default');

  keys_ingame[0] := key_name(dm.tKeyboardInGameleave_game.AsLongWord);
  keys_ingame[1] := key_name(dm.tKeyboardInGamepause_game.AsLongWord);
  keys_ingame[2] := key_name(dm.tKeyboardInGamefullscreen_game.AsLongWord);
  keys_ingame[3] := key_name(dm.tKeyboardInGameservice.AsLongWord);
  keys_ingame[4] := key_name(dm.tKeyboardInGamefastest.AsLongWord);
  keys_ingame[5] := key_name(dm.tKeyboardInGameslow.AsLongWord);
  keys_ingame[6] := key_name(dm.tKeyboardInGamereset.AsLongWord);
  keys_ingame[7] := key_name(dm.tKeyboardInGamesave_snap_player_1.AsLongWord);
  keys_ingame[8] := key_name(dm.tKeyboardInGamesave_snap_player_2.AsLongWord);
  keys_ingame[9] := key_name(dm.tKeyboardInGameload_snap_player_1.AsLongWord);
  keys_ingame[10] := key_name(dm.tKeyboardInGameload_snap_player_2.AsLongWord);
  keys_ingame[11] := key_name(dm.tKeyboardInGamesnapshot.AsLongWord);

  for vi := 0 to 11 do
  begin
    if keys_ingame[vi] = Key then
    begin
      Result := True;
      break;
    end;
  end;
end;

function Tfrm_config_controls.check_key_in_players(Key: string): boolean;
var
  keys_players: array [0 .. 3] of array [0 .. 11] of string;
  vi, vk: integer;
  key_map_num: string;
begin
  Result := False;
  for vi := 0 to 3 do
  begin
    dm.tPlayers.Filtered := False;
    dm.tPlayers.Filter := 'player=' + (vi + 1).ToString + ' AND key=1 AND platform=arcade AND selected=1';
    dm.tPlayers.Filtered := True;

    dm.tKeyboard.Locate('num', dm.tPlayerskey_map_num.AsString);

    keys_players[vi, 0] := key_name(dm.tKeyboardkey_up.AsLongWord);
    keys_players[vi, 1] := key_name(dm.tKeyboardkey_down.AsLongWord);
    keys_players[vi, 2] := key_name(dm.tKeyboardkey_left.AsLongWord);
    keys_players[vi, 3] := key_name(dm.tKeyboardkey_right.AsLongWord);
    keys_players[vi, 4] := key_name(dm.tKeyboardkey_b0.AsLongWord);
    keys_players[vi, 5] := key_name(dm.tKeyboardkey_b1.AsLongWord);
    keys_players[vi, 6] := key_name(dm.tKeyboardkey_b2.AsLongWord);
    keys_players[vi, 7] := key_name(dm.tKeyboardkey_b3.AsLongWord);
    keys_players[vi, 8] := key_name(dm.tKeyboardkey_b4.AsLongWord);
    keys_players[vi, 9] := key_name(dm.tKeyboardkey_b5.AsLongWord);
    keys_players[vi, 10] := key_name(dm.tKeyboardkey_coin.AsLongWord);
    keys_players[vi, 11] := key_name(dm.tKeyboardkey_start.AsLongWord);

    dm.tPlayers.Filtered := False;
  end;

  for vi := 0 to 3 do
    for vk := 0 to 11 do
    begin
      if keys_players[vi, vk] = Key then
      begin
        Result := True;
        break;
      end;
    end;
end;

function Tfrm_config_controls.check_key_in_use(Key: TText): boolean;
begin
  if tc_cc.TabIndex = 0 then
  begin
    Result := check_key_in_frontend(Key.Text);
    if Result then
    begin
      ShowMessage('This Key already in use in frontend');
      Key.Text := key_before;
    end;
  end
  else if tc_cc.TabIndex = 1 then
  begin
    Result := check_key_in_ingame(Key.Text);
    if Result then
    begin
      ShowMessage('This Key already in use in ingame actions keys');
      Key.Text := key_before;
      exit;
    end;
    Result := check_key_in_players(Key.Text);
    if Result then
    begin
      ShowMessage('This Key already in user from one of the players actions keys');
      Key.Text := key_before;
    end;
  end
  else if tc_cc.TabIndex = 2 then
  begin
    Result := check_key_in_players(Key.Text);
    if Result then
    begin
      ShowMessage('This Key already in user from one of the players actions keys');
      Key.Text := key_before;
      exit;
    end;
    Result := check_key_in_ingame(Key.Text);
    if Result then
    begin
      ShowMessage('This Key already in use in ingame actions keys');
      Key.Text := key_before;
    end;
  end;
end;

procedure Tfrm_config_controls.clear_pop(Sender: TObject);
begin
  txt_cc_footer_info.Text := '';
end;

procedure Tfrm_config_controls.close_all_frame_stands;
begin
  frame_p1_key.Close;
  frame_p2_key.Close;
  frame_p3_key.Close;
  frame_p4_key.Close;
  frame_p1_joy.Close;
  frame_p2_joy.Close;
  frame_p3_joy.Close;
  frame_p4_joy.Close;
  frame_p1_gpd.Close;
  frame_p2_gpd.Close;
  frame_p3_gpd.Close;
  frame_p4_gpd.Close;
end;

procedure Tfrm_config_controls.define_keys_from_temp_controllers(c_type: TCURRENT_CONTROLLER; player: byte);
var
  key_frame: TFrameInfo<Tcontrol_keyboard>;
  joy_frame: TFrameInfo<Tcontrol_joystick>;
  gpd_frame: TFrameInfo<Tcontrol_gamepad>;
begin
  case c_type of
    CC_Keyboard:
      begin
        case player of
          0:
            key_frame := frame_p1_key;
          1:
            key_frame := frame_p2_key;
          2:
            key_frame := frame_p3_key;
          3:
            key_frame := frame_p4_key;
        end;

        key_frame.Frame.txt_ck_up.Text := key_name(temp_key[player].Key.up);
        key_frame.Frame.txt_ck_down.Text := key_name(temp_key[player].Key.down);
        key_frame.Frame.txt_ck_left.Text := key_name(temp_key[player].Key.left);
        key_frame.Frame.txt_ck_right.Text := key_name(temp_key[player].Key.right);
        key_frame.Frame.txt_ck_but1.Text := key_name(temp_key[player].Key.b0);
        key_frame.Frame.txt_ck_but2.Text := key_name(temp_key[player].Key.b1);
        key_frame.Frame.txt_ck_but3.Text := key_name(temp_key[player].Key.b2);
        key_frame.Frame.txt_ck_but4.Text := key_name(temp_key[player].Key.b3);
        key_frame.Frame.txt_ck_but5.Text := key_name(temp_key[player].Key.b4);
        key_frame.Frame.txt_ck_but6.Text := key_name(temp_key[player].Key.b5);
        key_frame.Frame.txt_ck_coin.Text := key_name(temp_key[player].Key.coin);
        key_frame.Frame.txt_ck_start.Text := key_name(temp_key[player].Key.start);
      end;
    CC_Joy_Arcade_1:
      begin
        case player of
          0:
            joy_frame := frame_p1_joy;
          1:
            joy_frame := frame_p2_joy;
          2:
            joy_frame := frame_p3_joy;
          3:
            joy_frame := frame_p4_joy;
        end;

        joy_frame.Frame.txt_y_plus.Text := temp_joy[player].joy.up.ToString;
        joy_frame.Frame.txt_y_minus.Text := temp_joy[player].joy.down.ToString;
        joy_frame.Frame.txt_x_plus.Text := temp_joy[player].joy.right.ToString;
        joy_frame.Frame.txt_x_minus.Text := temp_joy[player].joy.left.ToString;
        joy_frame.Frame.txt_but0.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but1.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but2.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but3.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but4.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but5.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but6.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but7.Text := temp_joy[player].joy.b0.ToString;
        joy_frame.Frame.txt_but8.Text := temp_joy[player].joy.coin.ToString;
        joy_frame.Frame.txt_but9.Text := temp_joy[player].joy.start.ToString;
        joy_frame.Frame.txt_deadzone_x_value.Text := temp_joy[player].joy.deadzone_x.ToString;
        joy_frame.Frame.txt_deadzone_y_value.Text := temp_joy[player].joy.deadzone_y.ToString;
      end;
    CC_GP_PS4:
      begin
        case player of
          0:
            gpd_frame := frame_p1_gpd;
          1:
            gpd_frame := frame_p2_gpd;
          2:
            gpd_frame := frame_p3_gpd;
          3:
            gpd_frame := frame_p4_gpd;
        end;

        // must set the txt to setup the temp_keys

      end;
  end;
end;

procedure Tfrm_config_controls.display_right_controller(index: integer);
var
  emulator, controller_type: integer;
begin
  edit_mode := True;
  edit_player(index);
  emulator := cb_cc_players_platform_type.ItemIndex - 1;
  controller_type := cb_cc_players_controller.ItemIndex;

  if active_controls[emulator, index, controller_type] then
  begin

  end;
end;

procedure Tfrm_config_controls.edit_frontend;
var
  edit_set: TEDIT_MODE;
begin
  edit_set := set_edit_mode(edit_mode);
  rect_cc_frontend_quit.Fill.color := edit_set.color;
  rect_cc_frontend_quit.cursor := edit_set.cursor;
  rect_cc_frontend_move_up.Fill.color := edit_set.color;
  rect_cc_frontend_move_up.cursor := edit_set.cursor;
  rect_cc_frontend_show_controls.Fill.color := edit_set.color;
  rect_cc_frontend_show_controls.cursor := edit_set.cursor;
  rect_cc_frontend_move_left.Fill.color := edit_set.color;
  rect_cc_frontend_move_left.cursor := edit_set.cursor;
  rect_cc_frontend_show_display.Fill.color := edit_set.color;
  rect_cc_frontend_show_display.cursor := edit_set.cursor;
  rect_cc_frontend_move_right.Fill.color := edit_set.color;
  rect_cc_frontend_move_right.cursor := edit_set.cursor;
  rect_cc_frontend_move_down.Fill.color := edit_set.color;
  rect_cc_frontend_move_down.cursor := edit_set.cursor;
  rect_cc_frontend_show_configuration.Fill.color := edit_set.color;
  rect_cc_frontend_show_configuration.cursor := edit_set.cursor;
  rect_cc_frontend_play.Fill.color := edit_set.color;
  rect_cc_frontend_play.cursor := edit_set.cursor;
  rect_cc_frontend_show_information.Fill.color := edit_set.color;
  rect_cc_frontend_show_information.cursor := edit_set.cursor;
  rect_cc_frontend_time_play.Fill.color := edit_set.color;
  rect_cc_frontend_time_play.cursor := edit_set.cursor;
  rect_cc_frontend_choose_platform.Fill.color := edit_set.color;
  rect_cc_frontend_choose_platform.cursor := edit_set.cursor;
  eff_frgb_cc_frontend_edit.Enabled := edit_set.Edit;
  edit_mode_controller_type := EMCT_Keyboard;
end;

procedure Tfrm_config_controls.edit_ingame;
var
  edit_set: TEDIT_MODE;
begin
  edit_set := set_edit_mode(edit_mode);
  rect_cc_ingame_reset_game.Fill.color := edit_set.color;
  rect_cc_ingame_reset_game.cursor := edit_set.cursor;
  rect_cc_ingame_service.Fill.color := edit_set.color;
  rect_cc_ingame_service.cursor := edit_set.cursor;
  rect_cc_ingame_fast.Fill.color := edit_set.color;
  rect_cc_ingame_fast.cursor := edit_set.cursor;
  rect_cc_ingame_slow.Fill.color := edit_set.color;
  rect_cc_ingame_slow.cursor := edit_set.cursor;
  rect_cc_ingame_fullscreen.Fill.color := edit_set.color;
  rect_cc_ingame_fullscreen.cursor := edit_set.cursor;
  rect_cc_ingame_leave_game.Fill.color := edit_set.color;
  rect_cc_ingame_leave_game.cursor := edit_set.cursor;
  rect_cc_ingame_p1_load_state.Fill.color := edit_set.color;
  rect_cc_ingame_p1_load_state.cursor := edit_set.cursor;
  rect_cc_ingame_p1_save_state.Fill.color := edit_set.color;
  rect_cc_ingame_p1_save_state.cursor := edit_set.cursor;
  rect_cc_ingame_p2_load_state.Fill.color := edit_set.color;
  rect_cc_ingame_p2_load_state.cursor := edit_set.cursor;
  rect_cc_ingame_p2_save_state.Fill.color := edit_set.color;
  rect_cc_ingame_p2_save_state.cursor := edit_set.cursor;
  rect_cc_ingame_pause_game.Fill.color := edit_set.color;
  rect_cc_ingame_pause_game.cursor := edit_set.cursor;
  rect_cc_ingame_snapshot.Fill.color := edit_set.color;
  rect_cc_ingame_snapshot.cursor := edit_set.cursor;
  eff_frgb_cc_ingame_edit.Enabled := edit_set.Edit;
  edit_mode_controller_type := EMCT_Keyboard;
end;

procedure Tfrm_config_controls.edit_onClick(Sender: TObject);
begin
  case tc_cc.TabIndex of
    0:
      edit_frontend;
    1:
      edit_ingame;
    2:
      edit_player(tc_cc_players.TabIndex);
  end;
end;

procedure Tfrm_config_controls.edit_player(num: byte);
var
  edit_set: TEDIT_MODE;
  vComp: TComponent;
  player: integer;
begin
  edit_set := set_edit_mode(edit_mode);
  Inc(num);
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_b1');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_b2');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_b3');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_b4');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_b5');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_b6');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_up');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_down');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_left');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_right');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_coin');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('rect_cc_players_p' + num.ToString + '_start');
  (vComp as TRectangle).Fill.color := edit_set.color;
  (vComp as TRectangle).cursor := edit_set.cursor;
  vComp := Self.FindComponent('eff_frgb_cc_players_p' + num.ToString + '_edit');
  (vComp as TFillRGBEffect).Enabled := edit_set.Edit;
  if cb_cc_players_controller.ItemIndex = 0 then
    edit_mode_controller_type := EMCT_Keyboard
  else if cb_cc_players_controller.ItemIndex > 0 then
    edit_mode_controller_type := EMCT_Joystick;
  if (edit_mode = False) and (tmr_cc.Enabled) then
    count_time := 0;
end;

function Tfrm_config_controls.emulator_name(index: integer): string;
begin
  case index of
    1:
      Result := 'arcade';
    2:
      Result := 'spectrum';
    3:
      Result := 'amstrad';
    4:
      Result := 'commodore_64';
    5:
      Result := 'nes';
    6:
      Result := 'gameboy';
    7:
      Result := 'gameboy_color';
    8:
      Result := 'colecovision';
    9:
      Result := 'master_system';
    10:
      Result := 'gamegear';
    11:
      Result := 'sg_1000';
    12:
      Result := 'epoch_scv';
    13:
      Result := 'mega_drive';
    14:
      Result := 'chip8';
    15:
      Result := 'game_and_watch';
  end;
end;

procedure Tfrm_config_controls.enable_sdl_joystick_event;
var
  sdl2_handle: integer;
  sdl_event: TSDL_Event;
  running: boolean;
  player, mappings, joy_sel: integer;
  Action: String;
  sdl_axis: integer;
begin
  Action := '';
  if edit_mode then
    if edit_mode_controller_type = EMCT_Joystick then
    begin
      // sdl2_handle := FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd;
      // WinApi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
      window_render.W := 1;
      window_render.h := 1;
      player := tc_cc_players.TabIndex;
      joy_sel := cb_cc_players_controller.ItemIndex - 1;
      joystick_def[player] := SDL_JoystickOpen(joy_sel);
      if joy_data[player].vtype = 'Game Controller' then
      begin
        mappings := SDL_GameControllerAddMappingsFromFile('gamecontrollerdb.txt');
        game_controller_def[player] := SDL_GameControllerOpen(joy_sel);
      end;
      sdl_axis := SDL_JoystickNumAxes(joystick_def[player]);
      running := True;
      if game_controller_def[player] <> nil then
      begin
        while running do
        begin
          while (SDL_PollEvent(@sdl_event)) <> 0 do
          begin
            if sdl_event.type_ = SDL_KEYDOWN then
            begin
              if SDLK_ESCAPE = sdl_event.Key.keysym.sym then
              begin
                running := False;
                break;
              end;
            end
            else
            begin
              if sdl_event.type_ = SDL_CONTROLLERAXISMOTION then
              begin
                if sdl_event.caxis.axis in [0, 2, 4, 6] then
                begin
                  if sdl_event.caxis.value > 1000 then
                    Action := 'RIGHT'
                  else if sdl_event.caxis.value < -1000 then
                    Action := 'LEFT';
                end
                else if sdl_event.caxis.axis in [1, 3, 5, 7] then
                begin
                  if sdl_event.caxis.value > 1000 then
                    Action := 'DOWN'
                  else if sdl_event.caxis.value < -1000 then
                    Action := 'UP';
                end;
                running := False;
                break;
              end
              else if sdl_event.type_ = SDL_CONTROLLERBUTTONDOWN then
              begin
                if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_A then
                  Action := 'A'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_B then
                  Action := 'B'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_X then
                  Action := 'X'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_Y then
                  Action := 'Y'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_BACK then
                  Action := 'OPTIONS'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_START then
                  Action := 'START'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_LEFTSTICK then
                  Action := 'LEFT STICK'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_RIGHTSTICK then
                  Action := 'RIGHT STICK'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_LEFTSHOULDER then
                  Action := 'LEFT SHOULDER'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_RIGHTSHOULDER then
                  Action := 'RIGHT SHOULDER'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_UP then
                  Action := 'DPAD UP'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_DOWN then
                  Action := 'DPAD DOWN'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_LEFT then
                  Action := 'DPAD LEFT'
                else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_RIGHT then
                  Action := 'DPAD RIGHT';
                running := False;
                break;
              end
            end;
          end;
        end;
      end
      else
      begin
        while running do
        begin
          while (SDL_PollEvent(@sdl_event)) = 1 do
          begin
            if sdl_event.type_ = SDL_KEYDOWN then
            begin
              if SDLK_ESCAPE = sdl_event.Key.keysym.sym then
              begin
                running := False;
                break;
              end;
            end
            else
            begin
              if sdl_event.type_ = SDL_JOYAXISMOTION then
              begin
                if sdl_event.jaxis.axis in [0, 2, 4, 6] then
                begin
                  if sdl_event.jaxis.value > 1000 then
                    Action := 'JOY RIGHT'
                  else if sdl_event.jaxis.value < -1000 then
                    Action := 'JOY LEFT'
                end
                else if sdl_event.jaxis.axis in [1, 3, 5, 7] then
                begin
                  if sdl_event.jaxis.value > 1000 then
                    Action := 'JOY DOWN'
                  else if sdl_event.jaxis.value < -1000 then
                    Action := 'JOY UP'
                end;
                running := False;
                break;
              end
              else if sdl_event.type_ = SDL_JOYBUTTONDOWN then
              begin
                Action := 'JOY BUTTON ' + sdl_event.jbutton.button.ToString;
                running := False;
                break;
              end
              else if sdl_event.type_ = SDL_JOYHATMOTION then
              begin
                case SDL_JOYHATMOTION of
                  SDL_HAT_UP:
                    Action := 'JOY UP';
                  SDL_HAT_DOWN:
                    Action := 'JOY DOWN';
                  SDL_HAT_LEFT:
                    Action := 'JOY LEFT';
                  SDL_HAT_RIGHT:
                    Action := 'JOY RIGHT';
                end;
                running := False;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  tmr_cc.Enabled := False;
  count_time_dot := -1;
  if Action = '' then
    temp_text.Text := key_before
  else
    temp_text.Text := Action;
  temp_rect.Fill.color := TAlphaColorRec.Lightgray;
  SDL_JoystickClose(joystick_def[player]);
  joystick_def[player] := nil;
  if game_controller_def[player] <> nil then
  begin
    SDL_GameControllerClose(game_controller_def[player]);
    game_controller_def[player] := nil;
  end;
  Self.BringToFront;
end;

procedure Tfrm_config_controls.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  window_render := nil;
  close_all_frame_stands;
  set_current_controls;
end;

procedure Tfrm_config_controls.FormKeyDown(Sender: TObject; var Key: word; var KeyChar: Char; Shift: TShiftState);
begin
  if tc_cc.TabIndex = 2 then
  begin
    case player.ToInteger of
      1:
        frame_p1_key.Frame.FrameKeyDown(Sender, Key, KeyChar, Shift);
      2:
        frame_p2_key.Frame.FrameKeyDown(Sender, Key, KeyChar, Shift);
      3:
        frame_p3_key.Frame.FrameKeyDown(Sender, Key, KeyChar, Shift);
      4:
        frame_p4_key.Frame.FrameKeyDown(Sender, Key, KeyChar, Shift);
    end;
  end;

  if edit_mode then
    if edit_mode_controller_type = EMCT_Keyboard then
    begin
      tmr_cc.Enabled := False;
      count_time_dot := -1;
      if Key <> 0 then
        temp_text.Text := prj_functions.key_to_string(Key)
      else
        temp_text.Text := UpperCase(KeyChar);
      temp_rect.Fill.color := TAlphaColorRec.Lightgray;
      // case tc_cc.TabIndex of
      // 0:
      // save_key_to_frontend_data(temp_text);
      // 1:
      // save_key_to_ingame_data(temp_text);
      // 2:
      // save_key_to_players_data(temp_text);
      // end;
    end;
end;

procedure Tfrm_config_controls.FormShow(Sender: TObject);
var
  sdl2_handle: integer;
begin
  if Self.StyleBook = nil then
    Self.StyleBook := main.frm_main.stylebook_main;
  player := '1';
  set_frontend_controls;
  set_ingame_controls;

  frame_p1_key := framestand_p1_key.New<Tcontrol_keyboard>(lay_cc_players_p1_key);
  frame_p2_key := framestand_p2_key.New<Tcontrol_keyboard>(lay_cc_players_p2_key);
  frame_p3_key := framestand_p3_key.New<Tcontrol_keyboard>(lay_cc_players_p3_key);
  frame_p4_key := framestand_p4_key.New<Tcontrol_keyboard>(lay_cc_players_p4_key);
  frame_p1_joy := framestand_p1_joy.New<Tcontrol_joystick>(lay_cc_players_p1_joy);
  frame_p2_joy := framestand_p2_joy.New<Tcontrol_joystick>(lay_cc_players_p2_joy);
  frame_p3_joy := framestand_p3_joy.New<Tcontrol_joystick>(lay_cc_players_p3_joy);
  frame_p4_joy := framestand_p4_joy.New<Tcontrol_joystick>(lay_cc_players_p4_joy);
  frame_p1_gpd := framestand_p1_gpd.New<Tcontrol_gamepad>(lay_cc_players_p1_gpd);
  frame_p2_gpd := framestand_p2_gpd.New<Tcontrol_gamepad>(lay_cc_players_p2_gpd);
  frame_p3_gpd := framestand_p3_gpd.New<Tcontrol_gamepad>(lay_cc_players_p3_gpd);
  frame_p4_gpd := framestand_p4_gpd.New<Tcontrol_gamepad>(lay_cc_players_p4_gpd);

  cb_cc_players_platform_type.ItemIndex := 1;
  count_time := -1;
  edit_mode := True;
  edit_frontend;
  edit_mode_controller_type := EMCT_None;
  c_controller := CC_Keyboard;
  tc_cc.TabIndex := 0;
  if window_render = nil then
    window_render := SDL_CreateWindow('', 0, 0, 0, 0, SDL_WINDOW_SHOWN);
  lang_strings;
  selected_emulator := 'Arcade';
  define_keys_from_temp_controllers(CC_Keyboard, 1);

  frm_main.eff_blur_main.Enabled := True;
end;

procedure Tfrm_config_controls.save_key_to_frontend_data(tag: integer; Key: TText);
var
  col_name: string;
begin
  if check_key_in_use(Key) = False then
  begin
    case tag of
      1:
        dm.tKeyboardFrontendquit_dspfm.AsString := Key.Text;
      2:
        dm.tKeyboardFrontendplay.AsString := Key.Text;
      3:
        dm.tKeyboardFrontendmove_up.AsString := Key.Text;
      4:
        dm.tKeyboardFrontendmove_down.AsString := Key.Text;
      5:
        dm.tKeyboardFrontendmove_left.AsString := Key.Text;
      6:
        dm.tKeyboardFrontendmove_right.AsString := Key.Text;
      7:
        dm.tKeyboardFrontendshow_configuration.AsString := Key.Text;
      8:
        dm.tKeyboardFrontendshow_controls.AsString := Key.Text;
      9:
        dm.tKeyboardFrontendshow_display.AsString := Key.Text;
      10:
        dm.tKeyboardFrontendshow_information.AsString := Key.Text;
      11:
        dm.tKeyboardFrontendshow_hide_time_game.AsString := Key.Text;
      12:
        dm.tKeyboardFrontendplatform_emulators.AsString := Key.Text;
    end;

    // set_key_in_current_frontend_key_map(Key.Text, col_name);
  end;
end;

procedure Tfrm_config_controls.save_key_to_ingame_data(Key: TText);
var
  col_name: string;

  // procedure set_key_in_current_ingame_key_map(Key, col_mame: string);
  // begin
  // if col_mame = map_ingame_actions.leave_game_col_name then
  // map_ingame_actions.leave_game := key_num(Key)
  // else if col_mame = map_ingame_actions.pause_game_col_name then
  // map_ingame_actions.pause_game := key_num(Key)
  // else if col_mame = map_ingame_actions.fullscreen_game_col_name then
  // map_ingame_actions.fullscreen_game := key_num(Key)
  // else if col_mame = map_ingame_actions.service_col_name then
  // map_ingame_actions.service := key_num(Key)
  // else if col_mame = map_ingame_actions.fastest_col_name then
  // map_ingame_actions.fastest := key_num(Key)
  // else if col_mame = map_ingame_actions.slow_col_name then
  // map_ingame_actions.slow := key_num(Key)
  // else if col_mame = map_ingame_actions.reset_col_name then
  // map_ingame_actions.reset := key_num(Key)
  // else if col_mame = map_ingame_actions.save_state_player_1_col_name then
  // map_ingame_actions.save_state_player_1 := key_num(Key)
  // else if col_mame = map_ingame_actions.load_state_player_1_col_name then
  // map_ingame_actions.load_state_player_1 := key_num(Key)
  // else if col_mame = map_ingame_actions.save_state_player_2_col_name then
  // map_ingame_actions.save_state_player_2 := key_num(Key)
  // else if col_mame = map_ingame_actions.load_state_player_2_col_name then
  // map_ingame_actions.load_state_player_2 := key_num(Key)
  // else if col_mame = map_ingame_actions.snapshot_col_name then
  // map_ingame_actions.snapshot := key_num(Key)
  // else if col_mame = map_ingame_actions.show_info_col_name then
  // map_ingame_actions.show_info := key_num(Key);
  // end;

begin
  if check_key_in_use(Key) = False then
  begin
    col_name := Key.TagString;
    dm.tKeyboardInGame.Edit;
    dm.tKeyboardInGame.ExecSQL('update key_map_ingame set ' + col_name + '=' + Key.Text + ' where name=default');
    // set_key_in_current_ingame_key_map(Key.Text, col_name);
  end;
end;

procedure Tfrm_config_controls.save_key_to_players_data(Key: TText);
var
  player: integer;
  col_name: string;

  procedure set_key_in_current_player_key_map(Key, col_name: string; player: integer);
  begin
    if p_contrls.map_arcade.ncoin_col_name[player] = col_name then
      p_contrls.map_arcade.ncoin[player] := key_num(Key);
    if p_contrls.map_arcade.nstart_col_name[player] = col_name then
      p_contrls.map_arcade.nstart[player] := key_num(Key);
    if p_contrls.map_arcade.nup_col_name[player] = col_name then
      p_contrls.map_arcade.nup[player] := key_num(Key);
    if p_contrls.map_arcade.ndown_col_name[player] = col_name then
      p_contrls.map_arcade.ndown[player] := key_num(Key);
    if p_contrls.map_arcade.nleft_col_name[player] = col_name then
      p_contrls.map_arcade.nleft[player] := key_num(Key);
    if p_contrls.map_arcade.nright_col_name[player] = col_name then
      p_contrls.map_arcade.nright[player] := key_num(Key);
    if p_contrls.map_arcade.nbut0_col_name[player] = col_name then
      p_contrls.map_arcade.nbut0[player] := key_num(Key);
    if p_contrls.map_arcade.nbut1_col_name[player] = col_name then
      p_contrls.map_arcade.nbut1[player] := key_num(Key);
    if p_contrls.map_arcade.nbut2_col_name[player] = col_name then
      p_contrls.map_arcade.nbut2[player] := key_num(Key);
    if p_contrls.map_arcade.nbut3_col_name[player] = col_name then
      p_contrls.map_arcade.nbut3[player] := key_num(Key);
    if p_contrls.map_arcade.nbut4_col_name[player] = col_name then
      p_contrls.map_arcade.nbut4[player] := key_num(Key);
    if p_contrls.map_arcade.nbut5_col_name[player] = col_name then
      p_contrls.map_arcade.nbut5[player] := key_num(Key);

  end;

begin
  player := tc_cc_players.TabIndex + 1;
  if check_key_in_use(Key) = False then
  begin
    col_name := Key.TagString;
    dm.tKeyboard.Edit;
    dm.tKeyboard.ExecSQL('update key_map set ' + col_name + '=' + Key.Text + ' where num=' + tc_cc_players.TabIndex.ToString);
    set_key_in_current_player_key_map(Key.Text, col_name, player);
  end;
end;

procedure Tfrm_config_controls.sbConfigExitClick(Sender: TObject);
begin
  frm_main.eff_blur_main.Enabled := False;
  Close;
end;

procedure Tfrm_config_controls.sbConfigExitMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).cursor := crHandPoint;
end;

procedure Tfrm_config_controls.set_current_controls;
begin
  p_contrls.map_arcade.joy_up[0] := joy_temp.move.up;
  p_contrls.map_arcade.joy_down[0] := joy_temp.move.down;
  p_contrls.map_arcade.joy_left[0] := joy_temp.move.left;
  p_contrls.map_arcade.joy_right[0] := joy_temp.move.right;
  p_contrls.map_arcade.jbut0[0] := joy_temp.button.b0;
  p_contrls.map_arcade.jbut1[0] := joy_temp.button.b1;
  p_contrls.map_arcade.jbut2[0] := joy_temp.button.b2;
  p_contrls.map_arcade.jbut3[0] := joy_temp.button.b3;
  p_contrls.map_arcade.jbut4[0] := joy_temp.button.b4;
  p_contrls.map_arcade.jbut5[0] := joy_temp.button.b5;
  p_contrls.map_arcade.joy_coin[0] := joy_temp.button.b8;
  p_contrls.map_arcade.joy_start[0] := joy_temp.button.b9;
  p_contrls.map_arcade.num_joystick[0] := 2;
end;

function Tfrm_config_controls.set_edit_mode(mode: boolean): TEDIT_MODE;
begin
  edit_mode := not edit_mode;
  Result.Edit := edit_mode;
  if Result.Edit then
  begin
    Result.color := TAlphaColorRec.Lightgray;
    Result.cursor := crHandPoint;
  end
  else
  begin
    Result.color := TAlphaColorRec.White;
    Result.cursor := crDefault;
  end;
end;

procedure Tfrm_config_controls.set_frontend_controls;
begin
  txt_cc_frontend_quit.Text := dm.tKeyboardFrontendquit_dspfm.AsString;
  txt_cc_frontend_play.Text := dm.tKeyboardFrontendplay.AsString;
  txt_cc_frontend_move_up.Text := dm.tKeyboardFrontendmove_up.AsString;
  txt_cc_frontend_move_down.Text := dm.tKeyboardFrontendmove_down.AsString;
  txt_cc_frontend_move_left.Text := dm.tKeyboardFrontendmove_left.AsString;
  txt_cc_frontend_move_right.Text := dm.tKeyboardFrontendmove_right.AsString;
  txt_cc_frontend_show_configuration.Text := dm.tKeyboardFrontendshow_configuration.AsString;
  txt_cc_frontend_show_controls.Text := dm.tKeyboardFrontendshow_controls.AsString;
  txt_cc_frontend_show_display.Text := dm.tKeyboardFrontendshow_display.AsString;
  txt_cc_frontend_show_information.Text := dm.tKeyboardFrontendshow_information.AsString;
  txt_cc_frontend_time_play.Text := dm.tKeyboardFrontendshow_hide_time_game.AsString;
  txt_cc_frontend_choose_platform.Text := dm.tKeyboardFrontendplatform_emulators.AsString;
end;

procedure Tfrm_config_controls.set_ingame_controls;
begin
  txt_cc_ingame_leave_game.Text := key_name(map_ingame_actions.leave_game);
  txt_cc_ingame_pause_game.Text := key_name(map_ingame_actions.pause_game);
  txt_cc_ingame_fullscreen.Text := key_name(map_ingame_actions.fullscreen_game);
  txt_cc_ingame_reset_game.Text := key_name(map_ingame_actions.reset);
  txt_cc_ingame_service.Text := key_name(map_ingame_actions.service);
  txt_cc_ingame_snapshot.Text := key_name(map_ingame_actions.snapshot);
  txt_cc_ingame_fast.Text := key_name(map_ingame_actions.fastest);
  txt_cc_ingame_slow.Text := key_name(map_ingame_actions.slow);
  txt_cc_ingame_p1_save_state.Text := key_name(map_ingame_actions.save_state_player_1);
  txt_cc_ingame_p1_load_state.Text := key_name(map_ingame_actions.load_state_player_1);
  txt_cc_ingame_p2_save_state.Text := key_name(map_ingame_actions.save_state_player_2);
  txt_cc_ingame_p2_load_state.Text := key_name(map_ingame_actions.load_state_player_2);
end;

procedure Tfrm_config_controls.set_temp_active_controllers(emu_name: string; players: byte);
var
  vi: integer;
  Key, joy, gpd: boolean;
begin
  for vi := 0 to players do
  begin
    Key := active_controllers[0, vi];
    joy := active_controllers[1, vi];
    gpd := active_controllers[2, vi];
    if Key then
    begin
      p_contrls.map_arcade.profile[vi] := temp_key[vi].profile;
      p_contrls.player := temp_key[vi].player;
      p_contrls.platform_type := temp_key[vi].platform_type;
      p_contrls.map_arcade.nup[vi] := temp_key[vi].Key.up;
      p_contrls.map_arcade.ndown[vi] := temp_key[vi].Key.down;
      p_contrls.map_arcade.nleft[vi] := temp_key[vi].Key.left;
      p_contrls.map_arcade.nright[vi] := temp_key[vi].Key.right;
      p_contrls.map_arcade.nbut0[vi] := temp_key[vi].Key.b0;
      p_contrls.map_arcade.nbut1[vi] := temp_key[vi].Key.b1;
      p_contrls.map_arcade.nbut2[vi] := temp_key[vi].Key.b2;
      p_contrls.map_arcade.nbut3[vi] := temp_key[vi].Key.b3;
      p_contrls.map_arcade.nbut4[vi] := temp_key[vi].Key.b4;
      p_contrls.map_arcade.nbut5[vi] := temp_key[vi].Key.b5;
      p_contrls.map_arcade.ncoin[vi] := temp_key[vi].Key.coin;
      p_contrls.map_arcade.nstart[vi] := temp_key[vi].Key.start;
    end
    else if joy then
    begin
      p_contrls.map_arcade.profile[vi] := temp_joy[vi].profile;
      p_contrls.map_arcade.name[vi] := temp_joy[vi].name;
      p_contrls.map_arcade.player[vi] := temp_joy[vi].player;
      p_contrls.map_arcade.control_type[vi] := temp_joy[vi].vtype;
      p_contrls.map_arcade.serial[vi] := temp_joy[vi].serial;
      p_contrls.map_arcade.vendor[vi] := temp_joy[vi].vendor;
      p_contrls.map_arcade.guid[vi] := temp_joy[vi].guid;
      p_contrls.map_arcade.product[vi] := temp_joy[vi].product;
      p_contrls.map_arcade.product_version[vi] := temp_joy[vi].product_version;
      p_contrls.map_arcade.buttons[vi] := temp_joy[vi].buttons;
      p_contrls.map_arcade.joy_up[vi] := temp_joy[vi].joy.up;
      p_contrls.map_arcade.joy_down[vi] := temp_joy[vi].joy.down;
      p_contrls.map_arcade.joy_left[vi] := temp_joy[vi].joy.left;
      p_contrls.map_arcade.joy_right[vi] := temp_joy[vi].joy.right;
      p_contrls.map_arcade.jbut0[vi] := temp_joy[vi].joy.b0;
      p_contrls.map_arcade.jbut1[vi] := temp_joy[vi].joy.b1;
      p_contrls.map_arcade.jbut2[vi] := temp_joy[vi].joy.b2;
      p_contrls.map_arcade.jbut3[vi] := temp_joy[vi].joy.b3;
      p_contrls.map_arcade.jbut4[vi] := temp_joy[vi].joy.b4;
      p_contrls.map_arcade.jbut5[vi] := temp_joy[vi].joy.b5;
      p_contrls.map_arcade.jbut6[vi] := temp_joy[vi].joy.b6;
      p_contrls.map_arcade.jbut7[vi] := temp_joy[vi].joy.b7;
      p_contrls.map_arcade.jbut8[vi] := temp_joy[vi].joy.b8;
      p_contrls.map_arcade.jbut9[vi] := temp_joy[vi].joy.b9;
      p_contrls.map_arcade.jbut10[vi] := temp_joy[vi].joy.b10;
      p_contrls.map_arcade.jbut11[vi] := temp_joy[vi].joy.b11;
      p_contrls.map_arcade.jbut12[vi] := temp_joy[vi].joy.b12;
      p_contrls.map_arcade.jbut13[vi] := temp_joy[vi].joy.b13;
      p_contrls.map_arcade.jbut14[vi] := temp_joy[vi].joy.b14;
      p_contrls.map_arcade.jbut15[vi] := temp_joy[vi].joy.b15;
      p_contrls.map_arcade.deadzone_x[vi] := temp_joy[vi].joy.deadzone_x;
      p_contrls.map_arcade.deadzone_y[vi] := temp_joy[vi].joy.deadzone_y;
      p_contrls.map_arcade.joy_coin[vi] := temp_joy[vi].joy.coin;
      p_contrls.map_arcade.joy_start[vi] := temp_joy[vi].joy.start;
    end
    else if gpd then
    begin
      // too much work to do
    end;
  end;
end;

procedure Tfrm_config_controls.show_frame(player: string; controller: TCURRENT_CONTROLLER);
var
  sdl2_handle: integer;
begin
  // Hide frames
  frame_p1_key.Hide;
  frame_p1_joy.Hide;
  frame_p1_gpd.Hide;
  frame_p2_key.Hide;
  frame_p2_joy.Hide;
  frame_p2_gpd.Hide;
  frame_p3_key.Hide;
  frame_p3_joy.Hide;
  frame_p3_gpd.Hide;
  frame_p4_key.Hide;
  frame_p4_joy.Hide;
  frame_p4_gpd.Hide;

  // Open and show only the selected one
  case controller of
    CC_Keyboard:
      begin
        case player.ToInteger of
          0:
            begin
              frame_p1_key.show;
              frame_p1_key.Frame.show_cur_frame;
            end;
          1:
            begin
              frame_p2_key.show;
              frame_p2_key.Frame.show_cur_frame;
            end;
          2:
            begin
              frame_p3_key.show;
              frame_p3_key.Frame.show_cur_frame;
            end;
          3:
            begin
              frame_p4_key.show;
              frame_p4_key.Frame.show_cur_frame;
            end;
        end;
      end;
    CC_Joy_Arcade_1:
      begin
        case player.ToInteger of
          0:
            frame_p1_joy.show;

          1:
            frame_p2_joy.show;

          2:
            frame_p3_joy.show;

          3:
            frame_p4_joy.show;
        end;
      end;
    CC_GP_PS4:
      begin
        case player.ToInteger of
          0:
            frame_p1_gpd.show;
          1:
            frame_p2_gpd.show;
          2:
            frame_p3_gpd.show;
          3:
            frame_p4_gpd.show;
        end;
        window_render.W := 1;
        window_render.h := 1;
      end;
  end;
end;

procedure Tfrm_config_controls.show_pop_cb(Sender: TObject);
begin
  txt_cc_footer_info.Text := lang.getTransStringPop(-1);
end;

procedure Tfrm_config_controls.show_pop_cbe(Sender: TObject);
begin
  txt_cc_footer_info.Text := lang.getTransStringPop(-1);
end;

procedure Tfrm_config_controls.show_pop_rect(Sender: TObject);
begin
  txt_cc_footer_info.Text := lang.getTransStringPop(-1);
end;

procedure Tfrm_config_controls.show_pop_spb(Sender: TObject);
begin
  txt_cc_footer_info.Text := lang.getTransStringPop(-1);
end;

procedure Tfrm_config_controls.show_pop_tbi(Sender: TObject);
begin
  txt_cc_footer_info.Text := lang.getTransStringPop(-1);
end;

function Tfrm_config_controls.get_controllers_profile: TStringList;
begin
  Result := TStringList.Create;
  dm.query.SQL.Text := 'select distinct name from player';

  while not dm.query.Eof do
  begin
    Result.Add(dm.query.Fields[0].AsString);
    dm.query.Next;
  end;
end;

procedure Tfrm_config_controls.get_controller_data;
var
  player, joy_sel, dev_index, power_level, mappings: integer;

  function get_powerlevel_controller(index: integer): string;
  begin
    case index of
      - 1:
        Result := 'Unknown';
      0:
        Result := '0';
      1:
        Result := '20';
      2:
        Result := '70';
      3:
        Result := '100';
      4:
        Result := 'usb';
      5:
        Result := 'max';
    else
      Result := index.ToString + '%';
    end;
  end;

  procedure draw_power_level(num: integer);
  var
    temp: integer;
  begin
    rect_cc_players_joystick_battery_5.Visible := True;
    rect_cc_players_joystick_battery_25.Visible := True;
    rect_cc_players_joystick_battery_50.Visible := True;
    rect_cc_players_joystick_battery_75.Visible := True;
    rect_cc_players_joystick_battery_90.Visible := True;
    rect_cc_players_joystick_battery_100.Visible := True;

    if ((num = 3) or (num = 100)) then
      txt_cc_players_joystick_battery_percent.Text := '100%'
    else if ((num < 100) and (num >= 90)) then
    begin
      rect_cc_players_joystick_battery_100.Visible := False;
      txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end
    else if ((num >= 70) and (num < 90)) or (num = 2) then
    begin
      rect_cc_players_joystick_battery_90.Visible := False;
      rect_cc_players_joystick_battery_100.Visible := False;
      if num = 2 then
        temp := 70;
      txt_cc_players_joystick_battery_percent.Text := temp.ToString + '%';
    end
    else if ((num >= 50) and (num < 70)) then
    begin
      rect_cc_players_joystick_battery_75.Visible := False;
      rect_cc_players_joystick_battery_90.Visible := False;
      rect_cc_players_joystick_battery_100.Visible := False;
      txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end
    else if ((num >= 20) and (num < 50)) or (num = 1) then
    begin
      rect_cc_players_joystick_battery_50.Visible := False;
      rect_cc_players_joystick_battery_75.Visible := False;
      rect_cc_players_joystick_battery_90.Visible := False;
      rect_cc_players_joystick_battery_100.Visible := False;
      if num = 1 then
        temp := 20;
      txt_cc_players_joystick_battery_percent.Text := temp.ToString + '%';
    end
    else if ((num >= 5) and (num < 20)) then
    begin
      rect_cc_players_joystick_battery_25.Visible := False;
      rect_cc_players_joystick_battery_50.Visible := False;
      rect_cc_players_joystick_battery_75.Visible := False;
      rect_cc_players_joystick_battery_90.Visible := False;
      rect_cc_players_joystick_battery_100.Visible := False;
      txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end
    else if ((num >= 0) and (num < 5)) or (num = 0) then
    begin
      rect_cc_players_joystick_battery_5.Visible := False;
      rect_cc_players_joystick_battery_25.Visible := False;
      rect_cc_players_joystick_battery_50.Visible := False;
      rect_cc_players_joystick_battery_75.Visible := False;
      rect_cc_players_joystick_battery_90.Visible := False;
      rect_cc_players_joystick_battery_100.Visible := False;
      txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end;
  end;

  function get_type_controller(index: integer): string;
  begin
    case index of
      0:
        Result := 'Unknown';
      1:
        Result := 'Game Controller';
      2:
        Result := 'Wheel';
      3:
        Result := 'Arcade Stick';
      4:
        Result := 'Flight Stick';
      5:
        Result := 'Dance Pad';
      6:
        Result := 'Guitar';
      7:
        Result := 'Drum Kit';
      8:
        Result := 'Arcade Pad';
      9:
        Result := 'Throttle';
    end;
  end;

begin
  player := tc_cc_players.TabIndex;
  joy_sel := cb_cc_players_controller.ItemIndex - 1;

  joystick_def[player] := SDL_JoystickOpen(joy_sel);
  dev_index := SDL_JoystickGetPlayerIndex(joystick_def[player]);
  joy_data[player].name := SDL_JoystickName(joystick_def[player]);
  joy_data[player].vtype := get_type_controller(SDL_JoystickGetType(joystick_def[player]));
  joy_data[player].serial := SDL_JoystickGetSerial(joystick_def[player]);
  joy_data[player].buttons := SDL_JoystickNumButtons(joystick_def[player]).ToString;
  joy_data[player].guid := SDL_JoystickGetGUID(joystick_def[player]);
  joy_data[player].vendor := SDL_JoystickGetVendor(joystick_def[player]);
  joy_data[player].product := SDL_JoystickGetProduct(joystick_def[player]);
  joy_data[player].product_version := SDL_JoystickGetProductVersion(joystick_def[player]);
  joy_data[player].power := get_powerlevel_controller(SDL_JoystickCurrentPowerLevel(joystick_def[player]));
  power_level := SDL_JoystickCurrentPowerLevel(joystick_def[player]);

  lbl_cc_players_joystick_name_value.Text := joy_data[player].name;
  lbl_cc_players_joystick_buttons_value.Text := joy_data[player].buttons;
  lbl_cc_players_joystick_type_value.Text := joy_data[player].vtype;
  lbl_cc_players_joystick_vendor_value.Text := joy_data[player].vendor.ToString;
  lbl_cc_players_joystick_serial_value.Text := joy_data[player].serial;
  lbl_cc_players_joystick_product_value.Text := joy_data[player].product.ToString;
  lbl_cc_players_joystick_product_version_value.Text := joy_data[player].product_version.ToString;
  if (power_level = -1) or (power_level = 4) then
  begin
    rect_cc_players_joystick_battery.Visible := False;
    img_cc_players_joystick_usb.Visible := True;
  end
  else
  begin
    img_cc_players_joystick_usb.Visible := False;
    rect_cc_players_joystick_battery.Visible := True;
    draw_power_level(power_level);
  end;
  SDL_JoystickClose(joystick_def[player]);
end;

procedure Tfrm_config_controls.get_ingame_controls;
begin
  dm.tKeyboardInGame.Filtered := False;
  dm.tKeyboardInGame.Filter := 'name = ''Default''';
  dm.tKeyboardInGame.Filtered := True;

  map_ingame_actions.name := dm.tKeyboardInGamename.AsString;
  map_ingame_actions.leave_game := dm.tKeyboardInGameleave_game.AsLongWord;
  map_ingame_actions.pause_game := dm.tKeyboardInGamepause_game.AsLongWord;
  map_ingame_actions.fullscreen_game := dm.tKeyboardInGamefullscreen_game.AsLongWord;
  map_ingame_actions.service := dm.tKeyboardInGameservice.AsLongWord;
  map_ingame_actions.fastest := dm.tKeyboardInGamefastest.AsLongWord;
  map_ingame_actions.slow := dm.tKeyboardInGameslow.AsLongWord;
  map_ingame_actions.reset := dm.tKeyboardInGamereset.AsLongWord;
  map_ingame_actions.save_state_player_1 := dm.tKeyboardInGamesave_snap_player_1.AsLongWord;
  map_ingame_actions.save_state_player_2 := dm.tKeyboardInGamesave_snap_player_2.AsLongWord;
  map_ingame_actions.load_state_player_1 := dm.tKeyboardInGameload_snap_player_1.AsLongWord;
  map_ingame_actions.load_state_player_2 := dm.tKeyboardInGameload_snap_player_2.AsLongWord;
  map_ingame_actions.snapshot := dm.tKeyboardInGamesnapshot.AsLongWord;
  map_ingame_actions.show_info := dm.tKeyboardInGameshow_info.AsLongWord;

  dm.tKeyboardInGame.Filtered := False;
end;

function Tfrm_config_controls.get_key_map(player, emulator: string; control_type: TCURRENT_CONTROLLER): string;
begin
  dm.tPlayers.Filtered := False;
  case control_type of
    CC_Keyboard:
      dm.tPlayers.Filter := 'key=1 AND player=' + player + ' AND platform=' + emulator + ' AND active=1';
    CC_Joy_Arcade_1:
      dm.tPlayers.Filter := 'joy=1 AND player=' + player + ' AND platform=' + emulator + ' AND active=1';
    CC_GP_PS4:
      dm.tPlayers.Filter := 'gpd=1 AND player=' + player + ' AND platform=' + emulator + ' AND active=1';
  end;
  dm.tPlayers.Filtered := True;
  Result := dm.tPlayerskey_map_num.AsString;
  dm.tPlayers.Filtered := False;
end;

function Tfrm_config_controls.get_platform_type: string;
begin
  Result := cb_cc_players_platform_type.Items[cb_cc_players_platform_type.ItemIndex];
end;

procedure Tfrm_config_controls.get_players_controls;
var
  profile_list: TStringList;
  vi: integer;
begin
  profile_list := get_controllers_profile;

  for vi := 0 to profile_list.Count - 1 do
    cbe_cc_players_profile.Items[vi] := profile_list.Strings[vi];

  get_temp_active_controllers('arcade', 3);
  set_temp_active_controllers('arcade', 3);

  open_joystick(2);
end;

procedure Tfrm_config_controls.get_set_joysticks;
var
  vi, Count: integer;
begin
  Count := SDL_NumJoysticks;
  for vi := 0 to Count - 1 do
    cb_cc_players_controller.Items.Insert(vi + 1, (SDL_JoystickNameForIndex(vi)));
end;

procedure Tfrm_config_controls.get_temp_active_controllers(emu_name: string; players: byte);
var
  vi: integer;
  id: string;
  profile_name: string;
  Key, joy, gpd: boolean;
  key_map_num: string;
begin

  // for vi := 0 to players do
  // begin
  // Key := False;
  // joy := False;
  // gpd := False;
  // key_map_num := '';
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text :=
  // 'SELECT name, key, joy, gpd FROM players WHERE player=:player AND platform=:platform AND selected=:selected AND active=:active';
  // DSPFM_Data.Query.ParamByName('player').AsString := (vi + 1).ToString;
  // DSPFM_Data.Query.ParamByName('platform').AsString := emu_name;
  // DSPFM_Data.Query.ParamByName('selected').AsString := '1';
  // DSPFM_Data.Query.ParamByName('active').AsString := '1';
  // DSPFM_Data.Query.Open;
  //
  // profile_name := DSPFM_Data.Query.FieldByName('name').AsString;
  // Key := (DSPFM_Data.Query.FieldByName('key').AsInteger).ToBoolean;
  // joy := (DSPFM_Data.Query.FieldByName('joy').AsInteger).ToBoolean;
  // gpd := (DSPFM_Data.Query.FieldByName('gpd').AsInteger).ToBoolean;
  //
  // if Key then
  // begin
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text :=
  // 'SELECT key_map_num FROM players WHERE platform=:platform AND name=:name AND player=:player AND selected=:selected AND active=:active AND key=:key';
  // DSPFM_Data.Query.ParamByName('platform').AsString := emu_name;
  // DSPFM_Data.Query.ParamByName('name').AsString := profile_name;
  // DSPFM_Data.Query.ParamByName('player').AsString := (vi + 1).ToString;
  // DSPFM_Data.Query.ParamByName('selected').AsString := '1';
  // DSPFM_Data.Query.ParamByName('active').AsString := '1';
  // DSPFM_Data.Query.ParamByName('key').AsString := '1';
  // DSPFM_Data.Query.Open;
  //
  // key_map_num := DSPFM_Data.Query.Fields[0].AsString;
  //
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'SELECT * FROM key_map WHERE num=:num';
  // DSPFM_Data.Query.ParamByName('num').AsString := key_map_num;
  // DSPFM_Data.Query.Open;
  //
  // temp_key[vi].profile := profile_name;
  // temp_key[vi].player := (vi + 1).ToString;
  // temp_key[vi].platform_type := emu_name;
  // temp_key[vi].Key.up := DSPFM_Data.Query.FieldByName('key_up').AsLongWord;
  // temp_key[vi].Key.down := DSPFM_Data.Query.FieldByName('key_down').AsLongWord;
  // temp_key[vi].Key.left := DSPFM_Data.Query.FieldByName('key_left').AsLongWord;
  // temp_key[vi].Key.right := DSPFM_Data.Query.FieldByName('key_right').AsLongWord;
  // temp_key[vi].Key.b0 := DSPFM_Data.Query.FieldByName('key_b0').AsLongWord;
  // temp_key[vi].Key.b1 := DSPFM_Data.Query.FieldByName('key_b1').AsLongWord;
  // temp_key[vi].Key.b2 := DSPFM_Data.Query.FieldByName('key_b2').AsLongWord;
  // temp_key[vi].Key.b3 := DSPFM_Data.Query.FieldByName('key_b3').AsLongWord;
  // temp_key[vi].Key.b4 := DSPFM_Data.Query.FieldByName('key_b4').AsLongWord;
  // temp_key[vi].Key.b5 := DSPFM_Data.Query.FieldByName('key_b5').AsLongWord;
  // temp_key[vi].Key.coin := DSPFM_Data.Query.FieldByName('key_coin').AsLongWord;
  // temp_key[vi].Key.start := DSPFM_Data.Query.FieldByName('key_start').AsLongWord;
  //
  // active_controllers[0, vi] := True;
  // end
  // else if joy then
  // begin
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text :=
  // 'SELECT key_map_mum FROM players WHERE platform=:platform AND name=:name AND player=:player AND selected=:selected AND active=:active AND joy=:joy';
  // DSPFM_Data.Query.ParamByName('platform').AsString := emu_name;
  // DSPFM_Data.Query.ParamByName('name').AsString := profile_name;
  // DSPFM_Data.Query.ParamByName('player').AsString := (vi + 1).ToString;
  // DSPFM_Data.Query.ParamByName('selected').AsString := '1';
  // DSPFM_Data.Query.ParamByName('active').AsString := '1';
  // DSPFM_Data.Query.ParamByName('joy').AsString := '1';
  // DSPFM_Data.Query.Open;
  //
  // key_map_num := DSPFM_Data.Query.Fields[0].AsString;
  //
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'SELECT * FROM joy_map WHERE num=:num';
  // DSPFM_Data.Query.ParamByName('num').AsString := key_map_num;
  // DSPFM_Data.Query.Open;
  //
  // temp_joy[vi].profile := profile_name;
  // temp_joy[vi].player := (vi + 1).ToString;
  // temp_joy[vi].name := DSPFM_Data.Query.FieldByName('jname').AsString;
  // temp_joy[vi].vtype := DSPFM_Data.Query.FieldByName('jtype').AsString;
  // temp_joy[vi].serial := DSPFM_Data.Query.FieldByName('serial').AsString;
  // temp_joy[vi].buttons := DSPFM_Data.Query.FieldByName('buttons').AsString;
  // temp_joy[vi].guid := DSPFM_Data.Query.FieldByName('guid').AsString;
  // temp_joy[vi].vendor := DSPFM_Data.Query.FieldByName('vendor').AsString;
  // temp_joy[vi].product := DSPFM_Data.Query.FieldByName('product').AsString;
  // temp_joy[vi].product_version := DSPFM_Data.Query.FieldByName('product_version').AsString;
  // temp_joy[vi].joy.up := DSPFM_Data.Query.FieldByName('axis_y_plus').AsInteger;
  // temp_joy[vi].joy.down := DSPFM_Data.Query.FieldByName('axis_y_minus').AsInteger;
  // temp_joy[vi].joy.left := DSPFM_Data.Query.FieldByName('axis_x_minus').AsInteger;
  // temp_joy[vi].joy.right := DSPFM_Data.Query.FieldByName('axis_x_plus').AsInteger;
  // temp_joy[vi].joy.b0 := DSPFM_Data.Query.FieldByName('b0').AsInteger;
  // temp_joy[vi].joy.b1 := DSPFM_Data.Query.FieldByName('b1').AsInteger;
  // temp_joy[vi].joy.b2 := DSPFM_Data.Query.FieldByName('b2').AsInteger;
  // temp_joy[vi].joy.b3 := DSPFM_Data.Query.FieldByName('b3').AsInteger;
  // temp_joy[vi].joy.b4 := DSPFM_Data.Query.FieldByName('b4').AsInteger;
  // temp_joy[vi].joy.b5 := DSPFM_Data.Query.FieldByName('b5').AsInteger;
  // temp_joy[vi].joy.b6 := DSPFM_Data.Query.FieldByName('b6').AsInteger;
  // temp_joy[vi].joy.b7 := DSPFM_Data.Query.FieldByName('b7').AsInteger;
  // temp_joy[vi].joy.b8 := DSPFM_Data.Query.FieldByName('b8').AsInteger;
  // temp_joy[vi].joy.b9 := DSPFM_Data.Query.FieldByName('b9').AsInteger;
  // temp_joy[vi].joy.b10 := DSPFM_Data.Query.FieldByName('b10').AsInteger;
  // temp_joy[vi].joy.b11 := DSPFM_Data.Query.FieldByName('b11').AsInteger;
  // temp_joy[vi].joy.b12 := DSPFM_Data.Query.FieldByName('b12').AsInteger;
  // temp_joy[vi].joy.b13 := DSPFM_Data.Query.FieldByName('b13').AsInteger;
  // temp_joy[vi].joy.b14 := DSPFM_Data.Query.FieldByName('b14').AsInteger;
  // temp_joy[vi].joy.b15 := DSPFM_Data.Query.FieldByName('b15').AsInteger;
  // temp_joy[vi].joy.coin := DSPFM_Data.Query.FieldByName('coin').AsInteger;
  // temp_joy[vi].joy.start := DSPFM_Data.Query.FieldByName('start').AsInteger;
  // temp_joy[vi].joy.deadzone_x := DSPFM_Data.Query.FieldByName('deadzone_x').AsInteger;
  // temp_joy[vi].joy.deadzone_y := DSPFM_Data.Query.FieldByName('deadzone_y').AsInteger;
  // active_controllers[1, vi] := True;
  // end
  // else if gpd then
  // begin
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text :=
  // 'SELECT key_map_mum FROM players WHERE platform=:platform AND name=:name AND player=:player AND selected=:selected AND active=:active AND gpd=:gpd';
  // DSPFM_Data.Query.ParamByName('platform').AsString := emu_name;
  // DSPFM_Data.Query.ParamByName('name').AsString := profile_name;
  // DSPFM_Data.Query.ParamByName('player').AsString := (vi + 1).ToString;
  // DSPFM_Data.Query.ParamByName('selected').AsString := '1';
  // DSPFM_Data.Query.ParamByName('active').AsString := '1';
  // DSPFM_Data.Query.ParamByName('gpd').AsString := '1';
  // DSPFM_Data.Query.Open;
  //
  // key_map_num := DSPFM_Data.Query.Fields[0].AsString;
  //
  // DSPFM_Data.Query.Close;
  // DSPFM_Data.Query.SQL.Clear;
  // DSPFM_Data.Query.SQL.Text := 'SELECT * FROM gpd_map WHERE num=:num';
  // DSPFM_Data.Query.ParamByName('num').AsString := key_map_num;
  // DSPFM_Data.Query.Open;
  //
  // temp_gpd[vi].profile := profile_name;
  // temp_gpd[vi].player := (vi + 1).ToString;
  // temp_gpd[vi].name := DSPFM_Data.Query.FieldByName('gname').AsString;
  // temp_gpd[vi].vtype := DSPFM_Data.Query.FieldByName('gtype').AsString;
  // temp_gpd[vi].serial := DSPFM_Data.Query.FieldByName('serial').AsString;
  // temp_gpd[vi].buttons := DSPFM_Data.Query.FieldByName('buttons').AsString;
  // temp_gpd[vi].guid := DSPFM_Data.Query.FieldByName('guid').AsString;
  // temp_gpd[vi].vendor := DSPFM_Data.Query.FieldByName('vendor').AsString;
  // temp_gpd[vi].product := DSPFM_Data.Query.FieldByName('product').AsString;
  // temp_gpd[vi].product_version := DSPFM_Data.Query.FieldByName('product_version').AsString;
  // temp_gpd[vi].gpd.hat_up := DSPFM_Data.Query.FieldByName('hat_up').AsInteger;
  // temp_gpd[vi].gpd.hat_down := DSPFM_Data.Query.FieldByName('hat_down').AsInteger;
  // temp_gpd[vi].gpd.hat_left := DSPFM_Data.Query.FieldByName('hat_left').AsInteger;
  // temp_gpd[vi].gpd.hat_right := DSPFM_Data.Query.FieldByName('hat_right').AsInteger;
  // temp_gpd[vi].gpd.lx_plus := DSPFM_Data.Query.FieldByName('left_x_plus').AsInteger;
  // temp_gpd[vi].gpd.lx_minus := DSPFM_Data.Query.FieldByName('left_x_minus').AsInteger;
  // temp_gpd[vi].gpd.ly_plus := DSPFM_Data.Query.FieldByName('left_y_plus').AsInteger;
  // temp_gpd[vi].gpd.ly_minus := DSPFM_Data.Query.FieldByName('left_y_minus').AsInteger;
  // temp_gpd[vi].gpd.rx_plus := DSPFM_Data.Query.FieldByName('right_x_plus').AsInteger;
  // temp_gpd[vi].gpd.rx_minus := DSPFM_Data.Query.FieldByName('right_x_minus').AsInteger;
  // temp_gpd[vi].gpd.ry_plus := DSPFM_Data.Query.FieldByName('right_y_plus').AsInteger;
  // temp_gpd[vi].gpd.ry_minus := DSPFM_Data.Query.FieldByName('right_y_minus').AsInteger;
  // temp_gpd[vi].gpd.b0 := DSPFM_Data.Query.FieldByName('b0').AsInteger;
  // temp_gpd[vi].gpd.b1 := DSPFM_Data.Query.FieldByName('b1').AsInteger;
  // temp_gpd[vi].gpd.b2 := DSPFM_Data.Query.FieldByName('b2').AsInteger;
  // temp_gpd[vi].gpd.b3 := DSPFM_Data.Query.FieldByName('b3').AsInteger;
  // temp_gpd[vi].gpd.b4 := DSPFM_Data.Query.FieldByName('b4').AsInteger;
  // temp_gpd[vi].gpd.b5 := DSPFM_Data.Query.FieldByName('b5').AsInteger;
  // temp_gpd[vi].gpd.b6 := DSPFM_Data.Query.FieldByName('b6').AsInteger;
  // temp_gpd[vi].gpd.b7 := DSPFM_Data.Query.FieldByName('b7').AsInteger;
  // temp_gpd[vi].gpd.b8 := DSPFM_Data.Query.FieldByName('b8').AsInteger;
  // temp_gpd[vi].gpd.b9 := DSPFM_Data.Query.FieldByName('b9').AsInteger;
  // temp_gpd[vi].gpd.b10 := DSPFM_Data.Query.FieldByName('b10').AsInteger;
  // temp_gpd[vi].gpd.b11 := DSPFM_Data.Query.FieldByName('b11').AsInteger;
  // temp_gpd[vi].gpd.b12 := DSPFM_Data.Query.FieldByName('b12').AsInteger;
  // temp_gpd[vi].gpd.b13 := DSPFM_Data.Query.FieldByName('b13').AsInteger;
  // temp_gpd[vi].gpd.b14 := DSPFM_Data.Query.FieldByName('b14').AsInteger;
  // temp_gpd[vi].gpd.b15 := DSPFM_Data.Query.FieldByName('b15').AsInteger;
  // temp_gpd[vi].gpd.coin := DSPFM_Data.Query.FieldByName('coin').AsInteger;
  // temp_gpd[vi].gpd.start := DSPFM_Data.Query.FieldByName('start').AsInteger;
  // temp_gpd[vi].gpd.trigger_left := DSPFM_Data.Query.FieldByName('trigger_left').AsInteger;
  // temp_gpd[vi].gpd.trigger_right := DSPFM_Data.Query.FieldByName('trigger_right').AsInteger;
  // temp_gpd[vi].gpd.left := DSPFM_Data.Query.FieldByName('left').AsInteger;
  // temp_gpd[vi].gpd.right := DSPFM_Data.Query.FieldByName('right').AsInteger;
  // temp_gpd[vi].gpd.left_deadzone := DSPFM_Data.Query.FieldByName('left_deadzone').AsInteger;
  // temp_gpd[vi].gpd.right_deadzone := DSPFM_Data.Query.FieldByName('right_deadzone').AsInteger;
  // active_controllers[2, vi] := True;
  // end;
  // end;
end;

//
procedure Tfrm_config_controls.lang_strings;
begin
  Self.Caption := lang.getTransString(clCONFIGURATE_CONTROLS_INPUT);
  lbl_cc_header.Text := lang.getTransString(clCONFIGURATE_CONTROLS);
  // FrontEnd
  tbi_cc_frontend.Text := lang.getTransString(clFRONTEND);
  tbi_cc_ingame.Text := lang.getTransString(clIN_GAME);
  tbi_cc_players.Text := lang.getTransString(clPLAYERS);
  lbl_cc_frontend_quit.Text := lang.getTransString(clQUIT_DEmuFM);
  lbl_cc_frontend_play.Text := lang.getTransString(clPLAY);
  lbl_cc_frontend_move_up.Text := lang.getTransString(clMOVE_UP);
  lbl_cc_frontend_move_down.Text := lang.getTransString(clMOVE_DOWN);
  lbl_cc_frontend_move_left.Text := lang.getTransString(clMOVE_LEFT);
  lbl_cc_frontend_move_right.Text := lang.getTransString(clMOVE_RIGHT);
  lbl_cc_frontend_show_configuration.Text := lang.getTransString(clSHOW_CONFIGURATION);
  lbl_cc_frontend_show_controls.Text := lang.getTransString(clSHOW_CONFIGURATE_CONTROLS);
  lbl_cc_frontend_show_display.Text := lang.getTransString(clSHOW_CONFIGURATE_DISPLAY);
  lbl_cc_frontend_show_information.Text := lang.getTransString(clSHOW_INFORMATION);
  lbl_cc_frontend_choose_platform.Text := lang.getTransString(clCHOOSE_PLATFORM_EMULATION);
  lbl_cc_frontend_time_play.Text := lang.getTransString(clSHOW_HIDE_TIME_PLAY);
  // In Game
  lbl_cc_ingame_leave_game.Text := lang.getTransString(clLEAVE_GAME);
  lbl_cc_ingame_pause_game.Text := lang.getTransString(clPAUSE_GAME);
  lbl_cc_ingame_fullscreen.Text := lang.getTransString(clFULLSCREEN);
  lbl_cc_ingame_reset_game.Text := lang.getTransString(clRESET_GAME);
  lbl_cc_ingame_service.Text := lang.getTransString(clSERVICE);
  lbl_cc_ingame_snapshot.Text := lang.getTransString(clTAKE_SNAPSHOT_PNG_JPG);
  lbl_cc_ingame_fast.Text := lang.getTransString(clFAST_FPS);
  lbl_cc_ingame_slow.Text := lang.getTransString(clSLOW_FPS);
  lbl_cc_ingame_p1_save_state.Text := lang.getTransString(clPLAYER_1_SAVE_STATE);
  lbl_cc_ingame_p1_load_state.Text := lang.getTransString(clPLAYER_1_LOAD_STATE);
  lbl_cc_ingame_p2_save_state.Text := lang.getTransString(clPLAYER_2_SAVE_STATE);
  lbl_cc_ingame_p2_load_state.Text := lang.getTransString(clPLAYER_2_LOAD_STATE);
  // Players
  lbl_cc_players_platform_type.Text := lang.getTransString(clEMULATION_PLATFORM);
  lbl_cc_players_profile.Text := lang.getTransString(clPROFILE);
  spb_cc_players_profile_load.Text := lang.getTransString(clLOAD);
  spb_cc_players_profile_save.Text := lang.getTransString(clSAVE);
  lbl_players_arcade_contrl_choose.Text := lang.getTransString(clCONTROLLER);
  cb_cc_players_controller.Items.Clear;
  cb_cc_players_controller.Items.Add(lang.getTransString(clKEYBOARD));
  cb_cc_players_controller.ItemIndex := 0;
  get_set_joysticks;
  tbi_cc_players_p1.Text := lang.getTransString(clPLAYER) + ' 1';
  tbi_cc_players_p2.Text := lang.getTransString(clPLAYER) + ' 2';
  tbi_cc_players_p3.Text := lang.getTransString(clPLAYER) + ' 3';
  tbi_cc_players_p4.Text := lang.getTransString(clPLAYER) + ' 4';
end;

procedure Tfrm_config_controls.rect_onClick(Sender: TObject);
var
  name: string;
  vComp: TComponent;
begin
  if edit_mode and (tmr_cc.Enabled = False) then
  begin
    (Sender as TRectangle).Fill.color := TAlphaColorRec.Lightskyblue;
    temp_rect := (Sender as TRectangle);
    name := (Sender as TRectangle).name;
    Delete(name, 1, 4);
    name := 'txt' + name;
    vComp := Self.FindComponent(name);
    key_before := (vComp as TText).Text;
    (vComp as TText).Text := '';
    temp_text := (vComp as TText);
    count_time := 5;
    tmr_cc.Enabled := True;
    if cb_cc_players_controller.ItemIndex > 0 then
      enable_sdl_joystick_event;
  end;
end;

procedure Tfrm_config_controls.spb_cc_players_joystick_infoClick(Sender: TObject);
begin
  rect_cc_players_joystick_info.Visible := not rect_cc_players_joystick_info.Visible;
end;

procedure Tfrm_config_controls.tbi_cc_frontendClick(Sender: TObject);
begin
  edit_mode := True;
  edit_ingame;
end;

procedure Tfrm_config_controls.tbi_cc_ingameClick(Sender: TObject);
begin
  edit_mode := True;
  edit_frontend;
end;

procedure Tfrm_config_controls.tbi_cc_playersClick(Sender: TObject);
begin
  tc_cc_players.TabIndex := 0;
  edit_mode := True;
  edit_ingame;
  edit_mode := True;
  edit_frontend;
end;

procedure Tfrm_config_controls.tbi_cc_players_p1Click(Sender: TObject);
begin
  player := '0';
  cb_cc_players_platform_type.OnChange(cb_cc_players_platform_type);
end;

procedure Tfrm_config_controls.tbi_cc_players_p2Click(Sender: TObject);
begin
  player := '1';
  cb_cc_players_platform_type.OnChange(cb_cc_players_platform_type);
end;

procedure Tfrm_config_controls.tbi_cc_players_p3Click(Sender: TObject);
begin
  player := '2';
  cb_cc_players_platform_type.OnChange(cb_cc_players_platform_type);
end;

procedure Tfrm_config_controls.tbi_cc_players_p4Click(Sender: TObject);
begin
  player := '3';
  cb_cc_players_platform_type.OnChange(cb_cc_players_platform_type);
end;

procedure Tfrm_config_controls.tmr_ccTimer(Sender: TObject);
begin
  if count_time > 0 then
  begin
    Inc(count_time_dot);
    if count_time_dot < 4 then
      temp_text.Text := temp_text.Text + '.'
    else
    begin
      temp_text.Text := '';
      count_time_dot := -1;
      Dec(count_time);
    end;
  end
  else
  begin
    tmr_cc.Enabled := False;
    temp_text.Text := key_before;
    if edit_mode then
      temp_rect.Fill.color := TAlphaColorRec.Lightgray
    else
      temp_rect.Fill.color := TAlphaColorRec.White;
    Self.BringToFront;
  end;
end;

end.
