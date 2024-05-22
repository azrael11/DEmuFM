unit f_controls_joystick;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.StrUtils,
  FMX.Types,
  FMX.Objects,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.StdCtrls,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Graphics,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Platform.Win,
  WinApi.Windows,
  SDL2,
  FMX.DialogService;

type
  TJOYSTICK_INFORMATION = record
    name: string;
    profile: string;
    player: string;
    jtype: string;
    buttons: string;
    guid: TSDL_JoystickGUID;
    guid_string: string;
    vendor: word;
    product: word;
    product_version: word;
    serial: string;
    power: string;
  end;

type
  TJOYSTICK_KEYS_MOVEMENT = record
    up: integer;
    down: integer;
    left: integer;
    right: integer;
    deadzone_x: integer;
    deadzone_y: integer;
  end;

type
  TJOYSTICK_KEYS_BUTTONS = record
    b0: integer;
    b1: integer;
    b2: integer;
    b3: integer;
    b4: integer;
    b5: integer;
    b6: integer;
    b7: integer;
    b8: integer;
    b9: integer;
    b10: integer;
    b11: integer;
    b12: integer;
    b13: integer;
    b14: integer;
    b15: integer;
  end;

type
  TJOYSTICK_KEYS = record
    info: TJOYSTICK_INFORMATION;
    move: TJOYSTICK_KEYS_MOVEMENT;
    button: TJOYSTICK_KEYS_BUTTONS;
    num: string;
    set_in: boolean;
  end;

type
  Tcontrol_joystick = class(TFrame)
    spb_edit: TSpeedButton;
    img_edit: TImage;
    eff_frgb_edit: TFillRGBEffect;
    circle_joy: TCircle;
    circle_joy_ball: TCircle;
    img_button4: TImage;
    img_button9: TImage;
    img_button8: TImage;
    img_button0: TImage;
    img_button2: TImage;
    img_button3: TImage;
    img_button6: TImage;
    img_button7: TImage;
    img_button1: TImage;
    img_button5: TImage;
    txt_but8: TText;
    rect_but8: TRectangle;
    rect_but9: TRectangle;
    txt_but9: TText;
    rect_but7: TRectangle;
    txt_but7: TText;
    rect_but6: TRectangle;
    txt_but6: TText;
    rect_but5: TRectangle;
    txt_but5: TText;
    rect_but3: TRectangle;
    txt_but3: TText;
    rect_but2: TRectangle;
    txt_but2: TText;
    rect_but1: TRectangle;
    txt_but1: TText;
    rect_but4: TRectangle;
    txt_but4: TText;
    rect_but0: TRectangle;
    txt_but0: TText;
    rect_y_plus: TRectangle;
    txt_y_plus: TText;
    rect_x_plus: TRectangle;
    txt_x_plus: TText;
    rect_x_minus: TRectangle;
    txt_x_minus: TText;
    rect_y_minus: TRectangle;
    txt_y_minus: TText;
    rect_deadzone_x_value: TRectangle;
    txt_deadzone_x_value: TText;
    rect_deadzone_y_value: TRectangle;
    txt_deadzone_y_value: TText;
    rect_deadzone_y: TRectangle;
    txt_deadzone_y: TText;
    rect_deadzone_x: TRectangle;
    txt_deadzone_x: TText;
    tmr_joy_edit: TTimer;
    spb_test: TSpeedButton;
    img_test: TImage;
    eff_frgb_test: TFillRGBEffect;
    txt_mode: TText;
    spb_edit_new: TSpeedButton;
    img_edit_new: TImage;
    spb_restore: TSpeedButton;
    img_restore: TImage;
    edt_deadzone_x_value: TEdit;
    edt_deadzone_y_value: TEdit;
    circle_deadzone: TCircle;
    txt_guide: TText;
    tmr_joy_wizard: TTimer;
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure spb_editClick(Sender: TObject);
    procedure spb_testClick(Sender: TObject);
    procedure rect_edit(Sender: TObject);
    procedure spb_edit_newClick(Sender: TObject);
    procedure edt_enter(Sender: TObject; var Key: word; var KeyChar: Char; Shift: TShiftState);
    procedure spb_restoreClick(Sender: TObject);
    procedure tmr_joy_editTimer(Sender: TObject);
    procedure tmr_joy_wizardTimer(Sender: TObject);
  private
    { Private declarations }
    edit_joy: boolean;
    joy_text: TText;
    joy_rect: TRectangle;
    before_joykey: string;
    edit_key_set: boolean;

    wiz_joy: boolean;
    wiz_phase: integer;

    passes: integer;
    dots: integer;

    test_joy: boolean;
    joy_temp_virt: TJOYSTICK_KEYS;

    procedure set_standard_records_in_keys;

    function joystick_exists_in_database: boolean;
    function get_joystick_settings_from_database: TJOYSTICK_KEYS;
    function get_joystick_default_settings: TJOYSTICK_KEYS;
    function get_joystick_data: TJOYSTICK_INFORMATION;

    procedure temp_joystick;

    procedure input_keys_value_to_fields;

    procedure test_joystick;

    procedure edit_joystick;
    procedure set_new_key_to_temp(selKey: String; newValue: integer);
    procedure set_new_key_to_temp_virt(selKey: String; newValue: integer);

    procedure wizard_phase(num: byte);
    procedure set_mode(rect_color, txt_color: TColor; stroke: integer; rect_cursor: TCursor);

  public
    { Public declarations }
    joy_active: boolean;
    joy_edit_active: boolean;
    joy_data: TJOYSTICK_INFORMATION;
    procedure show;
  end;

implementation

{$R *.fmx}

uses
  main_engine,
  controls_engine,
  config_controls,
  umain_config,
  multi_platform,
  uDataModule;

procedure Tcontrol_joystick.edit_joystick;
var
  sdl_event: TSDL_Event;
  sdl_axis: integer;
  pl, js: integer;
  pressed, idle: string;
  vComp: TComponent;
  jNum: string;
  found_key: integer;
  l_Object: TObject;

  function line_object(num: byte): TObject;
  begin
    case num of
      1:
        Result := rect_y_minus;
      2:
        Result := rect_x_plus;
      3:
        Result := rect_x_minus;
      4:
        Result := rect_but0;
      5:
        Result := rect_but1;
      6:
        Result := rect_but2;
      7:
        Result := rect_but3;
      8:
        Result := rect_but4;
      9:
        Result := rect_but5;
      10:
        Result := rect_but6;
      11:
        Result := rect_but7;
      12:
        Result := rect_but8;
      13:
        Result := rect_but9;
    end;
  end;

  function check_key_is_used(Key: integer; out find: integer): boolean;
  begin
    Result := false;
    if Key = joy_temp.button.b0 then
    begin
      Result := true;
      find := 0;
    end
    else if Key = joy_temp.button.b1 then
    begin
      Result := true;
      find := 1;
    end
    else if Key = joy_temp.button.b2 then
    begin
      Result := true;
      find := 2;
    end
    else if Key = joy_temp.button.b3 then
    begin
      Result := true;
      find := 3;
    end
    else if Key = joy_temp.button.b4 then
    begin
      Result := true;
      find := 4;
    end
    else if Key = joy_temp.button.b5 then
    begin
      Result := true;
      find := 5;
    end
    else if Key = joy_temp.button.b6 then
    begin
      Result := true;
      find := 6;
    end
    else if Key = joy_temp.button.b7 then
    begin
      Result := true;
      find := 7;
    end
    else if Key = joy_temp.button.b8 then
    begin
      Result := true;
      find := 8;
    end
    else if Key = joy_temp.button.b9 then
    begin
      Result := true;
      find := 9;
    end
    else if Key = joy_temp.button.b10 then
    begin
      Result := true;
      find := 10;
    end
    else if Key = joy_temp.button.b11 then
    begin
      Result := true;
      find := 11;
    end
    else if Key = joy_temp.button.b12 then
    begin
      Result := true;
      find := 12;
    end
    else if Key = joy_temp.button.b13 then
    begin
      Result := true;
      find := 13;
    end
    else if Key = joy_temp.button.b14 then
    begin
      Result := true;
      find := 14;
    end
    else if Key = joy_temp.button.b15 then
    begin
      Result := true;
      find := 15;
    end
    else
      find := -1000
  end;

  procedure wiz_next;
  begin
    if wiz_phase > 12 then
    begin
      wiz_joy := false;
      tmr_joy_edit.Enabled := false;
      joy_temp := joy_temp_virt;
      spb_editClick(nil);
    end
    else
    begin
      inc(wiz_phase);
      wiz_joy := true;
      edit_joy := true;
      l_Object := line_object(wiz_phase);
      rect_edit(l_Object);
    end;
  end;

begin
  joy_edit_active := true;
  pl := frm_config_controls.tc_cc_players.TabIndex;
  js := frm_config_controls.cb_cc_players_controller.ItemIndex - 1;
  joystick_def[pl] := SDL_JoystickOpen(js);
  sdl_axis := SDL_JoystickNumAxes(joystick_def[pl]);
  while joy_edit_active do
    while (SDL_PollEvent(@sdl_event)) <> 0 do
    begin
      if sdl_event.type_ = SDL_KEYDOWN then
      begin
        if SDLK_ESCAPE = sdl_event.Key.keysym.sym then
        begin
          // do some stop editing actions if editing is enable
        end;
      end
      else if sdl_event.type_ = SDL_JOYAXISMOTION then
      begin
        if sdl_event.jaxis.axis in [0, 2, 4, 6] then
        begin
          if sdl_event.jaxis.value > joy_temp.move.deadzone_x then
          begin
            if joy_temp.move.right > 0 then
            begin
              if not wiz_joy then
                set_new_key_to_temp(joy_rect.TagString, 1);
              tmr_joy_edit.Enabled := false;
              joy_text.Text := 'X+';
              joy_edit_active := false;
              if wiz_joy then
              begin
                set_new_key_to_temp_virt(joy_rect.TagString, 1);
                wiz_next;
              end;
            end
          end
          else if sdl_event.jaxis.value < -joy_temp.move.deadzone_x then
          begin
            if joy_temp.move.left < 0 then
            begin
              if not wiz_joy then
                set_new_key_to_temp(joy_rect.TagString, -1);
              tmr_joy_edit.Enabled := false;
              joy_text.Text := 'X-';
              joy_edit_active := false;
              if wiz_joy then
              begin
                set_new_key_to_temp_virt(joy_rect.TagString, 1);
                wiz_next;
              end;
            end
          end
        end
        else if sdl_event.jaxis.axis in [1, 3, 5, 7] then
        begin
          if sdl_event.jaxis.value > joy_temp.move.deadzone_y then
          begin
            if joy_temp.move.up > 0 then
            begin
              if not wiz_joy then
                set_new_key_to_temp(joy_rect.TagString, 1);
              tmr_joy_edit.Enabled := false;
              joy_text.Text := 'Y+';
              joy_edit_active := false;
              if wiz_joy then
              begin
                set_new_key_to_temp_virt(joy_rect.TagString, 1);
                wiz_next;
              end;
            end
          end
          else if sdl_event.jaxis.value < -joy_temp.move.deadzone_y then
          begin
            if joy_temp.move.down < 0 then
            begin
              if not wiz_joy then
                set_new_key_to_temp(joy_rect.TagString, -1);
              tmr_joy_edit.Enabled := false;
              joy_text.Text := 'Y-';
              joy_edit_active := false;
              if wiz_joy then
              begin
                set_new_key_to_temp_virt(joy_rect.TagString, 1);
                wiz_next;
              end;
            end
          end
        end;
      end
      else if sdl_event.type_ = SDL_JOYBUTTONDOWN then
      begin
        if not wiz_joy then
        begin
          if check_key_is_used(sdl_event.jbutton.button, found_key) then
            if found_key <> -1000 then
            begin
              vComp := Self.FindComponent('rect_but' + found_key.ToString);
              (vComp as TRectangle).Fill.Color := TAlphaColorRec.Red;
              vComp := Self.FindComponent('txt_but' + found_key.ToString);
              (vComp as TText).Text := '';
              set_new_key_to_temp(found_key.ToString, -1);
            end;
        end;
        if not wiz_joy then
          set_new_key_to_temp(joy_rect.TagString, sdl_event.jbutton.button);
        tmr_joy_edit.Enabled := false;
        joy_text.Text := 'Button ' + sdl_event.jbutton.button.ToString;
        joy_edit_active := false;
        edit_key_set := true;
        if wiz_joy then
        begin
          set_new_key_to_temp_virt(joy_rect.TagString, 1);
          wiz_next;
        end;
      end;
      Sleep(20);
    end;
end;

procedure Tcontrol_joystick.edt_enter(Sender: TObject; var Key: word; var KeyChar: Char; Shift: TShiftState);
var
  tNum: String;
begin
  tNum := (Sender as TEdit).Text;
  if Key = vkReturn then
  begin
    if multi_platform.isNumeric(tNum) then
    begin
      if (tNum.ToInteger > 10) and (tNum.ToInteger < 32768) then
      begin
        joy_edit_active := false;
        tmr_joy_edit.Enabled := false;
        txt_deadzone_x_value.Text := tNum;
        txt_deadzone_y_value.Text := tNum;
        circle_deadzone.Width := Round((140 * tNum.ToInteger) / 32767);
        circle_deadzone.Height := Round((140 * tNum.ToInteger) / 32767);
        joy_temp.move.deadzone_x := tNum.ToInteger;
        joy_temp.move.deadzone_y := tNum.ToInteger;
        edt_deadzone_x_value.Visible := false;
        edt_deadzone_y_value.Visible := false;
        txt_guide.Text := '';
        Application.ProcessMessages;
      end
      else
      begin
        ShowMessage('Must be a number from 10 to 32767');
        (Sender as TEdit).Text := '';
      end;
    end
    else
    begin
      ShowMessage('Must be a number. Try not to add characters.');
      (Sender as TEdit).Text := '';
    end;
  end;
end;

procedure Tcontrol_joystick.FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if test_joy then
  begin
    if X < 4 then
      X := 4;
    if Y < 4 then
      Y := 4;
  end;
end;

function Tcontrol_joystick.get_joystick_data: TJOYSTICK_INFORMATION;
var
  player, joy_sel, dev_index, power_level, mappings: integer;
  guid_str: PAnsiChar;

  function get_powerlevel(index: integer): string;
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

  procedure draw_powerlevel(num: integer);
  var
    temp: integer;
  begin
    frm_config_controls.rect_cc_players_joystick_battery_5.Visible := true;
    frm_config_controls.rect_cc_players_joystick_battery_25.Visible := true;
    frm_config_controls.rect_cc_players_joystick_battery_50.Visible := true;
    frm_config_controls.rect_cc_players_joystick_battery_75.Visible := true;
    frm_config_controls.rect_cc_players_joystick_battery_90.Visible := true;
    frm_config_controls.rect_cc_players_joystick_battery_100.Visible := true;

    if ((num = 3) or (num = 100)) then
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := '100%'
    else if ((num < 100) and (num >= 90)) then
    begin
      frm_config_controls.rect_cc_players_joystick_battery_100.Visible := false;
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end
    else if ((num >= 70) and (num < 90)) or (num = 2) then
    begin
      frm_config_controls.rect_cc_players_joystick_battery_90.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_100.Visible := false;
      if num = 2 then
        temp := 70;
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := temp.ToString + '%';
    end
    else if ((num >= 50) and (num < 70)) then
    begin
      frm_config_controls.rect_cc_players_joystick_battery_75.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_90.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_100.Visible := false;
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end
    else if ((num >= 20) and (num < 50)) or (num = 1) then
    begin
      frm_config_controls.rect_cc_players_joystick_battery_50.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_75.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_90.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_100.Visible := false;
      if num = 1 then
        temp := 20;
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := temp.ToString + '%';
    end
    else if ((num >= 5) and (num < 20)) then
    begin
      frm_config_controls.rect_cc_players_joystick_battery_25.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_50.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_75.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_90.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_100.Visible := false;
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end
    else if ((num >= 0) and (num < 5)) or (num = 0) then
    begin
      frm_config_controls.rect_cc_players_joystick_battery_5.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_25.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_50.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_75.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_90.Visible := false;
      frm_config_controls.rect_cc_players_joystick_battery_100.Visible := false;
      frm_config_controls.txt_cc_players_joystick_battery_percent.Text := num.ToString + '%';
    end;
  end;

  function get_joystick_type(index: integer): string;
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
  joy_sel := frm_config_controls.cb_cc_players_controller.ItemIndex - 1;
  player := frm_config_controls.player.ToInteger;

  joystick_def[player] := SDL_JoystickOpen(joy_sel);
  dev_index := SDL_JoystickGetPlayerIndex(joystick_def[player]);
  Result.profile := frm_config_controls.profile;
  Result.player := frm_config_controls.player;
  Result.name := SDL_JoystickName(joystick_def[player]);
  Result.jtype := get_joystick_type(SDL_JoystickGetType(joystick_def[player]));
  Result.serial := SDL_JoystickGetSerial(joystick_def[player]);
  Result.buttons := SDL_JoystickNumButtons(joystick_def[player]).ToString;
  Result.guid := SDL_JoystickGetGUID(joystick_def[player]);
  // SDL_joystickGetGuidString(Result.guid, guid_str, 33);
  // Result.guid_string := guid_str;
  Result.vendor := SDL_JoystickGetVendor(joystick_def[player]);
  Result.product := SDL_JoystickGetProduct(joystick_def[player]);
  Result.product_version := SDL_JoystickGetProductVersion(joystick_def[player]);
  Result.power := get_powerlevel(SDL_JoystickCurrentPowerLevel(joystick_def[player]));
  power_level := SDL_JoystickCurrentPowerLevel(joystick_def[player]);

  frm_config_controls.lbl_cc_players_joystick_name_value.Text := joy_data.name;
  frm_config_controls.lbl_cc_players_joystick_buttons_value.Text := joy_data.buttons;
  frm_config_controls.lbl_cc_players_joystick_type_value.Text := joy_data.jtype;
  frm_config_controls.lbl_cc_players_joystick_vendor_value.Text := joy_data.vendor.ToString;
  frm_config_controls.lbl_cc_players_joystick_serial_value.Text := joy_data.serial;
  frm_config_controls.lbl_cc_players_joystick_product_value.Text := joy_data.product.ToString;
  frm_config_controls.lbl_cc_players_joystick_product_version_value.Text := joy_data.product_version.ToString;
  if (power_level = -1) or (power_level = 4) then
  begin
    frm_config_controls.rect_cc_players_joystick_battery.Visible := false;
    frm_config_controls.img_cc_players_joystick_usb.Visible := true;
  end
  else
  begin
    frm_config_controls.img_cc_players_joystick_usb.Visible := false;
    frm_config_controls.rect_cc_players_joystick_battery.Visible := true;
    draw_powerlevel(power_level);
  end;
  SDL_JoystickClose(joystick_def[player]);
end;

function Tcontrol_joystick.get_joystick_default_settings: TJOYSTICK_KEYS;
begin
  Result.info.name := joy_data.name;
  Result.info.profile := joy_data.profile;
  Result.info.player := joy_data.player;
  Result.info.jtype := joy_data.jtype;
  Result.info.buttons := joy_data.buttons;
  Result.info.guid := joy_data.guid;
  Result.info.vendor := joy_data.vendor;
  Result.info.product := joy_data.product;
  Result.info.product_version := joy_data.product_version;
  Result.info.serial := joy_data.serial;
  Result.info.power := joy_data.power;
  Result.move.up := 1;
  Result.move.down := -1;
  Result.move.left := -1;
  Result.move.right := 1;
  Result.move.deadzone_x := 8000;
  Result.move.deadzone_y := 8000;
  circle_deadzone.Width := Round((140 * Result.move.deadzone_x) / 32767);
  circle_deadzone.Height := Round((140 * Result.move.deadzone_y) / 32767);
  Result.button.b0 := 0;
  Result.button.b1 := 1;
  Result.button.b2 := 2;
  Result.button.b3 := 3;
  Result.button.b4 := 4;
  Result.button.b5 := 5;
  Result.button.b6 := 6;
  Result.button.b7 := 7;
  Result.button.b8 := 8;
  Result.button.b9 := 9;
  Result.button.b10 := 10;
  Result.button.b11 := 11;
  Result.button.b12 := 12;
  Result.button.b13 := 13;
  Result.button.b14 := 14;
  Result.button.b15 := 15;
  Result.set_in := true;
end;

function Tcontrol_joystick.get_joystick_settings_from_database: TJOYSTICK_KEYS;
begin
  dm.tJoystick.Locate('num', joy_temp.num);

  Result.info.name := dm.tJoystickjname.AsString;
  Result.info.profile := dm.tJoystickname.AsString;
  Result.info.player := dm.tJoystickplayer.AsString;
  Result.info.jtype := dm.tJoystickjtype.AsString;
  Result.info.buttons := dm.tJoystickbuttons.AsString;
  Result.info.guid_string := dm.tJoystickguid.AsString;
  Result.info.vendor := dm.tJoystickvendor.AsLongWord;
  Result.info.product := dm.tJoystickproduct.AsLongWord;
  Result.info.product_version := dm.tJoystickproduct_version.AsLongWord;
  Result.info.serial := dm.tJoystickserial.AsString;
  // Result.info.power := ;
  Result.move.up := dm.tJoystickaxis_y_plus.AsInteger;
  Result.move.down := dm.tJoystickaxis_y_minus.AsInteger;
  Result.move.left := dm.tJoystickaxis_x_minus.AsInteger;
  Result.move.right := dm.tJoystickaxis_x_plus.AsInteger;
  Result.move.deadzone_x := dm.tJoystickdeadzone_x.AsInteger;
  Result.move.deadzone_y := dm.tJoystickdeadzone_y.AsInteger;
  circle_deadzone.Width := Round((140 * Result.move.deadzone_x) / 32767);
  circle_deadzone.Height := Round((140 * Result.move.deadzone_y) / 32767);
  Result.button.b0 := dm.tJoystickbutton0.AsInteger;
  Result.button.b1 := dm.tJoystickbutton1.AsInteger;
  Result.button.b2 := dm.tJoystickbutton2.AsInteger;
  Result.button.b3 := dm.tJoystickbutton3.AsInteger;
  Result.button.b4 := dm.tJoystickbutton4.AsInteger;
  Result.button.b5 := dm.tJoystickbutton5.AsInteger;
  Result.button.b6 := dm.tJoystickbutton6.AsInteger;
  Result.button.b7 := dm.tJoystickbutton7.AsInteger;
  Result.button.b8 := dm.tJoystickbutton8.AsInteger;
  Result.button.b9 := dm.tJoystickbutton9.AsInteger;
  Result.button.b10 := dm.tJoystickbutton10.AsInteger;
  Result.button.b11 := dm.tJoystickbutton11.AsInteger;
  Result.button.b12 := dm.tJoystickbutton12.AsInteger;
  Result.button.b13 := dm.tJoystickbutton13.AsInteger;
  Result.button.b14 := dm.tJoystickbutton14.AsInteger;
  Result.button.b15 := dm.tJoystickbutton15.AsInteger;
  Result.set_in := true;
end;

procedure Tcontrol_joystick.input_keys_value_to_fields;
begin
  txt_but0.Text := 'Button ' + joy_temp.button.b0.ToString;
  txt_but1.Text := 'Button ' + joy_temp.button.b1.ToString;
  txt_but2.Text := 'Button ' + joy_temp.button.b2.ToString;
  txt_but3.Text := 'Button ' + joy_temp.button.b3.ToString;
  txt_but4.Text := 'Button ' + joy_temp.button.b4.ToString;
  txt_but5.Text := 'Button ' + joy_temp.button.b5.ToString;
  txt_but6.Text := 'Button ' + joy_temp.button.b6.ToString;
  txt_but7.Text := 'Button ' + joy_temp.button.b7.ToString;
  txt_but8.Text := 'Button ' + joy_temp.button.b8.ToString;
  txt_but9.Text := 'Button ' + joy_temp.button.b9.ToString;
  if joy_temp.move.left = 1 then
    txt_x_plus.Text := 'X+'
  else if joy_temp.move.left = -1 then
    txt_x_plus.Text := 'X-';
  if joy_temp.move.right = 1 then
    txt_x_minus.Text := 'X+'
  else if joy_temp.move.right = -1 then
    txt_x_minus.Text := 'X-';
  if joy_temp.move.up = 1 then
    txt_y_plus.Text := 'Y+'
  else if joy_temp.move.up = -1 then
    txt_y_plus.Text := 'Y-';
  if joy_temp.move.down = 1 then
    txt_y_minus.Text := 'Y+'
  else if joy_temp.move.down = -1 then
    txt_y_minus.Text := 'Y-';
  txt_deadzone_x_value.Text := joy_temp.move.deadzone_x.ToString;
  txt_deadzone_y_value.Text := joy_temp.move.deadzone_y.ToString;
end;

function Tcontrol_joystick.joystick_exists_in_database: boolean;
begin
   Result := false;
  dm.tJoystick.Filtered := false;
  dm.tJoystick.Filter := 'jname=' + joy_data.name+ ' AND player='+ frm_config_controls.player + ' AND name='+ frm_config_controls.profile;
  dm.tJoystick.Filtered := true;

  if dm.tJoystick.RecordCount > 0 then
  begin
    Result := true;
    joy_temp.num := dm.tJoysticknum.AsString;
  end
  else
    joy_temp.num := '';
end;

procedure Tcontrol_joystick.rect_edit(Sender: TObject);
var
  vComp: TComponent;
  name: string;

  procedure clear_warning;
  var
    vi: integer;
  begin
    for vi := 0 to 15 do
    begin
      vComp := Self.FindComponent('rect_but' + vi.ToString);
      if vComp <> nil then
        (vComp as TRectangle).Fill.Color := $FF44BEB0;
    end;
    rect_y_plus.Fill.Color := $FF44BEB0;
    rect_x_plus.Fill.Color := $FF44BEB0;
    rect_y_minus.Fill.Color := $FF44BEB0;
    rect_x_minus.Fill.Color := $FF44BEB0;
    rect_deadzone_x_value.Fill.Color := $FF44BEB0;
    rect_deadzone_y_value.Fill.Color := $FF44BEB0;
  end;

begin
  if edit_joy then
  begin
    clear_warning;
    if (edit_key_set = false) and (before_joykey <> '-1') then
      if not(joy_rect.name = 'rect_deadzone_x_value') and not(joy_rect.name = 'rect_deadzone_y_value') then
      begin
        joy_text.Text := before_joykey;
        before_joykey := '';
      end;
    edit_key_set := false;
    joy_rect := (Sender as TRectangle);
    name := (Sender as TRectangle).name;
    delete(name, 1, 4);
    name := 'txt' + name;
    vComp := Self.FindComponent(name);
    joy_text := (vComp as TText);
    if ContainsText(name, 'but') then
      txt_guide.Text := 'Press to set new button'
    else if ContainsText(name, 'plus') then
      txt_guide.Text := 'Move joystick to set x value'
    else if ContainsText(name, 'minus') then
      txt_guide.Text := 'Move joystick to set y value';
    before_joykey := joy_text.Text;
    joy_text.Text := '';
    if (joy_rect.name = 'rect_deadzone_x_value') or (joy_rect.name = 'rect_deadzone_y_value') then
    begin
      if (joy_rect.name = 'rect_deadzone_x_value') then
      begin
        edt_deadzone_x_value.Visible := true;
        edt_deadzone_x_value.Text := '';
        edt_deadzone_x_value.SetFocus;
        txt_guide.Text := 'Set Deadezone X';
      end
      else if (joy_rect.name = 'rect_deadzone_y_value') then
      begin
        edt_deadzone_y_value.Visible := true;
        edt_deadzone_y_value.Text := '';
        edt_deadzone_y_value.SetFocus;
        txt_guide.Text := 'Set Deadezone Y';
      end;
    end;
    // else
    // WinApi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
    passes := 0;
    dots := 0;
    tmr_joy_edit.Enabled := true;

    if wiz_joy then
      wizard_phase(wiz_phase);

    edit_joystick;
  end;
end;

procedure Tcontrol_joystick.set_new_key_to_temp(selKey: String; newValue: integer);
begin
  if selKey = '0' then
    joy_temp.button.b0 := newValue
  else if selKey = '1' then
    joy_temp.button.b1 := newValue
  else if selKey = '2' then
    joy_temp.button.b2 := newValue
  else if selKey = '3' then
    joy_temp.button.b3 := newValue
  else if selKey = '4' then
    joy_temp.button.b4 := newValue
  else if selKey = '5' then
    joy_temp.button.b5 := newValue
  else if selKey = '6' then
    joy_temp.button.b6 := newValue
  else if selKey = '7' then
    joy_temp.button.b7 := newValue
  else if selKey = '8' then
    joy_temp.button.b8 := newValue
  else if selKey = 'x+' then
    joy_temp.move.right := newValue
  else if selKey = 'x-' then
    joy_temp.move.left := newValue
  else if selKey = 'y+' then
    joy_temp.move.up := newValue
  else if selKey = 'y-' then
    joy_temp.move.down := newValue
  else if selKey = 'deadzone_x' then
    joy_temp.move.deadzone_x := newValue
  else if selKey = 'deadzone_y' then
    joy_temp.move.deadzone_y := newValue;
end;

procedure Tcontrol_joystick.set_new_key_to_temp_virt(selKey: String; newValue: integer);
begin
  if selKey = '0' then
    joy_temp_virt.button.b0 := newValue
  else if selKey = '1' then
    joy_temp_virt.button.b1 := newValue
  else if selKey = '2' then
    joy_temp_virt.button.b2 := newValue
  else if selKey = '3' then
    joy_temp_virt.button.b3 := newValue
  else if selKey = '4' then
    joy_temp_virt.button.b4 := newValue
  else if selKey = '5' then
    joy_temp_virt.button.b5 := newValue
  else if selKey = '6' then
    joy_temp_virt.button.b6 := newValue
  else if selKey = '7' then
    joy_temp_virt.button.b7 := newValue
  else if selKey = '8' then
    joy_temp_virt.button.b8 := newValue
  else if selKey = 'x+' then
    joy_temp_virt.move.right := newValue
  else if selKey = 'x-' then
    joy_temp_virt.move.left := newValue
  else if selKey = 'y+' then
    joy_temp_virt.move.up := newValue
  else if selKey = 'y-' then
    joy_temp_virt.move.down := newValue
  else if selKey = 'deadzone_x' then
    joy_temp_virt.move.deadzone_x := newValue
  else if selKey = 'deadzone_y' then
    joy_temp_virt.move.deadzone_y := newValue;
end;

procedure Tcontrol_joystick.set_standard_records_in_keys;
begin
  rect_but0.TagString := '0';
  rect_but1.TagString := '1';
  rect_but2.TagString := '2';
  rect_but3.TagString := '3';
  rect_but4.TagString := '4';
  rect_but5.TagString := '5';
  rect_but6.TagString := '6';
  rect_but7.TagString := '7';
  rect_but8.TagString := '8';
  rect_x_plus.TagString := 'x+';
  rect_x_minus.TagString := 'x-';
  rect_y_plus.TagString := 'y+';
  rect_y_minus.TagString := 'y-';
  rect_deadzone_x_value.TagString := 'deadzone_x';
  rect_deadzone_x_value.TagString := 'deadzone_y';
end;

procedure Tcontrol_joystick.show;
begin
  // WinApi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
  window_render.w := 1;
  window_render.h := 1;
  set_standard_records_in_keys;
  joy_data := get_joystick_data;
  temp_joystick;
  input_keys_value_to_fields;
  txt_deadzone_x_value.Text := joy_temp.move.deadzone_x.ToString;
  txt_deadzone_y_value.Text := joy_temp.move.deadzone_y.ToString;
  edit_joy := false;
  before_joykey := '-1';
  test_joy := false;
  wiz_phase := -1;
  // lbl_cc_players_joystick_info.Text := lang.load_lang_string('Joystick Information');
  // lbl_cc_players_joystick_name.Text := lang.load_lang_string('Name') + ' : ';
  // lbl_cc_players_joystick_buttons.Text := lang.load_lang_string('Buttons') + ' : ';
  // lbl_cc_players_joystick_type.Text := lang.load_lang_string('Type') + ' : ';
  // lbl_cc_players_joystick_vendor.Text := lang.load_lang_string('Vendor') + ' : ';
  // lbl_cc_players_joystick_serial.Text := lang.load_lang_string('Serial Number') + ' : ';
  // lbl_cc_players_joystick_product.Text := lang.load_lang_string('Product') + ' : ';
  // lbl_cc_players_joystick_product_version.Text := lang.load_lang_string('Product Number') + ' : ';
end;

procedure Tcontrol_joystick.set_mode(rect_color, txt_color: TColor; stroke: integer; rect_cursor: TCursor);
begin
  rect_y_plus.Fill.Color := rect_color;
  rect_y_plus.stroke.Thickness := stroke;
  rect_y_plus.Cursor := rect_cursor;
  rect_y_minus.Fill.Color := rect_color;
  rect_y_minus.stroke.Thickness := stroke;
  rect_y_minus.Cursor := rect_cursor;
  rect_x_plus.Fill.Color := rect_color;
  rect_x_plus.stroke.Thickness := stroke;
  rect_x_plus.Cursor := rect_cursor;
  rect_x_minus.Fill.Color := rect_color;
  rect_x_minus.stroke.Thickness := stroke;
  rect_x_minus.Cursor := rect_cursor;
  rect_but0.Fill.Color := rect_color;
  rect_but0.stroke.Thickness := stroke;
  rect_but0.Cursor := rect_cursor;
  rect_but1.Fill.Color := rect_color;
  rect_but1.stroke.Thickness := stroke;
  rect_but1.Cursor := rect_cursor;
  rect_but2.Fill.Color := rect_color;
  rect_but2.stroke.Thickness := stroke;
  rect_but2.Cursor := rect_cursor;
  rect_but3.Fill.Color := rect_color;
  rect_but3.stroke.Thickness := stroke;
  rect_but3.Cursor := rect_cursor;
  rect_but4.Fill.Color := rect_color;
  rect_but4.stroke.Thickness := stroke;
  rect_but4.Cursor := rect_cursor;
  rect_but5.Fill.Color := rect_color;
  rect_but5.stroke.Thickness := stroke;
  rect_but5.Cursor := rect_cursor;
  rect_but6.Fill.Color := rect_color;
  rect_but6.stroke.Thickness := stroke;
  rect_but6.Cursor := rect_cursor;
  rect_but7.Fill.Color := rect_color;
  rect_but7.stroke.Thickness := stroke;
  rect_but7.Cursor := rect_cursor;
  rect_but8.Fill.Color := rect_color;
  rect_but8.stroke.Thickness := stroke;
  rect_but8.Cursor := rect_cursor;
  rect_but9.Fill.Color := rect_color;
  rect_but9.stroke.Thickness := stroke;
  rect_but9.Cursor := rect_cursor;
  rect_deadzone_x_value.Fill.Color := rect_color;
  rect_deadzone_x_value.Cursor := rect_cursor;
  rect_deadzone_x_value.stroke.Thickness := stroke;
  rect_deadzone_y_value.Fill.Color := rect_color;
  rect_deadzone_y_value.Cursor := rect_cursor;
  rect_deadzone_y_value.stroke.Thickness := stroke;
  txt_y_plus.TextSettings.FontColor := txt_color;
  txt_y_minus.TextSettings.FontColor := txt_color;
  txt_x_plus.TextSettings.FontColor := txt_color;
  txt_x_minus.TextSettings.FontColor := txt_color;
  txt_but0.TextSettings.FontColor := txt_color;
  txt_but1.TextSettings.FontColor := txt_color;
  txt_but2.TextSettings.FontColor := txt_color;
  txt_but3.TextSettings.FontColor := txt_color;
  txt_but4.TextSettings.FontColor := txt_color;
  txt_but5.TextSettings.FontColor := txt_color;
  txt_but6.TextSettings.FontColor := txt_color;
  txt_but7.TextSettings.FontColor := txt_color;
  txt_but8.TextSettings.FontColor := txt_color;
  txt_but9.TextSettings.FontColor := txt_color;
  txt_deadzone_x_value.TextSettings.FontColor := txt_color;
  txt_deadzone_y_value.TextSettings.FontColor := txt_color;
end;

procedure Tcontrol_joystick.spb_editClick(Sender: TObject);
begin
  edit_joy := not edit_joy;
  txt_mode.Text := 'Mode : Edit';
  if joy_active then
    joy_active := false;
  eff_frgb_edit.Enabled := edit_joy;
  txt_guide.Visible := edit_joy;
  txt_mode.Visible := edit_joy;
  spb_edit_new.Visible := edit_joy;
  spb_restore.Visible := edit_joy;
  spb_test.Enabled := not edit_joy;
  if edit_joy then
  begin
    set_mode($FF44BEB0, TAlphaColorRec.White, 1, crHandPoint);
  end
  else
    set_mode(TAlphaColorRec.White, TAlphaColorRec.Black, 0, crDefault);
end;

procedure Tcontrol_joystick.spb_edit_newClick(Sender: TObject);
begin
  if wiz_joy then
  begin
    wiz_joy := not wiz_joy;
    edit_joy := false;
    tmr_joy_edit.Enabled := false;
    joy_edit_active := false;
  end
  else
  begin
    wiz_joy := not wiz_joy;
    inc(wiz_phase);
    edit_joy := true;
    rect_edit(rect_y_plus);
  end;
end;

procedure Tcontrol_joystick.spb_restoreClick(Sender: TObject);
begin
  TDialogService.MessageDialog('This will restore to default settings. Press OK to procced.',
    TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbCancel, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrOk then
      begin
        joy_temp := get_joystick_default_settings;
        input_keys_value_to_fields;
      end;
    end);
end;

procedure Tcontrol_joystick.spb_testClick(Sender: TObject);
begin
  test_joy := not test_joy;
  txt_mode.Text := 'Mode : Testing';
  eff_frgb_test.Enabled := test_joy;
  txt_mode.Visible := test_joy;
  spb_edit.Enabled := not test_joy;
  if test_joy then
    test_joystick
  else
    joy_active := false;
end;

procedure Tcontrol_joystick.temp_joystick;
begin
  if joy_temp.set_in = false then
  begin
    if joystick_exists_in_database then
      joy_temp := get_joystick_settings_from_database
    else
      joy_temp := get_joystick_default_settings;
  end;
end;

procedure Tcontrol_joystick.test_joystick;
var
  sdl_event: TSDL_Event;
  mappings, joy_sel: integer;
  sdl_axis: integer;
  pl, js: integer;
  pressed, idle: string;
  vComp: TComponent;
  jNum: string;

  function get_map_key(num: integer): string;
  begin
    case num of
      0:
        Result := joy_temp.button.b0.ToString;
      1:
        Result := joy_temp.button.b1.ToString;
      2:
        Result := joy_temp.button.b2.ToString;
      3:
        Result := joy_temp.button.b3.ToString;
      4:
        Result := joy_temp.button.b4.ToString;
      5:
        Result := joy_temp.button.b5.ToString;
      6:
        Result := joy_temp.button.b6.ToString;
      7:
        Result := joy_temp.button.b7.ToString;
      8:
        Result := joy_temp.button.b8.ToString;
      9:
        Result := joy_temp.button.b9.ToString;
      10:
        Result := joy_temp.button.b10.ToString;
      11:
        Result := joy_temp.button.b11.ToString;
      12:
        Result := joy_temp.button.b12.ToString;
      13:
        Result := joy_temp.button.b13.ToString;
      14:
        Result := joy_temp.button.b14.ToString;
      15:
        Result := joy_temp.button.b15.ToString;
    end;
  end;

begin
  // WinApi.Windows.SetFocus(FMX.Platform.Win.WindowHandleToPlatform(frm_sdl2.Handle).Wnd);
  joy_active := true;
//  pressed := config.main.prj_images_path.Controls + 'arcade_button_pressed.png';
//  idle := config.main.prj_images_path.Controls + 'arcade_button.png';
  pl := frm_config_controls.tc_cc_players.TabIndex;
  js := frm_config_controls.cb_cc_players_controller.ItemIndex - 1;
  joystick_def[pl] := SDL_JoystickOpen(js);
  sdl_axis := SDL_JoystickNumAxes(joystick_def[pl]);
  while joy_active do
  begin
    while (SDL_PollEvent(@sdl_event)) <> 0 do
    begin
      if sdl_event.type_ = SDL_KEYDOWN then
      begin
        if SDLK_ESCAPE = sdl_event.Key.keysym.sym then
        begin
          // do some stop editing actions if editing is enable
        end;
      end
      else
      begin
        if sdl_event.type_ = SDL_JOYAXISMOTION then
        begin
          if sdl_event.jaxis.axis in [0, 2, 4, 6] then
          begin
            if sdl_event.jaxis.value > joy_temp.move.deadzone_x then
            begin
              if joy_temp.move.right > 0 then
              begin
                circle_joy_ball.Position.X := Round((sdl_event.jaxis.value * 80) / 32767);
                txt_x_plus.Text := 'X(' + sdl_event.jaxis.value.ToString + ')';
              end
              else
              begin
                circle_joy_ball.Position.X := Round(((-sdl_event.jaxis.value * 80) / 32767) + 80);
                txt_x_minus.Text := 'X(' + sdl_event.jaxis.value.ToString + ')';
              end;
            end
            else if sdl_event.jaxis.value < -joy_temp.move.deadzone_x then
            begin
              if joy_temp.move.left < 0 then
              begin
                circle_joy_ball.Position.X := Round(((sdl_event.jaxis.value * 80) / 32767) + 80);
                txt_x_minus.Text := 'X(' + sdl_event.jaxis.value.ToString + ')';
              end
              else
              begin
                circle_joy_ball.Position.X := Round((-sdl_event.jaxis.value * 80) / 32767);
                txt_x_plus.Text := 'X(' + sdl_event.jaxis.value.ToString + ')';
              end;
            end
            else
            begin
              circle_joy_ball.Position.X := 40;
              txt_y_plus.Text := 'Y+';
              txt_y_minus.Text := 'Y-';
              txt_x_plus.Text := 'X+';
              txt_x_minus.Text := 'X-';
            end;
          end
          else if sdl_event.jaxis.axis in [1, 3, 5, 7] then
          begin
            if sdl_event.jaxis.value > joy_temp.move.deadzone_y then
            begin
              if joy_temp.move.up > 0 then
              begin
                circle_joy_ball.Position.Y := Round(((-sdl_event.jaxis.value * 80) / 32767) + 80);
                txt_y_plus.Text := 'Y(' + sdl_event.jaxis.value.ToString + ')';
              end
              else
              begin
                circle_joy_ball.Position.Y := Round((sdl_event.jaxis.value * 80) / 32767);
                txt_y_minus.Text := 'Y(' + sdl_event.jaxis.value.ToString + ')';
              end
            end
            else if sdl_event.jaxis.value < -joy_temp.move.deadzone_y then
            begin
              if joy_temp.move.down < 0 then
              begin
                circle_joy_ball.Position.Y := Round((-sdl_event.jaxis.value * 80) / 32767);
                txt_y_minus.Text := 'Y(' + sdl_event.jaxis.value.ToString + ')';
              end
              else
              begin
                circle_joy_ball.Position.Y := Round(((sdl_event.jaxis.value * 80) / 32767) + 80);
                txt_y_plus.Text := 'Y(' + sdl_event.jaxis.value.ToString + ')';
              end
            end
            else
            begin
              circle_joy_ball.Position.Y := 40;
              txt_y_plus.Text := 'Y+';
              txt_y_minus.Text := 'Y-';
              txt_x_plus.Text := 'X+';
              txt_x_minus.Text := 'X-';
            end;
          end
        end
        else if sdl_event.type_ = SDL_JOYBUTTONDOWN then
        begin
          jNum := get_map_key(sdl_event.jbutton.button);
          if jNum <> '-1' then
          begin
            /// Here is a problem
            vComp := Self.FindComponent('img_button' + sdl_event.jbutton.button.ToString);
            (vComp as TImage).Bitmap.LoadFromFile(pressed);
            vComp := Self.FindComponent('txt_but' + sdl_event.jbutton.button.ToString);
            (vComp as TText).Text := 'Button ' + sdl_event.jbutton.button.ToString;
          end;
        end
        else if sdl_event.type_ = SDL_JOYBUTTONUP then
        begin
          jNum := get_map_key(sdl_event.jbutton.button);
          if jNum <> '-1' then
          begin
            vComp := Self.FindComponent('img_button' + sdl_event.jbutton.button.ToString);
            (vComp as TImage).Bitmap.LoadFromFile(idle);
          end;
        end;
      end;
      Sleep(20);
    end;
  end;
end;

procedure Tcontrol_joystick.tmr_joy_editTimer(Sender: TObject);
begin
  if (passes > 3) and (wiz_joy = false) then
  begin
    joy_text.Text := before_joykey;
    tmr_joy_edit.Enabled := false;
    if (joy_rect.name = 'rect_deadzone_x_value') or (joy_rect.name = 'rect_deadzone_y_value') then
    begin
      edt_deadzone_x_value.Visible := false;
      edt_deadzone_y_value.Visible := false;
    end;
    if joy_edit_active then
      joy_edit_active := false;
    txt_guide.Text := '';
  end
  else
  begin
    inc(dots);
    joy_text.Text := joy_text.Text + '.';
    if dots = 4 then
    begin
      joy_text.Text := '';
      inc(passes);
      dots := 0;
    end;
  end;
end;

procedure Tcontrol_joystick.tmr_joy_wizardTimer(Sender: TObject);
begin
  //
end;

procedure Tcontrol_joystick.wizard_phase(num: byte);
  procedure set_enable_mode(j_rect: TRectangle; j_text: TText);
  begin
    j_rect.Fill.Color := $FF44BEB0;
    j_rect.stroke.Thickness := 1;
    j_rect.Cursor := crHandPoint;
    j_text.TextSettings.FontColor := TAlphaColorRec.White;

    if ContainsText(joy_text.name, 'but') then
      txt_guide.Text := 'Press to set new button'
    else if ContainsText(joy_text.name, 'x_plus') then
      txt_guide.Text := 'Move joystick to set x value'
    else if ContainsText(joy_text.name, 'x_minus') then
      txt_guide.Text := 'Move joystick to set x value'
    else if ContainsText(joy_text.name, 'y_plus') then
      txt_guide.Text := 'Move joystick to set y value'
    else if ContainsText(joy_text.name, 'y_minus') then
      txt_guide.Text := 'Move joystick to set y value';
  end;

begin
  set_mode(TAlphaColorRec.White, TAlphaColorRec.Black, 0, crDefault);
  set_enable_mode(joy_rect, joy_text);
end;

end.
