unit f_controls_gamepad;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.Objects,
  FMX.Controls.Presentation,
  SDL2;

type
  Tcontrol_gamepad = class(TFrame)
    spb_edit: TSpeedButton;
    img_cg_edit: TImage;
    eff_frgb_cc_players_p1_edit: TFillRGBEffect;
    circle_stick_left: TCircle;
    circle_stick_left_ball: TCircle;
    img_pad_up: TImage;
    img_pad_right: TImage;
    img_pad_left: TImage;
    img_pad_down: TImage;
    img_but_triangle: TImage;
    rect_but_triangle: TRectangle;
    img_but_ex: TImage;
    rect_but_square: TRectangle;
    img_but_square: TImage;
    img_but_circle: TImage;
    circle_stick_right: TCircle;
    circle_stick_right_ball: TCircle;
    rect_trigger_right: TRectangle;
    img_trigger_right: TImage;
    rect_but_right: TRectangle;
    img_but_right: TImage;
    rect_trigger_left: TRectangle;
    img_trigger_left: TImage;
    rect_but_left: TRectangle;
    img_but_left: TImage;
    pb_left_trigger: TProgressBar;
    pb_right_trigger: TProgressBar;
    img_but_coin: TImage;
    img_but_start: TImage;
    rect_down: TRectangle;
    txt_down: TText;
    rect_up: TRectangle;
    txt_up: TText;
    rect_right: TRectangle;
    txt_right: TText;
    rect_left: TRectangle;
    txt_left: TText;
    rect_left_one: TRectangle;
    txt_left_one: TText;
    rect_left_two: TRectangle;
    txt_left_two: TText;
    rect_start: TRectangle;
    txt_start: TText;
    rect_coin: TRectangle;
    txt_coin: TText;
    rect_right_one: TRectangle;
    txt_right_one: TText;
    rect_right_two: TRectangle;
    txt_right_two: TText;
    rect_triangle: TRectangle;
    txt_triangle: TText;
    rect_cross: TRectangle;
    txt_cross: TText;
    rect_square: TRectangle;
    txt_square: TText;
    rect_circle: TRectangle;
    txt_circle: TText;
    rect_deadzone_stick_left_value: TRectangle;
    txt_deadzone_stick_left_value: TText;
    rect_deadzone_stick_right_value: TRectangle;
    txt_deadzone_stick_right_value: TText;
    rect_left_stick_y_plus: TRectangle;
    txt_left_stick_y_plus: TText;
    rect_left_stick_y_minus: TRectangle;
    txt_left_stick_y_minus: TText;
    rect_right_stick_y_plus: TRectangle;
    txt_right_stick_y_plus: TText;
    rect_right_stick_y_minus: TRectangle;
    txt_right_stick_y_minus: TText;
    rect_left_stick_x_minus: TRectangle;
    txt_left_stick_x_minus: TText;
    rect_left_stick_x_plus: TRectangle;
    txt_left_stick_x_plus: TText;
    rect_right_stick_x_plus: TRectangle;
    txt_right_stick_x_plus: TText;
    rect_right_stick_x_minus: TRectangle;
    txt_right_stick_x_minus: TText;
    rect_deadzone_stick_left: TRectangle;
    txt_deadzone_stick_left: TText;
    rect_deadzone_stick_right: TRectangle;
    txt_deadzone_stick_right: TText;
    rect_but_circle: TRectangle;
    rect_but_cross: TRectangle;
  private
    { Private declarations }
  public
    { Public declarations }
    gpd_active: boolean;
    procedure test_gamepad;
  end;

implementation

uses
  umain_config,
  config_controls,
  controls_engine;

{$R *.fmx}
{ Tcontrol_gamepad }

procedure Tcontrol_gamepad.test_gamepad;
var
  // sdl2_handle: integer;
  sdl_event: TSDL_Event;
  // running: boolean;
  // player, mappings, joy_sel: integer;
  // Action: String;
  sdl_axis: integer;
  mappings: integer;
  pl, gpd: integer;
  g_axis: TSDL_GameControllerAxis;
begin
  gpd_active := true;
  pl := frm_config_controls.tc_cc_players.TabIndex;
  gpd := frm_config_controls.cb_cc_players_controller.ItemIndex - 1;
  mappings := SDL_GameControllerAddMappingsFromFile('gamecontrollerdb.txt');
  game_controller_def[pl] := SDL_GameControllerOpen(gpd);
  while gpd_active do
  begin
    Application.ProcessMessages;
    while (SDL_PollEvent(@sdl_event)) <> 0 do
    begin
      if sdl_event.type_ = SDL_KEYDOWN then
      begin
        if SDLK_ESCAPE = sdl_event.Key.keysym.sym then
        begin
          // do some stop editing actions if editing is enable
        end;
      end
      else if sdl_event.type_ = SDL_CONTROLLERAXISMOTION then
      begin
        if sdl_event.caxis.axis in [0, 2] then
        begin
          if sdl_event.caxis.axis = 0 then
          begin
            if sdl_event.caxis.value > 8000 then
              circle_stick_left_ball.Position.X := Round((sdl_event.jaxis.value * 50) / 32767)
            else if sdl_event.caxis.value < -8000 then
              circle_stick_left_ball.Position.X := Round((sdl_event.jaxis.value * 50) / 32767) + 50
            else
              circle_stick_left_ball.Position.X := 25;
          end
          else if sdl_event.caxis.axis = 2 then
          begin
            if sdl_event.caxis.value > 8000 then
              circle_stick_right_ball.Position.X := Round((sdl_event.jaxis.value * 50) / 32767)
            else if sdl_event.caxis.value < -8000 then
              circle_stick_right_ball.Position.X := Round((sdl_event.jaxis.value * 50) / 32767) + 50
            else
              circle_stick_right_ball.Position.X := 25;
          end
        end
        else if sdl_event.caxis.axis in [1, 3] then
        begin
          if sdl_event.caxis.axis = 1 then
          begin
            if sdl_event.caxis.value > 8000 then
              circle_stick_left_ball.Position.Y := Round((sdl_event.jaxis.value * 50) / 32767)
            else if sdl_event.caxis.value < -8000 then
              circle_stick_left_ball.Position.Y := Round((sdl_event.jaxis.value * 50) / 32767) + 50
            else
              circle_stick_left_ball.Position.Y := 25;
          end
          else if sdl_event.caxis.axis = 3 then
          begin
            if sdl_event.caxis.value > 8000 then
              circle_stick_right_ball.Position.Y := Round((sdl_event.jaxis.value * 50) / 32767)
            else if sdl_event.caxis.value < -8000 then
              circle_stick_right_ball.Position.Y := Round((sdl_event.jaxis.value * 50) / 32767) + 50
            else
              circle_stick_right_ball.Position.Y := 25;
          end;
        end
        else if sdl_event.caxis.axis = 4 then
        begin
          if sdl_event.caxis.value > 10 then
          begin
            pb_left_trigger.value := Round((sdl_event.jaxis.value * 100) / 32767);
            rect_trigger_left.Fill.Color := $FFF52D32;
          end
          else
          begin
            pb_left_trigger.value := 0;
            rect_trigger_left.Fill.Color := TAlphaColorRec.White;
          end;
        end
        else if sdl_event.caxis.axis = 5 then
        begin
          if sdl_event.caxis.value > 10 then
          begin
            pb_right_trigger.value := Round((sdl_event.jaxis.value * 100) / 32767);
            rect_trigger_right.Fill.Color := $FFF52D32;
          end
          else
          begin
            pb_right_trigger.value := 0;
            rect_trigger_right.Fill.Color := TAlphaColorRec.White;
          end;
        end;
      end
      else if sdl_event.type_ = SDL_CONTROLLERBUTTONDOWN then
      begin
        if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_A then
        begin
          rect_but_cross.Fill.Color := $FF6C8BC9;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_B then
        begin
          rect_but_circle.Fill.Color := $FFF52D32;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_X then
        begin
          rect_but_square.Fill.Color := $FFDB8FC4;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_Y then
        begin
          rect_but_triangle.Fill.Color := $FF30DE98;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_BACK then
        begin
//          img_but_coin.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'options_pressed.png');
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_START then
        begin
//          img_but_start.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'options_pressed.png');
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_LEFTSTICK then
        begin
          circle_stick_left_ball.Fill.Color := TAlphaColorRec.Deepskyblue;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_RIGHTSTICK then
        begin
          circle_stick_right_ball.Fill.Color := TAlphaColorRec.Deepskyblue;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_LEFTSHOULDER then
        begin
          rect_but_left.Fill.Color := $FF30DE98;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_RIGHTSHOULDER then
        begin
          rect_but_right.Fill.Color := $FF30DE98;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_UP then
        begin
//          img_pad_up.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'hat_pressed_button.png');
//          img_pad_up.RotationAngle := 90;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_DOWN then
        begin
//          img_pad_down.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'hat_pressed_button.png');
//          img_pad_down.RotationAngle := 270;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_LEFT then
        begin
//          img_pad_left.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'hat_pressed_button.png');
//          img_pad_left.RotationAngle := 0;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_RIGHT then
        begin
//          img_pad_right.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'hat_pressed_button.png');
//          img_pad_right.RotationAngle := 180;
        end;
      end
      else if sdl_event.type_ = SDL_CONTROLLERBUTTONUP then
      begin
        if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_A then
          rect_but_cross.Fill.Color := TAlphaColorRec.White
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_B then
          rect_but_circle.Fill.Color := TAlphaColorRec.White
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_X then
          rect_but_square.Fill.Color := TAlphaColorRec.White
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_Y then
          rect_but_triangle.Fill.Color := TAlphaColorRec.White
//        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_BACK then
//          img_but_coin.Bitmap.LoadFromFile(config.main.prj_images_path.Controls + 'options.png')
//        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_START then
//          img_but_start.Bitmap.LoadFromFile(config.main.prj_images_path.Controls + 'options.png')
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_LEFTSTICK then
          circle_stick_left_ball.Fill.Color := TAlphaColorRec.Red
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_RIGHTSTICK then
          circle_stick_right_ball.Fill.Color := TAlphaColorRec.Red
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_LEFTSHOULDER then
          rect_but_left.Fill.Color := TAlphaColorRec.White
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_RIGHTSHOULDER then
          rect_but_right.Fill.Color := TAlphaColorRec.White
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_UP then
        begin
//          img_pad_up.Bitmap.LoadFromFile(config.main.prj_images_path.Controls + 'hat_button.png');
//          img_pad_up.RotationAngle := 270;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_DOWN then
        begin
//          img_pad_down.Bitmap.LoadFromFile(config.main.prj_images_path.Controls + 'hat_button.png');
//          img_pad_down.RotationAngle := 90;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_LEFT then
        begin
//          img_pad_left.Bitmap.LoadFromFile(config.main.prj_images_path.Controls + 'hat_button.png');
//          img_pad_left.RotationAngle := 180;
        end
        else if sdl_event.cbutton.button = SDL_CONTROLLER_BUTTON_DPAD_RIGHT then
        begin
//          img_pad_right.Bitmap.LoadFromFile(config.main.prj_images_path.Controls +
//            'hat_button.png');
//          img_pad_right.RotationAngle := 0;
        end;
      end;
    end;
    sleep(20);
  end;
end;

end.
