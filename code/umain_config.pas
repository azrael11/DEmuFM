unit umain_config;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  WinApi.Windows,
  FireDac.stan.Param,
  FMX.Dialogs,
  bass,
  bass_fx,
  SDL2,
  SDL2_TTF,
  SDL2_IMAGE,
  SDL2_Mixer,
  main_engine,
  vars_consts;

type
  TMAIN_CONFIG_VARS = class
  private

  protected

  public
    constructor create;
    destructor destroy;

    function connectToDatabase: boolean;
    procedure createFirstConfig;

    procedure initBassAudioLibrary;
    procedure initSDLGraphicsLibrary;
    procedure loadSounds;
    procedure loadFonts;

    procedure getArcadeKeyMap;
    procedure getInGameKeyMap;
    procedure getFrontEndKeyMap;

  end;

var
  config: TMAIN_CONFIG_VARS;

implementation

uses
  main,
  uDataModule,
  controls_engine, FMX.Forms;

{ TMAIN_CONFIG_VARS }

function TMAIN_CONFIG_VARS.connectToDatabase: boolean;
begin
  dm.fdconn.Connected := true;
  result := dm.fdconn.Connected;
end;

constructor TMAIN_CONFIG_VARS.create;
begin
  if connectToDatabase then
  begin
    dm.tConfig.Active := true;
    if dm.tConfig.RecordCount = 0 then
      createFirstConfig;
    initBassAudioLibrary;
    loadSounds;
    initSDLGraphicsLibrary;
    loadFonts;
    getArcadeKeyMap;
    getInGameKeyMap;
    getFrontEndKeyMap;
  end
  else
  begin
    showMessage('Can''t connect to database. Something goes to wrong');
    application.Terminate;
  end;

end;

procedure TMAIN_CONFIG_VARS.createFirstConfig;
var
  vi: integer;
  emulator, not_found: string;
begin
  dm.tConfigfirstrun.value := 1;
  dm.tConfigcurrent_emu.value := 'arcade';
  dm.tConfigprj_name.value := ExtractFileName(ParamStr(0));
  dm.tConfigprj_path.value := ExtractFilePath(ParamStr(0));
  dm.tConfigprj_version.value := '0.2 (WIP)';
  dm.tConfigprj_images_main.value := dm.tConfigprj_path.value + 'images' + PathDelim;
  dm.tConfigprj_images_bar.value := dm.tConfigprj_path.value + 'bar' + PathDelim;
  dm.tConfigprj_images_config.value := dm.tConfigprj_path.value + 'config' + PathDelim;
  dm.tConfigprj_images_flags.value := dm.tConfigprj_path.value + 'lang_flags' + PathDelim;
  dm.tConfigprj_images_controls.value := dm.tConfigprj_path.value + 'controls' + PathDelim;
  dm.tConfigprj_media.value := dm.tConfigprj_path.value + 'media' + PathDelim;
  dm.tConfigprj_export.value := dm.tConfigprj_path.value + 'export' + PathDelim;
  dm.tConfigprj_themes.value := dm.tConfigprj_path.value + 'themes' + PathDelim;
  dm.tConfigprj_sounds.value := dm.tConfigprj_path.value + 'sounds' + PathDelim;
  dm.tConfigprj_fonts.value := dm.tConfigprj_path.value + 'fonts' + PathDelim;
  dm.tConfigprj_temp.value := dm.tConfigprj_path.value + 'temp' + PathDelim;
  dm.tConfigprj_kind.value := 'medium_thumps';
  dm.tConfiglang.value := 0;
  dm.tConfighiscore.value := dm.tConfigprj_path.value + 'hiscore' + PathDelim;
  dm.tConfignvram.value := dm.tConfigprj_path.value + 'nvram' + PathDelim;
  dm.tConfigqsnapshot.value := dm.tConfigprj_path.value + 'qsnap' + PathDelim;
  dm.tConfigsamples.value := dm.tConfigprj_path.value + 'media' + PathDelim + 'arcade' + PathDelim + 'samples' + PathDelim;
  dm.tConfigsound.value := 1;
  dm.tConfigvideo.value := 2;
  dm.tConfig.Post;
  dm.tConfig.ApplyUpdates();

  if not System.IOUtils.TDirectory.Exists(dm.tConfighiscore.value) then
    System.IOUtils.TDirectory.CreateDirectory(dm.tConfighiscore.value);
  if not System.IOUtils.TDirectory.Exists(dm.tConfignvram.value) then
    System.IOUtils.TDirectory.CreateDirectory(dm.tConfignvram.value);
  if not System.IOUtils.TDirectory.Exists(dm.tConfigqsnapshot.value) then
    System.IOUtils.TDirectory.CreateDirectory(dm.tConfigqsnapshot.value);
  if not System.IOUtils.TDirectory.Exists(dm.tConfigsamples.value) then
    System.IOUtils.TDirectory.CreateDirectory(dm.tConfigsamples.value);

  not_found := dm.tConfigprj_images_main.AsString + 'not_found.png';
end;

destructor TMAIN_CONFIG_VARS.destroy;
begin

end;

procedure TMAIN_CONFIG_VARS.getArcadeKeyMap;
var
  setNum: integer;
begin
  dm.tKeyboard.Active := true;
  with dm.tKeyboard do
  begin
    first;
    while not eof do
    begin
      if FieldByName('emulator').AsString = 'arcade' then
      begin
        setNum := FieldByName('player').AsInteger;
        dec(setNum);
        p_contrls.map_arcade.ncoin[setNum] := dm.tKeyboard.FieldByName('key_coin').AsInteger;
        p_contrls.map_arcade.nstart[setNum] := dm.tKeyboard.FieldByName('key_start').AsInteger;
        p_contrls.map_arcade.nup[setNum] := dm.tKeyboard.FieldByName('key_up').AsInteger;
        p_contrls.map_arcade.ndown[setNum] := dm.tKeyboard.FieldByName('key_down').AsInteger;
        p_contrls.map_arcade.nleft[setNum] := dm.tKeyboard.FieldByName('key_left').AsInteger;
        p_contrls.map_arcade.nright[setNum] := dm.tKeyboard.FieldByName('key_right').AsInteger;
        p_contrls.map_arcade.nbut0[setNum] := dm.tKeyboard.FieldByName('key_b0').AsInteger;
        p_contrls.map_arcade.nbut1[setNum] := dm.tKeyboard.FieldByName('key_b1').AsInteger;
        p_contrls.map_arcade.nbut2[setNum] := dm.tKeyboard.FieldByName('key_b2').AsInteger;
        p_contrls.map_arcade.nbut3[setNum] := dm.tKeyboard.FieldByName('key_b3').AsInteger;
        p_contrls.map_arcade.nbut4[setNum] := dm.tKeyboard.FieldByName('key_b4').AsInteger;
        p_contrls.map_arcade.nbut5[setNum] := dm.tKeyboard.FieldByName('key_b5').AsInteger;
      end;
      next;
    end;
  end;
  dm.tKeyboard.Active := false;

  dm.tKeyboard.Active := true;
  with dm.tKeyboard do
  begin
    first;
    while not eof do
    begin
      if FieldByName('emulator').AsString = 'arcade' then
      begin
        setNum := FieldByName('player').AsInteger;
        dec(setNum);
        temp_key[setNum].player := (setNum + 1).ToString;
        temp_key[setNum].profile := 'Default';
        temp_key[setNum].platform_type := 'arcade';
        temp_key[setNum].key.coin := dm.tKeyboard.FieldByName('key_coin').AsInteger;
        temp_key[setNum].key.start := dm.tKeyboard.FieldByName('key_start').AsInteger;
        temp_key[setNum].key.up := dm.tKeyboard.FieldByName('key_up').AsInteger;
        temp_key[setNum].key.down := dm.tKeyboard.FieldByName('key_down').AsInteger;
        temp_key[setNum].key.left := dm.tKeyboard.FieldByName('key_left').AsInteger;
        temp_key[setNum].key.right := dm.tKeyboard.FieldByName('key_right').AsInteger;
        temp_key[setNum].key.b0 := dm.tKeyboard.FieldByName('key_b0').AsInteger;
        temp_key[setNum].key.b1 := dm.tKeyboard.FieldByName('key_b1').AsInteger;
        temp_key[setNum].key.b2 := dm.tKeyboard.FieldByName('key_b2').AsInteger;
        temp_key[setNum].key.b3 := dm.tKeyboard.FieldByName('key_b3').AsInteger;
        temp_key[setNum].key.b4 := dm.tKeyboard.FieldByName('key_b4').AsInteger;
        temp_key[setNum].key.b5 := dm.tKeyboard.FieldByName('key_b5').AsInteger;
      end;
      next;
    end;
  end;

  dm.tKeyboard.Active := false;

  dm.tJoystick.Active := true;
  with dm.tJoystick do
  begin
    first;
    while not eof do
    begin
      if FieldByName('emulator').AsString = 'arcade' then
      begin
        setNum := FieldByName('player').AsInteger;
        dec(setNum);
        temp_joy[setNum].profile := 'Default';
        temp_joy[setNum].player := (setNum + 1).ToString;
        temp_joy[setNum].name := dm.tJoystickname.AsString;
        temp_joy[setNum].vtype := dm.tJoystickjtype.AsString;
        temp_joy[setNum].serial := dm.tJoystickserial.AsString;
        temp_joy[setNum].buttons := dm.tJoystickbuttons.AsString;
        temp_joy[setNum].guid := dm.tJoystickguid.AsString;
        temp_joy[setNum].vendor := dm.tJoystickvendor.AsString;
        temp_joy[setNum].product := dm.tJoystickproduct.AsString;
        temp_joy[setNum].product_version := dm.tJoystickproduct_version.AsString;
        temp_joy[setNum].joy.up := dm.tJoystickaxis_y_plus.AsInteger;
        temp_joy[setNum].joy.down := dm.tJoystickaxis_y_minus.AsInteger;
        temp_joy[setNum].joy.left := dm.tJoystickaxis_x_minus.AsInteger;
        temp_joy[setNum].joy.right := dm.tJoystickaxis_x_plus.AsInteger;
        temp_joy[setNum].joy.b0 := dm.tJoystickbutton0.AsInteger;
        temp_joy[setNum].joy.b1 := dm.tJoystickbutton1.AsInteger;
        temp_joy[setNum].joy.b2 := dm.tJoystickbutton2.AsInteger;
        temp_joy[setNum].joy.b3 := dm.tJoystickbutton3.AsInteger;
        temp_joy[setNum].joy.b4 := dm.tJoystickbutton4.AsInteger;
        temp_joy[setNum].joy.b5 := dm.tJoystickbutton5.AsInteger;
        temp_joy[setNum].joy.b6 := dm.tJoystickbutton6.AsInteger;
        temp_joy[setNum].joy.b7 := dm.tJoystickbutton7.AsInteger;
        temp_joy[setNum].joy.b8 := dm.tJoystickbutton8.AsInteger;
        temp_joy[setNum].joy.b9 := dm.tJoystickbutton9.AsInteger;
        temp_joy[setNum].joy.b10 := dm.tJoystickbutton10.AsInteger;
        temp_joy[setNum].joy.b11 := dm.tJoystickbutton11.AsInteger;
        temp_joy[setNum].joy.b12 := dm.tJoystickbutton12.AsInteger;
        temp_joy[setNum].joy.b13 := dm.tJoystickbutton13.AsInteger;
        temp_joy[setNum].joy.b14 := dm.tJoystickbutton14.AsInteger;
        temp_joy[setNum].joy.b15 := dm.tJoystickbutton15.AsInteger;
        temp_joy[setNum].joy.start := dm.tJoystickstart.AsInteger;
        temp_joy[setNum].joy.coin := dm.tJoystickcoin.AsInteger;
        temp_joy[setNum].joy.deadzone_x := dm.tJoystickdeadzone_x.AsInteger;
        temp_joy[setNum].joy.deadzone_y := dm.tJoystickdeadzone_y.AsInteger;
      end;
      next;
    end;
  end;
  dm.tJoystick.Active := false;

  dm.tGamepad.Active := true;
  dm.tGamepad.Active := false;

end;

procedure TMAIN_CONFIG_VARS.getFrontEndKeyMap;
begin
  dm.tKeyboardFrontend.Active := true;
  dm.tKeyboardFrontend.Locate('name', 'Default', []);
  map_frontend_actions.name := dm.tKeyboardFrontendname.AsString;
  map_frontend_actions.quit_DEmuFM := dm.tKeyboardFrontendquit_DEmuFM.AsLongWord;
  map_frontend_actions.play_game := dm.tKeyboardFrontendplay.AsLongWord;
  map_frontend_actions.move_up := dm.tKeyboardFrontendmove_up.AsLongWord;
  map_frontend_actions.move_left := dm.tKeyboardFrontendmove_left.AsLongWord;
  map_frontend_actions.move_right := dm.tKeyboardFrontendmove_right.AsLongWord;
  map_frontend_actions.move_down := dm.tKeyboardFrontendmove_down.AsLongWord;
  map_frontend_actions.show_configuration := dm.tKeyboardFrontendshow_configuration.AsLongWord;
  map_frontend_actions.show_display := dm.tKeyboardFrontendshow_display.AsLongWord;
  map_frontend_actions.show_controls := dm.tKeyboardFrontendshow_controls.AsLongWord;
  map_frontend_actions.show_information := dm.tKeyboardFrontendshow_information.AsLongWord;
  map_frontend_actions.show_hide_time_game := dm.tKeyboardFrontendshow_hide_time_game.AsLongWord;
  map_frontend_actions.show_choose_platform := dm.tKeyboardFrontendshow_choose_platform.AsLongWord;
  dm.tKeyboardFrontend.Active := false;
end;

procedure TMAIN_CONFIG_VARS.getInGameKeyMap;
begin
  dm.tKeyboardInGame.Active := true;
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
  map_ingame_actions.show_fps := dm.tKeyboardInGameshow_fps.AsLongWord;
  dm.tKeyboardInGame.Active := false;
end;

procedure TMAIN_CONFIG_VARS.initBassAudioLibrary;
begin
  if (HiWord(BASS_GetVersion)) <> BASSVERSION then
  begin

  end;

  if (HiWord(BASS_GetVersion) <> BASSVERSION) then
  begin
    MessageBox(0, 'An incorrect version of BASS.DLL was loaded', nil, MB_ICONERROR);
    Halt;
  end;

  if not BASS_Init(-1, 44100, BASS_DEVICE_SPEAKERS, WinApi.Windows.HANDLE_PTR(frm_main), nil) then
    MessageBox(0, 'Error initializing audio!', nil, MB_ICONERROR);
  if BASS_FX_GetVersion = 0 then
  begin
    showMessage('Plug in : BASS_FX not loading : incorrect version (Bass = ' + BASSVERSIONTEXT + ' <> ' + BASS_FX_GetVersion.ToString + ' (Bass error : ' + Bass_ErrorGetCode.ToString + ')');
  end;
end;

procedure TMAIN_CONFIG_VARS.initSDLGraphicsLibrary;
begin
  if SDL_Init(SDL_INIT_EVERYTHING) < 0 then
    exit;
  if TTF_Init = -1 then
    exit;
  if Mix_Init(MIX_INIT_MP3 or MIX_INIT_OGG) < 0 then
    exit;
  Mix_OpenAudio(44100, AUDIO_S32MSB, 2, 1024);
end;

procedure TMAIN_CONFIG_VARS.loadFonts;
begin
  // Font
  fps_fnt := TTF_OpenFont('fonts\Yeasty_Flavors.ttf', 24);
  if fps_fnt = nil then
    showMessage('Font not Loading');
  pause_fnt := TTF_OpenFont('fonts\Yeasty_Flavors.ttf', 48);
  if pause_fnt = nil then
    showMessage('Font not Loading');
  fps_font_color.r := 255;
  fps_font_color.g := 255;
  fps_font_color.b := 255;
end;

procedure TMAIN_CONFIG_VARS.loadSounds;
begin
  pause_sound := Mix_LoadWav(PAnsiChar(AnsiString(dm.tConfigprj_path.value + 'pause.wav')));
  unpause_sound := Mix_LoadWav(PAnsiChar(AnsiString(dm.tConfigprj_path.value + 'unpause.wav')));
end;

end.
