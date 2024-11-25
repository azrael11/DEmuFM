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
    initSDLGraphicsLibrary;
    getArcadeKeyMap;
    getInGameKeyMap;
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

procedure TMAIN_CONFIG_VARS.loadSounds;
begin
  pause_sound := Mix_LoadWav(PAnsiChar(AnsiString(dm.tConfigprj_path.Value + 'pause.wav')));
  unpause_sound := Mix_LoadWav(PAnsiChar(AnsiString(dm.tConfigprj_path.Value + 'unpause.wav')));
end;

end.
