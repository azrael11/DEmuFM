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
  vars_consts;

type
  TPRJ_IMAGES_PATH = record
    main: string;
    bar: string;
    config: string;
    flags: string;
    controls: string;
  end;

type
  TMAIN_CONFIG = record
    first_rum: string;
    prj_name: string;
    prj_version: string;
    prj_path: string;
    prj_images_path: TPRJ_IMAGES_PATH;
    prj_themes: string;
    prj_kind: TPRJ_KIND_TYPE;
    prj_sounds: string;
    prj_fonts: string;
    prj_temp: string;
    hi_score_path: string;
    nvram_path: string;
    qsnapshot_path: string;
    samples_path: string;
    sound: integer;
    video: integer;
    lang: integer;
    scraper: string;
  end;

type
  TEMULATOR_PATHS = record
    box_art: string;
    snapshots: string;
    video: string;
    manuals: string;
    roms: string;
    bezels: string;
  end;

type
  TEMU_IN_GAME = record
    fps_show: boolean;
    fps_count: boolean;
    fps_temp: string;
    pause: boolean;
  end;

const
  media_path = 'media' + PathDelim;
  folders: array [0 .. 5] of string = ('box_art' + PathDelim, 'snapshots' + PathDelim, 'video' + PathDelim, 'manuals' + PathDelim, 'rom' + PathDelim, 'bezels' + PathDelim);
  paths: array [0 .. 14] of string = ('arcade' + PathDelim, 'spectrum' + PathDelim, 'amstrad' + PathDelim, 'commodore64' + PathDelim, 'game_and_watch' + PathDelim, 'nes' + PathDelim,
    'gameboy' + PathDelim, 'gameboy_color' + PathDelim, 'colecovision' + PathDelim, 'chip8' + PathDelim, 'master_system' + PathDelim, 'sg_1000' + PathDelim, 'game_gear' + PathDelim,
    'super_cassette_vision' + PathDelim, 'mega_drive' + PathDelim);

type
  TMAIN_CONFIG_VARS = class
  private
    // function is_global_rom_path_exists(var emu_num: byte; out value: string): boolean;
  protected
    // procedure update_field();
  public
    // main: TMAIN_CONFIG;
    emu_path: array [0 .. 14] of TEMULATOR_PATHS;
    constructor create;
    destructor destroy;

    procedure create_first_config;
    procedure read_config_from_database;
    // procedure save_config_to_database;

    procedure init_bass;

    procedure getArcadeKeyMap;
  end;

var
  config: TMAIN_CONFIG_VARS;
  emu_active: TEmulatorSelected;
  Program_State: TPRJ_STATE;
  emu_in_game: TEMU_IN_GAME;

implementation

uses
  main,
  uDataModule,
  controls_engine;

{ TMAIN_CONFIG_VARS }

constructor TMAIN_CONFIG_VARS.create;
begin
  if dm.tConfig.RecordCount = 0 then
    create_first_config
  else
    read_config_from_database;
  init_bass;
  getArcadeKeyMap;
end;

procedure TMAIN_CONFIG_VARS.create_first_config;
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

  if not TDirectory.Exists(dm.tConfighiscore.value) then
    TDirectory.CreateDirectory(dm.tConfighiscore.value);
  if not TDirectory.Exists(dm.tConfignvram.value) then
    TDirectory.CreateDirectory(dm.tConfignvram.value);
  if not TDirectory.Exists(dm.tConfigqsnapshot.value) then
    TDirectory.CreateDirectory(dm.tConfigqsnapshot.value);
  if not TDirectory.Exists(dm.tConfigsamples.value) then
    TDirectory.CreateDirectory(dm.tConfigsamples.value);

  not_found := dm.tConfigprj_images_main.AsString + 'not_found.png';

  // for vi := 0 to 14 do
  // begin
  // emu_path[vi].box_art := main.prj_path + media_path + paths[vi] + folders[0];
  // TDirectory.CreateDirectory(emu_path[vi].box_art);
  // TFile.Copy(not_found, emu_path[vi].box_art + 'not_found.png', True);
  // emu_path[vi].snapshots := main.prj_path + media_path + paths[vi] + folders[1];
  // TDirectory.CreateDirectory(emu_path[vi].snapshots);
  // TFile.Copy(not_found, emu_path[vi].snapshots + 'not_found.png');
  // emu_path[vi].video := main.prj_path + media_path + paths[vi] + folders[2];
  // TDirectory.CreateDirectory(emu_path[vi].video);
  // TFile.Copy(not_found, emu_path[vi].video + 'not_found.png');
  // emu_path[vi].manuals := main.prj_path + media_path + paths[vi] + folders[3];
  // TDirectory.CreateDirectory(emu_path[vi].manuals);
  // TFile.Copy(not_found, emu_path[vi].manuals + 'not_found.png');
  // emu_path[vi].roms := main.prj_path + media_path + paths[vi] + folders[4];
  // TDirectory.CreateDirectory(emu_path[vi].roms);
  // emu_path[vi].bezels := main.prj_path + media_path + paths[vi] + folders[5];
  // TDirectory.CreateDirectory(emu_path[vi].bezels);
  //
  // emulator := paths[vi];
  // Delete(emulator, length(emulator), 1);
  //
  // dm.tConfigEmusemulator.value := emulator;
  // dm.tConfigEmusbox_art.value := emu_path[vi].box_art;
  // dm.tConfigEmussnapshot.value := emu_path[vi].snapshots;
  // dm.tConfigEmusvideo.value := emu_path[vi].video;
  // dm.tConfigEmusmanual.value := emu_path[vi].manuals;
  // dm.tConfigEmusbezels.value := emu_path[vi].bezels;
  // dm.tConfigEmus.Post;
  // dm.tConfigEmus.ApplyUpdates();
  // end;
  emu_active := emus_Arcade;
  Program_State := PRJ_STATE_FRONTEND;
end;

destructor TMAIN_CONFIG_VARS.destroy;
begin

end;

procedure TMAIN_CONFIG_VARS.getArcadeKeyMap;
var
  setNum: integer;
begin
  with dm.tKeyboard do
  begin
    first;
    while not eof do
    begin
      if (FieldByName('emulator').AsString = 'arcade') then
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

  // ������ �� ���� ��� ����� ��� �� joystick ��� �� ������ ��� ��� ����

  // with dm.tJoystick do
  // begin
  // first;
  // while not eof do
  // begin
  // if (FieldByName('emulator').AsString = 'arcade') then
  // begin
  // setNum:= FieldByName('player').AsInteger;
  // dec(setNum);
  // p_contrls.map_arcade.ncoin[setNum] := dm.tJoystick.FieldByName('key_coin').AsInteger;
  // p_contrls.map_arcade.nstart[setNum] := dm.tJoystick.FieldByName('key_start').AsInteger;
  // p_contrls.map_arcade.nup[setNum] := dm.tJoystick.FieldByName('key_up').AsInteger;
  // p_contrls.map_arcade.ndown[setNum] := dm.tJoystick.FieldByName('key_down').AsInteger;
  // p_contrls.map_arcade.nleft[setNum] := dm.tJoystick.FieldByName('key_left').AsInteger;
  // p_contrls.map_arcade.nright[setNum] := dm.tJoystick.FieldByName('key_right').AsInteger;
  // p_contrls.map_arcade.nbut0[setNum] := dm.tJoystick.FieldByName('key_b0').AsInteger;
  // p_contrls.map_arcade.nbut1[setNum] := dm.tJoystick.FieldByName('key_b1').AsInteger;
  // p_contrls.map_arcade.nbut2[setNum] := dm.tJoystick.FieldByName('key_b2').AsInteger;
  // p_contrls.map_arcade.nbut3[setNum] := dm.tJoystick.FieldByName('key_b3').AsInteger;
  // p_contrls.map_arcade.nbut4[setNum] := dm.tJoystick.FieldByName('key_b4').AsInteger;
  // p_contrls.map_arcade.nbut5[setNum] := dm.tJoystick.FieldByName('key_b5').AsInteger;
  // end;
  // next;
  // end;
  // end;
end;

procedure TMAIN_CONFIG_VARS.init_bass;
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
    ShowMessage('Plug in : BASS_FX not loading : incorrect version (Bass = ' + BASSVERSIONTEXT + ' <> ' + BASS_FX_GetVersion.ToString + ' (Bass error : ' + Bass_ErrorGetCode.ToString + ')');
  end;
end;

procedure TMAIN_CONFIG_VARS.read_config_from_database;
  procedure check_for_not_found_img(path: string);
  begin
    // if not FileExists(path) then
    // TFile.Copy(main.prj_images_path.main + 'not_found.png', path, True);
  end;

begin
  dm.tConfigEmus.first;
  while not dm.tConfigEmus.eof do
  begin
    check_for_not_found_img(dm.tConfigEmusbox_art.AsString + 'not_found.png');
    check_for_not_found_img(dm.tConfigEmussnapshot.AsString + 'not_found.png');
    check_for_not_found_img(dm.tConfigEmusvideo.AsString + 'not_found.png');
    check_for_not_found_img(dm.tConfigEmusmanual.AsString + 'not_found.png');
    dm.tConfigEmus.next;
  end;
end;

end.
