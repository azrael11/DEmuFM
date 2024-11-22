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
  TMAIN_CONFIG_VARS = class
  private

  protected

  public
    constructor create;
    destructor destroy;

    procedure createFirstConfig;
    // procedure save_config_to_database;

    procedure init_bass;
  end;

var
  config: TMAIN_CONFIG_VARS;

implementation

uses
  main,
  uDataModule,
  controls_engine;

{ TMAIN_CONFIG_VARS }

constructor TMAIN_CONFIG_VARS.create;
begin
  if dm.tConfig.RecordCount = 0 then
    createFirstConfig;
  init_bass;
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
  dm.tConfigprj_media.Value := dm.tConfigprj_path.value + 'media' + pathdelim;
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
end;

destructor TMAIN_CONFIG_VARS.destroy;
begin

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

end.
