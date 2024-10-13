unit sound_engine;

interface

uses
  System.SysUtils,
  WinApi.Windows,
  WinApi.MMSystem,
  FMX.Media,
  timer_engine,
  FMX.Dialogs,
  FMX.DialogService,
  System.UITypes,
  SDL2,
  Bass,
  Basswv;

const
  MAX_AUDIO_BUFFER = $F;
  MAX_CHANNELS = 9;
  LONG_MAX_AUDIO = 3000;
  FREQ_BASE_AUDIO = 44100;

type
  TAUDIO_TYPES = (at_none, at_mmsystem, at_sdl, at_bass);

type
  sound_type = record
    sound_position: word;
    audio: array [0 .. MAX_CHANNELS - 1] of HWAVEOUT;
    bass_audio: array [0 .. MAX_CHANNELS - 1] of HSAMPLE;
    cpu_clock: dword;
    cpu_num: byte;
    num_buffer: byte;
    long_sample: word;
    used_channels: integer;
    stereo, sound_exist, tsound_exist: boolean;
    filter_call: array [0 .. MAX_CHANNELS - 1] of procedure(chanel: byte);
  end;

  snd_chip_class = class
  public
    function get_sample_num: byte;
  protected
    tsample_num: byte;
    amp: single;
    clock: dword;
  end;

var
  audio_type: TAUDIO_TYPES;
  sound_status: sound_type;
  update_sound_proc: exec_type_simple;
  sound_engine_timer: byte;
  tsample: array [0 .. MAX_CHANNELS - 1, 0 .. LONG_MAX_AUDIO - 1] of smallint;
  cab_audio: array [0 .. MAX_CHANNELS - 1, 0 .. MAX_AUDIO_BUFFER - 1] of wavehdr;
  // bass
  bass_cab_audio: array [0 .. MAX_CHANNELS - 1, 0 .. MAX_AUDIO_BUFFER - 1] of HSAMPLE;
  // sdl
  sound_device: TSDL_AudioDeviceID;
  sdlsample: array [0 .. MAX_CHANNELS - 1, 0 .. LONG_MAX_AUDIO - 1] of integer;
  sample_final: array [0 .. LONG_MAX_AUDIO - 1] of smallint;

function start_SDL_audio(stereo_sound: boolean): boolean;
procedure close_SDL_audio;

function start_audio(stereo_sound: boolean): boolean;
procedure sound_engine_init(num_cpu: byte; clock: dword; update_call: exec_type_simple);
procedure sound_engine_close;
procedure sound_engine_change_clock(clock: single);
procedure reset_audio;
procedure play_sound;
procedure close_audio;
procedure sound_update_internal;
function init_channel: byte;

procedure update_sample(channel, max_audio: smallint; sample: integer);

implementation

uses
  main_engine,
  uarcade_actions, uDataModule;

function snd_chip_class.get_sample_num: byte;
begin
  get_sample_num := self.tsample_num;
end;

function start_SDL_audio(stereo_sound: boolean): boolean;
var
  wanted, have: TSDL_AudioSpec;
  channels: byte;
begin
  start_SDL_audio := false;
  sound_status.tsound_exist := false;
  // abrir el audio
  if stereo_sound then
  begin
    sound_status.stereo := true;
    channels := 2;
  end
  else
  begin
    sound_status.stereo := false;
    channels := 1;
  end;
  sound_status.long_sample := round(FREQ_BASE_AUDIO / machine_calls.fps_max) * channels;
  wanted.format := $8010;
  wanted.channels := channels;
  wanted.freq := FREQ_BASE_AUDIO;
  wanted.samples := sound_status.long_sample;
  wanted.silence := 0;
  wanted.size := 0; // wanted.size:=sound_status.sample_final*2;
  wanted.callback := nil; // sound_call_back;
  wanted.userdata := nil;
  wanted.padding := 0;
  sound_device := SDL_OpenAudioDevice(nil, 0, @wanted, @have, 0);
  if (sound_device = 0) then
    exit;
  SDL_PauseAudioDevice(sound_device, 0);
  SDL_ClearQueuedAudio(sound_device);
  sound_status.tsound_exist := true;
  reset_audio;
  audio_type := at_sdl;
  start_SDL_audio := true;
end;

procedure close_SDL_audio;
begin
  SDL_CloseAudioDevice(sound_device);
end;

function start_audio(stereo_sound: boolean): boolean;
var
  format: TWaveFormatEx;
  f, g: byte;
  canales: byte;
begin
  start_audio := false;
  sound_status.tsound_exist := false;
  if stereo_sound then
  begin
    sound_status.stereo := true;
    canales := 2;
  end
  else
  begin
    sound_status.stereo := false;
    canales := 1;
  end;

  fillchar(format, SizeOf(TWaveFormatEx), 0);
  format.wFormatTag := WAVE_FORMAT_PCM;
  format.nChannels := canales;
  format.nSamplesPerSec := FREQ_BASE_AUDIO;
  format.wBitsPerSample := 16;
  format.nBlockAlign := format.nChannels * (format.wBitsPerSample div 8);
  format.nAvgBytesPerSec := FREQ_BASE_AUDIO * format.nBlockAlign;
  format.cbSize := 0;
  sound_status.long_sample := round(FREQ_BASE_AUDIO / machine_calls.fps_max) * canales;
  for g := 0 to MAX_CHANNELS - 1 do
  begin
    if not((waveoutopen(@sound_status.audio[g], WAVE_MAPPER, @format, 0, 1, CALLBACK_NULL)) = 0) then
      exit;
    For f := 0 To MAX_AUDIO_BUFFER - 1 do
    begin
      getmem(cab_audio[g][f].lpData, sound_status.long_sample * 2);
      cab_audio[g][f].dwBufferLength := sound_status.long_sample * 2;
      cab_audio[g][f].dwUser := 0;
      cab_audio[g][f].dwFlags := 0;
      cab_audio[g][f].dwLoops := 0;
      cab_audio[g][f].lpNext := nil;
      if not(waveOutPrepareHeader(sound_status.audio[g], @cab_audio[g][f], uint(SizeOf(wavehdr))) = 0) then
        exit;
    end;
  end;
  reset_audio;
  start_audio := true;
  sound_status.tsound_exist := true;
  audio_type := at_mmsystem;
  sound_engine_close;
end;

procedure close_audio;
var
  j, f: byte;
begin
  if not(sound_status.tsound_exist) then
    exit;
  for j := 0 to MAX_CHANNELS - 1 do
  begin
    waveOutReset(sound_status.audio[j]);
    for f := 0 to MAX_AUDIO_BUFFER - 1 do
    begin
      waveoutunprepareheader(sound_status.audio[j], @cab_audio[j][f], SizeOf(cab_audio[j][f]));
      freemem(cab_audio[j][f].lpData);
      cab_audio[j][f].lpData := nil;
    end;
    waveoutclose(sound_status.audio[j]);
  end;
end;

procedure play_sound;
var
  f, h, j: integer;
begin

  case audio_type of
    at_none:
      ;
    at_mmsystem:
      begin
        if ((sound_status.tsound_exist) and (sound_status.sound_exist)) then
        begin
          for f := 0 to sound_status.used_channels do
          begin
            if @sound_status.filter_call[f] <> nil then
              sound_status.filter_call[f](f);
            copymemory(cab_audio[f][sound_status.num_buffer].lpData, @tsample[f], sound_status.long_sample * SizeOf(smallint));
            waveOutWrite(sound_status.audio[f], @cab_audio[f][sound_status.num_buffer], SizeOf(wavehdr));
            fillchar(tsample[f], LONG_MAX_AUDIO * SizeOf(smallint), 0);
          end;
          sound_status.num_buffer := sound_status.num_buffer + 1;
          if sound_status.num_buffer = MAX_AUDIO_BUFFER then
            sound_status.num_buffer := 0;
        end;
        sound_status.sound_position := 0;
      end;
    at_sdl:
      begin
        if (sound_status.used_channels <> -1) then
        begin
          if main_screen.fast then
            SDL_ClearQueuedAudio(sound_device);
          for h := 0 to (sound_status.long_sample - 1) do
          begin
            j := 0;
            for f := 0 to sound_status.used_channels do
              j := j + tsample[f, h];
            j := j div (sound_status.used_channels + 1);
            if j < -32767 then
              j := -32767
            else if j > 32767 then
              j := 32767;
            sample_final[h] := j;
          end;
        end;
        SDL_QueueAudio(sound_device, @sample_final[0], sound_status.long_sample * SizeOf(smallint));
        for f := 0 to sound_status.used_channels do
          fillchar(tsample[f], LONG_MAX_AUDIO * SizeOf(integer), 0);
      end;
    at_bass:
      begin

      end;
  end;

end;

procedure sound_engine_init(num_cpu: byte; clock: dword; update_call: exec_type_simple);
begin
  sound_status.cpu_clock := clock;
  sound_status.cpu_num := num_cpu;
  sound_engine_timer := timers.init(num_cpu, clock / FREQ_BASE_AUDIO, sound_update_internal, nil, true);
  update_sound_proc := update_call;
end;

procedure sound_engine_close;
begin
  sound_status.cpu_clock := 0;
  sound_status.cpu_num := $FF;
  sound_engine_timer := $FF;
  update_sound_proc := nil;
end;

procedure sound_engine_change_clock(clock: single);
begin
  timers.timer[sound_engine_timer].time_final := clock / FREQ_BASE_AUDIO;
  sound_status.cpu_clock := trunc(clock);
end;

procedure sound_update_internal;
begin
  if @update_sound_proc <> nil then
    update_sound_proc;
  if sound_status.sound_position = sound_status.long_sample then
    play_sound
  else
    sound_status.sound_position := sound_status.sound_position + 1 + 1 * byte(sound_status.stereo);
end;

procedure reset_audio;
var
  f: byte;
begin
  sound_status.sound_exist := dm.tArcadeConfigsound.AsInteger.ToBoolean;
  sound_status.sound_position := 0;
  sound_status.num_buffer := 0;
  for f := 0 to (MAX_CHANNELS - 1) do
    fillchar(tsample[f, 0], LONG_MAX_AUDIO, 0);
end;

function init_channel: byte;
begin
  sound_status.used_channels := sound_status.used_channels + 1;
  if sound_status.used_channels >= MAX_CHANNELS then
    // MessageDialog('Utilizados mas canales de sonido de los disponibles!!',
    // TMSgDlgType.mtInformation, [TMSgDlgBtn.mbOK], 0);
    MessageDlg('Utilizados mas canales de sonido de los disponibles!!', TMSgDlgType.mtInformation, [TMSgDlgBtn.mbOK], 0);
  init_channel := sound_status.used_channels;
end;

procedure update_sample(channel, max_audio: smallint; sample: integer);
begin
  case audio_type of
    at_none:
      ;
    at_mmsystem:
      begin
        tsample[channel, max_audio] := sample;
        if sound_status.stereo then
          tsample[channel, max_audio + 1] := sample;
      end;
    at_sdl:
      begin
        sdlsample[channel, max_audio] := sample;
        if sound_status.stereo then
          tsample[channel, max_audio + 1] := sample;
      end;
    at_bass:
      ;
  end;
end;

end.
