unit crazyclimber_hw_dac;

interface

uses
  WinApi.Windows,
  sound_engine,
  timer_engine;

type
  tcclimber_audio = class
    constructor create;
    destructor free;
  public
    procedure reset;
    procedure change_sample(valor: byte);
    procedure change_freq(valor: byte);
    procedure change_volume(valor: byte);
    procedure trigger_w;
    function get_rom_addr: pbyte;
    procedure update;
  private
    sample_freq: single;
    sample_volume: single;
    tsample_num: byte;
    timer_num: byte;
    start, pos: word;
    rom: array [0 .. $1FFF] of byte;
    out_: smallint;
    up_down: boolean;
  end;

var
  cclimber_audio: tcclimber_audio;

implementation

const
  AMP = 4;

procedure cclimer_update_internal;
var
  sample: byte;
  pos: word;
begin
  pos := cclimber_audio.start + cclimber_audio.pos;
  if ((pos > $1FFF) or (cclimber_audio.rom[pos] = $70)) then
  begin
    timers.enabled(cclimber_audio.timer_num, false);
    cclimber_audio.out_ := 0;
    exit;
  end;
  if not(cclimber_audio.up_down) then
  begin
    sample := (cclimber_audio.rom[pos] and $F0) shr 4;
    cclimber_audio.up_down := true;
  end
  else
  begin
    sample := cclimber_audio.rom[pos] and $F;
    cclimber_audio.pos := cclimber_audio.pos + 1;
    cclimber_audio.up_down := false;
  end;
  cclimber_audio.out_ := trunc((($1111 * sample) - $8000) * cclimber_audio.sample_volume);
end;

function tcclimber_audio.get_rom_addr: pbyte;
begin
  get_rom_addr := @self.rom[0];
end;

constructor tcclimber_audio.create;
begin
  self.tsample_num := init_channel;
  timer_num := timers.init(sound_status.cpu_num, 1, cclimer_update_internal, nil, false);
  self.reset;
end;

destructor tcclimber_audio.free;
begin
end;

procedure tcclimber_audio.reset;
begin
  timers.enabled(self.timer_num, false);
end;

procedure tcclimber_audio.change_sample(valor: byte);
begin
  self.start := valor * 32;
end;

procedure tcclimber_audio.change_freq(valor: byte);
begin
  self.sample_freq := 3072000 / 4 / (256 - valor);
  timers.timer[self.timer_num].time_final := 3072000 / self.sample_freq;
end;

procedure tcclimber_audio.change_volume(valor: byte);
begin
  cclimber_audio.sample_volume := (valor and $1F) / (31 * AMP);
end;

procedure tcclimber_audio.trigger_w;
begin
  timers.enabled(self.timer_num, true);
  self.pos := 0;
  up_down := false;
end;

procedure tcclimber_audio.update;
begin
  tsample[self.tsample_num, sound_status.sound_position] := self.out_;
  if sound_status.stereo then
    tsample[self.tsample_num, sound_status.sound_position + 1] := self.out_;
end;

end.
