unit ay_8910;

interface

uses
  WinApi.Windows,
  System.UITypes,
  main_engine,
  sound_engine,
  cpu_misc,
  FMX.Dialogs;

type
  ay8910_chip = class(snd_chip_class)
    constructor create(clock: integer; type_: byte; amp: single = 1; internal: boolean = false);
    destructor free;
  public
    procedure write(v: byte);
    procedure control(v: byte);
    function read: byte;
    procedure reset;
    procedure update;
    function update_internal: pinteger;
    function get_control: byte;
    function get_reg(reg: byte): byte;
    procedure set_reg(reg, valor: byte);
    procedure change_io_calls(porta_read, portb_read: cpu_inport_call; porta_write, portb_write: cpu_outport_call);
    function save_snapshot(data: pbyte): word;
    procedure load_snapshot(data: pbyte);
    procedure change_clock(clock: dword);
    procedure change_gain(gain0, gain1, gain2: single);
  private
    regs: array [0 .. 15] of byte;
    perioda, periodb, periodc, periodn, periode: integer;
    counta, countb, countc, countn, counte: integer;
    vola, volb, volc, vole: integer;
    envelopea, envelopeb, envelopec: integer;
    outputa, outputb, outputc, outputn: integer;
    latch, type_: byte;
    countenv: shortint;
    hold, alternate, attack, holding, rng, updatestep: integer;
    lastenable: smallint;
    porta_read, portb_read: cpu_inport_call;
    porta_write, portb_write: cpu_outport_call;
    gain0, gain1, gain2: single;
    procedure aywritereg(r, v: byte);
    function ayreadreg(r: byte): byte;
  end;

var
  ay8910_0, ay8910_1, ay8910_2, ay8910_3, ay8910_4: ay8910_chip;

const
  AY8910 = 0;
  AY8912 = 1;

implementation

const
  AY_AFINE = 0;
  AY_ACOARSE = 1;
  AY_BFINE = 2;
  AY_BCOARSE = 3;
  AY_CFINE = 4;
  AY_CCOARSE = 5;
  AY_NOISEPER = 6;
  AY_ENABLE = 7;
  AY_AVOL = 8;
  AY_BVOL = 9;
  AY_CVOL = 10;
  AY_EFINE = 11;
  AY_ECOARSE = 12;
  AY_ESHAPE = 13;
  AY_PORTA = 14;
  AY_PORTB = 15;
  STEP = $1000;
  MAX_OUTPUT = $7FFF;

var
  salida_ay: array [0 .. 3] of integer;
  vol_table: array [0 .. 31] of single;

procedure init_table;
var
  i: integer;
  l_out: single;
begin
  l_out := MAX_OUTPUT / 4;
  for i := 31 downto 1 do
  begin
    vol_table[i] := trunc(l_out + 0.5); // roun to nearest */
    l_out := l_out / 1.188502227; // = 10 ^ (1.5/20) = 1.5dB */
  end;
  vol_table[0] := 0;
end;

procedure ay8910_chip.change_gain(gain0, gain1, gain2: single);
begin
  self.gain0 := gain0;
  self.gain1 := gain1;
  self.gain2 := gain2;
end;

procedure ay8910_chip.change_clock(clock: dword);
begin
  self.clock := clock;
  self.updatestep := trunc((STEP * FREQ_BASE_AUDIO * 8) / self.clock);
end;

constructor ay8910_chip.create(clock: integer; type_: byte; amp: single = 1; internal: boolean = false);
begin
  if addr(update_sound_proc) = nil then
    MessageDlg('ERROR: Sound chip initialized without a sound CPU!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
  init_table;
  self.clock := clock;
  self.updatestep := trunc((STEP * FREQ_BASE_AUDIO * 8) / self.clock);
  self.porta_read := nil;
  self.portb_read := nil;
  self.porta_write := nil;
  self.portb_write := nil;
  self.perioda := self.updatestep;
  self.periodb := self.updatestep;
  self.periodc := self.updatestep;
  self.periode := self.updatestep;
  self.periodn := self.updatestep;
  if not(internal) then
    self.tsample_num := init_channel;
  if amp <> 1 then
    self.amp := amp
  else
    self.amp := 2;
  self.reset;
  self.type_ := type_;
  self.gain0 := 1;
  self.gain1 := 1;
  self.gain2 := 1;
end;

procedure ay8910_chip.reset;
var
  i: byte;
begin
  self.latch := 0;
  self.outputa := 0;
  self.outputb := 0;
  self.outputc := 0;
  self.outputn := $FF;
  self.rng := 1;
  self.lastenable := -1;
  For i := 0 To 13 do
    self.aywritereg(i, 0);
end;

destructor ay8910_chip.free;
begin
end;

function ay8910_chip.save_snapshot(data: pbyte): word;
var
  temp: pbyte;
  size: word;
begin
  temp := data;
  copymemory(temp, @self.regs[0], 16);
  inc(temp, 16);
  size := 16;
  copymemory(temp, @self.perioda, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.periodb, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.periodc, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.periodn, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.periode, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.counta, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.countb, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.countc, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.countn, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.counte, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.vola, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.volb, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.volc, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.vole, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.envelopea, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.envelopeb, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.envelopec, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.outputa, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.outputb, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.outputc, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.outputn, 4);
  inc(temp, 4);
  size := size + 4;
  temp^ := self.latch;
  inc(temp);
  size := size + 1;
  temp^ := self.type_;
  inc(temp);
  size := size + 1;
  copymemory(temp, @self.countenv, sizeof(shortint));
  inc(temp, sizeof(shortint));
  size := size + sizeof(shortint);
  copymemory(temp, @self.hold, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.alternate, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.attack, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.holding, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.rng, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.updatestep, 4);
  inc(temp, 4);
  size := size + 4;
  copymemory(temp, @self.lastenable, sizeof(smallint));
  size := size + sizeof(smallint);
  save_snapshot := size;
end;

procedure ay8910_chip.load_snapshot(data: pbyte);
var
  temp: pbyte;
begin
  temp := data;
  copymemory(@self.regs[0], temp, 16);
  inc(temp, 16);
  copymemory(@self.perioda, temp, 4);
  inc(temp, 4);
  copymemory(@self.periodb, temp, 4);
  inc(temp, 4);
  copymemory(@self.periodc, temp, 4);
  inc(temp, 4);
  copymemory(@self.periodn, temp, 4);
  inc(temp, 4);
  copymemory(@self.periode, temp, 4);
  inc(temp, 4);
  copymemory(@self.counta, temp, 4);
  inc(temp, 4);
  copymemory(@self.countb, temp, 4);
  inc(temp, 4);
  copymemory(@self.countc, temp, 4);
  inc(temp, 4);
  copymemory(@self.countn, temp, 4);
  inc(temp, 4);
  copymemory(@self.counte, temp, 4);
  inc(temp, 4);
  copymemory(@self.vola, temp, 4);
  inc(temp, 4);
  copymemory(@self.volb, temp, 4);
  inc(temp, 4);
  copymemory(@self.volc, temp, 4);
  inc(temp, 4);
  copymemory(@self.vole, temp, 4);
  inc(temp, 4);
  copymemory(@self.envelopea, temp, 4);
  inc(temp, 4);
  copymemory(@self.envelopeb, temp, 4);
  inc(temp, 4);
  copymemory(@self.envelopec, temp, 4);
  inc(temp, 4);
  copymemory(@self.outputa, temp, 4);
  inc(temp, 4);
  copymemory(@self.outputb, temp, 4);
  inc(temp, 4);
  copymemory(@self.outputc, temp, 4);
  inc(temp, 4);
  copymemory(@self.outputn, temp, 4);
  inc(temp, 4);
  self.latch := temp^;
  inc(temp);
  self.type_ := temp^;
  inc(temp);
  copymemory(@self.countenv, temp, sizeof(shortint));
  inc(temp, sizeof(shortint));
  copymemory(@self.hold, temp, 4);
  inc(temp, 4);
  copymemory(@self.alternate, temp, 4);
  inc(temp, 4);
  copymemory(@self.attack, temp, 4);
  inc(temp, 4);
  copymemory(@self.holding, temp, 4);
  inc(temp, 4);
  copymemory(@self.rng, temp, 4);
  inc(temp, 4);
  copymemory(@self.updatestep, temp, 4);
  inc(temp, 4);
  copymemory(@self.lastenable, temp, sizeof(smallint));
end;

procedure ay8910_chip.change_io_calls(porta_read, portb_read: cpu_inport_call; porta_write, portb_write: cpu_outport_call);
begin
  self.porta_read := porta_read;
  if self.type_ = AY8912 then
    self.portb_read := porta_read
  else
    self.portb_read := portb_read;
  self.porta_write := porta_write;
  if self.type_ = AY8912 then
    self.portb_write := porta_write
  else
    self.portb_write := portb_write;
end;

procedure ay8910_chip.aywritereg(r, v: byte);
var
  old: integer;
begin
  self.regs[r] := v;
  case r of
    AY_AFINE, AY_ACOARSE:
      begin
        self.regs[AY_ACOARSE] := self.regs[AY_ACOARSE] and $F;
        old := self.perioda;
        self.perioda := cardinal((self.regs[AY_AFINE] + (256 * self.regs[AY_ACOARSE])) * self.updatestep);
        if (self.perioda = 0) then
          self.perioda := self.updatestep;
        self.counta := self.counta + (self.perioda - old);
        if (self.counta <= 0) then
          self.counta := 1;
      end;
    AY_BFINE, AY_BCOARSE:
      begin
        self.regs[AY_BCOARSE] := self.regs[AY_BCOARSE] and $F;
        old := self.periodb;
        self.periodb := (self.regs[AY_BFINE] + (256 * self.regs[AY_BCOARSE])) * self.updatestep;
        if (self.periodb = 0) then
          self.periodb := self.updatestep;
        self.countb := self.countb + self.periodb - old;
        if (self.countb <= 0) then
          self.countb := 1;
      end;
    AY_CFINE, AY_CCOARSE:
      begin
        self.regs[AY_CCOARSE] := self.regs[AY_CCOARSE] and $F;
        old := self.periodc;
        self.periodc := (self.regs[AY_CFINE] + (256 * self.regs[AY_CCOARSE])) * self.updatestep;
        if (self.periodc = 0) then
          self.periodc := self.updatestep;
        self.countc := self.countc + (self.periodc - old);
        if (self.countc <= 0) then
          self.countc := 1;
      end;
    AY_NOISEPER:
      begin
        self.regs[AY_NOISEPER] := self.regs[AY_NOISEPER] and $1F;
        old := self.periodn;
        self.periodn := self.regs[AY_NOISEPER] * self.updatestep;
        if (self.periodn = 0) then
          self.periodn := self.updatestep;
        self.countn := self.countn + (self.periodn - old);
        if (self.countn <= 0) then
          self.countn := 1;
      end;
    AY_ENABLE:
      begin
        if ((self.lastenable = -1) or ((self.lastenable and $40) <> (self.regs[AY_ENABLE] and $40))) then
        begin
          // write out 0xff if port set to input */
          if (@self.porta_write <> nil) then
          begin
            if (self.regs[AY_ENABLE] and $40) <> 0 then
              self.porta_write(self.regs[AY_PORTA])
            else
              self.porta_write($FF);
          end;
        end;
        if ((self.lastenable = -1) or ((self.lastenable and $80) <> (self.regs[AY_ENABLE] and $80))) then
        begin
          // write out 0xff if port set to input */
          if (@self.portb_write <> nil) then
          begin
            if (self.regs[AY_ENABLE] and $80) <> 0 then
              self.portb_write(self.regs[AY_PORTB])
            else
              self.portb_write($FF);
          end;
        end;
        self.lastenable := self.regs[AY_ENABLE];
      end;
    AY_AVOL:
      begin
        self.regs[AY_AVOL] := self.regs[AY_AVOL] and $1F;
        self.envelopea := self.regs[AY_AVOL] and $10;
        if self.regs[AY_AVOL] <> 0 then
          old := self.regs[AY_AVOL] * 2 + 1
        else
          old := 0;
        if self.envelopea <> 0 then
          self.vola := self.vole
        else
          self.vola := trunc(vol_table[old]);
      end;
    AY_BVOL:
      begin
        self.regs[AY_BVOL] := self.regs[AY_BVOL] and $1F;
        self.envelopeb := self.regs[AY_BVOL] and $10;
        if self.regs[AY_BVOL] <> 0 then
          old := self.regs[AY_BVOL] * 2 + 1
        else
          old := 0;
        if self.envelopeb <> 0 then
          self.volb := self.vole
        else
          self.volb := trunc(vol_table[old]);
      end;
    AY_CVOL:
      begin
        self.regs[AY_CVOL] := self.regs[AY_CVOL] and $1F;
        self.envelopec := self.regs[AY_CVOL] and $10;
        if self.regs[AY_CVOL] <> 0 then
          old := self.regs[AY_CVOL] * 2 + 1
        else
          old := 0;
        if self.envelopec <> 0 then
          self.volc := self.vole
        else
          self.volc := trunc(vol_table[old]);
      end;
    AY_EFINE, AY_ECOARSE:
      begin
        old := self.periode;
        self.periode := (self.regs[AY_EFINE] + (256 * self.regs[AY_ECOARSE])) * self.updatestep;
        if self.periode = 0 then
          self.periode := self.updatestep div 2;
        self.counte := self.counte + (self.periode - old);
        if self.counte <= 0 then
          self.counte := 1;
      end;
    AY_ESHAPE:
      begin
        self.regs[AY_ESHAPE] := self.regs[AY_ESHAPE] and $F;
        if ((self.regs[AY_ESHAPE] and 4) <> 0) then
          self.attack := $1F
        else
          self.attack := 0;
        if ((self.regs[AY_ESHAPE] and 8) = 0) then
        begin
          self.hold := 1;
          self.alternate := self.attack;
        end
        else
        begin
          self.hold := self.regs[AY_ESHAPE] and 1;
          self.alternate := self.regs[AY_ESHAPE] and 2;
        end;
        self.counte := self.periode;
        self.countenv := $1F;
        self.holding := 0;
        self.vole := trunc(vol_table[self.countenv xor self.attack]);
        if (self.envelopea <> 0) then
          self.vola := self.vole;
        if (self.envelopeb <> 0) then
          self.volb := self.vole;
        if (self.envelopec <> 0) then
          self.volc := self.vole;
      end;
    AY_PORTA:
      if @self.porta_write <> nil then
        self.porta_write(v)
      else
        self.regs[AY_PORTA] := v;
    AY_PORTB:
      if @self.portb_write <> nil then
        self.portb_write(v)
      else
        self.regs[AY_PORTB] := v;
  end; // case
end;

function ay8910_chip.ayreadreg(r: byte): byte;
begin
  case r of
    AY_PORTA:
      if (@self.porta_read <> nil) then
        self.regs[AY_PORTA] := self.porta_read;
    AY_PORTB:
      if (@self.portb_read <> nil) then
        self.regs[AY_PORTB] := self.portb_read;
  end;
  ayreadreg := self.regs[r];
end;

function ay8910_chip.read: byte;
begin
  read := self.ayreadreg(self.latch);
end;

function ay8910_chip.get_reg(reg: byte): byte;
begin
  get_reg := self.regs[reg];
end;

procedure ay8910_chip.set_reg(reg, valor: byte);
begin
  self.regs[reg] := valor;
end;

procedure ay8910_chip.write(v: byte);
begin
  self.aywritereg(self.latch, v);
end;

procedure ay8910_chip.control(v: byte);
begin
  self.latch := v and $F;
end;

function ay8910_chip.get_control: byte;
begin
  get_control := self.latch;
end;

function ay8910_chip.update_internal: pinteger;
var
  AY_OutNoise: integer;
  vola, volb, volc: integer;
  AY_Left: integer;
  AY_NextEvent: integer;
begin
  if (self.regs[AY_ENABLE] and 1) <> 0 then
  begin
    if self.counta <= STEP then
      self.counta := self.counta + STEP;
    self.outputa := 1;
  end
  else if (self.regs[AY_AVOL] = 0) then
  begin
    if self.counta <= STEP then
      self.counta := self.counta + STEP;
  end;
  if (self.regs[AY_ENABLE] and 2) <> 0 then
  begin
    if self.countb <= STEP then
      self.countb := self.countb + STEP;
    self.outputb := 1;
  end
  else if self.regs[AY_BVOL] = 0 then
  begin
    if self.countb <= STEP then
      self.countb := self.countb + STEP;
  end;
  if (self.regs[AY_ENABLE] and 4) <> 0 then
  begin
    if self.countc <= STEP then
      self.countc := self.countc + STEP;
    self.outputc := 1;
  end
  else if (self.regs[AY_CVOL] = 0) then
  begin
    if self.countc <= STEP then
      self.countc := self.countc + STEP;
  end;
  if ((self.regs[AY_ENABLE] and $38) = $38) then
    if (self.countn <= STEP) then
      self.countn := self.countn + STEP;
  AY_OutNoise := (self.outputn or self.regs[AY_ENABLE]);
  vola := 0;
  volb := 0;
  volc := 0;
  AY_Left := STEP;
  repeat
    if (self.countn < AY_Left) then
      AY_NextEvent := self.countn
    else
      AY_NextEvent := AY_Left;
    if (AY_OutNoise And 8) <> 0 then
    begin
      if self.outputa <> 0 then
        vola := vola + self.counta;
      self.counta := self.counta - AY_NextEvent;
      while (self.counta <= 0) do
      begin
        self.counta := self.counta + self.perioda;
        if (self.counta > 0) then
        begin
          self.outputa := self.outputa xor 1;
          if (self.outputa <> 0) then
            vola := vola + self.perioda;
          break;
        end;
        self.counta := self.counta + self.perioda;
        vola := vola + self.perioda;
      end;
      if (self.outputa <> 0) Then
        vola := vola - self.counta;
    end
    else
    begin
      self.counta := self.counta - AY_NextEvent;
      while (self.counta <= 0) do
      begin
        self.counta := self.counta + self.perioda;
        if (self.counta > 0) then
        begin
          self.outputa := self.outputa xor 1;
          break;
        end;
        self.counta := self.counta + self.perioda;
      end;
    end;
    if (AY_OutNoise And $10) <> 0 then
    begin
      if self.outputb <> 0 then
        volb := volb + self.countb;
      self.countb := self.countb - AY_NextEvent;
      while (self.countb <= 0) do
      begin
        self.countb := self.countb + self.periodb;
        if (self.countb > 0) then
        begin
          self.outputb := self.outputb xor 1;
          if (self.outputb <> 0) then
            volb := volb + self.periodb;
          break;
        end;
        self.countb := self.countb + self.periodb;
        volb := volb + self.periodb;
      end;
      if (self.outputb <> 0) then
        volb := volb - self.countb;
    end
    else
    begin
      self.countb := self.countb - AY_NextEvent;
      while (self.countb <= 0) do
      begin
        self.countb := self.countb + self.periodb;
        if (self.countb > 0) then
        begin
          self.outputb := self.outputb xor 1;
          break;
        end;
        self.countb := self.countb + self.periodb;
      end;
    end;
    if (AY_OutNoise And $20) <> 0 then
    begin
      if (self.outputc <> 0) then
        volc := volc + self.countc;
      self.countc := self.countc - AY_NextEvent;
      while (self.countc <= 0) do
      begin
        self.countc := self.countc + self.periodc;
        if (self.countc > 0) then
        begin
          self.outputc := self.outputc xor 1;
          If (self.outputc <> 0) then
            volc := volc + self.periodc;
          break;
        end;
        self.countc := self.countc + self.periodc;
        volc := volc + self.periodc;
      end;
      If (self.outputc <> 0) Then
        volc := volc - self.countc;
    end
    else
    begin
      self.countc := self.countc - AY_NextEvent;
      while (self.countc <= 0) do
      begin
        self.countc := self.countc + self.periodc;
        if (self.countc > 0) then
        begin
          self.outputc := self.outputc xor 1;
          break;
        end;
        self.countc := self.countc + self.periodc;
      end;
    end;
    self.countn := self.countn - AY_NextEvent;
    if (self.countn <= 0) then
    begin
      if ((self.rng + 1) and 2) <> 0 then
      begin // * (bit0^bit1)? */
        self.outputn := not(self.outputn);
        AY_OutNoise := (self.outputn or self.regs[AY_ENABLE]);
      end;
      if (self.rng and 1) <> 0 then
        self.rng := self.rng xor $28000; // * This version is called the "Galois configuration". */
      self.rng := self.rng shr 1;
      self.countn := self.countn + self.periodn;
    end;
    AY_Left := AY_Left - AY_NextEvent;
  until (AY_Left <= 0);
  if (self.holding = 0) then
  begin
    self.counte := self.counte - STEP;
    If (self.counte <= 0) then
    begin
      repeat
        self.countenv := self.countenv - 1;
        self.counte := self.counte + self.periode;
      until (self.counte > 0);
      if (self.countenv < 0) then
      begin
        if (self.hold <> 0) then
        begin
          if (self.alternate <> 0) then
            self.attack := self.attack xor $1F;
          self.holding := 1;
          self.countenv := 0;
        end
        else
        begin
          If (self.alternate <> 0) and ((self.countenv and $20) <> 0) then
            self.attack := self.attack xor $1F;
          self.countenv := self.countenv and $1F;
        end;
      end;
      self.vole := trunc(vol_table[self.countenv xor self.attack]);
      if (self.envelopea <> 0) then
        self.vola := self.vole;
      if (self.envelopeb <> 0) then
        self.volb := self.vole;
      if (self.envelopec <> 0) then
        self.volc := self.vole;
    end;
  end;
  salida_ay[0] := trunc(((((vola * self.vola) / STEP) * self.gain0) + (((volb * self.volb) / STEP) * self.gain1) + (((volc * self.volc) / STEP) * self.gain2)) * self.amp);
  salida_ay[1] := trunc(((vola * self.vola) / STEP) * self.gain0 * self.amp);
  salida_ay[2] := trunc(((volb * self.volb) / STEP) * self.gain1 * self.amp);
  salida_ay[3] := trunc(((volc * self.volc) / STEP) * self.gain2 * self.amp);
  if salida_ay[1] > $7FFF then
    salida_ay[1] := $7FFF
  else if salida_ay[1] < -$7FFF then
    salida_ay[1] := -$7FFF;
  if salida_ay[2] > $7FFF then
    salida_ay[2] := $7FFF
  else if salida_ay[2] < -$7FFF then
    salida_ay[2] := -$7FFF;
  if salida_ay[3] > $7FFF then
    salida_ay[3] := $7FFF
  else if salida_ay[3] < -$7FFF then
    salida_ay[3] := -$7FFF;
  if salida_ay[0] > $7FFF then
    salida_ay[0] := $7FFF
  else if salida_ay[0] < -$7FFF then
    salida_ay[0] := -$7FFF;
  update_internal := @salida_ay[0];
end;

procedure ay8910_chip.update;
begin
  self.update_internal;
  tsample[self.tsample_num, sound_status.sound_position] := salida_ay[0];
  if sound_status.stereo then
    tsample[self.tsample_num, sound_status.sound_position + 1] := salida_ay[0];
end;

end.
