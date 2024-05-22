unit mb88xx;

interface

uses
  WinApi.Windows,
  main_engine,
  FMX.Dialogs,
  System.SysUtils,
  timer_engine,
  cpu_misc;

type
  type_mb88xx_inport_r = function(port: byte): byte;
  type_mb88xx_outport_r = procedure(port, valor: byte);

  reg_mb88xx = record
    pc: byte; // Program Counter: 6 bits */
    pa: byte; // Page Address: 4 bits */
    sp: array [0 .. 4 - 1] of word;
    // Stack is 4*10 bit addresses deep, but we also use 3 top bits per address to store flags during irq */
    si: byte; // Stack index: 2 bits */
    a: byte; // Accumulator: 4 bits */
    x: byte; // Index X: 4 bits */
    y: byte; // Index Y: 4 bits */
    st: byte; // State flag: 1 bit */
    zf: byte; // Zero flag: 1 bit */
    cf: byte; // Carry flag: 1 bit */
    vf: byte; // Timer overflow flag: 1 bit */
    sf: byte; // Serial Full/Empty flag: 1 bit */
    nf: byte; // Interrupt flag: 1 bit */
  end;

  preg_mb88xx = ^reg_mb88xx;

  cpu_mb88xx = class(cpu_class)
    constructor Create(clock: dword; frames_div: word);
    destructor free;
  public
    port_k, port_p_r: cpu_inport_call;
    port_o, port_p_w: cpu_outport_call;
    port_r_r: type_mb88xx_inport_r;
    port_r_w: type_mb88xx_outport_r;
    procedure reset;
    procedure run(maximo: single);
    procedure set_irq_line(state: byte);
    procedure change_io_calls(port_k: cpu_inport_call; port_o: cpu_outport_call;
      port_p_r: cpu_inport_call; port_p_w: cpu_outport_call; port_r_r: type_mb88xx_inport_r;
      port_r_w: type_mb88xx_outport_r);
    function get_rom_addr: pbyte;
  private
    r: preg_mb88xx;
    // Peripheral Control */
    pio: byte; // Peripheral enable bits: 8 bits */
    // Timer registers */
    th: byte; // Timer High: 4 bits */
    tl: byte; // Timer Low: 4 bits */
    tp: byte; // Timer Prescale: 6 bits? */
    ctr: byte; // current external counter value */
    // Serial registers */
    sb: byte; // Serial buffer: 4 bits */
    sbcount: word; // number of bits received */
    // PLA configuration */
    PLA_m: pbyte;
    // IRQ handling */
    pending_interrupt: byte;
    // cpu_irq_callback irqcallback;
    rom: array [0 .. $7FF] of byte;
    ram: array [0 .. $7F] of byte;
    // Functions
    procedure update_pio_enable(newpio: byte);
    procedure increment_timer;
    function update_pio(cycles: byte): byte;
  end;

var
  mb88xx_0: cpu_mb88xx;

implementation

const
  INT_CAUSE_SERIAL = 01;
  INT_CAUSE_TIMER = 02;
  INT_CAUSE_EXTERNAL = 04;
  TIMER_PRESCALE = 32;
  MB88_PORTK = 0; // input only, 4 bits */
  MB88_PORTO = 1; // output only, PLA function output */
  MB88_PORTP = 2; // 4 bits */
  MB88_PORTR0 = 3; // R0-R3, 4 bits */
  MB88_PORTR1 = 4; // R4-R7, 4 bits */
  MB88_PORTR2 = 5; // R8-R11, 4 bits */
  MB88_PORTR3 = 6; // R12-R15, 4 bits */
  MB88_PORTSI = 7; // SI, 1 bit */

constructor cpu_mb88xx.Create(clock: dword; frames_div: word);
begin
  getmem(self.r, sizeof(reg_mb88xx));
  fillchar(self.r^, sizeof(reg_mb88xx), 0);
  self.numero_cpu := cpu_main_init(clock div 6);
  self.clock := clock div 6;
  self.tframes := (clock / 6 / frames_div) / machine_calls.fps_max;
  self.port_k := nil;
  self.port_p_r := nil;
  self.port_o := nil;
  self.port_p_w := nil;
  self.port_r_r := nil;
  self.port_r_w := nil;
end;

destructor cpu_mb88xx.free;
begin
  freemem(self.r);
end;

function cpu_mb88xx.get_rom_addr: pbyte;
begin
  get_rom_addr := @self.rom[0];
end;

procedure cpu_mb88xx.change_io_calls(port_k: cpu_inport_call; port_o: cpu_outport_call;
  port_p_r: cpu_inport_call; port_p_w: cpu_outport_call; port_r_r: type_mb88xx_inport_r;
  port_r_w: type_mb88xx_outport_r);
begin
  self.port_k := port_k;
  self.port_o := port_o;
  self.port_p_r := port_p_r;
  self.port_p_w := port_p_w;
  self.port_r_r := port_r_r;
  self.port_r_w := port_r_w;
end;

procedure cpu_mb88xx.set_irq_line(state: byte);
begin
  // on falling edge trigger interrupt */
  if (((self.pio and $04) <> 0) and (r.nf = 0) and (state <> CLEAR_LINE)) then
  begin
    self.pending_interrupt := self.pending_interrupt or INT_CAUSE_EXTERNAL;
  end;
  if state <> CLEAR_LINE then
    r.nf := 1
  else
    r.nf := 0;
end;

procedure cpu_mb88xx.reset;
begin
  r.pc := 0;
  r.pa := 0;
  r.sp[0] := 0;
  r.sp[1] := 0;
  r.sp[2] := 0;
  r.sp[3] := 0;
  r.si := 0;
  r.a := 0;
  r.x := 0;
  r.y := 0;
  r.st := 1; // start off with st=1 */
  r.zf := 0;
  r.cf := 0;
  r.vf := 0;
  r.sf := 0;
  r.nf := 0;
  self.pio := 0;
  self.th := 0;
  self.tl := 0;
  self.tp := 0;
  self.sb := 0;
  self.sbcount := 0;
  self.pending_interrupt := 0;
  self.change_reset(CLEAR_LINE);
end;

procedure inc_pc(r: preg_mb88xx);
begin
  r.pc := r.pc + 1;
  if r.pc = $40 then
  begin
    r.pc := 0;
    r.pa := r.pa + 1;
  end;
end;

function get_pc(r: preg_mb88xx): word;
begin
  get_pc := (r.pa shl 6) + r.pc;
end;

function get_ea(r: preg_mb88xx): word;
begin
  get_ea := (r.x shl 4) + r.y;
end;

function pla(r: preg_mb88xx; inA, inB: byte): byte;
var
  index: byte;
begin
  index := ((inB and 1) shl 4) or (inA and $0F);
  // if @r.PLA<>nil then return cpustate->PLA[index];
  pla := index;
end;

procedure cpu_mb88xx.update_pio_enable(newpio: byte);
begin
  // if the serial state has changed, configure the timer */
  if ((self.pio xor newpio) and $30) <> 0 then
  begin
    if ((newpio and $30) = 0) then // cpustate->serial->adjust(attotime::never);
    else if ((newpio and $30) = $20) then
      // pustate->serial->adjust(attotime::from_hz(cpustate->device->clock() / SERIAL_PRESCALE), 0, attotime::from_hz(cpustate->device->clock() / SERIAL_PRESCALE));
    else; // fatalerror("mb88xx: update_pio_enable set serial enable to unsupported value %02X\n", newpio & 0x30);
  end;
  self.pio := newpio;
end;

function update_st_c(v: byte): byte;
begin
  if (v and $10) <> 0 then
    update_st_c := 0
  else
    update_st_c := 1;
end;

function update_st_z(v: byte): byte;
begin
  if (v = 0) then
    update_st_z := 0
  else
    update_st_z := 1;
end;

function update_cf(v: byte): byte;
begin
  if (v and $10) = 0 then
    update_cf := 0
  else
    update_cf := 1;
end;

function update_zf(v: byte): byte;
begin
  if (v <> 0) then
    update_zf := 0
  else
    update_zf := 1;
end;

procedure cpu_mb88xx.increment_timer;
begin
  self.tl := (self.tl + 1) and $0F;
  if (self.tl = 0) then
  begin
    self.th := (self.th + 1) and $0F;
    if (self.th = 0) then
    begin
      r.vf := 1;
      self.pending_interrupt := self.pending_interrupt or INT_CAUSE_TIMER;
    end;
  end;
end;

function cpu_mb88xx.update_pio(cycles: byte): byte;
var
  estados: byte;
begin
  estados := 0;
  // internal clock enable */
  if (self.pio and $80) <> 0 then
  begin
    self.tp := self.tp + cycles;
    while (self.tp >= TIMER_PRESCALE) do
    begin
      self.tp := self.tp - TIMER_PRESCALE;
      self.increment_timer;
    end;
  end;
  // process pending interrupts */
  if (self.pending_interrupt and self.pio) <> 0 then
  begin
    r.sp[r.si] := get_pc(r);
    r.sp[r.si] := r.sp[r.si] or (r.cf shl 15);
    r.sp[r.si] := r.sp[r.si] or (r.zf shl 14);
    r.sp[r.si] := r.sp[r.si] or (r.st shl 13);
    r.si := (r.si + 1) and 3;
    { /* the datasheet doesn't mention interrupt vectors but
      the Arabian MCU program expects the following }
    if (self.pending_interrupt and self.pio and INT_CAUSE_EXTERNAL) <> 0 then
    begin
      // if we have a live external source, call the irqcallback
      // (*cpustate->irqcallback)(cpustate->device, 0);
      r.pc := $02;
    end
    else if (self.pending_interrupt and self.pio and INT_CAUSE_TIMER) <> 0 then
    begin
      r.pc := $04;
    end
    else if (self.pending_interrupt and self.pio and INT_CAUSE_SERIAL) <> 0 then
    begin
      r.pc := $06;
    end;
    r.pa := $00;
    r.st := 1;
    self.pending_interrupt := 0;
    estados := 3;
  end;
  update_pio := estados;
end;

procedure cpu_mb88xx.run(maximo: single);
var
  instruccion, timming, arg, tempb: byte;
begin
  if self.pedir_reset <> CLEAR_LINE then
  begin
    tempb := self.pedir_reset;
    self.reset;
    if tempb = ASSERT_LINE then
    begin
      self.pedir_reset := ASSERT_LINE;
      self.contador := trunc(maximo);
      exit;
    end;
  end;
  self.contador := 0;
  while self.contador < maximo do
  begin
    timming := 1;
    instruccion := self.rom[get_pc(r)];
    inc_pc(r);
    // Comprobar irq
    case instruccion of
      $00:
        r.st := 1; // nop
      $01:
        begin // outO
          if @self.port_o <> nil then
            self.port_o(pla(r, r.a, r.cf));
          r.st := 1;
        end;
      $02:
        begin // outP
          if @self.port_p_w <> nil then
            self.port_p_w(r.a);
          r.st := 1;
        end;
      $03:
        begin // outR
          arg := r.y;
          if @self.port_r_w <> nil then
            self.port_r_w(arg and 3, r.a);
          r.st := 1;
        end;
      $04:
        begin // tay
          r.y := r.a;
          r.st := 1;
        end;
      $05:
        begin // tath
          self.th := r.a;
          r.st := 1;
        end;
      $06:
        begin // tatl
          self.tl := r.a;
          r.st := 1;
        end;
      $07:
        begin // tas
          self.sb := r.a;
          r.st := 1;
        end;
      $08:
        begin // icy
          r.y := r.y + 1;
          r.st := update_st_c(r.y);
          r.y := r.y and $0F;
          r.zf := update_zf(r.y);
        end;
      $09:
        begin // icm
          arg := self.ram[get_ea(r)];
          arg := arg + 1;
          r.st := update_st_c(arg);
          arg := arg and $0F;
          r.zf := update_zf(arg);
          self.ram[get_ea(r)] := arg;
        end;
      $0A:
        begin // stic
          self.ram[get_ea(r)] := r.a;
          r.y := r.y + 1;
          r.st := update_st_c(r.y);
          r.y := r.y and $0F;
          r.zf := update_zf(r.y);
        end;
      $0B:
        begin // x
          arg := self.ram[get_ea(r)];
          self.ram[get_ea(r)] := r.a;
          r.a := arg;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $0C:
        begin // rol
          r.a := r.a shl 1;
          r.a := r.a or r.cf;
          r.st := update_st_c(r.a);
          r.cf := r.st xor 1;
          r.a := r.a and $F;
          r.zf := update_zf(r.a);
        end;
      $0D:
        begin // l
          r.a := self.ram[get_ea(r)];
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $0E:
        begin // adc
          arg := self.ram[get_ea(r)];
          arg := arg + r.a + r.cf;
          r.st := update_st_c(arg);
          r.cf := r.st xor 1;
          r.a := arg and $0F;
          r.zf := update_zf(r.a);
        end;
      $0F:
        begin // and ZCS:x.x
          r.a := r.a and self.ram[get_ea(r)];
          r.zf := update_zf(r.a);
          r.st := r.zf xor 1;
        end;
      $10:
        begin // daa ZCS:.xx
          if ((r.cf <> 0) or (r.a > 9)) then
            r.a := r.a + 6;
          r.st := update_st_c(r.a);
          r.cf := r.st xor 1;
          r.a := r.a and $0F;
        end;
      $12:
        begin // inK
          if @self.port_k <> nil then
            r.a := self.port_k and $F;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $13:
        begin // inR
          if @self.port_r_r <> nil then
            r.a := self.port_r_r(r.y and 3) and $F;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $14:
        begin // tya
          r.a := r.y;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $15:
        begin // ttha
          r.a := self.th;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $16:
        begin // ttla
          r.a := self.tl;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $17:
        begin // tsa
          r.a := self.sb;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $18:
        begin // dcy
          r.y := r.y - 1;
          r.st := update_st_c(r.y);
          r.y := r.y and $F;
        end;
      $19:
        begin // dcm
          arg := self.ram[get_ea(r)];
          arg := arg - 1;
          r.st := update_st_c(arg);
          arg := arg and $F;
          r.zf := update_zf(arg);
          self.ram[get_ea(r)] := arg;
        end;
      $1A:
        begin // stdc ZCS:x.x
          self.ram[get_ea(r)] := r.a;
          r.y := r.y - 1;
          r.st := update_st_c(r.y);
          r.y := r.y and $F;
          r.zf := update_zf(r.y);
        end;
      $1B:
        begin // xx
          arg := r.x;
          r.x := r.a;
          r.a := arg;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $1C:
        begin // ror
          r.a := r.a or (r.cf shl 4);
          r.st := update_st_c(r.a shl 4);
          r.cf := r.st xor 1;
          r.a := (r.a shr 1) and $F;
          r.zf := update_zf(r.a);
        end;
      $1D:
        begin // st
          self.ram[get_ea(r)] := r.a;
          r.st := 1;
        end;
      $1E:
        begin // sbc
          arg := self.ram[get_ea(r)];
          arg := arg - r.a - r.cf;
          r.st := update_st_c(arg);
          r.cf := r.st xor 1;
          r.a := arg and $0F;
          r.zf := update_zf(r.a);
        end;
      $1F:
        begin // or
          r.a := r.a or self.ram[get_ea(r)];
          r.zf := update_zf(r.a);
          r.st := r.zf xor 1;
        end;
      $20:
        begin // setR ZCS:...
          if @self.port_r_r <> nil then
            arg := self.port_r_r(r.y div 4);
          if @self.port_r_w <> nil then
            self.port_r_w(r.y div 4, arg or not(1 shl (r.y mod 4)));
          r.st := 1;
        end;
      $21:
        begin // setc
          r.cf := 1;
          r.st := 1;
        end;
      $22:
        begin // rstR ZCS:...
          if @self.port_r_r <> nil then
            arg := self.port_r_r(r.y div 4);
          if @self.port_r_w <> nil then
            self.port_r_w(r.y div 4, arg and not(1 shl (r.y mod 4)));
          r.st := 1;
        end;
      $23:
        begin // rstc
          r.cf := 0;
          r.st := 1;
        end;
      $24:
        begin // tstr
          if @self.port_r_r <> nil then
            arg := self.port_r_r(r.y div 4) and $F;
          if (arg and (1 shl (r.y mod 4))) <> 0 then
            r.st := 0
          else
            r.st := 1;
        end;
      $25:
        r.st := r.nf xor 1; // tsti
      $28:
        r.st := r.cf xor 1; // tstc
      $2C:
        begin // rts
          r.si := (r.si - 1) and $3;
          r.pc := r.sp[r.si] and $3F;
          r.pa := (r.sp[r.si] shr 6) and $1F;
          r.st := 1;
        end;
      $2E:
        begin // c
          arg := self.ram[get_ea(r)];
          arg := arg - r.a;
          r.cf := update_cf(arg);
          arg := arg and $0F;
          r.st := update_st_z(arg);
          r.zf := r.st xor 1;
        end;
      $2F:
        begin // eor ZCS:x.x
          r.a := r.a xor self.ram[get_ea(r)];
          r.st := update_st_z(r.a);
          r.zf := r.st xor 1;
        end;
      $30 .. $33:
        begin // sbit
          arg := self.ram[get_ea(r)];
          self.ram[get_ea(r)] := arg or (1 shl (instruccion and 3));
          r.st := 1;
        end;
      $34 .. $37:
        begin // rbit
          arg := self.ram[get_ea(r)];
          self.ram[get_ea(r)] := arg and not(1 shl (instruccion and 3));
          r.st := 1;
        end;
      $38 .. $3B:
        begin // tbit
          arg := self.ram[get_ea(r)];
          if (arg and (1 shl (instruccion and $3))) <> 0 then
            r.st := 0
          else
            r.st := 1;
        end;
      $3C:
        begin // rti
          r.si := (r.si - 1) and 3;
          r.pc := r.sp[r.si] and $3F;
          r.pa := (r.sp[r.si] shr 6) and $1F;
          r.st := (r.sp[r.si] shr 13) and 1;
          r.zf := (r.sp[r.si] shr 14) and 1;
          r.cf := (r.sp[r.si] shr 15) and 1;
        end;
      $3D:
        begin // jpa imm
          r.pa := self.rom[get_pc(r)] and $1F;
          r.pc := r.a * 4;
          timming := 2;
          r.st := 1;
        end;
      $3E:
        begin // en imm
          self.update_pio_enable(self.pio or self.rom[get_pc(r)]);
          inc_pc(r);
          timming := 2;
          r.st := 1;
        end;
      $3F:
        begin // dis
          self.update_pio_enable(self.pio and not(self.rom[get_pc(r)]));
          inc_pc(r);
          timming := 2;
          r.st := 1;
        end;
      $40 .. $43:
        begin // setD
          if @self.port_r_r <> nil then
            arg := self.port_r_r(0) and $F;
          arg := arg or (1 shl (instruccion and 3));
          if @self.port_r_w <> nil then
            self.port_r_w(0, arg);
          r.st := 1;
        end;
      $44 .. $47:
        begin // rstD
          if @self.port_r_r <> nil then
            arg := self.port_r_r(0) and $F;
          arg := arg and (not(1 shl (instruccion and 3)));
          if @self.port_r_w <> nil then
            self.port_r_w(0, arg);
          r.st := 1;
        end;
      $48, $49, $4A, $4B:
        begin // tstD ZCS:..x
          if @self.port_r_r <> nil then
            arg := self.port_r_r(2);
          if (arg and (1 shl (instruccion and 3))) <> 0 then
            r.st := 0
          else
            r.st := 1;
        end;
      $4C .. $4F:
        if (r.a and (1 shl (instruccion and 3))) <> 0 then
          r.st := 0
        else
          r.st := 1; // tba
      $50 .. $53:
        begin // xd
          arg := self.ram[instruccion and $3];
          self.ram[instruccion and $3] := r.a;
          r.a := arg;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $54 .. $57:
        begin // xyd
          arg := self.ram[(instruccion and 3) + 4];
          self.ram[(instruccion and 3) + 4] := r.y;
          r.y := arg;
          r.zf := update_zf(r.y);
          r.st := 1;
        end;
      $58 .. $5F:
        begin // lxi
          r.x := instruccion and $7;
          r.zf := update_zf(r.x);
          r.st := 1;
        end;
      $60 .. $67:
        begin // call imm
          arg := self.rom[get_pc(r)];
          inc_pc(r);
          timming := 2;
          if r.st <> 0 then
          begin
            r.sp[r.si] := get_pc(r);
            r.si := (r.si + 1) and $3;
            r.pc := arg and $3F;
            r.pa := ((instruccion and 7) shl 2) or (arg shr 6);
          end;
          r.st := 1;
        end;
      $68 .. $6F:
        begin // jpl imm
          arg := self.rom[get_pc(r)];
          inc_pc(r);
          timming := 2;
          if r.st <> 0 then
          begin
            r.pc := arg and $3F;
            r.pa := ((instruccion and 7) shl 2) or (arg shr 6);
          end;
          r.st := 1;
        end;
      $70 .. $7F:
        begin // ai
          arg := instruccion and $0F;
          arg := arg + r.a;
          r.st := update_st_c(arg);
          r.cf := r.st xor 1;
          r.a := arg and $F;
          r.zf := update_zf(r.a);
        end;
      $80 .. $8F:
        begin // lyi
          r.y := instruccion and $0F;
          r.zf := update_zf(r.y);
          r.st := 1;
        end;
      $90 .. $9F:
        begin // li
          r.a := instruccion and $0F;
          r.zf := update_zf(r.a);
          r.st := 1;
        end;
      $A0 .. $AF:
        begin // cyi
          arg := (instruccion and $0F) - r.y;
          r.cf := update_cf(arg);
          arg := arg and $F;
          r.st := update_st_z(arg);
          r.zf := r.st xor 1;
        end;
      $B0 .. $BF:
        begin // ci
          arg := (instruccion and $0F) - r.a;
          r.cf := update_cf(arg);
          arg := arg and $0F;
          r.st := update_st_z(arg);
          r.zf := r.st xor 1;
        end;
      $C0 .. $FF:
        begin // jmp
          if r.st <> 0 then
            r.pc := instruccion and $3F;
          r.st := 1;
        end;
    else
      begin
//        MessageDlg('Instruccion MB88XX ' + inttohex(instruccion, 2) + ' desconocida. PC=' +
//          inttohex(get_pc(r) - 1, 10), mtInformation, [mbOk], 0);
      end;
    end;
    tempb := timming + self.update_pio(timming);
    self.contador := self.contador + tempb;
    timers.update(tempb, self.numero_cpu);
  end;
end;

end.
