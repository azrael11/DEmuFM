unit M6502;
// {$define DEBUG}

interface

uses
  WinApi.Windows,
  main_engine,
  FMX.Dialogs,
  System.SysUtils,
  timer_engine,
  cpu_misc;

type
  band_m6502 = record
    n, o_v, t, brk, dec, int, z, c: boolean;
  end;

  reg_m6502 = record
    ppc, pc: word;
    a, x, y: byte;
    sp: byte;
    p: band_m6502;
    // M65CE02
    z: byte;
    b: word;
  end;

  preg_m6502 = ^reg_m6502;

  cpu_m6502 = class(cpu_class)
    constructor create(clock: dword; frames_div: word; cpu_type: byte);
    destructor free;
  public
    after_ei: boolean;
    tipo_cpu: byte;
    procedure reset;
    procedure run(maximo: single);
    procedure change_io_calls(in_port0, in_port1: cpu_inport_call);
    function get_internal_r: preg_m6502;
    function save_snapshot(data: pbyte): word;
    procedure load_snapshot(data: pbyte);
  private
    // Internal Regs
    r: preg_m6502;
    // RAM calls/IO Calls
    in_port0, in_port1: cpu_inport_call;
    read_dummy: boolean;
    // IRQ
    function call_nmi: byte;
    function call_irq: byte;
  end;

const
  TCPU_M6502 = 0;
  TCPU_DECO16 = 1;
  TCPU_NES = 2;
  TCPU_M65C02 = 3;

var
  m6502_0, m6502_1: cpu_m6502;

implementation

const
  tipo_dir_m65c02: array [0 .. 255] of byte = (
    // 0  1  2  3 4  5 6 7 8  9 a b c  d e f
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 00
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 10
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 20
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 30
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 40
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 50
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 0, 2, 2, 2, // 60
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 70
    $D, $A, 1, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 80
    $D, $B, $E, $B, 8, 8, 9, 9, 0, 6, 0, 6, 2, 4, 5, 5, // 90
    1, $A, 1, 0, 7, 7, 7, 0, 0, 1, 0, 0, 2, 2, 2, 0, // A0
    $D, $B, $E, $B, 8, 8, 9, 0, 0, 6, 0, 0, 5, 5, 6, 0, // B0
    1, $A, 1, $A, 7, 7, 7, 0, 0, 1, 0, 0, 2, 2, 2, 2, // C0
    $D, $B, $E, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // D0
    1, $A, 1, $A, 7, 7, 7, 0, 0, 1, 0, 1, 2, 2, 2, 2, // E0
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5); // F0
  tipo_dir_m6502: array [0 .. 255] of byte = (
    // 0  1  2  3 4  5 6 7 8  9 a b c  d e f
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 00
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 10
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 20
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 30
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 40
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 50
    0, $A, 0, $A, 7, 7, 7, 7, 0, 1, 0, 1, 0, 2, 2, 2, // 60
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // 70
    $D, $A, 1, $A, 7, 7, 7, 7, 0, 1, 0, 1, 2, 2, 2, 2, // 80
    $D, $B, 0, $B, 8, 8, 9, 9, 0, 6, 0, 6, 2, 4, 5, 5, // 90
    1, $A, 1, 0, 7, 7, 7, 0, 0, 1, 0, 0, 2, 2, 2, 0, // A0
    $D, $B, 0, $B, 8, 8, 9, 0, 0, 6, 0, 0, 5, 5, 6, 0, // B0
    1, $A, 1, $A, 7, 7, 7, 0, 0, 1, 0, 0, 2, 2, 2, 2, // C0
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5, // D0
    1, $A, 1, $A, 7, 7, 7, 0, 0, 1, 0, 1, 2, 2, 2, 2, // E0
    $D, $B, 0, $B, 8, 8, 8, 8, 0, 6, 0, 6, 5, 5, 4, 5); // F0

  estados_t_m6502: array [0 .. 255] of byte = (
    // M6502 + NES
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  e
    7, 6, 1, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6, // 00
    2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 10
    6, 6, 1, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6, // 20
    2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 30
    6, 6, 1, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6, // 40
    2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 50
    6, 6, 1, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6, // 60
    2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 70
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, // 80
    2, 6, 1, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5, // 90
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, // a0
    2, 5, 1, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4, // b0
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, // c0
    2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // d0
    2, 6, 2, 7, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, // e0
    2, 5, 1, 7, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7); // f0
  // DECO16
  estados_t_deco16: array [0 .. 255] of byte = (7, 6, 2, 2, 2, 3, 5, 2, 3, 2, 2, 2, 4, 4, 6, 2, // 0
    2, 5, 2, 2, 2, 4, 6, 2, 2, 4, 2, 2, 4, 4, 7, 2, // 1
    6, 6, 2, 2, 3, 3, 5, 2, 4, 2, 2, 2, 4, 4, 6, 2, // 2
    2, 5, 2, 2, 2, 4, 6, 2, 2, 4, 2, 2, 4, 4, 7, 1, // 3
    6, 6, 2, 2, 2, 3, 5, 2, 3, 2, 2, 1, 3, 4, 6, 2, // 4
    2, 5, 2, 2, 2, 4, 6, 2, 2, 4, 3, 2, 2, 4, 7, 2, // 5
    6, 6, 2, 2, 2, 3, 5, 2, 4, 2, 2, 2, 5, 4, 6, 2, // 6
    2, 5, 2, 2, 2, 4, 6, 2, 2, 4, 4, 2, 6, 4, 7, 2, // 7
    2, 6, 2, 2, 3, 3, 3, 2, 2, 2, 2, 2, 4, 4, 4, 1, // 8
    2, 6, 2, 2, 4, 4, 4, 2, 2, 5, 2, 2, 4, 5, 5, 2, // 9
    2, 6, 2, 2, 3, 3, 3, 2, 2, 2, 2, 2, 4, 4, 4, 2, // a
    2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2, 1, 4, 4, 4, 2, // b
    2, 6, 2, 2, 3, 3, 5, 2, 2, 2, 2, 2, 4, 4, 6, 2, // c
    2, 5, 2, 2, 2, 4, 6, 2, 2, 4, 3, 2, 2, 4, 7, 2, // d
    2, 6, 2, 2, 3, 3, 5, 2, 2, 2, 2, 2, 4, 4, 6, 2, // e
    2, 5, 2, 2, 2, 4, 6, 2, 2, 4, 4, 2, 2, 4, 7, 2);
  // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f

var
  tipo_dir, estados_t: array [0 .. $FF] of byte;

constructor cpu_m6502.create(clock: dword; frames_div: word; cpu_type: byte);
begin
  getmem(self.r, sizeof(reg_m6502));
  fillchar(self.r^, sizeof(reg_m6502), 0);
  self.numero_cpu := cpu_main_init(clock);
  self.clock := clock;
  self.tipo_cpu := cpu_type;
  case cpu_type of
    TCPU_M6502, TCPU_NES:
      begin
        copymemory(@tipo_dir, @tipo_dir_m6502, $100);
        copymemory(@estados_t, @estados_t_m6502, $100);
      end;
    TCPU_DECO16:
      begin
        copymemory(@tipo_dir, @tipo_dir_m6502, $100);
        copymemory(@estados_t, @estados_t_deco16, $100);
      end;
    TCPU_M65C02:
      begin
        copymemory(@tipo_dir, @tipo_dir_m65c02, $100);
        copymemory(@estados_t, @estados_t_m6502, $100);
      end;
  end;
  self.tframes := (clock / frames_div) / machine_calls.fps_max;
  self.in_port0 := nil;
  self.in_port1 := nil;
end;

destructor cpu_m6502.free;
begin
  freemem(self.r);
end;

procedure cpu_m6502.change_io_calls(in_port0, in_port1: cpu_inport_call);
begin
  self.in_port0 := in_port0;
  self.in_port1 := in_port1;
end;

function cpu_m6502.get_internal_r: preg_m6502;
begin
  get_internal_r := self.r;
end;

function cpu_m6502.save_snapshot(data: pbyte): word;
var
  temp: pbyte;
  buffer: array [0 .. 2] of byte;
  size: word;
begin
  temp := data;
  copymemory(temp, self.r, sizeof(reg_m6502));
  inc(temp, sizeof(reg_m6502));
  size := sizeof(reg_m6502);
  buffer[0] := byte(self.after_ei);
  buffer[1] := self.tipo_cpu;
  buffer[2] := byte(self.read_dummy);
  copymemory(temp, @buffer[0], 3);
  save_snapshot := size + 3;
end;

procedure cpu_m6502.load_snapshot(data: pbyte);
var
  temp: pbyte;
begin
  temp := data;
  copymemory(self.r, temp, sizeof(reg_m6502));
  inc(temp, sizeof(reg_m6502));
  self.after_ei := temp^ <> 0;
  inc(temp);
  self.tipo_cpu := temp^;
  inc(temp);
  self.read_dummy := temp^ <> 0;
end;

procedure pon_pila(r: preg_m6502; valor: byte);
begin
  r.p.n := (valor and $80) <> 0;
  r.p.o_v := (valor and $40) <> 0;
  r.p.dec := (valor and 8) <> 0;
  r.p.int := (valor and 4) <> 0;
  r.p.z := (valor and 2) <> 0;
  r.p.c := (valor and 1) <> 0;
end;

function dame_pila(r: preg_m6502): byte;
var
  temp: byte;
begin
  temp := 0;
  if r.p.n then
    temp := temp or $80;
  if r.p.o_v then
    temp := temp or $40;
  if r.p.t then
    temp := temp or $20;
  if r.p.brk then
    temp := temp or $10;
  if r.p.dec then
    temp := temp or 8;
  if r.p.int then
    temp := temp or 4;
  if r.p.z then
    temp := temp or 2;
  if r.p.c then
    temp := temp or 1;
  dame_pila := temp;
end;

procedure cpu_m6502.reset;
begin
  case self.tipo_cpu of
    TCPU_M6502, TCPU_NES, TCPU_M65C02:
      r.pc := self.getbyte($FFFC) + (self.getbyte($FFFD) shl 8);
    TCPU_DECO16:
      r.pc := self.getbyte($FFF1) + (self.getbyte($FFF0) shl 8);
  end;
  r.a := 0;
  r.x := 0;
  r.y := 0;
  r.sp := $FD;
  r.z := 0;
  self.contador := 0;
  r.p.n := false;
  r.p.o_v := false;
  r.p.t := false;
  r.p.brk := false;
  r.p.dec := false;
  r.p.int := true;
  r.p.z := false;
  r.p.c := false;
  self.totalt := 0;
  self.after_ei := false;
  self.change_nmi(CLEAR_LINE);
  self.change_irq(CLEAR_LINE);
  self.change_reset(CLEAR_LINE);
end;

function cpu_m6502.call_nmi: byte;
begin
  call_nmi := 0;
  if self.nmi_state <> CLEAR_LINE then
    exit;
  self.putbyte($100 + r.sp, r.pc shr 8);
  r.sp := r.sp - 1;
  self.putbyte($100 + r.sp, r.pc and $FF);
  r.sp := r.sp - 1;
  self.putbyte($100 + r.sp, (dame_pila(self.r) and $DF));
  r.sp := r.sp - 1;
  r.p.int := true;
  case self.tipo_cpu of
    TCPU_M6502, TCPU_NES, TCPU_M65C02:
      r.pc := self.getbyte($FFFA) + (self.getbyte($FFFB) shl 8);
    TCPU_DECO16:
      r.pc := self.getbyte($FFF7) + (self.getbyte($FFF6) shl 8);
  end;
  call_nmi := 7;
  if (self.pedir_nmi = PULSE_LINE) then
    self.pedir_nmi := CLEAR_LINE;
  if (self.pedir_nmi = ASSERT_LINE) then
    self.nmi_state := ASSERT_LINE;
end;

function cpu_m6502.call_irq: byte;
begin
  if r.p.int then
  begin
    call_irq := 0;
    exit;
  end;
  self.putbyte($100 + r.sp, r.pc shr 8);
  r.sp := r.sp - 1;
  self.putbyte($100 + r.sp, r.pc and $FF);
  r.sp := r.sp - 1;
  self.putbyte($100 + r.sp, (dame_pila(self.r) and $DF));
  r.sp := r.sp - 1;
  r.p.int := true;
  case self.tipo_cpu of
    TCPU_M6502, TCPU_NES, TCPU_M65C02:
      r.pc := self.getbyte($FFFE) + (self.getbyte($FFFF) shl 8);
    TCPU_DECO16:
      r.pc := self.getbyte($FFF3) + (self.getbyte($FFF2) shl 8);
  end;
  call_irq := 7;
  if self.pedir_irq = HOLD_LINE then
    self.pedir_irq := CLEAR_LINE;
end;

procedure sbc(r: preg_m6502; numero, tipo_cpu: byte);
var
  carry, al, ah: byte;
  diff: word;
begin
  if (r.p.dec and (tipo_cpu <> TCPU_NES)) then
  begin
    carry := byte(not(r.p.c));
    diff := r.a - numero - carry;
    al := (r.a and 15) - (numero and 15) - carry;
    if (shortint(al) < 0) then
      al := al - 6;
    ah := (r.a shr 4) - (numero shr 4) - byte((shortint(al) < 0));
    r.p.z := (diff and $FF) = 0;
    if not(r.p.z) then
      r.p.n := (diff and $80) <> 0
    else
      r.p.n := false;
    r.p.o_v := ((r.a xor numero) and (r.a xor diff) and $80) <> 0;
    r.p.c := (diff and $FF00) = 0;
    if (shortint(ah) < 0) then
      ah := ah - 6;
    r.a := (ah shl 4) or (al and 15);
  end
  else
  begin
    if not(r.p.c) then
      diff := r.a - numero - 1
    else
      diff := r.a - numero;
    r.p.o_v := ((r.a xor numero) and (r.a xor diff) and $80) <> 0;
    r.p.c := (diff and $FF00) = 0;
    r.a := diff and $FF;
    r.p.z := (r.a = 0);
    if not(r.p.z) then
      r.p.n := (r.a and $80) <> 0
    else
      r.p.n := false;
  end;
end;

procedure adc(r: preg_m6502; numero, tipo_cpu: byte);
var
  al, ah, carry: byte;
  tempw: word;
begin
  if (r.p.dec and (tipo_cpu <> TCPU_NES)) then
  begin
    if r.p.c then
      carry := 1
    else
      carry := 0;
    al := (r.a and 15) + (numero and 15) + carry;
    if (al > 9) then
      al := al + 6;
    ah := (r.a shr 4) + (numero shr 4) + byte(al > 15);
    r.p.z := ((r.a + numero + carry) and $FF) = 0;
    if not(r.p.z) then
      r.p.n := (ah and 8) <> 0;
    r.p.o_v := (not(r.a xor numero) and (r.a xor (ah shl 4)) and $80) <> 0;
    if (ah > 9) then
      ah := ah + 6;
    r.p.c := (ah > 15);
    r.a := (ah shl 4) or (al and 15);
  end
  else
  begin
    if r.p.c then
      tempw := r.a + numero + 1
    else
      tempw := r.a + numero;
    r.p.o_v := (not(r.a xor numero) and (r.a xor tempw) and $80) <> 0;
    r.p.c := (tempw and $FF00) <> 0;
    r.a := tempw and $FF;
    r.p.z := (r.a = 0);
    if not(r.p.z) then
      r.p.n := (r.a and $80) <> 0
    else
      r.p.n := false;
  end;
end;

procedure cpu_m6502.run(maximo: single);
var
  instruccion, numero, tempb, carry: byte;
  tempw, posicion: word;
  old_contador: integer;
begin
  self.contador := 0;
  while self.contador < maximo do
  begin
    old_contador := self.contador;
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
    r.ppc := r.pc;
    self.read_dummy := false;
    if not(self.after_ei) then
    begin
      if (self.pedir_nmi <> CLEAR_LINE) then
        self.estados_demas := self.call_nmi
      else if (self.pedir_irq <> CLEAR_LINE) then
        self.estados_demas := self.call_irq
      else
        self.estados_demas := 0;
    end;
    self.after_ei := false;
    self.opcode := true;
    instruccion := self.getbyte(r.pc);
    self.opcode := false;
    r.pc := r.pc + 1;
    case tipo_dir[instruccion] of
      $00:
        ; // **implicito
      $01:
        begin // IMM
          posicion := r.pc;
          r.pc := r.pc + 1;
        end;
      $02:
        begin // absoluto opcode+direccion 16bits (rev)
          posicion := self.getbyte(r.pc);
          posicion := posicion or (self.getbyte(r.pc + 1) shl 8);
          r.pc := r.pc + 2;
        end;
      $03:
        begin // desconocido!!!! -> MAL
          // MessageDlg('Modo dir. mal, instruccion: ' + inttohex(instruccion, 2) + '. PC=' +
          // inttostr(r.pc), mtInformation, [mbOk], 0)
        end;
      $04:
        begin // absoluto indexado por X no page cross
          posicion := self.getbyte(r.pc);
          posicion := posicion + (self.getbyte(r.pc + 1) shl 8) + r.x;
          r.pc := r.pc + 2;
        end;
      $05:
        begin // absoluto indexado por X (rev)
          posicion := self.getbyte(r.pc) + (self.getbyte(r.pc + 1) shl 8);
          case instruccion of
            $1F, $3F, $5F, $7F:
              ;
          else
            if (((posicion + r.x) xor posicion) and $FF00) <> 0 then
              self.estados_demas := self.estados_demas + 1;
          end;
          posicion := posicion + r.x;
          r.pc := r.pc + 2;
        end;
      $06:
        begin // absoluto indexado por Y (rev)
          posicion := self.getbyte(r.pc) + (self.getbyte(r.pc + 1) shl 8);
          case instruccion of
            $1B, $3B, $5B, $7B, $99:
              ;
          else
            if (((posicion + r.y) xor posicion) and $FF00) <> 0 then
              self.estados_demas := self.estados_demas + 1;
          end;
          posicion := posicion + r.y;
          r.pc := r.pc + 2;
        end;
      $07:
        begin // pagina 0  opcode+direccion 8bits (rev)
          posicion := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $08:
        begin // pagina cero indexado por X (rev)
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion := (tempb + r.x) and $FF;
        end;
      $09:
        begin // pagina cero indexado por Y (rev)
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion := (tempb + r.y) and $FF;
        end;
      $0A:
        begin // indirecto indexado por X (rev)
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          tempb := tempb + r.x;
          posicion := self.getbyte(tempb) or (self.getbyte((tempb + 1) and $FF) shl 8);
        end;
      $0B:
        begin // indirecto indexado por Y (rev)
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion := self.getbyte(tempb);
          posicion := posicion or (self.getbyte((tempb + 1) and $FF) shl 8);
          case instruccion of
            $13, $33, $53, $73, $93, $91:
              ;
          else
            if (((posicion + r.y) xor posicion) and $FF00) <> 0 then
              self.estados_demas := self.estados_demas + 1;
          end;
          posicion := posicion + r.y;
        end;
      $0C:
        begin // indexado pagina 0 opcode+puntero 8bits (rev)
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion := self.getbyte(tempb);
          posicion := posicion + (self.getbyte((tempb + 1) and $FF) shl 8);
        end;
      $0D:
        begin // relativo
          numero := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $0E:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // Exclusivo del EC
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion := self.getbyte(self.r.b + tempb) or
            (self.getbyte((self.r.b + tempb + 1) and $FF) shl 8);
          posicion := posicion + self.r.z;
        end
{$IFDEF DEBUG}
        else
        begin
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
        end;
{$ENDIF}
    end; // del tipo de direccionamiento
    case instruccion of
      $00:
        begin // brk
          r.pc := r.pc + 1;
          self.putbyte($100 + r.sp, r.pc shr 8);
          r.sp := r.sp - 1;
          self.putbyte($100 + r.sp, r.pc and $FF);
          r.sp := r.sp - 1;
          self.putbyte($100 + r.sp, dame_pila(self.r) or $10);
          r.sp := r.sp - 1;
          r.p.int := true;
          r.p.brk := true;
          case self.tipo_cpu of
            TCPU_M6502, TCPU_NES, TCPU_M65C02:
              r.pc := self.getbyte($FFFE) + (self.getbyte($FFFF) shl 8);
            TCPU_DECO16:
              r.pc := self.getbyte($FFF3) + (self.getbyte($FFF2) shl 8);
          end;
        end;
      $01, $05, $09, $0D, $11, $15, $19, $1D:
        begin // ORA
          r.a := r.a or self.getbyte(posicion);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $02, $42:
        self.r.pc := self.r.pc - 1; // kil
      $03, $07, $0F, $13, $17, $1B, $1F:
        begin // SLO
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo de la CPU
          r.p.c := (tempb and $80) <> 0;
          tempb := tempb shl 1;
          r.a := r.a or tempb;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $04, $0C, $14, $1C, $3A, $3C, $44, $54, $5C, $7C, $82, $89, $C2, $D4, $DC, $E2, $F4, $FC:
        if self.tipo_cpu = TCPU_M65C02 then
        begin
{$IFDEF DEBUG}   // nop dobles y triples
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF};
        end
        else
          self.getbyte(r.pc); // <-- Fallo CPU NOP
      $EA:
        ; // nop
      $06, $0E, $16, $1E:
        begin // asl
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo de la CPU
          r.p.c := (tempb and $80) <> 0;
          tempb := tempb shl 1;
          r.p.z := (tempb = 0);
          r.p.n := (tempb and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $08:
        begin // PHP
          self.getbyte(r.pc); // <-- Fallo CPU
          tempb := dame_pila(self.r);
          tempb := tempb or $20 or $10;
          self.putbyte($100 + r.sp, tempb);
          r.sp := r.sp - 1;
        end;
      $0A:
        begin // asl A
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.c := (r.a and $80) <> 0;
          r.a := r.a shl 1;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $0B, $2B:
        begin // anc
          r.a := r.a and self.getbyte(posicion);
          if r.p.c then
            carry := 1
          else
            carry := 0;
          r.p.c := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or carry;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $10:
        if not(r.p.n) then
        begin // BPL salta si n false
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $1A:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // inc a
          self.r.a := self.r.a + 1;
          r.p.z := (self.r.a = 0);
          r.p.n := (self.r.a and $80) <> 0;
        end
        else
        begin
          self.getbyte(r.pc); // <-- Fallo CPU
          // {$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
          // {$ENDIF}
        end;
      $18:
        begin // CLC
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.c := false;
        end;
      $20:
        begin // JSR absoluto
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          self.putbyte($100 + r.sp, r.pc shr 8);
          r.sp := r.sp - 1;
          self.putbyte($100 + r.sp, r.pc and $FF);
          r.sp := r.sp - 1;
          r.pc := (self.getbyte(r.pc) shl 8) or tempb;
        end;
      $21, $25, $29, $2D, $31, $35, $39, $3D:
        begin // AND
          r.a := r.a and self.getbyte(posicion);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $23, $27, $2F, $33, $37, $3B, $3F:
        begin // RLA
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo de la CPU
          if r.p.c then
            tempw := (tempb shl 1) + 1
          else
            tempw := tempb shl 1;
          r.p.c := (tempw and $100) <> 0;
          r.a := r.a and (tempw and $FF);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
          self.putbyte(posicion, tempw and $FF);
        end;
      $24, $2C:
        begin // BIT
          numero := self.getbyte(posicion);
          r.p.z := (r.a and numero) = 0;
          r.p.n := (numero and $80) <> 0;
          r.p.o_v := (numero and $40) <> 0;
        end;
      $26, $2E, $36, $3E:
        begin // ROL
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo CPU
          if r.p.c then
            carry := 1
          else
            carry := 0;
          r.p.c := (tempb and $80) <> 0;
          tempb := (tempb shl 1) or carry;
          r.p.z := (tempb = 0);
          r.p.n := (tempb and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $28:
        begin // PLP
          self.getbyte(r.pc); // <-- Fallo CPU
          r.sp := r.sp + 1;
          tempb := self.getbyte($100 + r.sp);
          pon_pila(self.r, tempb);
          self.after_ei := true;
        end;
      $2A:
        begin // ROL A
          self.getbyte(r.pc); // <-- Fallo CPU
          if r.p.c then
            carry := 1
          else
            carry := 0;
          r.p.c := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or carry;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $30:
        if r.p.n then
        begin // BMI salta si n false
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $34:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // BIT
          numero := self.getbyte(posicion);
          r.p.z := (r.a and numero) = 0;
          r.p.n := (numero and $80) <> 0;
          r.p.o_v := (numero and $40) <> 0;
        end
        else
        begin
          self.getbyte(r.pc); // <-- Fallo CPU
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF}
        end;
      $38:
        begin // SEC
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.c := true;
        end;
      $40:
        begin // RTI
          r.sp := r.sp + 1;
          pon_pila(self.r, self.getbyte($100 + r.sp));
          r.sp := r.sp + 1;
          r.pc := self.getbyte($100 + r.sp);
          r.sp := r.sp + 1;
          r.pc := r.pc or (self.getbyte($100 + r.sp) shl 8);
          self.after_ei := false;
        end;
      $41, $45, $49, $4D, $51, $55, $59, $5D:
        begin // EOR
          r.a := r.a xor self.getbyte(posicion);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $43, $47, $4F, $53, $57, $5B, $5F:
        begin // SRE
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo de la CPU
          r.p.c := (tempb and $1) <> 0;
          tempb := tempb shr 1;
          r.a := r.a xor tempb;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $46, $4E, $56, $5E:
        begin // LSR
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo CPU
          r.p.c := (tempb and $1) <> 0;
          tempb := tempb shr 1;
          r.p.z := (tempb = 0);
          r.p.n := false;
          self.putbyte(posicion, tempb);
        end;
      $48:
        begin // PHA
          self.getbyte(r.pc); // <-- Fallo CPU
          self.putbyte($100 + r.sp, r.a);
          r.sp := r.sp - 1;
        end;
      $4A:
        begin // LSR A
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.c := (r.a and $1) <> 0;
          r.a := r.a shr 1;
          r.p.z := (r.a = 0);
          r.p.n := false;
        end;
      $4B:
        if self.tipo_cpu = TCPU_DECO16 then
          if @self.in_port1 <> nil then
            r.a := self.in_port1
          else
          begin // alr
            r.p.c := (r.a and $1) <> 0;
            r.a := r.a and self.getbyte(posicion);
            r.p.c := (r.a and $1) <> 0;
            r.a := r.a shr 1;
            r.p.z := (r.a = 0);
            r.p.n := false;
          end;
      $4C:
        r.pc := posicion; // JMP absoluto
      $50:
        if not(r.p.o_v) then
        begin // BVC salta si Overflow false
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $58:
        begin // CLI
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.int := false;
          self.after_ei := true;
        end;
      $5A:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // PHY
          self.getbyte(r.pc); // <-- Fallo CPU
          self.putbyte($100 + r.sp, r.y);
          r.sp := r.sp - 1;
        end
        else
        begin
          self.getbyte(r.pc); // <-- Fallo CPU
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF}
        end;
      $60:
        begin // RTS
          r.sp := r.sp + 1;
          r.pc := self.getbyte($100 + r.sp);
          r.sp := r.sp + 1;
          r.pc := r.pc or (self.getbyte($100 + r.sp) shl 8);
          r.pc := r.pc + 1;
        end;
      $61, $65, $69, $6D, $71, $75, $79, $7D:
        begin // ADC
          numero := self.getbyte(posicion);
          adc(self.r, numero, self.tipo_cpu);
        end;
      $63, $67, $6F, $73, $77, $7B, $7F:
        begin // RRA
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo de la CPU
          if r.p.c then
            tempw := tempb or $100
          else
            tempw := tempb;
          r.p.c := (tempb and $1) <> 0;
          tempw := tempw shr 1;
          adc(self.r, tempw, self.tipo_cpu);
          self.putbyte(posicion, tempw and $FF);
        end;
      $66, $6E, $76, $7E:
        begin // ROR
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo CPU
          if r.p.c then
            carry := $80
          else
            carry := 0;
          r.p.c := (tempb and $1) <> 0;
          tempb := (tempb shr 1) or carry;
          r.p.z := (tempb = 0);
          r.p.n := (tempb and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $68:
        begin // PLA
          self.getbyte(r.pc); // <-- Fallo CPU
          r.sp := r.sp + 1;
          r.a := self.getbyte($100 + r.sp);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $6A:
        begin // ROR A
          self.getbyte(r.pc); // <-- Fallo CPU
          if r.p.c then
            carry := $80
          else
            carry := 0;
          r.p.c := (r.a and $1) <> 0;
          r.a := (r.a shr 1) or carry;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $6B:
        begin // arr
          r.a := r.a and self.getbyte(posicion);
          if r.p.c then
            carry := $80
          else
            carry := 0;
          r.p.c := (r.a and $1) <> 0;
          r.a := (r.a shr 1) or carry;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $6C:
        begin // jmp rel
          // abs
          posicion := self.getbyte(r.pc);
          posicion := posicion + (self.getbyte(r.pc + 1) shl 8);
          r.pc := self.getbyte(posicion);
          r.pc := r.pc or (self.getbyte((((posicion + 1) and $00FF) or (posicion and $FF00)
            )) shl 8);
        end;
      $70:
        if r.p.o_v then
        begin // BVS salta si Overflow true
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $78:
        begin // SEI
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.int := true;
          self.after_ei := true;
        end;
      $7A:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // ply
          self.getbyte(r.pc); // <-- Fallo CPU
          r.sp := r.sp + 1;
          r.y := self.getbyte($100 + r.sp);
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end
        else
        begin
          self.getbyte(r.pc); // <-- Fallo CPU
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF}
        end;
      $80:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // bra
          r.pc := r.pc + shortint(numero);
        end
        else
        begin // nop doble
          self.getbyte(r.pc); // <-- Fallo CPU
        end;
      $81, $85, $8D, $91, $92, $95, $99, $9D:
        self.putbyte(posicion, r.a); // STA
      $83, $87, $8F, $97:
        self.putbyte(posicion, r.a and r.x); // sax
      $84, $8C, $94:
        self.putbyte(posicion, r.y); // STY
      $86, $8E, $96:
        self.putbyte(posicion, r.x); // STX
      $88:
        begin // DEY
          self.getbyte(r.pc); // <-- Fallo CPU
          r.y := r.y - 1;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $8A:
        begin // TXA
          self.getbyte(r.pc); // <-- Fallo CPU
          r.a := r.x;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $8B:
        begin // XAA
          r.a := r.x and self.getbyte(posicion);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $90:
        if not(r.p.c) then
        begin // BCC salta si c false
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $93, $9F:
        self.putbyte(posicion, r.a and r.x and tempb); // ahx
      $98:
        begin // TYA
          self.getbyte(r.pc); // <-- Fallo CPU
          r.a := r.y;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $9A:
        begin // TXS
          self.getbyte(r.pc); // <-- Fallo CPU
          r.sp := r.x;
        end;
      $64, $74, $9C, $9E:
        if self.tipo_cpu = TCPU_M65C02 then
          self.putbyte(posicion, 0) // stz
        else
        begin
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF}
        end;
      $A0, $A4, $AC, $B4, $BC:
        begin // LDY
          r.y := self.getbyte(posicion);
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $A1, $A5, $A9, $AD, $B1, $B5, $B9, $BD:
        begin // LDA
          r.a := self.getbyte(posicion);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $A2, $A6, $AE, $B6, $BE:
        begin // LDX
          r.x := self.getbyte(posicion);
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $A8:
        begin // TAY
          self.getbyte(r.pc); // <-- Fallo CPU
          r.y := r.a;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $AA:
        begin // TAX
          self.getbyte(r.pc); // <-- Fallo CPU
          r.x := r.a;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $B0:
        if r.p.c then
        begin // BCS salta si c true
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $B2:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // LDA
          r.a := self.getbyte(posicion);
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end
        else
        begin
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF};
        end;
      $B3:
        begin // LAX
          r.a := self.getbyte(posicion);
          r.x := r.a;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $B8:
        begin // CLV
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.o_v := false;
        end;
      $BA:
        begin // TSX
          self.getbyte(r.pc); // <-- Fallo CPU
          r.x := r.sp;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $C0, $C4, $CC:
        begin // CPY
          tempw := r.y - self.getbyte(posicion);
          r.p.c := (tempw and $100) = 0;
          r.p.z := (tempw and $FF) = 0;
          r.p.n := (tempw and $80) <> 0;
        end;
      $C1, $C5, $C9, $CD, $D1, $D2, $D5, $D9, $DD:
        begin // CMA
          tempw := r.a - self.getbyte(posicion);
          r.p.c := (tempw and $100) = 0;
          r.p.z := (tempw and $FF) = 0;
          r.p.n := (tempw and $80) <> 0;
        end;
      $C3, $CF, $D3, $DB, $DF:
        begin // DCP
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo de la CPU
          tempb := tempb - 1;
          if r.a >= tempb then
            r.p.c := true
          else
            r.p.c := false;
          r.p.z := ((r.a - tempb) and $FF) = 0;
          r.p.n := ((r.a - tempb) and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $C6, $CE, $D6, $DE:
        begin // DEC
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo CPU
          tempb := tempb - 1;
          r.p.z := (tempb = 0);
          r.p.n := (tempb and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $C8:
        begin // INY
          self.getbyte(r.pc); // <-- Fallo CPU
          r.y := r.y + 1;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $CA:
        begin // DEX
          self.getbyte(r.pc); // <-- Fallo CPU
          r.x := r.x - 1;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $D0:
        if not(r.p.z) then
        begin // BNE si z false
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $D7:
        begin
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo CPU
          tempb := tempb - 1;
          self.putbyte(posicion, tempb);
          tempw := self.r.a - tempb;
          r.p.c := (tempw and $100) = 0;
          r.p.z := (tempw and $FF) = 0;
          r.p.n := (tempw and $80) <> 0;
        end;
      $D8:
        begin // CLD
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.dec := false;
        end;
      $DA:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // PHX
          self.getbyte(r.pc); // <-- Fallo CPU
          self.putbyte($100 + r.sp, r.x);
          r.sp := r.sp - 1;
        end
        else
        begin
          self.getbyte(r.pc); // <-- Fallo CPU
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF}
        end;
      $E0, $E4, $EC:
        begin // CPX
          tempw := r.x - self.getbyte(posicion);
          r.p.c := (tempw and $100) = 0;
          r.p.z := (tempw and $FF) = 0;
          r.p.n := (tempw and $80) <> 0;
        end;
      $E1, $E5, $E9, $EB, $ED, $F1, $F5, $F9, $FD:
        begin // SBC
          numero := self.getbyte(posicion);
          sbc(self.r, numero, self.tipo_cpu);
        end;
      $E3, $EF, $F3, $F7, $FB, $FF:
        begin // ISB
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb);
          tempb := tempb + 1;
          sbc(self.r, tempb, self.tipo_cpu);
          self.putbyte(posicion, tempb);
        end;
      $E6, $EE, $F6, $FE:
        begin // INC
          tempb := self.getbyte(posicion);
          self.putbyte(posicion, tempb); // <-- Fallo CPU
          tempb := tempb + 1;
          r.p.z := (tempb = 0);
          r.p.n := (tempb and $80) <> 0;
          self.putbyte(posicion, tempb);
        end;
      $E8:
        begin // INX
          self.getbyte(r.pc); // <-- Fallo CPU
          r.x := r.x + 1;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $F0:
        if r.p.z then
        begin // BEQ salta si z true
          if ((r.pc + shortint(numero)) and $FF00) <> (r.pc and $FF00) then
            self.estados_demas := self.estados_demas + 2
          else
            self.estados_demas := self.estados_demas + 1;
          r.pc := r.pc + shortint(numero);
        end;
      $F8:
        begin // SED
          self.getbyte(r.pc); // <-- Fallo CPU
          r.p.dec := true;
        end;
      $FA:
        if self.tipo_cpu = TCPU_M65C02 then
        begin // plx
          self.getbyte(r.pc); // <-- Fallo CPU
          r.sp := r.sp + 1;
          r.x := self.getbyte($100 + r.sp);
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end
        else
        begin
          self.getbyte(r.pc); // <-- Fallo CPU
{$IFDEF DEBUG}
          // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' +
          // inttohex(instruccion, 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation,
          // [mbOk], 0)
{$ENDIF}
        end;
{$IFDEF DEBUG}
    else
      begin
        // MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: $' + inttohex(instruccion,
        // 2) + ' desconocida. PC=' + inttohex(r.ppc, 4), mtInformation, [mbOk], 0)
      end;
{$ENDIF}
    end; // del case!!
    tempw := estados_t[instruccion] + self.estados_demas;
    self.contador := self.contador + tempw;
    // The counter can be incremented in the following function!!
    if @self.despues_instruccion <> nil then
      self.despues_instruccion(tempw);
    tempw := self.contador - old_contador;
    timers.update(tempw, self.numero_cpu);
    self.totalt := self.totalt + tempw;
  end; // del while!!
end;

end.
