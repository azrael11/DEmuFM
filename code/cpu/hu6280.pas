unit hu6280;

interface

uses
  WinApi.Windows,
  main_engine,
  FMX.Dialogs,
  System.SysUtils,
  timer_engine,
  vars_hide,
  cpu_misc;

type
  band_h6280 = record
    n, o_v, t, brk, dec, int, z, c: boolean;
  end;

  reg_h6280 = record
    old_pc, pc: word;
    a, x, y, sp: byte;
    m: array [0 .. 7] of byte;
    p: band_h6280;
  end;

  preg_h6280 = ^reg_h6280;

  cpu_h6280 = class(cpu_class)
    constructor create(clock: dword; frames_div: word);
    destructor free;
  public
    getbyte: tgetbyte16;
    putbyte: tputbyte16;
    procedure reset;
    procedure run(maximo: single);
    procedure irq_status_w(direccion, valor: byte);
    procedure timer_w(posicion, valor: byte);
    procedure set_irq_line(irqline, state: byte);
    procedure change_ram_calls(getbyte: tgetbyte16; putbyte: tputbyte16);
  private
    r: preg_h6280;
    clocks_per_cycle: byte;
    timer_status: byte;
    timer_load, timer_value: dword;
    irq_pending: byte;
    irq_state: array [0 .. 2] of byte;
    io_buffer, irq_mask: byte;
    function translated(addr: word): dword;
    procedure pon_pila(temp: byte);
    function dame_pila: byte;
    function pull: byte;
    procedure push(valor: byte);
    procedure DO_INTERRUPT(vector: word);
    procedure CHECK_AND_TAKE_IRQ_LINES;
  end;

var
  h6280_0: cpu_h6280;

implementation

const
  tipo_dir: array [0 .. 255] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    0, 0, 1, 0, 7, 7, 7, 7, 1, 2, 1, 1, 0, 5, 0, 7, // 00
    2, $A, 0, 0, 0, 0, 0, 7, 1, 0, 1, 1, 0, 8, 0, 7, // 10
    3, 0, 1, 0, 0, 7, 7, 7, 1, 2, 0, 1, 0, 5, 0, 7, // 20
    2, 0, 0, 0, 0, 0, 0, 7, 1, 0, 1, 1, 0, 8, 0, 7, // 30
    1, 0, 0, 2, 2, 7, 7, 7, 1, 2, 1, 1, 3, 0, 0, 7, // 40
    2, 0, 0, 2, 0, 0, 0, 7, 1, 0, 1, 1, 0, 0, 0, 7, // 50
    1, 0, 1, 0, 6, 7, 7, 7, 1, 2, 1, 1, $D, 5, 0, 7, // 60
    0, $A, 0, 1, 4, 0, 0, 7, 1, $E, 1, 1, 3, 8, 0, 7, // 70
    2, 0, 1, 0, 6, 6, 6, 7, 1, 0, 1, 1, 3, 3, 3, 7, // 80
    2, $B, 0, 2, 4, 4, 0, 7, 1, $10, 1, 1, 3, $C, $C, 7, // 90
    2, 0, 2, 0, 7, 7, 7, 7, 1, 2, 1, 1, 5, 5, 5, 7, // A0
    2, $A, 9, 2, $F, $F, 0, 7, 0, $E, 0, 1, 8, 8, 0, 7, // B0
    2, 0, 1, 0, 7, 7, 7, 7, 1, 2, 1, 1, 0, 5, 5, 7, // C0
    2, $A, 9, 0, 1, $F, 0, 7, 1, $E, 1, 1, 0, 8, 8, 7, // D0
    2, 0, 0, 0, 0, 7, 7, 7, 1, 2, 1, 1, 0, 5, 5, 7, // E0
    2, $A, 0, 1, 0, $F, $F, 7, 1, 0, 1, 1, 0, 8, 8, 7); // F0

  estados_t: array [0 .. 255] of byte = (
    // 0 1 2 3 4 5 6 7 8 9 a b c d e f
    8, 7, 3, 5, 6, 4, 6, 7, 3, 2, 2, 2, 7, 5, 7, 4, // 00
    2, 7, 7, 5, 6, 4, 6, 7, 2, 5, 2, 2, 7, 5, 7, 4, // 10
    7, 7, 3, 5, 4, 4, 6, 7, 4, 2, 2, 2, 5, 5, 7, 4, // 20
    2, 7, 7, 2, 4, 4, 6, 7, 2, 5, 2, 2, 5, 5, 7, 4, // 30
    7, 7, 3, 4, 8, 4, 6, 7, 3, 2, 2, 2, 4, 5, 7, 4, // 40
    2, 7, 7, 5, 3, 4, 6, 7, 2, 5, 3, 2, 2, 5, 7, 4, // 50
    7, 7, 2, 4, 4, 4, 6, 7, 4, 2, 2, 2, 7, 5, 7, 4, // 60
    2, 7, 7, 0, 4, 4, 6, 7, 2, 5, 4, 2, 7, 5, 7, 4, // 70
    2, 7, 2, 7, 4, 4, 4, 7, 2, 2, 2, 2, 5, 5, 5, 4, // 80
    2, 7, 7, 8, 4, 4, 4, 7, 2, 5, 2, 2, 5, 5, 5, 4, // 90
    2, 7, 2, 7, 4, 4, 4, 7, 2, 2, 2, 2, 5, 5, 5, 4, // a0
    2, 7, 7, 8, 4, 4, 4, 7, 2, 5, 2, 2, 5, 5, 5, 4, // b0
    2, 7, 2, 0, 4, 4, 6, 7, 2, 2, 2, 2, 5, 5, 7, 4, // c0
    2, 7, 7, 0, 3, 4, 6, 7, 2, 5, 3, 2, 2, 5, 7, 4, // d0
    2, 7, 2, 0, 4, 4, 6, 7, 2, 2, 2, 2, 5, 5, 7, 4, // e0
    2, 7, 7, 0, 2, 4, 6, 7, 2, 5, 4, 2, 2, 5, 7, 4); // f0

constructor cpu_h6280.create(clock: dword; frames_div: word);
begin
  getmem(self.r, sizeof(reg_h6280));
  fillchar(self.r^, sizeof(reg_h6280), 0);
  self.numero_cpu := cpu_main_init(clock);
  self.clock := clock;
  self.tframes := (clock / frames_div) / machine_calls.fps_max;
end;

destructor cpu_h6280.free;
begin
  freemem(self.r);
end;

procedure cpu_h6280.change_ram_calls(getbyte: tgetbyte16; putbyte: tputbyte16);
begin
  self.getbyte := getbyte;
  self.putbyte := putbyte;
end;

procedure cpu_h6280.timer_w(posicion, valor: byte);
begin
  self.io_buffer := valor;
  case (posicion and 1) of
    0:
      begin // Counter preload */
        self.timer_load := ((valor and 127) + 1) * 1024;
        self.timer_value := self.timer_load;
      end;
    1:
      begin // Counter enable */
        if (valor and 1) <> 0 then
        begin // stop -> start causes reload */
          if self.timer_status = 0 then
            self.timer_value := self.timer_load;
        end;
        self.timer_status := valor and 1;
      end;
  end;
end;

procedure cpu_h6280.set_irq_line(irqline, state: byte);
begin
  if (irqline = INPUT_LINE_NMI) then
  begin
    if (state <> ASSERT_LINE) then
      exit;
    self.nmi_state := state;
  end
  else if (irqline < 3) then
  begin
    // If the state has not changed, just return */
    if (self.irq_state[irqline] = state) then
      exit;
    self.irq_state[irqline] := state;
  end;
  if (self.irq_pending = 0) then
    self.irq_pending := 2;
end;

procedure cpu_h6280.irq_status_w(direccion, valor: byte);
begin
  self.io_buffer := valor;
  case (direccion and 3) of
    2:
      begin // Write irq mask */
        self.irq_mask := valor and $7;
        if (self.irq_pending = 0) then
          self.irq_pending := 2;
      end;
    3:
      self.irq_state[2] := CLEAR_LINE; // Timer irq ack */
  end;
end;

function cpu_h6280.translated(addr: word): dword;
begin
  translated := ((r.m[addr shr 13] shl 13) or (addr and $1FFF));
end;

procedure cpu_h6280.reset;
begin
  r.p.int := true;
  r.p.brk := true;
  // stack starts at 0x01ff */
  r.sp := $FF;
  // read the reset vector into PC */
  r.pc := self.getbyte(self.translated($FFFE)) + (self.getbyte(self.translated($FFFF)) shl 8);
  // CPU starts in low speed mode */
  self.clocks_per_cycle := 4;
  // timer off by default */
  self.timer_status := 0;
  self.timer_load := 128 * 1024;
  // clear pending interrupts */
  self.irq_state[0] := CLEAR_LINE;
  self.irq_state[1] := CLEAR_LINE;
  self.irq_state[2] := CLEAR_LINE;
  self.nmi_state := CLEAR_LINE;
  self.irq_pending := 0;
end;

procedure cpu_h6280.pon_pila(temp: byte);
begin
  r.p.n := (temp and $80) <> 0;
  r.p.o_v := (temp and $40) <> 0;
  r.p.t := (temp and $20) <> 0;
  r.p.brk := (temp and $10) <> 0;
  r.p.dec := (temp and 8) <> 0;
  r.p.int := (temp and 4) <> 0;
  r.p.z := (temp and 2) <> 0;
  r.p.c := (temp and 1) <> 0;
end;

function cpu_h6280.dame_pila: byte;
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

function cpu_h6280.pull: byte;
begin
  r.sp := r.sp + 1;
  pull := self.getbyte((r.m[1] shl 13) or $100 or r.sp);
end;

procedure cpu_h6280.push(valor: byte);
begin
  self.putbyte((r.m[1] shl 13) or $100 or r.sp, valor);
  r.sp := r.sp - 1;
end;

procedure cpu_h6280.DO_INTERRUPT(vector: word);
begin
  self.contador := self.contador + (7 * self.clocks_per_cycle); // 7 cycles for an int */
  self.push(r.pc shr 8);
  self.push(r.pc and $FF);
  r.p.brk := false;
  self.push(self.dame_pila);
  r.p.int := true;
  r.p.dec := false;
  r.pc := self.getbyte(self.translated(vector)) + (self.getbyte(self.translated(vector + 1)) shl 8);
end;

procedure cpu_h6280.CHECK_AND_TAKE_IRQ_LINES;
begin
  if (self.nmi_state <> CLEAR_LINE) then
  begin
    self.nmi_state := CLEAR_LINE;
    self.DO_INTERRUPT($FFFC);
  end
  else if not(r.p.int) then
  begin
    if ((self.irq_state[2] <> CLEAR_LINE) and ((self.irq_mask and $4) = 0)) then
    begin
      self.DO_INTERRUPT($FFFA);
      if self.irq_state[2] = HOLD_LINE then
        self.irq_state[2] := CLEAR_LINE;
    end
    else if ((self.irq_state[0] <> CLEAR_LINE) and ((self.irq_mask and $2) = 0)) then
    begin
      self.DO_INTERRUPT($FFF8);
      if self.irq_state[0] = HOLD_LINE then
        self.irq_state[0] := CLEAR_LINE;
      // (*cpustate->irq_callback)(cpustate->device, 0);
    end
    else if ((self.irq_state[1] <> CLEAR_LINE) and ((self.irq_mask and $1) = 0)) then
    begin
      self.DO_INTERRUPT($FFF6);
      if self.irq_state[1] = HOLD_LINE then
        self.irq_state[1] := CLEAR_LINE;
      // (*cpustate->irq_callback)(cpustate->device, 1);
    end;
  end;
end;

procedure cpu_h6280.run(maximo: single);
var
  instruccion, numero, tempb, c: byte;
  estados_demas, from, to_, tempc, hi, lo, sum: word;
  posicion: parejas;
  length: dword;
begin
  self.contador := 0;
  while self.contador < maximo do
  begin
    if self.irq_pending = 2 then
      self.irq_pending := self.irq_pending - 1;
    r.old_pc := r.pc;
    estados_demas := 0;
    self.opcode := true;
    instruccion := self.getbyte(self.translated(r.pc));
    self.opcode := false;
    r.pc := r.pc + 1;
    case tipo_dir[instruccion] of
      0:
        begin
//        MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' Instruccion: ' + inttohex(instruccion,
//          2) + ' desconocida. PC=' + inttohex(r.old_pc, 10), mtInformation, [mbOk], 0);
        end;
      1:
        ; // Implicito
      2:
        begin // IMM
          numero := self.getbyte(self.translated(r.pc));
          r.pc := r.pc + 1;
        end;
      3:
        begin // ABS
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
        end;
      4:
        begin // ZPX
          posicion.l := self.getbyte(self.translated(r.pc)) + r.x;
          posicion.h := 0;
          r.pc := r.pc + 1;
        end;
      5:
        begin // ABS con get
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          numero := self.getbyte(self.translated(posicion.w));
        end;
      6:
        begin // ZPG
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := 0;
          r.pc := r.pc + 1;
        end;
      7:
        begin // ZPG con get
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := 0;
          r.pc := r.pc + 1;
          numero := self.getbyte((r.m[1] shl 13) or (posicion.w and $1FFF));
        end;
      8:
        begin // ABX con get
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          posicion.w := posicion.w + r.x;
          numero := self.getbyte(self.translated(posicion.w));
        end;
      9:
        begin // ZPI con get
          numero := self.getbyte(self.translated(r.pc));
          r.pc := r.pc + 1;
          posicion.l := self.getbyte((r.m[1] shl 13) or numero);
          numero := numero + 1;
          posicion.h := self.getbyte((r.m[1] shl 13) or numero);
          numero := self.getbyte(self.translated(posicion.w));
        end;
      $A:
        begin // IDY con get
          numero := self.getbyte(self.translated(r.pc));
          r.pc := r.pc + 1;
          posicion.l := self.getbyte((r.m[1] shl 13) or numero);
          numero := numero + 1;
          posicion.h := self.getbyte((r.m[1] shl 13) or numero);
          posicion.w := posicion.w + r.y;
          numero := self.getbyte(self.translated(posicion.w));
        end;
      $B:
        begin // IDY
          numero := self.getbyte(self.translated(r.pc));
          r.pc := r.pc + 1;
          posicion.l := self.getbyte((r.m[1] shl 13) or numero);
          numero := numero + 1;
          posicion.h := self.getbyte((r.m[1] shl 13) or numero);
          posicion.w := posicion.w + r.y;
        end;
      $C:
        begin // ABX
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          posicion.w := posicion.w + r.x;
        end;
      $D:
        begin // IND
          hi := self.getbyte(self.translated(r.pc));
          hi := hi or (self.getbyte(self.translated(r.pc + 1)) shl 8);
          r.pc := r.pc + 2;
          posicion.l := self.getbyte(self.translated(hi));
          posicion.h := self.getbyte(self.translated(hi + 1));
        end;
      $E:
        begin // ABY con get
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          posicion.w := posicion.w + r.y;
          numero := self.getbyte(self.translated(posicion.w));
        end;
      $F:
        begin // ZPX con get
          posicion.l := self.getbyte(self.translated(r.pc)) + r.x;
          posicion.h := 0;
          r.pc := r.pc + 1;
          numero := self.getbyte(self.translated(posicion.w));
        end;
      $10:
        begin // ABY
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          posicion.w := posicion.w + r.y;
        end;
    end;
    case instruccion of
      $00:
        begin // brk
          r.p.t := false;
          r.pc := r.pc + 1;
          self.push(r.pc shr 8);
          self.push(r.pc and $FF);
          self.push(self.dame_pila);
          r.p.int := true;
          r.p.dec := false;
          r.pc := self.getbyte(self.translated($FFF6)) +
            (self.getbyte(self.translated($FFF7)) shl 8);
        end;
      $02:
        begin // sxy
          r.p.t := false;
          tempb := r.x;
          r.x := r.y;
          r.y := tempb;
        end;
      $04:
        begin // tsb zp
          r.p.t := false;
          r.p.n := (numero and $80) <> 0;
          r.p.o_v := (numero and $40) <> 0;
          numero := numero or r.a;
          r.p.z := (numero = 0);
          self.putbyte(((r.m[1] shl 13) or (posicion.w and $1FFF)), numero);
        end;
      $05, $09, $0D, $11, $1D:
        if r.p.t then
        begin // ora
//          MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' ORA+T. PC=' + inttohex(r.old_pc, 4),
//            mtInformation, [mbOk], 0);
        end
        else
        begin
          r.a := r.a or numero;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $06:
        begin // asl zpg
          r.p.t := false;
          r.p.c := (numero and $80) <> 0;
          numero := numero shl 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte(((r.m[1] shl 13) or (posicion.w and $1FFF)), numero);
        end;
      $07, $17, $27, $37, $47, $57, $67, $77:
        begin // rmb0-7
          r.p.t := false;
          numero := numero and not(1 shl ((instruccion shr 4) and $7));
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), numero);
        end;
      $08:
        begin // php
          r.p.t := false;
          self.push(self.dame_pila);
        end;
      $0A:
        begin // asl a
          r.p.t := false;
          r.p.c := (r.a and $80) <> 0;
          r.a := r.a shl 1;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $0B, $1B, $2B, $3B, $4B, $5B, $6B, $7B, $8B, $9B, $AB, $BB, $CB, $DB, $EB, $FB:
        r.p.t := false; // nop
      $0F, $1F, $2F, $3F, $4F, $5F, $6F, $7F:
        begin // bbr
          r.p.t := false;
          tempb := self.getbyte(self.translated(r.pc));
          r.pc := r.pc + 1;
          if (numero and (1 shl ((instruccion shr 4) and $7))) = 0 then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(tempb);
          end;
        end;
      $10:
        begin // bpl
          r.p.t := false;
          if not(r.p.n) then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $18:
        begin // clc
          r.p.t := false;
          r.p.c := false;
        end;
      $1A:
        begin // ina
          r.p.t := false;
          r.a := r.a + 1;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $20:
        begin // jsr
          r.p.t := false;
          r.pc := r.pc - 1;
          self.push(r.pc shr 8);
          self.push(r.pc and $FF);
          r.pc := posicion.w;
        end;
      $22:
        begin // sax
          r.p.t := false;
          tempb := r.x;
          r.x := r.a;
          r.a := tempb;
        end;
      $25, $29, $2D, $3D:
        if r.p.t then
        begin // and
//          MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' AND+T. PC=' + inttohex(r.old_pc, 4),
//            mtInformation, [mbOk], 0);
        end
        else
        begin
          r.a := r.a and numero;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $26:
        begin // rol zpg
          r.p.t := false;
          if r.p.c then
            hi := (numero shl 1) or 1
          else
            hi := numero shl 1;
          r.p.c := (hi and $100) <> 0;
          numero := hi and $FF;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), numero);
        end;
      $28:
        begin // plp
          self.pon_pila(self.pull);
          r.p.brk := true;
          if (self.irq_pending = 0) then
            self.irq_pending := 2;
        end;
      $30:
        begin // bmi
          r.p.t := false;
          if r.p.n then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $38:
        begin // sec
          r.p.t := false;
          r.p.c := true;
        end;
      $3A:
        begin // dea
          r.p.t := false;
          r.a := r.a - 1;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $40:
        begin // rti
          self.pon_pila(self.pull);
          r.p.brk := true;
          r.pc := self.pull + (self.pull shl 8);
          if (self.irq_pending = 0) then
            self.irq_pending := 2;
        end;
      $43:
        begin // tma
          r.p.t := false;
          if (numero and $01) <> 0 then
            r.a := r.m[0];
          if (numero and $02) <> 0 then
            r.a := r.m[1];
          if (numero and $04) <> 0 then
            r.a := r.m[2];
          if (numero and $08) <> 0 then
            r.a := r.m[3];
          if (numero and $10) <> 0 then
            r.a := r.m[4];
          if (numero and $20) <> 0 then
            r.a := r.m[5];
          if (numero and $40) <> 0 then
            r.a := r.m[6];
          if (numero and $80) <> 0 then
            r.a := r.m[7];
        end;
      $44:
        begin // bsr
          r.p.t := false;
          self.push((r.pc - 1) shr 8);
          self.push((r.pc - 1) and $FF);
          r.pc := r.pc + shortint(numero);
        end;
      $45, $49:
        if r.p.t then
        begin // eor
//          MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' EOR+T. PC=' + inttohex(r.old_pc, 4),
//            mtInformation, [mbOk], 0);
        end
        else
        begin
          r.a := r.a xor numero;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $46:
        begin // lsr zpg
          r.p.t := false;
          r.p.c := (numero and 1) <> 0;
          numero := numero shr 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte(((r.m[1] shl 13) or (posicion.w and $1FFF)), numero);
        end;
      $48:
        begin // pha
          r.p.t := false;
          self.push(r.a);
        end;
      $4A:
        begin // lsr a
          r.p.t := false;
          r.p.c := (r.a and 1) <> 0;
          r.a := r.a shr 1;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $4C, $6C:
        begin // jmp
          r.p.t := false;
          r.pc := posicion.w;
        end;
      $50:
        begin // bvc
          r.p.t := false;
          if not(r.p.o_v) then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $53:
        begin // tam
          r.p.t := false;
          if (numero and $01) <> 0 then
            r.m[0] := r.a;
          if (numero and $02) <> 0 then
            r.m[1] := r.a;
          if (numero and $04) <> 0 then
            r.m[2] := r.a;
          if (numero and $08) <> 0 then
            r.m[3] := r.a;
          if (numero and $10) <> 0 then
            r.m[4] := r.a;
          if (numero and $20) <> 0 then
            r.m[5] := r.a;
          if (numero and $40) <> 0 then
            r.m[6] := r.a;
          if (numero and $80) <> 0 then
            r.m[7] := r.a;
        end;
      $58:
        begin // cli
          r.p.t := false;
          if r.p.int then
          begin
            r.p.int := false;
            if (self.irq_pending = 0) then
              self.irq_pending := 2;
          end;
        end;
      $5A:
        begin // phy
          r.p.t := false;
          self.push(r.y);
        end;
      $60:
        begin // rts
          r.p.t := false;
          r.pc := self.pull + (self.pull shl 8);
          r.pc := r.pc + 1;
        end;
      $62:
        begin // cla
          r.p.t := false;
          r.a := 0;
        end;
      $64, $74:
        begin // stz zp
          r.p.t := false;
          self.putbyte(((r.m[1] shl 13) or (posicion.w and $1FFF)), 0);
        end;
      $65, $69, $6D, $71, $79, $7D:
        begin // adc
          if r.p.t then
          begin
//            MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' ADC+T. PC=' + inttohex(r.old_pc,
//              4), mtInformation, [mbOk], 0);
          end
          else
          begin
            if r.p.dec then
            begin
              if r.p.c then
                c := 1
              else
                c := 0;
              lo := (r.a and $0F) + (numero and $0F) + c;
              hi := (r.a and $F0) + (numero and $F0);
              r.p.c := false;
              if (lo > $09) then
              begin
                hi := hi + $10;
                lo := lo + 06;
              end;
              if (hi > $90) then
                hi := hi + $60;
              if (hi and $FF00) <> 0 then
                r.p.c := true;
              r.a := (lo and $0F) + (hi and $F0);
              estados_demas := 1;
            end
            else
            begin
              if r.p.c then
                c := 1
              else
                c := 0;
              sum := r.a + numero + c;
              r.p.o_v := false;
              r.p.c := false;
              if (not(r.a xor numero) and (r.a xor sum) and $80) <> 0 then
                r.p.o_v := true;
              if (sum and $FF00) <> 0 then
                r.p.c := true;
              r.a := sum and $FF;
            end;
            r.p.z := (r.a = 0);
            r.p.n := (r.a and $80) <> 0;
          end;
        end;
      $66:
        begin // ror zpg
          r.p.t := false;
          if r.p.c then
            hi := $100 or numero
          else
            hi := numero;
          r.p.c := (numero and 1) <> 0;
          numero := hi shr 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), numero);
        end;
      $68:
        begin // pla
          r.p.t := false;
          r.a := self.pull;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $6A:
        begin // ror
          r.p.t := false;
          if r.p.c then
            hi := $100 or r.a
          else
            hi := r.a;
          r.p.c := (r.a and 1) <> 0;
          r.a := hi shr 1;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $73:
        begin // tii
          r.p.t := false;
          from := self.getbyte(self.translated(r.pc)) +
            (self.getbyte(self.translated(r.pc + 1)) shl 8);
          to_ := self.getbyte(self.translated(r.pc + 2)) +
            (self.getbyte(self.translated(r.pc + 3)) shl 8);
          length := self.getbyte(self.translated(r.pc + 4)) +
            (self.getbyte(self.translated(r.pc + 5)) shl 8);
          r.pc := r.pc + 6;
          if (length = 0) then
            length := $10000;
          estados_demas := (6 * length) + 17;
          while (length <> 0) do
          begin
            length := length - 1;
            numero := self.getbyte(self.translated(from));
            self.putbyte(self.translated(to_), numero);
            to_ := to_ + 1;
            from := from + 1;
          end;
        end;
      $78:
        begin // sei
          r.p.t := false;
          r.p.int := true;
        end;
      $7A:
        begin // ply
          r.p.t := false;
          r.y := self.pull;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $7C:
        begin // jmp ind+x
          r.p.t := false;
          posicion.w := posicion.w + r.x;
          r.pc := self.getbyte(self.translated(posicion.w));
          r.pc := r.pc or (self.getbyte(self.translated(posicion.w + 1)) shl 8);
        end;
      $80:
        begin // bra
          r.p.t := false;
          estados_demas := 2;
          r.pc := r.pc + shortint(numero);
        end;
      $82:
        begin // clx
          r.p.t := false;
          r.x := 0;
        end;
      $84, $94:
        begin // sty zp
          r.p.t := false;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), r.y);
        end;
      $85, $95:
        begin // sta zp
          r.p.t := false;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), r.a);
        end;
      $86:
        begin // stx zp
          r.p.t := false;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), r.x);
        end;
      $87, $97, $A7, $B7, $C7, $D7, $E7, $F7:
        begin // smb0-7
          r.p.t := false;
          numero := numero or (1 shl ((instruccion shr 4) and $7));
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), numero);
        end;
      $88:
        begin // dey
          r.p.t := false;
          r.y := r.y - 1;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $8A:
        begin // txa
          r.p.t := false;
          r.a := r.x;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $8C:
        begin // sty
          r.p.t := false;
          self.putbyte(self.translated(posicion.w), r.y);
        end;
      $8D, $91, $99, $9D:
        begin // sta
          r.p.t := false;
          self.putbyte(self.translated(posicion.w), r.a);
        end;
      $8E:
        begin // stx
          r.p.t := false;
          self.putbyte(self.translated(posicion.w), r.x);
        end;
      $8F, $9F, $AF, $BF, $CF, $DF, $EF, $FF:
        begin // bbs
          r.p.t := false;
          tempb := self.getbyte(self.translated(r.pc));
          r.pc := r.pc + 1;
          if (numero and (1 shl ((instruccion shr 4) and $7))) <> 0 then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(tempb);
          end;
        end;
      $90:
        begin // bcc
          r.p.t := false;
          if not(r.p.c) then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $93:
        begin // tst
          r.p.t := false;
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          tempb := self.getbyte(self.translated(posicion.w));
          r.p.n := (tempb and $80) <> 0;
          r.p.o_v := (tempb and $40) <> 0;
          r.p.z := (tempb and numero) = 0;
        end;
      $98:
        begin // tya
          r.p.t := false;
          r.a := r.y;
          r.p.z := (r.a = 0);
          r.p.n := (r.a and $80) <> 0;
        end;
      $9A:
        begin // txs
          r.p.t := false;
          r.sp := r.x;
        end;
      $9C, $9E:
        begin // stz
          r.p.t := false;
          self.putbyte(self.translated(posicion.w), 0);
        end;
      $A0, $A4, $AC, $B4, $BC:
        begin // ldy
          r.p.t := false;
          r.y := numero;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
        end;
      $A2, $A6, $AE:
        begin // ldx
          r.p.t := false;
          r.x := numero;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
        end;
      $A5, $A9, $AD, $B1, $B2, $B5, $B9, $BD:
        begin // lda
          r.p.t := false;
          r.a := numero;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
        end;
      $A8:
        begin // tay
          r.p.t := false;
          r.y := r.a;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $AA:
        begin // tax
          r.p.t := false;
          r.x := r.a;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $B0:
        begin // bcs
          r.p.t := false;
          if r.p.c then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $B3:
        begin // tst abx
          r.p.t := false;
          posicion.l := self.getbyte(self.translated(r.pc));
          posicion.h := self.getbyte(self.translated(r.pc + 1));
          r.pc := r.pc + 2;
          tempb := self.getbyte(self.translated(posicion.w + r.x));
          r.p.n := (tempb and $80) <> 0;
          r.p.o_v := (tempb and $40) <> 0;
          r.p.z := (tempb and numero) = 0;
        end;
      $C0, $C4:
        begin // cpy
          r.p.t := false;
          r.p.c := (r.y >= numero);
          r.p.z := (((r.y - numero) and $FF) = 0);
          r.p.n := ((r.y - numero) and $80) <> 0;
        end;
      $C2:
        begin // cly
          r.p.t := false;
          r.y := 0;
        end;
      $C5, $C9, $CD, $D1, $D2, $D5, $D9, $DD:
        begin // cmp
          r.p.t := false;
          r.p.c := (r.a >= numero);
          r.p.z := (((r.a - numero) and $FF) = 0);
          r.p.n := ((r.a - numero) and $80) <> 0;
        end;
      $C6:
        begin // dec zpg
          r.p.t := false;
          numero := numero - 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), numero);
        end;
      $C8:
        begin // iny
          r.p.t := false;
          r.y := r.y + 1;
          r.p.z := (r.y = 0);
          r.p.n := (r.y and $80) <> 0;
        end;
      $CA:
        begin // dex
          r.p.t := false;
          r.x := r.x - 1;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $CE, $DE:
        begin // dec
          r.p.t := false;
          numero := numero - 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte(self.translated(posicion.w), numero);
        end;
      $D0:
        begin // bne
          r.p.t := false;
          if not(r.p.z) then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $D4:
        self.clocks_per_cycle := 1; // csh
      $D8:
        begin // cld
          r.p.t := false;
          r.p.dec := false;
        end;
      $DA:
        begin // phx
          r.p.t := false;
          self.push(r.x);
        end;
      $E0:
        begin // cpx
          r.p.t := false;
          r.p.c := (r.x >= numero);
          r.p.z := (((r.x - numero) and $FF) = 0);
          r.p.n := ((r.x - numero) and $80) <> 0;
        end;
      $E5, $E9, $ED, $F1, $F5, $FD:
        begin // sbc
          if r.p.t then
          begin
//            MessageDlg('CPU: ' + inttohex(self.numero_cpu, 1) + ' SBC+T. PC=' + inttohex(r.old_pc,
//              4), mtInformation, [mbOk], 0);
          end
          else
          begin
            if r.p.dec then
            begin
              if r.p.c then
                c := 0
              else
                c := 1;
              sum := r.a - numero - c;
              lo := (r.a and $0F) - (numero and $0F) - c;
              hi := (r.a and $F0) - (numero and $F0);
              r.p.c := false;
              if (lo and $F0) <> 0 then
                lo := lo - 6;
              if (lo and $80) <> 0 then
                hi := hi - $10;
              if (hi and $0F00) <> 0 then
                hi := hi - $60;
              if ((sum and $FF00) = 0) then
                r.p.c := true;
              r.a := (lo and $0F) + (hi and $F0);
              estados_demas := 1;
            end
            else
            begin
              if r.p.c then
                c := 0
              else
                c := 1;
              sum := r.a - numero - c;
              r.p.o_v := false;
              r.p.c := false;
              if ((r.a xor numero) and (r.a xor sum) and $80) <> 0 then
                r.p.o_v := true;
              if ((sum and $FF00) = 0) then
                r.p.c := true;
              r.a := sum and $FF;
            end;
            r.p.z := (r.a = 0);
            r.p.n := (r.a and $80) <> 0;
          end;
        end;
      $E6, $F6:
        begin // inc zpg
          r.p.t := false;
          numero := numero + 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte((r.m[1] shl 13) or (posicion.w and $1FFF), numero);
        end;
      $E8:
        begin // inx
          r.p.t := false;
          r.x := r.x + 1;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
      $EA:
        r.p.t := false; // nop
      $EE, $FE:
        begin // inc
          r.p.t := false;
          numero := numero + 1;
          r.p.z := (numero = 0);
          r.p.n := (numero and $80) <> 0;
          self.putbyte(self.translated(posicion.w), numero);
        end;
      $F0:
        begin // beq
          r.p.t := false;
          if r.p.z then
          begin
            estados_demas := 2;
            r.pc := r.pc + shortint(numero);
          end;
        end;
      $F3:
        begin // tai
          r.p.t := false;
          from := self.getbyte(self.translated(r.pc)) +
            (self.getbyte(self.translated(r.pc + 1)) shl 8);
          to_ := self.getbyte(self.translated(r.pc + 2)) +
            (self.getbyte(self.translated(r.pc + 3)) shl 8);
          length := self.getbyte(self.translated(r.pc + 4)) +
            (self.getbyte(self.translated(r.pc + 5)) shl 8);
          r.pc := r.pc + 6;
          numero := 0;
          if (length = 0) then
            length := $10000;
          estados_demas := (6 * length) + 17;
          while (length <> 0) do
          begin
            self.putbyte(self.translated(to_), self.getbyte(self.translated(from + numero)));
            to_ := to_ + 1;
            numero := numero xor 1;
            length := length - 1;
          end;
        end;
      $F8:
        begin // sed
          r.p.t := false;
          r.p.dec := true;
        end;
      $FA:
        begin // plx
          r.p.t := false;
          r.x := self.pull;
          r.p.z := (r.x = 0);
          r.p.n := (r.x and $80) <> 0;
        end;
    end; // del case instruccion
    tempc := (estados_t[instruccion] + estados_demas) * self.clocks_per_cycle;
    self.contador := self.contador + tempc;
    self.timer_value := self.timer_value - tempc;
    // IRQ's
    if (self.irq_pending <> 0) then
    begin
      if (self.irq_pending = 1) then
      begin
        if not(r.p.int) then
        begin
          self.irq_pending := self.irq_pending - 1;
          self.CHECK_AND_TAKE_IRQ_LINES;
        end;
      end
      else
      begin
        self.irq_pending := self.irq_pending - 1;
      end;
    end;
    // Check internal timer */
    if (self.timer_status <> 0) then
    begin
      if (self.timer_value <= 0) then
      begin
        if (self.irq_pending <> 0) then
          self.irq_pending := 1;
        while (self.timer_value <= 0) do
          self.timer_value := self.timer_value + self.timer_load;
        self.set_irq_line(2, ASSERT_LINE);
      end;
    end;
    timers.update(tempc, self.numero_cpu);
  end;
end;

end.
