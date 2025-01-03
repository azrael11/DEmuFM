unit lr35902;

interface

uses
  WinApi.Windows,
  main_engine,
  timer_engine,
  FMX.Dialogs,
  System.SysUtils,
  vars_hide,
  cpu_misc;

type
  band_lr = record
    z, n, h, c: boolean;
  end;

  reg_lr = record
    pc, sp: word;
    a: byte;
    bc, de, hl: parejas;
    f: band_lr;
  end;
  preg_lr=^reg_lr;
  cpu_lr = class(cpu_class)
    constructor Create(clock: dword; frames_div: word);
    destructor free;
  public
    speed: byte;
    ime, change_speed, changed_speed: boolean;
    vblank_ena, lcdstat_ena, timer_ena, joystick_ena, serial_ena: boolean;
    vblank_req, lcdstat_req, timer_req, joystick_req, serial_req: boolean;
    procedure reset;
    procedure run(maximo: single);
        function get_internal_r:preg_lr;
        function save_snapshot(datos:pbyte):word;
        procedure load_snapshot(datos:pbyte);
  private
        r:reg_lr;
    after_ei, halt: boolean;
    function read_word(direccion: word): word;
    function inc_8bit(val: byte): byte;
    function dec_8bit(val: byte): byte;
    procedure bit_8bit(bit, val: byte);
    function sla_8bit(val: byte): byte;
    function sra_8bit(val: byte): byte;
    function rl_8bit(val: byte): byte;
    function rlc_8bit(val: byte): byte;
    function rr_8bit(val: byte): byte;
    function rrc_8bit(val: byte): byte;
    function swap_8bit(val: byte): byte;
    function srl_8bit(val: byte): byte;
    procedure cmp_a(val: byte);
    procedure or_a(val: byte);
    procedure xor_a(val: byte);
    procedure and_a(val: byte);
    procedure add_a(val: byte);
    procedure addc_a(val: byte);
    procedure sub_a(val: byte);
    procedure sbc_a(val: byte);
    procedure add_hl(val: word);
  end;

var
  lr35902_0: cpu_lr;

implementation
uses
  gb;

const
  gb_t: array [0 .. 255] of byte = (4, 12, 8, 8, 4, 4, 8, 4, 20, 8, 8, 8, 4, 4, 8, 4, // 0
    4, 12, 8, 8, 4, 4, 8, 4, 12, 8, 8, 8, 4, 4, 8, 4, // 1
    8, 12, 8, 8, 4, 4, 8, 4, 8, 8, 8, 8, 4, 4, 8, 4, // 2
    8, 12, 8, 8, 12, 12, 12, 4, 8, 8, 8, 8, 4, 4, 8, 4, // 3
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // 4
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // 5
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // 6
    8, 8, 8, 8, 8, 8, 4, 8, 4, 4, 4, 4, 4, 4, 8, 4, // 7
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // 8
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // 9
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // a
    4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, // b
    8, 12, 12, 16, 12, 16, 8, 16, 8, 16, 12, 0, 12, 24, 8, 16, // c
    8, 12, 12, 4, 12, 16, 8, 16, 8, 16, 12, 4, 12, 4, 8, 16, // d
    12, 12, 8, 4, 4, 16, 8, 16, 16, 4, 16, 4, 4, 4, 8, 16, // e
    12, 12, 8, 4, 4, 16, 8, 16, 12, 8, 16, 4, 4, 4, 8, 16); // f
  gb_cb_t: array [0 .. $FF] of byte = (8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // 0
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // 1
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // 2
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // 3
    8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, // 4
    8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, // 5
    8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, // 6
    8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8, // 7
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // 8
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // 9
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // a
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // b
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // c
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // d
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8, // e
    8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8); // f

constructor cpu_lr.Create(clock: dword; frames_div: word);
begin
fillchar(self.r,sizeof(reg_lr),0);
  self.numero_cpu := cpu_main_init(clock);
  self.clock := clock;
  self.tframes := (clock / frames_div) / machine_calls.fps_max;
end;

destructor cpu_lr.free;
begin
end;

procedure cpu_lr.reset;
begin
  self.speed := 0;
  self.change_speed := false;
  self.tframes:=(GB_CLOCK/154)/machine_calls.fps_max;
  r.sp := 0;
  r.pc := 0;
  r.bc.w := 0;
  r.de.w := 0;
  r.hl.w := 0;
  //Es es importante! Hook depede de ello
  self.ime:=false;
  self.change_irq(CLEAR_LINE);
  self.after_ei := false;
  r.a := 0;
  r.f.z := false;
  r.f.n := false;
  r.f.h := false;
  r.f.c := false;
  self.halt := false;
  self.vblank_ena := false;
  self.lcdstat_ena := false;
  self.timer_ena := false;
  self.joystick_ena := false;
  self.serial_ena := false;
  self.vblank_req := false;
  self.lcdstat_req := false;
  self.timer_req := false;
  self.joystick_req := false;
  self.serial_req := false;
end;

function cpu_lr.save_snapshot(datos:pbyte):word;
var
  temp:pbyte;
  buffer:array[0..15] of byte;
  size:dword;
begin
  temp:=datos;
  copymemory(temp,@self.r,sizeof(reg_lr));
  size:=sizeof(reg_lr);
  inc(temp,sizeof(reg_lr));
  buffer[0]:=byte(self.after_ei);
  buffer[1]:=byte(self.halt);
  buffer[2]:=self.speed;
  buffer[3]:=byte(self.ime);
  buffer[4]:=byte(self.change_speed);
  buffer[5]:=byte(self.changed_speed);
  buffer[6]:=byte(self.vblank_ena);
  buffer[7]:=byte(self.lcdstat_ena);
  buffer[8]:=byte(self.timer_ena);
  buffer[9]:=byte(self.joystick_ena);
  buffer[10]:=byte(self.serial_ena);
  buffer[11]:=byte(self.vblank_req);
  buffer[12]:=byte(self.lcdstat_req);
  buffer[13]:=byte(self.timer_req);
  buffer[14]:=byte(self.joystick_req);
  buffer[15]:=byte(self.serial_req);
  copymemory(temp,@buffer[0],16);
  save_snapshot:=size+16;
end;

procedure cpu_lr.load_snapshot(datos:pbyte);
var
  temp:pbyte;
  buffer:array[0..15] of byte;
begin
  temp:=datos;
  copymemory(@self.r,temp,sizeof(reg_lr));
  inc(temp,sizeof(reg_lr));
  copymemory(@buffer[0],temp,16);
  self.after_ei:=buffer[0]<>0;
  self.halt:=buffer[1]<>0;
  self.speed:=buffer[2];
  self.ime:=buffer[3]<>0;
  self.change_speed:=buffer[4]<>0;
  self.changed_speed:=buffer[5]<>0;
  self.vblank_ena:=buffer[6]<>0;
  self.lcdstat_ena:=buffer[7]<>0;
  self.timer_ena:=buffer[8]<>0;
  self.joystick_ena:=buffer[9]<>0;
  self.serial_ena:=buffer[10]<>0;
  self.vblank_req:=buffer[11]<>0;
  self.lcdstat_req:=buffer[12]<>0;
  self.timer_req:=buffer[13]<>0;
  self.joystick_req:=buffer[14]<>0;
  self.serial_req:=buffer[15]<>0;
end;

function cpu_lr.get_internal_r:preg_lr;
begin
  get_internal_r:=@self.r;
end;


function cpu_lr.read_word(direccion: word): word;
begin
  read_word := self.getbyte(direccion) + (self.getbyte(direccion + 1) shl 8);
end;

function cpu_lr.inc_8bit(val: byte): byte;
var
  res: byte;
begin
  res := val + 1;
  r.f.n := false;
  r.f.z := (res = 0);
  r.f.h := (res and $F) = 0;
  inc_8bit := res;
end;

function cpu_lr.dec_8bit(val: byte): byte;
var
  res: byte;
begin
  res := val - 1;
  r.f.n := true;
  r.f.z := (res = 0);
  r.f.h := (res and $F) = $F;
  dec_8bit := res;
end;

function res_8bit(bit, val: byte): byte;
begin
  res_8bit := val and (not(1 shl bit));
end;

function set_8bit(bit, val: byte): byte;
begin
  set_8bit := val or (1 shl bit);
end;

procedure cpu_lr.bit_8bit(bit, val: byte);
begin
  r.f.z := (val and (1 shl bit)) = 0;
  r.f.h := true;
  r.f.n := false;
end;

function cpu_lr.sla_8bit(val: byte): byte;
var
  res: byte;
begin
  r.f.c := (val and $80) <> 0;
  res := val shl 1;
  r.f.z := (res = 0);
  r.f.n := false;
  r.f.h := false;
  sla_8bit := res;
end;

function cpu_lr.sra_8bit(val: byte): byte;
var
  res: byte;
begin
  r.f.c := (val and $1) <> 0;
  res := (val shr 1) or (val and $80);
  r.f.z := (res = 0);
  r.f.n := false;
  r.f.h := false;
  sra_8bit := res;
end;

function cpu_lr.rl_8bit(val: byte): byte;
var
  res, c: byte;
begin
  if r.f.c then
    c := 1
  else
    c := 0;
  r.f.c := (val and $80) <> 0;
  res := (val shl 1) or c;
  r.f.z := (res = 0);
  r.f.h := false;
  r.f.n := false;
  rl_8bit := res;
end;

function cpu_lr.rlc_8bit(val: byte): byte;
var
  res: byte;
begin
  res := (val shl 1) or (val shr 7);
  r.f.c := (res and 1) <> 0;
  r.f.z := (res = 0);
  r.f.h := false;
  r.f.n := false;
  rlc_8bit := res;
end;

function cpu_lr.rr_8bit(val: byte): byte;
var
  res, c: byte;
begin
  if r.f.c then
    c := $80
  else
    c := 0;
  r.f.c := (val and $1) <> 0;
  res := (val shr 1) or c;
  r.f.z := (res = 0);
  r.f.h := false;
  r.f.n := false;
  rr_8bit := res;
end;

function cpu_lr.rrc_8bit(val: byte): byte;
var
  res: byte;
begin
  res := ((val shr 1) or (val shl 7));
  r.f.c := (res and $80) <> 0;
  r.f.z := (res = 0);
  r.f.n := false;
  r.f.h := false;
  rrc_8bit := res;
end;

procedure cpu_lr.cmp_a(val: byte);
var
  r1, r2: word;
begin
  r1 := (r.a and $F) - (val and $F);
  r2 := r.a - val;
  r.f.z := ((r2 and $FF) = 0);
  r.f.c := (r2 > $FF);
  r.f.h := (r1 > $0F);
  r.f.n := true;
end;

procedure cpu_lr.or_a(val: byte);
begin
  r.a := r.a or val;
  r.f.z := (r.a = 0);
  r.f.n := false;
  r.f.h := false;
  r.f.c := false;
end;

procedure cpu_lr.xor_a(val: byte);
begin
  r.a := r.a xor val;
  r.f.z := (r.a = 0);
  r.f.n := false;
  r.f.h := false;
  r.f.c := false;
end;

procedure cpu_lr.and_a(val: byte);
begin
  r.a := r.a and val;
  r.f.z := (r.a = 0);
  r.f.n := false;
  r.f.h := true;
  r.f.c := false;
end;

function cpu_lr.swap_8bit(val: byte): byte;
var
  res: byte;
begin
  res := (val shr 4) or (val shl 4);
  r.f.z := (res = 0);
  r.f.n := false;
  r.f.h := false;
  r.f.c := false;
  swap_8bit := res;
end;

function cpu_lr.srl_8bit(val: byte): byte;
var
  res: byte;
begin
  r.f.c := (val and 1) <> 0;
  res := val shr 1;
  r.f.z := (res = 0);
  r.f.n := false;
  r.f.h := false;
  srl_8bit := res;
end;

procedure cpu_lr.add_a(val: byte);
var
  r1, r2: word;
begin
  r1 := (r.a and $F) + (val and $F);
  r2 := r.a + val;
  r.a := r2 and $FF;
  r.f.z := (r.a = 0);
  r.f.c := (r2 > $FF);
  r.f.h := (r1 > $0F);
  r.f.n := false;
end;

procedure cpu_lr.addc_a(val: byte);
var
  carry: byte;
  r1, r2: word;
begin
  if r.f.c then
    carry := 1
  else
    carry := 0;
  r1 := (r.a and $F) + (val and $F) + carry;
  r2 := r.a + val + carry;
  r.a := r2 and $FF;
  r.f.n := false;
  r.f.z := (r2 and $FF) = 0;
  r.f.c := (r2 > $FF);
  r.f.h := (r1 > $F);
end;

procedure cpu_lr.sub_a(val: byte);
var
  r1, r2: word;
begin
  r1 := (r.a and $F) - (val and $F);
  r2 := r.a - val;
  r.a := r2 and $FF;
  r.f.z := (r.a = 0);
  r.f.c := (r2 > $FF);
  r.f.h := (r1 > $0F);
  r.f.n := true;
end;

procedure cpu_lr.sbc_a(val: byte);
var
  carry: byte;
  r1, r2: word;
begin
  if r.f.c then
    carry := 1
  else
    carry := 0;
  r1 := (r.a and $F) - (val and $F) - carry;
  r2 := r.a - val - carry;
  r.a := r2 and $FF;
  r.f.n := true;
  r.f.z := (r.a = 0);
  r.f.c := (r2 > $FF);
  r.f.h := (r1 > $F);
end;

procedure cpu_lr.add_hl(val: word);
var
  r1, r2: dword;
begin
  r1 := r.hl.w + val;
  r2 := (r.hl.w and $FFF) + (val and $FFF);
  r.f.n := false;
  r.f.c := (r1 > $FFFF);
  r.f.h := (r2 > $0FFF);
  r.hl.w := r1 and $FFFF;
end;

procedure cpu_lr.run(maximo: single);
var
  instruccion: byte;
  tempw, oldpc, old_contador: word;
  tempb, tempb2: byte;
begin
  self.contador := 0;
  while self.contador < maximo do
  begin
    // Aqui van las IRQ's
    self.estados_demas := 0;
    old_contador := self.contador;
    if not(self.after_ei) then
    begin
      if (self.vblank_ena and self.vblank_req) then
      begin // Vblank
        if self.halt then
          self.estados_demas := 4;
        self.halt := false;
        if self.ime then
        begin
          self.ime := false;
          self.vblank_req := false;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $40;
          self.estados_demas := self.estados_demas + 20;
        end;
      end
      else
      begin
        if (self.lcdstat_ena and self.lcdstat_req) then
        begin // STAT
          if self.halt then
            self.estados_demas := 4;
          self.halt := false;
          if self.ime then
          begin
            self.ime := false;
            self.lcdstat_req := false;
            r.sp := r.sp - 2;
            self.putbyte(r.sp, r.pc and $FF);
            self.putbyte(r.sp + 1, r.pc shr 8);
            r.pc := $48;
            self.estados_demas := self.estados_demas + 20;
          end;
        end
        else
        begin
          if (self.timer_ena and self.timer_req) then
          begin // Timers
            if self.halt then
              self.estados_demas := 4;
            self.halt := false;
            if self.ime then
            begin
              self.ime := false;
              self.timer_req := false;
              r.sp := r.sp - 2;
              self.putbyte(r.sp, r.pc and $FF);
              self.putbyte(r.sp + 1, r.pc shr 8);
              r.pc := $50;
              self.estados_demas := self.estados_demas + 20;
            end
          end
          else
          begin
            if (self.serial_ena and self.serial_req) then
            begin // Serial
              if self.halt then
                self.estados_demas := 4;
              self.halt := false;
              if self.ime then
              begin
                self.ime := false;
                self.serial_req := false;
                r.sp := r.sp - 2;
                self.putbyte(r.sp, r.pc and $FF);
                self.putbyte(r.sp + 1, r.pc shr 8);
                r.pc := $58;
                self.estados_demas := self.estados_demas + 20;
              end
            end
            else
            begin
              if (self.joystick_ena and self.joystick_req) then
              begin // Joystick
                if self.halt then
                  self.estados_demas := 4;
                self.halt := false;
                if self.ime then
                begin
                  self.ime := false;
                  self.joystick_req := false;
                  r.sp := r.sp - 2;
                  self.putbyte(r.sp, r.pc and $FF);
                  self.putbyte(r.sp + 1, r.pc shr 8);
                  r.pc := $60;
                  self.estados_demas := self.estados_demas + 20;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    self.after_ei := false;
    if self.halt then
      r.pc := r.pc - 1;
    oldpc := r.pc;
    instruccion := self.getbyte(r.pc);
    r.pc := r.pc + 1;
    case instruccion of
      $00:
        ; // nop
      $01:
        begin // ld bc,nnnn
          r.bc.w := read_word(r.pc);
          r.pc := r.pc + 2;
        end;
      $02:
        self.putbyte(r.bc.w, r.a); // ld (bc),a
      $03:
        r.bc.w := r.bc.w + 1; // inc bc
      $04:
        r.bc.h := inc_8bit(r.bc.h); // inc b
      $05:
        r.bc.h := dec_8bit(r.bc.h); // dec b
      $06:
        begin // ld b,nn
          r.bc.h := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $07:
        begin // RLCA
          r.f.c := (r.a and $80) <> 0;
          r.a := ((r.a shl 1) or (r.a shr 7));
          r.f.h := false;
          r.f.z := false;
          r.f.n := false;
        end;
      $08:
        begin // LD (nnnn),SP
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          self.putbyte(tempw, r.sp and $FF);
          self.putbyte(tempw + 1, r.sp shr 8);
        end;
      $09:
        add_hl(r.bc.w); // ADD HL,BC
      $0A:
        r.a := self.getbyte(r.bc.w); // ld a,(bc)
      $0B:
        r.bc.w := r.bc.w - 1; // dec bc
      $0C:
        r.bc.l := inc_8bit(r.bc.l); // inc c
      $0D:
        r.bc.l := dec_8bit(r.bc.l); // dec c
      $0E:
        begin // ld c,nn
          r.bc.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $0F:
        begin // RRCA
          r.f.c := (r.a and $1) <> 0;
          r.a := (r.a shr 1) or (r.a shl 7);
          r.f.z := false;
          r.f.h := false;
          r.f.n := false;
        end;
      $10:
        begin
          self.getbyte(self.r.pc);
          r.pc := r.pc + 1;
          if self.change_speed then
          begin // stop
            self.speed := self.speed xor 1;
            self.change_speed := false;
            self.changed_speed := true;
          end;
        end;
      $11:
        begin // ld de,nnnn
          r.de.w := read_word(r.pc);
          r.pc := r.pc + 2;
        end;
      $12:
        self.putbyte(r.de.w, r.a); // ld (de),a
      $13:
        r.de.w := r.de.w + 1; // inc de
      $14:
        r.de.h := inc_8bit(r.de.h); // inc d
      $15:
        r.de.h := dec_8bit(r.de.h); // dec d
      $16:
        begin // ld d,nn
          r.de.h := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $17:
        begin // RLA
          if r.f.c then
            tempb := 1
          else
            tempb := 0;
          r.f.c := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or tempb;
          r.f.n := false;
          r.f.z := false;
          r.f.h := false;
        end;
      $18:
        r.pc := r.pc + 1 + shortint(self.getbyte(r.pc)); // JR +-nn
      $19:
        add_hl(r.de.w); // ADD HL,DE
      $1A:
        r.a := self.getbyte(r.de.w); // ld a,(de)
      $1B:
        r.de.w := r.de.w - 1; // dec de
      $1C:
        r.de.l := inc_8bit(r.de.l); // inc e
      $1D:
        r.de.l := dec_8bit(r.de.l); // dec e
      $1E:
        begin // LD E,nn
          r.de.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $1F:
        begin // RRA
          if r.f.c then
            tempb := $80
          else
            tempb := 0;
          r.f.c := (r.a and $1) <> 0;
          r.a := (r.a shr 1) or tempb;
          r.f.z := false;
          r.f.h := false;
          r.f.n := false;
        end;
      $20:
        if not(r.f.z) then
        begin // JR NZ,nn
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1 + shortint(tempb);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 1;
        end;
      $21:
        begin // ld hl,nnnn
          r.hl.w := read_word(r.pc);
          r.pc := r.pc + 2;
        end;
      $22:
        begin // ld (hl+),a
          self.putbyte(r.hl.w, r.a);
          r.hl.w := r.hl.w + 1;
        end;
      $23:
        r.hl.w := r.hl.w + 1; // inc hl
      $24:
        r.hl.h := inc_8bit(r.hl.h); // INC H
      $25:
        r.hl.h := dec_8bit(r.hl.h); // DEC H
      $26:
        begin // LD H,nn
          r.hl.h := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $27:
        begin // DAA
          tempw := r.a;
          if not(r.f.n) then
          begin
            if (r.f.h or ((tempw and $F) > 9)) then
              tempw := tempw + $6;
            if (r.f.c or (tempw > $9F)) then
              tempw := tempw + $60;
          end
          else
          begin
            if r.f.h then
            begin
              tempw := tempw - 6;
              if not(r.f.c) then
                tempw := tempw and $FF;
            end;
            if r.f.c then
              tempw := tempw - $60;
          end;
          r.f.h := false;
          if (tempw and $100) <> 0 then
            r.f.c := true;
          r.a := tempw and $FF;
          r.f.z := (r.a = 0);
        end;
      $28:
        if r.f.z then
        begin // JR Z,nn
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1 + shortint(tempb);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 1;
        end;
      $29:
        add_hl(r.hl.w); // add hl,hl
      $2A:
        begin // LD A,(HL+)
          r.a := self.getbyte(r.hl.w);
          r.hl.w := r.hl.w + 1;
        end;
      $2B:
        r.hl.w := r.hl.w - 1; // DEC HL
      $2C:
        r.hl.l := inc_8bit(r.hl.l); // INC L
      $2D:
        r.hl.l := dec_8bit(r.hl.l); // DEC L */
      $2E:
        begin // LD L,nn
          r.hl.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $2F:
        begin // CPL
          r.a := r.a xor $FF;
          r.f.n := true;
          r.f.h := true;
        end;
      $30:
        if not(r.f.c) then
        begin // JR NC,nn
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1 + shortint(tempb);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 1;
        end;
      $31:
        begin // LD SP,nnnn
          r.sp := read_word(r.pc);
          r.pc := r.pc + 2;
        end;
      $32:
        begin // ld (hl-),a
          self.putbyte(r.hl.w, r.a);
          r.hl.w := r.hl.w - 1;
        end;
      $33:
        r.sp := r.sp + 1; // inc sp
      $34:
        begin // inc (hl)
          tempb := self.getbyte(r.hl.w) + 1;
          self.putbyte(r.hl.w, tempb);
          r.f.n := false;
          r.f.z := (tempb = 0);
          r.f.h := (tempb and $F) = 0;
        end;
      $35:
        begin // dec (hl)
          tempb := self.getbyte(r.hl.w) - 1;
          self.putbyte(r.hl.w, tempb);
          r.f.n := true;
          r.f.z := (tempb = 0);
          r.f.h := (tempb and $F) = $F;
        end;
      $36:
        begin // ld (hl),nn
          self.putbyte(r.hl.w, self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $37:
        begin // scf
          r.f.n := false;
          r.f.h := false;
          r.f.c := true;
        end;
      $38:
        if r.f.c then
        begin // JR C,nn
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1 + shortint(tempb);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 1;
        end;
      $39:
        add_hl(r.sp); // ADD HL,SP
      $3A:
        begin // LD A,(HL-)
          r.a := self.getbyte(r.hl.w);
          r.hl.w := r.hl.w - 1;
        end;
      $3B:
        r.sp := r.sp - 1; // dec sp
      $3C:
        r.a := inc_8bit(r.a); // inc a
      $3D:
        r.a := dec_8bit(r.a); // dec a
      $3E:
        begin // ld a,nn
          r.a := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $3F:
        begin // ccf
          r.f.c := not(r.f.c);
          r.f.n := false;
          r.f.h := false;
        end;
      $40:
        ; // LD B,B
      $41:
        r.bc.h := r.bc.l; // LD B,C
      $42:
        r.bc.h := r.de.h; // LD B,D
      $43:
        r.bc.h := r.de.l; // LD B,E
      $44:
        r.bc.h := r.hl.h; // LD B,H
      $45:
        r.bc.h := r.hl.l; // LD B,L
      $46:
        r.bc.h := self.getbyte(r.hl.w); // LD B,(HL)
      $47:
        r.bc.h := r.a; // LD B,A
      $48:
        r.bc.l := r.bc.h; // LD C,B
      $49:
        ; // LD C,C
      $4A:
        r.bc.l := r.de.h; // LD C,D
      $4B:
        r.bc.l := r.de.l; // LD C,E
      $4C:
        r.bc.l := r.hl.h; // LD C,H
      $4D:
        r.bc.l := r.hl.l; // LD C,L
      $4E:
        r.bc.l := self.getbyte(r.hl.w); // LD C,(HL)
      $4F:
        r.bc.l := r.a; // LD C,A
      $50:
        r.de.h := r.bc.h; // LD D,B
      $51:
        r.de.h := r.bc.l; // LD D,C
      $52:
        ; // LD D,D
      $53:
        r.de.h := r.de.l; // LD D,E
      $54:
        r.de.h := r.hl.h; // LD D,H
      $55:
        r.de.h := r.hl.l; // LD D,L
      $56:
        r.de.h := self.getbyte(r.hl.w); // LD D,(HL)
      $57:
        r.de.h := r.a; // LD D,A
      $58:
        r.de.l := r.bc.h; // LD E,B
      $59:
        r.de.l := r.bc.l; // LD E,C
      $5A:
        r.de.l := r.de.h; // LD E,D
      $5B:
        ; // LD E,E
      $5C:
        r.de.l := r.hl.h; // LD E,H
      $5D:
        r.de.l := r.hl.l; // LD E,L
      $5E:
        r.de.l := self.getbyte(r.hl.w); // LD E,(HL)
      $5F:
        r.de.l := r.a; // LD E,A
      $60:
        r.hl.h := r.bc.h; // LD H,B
      $61:
        r.hl.h := r.bc.l; // LD H,C
      $62:
        r.hl.h := r.de.h; // LD H,D
      $63:
        r.hl.h := r.de.l; // LD H,E
      $64:
        ; // LD H,H
      $65:
        r.hl.h := r.hl.l; // LD H,L
      $66:
        r.hl.h := self.getbyte(r.hl.w); // LD H,(HL)
      $67:
        r.hl.h := r.a; // LD H,A
      $68:
        r.hl.l := r.bc.h; // LD L,B
      $69:
        r.hl.l := r.bc.l; // LD L,C
      $6A:
        r.hl.l := r.de.h; // LD L,D
      $6B:
        r.hl.l := r.de.l; // LD L,E
      $6C:
        r.hl.l := r.hl.h; // LD L,H
      $6D:
        ; // LD L,L
      $6E:
        r.hl.l := self.getbyte(r.hl.w); // LD L,(HL)
      $6F:
        r.hl.l := r.a; // LD L,A
      $70:
        self.putbyte(r.hl.w, r.bc.h); // LD (HL),B
      $71:
        self.putbyte(r.hl.w, r.bc.l); // LD (HL),C
      $72:
        self.putbyte(r.hl.w, r.de.h); // LD (HL),D
      $73:
        self.putbyte(r.hl.w, r.de.l); // LD (HL),E
      $74:
        self.putbyte(r.hl.w, r.hl.h); // LD (HL),H
      $75:
        self.putbyte(r.hl.w, r.hl.l); // LD (HL),L
      $76:
        self.halt := true; // halt
      $77:
        self.putbyte(r.hl.w, r.a); // LD (HL),A
      $78:
        r.a := r.bc.h; // LD A,B
      $79:
        r.a := r.bc.l; // LD A,C
      $7A:
        r.a := r.de.h; // LD A,D
      $7B:
        r.a := r.de.l; // LD A,E
      $7C:
        r.a := r.hl.h; // LD A,H
      $7D:
        r.a := r.hl.l; // LD A,L
      $7E:
        r.a := self.getbyte(r.hl.w); // LD A,(HL)
      $7F:
        ; // LD A,A
      $80:
        add_a(r.bc.h); // ADD A,B
      $81:
        add_a(r.bc.l); // ADD A,C
      $82:
        add_a(r.de.h); // ADD A,D
      $83:
        add_a(r.de.l); // ADD A,E
      $84:
        add_a(r.hl.h); // ADD A,H
      $85:
        add_a(r.hl.l); // ADD A,L
      $86:
        add_a(self.getbyte(r.hl.w)); // ADD A,(HL)
      $87:
        add_a(r.a); // ADD A,A
      $88:
        addc_a(r.bc.h); // ADDC A,B
      $89:
        addc_a(r.bc.l); // ADDC A,C
      $8A:
        addc_a(r.de.h); // ADDC A,D
      $8B:
        addc_a(r.de.l); // ADDC A,E
      $8C:
        addc_a(r.hl.h); // ADDC A,H
      $8D:
        addc_a(r.hl.l); // ADDC A,L
      $8E:
        addc_a(self.getbyte(r.hl.w)); // ADDC A,(HL)
      $8F:
        addc_a(r.a); // ADD A,A
      $90:
        sub_a(r.bc.h); // SUB A,B
      $91:
        sub_a(r.bc.l); // SUB A,C
      $92:
        sub_a(r.de.h); // SUB A,D
      $93:
        sub_a(r.de.l); // SUB A,E
      $94:
        sub_a(r.hl.h); // SUB A,H
      $95:
        sub_a(r.hl.l); // SUB A,L
      $96:
        sub_a(self.getbyte(r.hl.w)); // SUB A,(HL)
      $97:
        sub_a(r.a); // SUB A,A
      $98:
        sbc_a(r.bc.h); // SBC A,B
      $99:
        sbc_a(r.bc.l); // SBC A,C
      $9A:
        sbc_a(r.de.h); // SBC A,D
      $9B:
        sbc_a(r.de.l); // SBC A,E
      $9C:
        sbc_a(r.hl.h); // SBC A,H
      $9D:
        sbc_a(r.hl.l); // SBC A,L
      $9E:
        sbc_a(self.getbyte(r.hl.w)); // SBC A,(HL)
      $9F:
        sbc_a(r.a); // SBC A,A
      $A0:
        and_a(r.bc.h); // AND A,B
      $A1:
        and_a(r.bc.l); // AND A,C
      $A2:
        and_a(r.de.h); // AND A,D
      $A3:
        and_a(r.de.l); // AND A,E
      $A4:
        and_a(r.hl.h); // AND A,H
      $A5:
        and_a(r.hl.l); // AND A,L
      $A6:
        and_a(self.getbyte(r.hl.w)); // AND A,(HL)
      $A7:
        and_a(r.a); // AND A,A
      $A8:
        xor_a(r.bc.h); // XOR A,B
      $A9:
        xor_a(r.bc.l); // XOR A,C
      $AA:
        xor_a(r.de.h); // XOR A,D
      $AB:
        xor_a(r.de.l); // XOR A,E
      $AC:
        xor_a(r.hl.h); // XOR A,H
      $AD:
        xor_a(r.hl.l); // XOR A,L
      $AE:
        xor_a(self.getbyte(r.hl.w)); // XOR A,(HL)
      $AF:
        xor_a(r.a); // Xor a,a
      $B0:
        or_a(r.bc.h); // OR A,B
      $B1:
        or_a(r.bc.l); // OR A,C
      $B2:
        or_a(r.de.h); // OR A,D
      $B3:
        or_a(r.de.l); // OR A,E
      $B4:
        or_a(r.hl.h); // OR A,H
      $B5:
        or_a(r.hl.l); // OR A,L
      $B6:
        or_a(self.getbyte(r.hl.w)); // OR A,(HL)
      $B7:
        or_a(r.a); // OR A,A
      $B8:
        cmp_a(r.bc.h); // CP A,B
      $B9:
        cmp_a(r.bc.l); // CP A,C
      $BA:
        cmp_a(r.de.h); // CP A,D
      $BB:
        cmp_a(r.de.l); // CP A,E
      $BC:
        cmp_a(r.hl.h); // CP A,H
      $BD:
        cmp_a(r.hl.l); // CP A,L
      $BE:
        cmp_a(self.getbyte(r.hl.w)); // CP A,(HL)
      $BF:
        cmp_a(r.a); // CP A,A
      $C0:
        if not(r.f.z) then
        begin // RET NZ
          r.pc := read_word(r.sp);
          r.sp := r.sp + 2;
          self.estados_demas := self.estados_demas + 12;
        end;
      $C1:
        begin // POP bc
          r.bc.w := read_word(r.sp);
          r.sp := r.sp + 2;
        end;
      $C2:
        if not(r.f.z) then
        begin // JP NZ,nnnnn
          r.pc := read_word(r.pc);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $C3:
        r.pc := read_word(r.pc); // JP nnnn
      $C4:
        if not(r.f.z) then
        begin // call nz,nnnn
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := tempw;
          self.estados_demas := self.estados_demas + 12;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $C5:
        begin // PUSH BC
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.bc.l);
          self.putbyte(r.sp + 1, r.bc.h);
        end;
      $C6:
        begin // ADD A,nn
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          add_a(tempb);
        end;
      $C7:
        begin // RST 0
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $0;
        end;
      $C8:
        if (r.f.z) then
        begin // RET Z
          r.pc := read_word(r.sp);
          r.sp := r.sp + 2;
          self.estados_demas := self.estados_demas + 12;
        end;
      $C9:
        begin // RET */
          r.pc := read_word(r.sp);
          r.sp := r.sp + 2;
        end;
      $CA:
        if r.f.z then
        begin // jmp z,nnnn
          r.pc := read_word(r.pc);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $CB:
        begin // prefijo CB
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          self.estados_demas := self.estados_demas + gb_cb_t[tempb];
          case tempb of
            $00:
              r.bc.h := rlc_8bit(r.bc.h); // RLC B
            $01:
              r.bc.l := rlc_8bit(r.bc.l); // RLC C
            $02:
              r.de.h := rlc_8bit(r.de.h); // RLC D
            $03:
              r.de.l := rlc_8bit(r.de.l); // RLC E
            $04:
              r.hl.h := rlc_8bit(r.hl.h); // RLC H
            $05:
              r.hl.l := rlc_8bit(r.hl.l); // RLC L
            $06:
              begin // RLC (HL)
                tempb2 := rlc_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $07:
              r.a := rlc_8bit(r.a); // RLC A
            $08:
              r.bc.h := rrc_8bit(r.bc.h); // RRC B
            $09:
              r.bc.l := rrc_8bit(r.bc.l); // RRC C
            $0A:
              r.de.h := rrc_8bit(r.de.h); // RRC D
            $0B:
              r.de.l := rrc_8bit(r.de.l); // RRC E
            $0C:
              r.hl.h := rrc_8bit(r.hl.h); // RRC H
            $0D:
              r.hl.l := rrc_8bit(r.hl.l); // RRC L
            $0E:
              begin // RRC (HL)
                tempb2 := rrc_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $0F:
              r.a := rrc_8bit(r.a); // RRC A
            $10:
              r.bc.h := rl_8bit(r.bc.h); // RL B
            $11:
              r.bc.l := rl_8bit(r.bc.l); // RL C
            $12:
              r.de.h := rl_8bit(r.de.h); // RL D
            $13:
              r.de.l := rl_8bit(r.de.l); // RL E
            $14:
              r.hl.h := rl_8bit(r.hl.h); // RL H
            $15:
              r.hl.l := rl_8bit(r.hl.l); // RL L
            $16:
              begin // RL (HL)
                tempb2 := rl_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $17:
              r.a := rl_8bit(r.a); // RL A
            $18:
              r.bc.h := rr_8bit(r.bc.h); // RR B
            $19:
              r.bc.l := rr_8bit(r.bc.l); // RR C
            $1A:
              r.de.h := rr_8bit(r.de.h); // RR D
            $1B:
              r.de.l := rr_8bit(r.de.l); // RR E
            $1C:
              r.hl.h := rr_8bit(r.hl.h); // RR H
            $1D:
              r.hl.l := rr_8bit(r.hl.l); // RR L
            $1E:
              begin // RR (HL)
                tempb2 := rr_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $1F:
              r.a := rr_8bit(r.a); // RR A
            $20:
              r.bc.h := sla_8bit(r.bc.h); // SLA B
            $21:
              r.bc.l := sla_8bit(r.bc.l); // SLA C
            $22:
              r.de.h := sla_8bit(r.de.h); // SLA D
            $23:
              r.de.l := sla_8bit(r.de.l); // SLA E
            $24:
              r.hl.h := sla_8bit(r.hl.h); // SLA H
            $25:
              r.hl.l := sla_8bit(r.hl.l); // SLA L
            $26:
              begin // SLA(HL)
                tempb2 := sla_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $27:
              r.a := sla_8bit(r.a); // SLA A
            $28:
              r.bc.h := sra_8bit(r.bc.h); // SRA B
            $29:
              r.bc.l := sra_8bit(r.bc.l); // SRA C
            $2A:
              r.de.h := sra_8bit(r.de.h); // SRA D
            $2B:
              r.de.l := sra_8bit(r.de.l); // SRA E
            $2C:
              r.hl.h := sra_8bit(r.hl.h); // SRA H
            $2D:
              r.hl.l := sra_8bit(r.hl.l); // SRA L
            $2E:
              begin // SRA(HL)
                tempb2 := sra_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $2F:
              r.a := sra_8bit(r.a); // SRA A
            $30:
              r.bc.h := swap_8bit(r.bc.h); // SWAP B
            $31:
              r.bc.l := swap_8bit(r.bc.l); // SWAP C
            $32:
              r.de.h := swap_8bit(r.de.h); // SWAP D
            $33:
              r.de.l := swap_8bit(r.de.l); // SWAP E
            $34:
              r.hl.h := swap_8bit(r.hl.h); // SWAP H
            $35:
              r.hl.l := swap_8bit(r.hl.l); // SWAP L
            $36:
              begin // SWAP (HL)
                tempb2 := swap_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $37:
              r.a := swap_8bit(r.a); // SWAP A
            $38:
              r.bc.h := srl_8bit(r.bc.h); // SRL B
            $39:
              r.bc.l := srl_8bit(r.bc.l); // SRL C
            $3A:
              r.de.h := srl_8bit(r.de.h); // SRL D
            $3B:
              r.de.l := srl_8bit(r.de.l); // SRL E
            $3C:
              r.hl.h := srl_8bit(r.hl.h); // SRL H
            $3D:
              r.hl.l := srl_8bit(r.hl.l); // SRL L
            $3E:
              begin // SRL (HL)
                tempb2 := srl_8bit(self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $3F:
              r.a := srl_8bit(r.a); // SRL A
            $40:
              bit_8bit(0, r.bc.h); // BIT 0,B
            $41:
              bit_8bit(0, r.bc.l); // BIT 0,C
            $42:
              bit_8bit(0, r.de.h); // BIT 0,D
            $43:
              bit_8bit(0, r.de.l); // BIT 0,E
            $44:
              bit_8bit(0, r.hl.h); // BIT 0,H
            $45:
              bit_8bit(0, r.hl.l); // BIT 0,L
            $46:
              bit_8bit(0, self.getbyte(r.hl.w)); // BIT 0,(HL)
            $47:
              bit_8bit(0, r.a); // BIT 0,A
            $48:
              bit_8bit(1, r.bc.h); // BIT 1,B
            $49:
              bit_8bit(1, r.bc.l); // BIT 1,C
            $4A:
              bit_8bit(1, r.de.h); // BIT 1,D
            $4B:
              bit_8bit(1, r.de.l); // BIT 1,E
            $4C:
              bit_8bit(1, r.hl.h); // BIT 1,H
            $4D:
              bit_8bit(1, r.hl.l); // BIT 1,L
            $4E:
              bit_8bit(1, self.getbyte(r.hl.w)); // BIT 1,(HL)
            $4F:
              bit_8bit(1, r.a); // BIT 1,A
            $50:
              bit_8bit(2, r.bc.h); // BIT 2,B
            $51:
              bit_8bit(2, r.bc.l); // BIT 2,C
            $52:
              bit_8bit(2, r.de.h); // BIT 2,D
            $53:
              bit_8bit(2, r.de.l); // BIT 2,E
            $54:
              bit_8bit(2, r.hl.h); // BIT 2,H
            $55:
              bit_8bit(2, r.hl.l); // BIT 2,L
            $56:
              bit_8bit(2, self.getbyte(r.hl.w)); // BIT 2,(hl)
            $57:
              bit_8bit(2, r.a); // BIT 2,A
            $58:
              bit_8bit(3, r.bc.h); // BIT 3,B
            $59:
              bit_8bit(3, r.bc.l); // BIT 3,C
            $5A:
              bit_8bit(3, r.de.h); // BIT 3,D
            $5B:
              bit_8bit(3, r.de.l); // BIT 3,E
            $5C:
              bit_8bit(3, r.hl.h); // BIT 3,H
            $5D:
              bit_8bit(3, r.hl.l); // BIT 3,L
            $5E:
              bit_8bit(3, self.getbyte(r.hl.w)); // BIT 3,(hl)
            $5F:
              bit_8bit(3, r.a); // BIT 3,A
            $60:
              bit_8bit(4, r.bc.h); // BIT 4,B
            $61:
              bit_8bit(4, r.bc.l); // BIT 4,C
            $62:
              bit_8bit(4, r.de.h); // BIT 4,D
            $63:
              bit_8bit(4, r.de.l); // BIT 4,E
            $64:
              bit_8bit(4, r.hl.h); // BIT 4,H
            $65:
              bit_8bit(4, r.hl.l); // BIT 4,L
            $66:
              bit_8bit(4, self.getbyte(r.hl.w)); // BIT 4,(hl)
            $67:
              bit_8bit(4, r.a); // BIT 4,A
            $68:
              bit_8bit(5, r.bc.h); // BIT 5,B
            $69:
              bit_8bit(5, r.bc.l); // BIT 5,C
            $6A:
              bit_8bit(5, r.de.h); // BIT 5,D
            $6B:
              bit_8bit(5, r.de.l); // BIT 5,E
            $6C:
              bit_8bit(5, r.hl.h); // BIT 5,H
            $6D:
              bit_8bit(5, r.hl.l); // BIT 5,L
            $6E:
              bit_8bit(5, self.getbyte(r.hl.w)); // BIT 5,(hl)
            $6F:
              bit_8bit(5, r.a); // BIT 5,A
            $70:
              bit_8bit(6, r.bc.h); // BIT 6,B
            $71:
              bit_8bit(6, r.bc.l); // BIT 6,C
            $72:
              bit_8bit(6, r.de.h); // BIT 6,D
            $73:
              bit_8bit(6, r.de.l); // BIT 6,E
            $74:
              bit_8bit(6, r.hl.h); // BIT 6,H
            $75:
              bit_8bit(6, r.hl.l); // BIT 6,L
            $76:
              bit_8bit(6, self.getbyte(r.hl.w)); // BIT 6,(hl)
            $77:
              bit_8bit(6, r.a); // BIT 6,A
            $78:
              bit_8bit(7, r.bc.h); // BIT 7,B
            $79:
              bit_8bit(7, r.bc.l); // BIT 7,C
            $7A:
              bit_8bit(7, r.de.h); // BIT 7,D
            $7B:
              bit_8bit(7, r.de.l); // BIT 7,E
            $7C:
              bit_8bit(7, r.hl.h); // BIT 7,H
            $7D:
              bit_8bit(7, r.hl.l); // BIT 7,L
            $7E:
              bit_8bit(7, self.getbyte(r.hl.w)); // BIT 7,(hl)
            $7F:
              bit_8bit(7, r.a); // BIT 7,A
            $80:
              r.bc.h := res_8bit(0, r.bc.h); // RES 0,B
            $81:
              r.bc.l := res_8bit(0, r.bc.l); // RES 0,C
            $82:
              r.de.h := res_8bit(0, r.de.h); // RES 0,D
            $83:
              r.de.l := res_8bit(0, r.de.l); // RES 0,E
            $84:
              r.hl.h := res_8bit(0, r.hl.h); // RES 0,H
            $85:
              r.hl.l := res_8bit(0, r.hl.l); // RES 0,L
            $86:
              begin
                tempb2 := res_8bit(0, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $87:
              r.a := res_8bit(0, r.a); // RES 0,A
            $88:
              r.bc.h := res_8bit(1, r.bc.h); // RES 1,B
            $89:
              r.bc.l := res_8bit(1, r.bc.l); // RES 1,C
            $8A:
              r.de.h := res_8bit(1, r.de.h); // RES 1,D
            $8B:
              r.de.l := res_8bit(1, r.de.l); // RES 1,E
            $8C:
              r.hl.h := res_8bit(1, r.hl.h); // RES 1,H
            $8D:
              r.hl.l := res_8bit(1, r.hl.l); // RES 1,L
            $8E:
              begin
                tempb2 := res_8bit(1, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $8F:
              r.a := res_8bit(1, r.a); // RES 1,A
            $90:
              r.bc.h := res_8bit(2, r.bc.h); // RES 2,B
            $91:
              r.bc.l := res_8bit(2, r.bc.l); // RES 2,C
            $92:
              r.de.h := res_8bit(2, r.de.h); // RES 2,D
            $93:
              r.de.l := res_8bit(2, r.de.l); // RES 2,E
            $94:
              r.hl.h := res_8bit(2, r.hl.h); // RES 2,H
            $95:
              r.hl.l := res_8bit(2, r.hl.l); // RES 2,L
            $96:
              begin // RES 2,(hl)
                tempb2 := res_8bit(2, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $97:
              r.a := res_8bit(2, r.a); // RES 2,A
            $98:
              r.bc.h := res_8bit(3, r.bc.h); // RES 3,B
            $99:
              r.bc.l := res_8bit(3, r.bc.l); // RES 3,C
            $9A:
              r.de.h := res_8bit(3, r.de.h); // RES 3,D
            $9B:
              r.de.l := res_8bit(3, r.de.l); // RES 3,E
            $9C:
              r.hl.h := res_8bit(3, r.hl.h); // RES 3,H
            $9D:
              r.hl.l := res_8bit(3, r.hl.l); // RES 3,L
            $9E:
              begin // RES 3,(hl)
                tempb2 := res_8bit(3, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $9F:
              r.a := res_8bit(3, r.a); // RES 3,A
            $A0:
              r.bc.h := res_8bit(4, r.bc.h); // RES 4,B
            $A1:
              r.bc.l := res_8bit(4, r.bc.l); // RES 4,C
            $A2:
              r.de.h := res_8bit(4, r.de.h); // RES 4,D
            $A3:
              r.de.l := res_8bit(4, r.de.l); // RES 4,E
            $A4:
              r.hl.h := res_8bit(4, r.hl.h); // RES 4,H
            $A5:
              r.hl.l := res_8bit(4, r.hl.l); // RES 4,L
            $A6:
              begin // RES 4,(hl)
                tempb2 := res_8bit(4, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $A7:
              r.a := res_8bit(4, r.a); // RES 4,A
            $A8:
              r.bc.h := res_8bit(5, r.bc.h); // RES 5,B
            $A9:
              r.bc.l := res_8bit(5, r.bc.l); // RES 5,C
            $AA:
              r.de.h := res_8bit(5, r.de.h); // RES 5,D
            $AB:
              r.de.l := res_8bit(5, r.de.l); // RES 5,E
            $AC:
              r.hl.h := res_8bit(5, r.hl.h); // RES 5,H
            $AD:
              r.hl.l := res_8bit(5, r.hl.l); // RES 5,L
            $AE:
              begin // RES 5,(hl)
                tempb2 := res_8bit(5, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $AF:
              r.a := res_8bit(5, r.a); // RES 5,A
            $B0:
              r.bc.h := res_8bit(6, r.bc.h); // RES 6,B
            $B1:
              r.bc.l := res_8bit(6, r.bc.l); // RES 6,C
            $B2:
              r.de.h := res_8bit(6, r.de.h); // RES 6,D
            $B3:
              r.de.l := res_8bit(6, r.de.l); // RES 6,E
            $B4:
              r.hl.h := res_8bit(6, r.hl.h); // RES 6,H
            $B5:
              r.hl.l := res_8bit(6, r.hl.l); // RES 6,L
            $B6:
              begin // RES 6,(hl)
                tempb2 := res_8bit(6, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $B7:
              r.a := res_8bit(6, r.a); // RES 6,A
            $B8:
              r.bc.h := res_8bit(7, r.bc.h); // RES 7,B
            $B9:
              r.bc.l := res_8bit(7, r.bc.l); // RES 7,C
            $BA:
              r.de.h := res_8bit(7, r.de.h); // RES 7,D
            $BB:
              r.de.l := res_8bit(7, r.de.l); // RES 7,E
            $BC:
              r.hl.h := res_8bit(7, r.hl.h); // RES 7,H
            $BD:
              r.hl.l := res_8bit(7, r.hl.l); // RES 7,L
            $BE:
              begin // RES 7,(hl)
                tempb2 := res_8bit(7, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $BF:
              r.a := res_8bit(7, r.a); // RES 7,A
            $C0:
              r.bc.h := set_8bit(0, r.bc.h); // set 0,B
            $C1:
              r.bc.l := set_8bit(0, r.bc.l); // set 0,C
            $C2:
              r.de.h := set_8bit(0, r.de.h); // set 0,D
            $C3:
              r.de.l := set_8bit(0, r.de.l); // set 0,E
            $C4:
              r.hl.h := set_8bit(0, r.hl.h); // set 0,H
            $C5:
              r.hl.l := set_8bit(0, r.hl.l); // set 0,L
            $C6:
              begin
                tempb2 := set_8bit(0, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $C7:
              r.a := set_8bit(0, r.a); // set 0,A
            $C8:
              r.bc.h := set_8bit(1, r.bc.h); // set 1,B
            $C9:
              r.bc.l := set_8bit(1, r.bc.l); // set 1,C
            $CA:
              r.de.h := set_8bit(1, r.de.h); // set 1,D
            $CB:
              r.de.l := set_8bit(1, r.de.l); // set 1,E
            $CC:
              r.hl.h := set_8bit(1, r.hl.h); // set 1,H
            $CD:
              r.hl.l := set_8bit(1, r.hl.l); // set 1,L
            $CE:
              begin
                tempb2 := set_8bit(1, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $CF:
              r.a := set_8bit(1, r.a); // set 1,A
            $D0:
              r.bc.h := set_8bit(2, r.bc.h); // set 2,B
            $D1:
              r.bc.l := set_8bit(2, r.bc.l); // set 2,C
            $D2:
              r.de.h := set_8bit(2, r.de.h); // set 2,D
            $D3:
              r.de.l := set_8bit(2, r.de.l); // set 2,E
            $D4:
              r.hl.h := set_8bit(2, r.hl.h); // set 2,H
            $D5:
              r.hl.l := set_8bit(2, r.hl.l); // set 2,L
            $D6:
              begin // set 2,(hl)
                tempb2 := set_8bit(2, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $D7:
              r.a := set_8bit(2, r.a); // set 2,A
            $D8:
              r.bc.h := set_8bit(3, r.bc.h); // set 3,B
            $D9:
              r.bc.l := set_8bit(3, r.bc.l); // set 3,C
            $DA:
              r.de.h := set_8bit(3, r.de.h); // set 3,D
            $DB:
              r.de.l := set_8bit(3, r.de.l); // set 3,E
            $DC:
              r.hl.h := set_8bit(3, r.hl.h); // set 3,H
            $DD:
              r.hl.l := set_8bit(3, r.hl.l); // set 3,L
            $DE:
              begin // set 3,(hl)
                tempb2 := set_8bit(3, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $DF:
              r.a := set_8bit(3, r.a); // set 3,A
            $E0:
              r.bc.h := set_8bit(4, r.bc.h); // set 4,B
            $E1:
              r.bc.l := set_8bit(4, r.bc.l); // set 4,C
            $E2:
              r.de.h := set_8bit(4, r.de.h); // set 4,D
            $E3:
              r.de.l := set_8bit(4, r.de.l); // set 4,E
            $E4:
              r.hl.h := set_8bit(4, r.hl.h); // set 4,H
            $E5:
              r.hl.l := set_8bit(4, r.hl.l); // set 4,L
            $E6:
              begin // set 4,(hl)
                tempb2 := set_8bit(4, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $E7:
              r.a := set_8bit(4, r.a); // set 4,A
            $E8:
              r.bc.h := set_8bit(5, r.bc.h); // set 5,B
            $E9:
              r.bc.l := set_8bit(5, r.bc.l); // set 5,C
            $EA:
              r.de.h := set_8bit(5, r.de.h); // set 5,D
            $EB:
              r.de.l := set_8bit(5, r.de.l); // set 5,E
            $EC:
              r.hl.h := set_8bit(5, r.hl.h); // set 5,H
            $ED:
              r.hl.l := set_8bit(5, r.hl.l); // set 5,L
            $EE:
              begin // set 5,(hl)
                tempb2 := set_8bit(5, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $EF:
              r.a := set_8bit(5, r.a); // set 5,A
            $F0:
              r.bc.h := set_8bit(6, r.bc.h); // set 6,B
            $F1:
              r.bc.l := set_8bit(6, r.bc.l); // set 6,C
            $F2:
              r.de.h := set_8bit(6, r.de.h); // set 6,D
            $F3:
              r.de.l := set_8bit(6, r.de.l); // set 6,E
            $F4:
              r.hl.h := set_8bit(6, r.hl.h); // set 6,H
            $F5:
              r.hl.l := set_8bit(6, r.hl.l); // set 6,L
            $F6:
              begin // set 6,(hl)
                tempb2 := set_8bit(6, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $F7:
              r.a := set_8bit(6, r.a); // set 6,A
            $F8:
              r.bc.h := set_8bit(7, r.bc.h); // set 7,B
            $F9:
              r.bc.l := set_8bit(7, r.bc.l); // set 7,C
            $FA:
              r.de.h := set_8bit(7, r.de.h); // set 7,D
            $FB:
              r.de.l := set_8bit(7, r.de.l); // set 7,E
            $FC:
              r.hl.h := set_8bit(7, r.hl.h); // set 7,H
            $FD:
              r.hl.l := set_8bit(7, r.hl.l); // set 7,L
            $FE:
              begin // set 7,(hl)
                tempb2 := set_8bit(7, self.getbyte(r.hl.w));
                self.putbyte(r.hl.w, tempb2);
              end;
            $FF:
              r.a := set_8bit(7, r.a); // set 7,A
          end;
        end;
      $CC:
        if r.f.z then
        begin // CALL Z,n16
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := tempw;
          self.estados_demas := self.estados_demas + 12;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $CD:
        begin // CALL nnnn
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := tempw;
        end;
      $CE:
        begin // ADC A,nn
          addc_a(self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $CF:
        begin // rst $8
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $8;
        end;
      $D0:
        if not(r.f.c) then
        begin // RET NC
          r.pc := read_word(r.sp);
          r.sp := r.sp + 2;
          self.estados_demas := self.estados_demas + 12;
        end;
      $D1:
        begin // POP DE
          r.de.w := read_word(r.sp);
          r.sp := r.sp + 2;
        end;
      $D2:
        if not(r.f.c) then
        begin // JP NC,nnnn
          r.pc := read_word(r.pc);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $D4:
        if not(r.f.c) then
        begin // call nc,nnnn
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := tempw;
          self.estados_demas := self.estados_demas + 12;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $D5:
        begin // PUSH DE
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.de.l);
          self.putbyte(r.sp + 1, r.de.h);
        end;
      $D6:
        begin // SUB A,nn
          sub_a(self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $D7:
        begin // RST $10
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $10;
        end;
      $D8:
        if r.f.c then
        begin // RET C
          r.pc := read_word(r.sp);
          r.sp := r.sp + 2;
          self.estados_demas := self.estados_demas + 12;
        end;
      $D9:
        begin // RETi
          r.pc := read_word(r.sp);
          r.sp := r.sp + 2;
          self.ime := true;
          self.after_ei := true;
        end;
      $DA:
        if r.f.c then
        begin // JP C,nnnn
          r.pc := read_word(r.pc);
          self.estados_demas := self.estados_demas + 4;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $DC:
        if r.f.c then
        begin // CALL C,nnnn
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := tempw;
          self.estados_demas := self.estados_demas + 12;
        end
        else
        begin
          r.pc := r.pc + 2;
        end;
      $DE:
        begin // SBC A,nn
          sbc_a(self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $DF:
        begin // rst $18
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $18;
        end;
      $E0:
        begin // ld ($FF00+nn),a
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          self.putbyte($FF00 + tempb, r.a);
        end;
      $E1:
        begin // POP HL
          r.hl.w := read_word(r.sp);
          r.sp := r.sp + 2;
        end;
      $E2:
        self.putbyte($FF00 + r.bc.l, r.a); // LD ($FF00+C),A
      $E5:
        begin // PUSH HL
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.hl.l);
          self.putbyte(r.sp + 1, r.hl.h);
        end;
      $E6:
        begin // AND A,nn
          and_a(self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $E7:
        begin // rst $20
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $20;
        end;
      $E8:
        begin // ADD SP,nn
          r.f.z := false;
          r.f.n := false;
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          r.f.c := ((r.sp and $FF) + tempb) > $FF;
          r.f.h := ((r.sp and $F) + (tempb and $F)) > $F;
          r.sp := r.sp + shortint(tempb);
        end;
      $E9:
        r.pc := r.hl.w; // jmp hl
      $EA:
        begin // ld (nnnn),a
          tempw := read_word(r.pc);
          r.pc := r.pc + 2;
          self.putbyte(tempw, r.a);
        end;
      $EE:
        begin // XOR A,nn
          xor_a(self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $EF:
        begin // rst $28
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $28;
        end;
      $F0:
        begin // ld A,($FF00+nn)
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          r.a := self.getbyte($FF00 + tempb);
        end;
      $F1:
        begin // POP af
          tempb := self.getbyte(r.sp);
          r.f.z := (tempb and $80) <> 0;
          r.f.n := (tempb and $40) <> 0;
          r.f.h := (tempb and $20) <> 0;
          r.f.c := (tempb and $10) <> 0;
          r.a := self.getbyte(r.sp + 1);
          r.sp := r.sp + 2;
        end;
      $F2:
        r.a := self.getbyte($FF00 + r.bc.l); // LD A,($FF00+C)
      $F3:
        begin // di
          self.ime := false;
          self.after_ei := false;
        end;
      $F5:
        begin // PUSH AF
          tempb := 0;
          if r.f.z then
            tempb := tempb or $80;
          if r.f.n then
            tempb := tempb or $40;
          if r.f.h then
            tempb := tempb or $20;
          if r.f.c then
            tempb := tempb or $10;
          r.sp := r.sp - 2;
          self.putbyte(r.sp, tempb);
          self.putbyte(r.sp + 1, r.a);
        end;
      $F6:
        begin // or a,nn
          or_a(self.getbyte(r.pc));
          r.pc := r.pc + 1;
        end;
      $F7:
        begin // rst $30
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $30;
        end;
      $F8:
        begin // LD HL,SP+nn
          r.f.z := false;
          r.f.n := false;
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          r.f.c := ((r.sp and $FF) + tempb) > $FF;
          r.f.h := ((r.sp and $F) + (tempb and $F)) > $F;
          r.hl.w := r.sp + shortint(tempb);
        end;
      $F9:
        r.sp := r.hl.w; // LD SP,HL
      $FA:
        begin // LD A,(nnnn)
          tempw := read_word(r.pc);
          r.a := self.getbyte(tempw);
          r.pc := r.pc + 2;
        end;
      $FB:
        begin // ei
          self.ime := true;
          self.after_ei := true;
        end;
      $FE:
        begin // cmp a,nn
          tempb := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          cmp_a(tempb);
        end;
      $FF:
        begin // rst $38
          r.sp := r.sp - 2;
          self.putbyte(r.sp, r.pc and $FF);
          self.putbyte(r.sp + 1, r.pc shr 8);
          r.pc := $38;
        end;
    else
      begin
        // MessageDlg('Instruccion desconocida LR35902. PC= ' + inttohex(oldpc, 4) + ' - ' +
        // inttohex(instruccion, 2), mtInformation, [mbOk], 0);
      end;
    end; // Del case
    tempw := gb_t[instruccion] + self.estados_demas;
    for tempb := 1 to (tempw div 4) do
    begin
      self.despues_instruccion(4);
      self.contador := self.contador + 4;
    end;
    timers.update(self.contador - old_contador, self.numero_cpu);
  end; // Del while
end;

end.
