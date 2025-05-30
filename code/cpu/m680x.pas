unit m680x;

interface

{$DEFINE DEBUG=1}

uses
  WinApi.Windows,
  main_engine,
  FMX.Dialogs,
  System.SysUtils,
  System.UITypes,
  timer_engine,
  vars_hide,
  cpu_misc;

const
  TCPU_M6801 = 1; // RAM 128b y ROM 2k internas
  TCPU_M6803 = 3; // RAM 128b y ROM 2k internas
  TCPU_M6808 = 8; // Ni ROM ni RAM, todo externo
  TCPU_HD63701Y = 10; // RAM 256b y ROM 16k internas
  TCPU_HD63701V = 11; // RAM 192b y ROM 4k internas

type
  band_m6800 = record
    h, i, n, z, v, c: boolean;
  end;

  reg_m6800 = record
    pc, sp, oldpc: word;
    cc: band_m6800;
    d: parejas680X;
    wai: boolean;
    x: word;
  end;

  preg_m6800 = ^reg_m6800;

  cpu_m6800 = class(cpu_class)
    constructor create(clock: dword; frames_div: word; tipo_cpu: byte);
    destructor Free;
  public
    procedure run(maximo: single);
    procedure reset;
    procedure change_io_calls(in_port1, in_port2, in_port3, in_port4: cpu_inport_call; out_port1, out_port2, out_port3, out_port4: cpu_outport_call);
    procedure change_iox_calls(in_port5, in_port6: cpu_inport_call; out_port5, out_port6: cpu_outport_call);
    function get_rom_addr: pbyte;
  private
    r: preg_m6800;
    internal_ram: array [0 .. $1FF] of byte;
    in_port: array [0 .. 3] of cpu_inport_call;
    in_portx: array [0 .. 1] of cpu_inport_call;
    out_port: array [0 .. 3] of cpu_outport_call;
    out_portx: array [0 .. 1] of cpu_outport_call;
    port_ddr: array [0 .. 3] of byte;
    portx_ddr: array [0 .. 1] of byte;
    port_in_data: array [0 .. 3] of byte;
    port_out_data: array [0 .. 3] of byte;
    portx_in_data: array [0 .. 1] of byte;
    portx_out_data: array [0 .. 1] of byte;
    ram_ctrl, trcsr, tdr, tcsr, tipo_cpu, latch09, pending_tcsr: byte;
    timer_next: longword;
    ctd, ocd: dparejas;
    tx: integer;
    port2_written, trcsr_read: boolean;
    estados_t: array [0 .. $FF] of byte;
    rom: array [0 .. $3FFF] of byte;
    procedure in_putbyte(direccion: word; valor: byte);
    function in_getbyte(direccion: word): byte;
    function basic_io_ports_r(direccion: byte): byte;
    procedure basic_io_ports_w(direccion, valor: byte);
    procedure putword(direccion: word; valor: word);
    function getword(direccion: word): word;
    procedure pushw(reg: word);
    function popw: word;
    procedure pushb(reg: byte);
    function popb: byte;
    procedure poner_band(valor: byte);
    function coger_band: byte;
    function call_int(dir: word): byte;
    procedure MODIFIED_counters;
    procedure check_timer_event;
    procedure write_port2;
    procedure write_port2_301;
    // Opcodes
    function neg8(valor: byte): byte;
    function com8(valor: byte): byte;
    function lsr8(valor: byte): byte;
    function asl8(valor: byte): byte;
    function ror8(valor: byte): byte;
    function rol8(valor: byte): byte;
    function dec8(valor: byte): byte;
    function inc8(valor: byte): byte;
    function asr8(valor: byte): byte;
    procedure tst8(valor: byte);
    function sub8(valor1, valor2: byte): byte;
    function sbc8(valor1, valor2: byte): byte;
    function and8(valor1, valor2: byte): byte;
    function eor8(valor1, valor2: byte): byte;
    function adc8(valor1, valor2: byte): byte;
    function or8(valor1, valor2: byte): byte;
    function add8(valor1, valor2: byte): byte;
  end;

var
  m6800_0: cpu_m6800;

implementation

const
  direc_680x: array [0 .. $FF] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    $F, 0, $F, $F, 0, 0, 0, $F, 0, 0, $F, $F, 0, 0, 0, 0, // 00
    0, 0, 0, 0, $F, $F, 0, 0, 0, 0, $F, 0, $F, $F, $F, $F, // 10
    1, $F, 1, 1, 1, $1, 1, 1, $F, $F, 1, 1, 1, 1, 1, 1, // 20
    $F, 0, 0, 0, 0, $F, 0, 0, 0, 0, 0, 0, 0, 0, $F, $F, // 30
    0, $F, $F, 0, 0, $F, 0, 0, 0, 0, 0, $F, 0, 0, $F, 0, // 40
    0, $F, $F, 0, 0, $F, 0, $F, 0, 0, 0, $F, 0, 0, $F, 0, // 50
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    4, 1, 1, $F, 6, $F, 6, $F, $F, $F, 6, 1, 6, 6, 4, 4, // 60
    $F, 1, 1, 8, 8, $F, 8, $F, 8, $F, 8, $F, 8, 8, 3, 3, // 70
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    1, 1, 1, 2, 1, 1, 1, $F, 1, 1, 1, 1, 2, 1, 2, $F, // 80
    5, 5, 5, $A, 5, 5, 5, $B, 5, 5, 5, 5, $A, $F, $F, $F, // 90
    6, 6, $F, 9, 6, $F, 6, 4, $F, $F, 6, 6, $F, 4, 9, $F, // a0
    $F, 8, $F, 7, $F, $F, 8, 3, $F, $F, 8, 8, $F, 3, $F, $F, // b0
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    1, 1, 1, 2, 1, 1, 1, $F, 1, 1, 1, 1, 2, $F, 2, $F, // c0
    5, 5, $F, $A, 5, $F, 5, $B, 5, 5, 5, 5, $A, $B, $A, $B, // d0
    $F, 4, $F, 9, 6, $F, 6, 4, 4, 4, 6, 6, 9, 4, 9, $F, // e0
    8, $F, $F, 7, $F, $F, 8, 3, $F, $F, 8, 8, 7, 3, 7, 3); // f0

  ciclos_6803: array [0 .. $FF] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    99, 2, 99, 99, 3, 3, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 99, 99, 99, 99, 2, 2, 99, 2, 99, 2, 99, 99, 99, 99, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 3, 3, 3, 3, 5, 5, 3, 10, 4, 10, 9, 12, 2, 99, 99, 2, 2, 99, 2, 2, 2, 2, 2, 99, 2, 2, 99, 2, 2, 99, 99, 2,
    2, 99, 2, 2, 2, 2, 2, 99, 2, 2, 99, 2, 6, 99, 99, 6, 6, 99, 6, 6, 6, 6, 6, 99, 6, 6, 3, 6, 6, 99, 99, 6, 6, 99, 6, 6, 6, 6, 6, 99, 6, 6, 3, 6, 2, 2, 2, 4, 2, 2, 2, 99, 2, 2, 2, 2, 4, 6, 3, 99, 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 4, 4, 4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4,
    6, 6, 5, 5, 4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 6, 6, 5, 5, 2, 2, 2, 4, 2, 2, 2, 99, 2, 2, 2, 2, 3, 99, 3, 99, 3, 3, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 6, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5);

  ciclos_63701: array [0 .. $FF] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    99, 1, 99, 99, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 99, 99, 99, 99, 1, 1, 2, 2, 4, 1, 99, 99, 99, 99, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 3, 3, 1, 1, 4, 4, 4, 5, 1, 10, 5, 7, 9, 12, 1, 99, 99, 1, 1, 99, 1, 1, 1, 1, 1, 99, 1, 1, 99, 1, 1, 99, 99, 1, 1,
    99, 1, 1, 1, 1, 1, 99, 1, 1, 99, 1, 6, 7, 7, 6, 6, 7, 6, 6, 6, 6, 6, 5, 6, 4, 3, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 4, 6, 4, 3, 5, 2, 2, 2, 3, 2, 2, 2, 99, 2, 2, 2, 2, 3, 5, 3, 99, 3, 3, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 4, 5, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5,
    5, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 5, 6, 5, 5, 2, 2, 2, 3, 2, 2, 2, 99, 2, 2, 2, 2, 3, 99, 3, 99, 3, 3, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5);

  M6800_TRCSR_RDRF = $80; // Receive Data Register Full
  M6800_TRCSR_ORFE = $40; // Over Run Framing Error
  M6800_TRCSR_TDRE = $20; // Transmit Data Register Empty
  M6800_TRCSR_RIE = $10; // Receive Interrupt Enable
  M6800_TRCSR_RE = $08; // Receive Enable
  M6800_TRCSR_TIE = $04; // Transmit Interrupt Enable
  M6800_TRCSR_TE = $02; // Transmit Enable
  M6800_TRCSR_WU = $01; // Wake Up
  TCSR_OLVL = $01;
  TCSR_IEDG = $02;
  TCSR_ETOI = $04;
  TCSR_EOCI = $08;
  TCSR_EICI = $10;
  TCSR_TOF = $20;
  TCSR_OCF = $40;
  TCSR_ICF = $80;

function cpu_m6800.neg8(valor: byte): byte;
var
  tempw: word;
begin
  tempw := -valor;
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.n := (tempw and $80) <> 0;
  r.cc.c := (tempw and $100) <> 0;
  r.cc.v := ((0 xor valor xor tempw xor (tempw shr 1)) and $80) <> 0;
  neg8 := tempw;
end;

function cpu_m6800.com8(valor: byte): byte;
var
  tempb: byte;
begin
  tempb := not(valor);
  r.cc.v := false;
  r.cc.c := true;
  r.cc.n := (tempb and $80) <> 0;
  r.cc.z := (tempb = 0);
  com8 := tempb;
end;

function cpu_m6800.lsr8(valor: byte): byte;
var
  tempb: byte;
begin
  tempb := valor shr 1;
  r.cc.n := false;
  r.cc.c := (valor and 1) <> 0;
  r.cc.z := (tempb = 0);
  r.cc.v := r.cc.n xor r.cc.c;
  lsr8 := tempb;
end;

function cpu_m6800.asl8(valor: byte): byte;
var
  tempw: word;
begin
  tempw := valor shl 1;
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.c := (tempw and $100) <> 0;
  r.cc.n := (tempw and $80) <> 0;
  r.cc.v := ((valor xor valor xor tempw xor (tempw shr 1)) and $80) <> 0;
  asl8 := tempw;
end;

function cpu_m6800.rol8(valor: byte): byte;
var
  tempw: word;
begin
  tempw := (valor shl 1) or byte(r.cc.c);
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.c := (tempw and $100) <> 0;
  r.cc.n := (tempw and $80) <> 0;
  r.cc.v := ((valor xor valor xor tempw xor (tempw shr 1)) and $80) <> 0;
  rol8 := tempw;
end;

function cpu_m6800.dec8(valor: byte): byte;
var
  tempb: byte;
begin
  tempb := valor - 1;
  r.cc.z := (tempb = 0);
  r.cc.n := (tempb and $80) <> 0;
  r.cc.v := (tempb = $7F);
  dec8 := tempb;
end;

function cpu_m6800.inc8(valor: byte): byte;
var
  tempb: byte;
begin
  tempb := valor + 1;
  r.cc.z := (tempb = 0);
  r.cc.n := (tempb and $80) <> 0;
  r.cc.v := (tempb = $80);
  inc8 := tempb;
end;

procedure cpu_m6800.tst8(valor: byte);
begin
  r.cc.v := false;
  r.cc.c := false;
  r.cc.z := (valor = 0);
  r.cc.n := (valor and $80) <> 0;
end;

function cpu_m6800.ror8(valor: byte): byte;
var
  tempb: byte;
begin
  tempb := (valor shr 1) or (byte(r.cc.c) shl 7);
  r.cc.c := (valor and $1) <> 0;
  r.cc.z := (tempb = 0);
  r.cc.n := (tempb and $80) <> 0;
  r.cc.v := r.cc.n xor r.cc.c;
  ror8 := tempb;
end;

function cpu_m6800.asr8(valor: byte): byte;
begin
  r.cc.c := (valor and $1) <> 0;
  valor := (valor shr 1);
  valor := valor or ((valor and $40) shl 1);
  r.cc.z := (valor = 0);
  r.cc.n := (valor and $80) <> 0;
  r.cc.v := r.cc.n xor r.cc.c;
  asr8 := valor;
end;

function cpu_m6800.sub8(valor1, valor2: byte): byte;
var
  tempw: word;
begin
  tempw := valor1 - valor2;
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.n := (tempw and $80) <> 0;
  r.cc.c := (tempw and $100) <> 0;
  r.cc.v := ((valor1 xor valor2 xor tempw xor (tempw shr 1)) and $80) <> 0;
  sub8 := tempw;
end;

function cpu_m6800.sbc8(valor1, valor2: byte): byte;
var
  tempw: word;
begin
  tempw := valor1 - valor2 - byte(r.cc.c);
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.n := (tempw and $80) <> 0;
  r.cc.c := (tempw and $100) <> 0;
  r.cc.v := ((valor1 xor valor2 xor tempw xor (tempw shr 1)) and $80) <> 0;
  sbc8 := tempw;
end;

function cpu_m6800.and8(valor1, valor2: byte): byte;
var
  tempb: byte;
begin
  tempb := valor1 and valor2;
  r.cc.v := false;
  r.cc.z := (tempb = 0);
  r.cc.n := (tempb and $80) <> 0;
  and8 := tempb;
end;

function cpu_m6800.eor8(valor1, valor2: byte): byte;
var
  tempb: byte;
begin
  tempb := valor1 xor valor2;
  r.cc.v := false;
  r.cc.z := (tempb = 0);
  r.cc.n := (tempb and $80) <> 0;
  eor8 := tempb;
end;

function cpu_m6800.adc8(valor1, valor2: byte): byte;
var
  tempw: word;
begin
  tempw := valor1 + valor2 + byte(r.cc.c);
  r.cc.n := (tempw and $80) <> 0;
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.c := (tempw and $100) <> 0;
  r.cc.v := ((valor1 xor valor2 xor tempw xor (tempw shr 1)) and $80) <> 0;
  r.cc.h := ((valor1 xor valor2 xor tempw) and $10) <> 0;
  adc8 := tempw;
end;

function cpu_m6800.or8(valor1, valor2: byte): byte;
var
  tempb: byte;
begin
  tempb := valor1 or valor2;
  r.cc.v := false;
  r.cc.z := (tempb = 0);
  r.cc.n := (tempb and $80) <> 0;
  or8 := tempb;
end;

function cpu_m6800.add8(valor1, valor2: byte): byte;
var
  tempw: word;
begin
  tempw := valor1 + valor2;
  r.cc.n := (tempw and $80) <> 0;
  r.cc.z := ((tempw and $FF) = 0);
  r.cc.c := (tempw and $100) <> 0;
  r.cc.v := ((valor1 xor valor2 xor tempw xor (tempw shr 1)) and $80) <> 0;
  r.cc.h := ((valor1 xor valor2 xor tempw) and $10) <> 0;
  add8 := tempw;
end;

function ret_in_ff: byte;
begin
  ret_in_ff := $FF;
end;

procedure ret_out_ff(valor: byte);
begin
end;

constructor cpu_m6800.create(clock: dword; frames_div: word; tipo_cpu: byte);
begin
  getmem(self.r, sizeof(reg_m6800));
  fillchar(self.r^, sizeof(reg_m6800), 0);
  clock := clock div 4;
  self.numero_cpu := cpu_main_init(clock);
  self.clock := clock;
  self.tipo_cpu := tipo_cpu;
  case tipo_cpu of
    TCPU_M6801, TCPU_M6803, TCPU_M6808:
      copymemory(@estados_t[0], @ciclos_6803[0], $100);
    TCPU_HD63701Y, TCPU_HD63701V:
      copymemory(@estados_t[0], @ciclos_63701[0], $100);
  else
    MessageDlg('Unknown M680X type', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0)
  end;
  self.tframes := (clock / frames_div) / machine_calls.fps_max;
  self.out_port[0] := ret_out_ff;
  self.out_port[1] := ret_out_ff;
  self.out_port[2] := ret_out_ff;
  self.out_port[3] := ret_out_ff;
  self.out_portx[0] := ret_out_ff;
  self.out_portx[1] := ret_out_ff;
  self.in_port[0] := ret_in_ff;
  self.in_port[1] := ret_in_ff;
  self.in_port[2] := ret_in_ff;
  self.in_port[3] := ret_in_ff;
  self.in_portx[0] := ret_in_ff;
  self.in_portx[1] := ret_in_ff;
end;

destructor cpu_m6800.Free;
begin
  freemem(self.r);
end;

function cpu_m6800.get_rom_addr: pbyte;
begin
  get_rom_addr := @self.rom;
end;

procedure cpu_m6800.reset;
begin
  r.pc := self.getword($FFFE);
  r.x := 0;
  r.d.a := 0;
  r.d.b := 0;
  r.sp := 0;
  r.cc.h := false;
  r.cc.n := false;
  r.cc.z := false;
  r.cc.v := false;
  r.cc.c := false;
  r.cc.i := true;
  self.change_nmi(CLEAR_LINE);
  self.change_irq(CLEAR_LINE);
  self.change_reset(CLEAR_LINE);
  r.wai := false;
  self.port_ddr[0] := 0;
  self.port_ddr[1] := 0;
  self.port_ddr[2] := 0;
  self.port_ddr[3] := 0;
  self.ram_ctrl := $40;
  self.tcsr := M6800_TRCSR_TDRE;
  self.ctd.l := 0;
  self.ocd.l := $FFFF;
  self.trcsr_read := false;
  self.pending_tcsr := 0;
  self.port2_written := false;
end;

procedure cpu_m6800.change_io_calls(in_port1, in_port2, in_port3, in_port4: cpu_inport_call; out_port1, out_port2, out_port3, out_port4: cpu_outport_call);
begin
  if @in_port1 <> nil then
    self.in_port[0] := in_port1;
  if @in_port2 <> nil then
    self.in_port[1] := in_port2;
  if @in_port3 <> nil then
    self.in_port[2] := in_port3;
  if @in_port4 <> nil then
    self.in_port[3] := in_port4;
  if @out_port1 <> nil then
    self.out_port[0] := out_port1;
  if @out_port2 <> nil then
    self.out_port[1] := out_port2;
  if @out_port3 <> nil then
    self.out_port[2] := out_port3;
  if @out_port4 <> nil then
    self.out_port[3] := out_port4;
end;

procedure cpu_m6800.change_iox_calls(in_port5, in_port6: cpu_inport_call; out_port5, out_port6: cpu_outport_call);
begin
  if @in_port5 <> nil then
    self.in_portx[0] := in_port5;
  if @in_port6 <> nil then
    self.in_portx[1] := in_port6;
  if @out_port5 <> nil then
    self.out_portx[0] := out_port5;
  if @out_port6 <> nil then
    self.out_portx[1] := out_port6;
end;

function cpu_m6800.basic_io_ports_r(direccion: byte): byte;
var
  res: byte;
begin
  res := $FF;
  case direccion of
    $0:
      res := self.port_ddr[0];
    $1:
      res := self.port_ddr[1];
    $2:
      begin // port1
        if @self.in_port[0] <> nil then
          self.port_in_data[0] := self.in_port[0];
        res := (self.port_out_data[0] and self.port_ddr[0]) or (self.port_in_data[0] and not(self.port_ddr[0]));
      end;
    $3:
      begin // port2
        if @self.in_port[1] <> nil then
          self.port_in_data[1] := self.in_port[1];
        res := (self.port_out_data[1] and self.port_ddr[1]) or (self.port_in_data[1] and not(self.port_ddr[1]));
      end;
    $4:
      res := self.port_ddr[2];
    $5:
      res := self.port_ddr[3];
    $6:
      begin // port3
        if @self.in_port[2] <> nil then
          self.port_in_data[2] := self.in_port[2];
        res := (port_out_data[2] and port_ddr[2]) or (port_in_data[2] and not(port_ddr[2]));
      end;
    $7:
      begin // port4
        if @self.in_port[3] <> nil then
          self.port_in_data[3] := self.in_port[3];
        res := (self.port_out_data[3] and self.port_ddr[3]) or (self.port_in_data[3] and not(self.port_ddr[3]));
      end;
  end;
  basic_io_ports_r := res;
end;

function cpu_m6800.in_getbyte(direccion: word): byte;
var
  res: byte;
begin
  res := $FF;
  case self.tipo_cpu of
    TCPU_M6801, TCPU_M6803:
      case direccion of
        $0 .. $7:
          res := self.basic_io_ports_r(direccion);
        $40 .. $FF:
          res := self.internal_ram[direccion];
        $100 .. $EFFF:
          res := self.getbyte(direccion);
        $F000 .. $FFFF:
          if self.tipo_cpu = TCPU_M6803 then
            res := self.getbyte(direccion)
          else
            res := self.rom[direccion and $FFF];
      end;
    TCPU_M6808:
      res := self.getbyte(direccion);
    TCPU_HD63701Y, TCPU_HD63701V:
      case direccion of
        $0 .. $7:
          res := self.basic_io_ports_r(direccion);
        $08:
          begin // tcsr_r
            res := self.tcsr;
            self.pending_tcsr := 0;
          end;
        $09:
          begin
            res := self.ctd.h0;
          end;
        $0B:
          begin // ocrh_r
            if (self.pending_tcsr and TCSR_OCF) = 0 then
              self.tcsr := self.tcsr and not(TCSR_OCF);
            res := self.ocd.h0;
          end;
        $0C:
          begin // ocrl_r
            if (self.pending_tcsr and TCSR_OCF) = 0 then
              self.tcsr := self.tcsr and not(TCSR_OCF);
            res := self.ocd.l0;
          end;
        $11:
          begin // sci_trcsr_r
            self.trcsr_read := true;
            res := self.trcsr;
          end;
        $14:
          res := self.ram_ctrl; // rcr_r
        $15:
          begin // p5_data_r
            if @self.in_portx[0] <> nil then
              self.portx_in_data[0] := self.in_portx[0];
            res := (self.portx_out_data[0] and self.portx_ddr[0]) or (self.portx_in_data[0] and not(self.portx_ddr[0]));
          end;
        $17:
          begin // p6_data_r
            if @self.in_portx[1] <> nil then
              self.portx_in_data[1] := self.in_portx[1];
            res := (self.portx_out_data[1] and self.portx_ddr[1]) or (self.portx_in_data[1] and not(self.portx_ddr[1]));
          end;
        $40 .. $1FF:
          res := self.internal_ram[direccion];
        $200 .. $BFFF:
          res := self.getbyte(direccion);
        $C000 .. $DFFF:
          if self.tipo_cpu = TCPU_HD63701Y then
            res := self.rom[direccion and $3FFF]
          else
            res := self.getbyte(direccion);
        $E000 .. $FFFF:
          if self.tipo_cpu = TCPU_HD63701Y then
            res := self.rom[direccion and $3FFF]
          else
            res := self.rom[direccion and $1FFF]
      end;
  end;
  in_getbyte := res;
end;

procedure cpu_m6800.basic_io_ports_w(direccion, valor: byte);
begin
  case direccion of
    $0:
      begin
        self.port_ddr[0] := valor;
        if @self.out_port[0] <> nil then
          self.out_port[0]((self.port_out_data[0] and self.port_ddr[0]) or (self.port_in_data[0] and not(self.port_ddr[0])));
      end;
    $1:
      begin
        self.port_ddr[1] := valor;
        if @self.out_port[1] <> nil then
          self.out_port[1]((self.port_out_data[1] and self.port_ddr[1]) or (self.port_in_data[1] and not(self.port_ddr[1])));
      end;
    $2:
      begin // port1
        self.port_out_data[0] := valor;
        if @self.out_port[0] <> nil then
          self.out_port[0]((self.port_out_data[0] and self.port_ddr[0]) or (self.port_in_data[0] and not(self.port_ddr[0])));
      end;
    $3:
      begin // port2
        self.port_out_data[1] := valor;
        if @self.out_port[1] <> nil then
          self.out_port[1]((self.port_out_data[1] and self.port_ddr[1]) or (self.port_in_data[1] and not(self.port_ddr[1])));
      end;
    $4:
      begin
        self.port_ddr[2] := valor;
        if @self.out_port[2] <> nil then
          self.out_port[2]((self.port_out_data[2] and self.port_ddr[2]) or (self.port_in_data[2] and not(self.port_ddr[2])));
      end;
    $5:
      begin
        self.port_ddr[3] := valor;
        if @self.out_port[3] <> nil then
          self.out_port[3]((self.port_out_data[3] and self.port_ddr[3]) or (self.port_in_data[3] and not(self.port_ddr[3])));
      end;
    $6:
      begin // port3
        self.port_out_data[2] := valor;
        if @self.out_port[2] <> nil then
          self.out_port[2]((self.port_out_data[2] and self.port_ddr[2]) or (self.port_in_data[2] and not(self.port_ddr[2])));
      end;
    $7:
      begin // port4
        self.port_out_data[3] := valor;
        if @self.out_port[3] <> nil then
          self.out_port[3]((self.port_out_data[3] and self.port_ddr[3]) or (self.port_in_data[3] and not(self.port_ddr[3])));
      end;
  end;
end;

procedure cpu_m6800.write_port2;
var
  data, ddr: byte;
begin
  if not(self.port2_written) then
    exit;
  data := self.port_out_data[1];
  ddr := self.port_ddr[1] and $1F;
  if ((ddr <> $1F) and (ddr <> 0)) then
    data := (self.port_out_data[1] and ddr) or (self.port_in_data[1] and not(ddr));
  if (self.trcsr and M6800_TRCSR_TE) <> 0 then
  begin
    data := (data and $EF) or (tx shl 4);
    ddr := ddr or $10;
  end;
  data := data and $1F;
  self.out_port[1](data);
end;

procedure cpu_m6800.write_port2_301;
var
  data, ddr: byte;
begin
  if not(port2_written) then
    exit;
  ddr := self.port_ddr[1];
  // if (self.tcsr2 and TCSR2_OE1)<>0 then ddr:=ddr2 or $02;
  // if (m_tcsr2 and TCSR2_OE2)<>0 then ddr:=ddr or $20;
  data := (self.port_out_data[1] and ddr) or (self.port_in_data[1] and not(ddr));
  if (self.trcsr and M6800_TRCSR_TE) <> 0 then
  begin
    data := (data and $EF) or (self.tx shl 4);
    ddr := ddr or $10;
  end;
  // if ((self.tcsr3 and $0c)<>0) then begin
  // data:=(data and $bf) or (self.tout3 shl 6);
  // ddr:=ddr or $40;
  // end;
  self.out_port[1](data);
end;

procedure cpu_m6800.in_putbyte(direccion: word; valor: byte);
begin
  case self.tipo_cpu of
    TCPU_M6801, TCPU_M6803:
      case direccion of
        $0 .. $7:
          self.basic_io_ports_w(direccion, valor);
        $40 .. $FF:
          self.internal_ram[direccion] := valor;
        $100 .. $EFFF:
          self.putbyte(direccion, valor);
        $F000 .. $FFFF:
          ; // ROM
      end;
    TCPU_M6808:
      self.putbyte(direccion, valor);
    TCPU_HD63701Y, TCPU_HD63701V:
      case direccion of
        $0, $2, $4 .. $7:
          self.basic_io_ports_w(direccion, valor);
        $01:
          if (self.port_ddr[1] <> valor) then
          begin // p2_ddr_w
            if self.tipo_cpu = TCPU_HD63701Y then
            begin
              self.port_ddr[1] := valor;
              self.write_port2_301;
            end
            else
            begin
              self.port_ddr[1] := valor;
              self.write_port2;
            end;
          end;
        $03:
          if self.tipo_cpu = TCPU_HD63701Y then
          begin // p2_data_w
            self.basic_io_ports_w(direccion, valor);
          end
          else
          begin
            self.port_out_data[1] := valor;
            self.port2_written := true;
            self.write_port2;
          end;
        $8:
          begin // tcsr_w
            self.tcsr := (valor and $1F) or (self.tcsr and $E0);
            self.pending_tcsr := self.pending_tcsr and self.tcsr;
            // modified_tcsr();
            if not(r.cc.i) then
              if ((self.tcsr and (TCSR_EOCI or TCSR_OCF)) = (TCSR_EOCI or TCSR_OCF)) then
                call_int($FFF4);
          end;
        $9:
          begin // ch_w
            self.latch09 := valor; // 6301 only
            self.ctd.wl := $FFF8;
            // TOH = CTH;
            self.MODIFIED_counters;
          end;
        $A:
          begin // ch_l 6301 only
            self.ctd.wl := (self.latch09 shl 8) + valor;
            // TOH = CTH;
            self.MODIFIED_counters;
          end;
        $B:
          begin // ocrh_w
            if self.ocd.h0 <> valor then
            begin // ocrh_w
              self.ocd.h0 := valor;
              self.MODIFIED_counters;
            end;
          end;
        $C:
          begin // ocrl_w
            if self.ocd.l0 <> valor then
            begin
              self.ocd.l0 := valor;
              self.MODIFIED_counters;
            end;
          end;
        $11:
          self.trcsr := (self.trcsr and $E0) or (valor and $1F); // sci_trcsr_w
        $13:
          begin // sci_tdr_w
            if self.trcsr_read then
            begin
              self.trcsr_read := false;
              self.trcsr := self.trcsr and not(M6800_TRCSR_TDRE);
            end;
            self.tdr := valor;
          end;
        $14:
          self.ram_ctrl := valor;
        $15:
          begin // p5_data_w
            self.portx_out_data[0] := valor;
            self.out_portx[0]((self.portx_out_data[0] and self.portx_ddr[0]) or (self.portx_in_data[0] and not(self.portx_ddr[0])));
          end;
        $16:
          if (self.portx_ddr[1] <> valor) then
          begin // p6_ddr_w
            self.portx_ddr[1] := valor;
            self.out_portx[1]((self.portx_out_data[1] and self.portx_ddr[1]) or (self.portx_in_data[1] and not(self.portx_ddr[1])));
          end;
        $17:
          begin // p6_data_w
            self.portx_out_data[1] := valor;
            self.out_portx[1]((self.portx_out_data[1] and self.portx_ddr[1]) or (self.portx_in_data[1] and not(self.portx_ddr[1])));
          end;
        $20:
          if (self.portx_ddr[0] <> valor) then
          begin // p5_ddr_w
            self.portx_ddr[0] := valor;
            self.out_portx[0]((self.portx_out_data[0] and self.portx_ddr[0]) or (self.portx_in_data[0] and not(self.portx_ddr[0])));
          end;
        $40 .. $1FF:
          self.internal_ram[direccion] := valor;
        $200 .. $BFFF:
          self.putbyte(direccion, valor);
        $C000 .. $DFFF:
          if self.tipo_cpu = TCPU_HD63701V then
            self.putbyte(direccion, valor);
        $E000 .. $FFFF:
          ; // ROM
      end;
  end;
end;

procedure cpu_m6800.putword(direccion: word; valor: word);
begin
  self.in_putbyte(direccion, valor shr 8);
  self.in_putbyte(direccion + 1, valor and $FF);
end;

function cpu_m6800.getword(direccion: word): word;
var
  valor: byte;
begin
  valor := self.in_getbyte(direccion);
  getword := (valor shl 8) + (self.in_getbyte(direccion + 1));
end;

procedure cpu_m6800.pushw(reg: word);
begin
  self.in_putbyte(r.sp, reg and $FF);
  r.sp := r.sp - 1;
  self.in_putbyte(r.sp, (reg shr 8));
  r.sp := r.sp - 1;
end;

function cpu_m6800.popw: word;
var
  temp: byte;
begin
  r.sp := r.sp + 1;
  temp := self.in_getbyte(r.sp);
  r.sp := r.sp + 1;
  popw := (temp shl 8) or self.in_getbyte(r.sp);
end;

procedure cpu_m6800.pushb(reg: byte);
begin
  self.in_putbyte(r.sp, reg);
  r.sp := r.sp - 1;
end;

function cpu_m6800.popb: byte;
begin
  r.sp := r.sp + 1;
  popb := self.in_getbyte(r.sp);
end;

procedure cpu_m6800.poner_band(valor: byte);
begin
  r.cc.c := (valor and 1) <> 0;
  r.cc.v := (valor and 2) <> 0;
  r.cc.z := (valor and 4) <> 0;
  r.cc.n := (valor and 8) <> 0;
  r.cc.i := (valor and $10) <> 0;
  r.cc.h := (valor and $20) <> 0;
end;

function cpu_m6800.coger_band: byte;
var
  temp: byte;
begin
  temp := byte(r.cc.c);
  temp := temp or (byte(r.cc.v) shl 1);
  temp := temp or (byte(r.cc.z) shl 2);
  temp := temp or (byte(r.cc.n) shl 3);
  temp := temp or (byte(r.cc.i) shl 4);
  coger_band := temp or (byte(r.cc.h) shl 5);
end;

// OCI -> fff4
// NMI -> fffc
// IRQ -> fff8
function cpu_m6800.call_int(dir: word): byte;
begin
  self.pushw(r.pc);
  self.pushw(r.x);
  self.pushb(r.d.a);
  self.pushb(r.d.b);
  self.pushb(self.coger_band);
  call_int := 12;
  r.cc.i := true;
  r.pc := self.getword(dir);
end;

procedure cpu_m6800.MODIFIED_counters;
begin
  if self.ocd.wl >= self.ctd.wl then
    self.ocd.wh := self.ctd.wh
  else
    self.ocd.wh := self.ctd.wh + 1;
  // timer_next = (OCD - r.cdt < TOD - CTD) ? OCD : TOD;
  self.timer_next := self.ocd.l;
end;

procedure cpu_m6800.check_timer_event;
begin
  // OCI
  if (self.ctd.l >= self.ocd.l) then
  begin
    self.ocd.wh := self.ocd.wh + $1; // next IRQ point
    self.pending_tcsr := self.pending_tcsr or TCSR_OCF;
    self.tcsr := self.tcsr or TCSR_OCF;
    // MODIFIED_tcsr;
    if (not(r.cc.i) and ((self.tcsr and TCSR_EOCI) <> 0)) then
      self.call_int($FFF4);
  end;
  // set next event
  // timer_next = (OCD - r.cdt < TOD - CTD) ? OCD : TOD;
  self.timer_next := self.ocd.l;
end;

procedure cpu_m6800.run(maximo: single);
var
  instruccion, numero, tempb, tempb2: byte;
  posicion, tempw, tempw2, numerow: word;
  templ: dword;
begin
  self.contador := 0;
  while self.contador < maximo do
  begin
    if (self.pedir_halt <> CLEAR_LINE) then
    begin
      tempw := trunc(maximo);
      for tempw2 := 1 to tempw do
      begin
        self.contador := self.contador + 1;
        // if @self.despues_instruccion<>nil then self.despues_instruccion(1);
        timers.update(1, self.numero_cpu);
        if self.pedir_halt = CLEAR_LINE then
          break;
      end;
      if self.pedir_halt <> CLEAR_LINE then
        exit;
    end;
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
    self.estados_demas := 0;
    self.r.oldpc := self.r.pc;
    // comprobar irq's
    if (self.pedir_nmi <> CLEAR_LINE) then
    begin
      if self.nmi_state = CLEAR_LINE then
        self.estados_demas := self.call_int($FFFC);
      if self.pedir_nmi = PULSE_LINE then
        self.pedir_nmi := CLEAR_LINE;
      if self.pedir_nmi = ASSERT_LINE then
        self.nmi_state := ASSERT_LINE;
    end
    else
    begin
      if ((self.pedir_irq <> CLEAR_LINE) and not(r.cc.i)) then
      begin
        self.pedir_irq := CLEAR_LINE;
        self.estados_demas := self.call_int($FFF8);
      end
      else
      begin
        if not(r.cc.i) then
          if ((self.tcsr and (TCSR_EOCI or TCSR_OCF)) = (TCSR_EOCI or TCSR_OCF)) then
            self.call_int($FFF4);
      end;
    end;
    // CLEANUP_COUNTERS()
    self.ocd.wh := self.ocd.wh - self.ctd.wh;
    self.ctd.wh := 0;
    // timer_next = (OCD - r.cdt < TOD - CTD) ? OCD : TOD;
    self.timer_next := self.ocd.l;
    self.opcode := true;
    instruccion := self.in_getbyte(r.pc);
    r.pc := r.pc + 1;
    self.opcode := false;
    // tipo de direccionamiento
    case direc_680x[instruccion] of
      0:
        ; // inerente
      1:
        begin // IMMBYTE
          numero := self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      2:
        begin // IMMWORD
          numerow := self.getword(r.pc);
          r.pc := r.pc + 2;
        end;
      3:
        begin // EXTENDED
          posicion := self.getword(r.pc);
          r.pc := r.pc + 2;
        end;
      4:
        begin // INDEXED
          posicion := r.x + self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      5:
        begin // DIRBYTE
          posicion := self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          numero := self.in_getbyte(posicion);
        end;
      6:
        begin // IDXBYTE
          posicion := r.x + self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          numero := self.in_getbyte(posicion);
        end;
      7:
        begin // EXTWORD
          posicion := self.getword(r.pc);
          r.pc := r.pc + 2;
          numerow := self.getword(posicion);
        end;
      8:
        begin // EXTBYTE
          posicion := self.getword(r.pc);
          r.pc := r.pc + 2;
          numero := self.in_getbyte(posicion);
        end;
      9:
        begin // IDXWORD
          posicion := r.x + self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          numerow := self.getword(posicion);
        end;
      $A:
        begin // DIRWORD
          posicion := self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          numerow := self.getword(posicion);
        end;
      $B:
        begin // DIRECT
          posicion := self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
{$IFDEF DEBUG}$F:
        MessageDlg('instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.pc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0); {$ENDIF}
    end;
    case instruccion of
      $01:
        ; // nop
      $04:
        begin // lsrd
          r.cc.n := false;
          tempw := r.d.w;
          r.cc.c := (tempw and $1) <> 0;
          tempw := tempw shr 1;
          r.cc.z := (tempw = 0);
          r.cc.v := r.cc.n xor r.cc.c;
          r.d.w := tempw;
        end;
      $05:
        begin // asld
          templ := r.d.w shl 1;
          r.cc.n := (templ and $8000) <> 0;
          r.cc.z := ((templ and $FFFF) = 0);
          r.cc.c := (templ and $10000) <> 0;
          r.cc.v := ((r.d.w xor r.d.w xor templ xor (templ shr 1)) and $8000) <> 0;
          r.d.w := templ;
        end;
      $06:
        self.poner_band(r.d.a); // tap
      $08:
        begin // inx
          r.x := r.x + 1;
          r.cc.z := (r.x = 0);
        end;
      $09:
        begin // dex
          r.x := r.x - 1;
          r.cc.z := (r.x = 0);
        end;
      $0C:
        r.cc.c := false; // clc
      $0D:
        r.cc.c := true; // sec
      $0E:
        r.cc.i := false; // cli
      $0F:
        r.cc.i := true; // sei
      $10:
        begin // sba
          tempw := r.d.a - r.d.b;
          r.cc.z := ((tempw and $FF) = 0);
          r.cc.n := (tempw and $80) <> 0;
          r.cc.c := (tempw and $100) <> 0;
          r.cc.v := ((r.d.a xor r.d.b xor tempw xor (tempw shr 1)) and $80) <> 0;
          r.d.a := tempw;
        end;
      $11:
        begin // cba
          tempw := r.d.a - r.d.b;
          r.cc.z := ((tempw and $FF) = 0);
          r.cc.n := (tempw and $80) <> 0;
          r.cc.c := (tempw and $100) <> 0;
          r.cc.v := ((r.d.a xor r.d.b xor tempw xor (tempw shr 1)) and $80) <> 0;
        end;
      $12, $13:
        r.x := r.x + self.in_getbyte(r.sp + 1); // undocumented asx1 y asx2
      $16:
        begin // tab
          r.d.b := r.d.a;
          r.cc.v := false;
          r.cc.z := (r.d.b = 0);
          r.cc.n := (r.d.b and $80) <> 0;
        end;
      $17:
        begin // tba
          r.d.a := r.d.b;
          r.cc.v := false;
          r.cc.z := (r.d.a = 0);
          r.cc.n := (r.d.a and $80) <> 0;
        end;
      $18:
        if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
        begin // XGDX 63701YO
          tempw := r.x;
          r.x := r.d.w;
          r.d.w := tempw;
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
      $19:
        begin // daa
          tempb := r.d.a and $F0; // msn
          tempb2 := r.d.a and $F; // lsn
          tempw := 0; // cf
          if ((tempb2 > $09) or r.cc.h) then
            tempw := tempw or $06;
          if ((tempb > $80) and (tempb2 > $09)) then
            tempw := tempw or $60;
          if ((tempb > $90) or r.cc.c) then
            tempw := tempw or $60;
          tempw2 := tempw + r.d.a;
          r.cc.z := (tempw2 and $FF) = 0;
          r.cc.n := (tempw2 and $80) <> 0;
          r.cc.c := r.cc.c or ((tempw2 and $100) <> 0);
          r.cc.v := false;
          r.d.a := tempw2;
        end;
      $1B:
        begin // aba
          tempw := r.d.a + r.d.b;
          r.cc.z := ((tempw and $FF) = 0);
          r.cc.c := (tempw and $100) <> 0;
          r.cc.n := (tempw and $80) <> 0;
          r.cc.v := ((r.d.a xor r.d.b xor tempw xor (tempw shr 1)) and $80) <> 0;
          r.cc.h := ((r.d.a xor r.d.b xor tempw) and $10) <> 0;
          r.d.a := tempw;
        end;
      $20:
        r.pc := r.pc + shortint(numero);
      $22:
        if not(r.cc.c or r.cc.z) then
          r.pc := r.pc + shortint(numero); // bhi
      $23:
        if (r.cc.c or r.cc.z) then
          r.pc := r.pc + shortint(numero); // bls
      $24:
        if not(r.cc.c) then
          r.pc := r.pc + shortint(numero); // bcc
      $25:
        if r.cc.c then
          r.pc := r.pc + shortint(numero); // bcs
      $26:
        if not(r.cc.z) then
          r.pc := r.pc + shortint(numero); // bne
      $27:
        if r.cc.z then
          r.pc := r.pc + shortint(numero); // beq
      $2A:
        if not(r.cc.n) then
          r.pc := r.pc + shortint(numero); // bpl
      $2B:
        if r.cc.n then
          r.pc := r.pc + shortint(numero); // bmi
      $2C:
        if not(r.cc.n xor r.cc.v) then
          r.pc := r.pc + shortint(numero); // bge
      $2D:
        if (r.cc.n xor r.cc.v) then
          r.pc := r.pc + shortint(numero); // blt
      $2E:
        if not((r.cc.n xor r.cc.v) or r.cc.z) then
          r.pc := r.pc + shortint(numero); // bgt
      $2F:
        if ((r.cc.n xor r.cc.v) or r.cc.z) then
          r.pc := r.pc + shortint(numero); // ble
      $31:
        r.sp := r.sp + 1; // ins
      $32:
        r.d.a := self.popb; // popa
      $33:
        r.d.b := self.popb; // popb
      $34:
        r.sp := r.sp - 1; // des
      $36:
        self.pushb(r.d.a); // psha
      $37:
        self.pushb(r.d.b); // pshb
      $38:
        r.x := self.popw;
      $39:
        r.pc := self.popw; // rts
      $3A:
        r.x := r.x + r.d.b; // abx
      $3B:
        begin // rti
          self.poner_band(self.popb);
          r.d.b := self.popb;
          r.d.a := self.popb;
          r.x := self.popw;
          r.pc := self.popw;
        end;
      $3C:
        self.pushw(r.x); // pshx
      $3D:
        begin // mul
          r.d.w := r.d.a * r.d.b;
          r.cc.c := (r.d.w and $80) <> 0;
        end;
      $40:
        r.d.a := self.neg8(r.d.a); // nega
      $43:
        r.d.a := self.com8(r.d.a); // coma
      $44:
        r.d.a := self.lsr8(r.d.a); // lsra
      $46:
        r.d.a := self.ror8(r.d.a); // rora
      $47:
        r.d.a := self.asr8(r.d.a); // asra
      $48:
        r.d.a := self.asl8(r.d.a); // asla
      $49:
        r.d.a := self.rol8(r.d.a); // rola
      $4A:
        r.d.a := self.dec8(r.d.a); // deca
      $4C:
        r.d.a := self.inc8(r.d.a); // inca
      $4D:
        self.tst8(r.d.a); // tsta
      $4F:
        begin // clra
          r.d.a := 0;
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $50:
        r.d.b := self.neg8(r.d.b); // negb
      $53:
        r.d.b := self.com8(r.d.b); // comb
      $54:
        r.d.b := self.lsr8(r.d.b); // lsrb
      $56:
        r.d.b := self.ror8(r.d.b); // rorb
      $58:
        r.d.b := self.asl8(r.d.b); // aslb
      $59:
        r.d.b := self.rol8(r.d.b); // rolb
      $5A:
        r.d.b := self.dec8(r.d.b); // decb
      $5C:
        r.d.b := self.inc8(r.d.b); // incb
      $5D:
        self.tst8(r.d.b); // tstb
      $5F:
        begin // clrb
          r.d.b := 0;
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $60:
        begin // neg
          tempb := self.in_getbyte(posicion);
          tempb := self.neg8(tempb);
          self.in_putbyte(posicion, tempb);
        end;
      $61:
        if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
        begin // aim_ix - HD63701
          tempw := r.x + self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          tempb := self.in_getbyte(tempw) and numero;
          r.cc.v := false;
          r.cc.z := (tempb = 0);
          r.cc.n := (tempb and $80) <> 0;
          self.in_putbyte(tempw, tempb);
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
      $62:
        if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
        begin // OIM - HD63701
          tempw := r.x + self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          tempb := self.in_getbyte(tempw) or numero;
          r.cc.v := false;
          r.cc.z := (tempb = 0);
          r.cc.n := (tempb and $80) <> 0;
          self.in_putbyte(tempw, tempb);
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
      $71:
        if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
        begin // aim - HD63701
          tempw := self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          tempb := self.in_getbyte(tempw) and numero;
          r.cc.v := false;
          r.cc.z := (tempb = 0);
          r.cc.n := (tempb and $80) <> 0;
          self.in_putbyte(tempw, tempb);
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
      $72:
        if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
        begin // oim - HD63701
          tempw := self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          tempb := self.in_getbyte(tempw) or numero;
          r.cc.v := false;
          r.cc.z := (tempb = 0);
          r.cc.n := (tempb and $80) <> 0;
          self.in_putbyte(tempw, tempb);
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
      $73:
        begin // com
          tempb := self.com8(numero);
          self.in_putbyte(posicion, tempb);
        end;
      $64, $74:
        begin // lsr
          tempb := self.lsr8(numero);
          self.in_putbyte(posicion, tempb);
        end;
      $66, $76:
        begin // ror
          tempb := self.ror8(numero);
          self.in_putbyte(posicion, tempb);
        end;
      $78:
        begin // asl
          tempb := self.asl8(numero);
          self.in_putbyte(posicion, tempb);
        end;
      $6A, $7A:
        begin // dec
          tempb := self.dec8(numero);
          self.in_putbyte(posicion, tempb);
        end;
      $6B:
        if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
        begin // TIM HD63701
          tempw := r.x + self.in_getbyte(r.pc);
          r.pc := r.pc + 1;
          tempb := self.in_getbyte(tempw) and numero;
          r.cc.v := false;
          r.cc.z := (tempb = 0);
          r.cc.n := (tempb and $80) <> 0;
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
      $6C, $7C:
        begin // inc
          tempb := self.inc8(numero);
          self.in_putbyte(posicion, tempb);
        end;
      $6D, $7D:
        self.tst8(numero); // tst
      $6E, $7E:
        r.pc := posicion; // jmp
      $6F, $7F:
        begin // clr
          self.in_putbyte(posicion, 0);
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $80, $90, $A0:
        r.d.a := self.sub8(r.d.a, numero); // suba
      $81, $91, $A1, $B1:
        self.sub8(r.d.a, numero); // cmpa
      $82, $92:
        r.d.a := self.sbc8(r.d.a, numero); // sbca
      $83, $93, $A3, $B3:
        begin // subd
          templ := r.d.w - numerow;
          r.cc.z := ((templ and $FFFF) = 0);
          r.cc.n := (templ and $8000) <> 0;
          r.cc.c := (templ and $10000) <> 0;
          r.cc.v := ((r.d.w xor numerow xor templ xor (templ shr 1)) and $8000) <> 0;
          r.d.w := templ;
        end;
      $84, $94, $A4:
        r.d.a := self.and8(r.d.a, numero); // anda
      $85, $95:
        self.and8(r.d.a, numero); // bita
      $86, $96, $A6, $B6:
        begin // lda
          r.d.a := numero;
          r.cc.v := false;
          r.cc.z := (numero = 0);
          r.cc.n := (numero and $80) <> 0;
        end;
      $97, $A7, $B7:
        begin // sta
          r.cc.v := false;
          r.cc.z := (r.d.a = 0);
          r.cc.n := (r.d.a and $80) <> 0;
          self.in_putbyte(posicion, r.d.a);
        end;
      $88, $98:
        r.d.a := self.eor8(r.d.a, numero); // eora
      $89, $99:
        r.d.a := self.adc8(r.d.a, numero); // adca
      $8A, $9A, $AA, $BA:
        r.d.a := self.or8(r.d.a, numero); // ora
      $8B, $9B, $AB, $BB:
        r.d.a := self.add8(r.d.a, numero); // adda
      $8C, $9C:
        begin // cpmx solo 6801/03
          templ := r.x - numerow;
          r.cc.z := ((templ and $FFFF) = 0);
          r.cc.n := (templ and $8000) <> 0;
          r.cc.v := ((r.x xor numerow xor templ xor (templ shr 1)) and $8000) <> 0;
          if self.tipo_cpu = TCPU_M6803 then
            r.cc.c := (templ and $10000) <> 0;
        end;
      $8D:
        begin // bsr
          self.pushw(r.pc);
          r.pc := r.pc + shortint(numero);
        end;
      $AD, $BD:
        begin // jsr
          self.pushw(r.pc);
          r.pc := posicion;
        end;
      $8E, $AE:
        begin // lds
          r.sp := numerow;
          r.cc.v := false;
          r.cc.z := (numerow = 0);
          r.cc.n := (numerow and $8000) <> 0;
        end;
      $C0, $D0, $F0:
        r.d.b := self.sub8(r.d.b, numero); // subb
      $C1, $D1, $E1:
        self.sub8(r.d.b, numero); // cmpb
      $C2:
        r.d.b := self.sbc8(r.d.b, numero); // sbcb
      $C3, $D3, $E3:
        begin // addd
          templ := r.d.w + numerow;
          r.cc.z := ((templ and $FFFF) = 0);
          r.cc.n := (templ and $8000) <> 0;
          r.cc.c := (templ and $10000) <> 0;
          r.cc.v := ((r.d.w xor numerow xor templ xor (templ shr 1)) and $8000) <> 0;
          r.d.w := templ;
        end;
      $C4, $D4, $E4:
        r.d.b := self.and8(r.d.b, numero); // andb
      $C5:
        self.and8(r.d.b, numero); // bitb
      $C6, $D6, $E6, $F6:
        begin // ldb
          r.d.b := numero;
          r.cc.v := false;
          r.cc.z := (numero = 0);
          r.cc.n := (numero and $80) <> 0;
        end;
      $D7, $E7, $F7:
        begin // stb
          r.cc.v := false;
          r.cc.z := (r.d.b = 0);
          r.cc.n := (r.d.b and $80) <> 0;
          self.in_putbyte(posicion, r.d.b);
        end;
      $C8, $D8, $E8:
        r.d.b := self.eor8(r.d.b, numero); // eorb
      $C9, $D9, $E9:
        r.d.b := self.adc8(r.d.b, numero); // adcb
      $CA, $DA, $EA, $FA:
        r.d.b := self.or8(r.d.b, numero); // orb
      $CB, $DB, $EB, $FB:
        r.d.b := self.add8(r.d.b, numero); // addb
      $CC, $DC, $EC, $FC:
        begin // ldd 6803 Only
          r.d.w := numerow;
          r.cc.v := false;
          r.cc.z := (numerow = 0);
          r.cc.n := (numerow and $8000) <> 0;
        end;
      $DD, $ED, $FD:
        begin // std
          r.cc.v := false;
          r.cc.z := (r.d.w = 0);
          r.cc.n := (r.d.w and $8000) <> 0;
          self.putword(posicion, r.d.w);
        end;
      $CE, $DE, $EE, $FE:
        begin // ldx
          r.x := numerow;
          r.cc.v := false;
          r.cc.z := (numerow = 0);
          r.cc.n := (numerow and $8000) <> 0;
        end;
      $DF, $FF:
        begin // stx
          r.cc.v := false;
          r.cc.z := (r.x = 0);
          r.cc.n := (r.x and $8000) <> 0;
          self.putword(posicion, r.x);
        end;
      $F3:
        if self.tipo_cpu <> TCPU_M6808 then
        begin // addd
          templ := r.d.w + numerow;
          r.cc.z := ((templ and $FFFF) = 0);
          r.cc.n := (templ and $8000) <> 0;
          r.cc.c := (templ and $10000) <> 0;
          r.cc.v := ((r.d.w xor numerow xor templ xor (templ shr 1)) and $8000) <> 0;
          r.d.w := templ;
        end
        else
          MessageDlg('Instruction M6800 ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.oldpc, 10) + ' - ' + inttohex(r.oldpc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
    end; // del case
    tempb := estados_t[instruccion] + self.estados_demas;
    self.contador := self.contador + tempb;
    timers.update(tempb, self.numero_cpu);
    if ((self.tipo_cpu = TCPU_HD63701Y) or (self.tipo_cpu = TCPU_HD63701V)) then
    begin
      self.ctd.l := self.ctd.l + tempb;
      if (self.ctd.l >= self.timer_next) then
        self.check_timer_event;
    end;
  end; // del while!
end;

end.
