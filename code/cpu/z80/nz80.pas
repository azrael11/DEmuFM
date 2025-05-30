unit nz80;

interface

uses
  WinApi.Windows,
  System.UITypes,
  cpu_misc,
  z80daisy,
  timer_engine,
  FMX.Dialogs,
  vars_hide,
  main_engine;

const
  paridad: array [0 .. 255] of boolean = (True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False, False, True, True, False,
    True, False, False, True, True, False, False, True, False, True, True, False, True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, False, True, True, False, True, False, False, True, True, False, False, True, False, True,
    True, False, True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, False, True, True, False, True, False, False, True,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False, True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, True, False,
    False, True, False, True, True, False, False, True, True, False, True, False, False, True, False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False, True, False, False, True, False, True, True, False, False, True, True, False,
    True, False, False, True, False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False, True, False, False, True, False, True,
    True, False, False, True, True, False, True, False, False, True);

type
  band_z80 = record
    c, n, p_v, bit3, h, bit5, z, s: boolean;
  end;

  tdespues_instruccion = procedure(estados_t: word);
  type_raised = procedure;
  type_m1_raise = procedure(opcode: byte);
  type_external_vector = function: byte;

  nreg_z80 = packed record
    ppc, pc, sp: word;
    bc, de, hl: parejas;
    bc2, de2, hl2: parejas;
    wz: word;
    ix, iy: parejas;
    iff1, iff2, halt_opcode: boolean;
    a, a2, i, r: byte;
    f, f2: band_z80;
    im: byte;
  end;

  npreg_z80 = ^nreg_z80;

  cpu_z80 = class(cpu_class)
    constructor create(clock: dword; frames_div: single);
    destructor free;
  public
    procedure change_irq_vector(estado: byte; irq_vector: byte);
    procedure reset;
    procedure run(maximo: single);
    procedure change_timmings(z80t_set, z80t_cb_set, z80t_dd_set, z80t_ddcb_set, z80t_ed_set, z80t_ex_set: pbyte);
    procedure change_io_calls(in_port: tgetbyte; out_port: tputbyte);
    procedure change_misc_calls(despues_instruccion: tdespues_instruccion; raised_z80: type_raised = nil; m1_raised: type_m1_raise = nil; irq_vector_cb: type_external_vector = nil);
    function get_safe_pc: word;
    function get_internal_r: npreg_z80;
    function save_snapshot(data: pbyte): word;
    procedure load_snapshot(data: pbyte);
    procedure enable_daisy;
  protected
    after_ei, daisy: boolean;
    r: npreg_z80;
    in_port: tgetbyte;
    out_port: tputbyte;
    irq_vector: byte;
    // pila
    procedure push_sp(reg: word);
    function pop_sp: word;
    // opcodes
    procedure and_a(valor: byte);
    procedure or_a(valor: byte);
    procedure xor_a(valor: byte);
    procedure cp_a(valor: byte);
    procedure sra_8(reg: pbyte);
    procedure sla_8(reg: pbyte);
    procedure sll_8(reg: pbyte);
    procedure srl_8(reg: pbyte);
    procedure rlc_8(reg: pbyte);
    procedure rr_8(reg: pbyte);
    procedure rrc_8(reg: pbyte);
    procedure rl_8(reg: pbyte);
    function dec_8(valor: byte): byte;
    function inc_8(valor: byte): byte;
    procedure add_8(valor: byte);
    procedure adc_8(valor: byte);
    procedure sub_8(valor: byte);
    procedure sbc_8(valor: byte);
    procedure bit_8(bit, valor: byte);
    procedure bit_7(valor: byte);
    function add_16(valor1, valor2: word): word;
    function adc_hl(valor: word): word;
    function sbc_hl(valor: word): word;
  private
    z80t, z80t_cb, z80t_dd, z80t_ddcb, z80t_ed, z80t_ex: array [0 .. 255] of byte;
    raised_z80: type_raised;
    m1_raised: type_m1_raise;
    irq_vector_cb: type_external_vector;
    function call_nmi: byte;
    function call_irq: byte;
    // resto de opcodes
    procedure exec_cb;
    procedure exec_dd_fd(tipo: boolean);
    procedure exec_dd_cb(tipo: boolean);
    procedure exec_ed;
  end;

var
  z80_0, z80_1, z80_2: cpu_z80;

implementation

const
  z80t_m: array [0 .. 255] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    4, 10, 7, 6, 4, 4, 7, 4, 4, 11, 7, 6, 4, 4, 7, 4, 8, 10, 7, 6, 4, 4, 7, 4, 12, 11, 7, 6, 4, 4, 7, 4, 7, 10, 16, 6, 4, 4, 7, 4, 7, 11, 16, 6, 4, 4, 7, 4, 7, 10, 13, 6, 11, 11, 10, 4, 7, 11, 13, 6, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7,
    4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 7, 7, 7, 7, 7, 7, 4, 7, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4,
    4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 5, 10, 10, 10, 10, 11, 7, 11, 5, 10, 10, 4, 10, 17, 7, 11, 5, 10, 10, 11, 10, 11, 7, 11, 5, 4, 10, 11, 10, 4, 7, 11, // D0
    5, 10, 10, 19, 10, 11, 7, 11, 5, 4, 10, 4, 10, 4, 7, 11, // E0
    5, 10, 10, 4, 10, 11, 7, 11, 5, 6, 10, 4, 10, 4, 7, 11); // F0
  z80t_cb_m: array [0 .. 255] of byte = (
    // 0 1 2 3 4 5  6 7 8 9 a b c d  e f
    4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4,
    4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4,
    4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4, 4, 4, 4, 4, 4, 4, 11, 4);
  z80t_dd_m: array [0 .. 255] of byte = ( // cb_xy
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    4, 10, 7, 6, 4, 4, 7, 4, 4, 11, 7, 6, 4, 4, 7, 4, 8, 10, 7, 6, 4, 4, 7, 4, 12, 11, 7, 6, 4, 4, 7, 4, 7, 10, 16, 6, 4, 4, 7, 4, 7, 11, 16, 6, 4, 4, 7, 4, 7, 10, 13, 6, 19, 19, 15, 4, 7, 11, 13, 6, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4,
    15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 15, 15, 15, 15, 15, 15, 4, 15, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4,
    4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 4, 4, 4, 4, 4, 4, 15, 4, 5, 10, 10, 10, 10, 11, 7, 11, 5, 10, 10, 7, 10, 17, 7, 11, 5, 10, 10, 11, 10, 11, 7, 11, 5, 4, 10, 11, 10, 4, 7, 11, 5, 10, 10, 19, 10, 11, 7, 11, 5, 4, 10, 4, 10, 4, 7, 11, 5, 10, 10, 4, 10, 11, 7, 11, 5, 6, 10,
    4, 10, 4, 7, 11);
  z80t_ddcb_m: array [0 .. 255] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12);
  z80t_ed_m: array [0 .. 255] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 8, 8, 11, 16, 4, 10, 4, 5, 8, 8, 11, 16, 4, 10, 4, 5, 8, 8, 11, 16, 4, 10, 4, 5, 8,
    8, 11, 16, 4, 10, 4, 5, 8, 8, 11, 16, 4, 10, 4, 14, 8, 8, 11, 16, 4, 10, 4, 14, 8, 8, 11, 16, 4, 10, 4, 4, 8, 8, 11, 16, 4, 10, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 12, 12, 12, 12, 4, 4, 4, 4, 12, 12, 12, 12, 4,
    4, 4, 4, 12, 12, 12, 12, 4, 4, 4, 4, 12, 12, 12, 12, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4); // F0
  z80t_ex_m: array [0 .. 255] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 5, 5, 5, 5, 0, 0, 0, 0, 5, 5, 5, 5, 0, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0);

procedure cpu_z80.change_irq_vector(estado: byte; irq_vector: byte);
begin
  self.pedir_irq := estado;
  self.irq_vector := irq_vector;
end;

procedure cpu_z80.and_a(valor: byte);
begin
  r.a := r.a and valor;
  r.f.s := (r.a and $80) <> 0;
  r.f.z := (r.a = 0);
  r.f.bit5 := (r.a and $20) <> 0;
  r.f.h := True;
  r.f.bit3 := (r.a and 8) <> 0;
  r.f.p_v := paridad[r.a];
  r.f.n := False;
  r.f.c := False;
end;

procedure cpu_z80.or_a(valor: byte);
begin
  r.a := r.a or valor;
  r.f.s := (r.a and $80) <> 0;
  r.f.z := (r.a = 0);
  r.f.bit5 := (r.a and $20) <> 0;
  r.f.h := False;
  r.f.bit3 := (r.a and 8) <> 0;
  r.f.p_v := paridad[r.a];
  r.f.n := False;
  r.f.c := False;
end;

procedure cpu_z80.xor_a(valor: byte);
begin
  r.a := r.a xor valor;
  r.f.s := (r.a and $80) <> 0;
  r.f.z := (r.a = 0);
  r.f.bit5 := (r.a and $20) <> 0;
  r.f.h := False;
  r.f.bit3 := (r.a and 8) <> 0;
  r.f.p_v := paridad[r.a];
  r.f.n := False;
  r.f.c := False;
end;

procedure cpu_z80.cp_a(valor: byte);
var
  temp: byte;
begin
  temp := r.a - valor;
  r.f.s := (temp and $80) <> 0;
  r.f.z := (temp = 0);
  r.f.bit5 := (valor and $20) <> 0;
  r.f.h := (((r.a and $F) - (valor and $F)) and $10) <> 0;
  r.f.bit3 := (valor and 8) <> 0;
  r.f.p_v := ((r.a xor valor) and (r.a xor temp) and $80) <> 0;
  r.f.n := True;
  r.f.c := (r.a - valor) < 0;
end;

procedure cpu_z80.sra_8(reg: pbyte);
begin
  r.f.c := (reg^ and $1) <> 0;
  reg^ := (reg^ shr 1) or (reg^ and $80);
  r.f.h := False;
  r.f.n := False;
  r.f.p_v := paridad[reg^];
  r.f.s := (reg^ and $80) <> 0;
  r.f.z := (reg^ = 0);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
end;

procedure cpu_z80.sla_8(reg: pbyte);
begin
  r.f.c := (reg^ and $80) <> 0;
  reg^ := reg^ shl 1;
  r.f.h := False;
  r.f.n := False;
  r.f.p_v := paridad[reg^];
  r.f.s := (reg^ and $80) <> 0;
  r.f.z := (reg^ = 0);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
end;

procedure cpu_z80.sll_8(reg: pbyte);
begin
  r.f.c := (reg^ and $80) <> 0;
  reg^ := (reg^ shl 1) or 1;
  r.f.h := False;
  r.f.n := False;
  r.f.p_v := paridad[reg^];
  r.f.s := (reg^ and $80) <> 0;
  r.f.z := (reg^ = 0);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
end;

procedure cpu_z80.srl_8(reg: pbyte);
begin
  r.f.h := False;
  r.f.n := False;
  r.f.c := (reg^ and 1) <> 0;
  reg^ := reg^ shr 1;
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
  r.f.p_v := paridad[reg^];
  r.f.z := (reg^ = 0);
  r.f.s := (reg^ and $80) <> 0;
end;

procedure cpu_z80.rlc_8(reg: pbyte);
begin
  r.f.c := (reg^ and $80) <> 0;
  reg^ := (reg^ shl 1) or byte(r.f.c);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
  r.f.p_v := paridad[reg^];
  r.f.h := False;
  r.f.n := False;
  r.f.z := (reg^ = 0);
  r.f.s := (reg^ and $80) <> 0;
end;

procedure cpu_z80.rr_8(reg: pbyte);
begin
  r.f.n := r.f.c;
  r.f.c := (reg^ and 1) <> 0;
  reg^ := (reg^ shr 1) or (byte(r.f.n) shl 7);
  r.f.h := False;
  r.f.n := False;
  r.f.p_v := paridad[reg^];
  r.f.s := (reg^ and $80) <> 0;
  r.f.z := (reg^ = 0);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
end;

procedure cpu_z80.rrc_8(reg: pbyte);
begin
  r.f.c := (reg^ and $1) <> 0;
  reg^ := (reg^ shr 1) or (byte(r.f.c) shl 7);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
  r.f.p_v := paridad[reg^];
  r.f.h := False;
  r.f.n := False;
  r.f.z := (reg^ = 0);
  r.f.s := (reg^ and $80) <> 0;
end;

procedure cpu_z80.rl_8(reg: pbyte);
begin
  r.f.n := r.f.c;
  r.f.c := (reg^ and $80) <> 0;
  reg^ := (reg^ shl 1) or byte(r.f.n);
  r.f.h := False;
  r.f.n := False;
  r.f.p_v := paridad[reg^];
  r.f.s := (reg^ and $80) <> 0;
  r.f.z := (reg^ = 0);
  r.f.bit5 := (reg^ and $20) <> 0;
  r.f.bit3 := (reg^ and 8) <> 0;
end;

function cpu_z80.dec_8(valor: byte): byte;
var
  tempb: byte;
begin
  r.f.h := (((valor and $F) - 1) and $10) <> 0;
  r.f.p_v := (valor = $80);
  tempb := valor - 1;
  r.f.s := (tempb and $80) <> 0;
  r.f.z := (tempb = 0);
  r.f.bit5 := (tempb and $20) <> 0;
  r.f.bit3 := (tempb and 8) <> 0;
  r.f.n := True;
  dec_8 := tempb;
end;

function cpu_z80.inc_8(valor: byte): byte;
var
  tempb: byte;
begin
  r.f.h := (((valor and $F) + 1) and $10) <> 0;
  r.f.p_v := (valor = $7F);
  tempb := valor + 1;
  r.f.s := (tempb and $80) <> 0;
  r.f.z := (tempb = 0);
  r.f.bit5 := (tempb and $20) <> 0;
  r.f.bit3 := (tempb and 8) <> 0;
  r.f.n := False;
  inc_8 := tempb;
end;

procedure cpu_z80.add_8(valor: byte);
var
  temp: byte;
begin
  temp := r.a + valor;
  r.f.p_v := (((r.a xor not(valor)) and $FFFF) and (r.a xor temp) and $80) <> 0;
  r.f.h := (((r.a and $F) + (valor and $F)) and $10) <> 0;
  r.f.s := (temp and $80) <> 0;
  r.f.z := (temp = 0);
  r.f.bit5 := (temp and $20) <> 0;
  r.f.bit3 := (temp and 8) <> 0;
  r.f.n := False;
  r.f.c := ((r.a + valor) and $100) <> 0;
  r.a := temp;
end;

procedure cpu_z80.adc_8(valor: byte);
var
  carry, temp: byte;
begin
  carry := byte(r.f.c);
  temp := r.a + valor + carry;
  r.f.p_v := (((r.a xor not(valor)) and $FFFF) and ((r.a xor temp) and $80)) <> 0;
  r.f.h := (((r.a and $F) + (valor and $F) + carry) and $10) <> 0;
  r.f.s := (temp and $80) <> 0;
  r.f.z := (temp = 0);
  r.f.bit5 := (temp and $20) <> 0;
  r.f.bit3 := (temp and 8) <> 0;
  r.f.n := False;
  r.f.c := ((r.a + valor + carry) and $100) <> 0;
  r.a := temp;
end;

procedure cpu_z80.sub_8(valor: byte);
var
  temp: byte;
  temp2: word;
begin
  temp2 := r.a - valor;
  temp := temp2 and $FF;
  r.f.h := (((r.a and $0F) - (valor and $0F)) and $10) <> 0;
  r.f.p_v := (((r.a xor valor) and (r.a xor temp)) and $80) <> 0;
  r.f.s := (temp and $80) <> 0;
  r.f.z := (temp = 0);
  r.f.bit5 := (temp and $20) <> 0;
  r.f.bit3 := (temp and 8) <> 0;
  r.f.n := True;
  r.f.c := (temp2 and $100) <> 0;
  r.a := temp;
end;

procedure cpu_z80.sbc_8(valor: byte);
var
  carry, temp: byte;
begin
  carry := byte(r.f.c);
  temp := r.a - valor - carry;
  r.f.h := (((r.a and $0F) - (valor and $0F) - carry) and $10) <> 0;
  r.f.p_v := (((r.a xor valor) and (r.a xor temp)) and $80) <> 0;
  r.f.s := (temp and $80) <> 0;
  r.f.z := (temp = 0);
  r.f.bit5 := (temp and $20) <> 0;
  r.f.bit3 := (temp and 8) <> 0;
  r.f.n := True;
  r.f.c := ((r.a - valor - carry) and $100) <> 0;
  r.a := temp;
end;

procedure cpu_z80.bit_8(bit, valor: byte);
begin
  r.f.h := True;
  r.f.n := False;
  r.f.s := False;
  r.f.z := not((valor and (1 shl bit)) <> 0);
  r.f.p_v := r.f.z;
  r.f.bit5 := (valor and $20) <> 0;
  r.f.bit3 := (valor and $8) <> 0;
end;

procedure cpu_z80.bit_7(valor: byte);
begin
  r.f.z := not((valor and $80) <> 0);
  r.f.h := True;
  r.f.n := False;
  r.f.p_v := r.f.z;
  r.f.s := (valor and $80) <> 0;
  r.f.bit5 := (valor and $20) <> 0;
  r.f.bit3 := (valor and $8) <> 0;
end;

function cpu_z80.add_16(valor1, valor2: word): word;
var
  templ: dword;
begin
  templ := valor1 + valor2;
  r.wz := valor1 + 1;
  r.f.bit3 := (templ and $800) <> 0;
  r.f.bit5 := (templ and $2000) <> 0;
  r.f.c := (templ and $10000) <> 0;
  r.f.h := (((valor1 and $FFF) + (valor2 and $FFF)) and $1000) <> 0;
  r.f.n := False;
  add_16 := templ;
end;

function cpu_z80.adc_hl(valor: word): word;
var
  templ: dword;
  carry: byte;
begin
  carry := byte(r.f.c);
  templ := r.hl.w + valor + carry;
  r.wz := r.hl.w + 1;
  r.f.h := ((r.hl.w xor templ xor valor) and $1000) <> 0;
  r.f.n := False;
  r.f.c := (templ and $10000) <> 0;
  r.f.s := (templ and $8000) <> 0;
  r.f.bit5 := (templ and $2000) <> 0;
  r.f.bit3 := (templ and $800) <> 0;
  r.f.z := ((templ and $FFFF) = 0);
  r.f.p_v := ((valor xor r.hl.w xor $8000) and (valor xor templ) and $8000) <> 0;
  adc_hl := templ;
end;

function cpu_z80.sbc_hl(valor: word): word;
var
  carry: byte;
  templ: dword;
begin
  carry := byte(r.f.c);
  r.wz := r.hl.w + 1;
  templ := r.hl.w - valor - carry;
  r.f.h := ((r.hl.w xor templ xor valor) and $1000) <> 0;
  r.f.n := True;
  r.f.s := (templ and $8000) <> 0;
  r.f.bit3 := (templ and $800) <> 0;
  r.f.bit5 := (templ and $2000) <> 0;
  r.f.z := ((templ and $FFFF) = 0);
  r.f.c := (templ and $10000) <> 0;
  r.f.p_v := ((valor xor r.hl.w) and (r.hl.w xor templ) and $8000) <> 0;
  sbc_hl := templ;
end;

function res_ff(direccion: word): byte;
begin
  res_ff := $FF;
end;

procedure out_ff(direccion: word; valor: byte);
begin
end;

procedure cpu_z80.enable_daisy;
begin
  self.daisy := True;
  self.irq_vector_cb := z80daisy_ack;
end;

constructor cpu_z80.create(clock: dword; frames_div: single);
begin
  getmem(self.r, sizeof(nreg_z80));
  fillchar(self.r^, sizeof(nreg_z80), 0);
  self.numero_cpu := cpu_main_init(clock);
  self.clock := clock;
  self.tframes := (clock / frames_div) / machine_calls.fps_max;
  self.in_port := res_ff;
  self.out_port := out_ff;
  self.despues_instruccion := nil;
  self.raised_z80 := nil;
  self.m1_raised := nil;
  self.daisy := False;
  copymemory(@z80t, @z80t_m, $100);
  copymemory(@z80t_cb, @z80t_cb_m, $100);
  copymemory(@z80t_dd, @z80t_dd_m, $100);
  copymemory(@z80t_ddcb, @z80t_ddcb_m, $100);
  copymemory(@z80t_ed, @z80t_ed_m, $100);
  copymemory(@z80t_ex, @z80t_ex_m, $100);
end;

destructor cpu_z80.free;
begin
  freemem(self.r);
end;

procedure cpu_z80.change_timmings(z80t_set, z80t_cb_set, z80t_dd_set, z80t_ddcb_set, z80t_ed_set, z80t_ex_set: pbyte);
begin
  copymemory(@z80t, z80t_set, $100);
  copymemory(@z80t_cb, z80t_cb_set, $100);
  copymemory(@z80t_dd, z80t_dd_set, $100);
  copymemory(@z80t_ddcb, z80t_ddcb_set, $100);
  copymemory(@z80t_ed, z80t_ed_set, $100);
  copymemory(@z80t_ex, z80t_ex_set, $100);
end;

procedure cpu_z80.reset;
begin
  r.sp := $FFFF;
  r.pc := 0;
  r.a := 0;
  r.bc.w := 0;
  r.de.w := 0;
  r.hl.w := 0;
  r.a2 := 0;
  r.bc2.w := 0;
  r.de2.w := 0;
  r.hl2.w := 0;
  r.wz := 0;
  r.ix.w := $FFFF;
  r.iy.w := $FFFF;
  r.iff1 := False;
  r.iff2 := False;
  r.i := 0;
  r.r := 0;
  r.im := 0;
  r.f.c := False;
  r.f.n := False;
  r.f.p_v := False;
  r.f.bit3 := False;
  r.f.h := False;
  r.f.bit5 := False;
  r.f.z := True;
  r.f.s := False;
  r.f2.c := False;
  r.f2.n := False;
  r.f2.p_v := False;
  r.f2.bit3 := False;
  r.f2.h := False;
  r.f2.bit5 := False;
  r.f2.z := False;
  r.f2.s := False;
  self.change_nmi(CLEAR_LINE);
  self.pedir_irq := CLEAR_LINE;
  self.change_reset(CLEAR_LINE);
  self.change_halt(CLEAR_LINE);
  self.r.halt_opcode := False;
  self.opcode := False;
  self.after_ei := False;
  self.totalt := 0;
  self.irq_vector := $FF;
end;

function cpu_z80.get_safe_pc: word;
begin
  get_safe_pc := self.r.ppc;
end;

function cpu_z80.get_internal_r: npreg_z80;
begin
  get_internal_r := self.r;
end;

function cpu_z80.save_snapshot(data: pbyte): word;
var
  temp: pbyte;
  buffer: array [0 .. 11] of byte;
  size: word;
begin
  temp := data;
  copymemory(temp, self.r, sizeof(nreg_z80));
  inc(temp, sizeof(nreg_z80));
  size := sizeof(nreg_z80);
  buffer[0] := byte(self.r.halt_opcode);
  buffer[1] := byte(self.daisy);
  buffer[2] := byte(self.after_ei);
  buffer[3] := self.pedir_irq;
  buffer[4] := self.pedir_nmi;
  buffer[5] := self.nmi_state;
  copymemory(@buffer[6], @self.contador, 4);
  buffer[10] := self.irq_vector;
  buffer[11] := 0;
  copymemory(temp, @buffer[0], 12);
  save_snapshot := size + 12;
end;

procedure cpu_z80.load_snapshot(data: pbyte);
var
  temp: pbyte;
begin
  temp := data;
  copymemory(self.r, temp, sizeof(nreg_z80));
  inc(temp, sizeof(nreg_z80));
  self.r.halt_opcode := temp^ <> 0;
  inc(temp);
  self.daisy := (temp^ <> 0);
  inc(temp);
  self.after_ei := (temp^ <> 0);
  inc(temp);
  self.pedir_irq := temp^;
  inc(temp);
  self.pedir_nmi := temp^;
  inc(temp);
  self.nmi_state := temp^;
  inc(temp);
  copymemory(@self.contador, temp, 4);
  inc(temp, 4);
  self.irq_vector := temp^;
  inc(temp);
end;

procedure cpu_z80.change_io_calls(in_port: tgetbyte; out_port: tputbyte);
begin
  if @in_port <> nil then
    self.in_port := in_port;
  if @out_port <> nil then
    self.out_port := out_port;
end;

procedure cpu_z80.change_misc_calls(despues_instruccion: tdespues_instruccion; raised_z80: type_raised = nil; m1_raised: type_m1_raise = nil; irq_vector_cb: type_external_vector = nil);
begin
  if @despues_instruccion <> nil then
    self.despues_instruccion := despues_instruccion;
  if @raised_z80 <> nil then
    self.raised_z80 := raised_z80;
  if @m1_raised <> nil then
    self.m1_raised := m1_raised;
  if @irq_vector_cb <> nil then
    self.irq_vector_cb := irq_vector_cb;
end;

function cpu_z80.call_nmi: byte;
begin
  call_nmi := 0;
  self.r.halt_opcode := False;
  if self.nmi_state <> CLEAR_LINE then
    exit;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  self.push_sp(r.pc);
  r.iff1 := False;
  r.pc := $66;
  r.wz := $66;
  call_nmi := 11;
  if (self.pedir_nmi = PULSE_LINE) then
    self.pedir_nmi := CLEAR_LINE;
  if (self.pedir_nmi = ASSERT_LINE) then
    self.nmi_state := ASSERT_LINE;
end;

function cpu_z80.call_irq: byte;
var
  posicion: word;
  estados_t: byte;
begin
  call_irq := 0;
  if not(r.iff1) then
    exit; // se esta ejecutando otra
  if @self.raised_z80 <> nil then
    self.raised_z80;
  self.r.halt_opcode := False;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  estados_t := 0;
  if self.pedir_irq = HOLD_LINE then
    self.pedir_irq := CLEAR_LINE;
  push_sp(r.pc);
  r.iff2 := False;
  r.iff1 := False;
  case r.im of
    0:
      begin
        if self.daisy then
          MessageDlg('Damn it! A daisy chain setup was found (or done) on IM0!', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
        if @self.irq_vector_cb <> nil then
          self.irq_vector := self.irq_vector_cb;
        r.pc := self.irq_vector and $38;
        estados_t := estados_t + 12;
      end;
    1:
      begin
        r.pc := $38;
        estados_t := estados_t + 13;
      end;
    2:
      begin
        if @self.irq_vector_cb <> nil then
          self.irq_vector := self.irq_vector_cb;
        posicion := self.irq_vector or (r.i shl 8);
        r.pc := self.getbyte(posicion) + (self.getbyte(posicion + 1) shl 8);
        estados_t := estados_t + 19;
      end;
  end;
  r.wz := r.pc;
  call_irq := estados_t;
end;

procedure cpu_z80.push_sp(reg: word);
begin
  r.sp := r.sp - 2;
  self.putbyte(r.sp + 1, reg shr 8);
  self.putbyte(r.sp, reg and $FF);
end;

function cpu_z80.pop_sp: word;
var
  temp: word;
begin
  temp := self.getbyte(r.sp);
  temp := temp + (self.getbyte(r.sp + 1) shl 8);
  r.sp := r.sp + 2;
  pop_sp := temp;
end;

procedure cpu_z80.run(maximo: single);
var
  instruccion, temp: byte;
  posicion: parejas;
  ban_temp: band_z80;
  irq_temp: boolean;
  old_contador: integer;
  f, tempw: word;
begin
  irq_temp := False;
  self.contador := 0;
  while self.contador < maximo do
  begin
    old_contador := self.contador;
    if self.pedir_halt <> CLEAR_LINE then
    begin
      for f := 1 to tempw do
      begin
        self.contador := self.contador + 4;
        if @self.despues_instruccion <> nil then
          self.despues_instruccion(4);
        timers.update(4, self.numero_cpu);
        self.totalt := self.totalt + 4;
        if self.pedir_halt = CLEAR_LINE then
          break;
      end;
      if self.pedir_halt <> CLEAR_LINE then
        exit;
    end;
    if self.pedir_reset <> CLEAR_LINE then
    begin
      temp := self.pedir_reset;
      self.reset;
      if temp = ASSERT_LINE then
      begin
        self.pedir_reset := ASSERT_LINE;
        self.contador := trunc(maximo);
        exit;
      end;
    end;
    r.ppc := r.pc;
    self.estados_demas := 0;
    if not(self.after_ei) then
    begin
      if self.pedir_nmi <> CLEAR_LINE then
        self.estados_demas := self.call_nmi
      else
      begin
        if self.daisy then
          irq_temp := z80daisy_state;
        if (irq_temp or (self.pedir_irq <> CLEAR_LINE)) then
          self.estados_demas := self.call_irq;
      end;
    end;
    self.after_ei := False;
    if self.r.halt_opcode then
      r.pc := r.pc - 1;
    self.opcode := True;
    instruccion := self.getbyte(r.pc);
    if @self.m1_raised <> nil then
      self.m1_raised(instruccion);
    self.opcode := False;
    r.pc := r.pc + 1;
    r.r := ((r.r + 1) and $7F) or (r.r and $80);
    case instruccion of
      $00, $40, $49, $52, $5B, $64, $6D, $7F: { nop }
        ;
      $01:
        begin { ld BC,nn }
          r.bc.l := self.getbyte(r.pc);
          r.bc.h := self.getbyte(r.pc + 1);
          r.pc := r.pc + 2;
        end;
      $02:
        begin // ld (BC),A
          self.putbyte(r.bc.w, r.a);
          r.wz := ((r.bc.w + 1) and $FF) or (r.a shl 8);
        end;
      $03:
        r.bc.w := r.bc.w + 1; { inc BC }
      $04:
        r.bc.h := inc_8(r.bc.h); // inc B
      $05:
        r.bc.h := dec_8(r.bc.h); // dec B
      $06:
        begin { ld B,n }
          r.bc.h := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $07:
        begin // rlca
          r.f.c := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or byte(r.f.c);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := False;
          r.f.n := False;
        end;
      $08:
        begin { ex AF,AF' }
          ban_temp := r.f;
          r.f := r.f2;
          r.f2 := ban_temp;
          temp := r.a;
          r.a := r.a2;
          r.a2 := temp;
        end;
      $09:
        r.hl.w := add_16(r.hl.w, r.bc.w); // add HL,BC
      $0A:
        begin // ld A,(BC)
          r.a := self.getbyte(r.bc.w);
          r.wz := r.bc.w + 1;
        end;
      $0B:
        r.bc.w := r.bc.w - 1; { dec BC }
      $0C:
        r.bc.l := inc_8(r.bc.l); // inc C
      $0D:
        r.bc.l := dec_8(r.bc.l); // dec C
      $0E:
        begin { ld C,n }
          r.bc.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $0F:
        begin // rrca
          r.f.c := (r.a and 1) <> 0;
          r.a := (r.a shr 1) or (byte(r.f.c) shl 7);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := False;
          r.f.n := False;
        end;
      $10:
        begin // dnjz (PC+e)
          r.bc.h := r.bc.h - 1;
          r.pc := r.pc + 1;
          if r.bc.h <> 0 then
          begin
            temp := self.getbyte(r.pc - 1);
            r.pc := r.pc + shortint(temp);
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
            r.wz := r.pc;
          end;
        end;
      $11:
        begin { ld DE,nn }
          r.de.l := self.getbyte(r.pc);
          r.de.h := self.getbyte(r.pc + 1);
          r.pc := r.pc + 2;
        end;
      $12:
        begin // ld (DE),A
          self.putbyte(r.de.w, r.a);
          r.wz := ((r.de.w + 1) and $FF) or (r.a shl 8);
        end;
      $13:
        r.de.w := r.de.w + 1; { inc DE }
      $14:
        r.de.h := inc_8(r.de.h); // inc D
      $15:
        r.de.h := dec_8(r.de.h); // dec D
      $16:
        begin { ld D,n }
          r.de.h := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $17:
        begin // rla
          r.f.h := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or byte(r.f.c);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.c := r.f.h;
          r.f.h := False;
          r.f.n := False;
        end;
      $18:
        begin // jr e
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          r.pc := r.pc + shortint(temp);
          r.wz := r.pc;
        end;
      $19:
        r.hl.w := add_16(r.hl.w, r.de.w); // add HL,DE
      $1A:
        begin // ld A,(DE)
          r.a := self.getbyte(r.de.w);
          r.wz := r.de.w + 1;
        end;
      $1B:
        r.de.w := r.de.w - 1; { dec DE }
      $1C:
        r.de.l := inc_8(r.de.l); // inc E
      $1D:
        r.de.l := dec_8(r.de.l); // dec E
      $1E:
        begin { ld E,n }
          r.de.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $1F:
        begin // rra
          r.f.h := (r.a and 1) <> 0;
          r.a := (r.a shr 1) or (byte(r.f.c) shl 7);
          r.f.n := False;
          r.f.c := r.f.h;
          r.f.h := False;
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
        end;
      $20:
        begin // jr NZ,(PC+e)
          r.pc := r.pc + 1;
          if not(r.f.z) then
          begin
            temp := self.getbyte(r.pc - 1);
            r.pc := r.pc + shortint(temp);
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
            r.wz := r.pc;
          end;
        end;
      $21:
        begin { ld HL,nn }
          r.hl.l := self.getbyte(r.pc);
          r.hl.h := self.getbyte(r.pc + 1);
          r.pc := r.pc + 2;
        end;
      $22:
        begin { ld (nn),HL }
          posicion.l := self.getbyte(r.pc);
          posicion.h := self.getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          self.putbyte(posicion.w, r.hl.l);
          self.putbyte(posicion.w + 1, r.hl.h);
          r.wz := posicion.w + 1;
        end;
      $23:
        r.hl.w := r.hl.w + 1; { inc HL }
      $24:
        r.hl.h := inc_8(r.hl.h); // inc H
      $25:
        r.hl.h := dec_8(r.hl.h); // dec H
      $26:
        begin { ld H,n }
          r.hl.h := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $27:
        begin { daa }
          temp := 0;
          if (r.f.h or ((r.a and $0F) > 9)) then
            temp := temp or 6;
          if (r.f.c or (r.a > $9F)) then
            temp := temp or $60;
          if ((r.a > $8F) and ((r.a and $0F) > 9)) then
            temp := temp or $60;
          if (r.a > $99) then
            r.f.c := True;
          if r.f.n then
          begin
            r.f.h := (((r.a and $0F) - (temp and $0F)) and $10) <> 0;
            r.a := r.a - temp;
          end
          else
          begin
            r.f.h := (((r.a and $0F) + (temp and $0F)) and $10) <> 0;
            r.a := r.a + temp;
          end;
          r.f.p_v := paridad[r.a];
          r.f.s := (r.a and $80) <> 0;
          r.f.z := (r.a = 0);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
        end;
      $28:
        begin // jr Z,(PC+e)
          r.pc := r.pc + 1;
          if r.f.z then
          begin
            temp := self.getbyte(r.pc - 1);
            r.pc := r.pc + shortint(temp);
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
            r.wz := r.pc;
          end;
        end;
      $29:
        r.hl.w := add_16(r.hl.w, r.hl.w); // add HL,HL
      $2A:
        begin { ld HL,(nn) }
          posicion.h := self.getbyte(r.pc + 1);
          posicion.l := self.getbyte(r.pc);
          r.pc := r.pc + 2;
          r.hl.l := self.getbyte(posicion.w);
          r.hl.h := self.getbyte(posicion.w + 1);
          r.wz := posicion.w + 1;
        end;
      $2B:
        r.hl.w := r.hl.w - 1; { dec HL }
      $2C:
        r.hl.l := inc_8(r.hl.l); // inc L
      $2D:
        r.hl.l := dec_8(r.hl.l); // dec L
      $2E:
        begin { ld L,n }
          r.hl.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $2F:
        begin { cpl }
          r.a := r.a xor $FF;
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := True;
          r.f.n := True;
        end;
      $30:
        begin // jr NC,(PC+e)
          r.pc := r.pc + 1;
          if not(r.f.c) then
          begin
            temp := self.getbyte(r.pc - 1);
            r.pc := r.pc + shortint(temp);
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
            r.wz := r.pc;
          end;
        end;
      $31:
        begin { ld SP,nn }
          r.sp := self.getbyte(r.pc) + (self.getbyte(r.pc + 1) shl 8);
          r.pc := r.pc + 2;
        end;
      $32:
        begin { ld (nn),A }
          posicion.h := self.getbyte(r.pc + 1);
          posicion.l := self.getbyte(r.pc);
          r.pc := r.pc + 2;
          self.putbyte(posicion.w, r.a);
          r.wz := ((posicion.w + 1) and $FF) or (r.a shl 8);
        end;
      $33:
        r.sp := r.sp + 1; { inc SP }
      $34:
        begin // inc (HL)
          temp := inc_8(self.getbyte(r.hl.w));
          self.putbyte(r.hl.w, temp);
        end;
      $35:
        begin // dec (HL)
          temp := dec_8(self.getbyte(r.hl.w));
          self.putbyte(r.hl.w, temp);
        end;
      $36:
        begin { ld (HL),n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          self.putbyte(r.hl.w, temp);
        end;
      $37:
        begin { scf }
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.c := True;
          r.f.h := False;
          r.f.n := False;
        end;
      $38:
        begin // jr C,(PC+e)
          r.pc := r.pc + 1;
          if r.f.c then
          begin
            temp := self.getbyte(r.pc - 1);
            r.pc := r.pc + shortint(temp);
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
            r.wz := r.pc;
          end;
        end;
      $39:
        r.hl.w := add_16(r.hl.w, r.sp); // add HL,SP
      $3A:
        begin { ld A,(nn) }
          posicion.h := self.getbyte(r.pc + 1);
          posicion.l := self.getbyte(r.pc);
          r.pc := r.pc + 2;
          r.a := self.getbyte(posicion.w);
          r.wz := posicion.w + 1;
        end;
      $3B:
        r.sp := r.sp - 1; { dec SP }
      $3C:
        r.a := inc_8(r.a); // inc A
      $3D:
        r.a := dec_8(r.a); // dec A
      $3E:
        begin { ld A,n }
          r.a := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $3F:
        begin { ccf }
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := r.f.c;
          r.f.n := False;
          r.f.c := not(r.f.c);
        end;
      { '$'40: igual que el nop ld B,B }
      $41:
        r.bc.h := r.bc.l; { ld B,C }
      $42:
        r.bc.h := r.de.h; { ld B,D }
      $43:
        r.bc.h := r.de.l; { ld B,E }
      $44:
        r.bc.h := r.hl.h; { ld B,H }
      $45:
        r.bc.h := r.hl.l; { ld B,L }
      $46:
        r.bc.h := self.getbyte(r.hl.w); { ld B,(HL) }
      $47:
        r.bc.h := r.a; { ld B,A }
      $48:
        r.bc.l := r.bc.h; { ld C,B }
      { '$'49: igual que el nop ld C,C }
      $4A:
        r.bc.l := r.de.h; { ld C,D }
      $4B:
        r.bc.l := r.de.l; { ld C,E }
      $4C:
        r.bc.l := r.hl.h; { ld C,H }
      $4D:
        r.bc.l := r.hl.l; { ld C,L }
      $4E:
        r.bc.l := self.getbyte(r.hl.w); { ld C,(HL) }
      $4F:
        r.bc.l := r.a; { ld C,A }
      $50:
        r.de.h := r.bc.h; { ld D,B }
      $51:
        r.de.h := r.bc.l; { ld D,C }
      { '$'52 igual que el nop ld D,D }
      $53:
        r.de.h := r.de.l; { ld D,E }
      $54:
        r.de.h := r.hl.h; { ld D,H }
      $55:
        r.de.h := r.hl.l; { ld D,L }
      $56:
        r.de.h := self.getbyte(r.hl.w); { ld D,(HL) }
      $57:
        r.de.h := r.a; { ld D,A }
      $58:
        r.de.l := r.bc.h; { ld E,B }
      $59:
        r.de.l := r.bc.l; { ld E,C }
      $5A:
        r.de.l := r.de.h; { ld E,D }
      { '$'5b igual que el nop ld E,E }
      $5C:
        r.de.l := r.hl.h; { ld E,H }
      $5D:
        r.de.l := r.hl.l; { ld E,L }
      $5E:
        r.de.l := self.getbyte(r.hl.w); { ld E,(HL) }
      $5F:
        r.de.l := r.a; { ld E,A }
      $60:
        r.hl.h := r.bc.h; { ld H,B }
      $61:
        r.hl.h := r.bc.l; { ld H,C }
      $62:
        r.hl.h := r.de.h; { ld H,D }
      $63:
        r.hl.h := r.de.l; { ld H,E }
      { '$'64: igual que el nop ld H,H }
      $65:
        r.hl.h := r.hl.l; { ld H,L }
      $66:
        r.hl.h := self.getbyte(r.hl.w); { ld H,(HL) }
      $67:
        r.hl.h := r.a; { ld H,A }
      $68:
        r.hl.l := r.bc.h; { ld L,B }
      $69:
        r.hl.l := r.bc.l; { ld L,C }
      $6A:
        r.hl.l := r.de.h; { ld L,D }
      $6B:
        r.hl.l := r.de.l; { ld L,E }
      $6C:
        r.hl.l := r.hl.h; { ld L,H }
      { '$'6d: igual que el nop ld L,L }
      $6E:
        r.hl.l := self.getbyte(r.hl.w); { ld L,(HL) }
      $6F:
        r.hl.l := r.a; { ld L,A }
      $70:
        self.putbyte(r.hl.w, r.bc.h); { ld (HL),B }
      $71:
        self.putbyte(r.hl.w, r.bc.l); { ld (HL),C }
      $72:
        self.putbyte(r.hl.w, r.de.h); { ld (HL),D }
      $73:
        self.putbyte(r.hl.w, r.de.l); { ld (HL),E }
      $74:
        self.putbyte(r.hl.w, r.hl.h); { ld (HL),H }
      $75:
        self.putbyte(r.hl.w, r.hl.l); { ld (HL),L }
      $76:
        self.r.halt_opcode := True; // halt
      $77:
        self.putbyte(r.hl.w, r.a); { ld (HL),A }
      $78:
        r.a := r.bc.h; { ld A,B }
      $79:
        r.a := r.bc.l; { ld A,C }
      $7A:
        r.a := r.de.h; { ld A,D }
      $7B:
        r.a := r.de.l; { ld A,E }
      $7C:
        r.a := r.hl.h; { ld A,H }
      $7D:
        r.a := r.hl.l; { ld A,L }
      $7E:
        r.a := self.getbyte(r.hl.w); { ld A,(HL) }
      { '$'7f: igual que el nop ld A,A }
      $80:
        add_8(r.bc.h); { add A,B }
      $81:
        add_8(r.bc.l); { add A,C }
      $82:
        add_8(r.de.h); { add A,D }
      $83:
        add_8(r.de.l); { add A,E }
      $84:
        add_8(r.hl.h); { add A,H }
      $85:
        add_8(r.hl.l); { add A,L }
      $86:
        add_8(self.getbyte(r.hl.w)); { add A,(HL) }
      $87:
        add_8(r.a); { add A,A }
      $88:
        adc_8(r.bc.h); { adc A,B }
      $89:
        adc_8(r.bc.l); { adc A,C }
      $8A:
        adc_8(r.de.h); { adc A,D }
      $8B:
        adc_8(r.de.l); { adc A,E }
      $8C:
        adc_8(r.hl.h); { adc A,H }
      $8D:
        adc_8(r.hl.l); { adc A,L }
      $8E:
        adc_8(self.getbyte(r.hl.w)); { adc A,(HL) }
      $8F:
        adc_8(r.a); { adc A,A }
      $90:
        sub_8(r.bc.h); { sub B }
      $91:
        sub_8(r.bc.l); { sub C }
      $92:
        sub_8(r.de.h); { sub D }
      $93:
        sub_8(r.de.l); { sub E }
      $94:
        sub_8(r.hl.h); { sub H }
      $95:
        sub_8(r.hl.l); { sub L }
      $96:
        sub_8(self.getbyte(r.hl.w)); { sub (HL) }
      $97:
        sub_8(r.a); { sub A }
      $98:
        sbc_8(r.bc.h); { sbc A,B }
      $99:
        sbc_8(r.bc.l); { sbc A,C }
      $9A:
        sbc_8(r.de.h); { sbc A,D }
      $9B:
        sbc_8(r.de.l); { sbc A,E }
      $9C:
        sbc_8(r.hl.h); { sbc A,H }
      $9D:
        sbc_8(r.hl.l); { sbc A,L }
      $9E:
        sbc_8(self.getbyte(r.hl.w)); { sbc A,(HL) }
      $9F:
        sbc_8(r.a); { sbc A,A }
      $A0:
        and_a(r.bc.h); { and A,B }
      $A1:
        and_a(r.bc.l); { and A,C }
      $A2:
        and_a(r.de.h); { and A,D }
      $A3:
        and_a(r.de.l); { and A,E }
      $A4:
        and_a(r.hl.h); { and A,H }
      $A5:
        and_a(r.hl.l); { and A,L }
      $A6:
        and_a(self.getbyte(r.hl.w)); { and A,(HL) }
      $A7:
        and_a(r.a); { and A,A }
      $A8:
        xor_a(r.bc.h); { xor A,B }
      $A9:
        xor_a(r.bc.l); { xor A,C }
      $AA:
        xor_a(r.de.h); { xor A,D }
      $AB:
        xor_a(r.de.l); { xor A,E }
      $AC:
        xor_a(r.hl.h); { xor A,H }
      $AD:
        xor_a(r.hl.l); { xor A,L }
      $AE:
        xor_a(self.getbyte(r.hl.w)); { xor A,(HL) }
      $AF:
        begin { xor A,A }
          r.a := 0;
          r.f.s := False;
          r.f.z := True;
          r.f.bit5 := False;
          r.f.h := False;
          r.f.bit3 := False;
          r.f.p_v := True;
          r.f.n := False;
          r.f.c := False;
        end;
      $B0:
        or_a(r.bc.h); { or B }
      $B1:
        or_a(r.bc.l); { or C }
      $B2:
        or_a(r.de.h); { or D }
      $B3:
        or_a(r.de.l); { or E }
      $B4:
        or_a(r.hl.h); { or H }
      $B5:
        or_a(r.hl.l); { or L }
      $B6:
        or_a(self.getbyte(r.hl.w)); { or (HL) }
      $B7:
        or_a(r.a); { or A }
      $B8:
        cp_a(r.bc.h); { cp B }
      $B9:
        cp_a(r.bc.l); { cp C }
      $BA:
        cp_a(r.de.h); { cp D }
      $BB:
        cp_a(r.de.l); { cp E }
      $BC:
        cp_a(r.hl.h); { cp H }
      $BD:
        cp_a(r.hl.l); { cp L }
      $BE:
        cp_a(self.getbyte(r.hl.w)); { cp (HL) }
      $BF:
        cp_a(r.a); { cp A }
      $C0:
        if not(r.f.z) then
        begin // ret NZ
          r.pc := self.pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $C1:
        r.bc.w := self.pop_sp; { pop BC }
      $C2:
        begin // jp NZ,nn
          if not(r.f.z) then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $C3:
        begin { jp nn }
          posicion.h := self.getbyte(r.pc + 1);
          posicion.l := self.getbyte(r.pc);
          r.pc := posicion.w;
          r.wz := posicion.w;
        end;
      $C4:
        begin // call NZ,nn
          r.pc := r.pc + 2;
          if not(r.f.z) then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $C5:
        self.push_sp(r.bc.w); { push BC }
      $C6:
        begin { add A,n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          add_8(temp);
        end;
      $C7:
        begin // rst 00H
          self.push_sp(r.pc);
          r.pc := 0;
          r.wz := 0;
        end;
      $C8:
        if r.f.z then
        begin // ret Z
          r.pc := self.pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $C9:
        begin // ret
          r.pc := pop_sp;
          r.wz := r.pc;
        end;
      $CA:
        begin // jp Z,nn
          if r.f.z then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $CB:
        self.exec_cb;
      $CC:
        begin // call Z,nn
          r.pc := r.pc + 2;
          if r.f.z then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $CD:
        begin { call nn }
          posicion.h := self.getbyte(r.pc + 1);
          posicion.l := self.getbyte(r.pc);
          r.wz := posicion.w;
          r.pc := r.pc + 2;
          self.push_sp(r.pc);
          r.pc := posicion.w;
        end;
      $CE:
        begin { adc A,n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          adc_8(temp);
        end;
      $CF:
        begin // rst 08H
          self.push_sp(r.pc);
          r.pc := $8;
          r.wz := $8;
        end;
      $D0:
        if not(r.f.c) then
        begin // ret NC
          r.pc := pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $D1:
        r.de.w := pop_sp; { pop DE }
      $D2:
        begin // jp NC,nn
          if not(r.f.c) then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $D3:
        begin { out (n),A }
          posicion.l := self.getbyte(r.pc);
          posicion.h := r.a;
          r.pc := r.pc + 1;
          self.out_port(posicion.w, r.a);
          r.wz := ((posicion.l + 1) and $FF) or (r.a shl 8);
        end;
      $D4:
        begin // call NC,nn
          r.pc := r.pc + 2;
          if not(r.f.c) then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $D5:
        self.push_sp(r.de.w); { push DE }
      $D6:
        begin { sub n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          sub_8(temp);
        end;
      $D7:
        begin // rst 10H
          self.push_sp(r.pc);
          r.pc := $10;
          r.wz := $10;
        end;
      $D8:
        if r.f.c then
        begin // ret C
          r.pc := pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $D9:
        begin { exx }
          posicion := r.bc;
          r.bc := r.bc2;
          r.bc2 := posicion;
          posicion := r.de;
          r.de := r.de2;
          r.de2 := posicion;
          posicion := r.hl;
          r.hl := r.hl2;
          r.hl2 := posicion;
        end;
      $DA:
        begin // jp C,nn
          if r.f.c then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $DB:
        begin // in A,(n)
          posicion.l := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion.h := r.a;
          r.a := self.in_port(posicion.w);
          r.wz := posicion.w + 1;
        end;
      $DC:
        begin // call C,nn
          r.pc := r.pc + 2;
          if r.f.c then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $DD:
        self.exec_dd_fd(True);
      $DE:
        begin { sbc A,n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          sbc_8(temp);
        end;
      $DF:
        begin // rst 18H
          self.push_sp(r.pc);
          r.pc := $18;
          r.wz := $18;
        end;
      $E0:
        if not(r.f.p_v) then
        begin // ret PO
          r.pc := self.pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $E1:
        r.hl.w := pop_sp; { pop HL }
      $E2:
        begin // jp PO,nn
          if not(r.f.p_v) then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $E3:
        begin { ex (sp),hl }
          posicion.w := pop_sp;
          self.push_sp(r.hl.w);
          r.hl := posicion;
          r.wz := posicion.w;
        end;
      $E4:
        begin // call PO,nn
          r.pc := r.pc + 2;
          if not(r.f.p_v) then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $E5:
        self.push_sp(r.hl.w); { push HL }
      $E6:
        begin { and A,n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          and_a(temp);
        end;
      $E7:
        begin // rst 20H
          self.push_sp(r.pc);
          r.pc := $20;
          r.wz := $20;
        end;
      $E8:
        if r.f.p_v then
        begin // ret PE
          r.pc := pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $E9:
        r.pc := r.hl.w; { jp (HL) }
      $EA:
        begin // jp PE,nn
          if r.f.p_v then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $EB:
        begin { ex DE,HL }
          posicion := r.de;
          r.de := r.hl;
          r.hl := posicion;
        end;
      $EC:
        begin // call PE,nn
          r.pc := r.pc + 2;
          if r.f.p_v then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $ED:
        exec_ed;
      $EE:
        begin { xor A,n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          xor_a(temp);
        end;
      $EF:
        begin // rst 28H
          self.push_sp(r.pc);
          r.pc := $28;
          r.wz := $28;
        end;
      $F0:
        if not(r.f.s) then
        begin // ret NP
          r.pc := self.pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $F1:
        begin { pop AF }
          posicion.w := pop_sp;
          r.a := posicion.h;
          if (posicion.l and 128) <> 0 then
            r.f.s := True
          else
            r.f.s := False;
          if (posicion.l and 64) <> 0 then
            r.f.z := True
          else
            r.f.z := False;
          if (posicion.l and 32) <> 0 then
            r.f.bit5 := True
          else
            r.f.bit5 := False;
          if (posicion.l and 16) <> 0 then
            r.f.h := True
          else
            r.f.h := False;
          if (posicion.l and 8) <> 0 then
            r.f.bit3 := True
          else
            r.f.bit3 := False;
          if (posicion.l and 4) <> 0 then
            r.f.p_v := True
          else
            r.f.p_v := False;
          if (posicion.l and 2) <> 0 then
            r.f.n := True
          else
            r.f.n := False;
          if (posicion.l and 1) <> 0 then
            r.f.c := True
          else
            r.f.c := False;
        end;
      $F2:
        begin // jp P,nn
          if not(r.f.s) then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $F3:
        begin { di }
          r.iff1 := False;
          r.iff2 := False;
        end;
      $F4:
        begin // call P,nn
          r.pc := r.pc + 2;
          if not(r.f.s) then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $F5:
        begin // push AF
          posicion.h := r.a;
          posicion.l := byte(r.f.s) shl 7;
          posicion.l := posicion.l or (byte(r.f.z) shl 6);
          posicion.l := posicion.l or (byte(r.f.bit5) shl 5);
          posicion.l := posicion.l or (byte(r.f.h) shl 4);
          posicion.l := posicion.l or (byte(r.f.bit3) shl 3);
          posicion.l := posicion.l or (byte(r.f.p_v) shl 2);
          posicion.l := posicion.l or (byte(r.f.n) shl 1);
          posicion.l := posicion.l or byte(r.f.c);
          self.push_sp(posicion.w);
        end;
      $F6:
        begin { or n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          or_a(temp);
        end;
      $F7:
        begin // rst 30H
          self.push_sp(r.pc);
          r.pc := $30;
          r.wz := $30;
        end;
      $F8:
        if r.f.s then
        begin // ret M
          r.pc := self.pop_sp;
          r.wz := r.pc;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      $F9:
        r.sp := r.hl.w; { ld SP,HL }
      $FA:
        begin // jp M,nn
          if r.f.s then
          begin
            posicion.h := self.getbyte(r.pc + 1);
            posicion.l := self.getbyte(r.pc);
            r.pc := posicion.w;
          end
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $FB:
        begin { ei }
          r.iff1 := True;
          r.iff2 := True;
          self.after_ei := True;
        end;
      $FC:
        begin // call M,nn
          r.pc := r.pc + 2;
          if r.f.s then
          begin
            self.push_sp(r.pc);
            posicion.h := self.getbyte(r.pc - 1);
            posicion.l := self.getbyte(r.pc - 2);
            r.pc := posicion.w;
            self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          end;
          r.wz := r.pc;
        end;
      $FD:
        self.exec_dd_fd(False);
      $FE:
        begin { cp n }
          temp := self.getbyte(r.pc);
          r.pc := r.pc + 1;
          cp_a(temp);
        end;
      $FF:
        begin // rst 38H
          self.push_sp(r.pc);
          r.pc := $38;
          r.wz := $38;
        end;
    end; // del case
    self.contador := self.contador + self.estados_demas + z80t[instruccion];
    // Ojo!! el contador se puede incrementar en la funcion siguiente!! Debo volver a calcular los estados
    if @self.despues_instruccion <> nil then
      self.despues_instruccion(self.contador - old_contador);
    tempw := self.contador - old_contador;
    timers.update(tempw, self.numero_cpu);
    self.totalt := self.totalt + tempw;
  end; // del while
end;

procedure cpu_z80.exec_cb;
var
  instruccion, temp: byte;
begin
  self.opcode := True;
  instruccion := self.getbyte(r.pc);
  if @self.m1_raised <> nil then
    self.m1_raised(instruccion);
  self.opcode := False;
  r.pc := r.pc + 1;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  case instruccion of
    $00:
      rlc_8(@r.bc.h); { rlc B }
    $01:
      rlc_8(@r.bc.l); { rlc C }
    $02:
      rlc_8(@r.de.h); { rlc D }
    $03:
      rlc_8(@r.de.l); { rlc E }
    $04:
      rlc_8(@r.hl.h); { rlc H }
    $05:
      rlc_8(@r.hl.l); { rlc L }
    $06:
      begin { rlc (HL) }
        temp := self.getbyte(r.hl.w);
        rlc_8(@temp);
        self.putbyte(r.hl.w, temp);
      end;
    $07:
      rlc_8(@r.a); { rlc A }
    $08:
      rrc_8(@r.bc.h); { rrc B }
    $09:
      rrc_8(@r.bc.l); { rrc C }
    $0A:
      rrc_8(@r.de.h); { rrc D }
    $0B:
      rrc_8(@r.de.l); { rrc E }
    $0C:
      rrc_8(@r.hl.h); { rrc H }
    $0D:
      rrc_8(@r.hl.l); { rrc L }
    $0E:
      begin { rrc (HL) }
        temp := self.getbyte(r.hl.w);
        rrc_8(@temp);
        self.putbyte(r.hl.w, temp);
      end;
    $0F:
      rrc_8(@r.a); { rrc A }
    $10:
      rl_8(@r.bc.h); { rl B }
    $11:
      rl_8(@r.bc.l); { rl C }
    $12:
      rl_8(@r.de.h); { rl D }
    $13:
      rl_8(@r.de.l); { rl E }
    $14:
      rl_8(@r.hl.h); { rl H }
    $15:
      rl_8(@r.hl.l); { rl L }
    $16:
      begin
        temp := self.getbyte(r.hl.w);
        rl_8(@temp); { rl (HL) }
        self.putbyte(r.hl.w, temp);
      end;
    $17:
      rl_8(@r.a); { rl A }
    $18:
      rr_8(@r.bc.h); { rr B }
    $19:
      rr_8(@r.bc.l); { rr C }
    $1A:
      rr_8(@r.de.h); { rr D }
    $1B:
      rr_8(@r.de.l); { rr E }
    $1C:
      rr_8(@r.hl.h); { rr H }
    $1D:
      rr_8(@r.hl.l); { rr L }
    $1E:
      begin
        temp := self.getbyte(r.hl.w);
        rr_8(@temp); { rr (HL) }
        self.putbyte(r.hl.w, temp);
      end;
    $1F:
      rr_8(@r.a); { rr A }
    $20:
      sla_8(@r.bc.h); { sla B }
    $21:
      sla_8(@r.bc.l); { sla C }
    $22:
      sla_8(@r.de.h); { sla D }
    $23:
      sla_8(@r.de.l); { sla E }
    $24:
      sla_8(@r.hl.h); { sla H }
    $25:
      sla_8(@r.hl.l); { sla L }
    $26:
      begin { sla (HL) }
        temp := self.getbyte(r.hl.w);
        sla_8(@temp);
        self.putbyte(r.hl.w, temp);
      end;
    $27:
      sla_8(@r.a); { sla A }
    $28:
      sra_8(@r.bc.h); // sra B
    $29:
      sra_8(@r.bc.l); // sra C
    $2A:
      sra_8(@r.de.h); // sra D
    $2B:
      sra_8(@r.de.l); // sra E
    $2C:
      sra_8(@r.hl.h); // sra H
    $2D:
      sra_8(@r.hl.l); // sra L
    $2E:
      begin // sra (HL)
        temp := self.getbyte(r.hl.w);
        sra_8(@temp);
        self.putbyte(r.hl.w, temp);
      end;
    $2F:
      sra_8(@r.a); // sra A
    $30:
      sll_8(@r.bc.h); { sll B }
    $31:
      sll_8(@r.bc.l); { sll C }
    $32:
      sll_8(@r.de.h); { sll D }
    $33:
      sll_8(@r.de.l); { sll E }
    $34:
      sll_8(@r.hl.h); { sll H }
    $35:
      sll_8(@r.hl.l); { sll L }
    $36:
      begin { sll (HL) }
        temp := self.getbyte(r.hl.w);
        sll_8(@temp);
        self.putbyte(r.hl.w, temp);
      end;
    $37:
      sll_8(@r.a); { sll a }
    $38:
      srl_8(@r.bc.h); { srl B }
    $39:
      srl_8(@r.bc.l); { srl C }
    $3A:
      srl_8(@r.de.h); { srl D }
    $3B:
      srl_8(@r.de.l); { srl E }
    $3C:
      srl_8(@r.hl.h); { srl H }
    $3D:
      srl_8(@r.hl.l); { srl L }
    $3E:
      begin { srl (HL) }
        temp := self.getbyte(r.hl.w);
        srl_8(@temp);
        self.putbyte(r.hl.w, temp);
      end;
    $3F:
      srl_8(@r.a); { srl a }
    $40:
      bit_8(0, r.bc.h); { bit 0,B }
    $41:
      bit_8(0, r.bc.l); { bit 0,C }
    $42:
      bit_8(0, r.de.h); { bit 0,D }
    $43:
      bit_8(0, r.de.l); { bit 0,E }
    $44:
      bit_8(0, r.hl.h); { bit 0,H }
    $45:
      bit_8(0, r.hl.l); { bit 0,L }
    $46:
      begin // bit 0,(HL)
        bit_8(0, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $47:
      bit_8(0, r.a); { bit 0,A }
    $48:
      bit_8(1, r.bc.h); { bit 1,B }
    $49:
      bit_8(1, r.bc.l); { bit 1,C }
    $4A:
      bit_8(1, r.de.h); { bit 1,D }
    $4B:
      bit_8(1, r.de.l); { bit 1,E }
    $4C:
      bit_8(1, r.hl.h); { bit 1,H }
    $4D:
      bit_8(1, r.hl.l); { bit 1,L }
    $4E:
      begin // bit 1,(HL)
        bit_8(1, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $4F:
      bit_8(1, r.a); { bit 1,A }
    $50:
      bit_8(2, r.bc.h); { bit 2,B }
    $51:
      bit_8(2, r.bc.l); { bit 2,C }
    $52:
      bit_8(2, r.de.h); { bit 2,D }
    $53:
      bit_8(2, r.de.l); { bit 2,E }
    $54:
      bit_8(2, r.hl.h); { bit 2,H }
    $55:
      bit_8(2, r.hl.l); { bit 2,L }
    $56:
      begin // bit 2,(HL)
        bit_8(2, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $57:
      bit_8(2, r.a); { bit 2,A }
    $58:
      bit_8(3, r.bc.h); { bit 3,B }
    $59:
      bit_8(3, r.bc.l); { bit 3,C }
    $5A:
      bit_8(3, r.de.h); { bit 3,D }
    $5B:
      bit_8(3, r.de.l); { bit 3,E }
    $5C:
      bit_8(3, r.hl.h); { bit 3,H }
    $5D:
      bit_8(3, r.hl.l); { bit 3,L }
    $5E:
      begin // bit 3,(HL)
        bit_8(3, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $5F:
      bit_8(3, r.a); { bit 3,A }
    $60:
      bit_8(4, r.bc.h); { bit 4,B }
    $61:
      bit_8(4, r.bc.l); { bit 4,C }
    $62:
      bit_8(4, r.de.h); { bit 4,D }
    $63:
      bit_8(4, r.de.l); { bit 4,E }
    $64:
      bit_8(4, r.hl.h); { bit 4,H }
    $65:
      bit_8(4, r.hl.l); { bit 4,L }
    $66:
      begin // bit 4,(HL)
        bit_8(4, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $67:
      bit_8(4, r.a); { bit 4,A }
    $68:
      bit_8(5, r.bc.h); { bit 5,B }
    $69:
      bit_8(5, r.bc.l); { bit 5,C }
    $6A:
      bit_8(5, r.de.h); { bit 5,D }
    $6B:
      bit_8(5, r.de.l); { bit 5,E }
    $6C:
      bit_8(5, r.hl.h); { bit 5,H }
    $6D:
      bit_8(5, r.hl.l); { bit 5,L }
    $6E:
      begin // bit 5,(HL)
        bit_8(5, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $6F:
      bit_8(5, r.a); { bit 5,A }
    $70:
      bit_8(6, r.bc.h); { bit 6,B }
    $71:
      bit_8(6, r.bc.l); { bit 6,C }
    $72:
      bit_8(6, r.de.h); { bit 6,D }
    $73:
      bit_8(6, r.de.l); { bit 6,E }
    $74:
      bit_8(6, r.hl.h); { bit 6,H }
    $75:
      bit_8(6, r.hl.l); { bit 6,L }
    $76:
      begin // bit 6,(HL)
        bit_8(6, self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $77:
      bit_8(6, r.a); { bit 6,A }
    $78:
      bit_7(r.bc.h); { bit 7,B }
    $79:
      bit_7(r.bc.l); { bit 7,C }
    $7A:
      bit_7(r.de.h); { bit 7,D }
    $7B:
      bit_7(r.de.l); { bit 7,E }
    $7C:
      bit_7(r.hl.h); { bit 7,H }
    $7D:
      bit_7(r.hl.l); { bit 7,L }
    $7E:
      begin // bit 7,(HL)
        bit_7(self.getbyte(r.hl.w));
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $7F:
      bit_7(r.a); { bit 7,A }
    $80:
      r.bc.h := (r.bc.h and $FE); { res 0,B }
    $81:
      r.bc.l := (r.bc.l and $FE); { res 0,C }
    $82:
      r.de.h := (r.de.h and $FE); { res 0,D }
    $83:
      r.de.l := (r.de.l and $FE); { res 0,E }
    $84:
      r.hl.h := (r.hl.h and $FE); { res 0,H }
    $85:
      r.hl.l := (r.hl.l and $FE); { res 0,L }
    $86:
      begin { res 0,(hl) }
        temp := (self.getbyte(r.hl.w) and $FE);
        self.putbyte(r.hl.w, temp);
      end;
    $87:
      r.a := r.a and $FE; { res 0,A }
    $88:
      r.bc.h := r.bc.h and $FD; { res 1,B }
    $89:
      r.bc.l := r.bc.l and $FD; { res 1,C }
    $8A:
      r.de.h := r.de.h and $FD; { res 1,D }
    $8B:
      r.de.l := r.de.l and $FD; { res 1,E }
    $8C:
      r.hl.h := r.hl.h and $FD; { res 1,H }
    $8D:
      r.hl.l := r.hl.l and $FD; { res 1,L }
    $8E:
      begin { res 1,(hl) }
        temp := (self.getbyte(r.hl.w) and $FD);
        self.putbyte(r.hl.w, temp);
      end;
    $8F:
      r.a := r.a and $FD; { res 1,A }
    $90:
      r.bc.h := r.bc.h and $FB; { res 2,B }
    $91:
      r.bc.l := r.bc.l and $FB; { res 2,C }
    $92:
      r.de.h := r.de.h and $FB; { res 2,D }
    $93:
      r.de.l := r.de.l and $FB; { res 2,E }
    $94:
      r.hl.h := r.hl.h and $FB; { res 2,H }
    $95:
      r.hl.l := r.hl.l and $FB; { res 2,L }
    $96:
      begin { res 2,(HL) }
        temp := self.getbyte(r.hl.w) and $FB;
        self.putbyte(r.hl.w, temp);
      end;
    $97:
      r.a := r.a and $FB; { res 2,A }
    $98:
      r.bc.h := r.bc.h and $F7; { res 3,B }
    $99:
      r.bc.l := r.bc.l and $F7; { res 3,C }
    $9A:
      r.de.h := r.de.h and $F7; { res 3,D }
    $9B:
      r.de.l := r.de.l and $F7; { res 3,E }
    $9C:
      r.hl.h := r.hl.h and $F7; { res 3,H }
    $9D:
      r.hl.l := r.hl.l and $F7; { res 3,L }
    $9E:
      begin { res 3,(HL) }
        temp := self.getbyte(r.hl.w) and $F7;
        self.putbyte(r.hl.w, temp);
      end;
    $9F:
      r.a := r.a and $F7; { res 3,A }
    $A0:
      r.bc.h := r.bc.h and $EF; { res 4,B }
    $A1:
      r.bc.l := r.bc.l and $EF; { res 4,C }
    $A2:
      r.de.h := r.de.h and $EF; { res 4,D }
    $A3:
      r.de.l := r.de.l and $EF; { res 4,E }
    $A4:
      r.hl.h := r.hl.h and $EF; { res 4,H }
    $A5:
      r.hl.l := r.hl.l and $EF; { res 4,L }
    $A6:
      begin { res 4,(HL) }
        temp := self.getbyte(r.hl.w) and $EF;
        self.putbyte(r.hl.w, temp);
      end;
    $A7:
      r.a := r.a and $EF; { res 4,A }
    $A8:
      r.bc.h := r.bc.h and $DF; { res 5,B }
    $A9:
      r.bc.l := r.bc.l and $DF; { res 5,C }
    $AA:
      r.de.h := r.de.h and $DF; { res 5,D }
    $AB:
      r.de.l := r.de.l and $DF; { res 5,E }
    $AC:
      r.hl.h := r.hl.h and $DF; { res 5,H }
    $AD:
      r.hl.l := r.hl.l and $DF; { res 5,L }
    $AE:
      begin { res 5,(HL) }
        temp := self.getbyte(r.hl.w) and $DF;
        self.putbyte(r.hl.w, temp);
      end;
    $AF:
      r.a := r.a and $DF; { res 5,A }
    $B0:
      r.bc.h := r.bc.h and $BF; { res 6,B }
    $B1:
      r.bc.l := r.bc.l and $BF; { res 6,C }
    $B2:
      r.de.h := r.de.h and $BF; { res 6,D }
    $B3:
      r.de.l := r.de.l and $BF; { res 6,E }
    $B4:
      r.hl.h := r.hl.h and $BF; { res 6,H }
    $B5:
      r.hl.l := r.hl.l and $BF; { res 6,L }
    $B6:
      begin { res 6,(HL) }
        temp := self.getbyte(r.hl.w) and $BF;
        self.putbyte(r.hl.w, temp);
      end;
    $B7:
      r.a := r.a and $BF; { res 6,A }
    $B8:
      r.bc.h := r.bc.h and $7F; { res 7,B }
    $B9:
      r.bc.l := r.bc.l and $7F; { res 7,C }
    $BA:
      r.de.h := r.de.h and $7F; { res 7,D }
    $BB:
      r.de.l := r.de.l and $7F; { res 7,E }
    $BC:
      r.hl.h := r.hl.h and $7F; { res 7,H }
    $BD:
      r.hl.l := r.hl.l and $7F; { res 7,L }
    $BE:
      begin { res 7,(HL) }
        temp := self.getbyte(r.hl.w) and $7F;
        self.putbyte(r.hl.w, temp);
      end;
    $BF:
      r.a := r.a and $7F; { res 7,A }
    $C0:
      r.bc.h := r.bc.h or $1; { set 0,B }
    $C1:
      r.bc.l := r.bc.l or $1; { set 0,C }
    $C2:
      r.de.h := r.de.h or $1; { set 0,D }
    $C3:
      r.de.l := r.de.l or $1; { set 0,E }
    $C4:
      r.hl.h := r.hl.h or $1; { set 0,H }
    $C5:
      r.hl.l := r.hl.l or $1; { set 0,L }
    $C6:
      begin { set 0,(HL) }
        temp := self.getbyte(r.hl.w) or 1;
        self.putbyte(r.hl.w, temp);
      end;
    $C7:
      r.a := r.a or $1; { set 0,A }
    $C8:
      r.bc.h := r.bc.h or $2; { set 1,B }
    $C9:
      r.bc.l := r.bc.l or $2; { set 1,C }
    $CA:
      r.de.h := r.de.h or $2; { set 1,D }
    $CB:
      r.de.l := r.de.l or $2; { set 1,E }
    $CC:
      r.hl.h := r.hl.h or $2; { set 1,H }
    $CD:
      r.hl.l := r.hl.l or $2; { set 1,L }
    $CE:
      begin { set 1,(HL) }
        temp := self.getbyte(r.hl.w) or 2;
        self.putbyte(r.hl.w, temp);
      end;
    $CF:
      r.a := r.a or $2; { set 1,A }
    $D0:
      r.bc.h := r.bc.h or $4; { set 2,B }
    $D1:
      r.bc.l := r.bc.l or $4; { set 2,C }
    $D2:
      r.de.h := r.de.h or $4; { set 2,D }
    $D3:
      r.de.l := r.de.l or $4; { set 2,E }
    $D4:
      r.hl.h := r.hl.h or $4; { set 2,H }
    $D5:
      r.hl.l := r.hl.l or $4; { set 2,L }
    $D6:
      begin { set 2,(HL) }
        temp := self.getbyte(r.hl.w) or 4;
        self.putbyte(r.hl.w, temp);
      end;
    $D7:
      r.a := r.a or $4; { set 2,A }
    $D8:
      r.bc.h := r.bc.h or $8; { set 3,B }
    $D9:
      r.bc.l := r.bc.l or $8; { set 3,C }
    $DA:
      r.de.h := r.de.h or $8; { set 3,D }
    $DB:
      r.de.l := r.de.l or $8; { set 3,E }
    $DC:
      r.hl.h := r.hl.h or $8; { set 3,H }
    $DD:
      r.hl.l := r.hl.l or $8; { set 3,L }
    $DE:
      begin { set 3,(HL) }
        temp := self.getbyte(r.hl.w) or 8;
        self.putbyte(r.hl.w, temp);
      end;
    $DF:
      r.a := r.a or $8; { set 3,A }
    $E0:
      r.bc.h := r.bc.h or $10; { set 4,B }
    $E1:
      r.bc.l := r.bc.l or $10; { set 4,C }
    $E2:
      r.de.h := r.de.h or $10; { set 4,D }
    $E3:
      r.de.l := r.de.l or $10; { set 4,E }
    $E4:
      r.hl.h := r.hl.h or $10; { set 4,H }
    $E5:
      r.hl.l := r.hl.l or $10; { set 4,L }
    $E6:
      begin { set 4,(HL) }
        temp := self.getbyte(r.hl.w) or $10;
        self.putbyte(r.hl.w, temp);
      end;
    $E7:
      r.a := r.a or $10; { set 4,A }
    $E8:
      r.bc.h := r.bc.h or $20; { set 5,B }
    $E9:
      r.bc.l := r.bc.l or $20; { set 5,C }
    $EA:
      r.de.h := r.de.h or $20; { set 5,D }
    $EB:
      r.de.l := r.de.l or $20; { set 5,E }
    $EC:
      r.hl.h := r.hl.h or $20; { set 5,H }
    $ED:
      r.hl.l := r.hl.l or $20; { set 5,L }
    $EE:
      begin { set 5,(HL) }
        temp := self.getbyte(r.hl.w) or $20;
        self.putbyte(r.hl.w, temp);
      end;
    $EF:
      r.a := r.a or $20; { set 5,A }
    $F0:
      r.bc.h := r.bc.h or $40; { set 6,B }
    $F1:
      r.bc.l := r.bc.l or $40; { set 6,C }
    $F2:
      r.de.h := r.de.h or $40; { set 6,D }
    $F3:
      r.de.l := r.de.l or $40; { set 6,E }
    $F4:
      r.hl.h := r.hl.h or $40; { set 6,H }
    $F5:
      r.hl.l := r.hl.l or $40; { set 6,L }
    $F6:
      begin { set 6,(HL) }
        temp := self.getbyte(r.hl.w) or $40;
        self.putbyte(r.hl.w, temp);
      end;
    $F7:
      r.a := r.a or $40; { set 6,A }
    $F8:
      r.bc.h := r.bc.h or $80; { set 7,B }
    $F9:
      r.bc.l := r.bc.l or $80; { set 7,C }
    $FA:
      r.de.h := r.de.h or $80; { set 7,D }
    $FB:
      r.de.l := r.de.l or $80; { set 7,E }
    $FC:
      r.hl.h := r.hl.h or $80; { set 7,H }
    $FD:
      r.hl.l := r.hl.l or $80; { set 7,L }
    $FE:
      begin { set 7,(HL) }
        temp := self.getbyte(r.hl.w) or $80;
        self.putbyte(r.hl.w, temp);
      end;
    $FF:
      r.a := r.a or $80; { set 7,A }
  end;
  self.estados_demas := self.estados_demas + z80t_cb[instruccion];
end;

procedure cpu_z80.exec_dd_fd(tipo: boolean);
var
  instruccion, temp: byte;
  temp2: word;
  registro: pparejas;
  posicion: parejas;
begin
  if tipo then
    registro := @r.ix
  else
    registro := @r.iy;
  temp2 := registro.w;
  self.opcode := True;
  instruccion := self.getbyte(r.pc);
  if @self.m1_raised <> nil then
    self.m1_raised(instruccion);
  self.opcode := False;
  r.pc := r.pc + 1;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  case instruccion of
    $09:
      registro.w := add_16(registro.w, r.bc.w); // add IX,BC
    $19:
      registro.w := add_16(registro.w, r.de.w); // add IX,DE
    $21:
      begin { ld IX,nn }
        registro.h := self.getbyte(r.pc + 1);
        registro.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;
      end;
    $22:
      begin { ld (nn),IX }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;
        self.putbyte(posicion.w, registro.l);
        self.putbyte(posicion.w + 1, registro.h);
        r.wz := posicion.w + 1;
      end;
    $23:
      registro.w := registro.w + 1; { inc IX }
    $24:
      registro.h := inc_8(registro.h); // inc IXh
    $25:
      registro.h := dec_8(registro.h); // dec IXh
    $26:
      begin { ld IXh,n }
        registro^.h := self.getbyte(r.pc);
        r.pc := r.pc + 1;
      end;
    $29:
      registro.w := add_16(registro.w, registro.w); // add IX,IX
    $2A:
      begin { ld (IX,(nn) }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;
        registro^.l := self.getbyte(posicion.w);
        registro^.h := self.getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    $2B:
      registro^.w := registro^.w - 1; { dec IX }
    $2C:
      registro.l := inc_8(registro.l); // inc IXl
    $2D:
      registro.l := dec_8(registro.l); // dec IXl
    $2E:
      begin { ld IXl,n }
        registro^.l := self.getbyte(r.pc);
        r.pc := r.pc + 1;
      end;
    $34:
      begin { inc (IX+d) } // debo tener en cuenta que temp2=registro.w
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := inc_8(self.getbyte(temp2));
        self.putbyte(temp2, temp);
      end;
    $35:
      begin { dec (IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := dec_8(self.getbyte(temp2));
        self.putbyte(temp2, temp);
      end;
    $36:
      begin { ld (IX+d),n }
        temp := self.getbyte(r.pc);
        temp2 := temp2 + shortint(temp);
        temp := self.getbyte(r.pc + 1);
        r.wz := temp2;
        r.pc := r.pc + 2;
        self.putbyte(temp2, temp);
      end;
    $39:
      registro.w := add_16(registro.w, r.sp); // add IX,SP
    $44:
      r.bc.h := registro^.h; { ld B,IXh }
    $45:
      r.bc.h := registro^.l; { ld B,IXl }
    $46:
      begin { ld B,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.bc.h := self.getbyte(temp2);
      end;
    $4C:
      r.bc.l := registro^.h; { ld C,IXh }
    $4D:
      r.bc.l := registro^.l; { ld C,IXl }
    $4E:
      begin { ld C,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.bc.l := self.getbyte(temp2);
      end;
    $54:
      r.de.h := registro^.h; { ld D,IXh }
    $55:
      r.de.h := registro^.l; { ld D,IXl }
    $56:
      begin { ld D,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.de.h := self.getbyte(temp2);
      end;
    $5C:
      r.de.l := registro^.h; { ld E,IXh }
    $5D:
      r.de.l := registro^.l; { ld E,IXh }
    $5E:
      begin { ld E,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.de.l := self.getbyte(temp2);
      end;
    $60:
      registro^.h := r.bc.h; { ld IXh,B }
    $61:
      registro^.h := r.bc.l; { ld IXh,C }
    $62:
      registro^.h := r.de.h; { ld IXh,D }
    $63:
      registro^.h := r.de.l; { ld IXh,E }
    $64:
      ;
    $65:
      registro^.h := registro^.l; { ld IXh,IXl }
    $66:
      begin { ld H,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.hl.h := self.getbyte(temp2);
      end;
    $67:
      registro^.h := r.a; { ld IXh,A }
    $68:
      registro^.l := r.bc.h; { ld IXl,B }
    $69:
      registro^.l := r.bc.l; { ld IXl,C }
    $6A:
      registro^.l := r.de.h; { ld IXl,D }
    $6B:
      registro^.l := r.de.l; { ld IXl,E }
    $6C:
      registro^.l := registro^.h; { ld IXl,IXh }
    $6D:
      ; { ld IXl,IXl }
    $6E:
      begin { ld L,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.hl.l := self.getbyte(temp2);
      end;
    $6F:
      registro^.l := r.a; { ld IXl,A }
    $70:
      begin { ld (IX+d),B }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.bc.h);
      end;
    $71:
      begin { ld (IX+d),C }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.bc.l);
      end;
    $72:
      begin { ld (IX+d),D }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.de.h);
      end;
    $73:
      begin { ld (IX+d),E }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.de.l);
      end;
    $74:
      begin { ld (IX+d),H }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.hl.h);
      end;
    $75:
      begin { ld (IX+d),L }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.hl.l);
      end;
    $77:
      begin { ld (IX+d),A }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        self.putbyte(temp2, r.a);
      end;
    $7C:
      r.a := registro^.h; { ld A,IXh }
    $7D:
      r.a := registro^.l; { ld A,IXl }
    $7E:
      begin { ld A,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        r.a := self.getbyte(temp2);
      end;
    $84:
      add_8(registro^.h); { add A,IXh }
    $85:
      add_8(registro^.l); { add A,IXl }
    $86:
      begin { add A,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        add_8(temp);
      end;
    $8C:
      adc_8(registro^.h); { adc A,IXh }
    $8D:
      adc_8(registro^.l); { adc A,IXl }
    $8E:
      begin { adc A,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        adc_8(temp);
      end;
    $94:
      sub_8(registro^.h); { sub IXh }
    $95:
      sub_8(registro^.l); { sub IXh }
    $96:
      begin { sub (IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        sub_8(temp);
      end;
    $9C:
      sbc_8(registro^.h); { sbc IXh }
    $9D:
      sbc_8(registro^.l); { sbc IXl }
    $9E:
      begin { sbc (IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        sbc_8(temp);
      end;
    $A4:
      and_a(registro^.h); { and IXh }
    $A5:
      and_a(registro^.l); { and IXl }
    $A6:
      begin { and A,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        and_a(temp);
      end;
    $AC:
      xor_a(registro^.h); { xor IXh }
    $AD:
      xor_a(registro^.l); { xor IXl }
    $AE:
      begin { xor A,(IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        xor_a(temp);
      end;
    $B4:
      or_a(registro^.h); { or IXh }
    $B5:
      or_a(registro^.l); { or IXl }
    $B6:
      begin { or (IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        or_a(temp);
      end;
    $BC:
      cp_a(registro^.h); { cp IXh }
    $BD:
      cp_a(registro^.l); { cp IXl }
    $BE:
      begin { cp (IX+d) }
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        temp2 := temp2 + shortint(temp);
        r.wz := temp2;
        temp := self.getbyte(temp2);
        cp_a(temp);
      end;
    $CB, $DD, $FD:
      self.exec_dd_cb(tipo);
    $E1:
      registro.w := self.pop_sp; { pop IX }
    $E3:
      begin { ex (SP),IX }
        posicion.w := self.pop_sp;
        self.push_sp(registro^.w);
        registro^.w := posicion.w;
        r.wz := posicion.w;
      end;
    $E5:
      self.push_sp(registro^.w); { push IX }
    $E9:
      r.pc := registro^.w; { jp IX }
    $F9:
      r.sp := registro^.w; { ld SP,IX }
  else
    r.pc := r.pc - 1;
  end;
  self.estados_demas := self.estados_demas + z80t_dd[instruccion];
end;

procedure cpu_z80.exec_dd_cb(tipo: boolean);
var
  instruccion, tempb: byte;
  temp2: word;
begin
  if tipo then
    temp2 := r.ix.w
  else
    temp2 := r.iy.w;
  // NO, NO y NO se considera un opcode
  instruccion := self.getbyte(r.pc);
  temp2 := temp2 + shortint(instruccion);
  instruccion := self.getbyte(r.pc + 1);
  if @self.m1_raised <> nil then
    self.m1_raised(instruccion);
  r.pc := r.pc + 2;
  r.wz := temp2;
  case instruccion of
    $00:
      begin { ld B,rlc (IX+d) >23t< }
        r.bc.h := self.getbyte(temp2);
        rlc_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $01:
      begin { ld C,rlc (IX+d) >23t< }
        r.bc.l := self.getbyte(temp2);
        rlc_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $02:
      begin { ld D,rlc (IX+d) >23t< }
        r.de.h := self.getbyte(temp2);
        rlc_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $03:
      begin { ld E,rlc (IX+d) >23t< }
        r.de.l := self.getbyte(temp2);
        rlc_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $04:
      begin { ld H,rlc (IX+d) >23t< }
        r.hl.h := self.getbyte(temp2);
        rlc_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $05:
      begin { ld L,rlc (IX+d) >23t< }
        r.hl.l := self.getbyte(temp2);
        rlc_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $06:
      begin { rlc (IX+d) >23t< }
        tempb := self.getbyte(temp2);
        rlc_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $07:
      begin { ld A,rlc (IX+d) >23t< }
        r.a := self.getbyte(temp2);
        rlc_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $08:
      begin { ld B,rrc (IX+d) >23t< }
        r.bc.h := self.getbyte(temp2);
        rrc_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $09:
      begin { ld C,rrc (IX+d) >23t< }
        r.bc.l := self.getbyte(temp2);
        rrc_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $0A:
      begin { ld D,rrc (IX+d) >23t< }
        r.de.h := self.getbyte(temp2);
        rrc_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $0B:
      begin { ld E,rrc (IX+d) >23t< }
        r.de.l := self.getbyte(temp2);
        rrc_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $0C:
      begin { ld H,rrc (IX+d) >23t< }
        r.hl.h := self.getbyte(temp2);
        rrc_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $0D:
      begin { ld L,rlc (IX+d) >23t< }
        r.hl.l := self.getbyte(temp2);
        rrc_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $0E:
      begin { rrc (IX+d) }
        tempb := self.getbyte(temp2);
        rrc_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $0F:
      begin { ld A,rrc (IX+d) }
        r.a := self.getbyte(temp2);
        rrc_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $10:
      begin { ld B,rl (IX+d) }
        r.bc.h := self.getbyte(temp2);
        rl_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $11:
      begin { ld C,rl (IX+d) }
        r.bc.l := self.getbyte(temp2);
        rl_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $12:
      begin { ld D,rl (IX+d) }
        r.de.h := self.getbyte(temp2);
        rl_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $13:
      begin { ld E,rl (IX+d) }
        r.de.l := self.getbyte(temp2);
        rl_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $14:
      begin { ld H,rl (IX+d) }
        r.hl.h := self.getbyte(temp2);
        rl_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $15:
      begin { ld L,rlc (IX+d) }
        r.hl.l := self.getbyte(temp2);
        rl_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $16:
      begin { rl (IX+d) }
        tempb := self.getbyte(temp2);
        rl_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $17:
      begin { ld A,rl (IX+d) }
        r.a := self.getbyte(temp2);
        rl_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $18:
      begin { ld B,rr (IX+d) }
        r.bc.h := self.getbyte(temp2);
        rr_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $19:
      begin { ld C,rr (IX+d) }
        r.bc.l := self.getbyte(temp2);
        rr_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $1A:
      begin { ld D,rr (IX+d) }
        r.de.h := self.getbyte(temp2);
        rr_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $1B:
      begin { ld E,rr (IX+d) }
        r.de.l := self.getbyte(temp2);
        rr_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $1C:
      begin { ld H,rr (IX+d) }
        r.hl.h := self.getbyte(temp2);
        rr_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $1D:
      begin { ld L,rr (IX+d) }
        r.hl.l := self.getbyte(temp2);
        rr_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $1E:
      begin { rr (IX+d) }
        tempb := self.getbyte(temp2);
        rr_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $1F:
      begin { ld A,rr (IX+d) }
        r.a := self.getbyte(temp2);
        rr_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $20:
      begin { ld B,sla (IX+d) }
        r.bc.h := self.getbyte(temp2);
        sla_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $21:
      begin { ld C,sla (IX+d) }
        r.bc.l := self.getbyte(temp2);
        sla_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $22:
      begin { ld D,sla (IX+d) }
        r.de.h := self.getbyte(temp2);
        sla_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $23:
      begin { ld E,sla (IX+d) }
        r.de.l := self.getbyte(temp2);
        sla_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $24:
      begin { ld H,sla (IX+d) }
        r.hl.h := self.getbyte(temp2);
        sla_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $25:
      begin { ld L,sla (IX+d) }
        r.hl.l := self.getbyte(temp2);
        sla_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $26:
      begin { sla (IX+d) }
        tempb := self.getbyte(temp2);
        sla_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $27:
      begin { ld A,sla (IX+d) }
        r.a := self.getbyte(temp2);
        sla_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $28:
      begin // ld B,sra (IX+d)
        r.bc.h := self.getbyte(temp2);
        sra_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $29:
      begin // ld C,sra (IX+d)
        r.bc.l := self.getbyte(temp2);
        sra_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $2A:
      begin // ld D,sra (IX+d)
        r.de.h := self.getbyte(temp2);
        sra_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $2B:
      begin // ld E,sra (IX+d)
        r.de.l := self.getbyte(temp2);
        sra_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $2C:
      begin // ld H,sra (IX+d)
        r.hl.h := self.getbyte(temp2);
        sra_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $2D:
      begin // ld L,sra (IX+d)
        r.hl.l := self.getbyte(temp2);
        sra_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $2E:
      begin // sra (IX+d)
        tempb := self.getbyte(temp2);
        sra_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $2F:
      begin { ld A,sra (IX+d) }
        r.a := self.getbyte(temp2);
        sra_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $30:
      begin { ld B,sll (IX+d) }
        r.bc.h := self.getbyte(temp2);
        sll_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $31:
      begin { ld C,sll (IX+d) }
        r.bc.l := self.getbyte(temp2);
        sll_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $32:
      begin { ld D,sll (IX+d) }
        r.de.h := self.getbyte(temp2);
        sll_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $33:
      begin { ld E,sll (IX+d) }
        r.de.l := self.getbyte(temp2);
        sll_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $34:
      begin { ld H,sll (IX+d) }
        r.hl.h := self.getbyte(temp2);
        sll_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $35:
      begin { ld L,sll (IX+d) }
        r.hl.l := self.getbyte(temp2);
        sll_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $36:
      begin { sll (IX+d) }
        tempb := self.getbyte(temp2);
        sll_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $37:
      begin { ld A,sll (IX+d) }
        r.a := self.getbyte(temp2);
        sll_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $38:
      begin { ld B,srl (IX+d) }
        r.bc.h := self.getbyte(temp2);
        srl_8(@r.bc.h);
        self.putbyte(temp2, r.bc.h);
      end;
    $39:
      begin { ld C,srl (IX+d) }
        r.bc.l := self.getbyte(temp2);
        srl_8(@r.bc.l);
        self.putbyte(temp2, r.bc.l);
      end;
    $3A:
      begin { ld D,srl (IX+d) }
        r.de.h := self.getbyte(temp2);
        srl_8(@r.de.h);
        self.putbyte(temp2, r.de.h);
      end;
    $3B:
      begin { ld E,srl (IX+d) }
        r.de.l := self.getbyte(temp2);
        srl_8(@r.de.l);
        self.putbyte(temp2, r.de.l);
      end;
    $3C:
      begin { ld H,srl (IX+d) }
        r.hl.h := self.getbyte(temp2);
        srl_8(@r.hl.h);
        self.putbyte(temp2, r.hl.h);
      end;
    $3D:
      begin { ld L,srl (IX+d) }
        r.hl.l := self.getbyte(temp2);
        srl_8(@r.hl.l);
        self.putbyte(temp2, r.hl.l);
      end;
    $3E:
      begin { srl (IX+d) }
        tempb := self.getbyte(temp2);
        srl_8(@tempb);
        self.putbyte(temp2, tempb);
      end;
    $3F:
      begin { ld A,srl (IX+d) }
        r.a := self.getbyte(temp2);
        srl_8(@r.a);
        self.putbyte(temp2, r.a);
      end;
    $40 .. $47:
      begin { bit 0,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(0, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $48 .. $4F:
      begin { bit 1,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(1, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $50 .. $57:
      begin { bit 2,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(2, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $58 .. $5F:
      begin { bit 3,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(3, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $60 .. $67:
      begin { bit 4,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(4, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $68 .. $6F:
      begin { bit 5,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(5, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $70 .. $77:
      begin { bit 6,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_8(6, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $78 .. $7F:
      begin { bit 7,(IX+d) }
        tempb := self.getbyte(temp2);
        bit_7(tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $80:
      begin { ld B,res 0,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.bc.h);
      end;
    $81:
      begin { ld C,res 0,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.bc.l);
      end;
    $82:
      begin { ld D,res 0,(IX+d) }
        r.de.h := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.de.h);
      end;
    $83:
      begin { ld E,res 0,(IX+d) }
        r.de.l := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.de.l);
      end;
    $84:
      begin { ld H,res 0,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.hl.h);
      end;
    $85:
      begin { ld L,res 0,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.hl.l);
      end;
    $86:
      begin { res 0,(IX+d) }
        tempb := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, tempb);
      end;
    $87:
      begin // LD A,RES 0,(REGISTER+dd) */
        r.a := self.getbyte(temp2) and $FE;
        self.putbyte(temp2, r.a);
      end;
    $88:
      begin // LD B,RES 1,(REGISTER+dd) */
        r.bc.h := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.bc.h);
      end;
    $89:
      begin // LD C,RES 1,(REGISTER+dd) */
        r.bc.l := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.bc.l);
      end;
    $8A:
      begin // LD D,RES 1,(REGISTER+dd) */
        r.de.h := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.de.h);
      end;
    $8B:
      begin // LD E,RES 1,(REGISTER+dd) */
        r.de.l := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.de.l);
      end;
    $8C:
      begin // LD H,RES 1,(REGISTER+dd) */
        r.hl.h := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.hl.h);
      end;
    $8D:
      begin // LD L,RES 1,(REGISTER+dd) */
        r.hl.l := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.hl.l);
      end;
    $8E:
      begin { res 1,(IX+d) }
        tempb := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, tempb);
      end;
    $8F:
      begin { ld A,res 1,(IX+d) }
        r.a := self.getbyte(temp2) and $FD;
        self.putbyte(temp2, r.a);
      end;
    $90:
      begin { ld B,res 2,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.bc.h);
      end;
    $91:
      begin { ld C,res 2,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.bc.l);
      end;
    $92:
      begin { ld D,res 2,(IX+d) }
        r.de.h := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.de.h);
      end;
    $93:
      begin { ld E,res 2,(IX+d) }
        r.de.l := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.de.l);
      end;
    $94:
      begin { ld H,res 2,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.hl.h);
      end;
    $95:
      begin { ld L,res 2,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.hl.l);
      end;
    $96:
      begin { res 2,(IX+d) }
        tempb := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, tempb);
      end;
    $97:
      begin { ld A,res 2,(IX+d) }
        r.a := self.getbyte(temp2) and $FB;
        self.putbyte(temp2, r.a);
      end;
    $98:
      begin { ld B,res 3,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.bc.h);
      end;
    $99:
      begin { ld C,res 3,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.bc.l);
      end;
    $9A:
      begin { ld D,res 3,(IX+d) }
        r.de.h := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.de.h);
      end;
    $9B:
      begin { ld E,res 3,(IX+d) }
        r.de.l := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.de.l);
      end;
    $9C:
      begin { ld H,res 3,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.hl.h);
      end;
    $9D:
      begin { ld L,res 3,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.hl.l);
      end;
    $9E:
      begin { res 3,(IX+d) }
        tempb := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, tempb);
      end;
    $9F:
      begin { ld A,res 3,(IX+d) }
        r.a := self.getbyte(temp2) and $F7;
        self.putbyte(temp2, r.a);
      end;
    $A0:
      begin { ld B,res 4,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.bc.h);
      end;
    $A1:
      begin { ld C,res 4,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.bc.l);
      end;
    $A2:
      begin { ld D,res 4,(IX+d) }
        r.de.h := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.de.h);
      end;
    $A3:
      begin { ld E,res 4,(IX+d) }
        r.de.l := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.de.l);
      end;
    $A4:
      begin { ld H,res 4,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.hl.h);
      end;
    $A5:
      begin { ld L,res 4,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.hl.l);
      end;
    $A6:
      begin { res 4,(IX+d) }
        tempb := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, tempb);
      end;
    $A7:
      begin { ld A,res 4,(IX+d) }
        r.a := self.getbyte(temp2) and $EF;
        self.putbyte(temp2, r.a);
      end;
    $A8:
      begin { ld B,res 5,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.bc.h);
      end;
    $A9:
      begin { ld C,res 5,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.bc.l);
      end;
    $AA:
      begin { ld D,res 5,(IX+d) }
        r.de.h := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.de.h);
      end;
    $AB:
      begin { ld E,res 5,(IX+d) }
        r.de.l := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.de.l);
      end;
    $AC:
      begin { ld H,res 5,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.hl.h);
      end;
    $AD:
      begin { ld L,res 5,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.hl.l);
      end;
    $AE:
      begin { res 5,(IX+d) }
        tempb := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, tempb);
      end;
    $AF:
      begin { ld A,res 5,(IX+d) }
        r.a := self.getbyte(temp2) and $DF;
        self.putbyte(temp2, r.a);
      end;
    $B0:
      begin { ld B,res 6,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.bc.h);
      end;
    $B1:
      begin { ld C,res 6,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.bc.l);
      end;
    $B2:
      begin { ld D,res 6,(IX+d) }
        r.de.h := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.de.h);
      end;
    $B3:
      begin { ld E,res 6,(IX+d) }
        r.de.l := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.de.l);
      end;
    $B4:
      begin { ld H,res 6,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.hl.h);
      end;
    $B5:
      begin { ld L,res 6,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.hl.l);
      end;
    $B6:
      begin { res 6,(IX+d) }
        tempb := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, tempb);
      end;
    $B7:
      begin { ld A,res 6,(IX+d) }
        r.a := self.getbyte(temp2) and $BF;
        self.putbyte(temp2, r.a);
      end;
    $B8:
      begin { ld B,res 7,(IX+d) }
        r.bc.h := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.bc.h);
      end;
    $B9:
      begin { ld C,res 7,(IX+d) }
        r.bc.l := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.bc.l);
      end;
    $BA:
      begin { ld D,res 7,(IX+d) }
        r.de.h := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.de.h);
      end;
    $BB:
      begin { ld E,res 7,(IX+d) }
        r.de.l := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.de.l);
      end;
    $BC:
      begin { ld H,res 7,(IX+d) }
        r.hl.h := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.hl.h);
      end;
    $BD:
      begin { ld L,res 7,(IX+d) }
        r.hl.l := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.hl.l);
      end;
    $BE:
      begin { res 7,(IX+d) }
        tempb := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, tempb);
      end;
    $BF:
      begin { ld A,res 7,(IX+d) }
        r.a := self.getbyte(temp2) and $7F;
        self.putbyte(temp2, r.a);
      end;
    $C0:
      begin { ld B,set 0,(IX+d) }
        r.bc.h := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.bc.h);
      end;
    $C1:
      begin { ld C,set 0,(IX+d) }
        r.bc.l := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.bc.l);
      end;
    $C2:
      begin { ld D,set 0,(IX+d) }
        r.de.h := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.de.h);
      end;
    $C3:
      begin { ld E,set 0,(IX+d) }
        r.de.l := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.de.l);
      end;
    $C4:
      begin { ld H,set 0,(IX+d) }
        r.hl.h := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.hl.h);
      end;
    $C5:
      begin { ld L,set 0,(IX+d) }
        r.hl.l := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.hl.l);
      end;
    $C6:
      begin { set 0,(IX+d) }
        tempb := self.getbyte(temp2) or 1;
        self.putbyte(temp2, tempb);
      end;
    $C7:
      begin { ld A,set 0,(IX+d) }
        r.a := self.getbyte(temp2) or 1;
        self.putbyte(temp2, r.a);
      end;
    $C8:
      begin { ld B,set 1,(IX+d) }
        r.bc.h := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.bc.h);
      end;
    $C9:
      begin { ld C,set 1,(IX+d) }
        r.bc.l := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.bc.l);
      end;
    $CA:
      begin { ld D,set 1,(IX+d) }
        r.de.h := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.de.h);
      end;
    $CB:
      begin { ld E,set 1,(IX+d) }
        r.de.l := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.de.l);
      end;
    $CC:
      begin { ld H,set 1,(IX+d) }
        r.hl.h := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.hl.h);
      end;
    $CD:
      begin { ld L,set 1,(IX+d) }
        r.hl.l := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.hl.l);
      end;
    $CE:
      begin { set 1,(IX+d) }
        tempb := self.getbyte(temp2) or 2;
        self.putbyte(temp2, tempb);
      end;
    $CF:
      begin { ld A,set 1,(IX+d) }
        r.a := self.getbyte(temp2) or 2;
        self.putbyte(temp2, r.a);
      end;
    $D0:
      begin { ld B,set 2,(IX+d) }
        r.bc.h := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.bc.h);
      end;
    $D1:
      begin { ld C,set 2,(IX+d) }
        r.bc.l := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.bc.l);
      end;
    $D2:
      begin { ld D,set 2,(IX+d) }
        r.de.h := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.de.h);
      end;
    $D3:
      begin { ld E,set 2,(IX+d) }
        r.de.l := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.de.l);
      end;
    $D4:
      begin { ld H,set 2,(IX+d) }
        r.hl.h := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.hl.h);
      end;
    $D5:
      begin { ld L,set 2,(IX+d) }
        r.hl.l := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.hl.l);
      end;
    $D6:
      begin { set 2,(IX+d) }
        tempb := self.getbyte(temp2) or 4;
        self.putbyte(temp2, tempb);
      end;
    $D7:
      begin { ld A,set 2,(IX+d) }
        r.a := self.getbyte(temp2) or 4;
        self.putbyte(temp2, r.a);
      end;
    $D8:
      begin { ld B,set 3,(IX+d) }
        r.bc.h := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.bc.h);
      end;
    $D9:
      begin { ld C,set 3,(IX+d) }
        r.bc.l := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.bc.l);
      end;
    $DA:
      begin { ld D,set 3,(IX+d) }
        r.de.h := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.de.h);
      end;
    $DB:
      begin { ld E,set 3,(IX+d) }
        r.de.l := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.de.l);
      end;
    $DC:
      begin { ld H,set 3,(IX+d) }
        r.hl.h := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.hl.h);
      end;
    $DD:
      begin { ld L,set 3,(IX+d) }
        r.hl.l := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.hl.l);
      end;
    $DE:
      begin { set 3,(IX+d) }
        tempb := self.getbyte(temp2) or 8;
        self.putbyte(temp2, tempb);
      end;
    $DF:
      begin { ld A,set 3,(IX+d) }
        r.a := self.getbyte(temp2) or 8;
        self.putbyte(temp2, r.a);
      end;
    $E0:
      begin { ld B,set 4,(IX+d) }
        r.bc.h := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.bc.h);
      end;
    $E1:
      begin { ld C,set 4,(IX+d) }
        r.bc.l := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.bc.l);
      end;
    $E2:
      begin { ld D,set 4,(IX+d) }
        r.de.h := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.de.h);
      end;
    $E3:
      begin { ld E,set 4,(IX+d) }
        r.de.l := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.de.l);
      end;
    $E4:
      begin { ld H,set 4,(IX+d) }
        r.hl.h := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.hl.h);
      end;
    $E5:
      begin { ld L,set 4,(IX+d) }
        r.hl.l := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.hl.l);
      end;
    $E6:
      begin { set 4,(IX+d) }
        tempb := self.getbyte(temp2) or $10;
        self.putbyte(temp2, tempb);
      end;
    $E7:
      begin { ld A,set 4,(IX+d) }
        r.a := self.getbyte(temp2) or $10;
        self.putbyte(temp2, r.a);
      end;
    $E8:
      begin { ld B,set 5,(IX+d) }
        r.bc.h := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.bc.h);
      end;
    $E9:
      begin { ld C,set 5,(IX+d) }
        r.bc.l := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.bc.l);
      end;
    $EA:
      begin { ld D,set 5,(IX+d) }
        r.de.h := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.de.h);
      end;
    $EB:
      begin { ld E,set 5,(IX+d) }
        r.de.l := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.de.l);
      end;
    $EC:
      begin { ld H,set 5,(IX+d) }
        r.hl.h := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.hl.h);
      end;
    $ED:
      begin { ld L,set 5,(IX+d) }
        r.hl.l := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.hl.l);
      end;
    $EE:
      begin { set 5,(IX+d) }
        tempb := self.getbyte(temp2) or $20;
        self.putbyte(temp2, tempb);
      end;
    $EF:
      begin { ld A,set 5,(IX+d) }
        r.a := self.getbyte(temp2) or $20;
        self.putbyte(temp2, r.a);
      end;
    $F0:
      begin { ld B,set 6,(IX+d) }
        r.bc.h := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.bc.h);
      end;
    $F1:
      begin { ld C,set 6,(IX+d) }
        r.bc.l := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.bc.l);
      end;
    $F2:
      begin { ld D,set 6,(IX+d) }
        r.de.h := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.de.h);
      end;
    $F3:
      begin { ld E,set 6,(IX+d) }
        r.de.l := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.de.l);
      end;
    $F4:
      begin { ld H,set 6,(IX+d) }
        r.hl.h := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.hl.h);
      end;
    $F5:
      begin { ld L,set 6,(IX+d) }
        r.hl.l := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.hl.l);
      end;
    $F6:
      begin { set 6,(IX+d) }
        tempb := self.getbyte(temp2) or $40;
        self.putbyte(temp2, tempb);
      end;
    $F7:
      begin { ld A,set 6,(IX+d) }
        r.a := self.getbyte(temp2) or $40;
        self.putbyte(temp2, r.a);
      end;
    $F8:
      begin { ld B,set 7,(IX+d) }
        r.bc.h := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.bc.h);
      end;
    $F9:
      begin { ld C,set 7,(IX+d) }
        r.bc.l := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.bc.l);
      end;
    $FA:
      begin { ld D,set 7,(IX+d) }
        r.de.h := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.de.h);
      end;
    $FB:
      begin { ld E,set 7,(IX+d) }
        r.de.l := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.de.l);
      end;
    $FC:
      begin { ld H,set 7,(IX+d) }
        r.hl.h := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.hl.h);
      end;
    $FD:
      begin { ld L,set 7,(IX+d) }
        r.hl.l := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.hl.l);
      end;
    $FE:
      begin { set 7,(IX+d) }
        tempb := self.getbyte(temp2) or $80;
        self.putbyte(temp2, tempb);
      end;
    $FF:
      begin { ld A,set 7,(IX+d) }
        r.a := self.getbyte(temp2) or $80;
        self.putbyte(temp2, r.a);
      end;
  end;
  self.estados_demas := self.estados_demas + z80t_ddcb[instruccion];
end;

procedure cpu_z80.exec_ed;
var
  instruccion, temp, temp2, temp3: byte;
  posicion: parejas;
  tempw: word;
begin
  self.opcode := True;
  instruccion := self.getbyte(r.pc);
  if @self.m1_raised <> nil then
    self.m1_raised(instruccion);
  self.opcode := False;
  r.pc := r.pc + 1;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  case instruccion of
    $00 .. $3F, $77, $7F .. $9F, $A4 .. $A7, $AC .. $AF, $B4 .. $B7, $BC .. $FF:
      ; { nop*2 }
    $40:
      begin { in B,(c) }
        r.bc.h := self.in_port(r.bc.w);
        r.f.z := (r.bc.h = 0);
        r.f.s := (r.bc.h and $80) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.p_v := paridad[r.bc.h];
        r.f.n := False;
        r.f.h := False;
      end;
    $41:
      self.out_port(r.bc.w, r.bc.h); { out (C),B }
    $42:
      r.hl.w := sbc_hl(r.bc.w); // sbc HL,BC
    $43:
      begin { ld (nn),BC }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;
        self.putbyte(posicion.w, r.bc.l);
        self.putbyte(posicion.w + 1, r.bc.h);
        r.wz := posicion.w + 1;
      end;
    $44, $4C, $54, $5C, $64, $6C, $74, $7C:
      begin { neg }
        temp := r.a;
        r.a := 0;
        sub_8(temp);
      end;
    $45, $55, $65, $75:
      begin { retn }
        r.pc := pop_sp;
        r.wz := r.pc;
        r.iff1 := r.iff2;
      end;
    $46, $4E, $66, $6E:
      r.im := 0; { im 0 }
    $47:
      r.i := r.a; { ld I,A }
    $48:
      begin { in C,(C) }
        r.bc.l := self.in_port(r.bc.w);
        r.f.z := (r.bc.l = 0);
        r.f.s := (r.bc.l and $80) <> 0;
        r.f.bit3 := (r.bc.l and 8) <> 0;
        r.f.bit5 := (r.bc.l and $20) <> 0;
        r.f.p_v := paridad[r.bc.l];
        r.f.n := False;
        r.f.h := False;
      end;
    $49:
      self.out_port(r.bc.w, r.bc.l); { out (C),C }
    $4A:
      r.hl.w := adc_hl(r.bc.w); // adc HL,BC
    $4B:
      begin { ld BC,(nn) }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        r.bc.l := self.getbyte(posicion.w);
        r.bc.h := self.getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    { 4c: neg }
    $4D, $5D, $6D, $7D:
      begin { reti }
        r.iff1 := r.iff2;
        r.pc := pop_sp;
        r.wz := r.pc;
        if self.daisy then
          z80daisy_reti;
      end;
    { 4e: im 0 }
    $4F:
      r.r := r.a; { ld R,A }
    $50:
      begin { in D,(c) }
        r.de.h := self.in_port(r.bc.w);
        r.f.z := (r.de.h = 0);
        r.f.s := (r.de.h and $80) <> 0;
        r.f.bit3 := (r.de.h and 8) <> 0;
        r.f.bit5 := (r.de.h and $20) <> 0;
        r.f.p_v := paridad[r.de.h];
        r.f.n := False;
        r.f.h := False;
      end;
    $51:
      self.out_port(r.bc.w, r.de.h); { out (C),D }
    $52:
      r.hl.w := sbc_hl(r.de.w); // sbc HL,DE
    $53:
      begin { ld (nn),DE }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        self.putbyte(posicion.w, r.de.l);
        self.putbyte(posicion.w + 1, r.de.h);
        r.wz := posicion.w + 1;
      end;
    { 54: neg
      $55:retn }
    $56, $76:
      r.im := 1; { im 1 }
    $57:
      begin { ld A,I }
        r.a := r.i;
        r.f.s := False;
        r.f.z := (r.a = 0);
        r.f.bit5 := False;
        r.f.h := False;
        r.f.bit3 := False;
        r.f.p_v := r.iff2;
        r.f.n := False;
      end;
    $58:
      begin { in E,(C) }
        r.de.l := self.in_port(r.bc.w);
        r.f.z := (r.de.l = 0);
        r.f.s := (r.de.l and $80) <> 0;
        r.f.bit3 := (r.de.l and 8) <> 0;
        r.f.bit5 := (r.de.l and $20) <> 0;
        r.f.p_v := paridad[r.de.l];
        r.f.n := False;
        r.f.h := False;
      end;
    $59:
      self.out_port(r.bc.w, r.de.l); { out (C),E }
    $5A:
      r.hl.w := adc_hl(r.de.w); // adc HL,DE
    $5B:
      begin { ld DE,(nn) }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        r.de.l := self.getbyte(posicion.w);
        r.de.h := self.getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    { 5c:neg
      5d:reti }
    $5E, $7E:
      r.im := 2; { im 2 }
    $5F:
      begin { ld A,R }
        r.a := r.r;
        r.f.h := False;
        r.f.n := False;
        r.f.p_v := r.iff2;
        r.f.bit5 := False;
        r.f.bit3 := False;
        r.f.s := False;
        r.f.z := (r.a = 0);
      end;
    $60:
      begin { in H,(c) }
        r.hl.h := self.in_port(r.bc.w);
        r.f.z := (r.hl.h = 0);
        r.f.s := (r.hl.h and $80) <> 0;
        r.f.bit3 := (r.hl.h and 8) <> 0;
        r.f.bit5 := (r.hl.h and $20) <> 0;
        r.f.p_v := paridad[r.hl.h];
        r.f.n := False;
        r.f.h := False;
      end;
    $61:
      self.out_port(r.bc.w, r.hl.h); { out (C),H }
    $62:
      r.hl.w := sbc_hl(r.hl.w); // sbc HL,HL
    $63:
      begin { ld (nn),HL }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        self.putbyte(posicion.w, r.hl.l);
        self.putbyte(posicion.w + 1, r.hl.h);
        r.wz := posicion.w + 1;
      end;
    { 64:neg
      $65:retn
      $66:im 0 }
    $67:
      begin // rrd
        temp2 := self.getbyte(r.hl.w);
        r.wz := r.hl.w + 1;
        temp := (r.a and $F) * 16;
        r.a := (r.a and $F0) + (temp2 and $F);
        temp2 := (temp2 div 16) + temp;
        self.putbyte(r.hl.w, temp2);
        r.f.s := (r.a and $80) <> 0;
        r.f.z := (r.a = 0);
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.h := False;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.p_v := paridad[r.a];
        r.f.n := False;
      end;
    $68:
      begin { in L,(c) }
        r.hl.l := self.in_port(r.bc.w);
        r.f.z := (r.hl.l = 0);
        r.f.s := (r.hl.l and $80) <> 0;
        r.f.bit3 := (r.hl.l and 8) <> 0;
        r.f.bit5 := (r.hl.l and $20) <> 0;
        r.f.p_v := paridad[r.hl.l];
        r.f.n := False;
        r.f.h := False;
      end;
    $69:
      self.out_port(r.bc.w, r.hl.l); { out (C),L }
    $6A:
      r.hl.w := adc_hl(r.hl.w); // adc HL,HL
    $6B:
      begin { ld HL,(nn) }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        r.hl.l := self.getbyte(posicion.w);
        r.hl.h := self.getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    { 6c:neg
      $6d:reti
      $6e:im 0 }
    $6F:
      begin // rld
        temp2 := self.getbyte(r.hl.w);
        r.wz := r.hl.w + 1;
        temp := r.a and $0F;
        r.a := (r.a and $F0) + (temp2 div 16);
        temp2 := (temp2 * 16) + temp;
        self.putbyte(r.hl.w, temp2);
        r.f.s := (r.a and $80) <> 0;
        r.f.z := (r.a = 0);
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.h := False;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.p_v := paridad[r.a];
        r.f.n := False;
      end;
    $70:
      begin { in (C) }
        temp := self.in_port(r.bc.w);
        r.f.z := (temp = 0);
        r.f.s := (temp and $80) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
        r.f.bit5 := (temp and $20) <> 0;
        r.f.p_v := paridad[temp];
        r.f.n := False;
        r.f.h := False;
      end;
    $71:
      self.out_port(r.bc.w, 0); { out (C),0 }
    $72:
      r.hl.w := sbc_hl(r.sp); // sbc HL,SP
    $73:
      begin { ld (nn),SP }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        self.putbyte(posicion.w, r.sp and $FF);
        self.putbyte(posicion.w + 1, r.sp shr 8);
        r.wz := posicion.w + 1;
      end;
    { 74:neg
      $75:retn
      $76:im 1
      $77:nop*2 }
    $78:
      begin // in A,(C)
        r.a := self.in_port(r.bc.w);
        r.f.z := (r.a = 0);
        r.f.s := (r.a and $80) <> 0;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.p_v := paridad[r.a];
        r.f.n := False;
        r.f.h := False;
        r.wz := r.bc.w + 1;
      end;
    $79:
      begin // out (C),A
        self.out_port(r.bc.w, r.a);
        r.wz := r.bc.w + 1;
      end;
    $7A:
      r.hl.w := adc_hl(r.sp); // adc HL,SP
    $7B:
      begin { ld SP,(nn) }
        posicion.h := self.getbyte(r.pc + 1);
        posicion.l := self.getbyte(r.pc);
        r.pc := r.pc + 2;;
        r.sp := self.getbyte(posicion.w) + (self.getbyte(posicion.w + 1) shl 8);
        r.wz := posicion.w + 1;
      end;
    { 7c:neg
      $7d:reti
      $7e:im 2
      $7f..9c:nop*2 }
    $A0:
      begin // ldi
        temp := self.getbyte(r.hl.w);
        r.hl.w := r.hl.w + 1;
        self.putbyte(r.de.w, temp);
        r.de.w := r.de.w + 1;
        r.bc.w := r.bc.w - 1;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := False;
        r.f.h := False;
        temp := temp + r.a;
        r.f.bit5 := (temp and 2) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
      end;
    $A1:
      begin // cpi el primer programa que lo usa una demo!!!
        // 08 de feb 2003
        temp2 := self.getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz + 1;
        r.hl.w := r.hl.w + 1;
        r.bc.w := r.bc.w - 1;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := True;
        r.f.s := (temp and $80) <> 0;
        r.f.z := (temp = 0);
        r.f.h := (temp3 and 16) <> 0;
        r.f.bit5 := ((temp - ((temp3 and 16) shr 4)) and 2) <> 0;
        r.f.bit3 := ((temp - ((temp3 shr 4) and 1)) and 8) <> 0;
      end;
    $A2:
      begin // ini
        // Primer juego que lo usa Titan 09 de Sep 2006
        temp := self.in_port(r.bc.w);
        r.wz := r.bc.w + 1;
        r.bc.h := r.bc.h - 1;
        self.putbyte(r.hl.w, temp);
        r.hl.w := r.hl.w + 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.bc.l + 1;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
      end;
    $A3:
      begin // outi el primer programa que lo usa una demo!!!
        // 08 de feb 2003
        temp := self.getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w + 1;
        self.out_port(r.bc.w, self.getbyte(r.hl.w));
        r.hl.w := r.hl.w + 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.hl.l;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
      end;
{$A4..$A7:NOP*2}
    $A8:
      begin { ldd }
        temp := self.getbyte(r.hl.w);
        r.hl.w := r.hl.w - 1;
        self.putbyte(r.de.w, temp);
        r.de.w := r.de.w - 1;
        r.bc.w := r.bc.w - 1;
        r.f.n := False;
        r.f.h := False;
        r.f.p_v := (r.bc.w <> 0);
        r.f.bit5 := ((r.a + temp) and $2) <> 0;
        r.f.bit3 := ((r.a + temp) and $8) <> 0;
      end;
    $A9:
      begin // cpd el primer juego que la usa Ace 2
        // 20-09-04
        temp2 := self.getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz - 1;
        r.hl.w := r.hl.w - 1;
        r.bc.w := r.bc.w - 1;
        r.f.s := (temp and $80) <> 0;
        r.f.z := (temp = 0);
        r.f.h := (temp3 and 16) <> 0;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := True;
        r.f.bit5 := ((temp - ((temp3 and 16) shr 4)) and 2) <> 0;
        r.f.bit3 := ((temp - ((temp3 shr 4) and 1)) and 8) <> 0;
      end;
    $AA:
      begin // ind a�adido el 03-12-08 usado por CPC test
        temp := self.in_port(r.bc.w);
        r.wz := r.bc.w - 1;
        r.bc.h := r.bc.h - 1;
        self.putbyte(r.hl.w, temp);
        r.hl.w := r.hl.w - 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.bc.l - 1;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
      end;
    $AB:
      begin // outd
        temp := self.getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w - 1;
        self.out_port(r.bc.w, self.getbyte(r.hl.w));
        dec(r.hl.w);
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.hl.l;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
      end;
    { ac..$af:nop*2 }
    $B0:
      begin // ldir
        temp := self.getbyte(r.hl.w);
        r.hl.w := r.hl.w + 1;
        self.putbyte(r.de.w, temp);
        r.de.w := r.de.w + 1;
        r.bc.w := r.bc.w - 1;
        if (r.bc.w <> 0) then
        begin
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := False;
        r.f.h := False;
        temp := temp + r.a;
        r.f.bit5 := (temp and 2) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
      end;
    $B1:
      begin // cpir
        temp2 := self.getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz + 1;
        r.hl.w := r.hl.w + 1;
        r.bc.w := r.bc.w - 1;
        r.f.s := (temp and $80) <> 0;
        r.f.z := (temp = 0);
        r.f.h := (temp3 and 16) <> 0;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := True;
        r.f.bit5 := ((temp - ((temp3 and 16) shr 4)) and 2) <> 0;
        r.f.bit3 := ((temp - ((temp3 shr 4) and 1)) and 8) <> 0;
        If (r.f.p_v and not(r.f.z)) then
        begin
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
      end;
    $B2:
      begin // inir a�adido el 05-10-08, lo usa una rom de Coleco!
        temp := self.in_port(r.bc.w);
        r.wz := r.bc.w + 1;
        r.bc.h := r.bc.h - 1;
        self.putbyte(r.hl.w, temp);
        r.hl.w := r.hl.w + 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.bc.l + 1;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
        if r.bc.h <> 0 then
        begin
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      end;
    $B3:
      begin // otir a�adido el dia 18-09-04
        temp := self.getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w + 1;
        self.out_port(r.bc.w, self.getbyte(r.hl.w));
        r.hl.w := r.hl.w + 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.hl.l;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        if r.bc.h <> 0 then
        begin
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
        end;
      end;
    { $b4..$b7:nop*2 }
    $B8:
      begin // lddr
        temp := self.getbyte(r.hl.w);
        r.hl.w := r.hl.w - 1;
        self.putbyte(r.de.w, temp);
        r.de.w := r.de.w - 1;
        r.bc.w := r.bc.w - 1;
        r.f.n := False;
        r.f.h := False;
        r.f.p_v := (r.bc.w <> 0);
        r.f.bit5 := ((r.a + temp) and $2) <> 0;
        r.f.bit3 := ((r.a + temp) and $8) <> 0;
        if (r.bc.w <> 0) then
        begin
          r.pc := r.pc - 2;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          r.wz := r.pc + 1;
        end;
      end;
    $B9:
      begin // cpdr
        temp2 := self.getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz - 1;
        r.hl.w := r.hl.w - 1;
        r.bc.w := r.bc.w - 1;
        r.f.s := (temp and $80) <> 0;
        r.f.z := (temp = 0);
        r.f.h := (temp3 and 16) <> 0;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := True;
        r.f.bit5 := ((temp - ((temp3 and 16) shr 4)) and 2) <> 0;
        r.f.bit3 := ((temp - ((temp3 shr 4) and 1)) and 8) <> 0;
        if r.f.p_v and not(r.f.z) then
        begin
          r.pc := r.pc - 2;
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          r.wz := r.pc + 1;
        end;
      end;
    $BA:
      begin // indr  >16t<
        temp := self.in_port(r.bc.w);
        self.putbyte(r.hl.w, temp);
        r.wz := r.bc.w - 1;
        r.bc.h := r.bc.h - 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.bc.l - 1;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
        if (r.bc.h <> 0) then
        begin
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
        r.hl.w := r.hl.w - 1;
      end;
    $BB:
      begin // otdr
        temp := self.getbyte(r.hl.w);
        dec(r.bc.h);
        r.wz := r.bc.w - 1;
        self.out_port(r.bc.w, temp);
        dec(r.hl.w);
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.hl.l;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
        if (r.bc.h <> 0) then
        begin
          self.estados_demas := self.estados_demas + z80t_ex[instruccion];
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
      end;
  end;
  self.estados_demas := self.estados_demas + z80t_ed[instruccion];
end;

end.
