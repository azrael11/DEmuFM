unit konami;

interface

uses
  WinApi.Windows,
  main_engine,
  FMX.Dialogs,
  System.SysUtils,
  System.UITypes,
  timer_engine,
  m6809,
  cpu_misc;

type
  tset_lines = procedure(valor: byte);

  cpu_konami = class(cpu_m6809)
  public
    constructor create(clock: dword; frames_div: word);
    procedure run(maximo: single);
    procedure change_set_lines(tset_lines_call: tset_lines);
  private
    set_lines_call: tset_lines;
    // Llamadas IRQ
    function call_irq: byte;
    function call_firq: byte;
    // Misc Func
    function get_indexed: word;
    procedure trf(valor: byte);
    procedure trf_ex(valor: byte);
  end;

var
  konami_0: cpu_konami;

implementation

const
  estados_t: array [0 .. 255] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 5, 5, 4, 4, // 0
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 10
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // 20
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 8, 6, // 30
    4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 5, 3, 5, 3, 5, 3, // 40
    5, 3, 5, 3, 5, 4, 5, 4, 3, 3, 3, 3, 3, 0, 0, 0, // 50
    3, 3, 3, 3, 0, 3, 3, 3, 4, 4, 4, 4, 0, 4, 4, 4, // 60
    3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 0, 4, 4, 4, // 70
    1, 1, 4, 2, 2, 4, 2, 2, 4, 2, 2, 4, 2, 2, 4, 4, // 80
    2, 2, 4, 2, 2, 4, 2, 2, 4, 2, 2, 4, 2, 2, 4, 4, // 90
    2, 2, 4, 2, 0, 2, 2, 0, 1, 5, 7, 9, 3, 3, 2, 0, // A0
    3, 2, 2, 11, 21, 10, 1, 0, 2, 0, 0, 0, 2, 0, 2, 0, // B0
    0, 0, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 1, // C0
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // D0
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // E0
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0); // F0

  paginacion: array [0 .. 255] of byte = (
    // 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
    $F, $F, $F, $F, $F, $F, $F, $F, 4, 4, 4, 4, 2, 2, 2, 2, // 00
    2, 2, 6, 6, 2, 2, 6, 6, 2, 2, 6, 6, 2, 2, 6, 6, // 10
    2, 2, 6, 6, 2, 2, 6, 6, 2, 2, 6, 6, 2, 2, 6, 6, // 20
    2, 2, 6, 6, 2, 2, 6, 6, 2, 6, 4, 4, 2, 2, 2, 2, // 30
    3, 9, 3, 9, 3, 9, 3, 9, 3, 9, 3, 9, 3, 9, 3, 9, // 40
    3, 9, 3, 9, 3, 9, 3, 9, 4, 4, 4, 4, 4, $F, $F, $F, // 50
    2, 2, 2, 2, $F, 2, 2, 2, 3, 3, 3, 3, $F, 3, 3, 3, // 60
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, $F, 3, 3, 3, // 70
    0, 0, 4, 0, 0, 4, 0, 0, 4, 0, 0, 4, 0, 0, 4, 0, // 80
    0, 0, 4, 0, 0, 4, 0, 0, 4, 0, 0, 4, 0, 0, 4, 0, // 90
    0, 0, 4, 4, $F, 4, 4, $F, 4, 4, 2, 3, 2, 2, 0, $F, // a0
    0, 0, 0, 0, 0, 0, 0, $F, 2, $F, $F, $F, 2, $F, 2, $F, // b0
    $F, $F, 0, 4, 0, 4, 4, 4, $F, 4, 0, 4, 0, 0, 0, 0, // c0
    0, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, // d0
    $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, // e0
    $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F, $F); // f0

constructor cpu_konami.create(clock: dword; frames_div: word);
begin
  getmem(self.r, sizeof(reg_m6809));
  fillchar(self.r^, sizeof(reg_m6809), 0);
  clock := clock div 4;
  self.numero_cpu := cpu_main_init(clock);
  self.clock := clock;
  self.tframes := (clock / frames_div) / machine_calls.fps_max;
end;

procedure cpu_konami.change_set_lines(tset_lines_call: tset_lines);
begin
  self.set_lines_call := tset_lines_call;
end;

function cpu_konami.call_irq: byte;
begin
  self.push_sw(r.pc);
  self.push_sw(r.u);
  self.push_sw(r.y);
  self.push_sw(r.x);
  self.push_s(r.dp);
  self.push_s(r.d.b);
  self.push_s(r.d.a);
  r.cc.e := true;
  self.push_s(self.dame_pila);
  call_irq := 19;
  r.pc := self.getword($FFF8);
  r.cc.i := true;
  if self.pedir_irq = HOLD_LINE then
    self.pedir_irq := CLEAR_LINE;
end;

function cpu_konami.call_firq: byte;
begin
  r.cc.e := false;
  self.push_sw(r.pc);
  self.push_s(self.dame_pila);
  call_firq := 10;
  r.cc.f := true;
  r.cc.i := true;
  r.pc := self.getword($FFF6);
  if self.pedir_firq = HOLD_LINE then
    self.pedir_firq := CLEAR_LINE;
end;

procedure cpu_konami.trf(valor: byte);
var
  temp: word;
begin
  case (valor and 7) of
    $0:
      temp := r.d.a; // A
    $1:
      temp := r.d.b; // B
    $2:
      temp := r.x; // X
    $3:
      temp := r.y; // Y
    $4:
      temp := r.s; // S
    $5:
      temp := r.u; // U
  end;
  case ((valor shr 4) and 7) of
    $0:
      r.d.a := temp; // A
    $1:
      r.d.b := temp; // B
    $2:
      r.x := temp; // X
    $3:
      r.y := temp; // Y
    $4:
      r.s := temp; // S
    $5:
      r.u := temp; // U
  end;
end;

procedure cpu_konami.trf_ex(valor: byte);
var
  temp1, temp2: word;
begin
  case (valor and 7) of
    $0:
      temp1 := r.d.a; // A
    $1:
      temp1 := r.d.b; // B
    $2:
      temp1 := r.x; // X
    $3:
      temp1 := r.y; // Y
    $4:
      temp1 := r.s; // S
    $5:
      temp1 := r.u; // U
  end;
  case ((valor shr 4) and 7) of
    $0:
      temp2 := r.d.a; // A
    $1:
      temp2 := r.d.b; // B
    $2:
      temp2 := r.x; // X
    $3:
      temp2 := r.y; // Y
    $4:
      temp2 := r.s; // S
    $5:
      temp2 := r.u; // U
  end;
  case (valor and 7) of
    $0:
      r.d.a := temp2; // A
    $1:
      r.d.b := temp2; // B
    $2:
      r.x := temp2; // X
    $3:
      r.y := temp2; // Y
    $4:
      r.s := temp2; // S
    $5:
      r.u := temp2; // U
  end;
  case ((valor shr 4) and 7) of
    $0:
      r.d.a := temp1; // A
    $1:
      r.d.b := temp1; // B
    $2:
      r.x := temp1; // X
    $3:
      r.y := temp1; // Y
    $4:
      r.s := temp1; // S
    $5:
      r.u := temp1; // U
  end;
end;

function cpu_konami.get_indexed: word;
var
  iindexed, temp: byte;
  origen: pword;
  direccion, temp2: word;
begin
  iindexed := self.getbyte(r.pc); // Hay que añadir 1 estado por cojer un byte...
  r.pc := r.pc + 1;
  case (iindexed and $70) of
    $20:
      origen := @r.x;
    $30:
      origen := @r.y;
    $50:
      origen := @r.u;
    $60:
      origen := @r.s;
    $70:
      origen := @r.pc;
  end;
  direccion := $FFFF;
  case (iindexed and $F7) of
    7:
      begin // =
        direccion := self.getword(r.pc);
        r.pc := r.pc + 2;
        self.estados_demas := self.estados_demas + 1 + 2;
      end;
    $20, $30, $50, $60, $70:
      begin // reg+
        direccion := origen^;
        origen^ := origen^ + 1;
        self.estados_demas := self.estados_demas + 1 + 2;
      end;
    $21, $31, $51, $61, $71:
      begin // reg++
        direccion := origen^;
        origen^ := origen^ + 2;
        self.estados_demas := self.estados_demas + 1 + 3;
      end;
    $22, $32, $52, $62, $72:
      begin // -reg
        origen^ := origen^ - 1;
        direccion := origen^;
        self.estados_demas := self.estados_demas + 1 + 3;
      end;
    $23, $33, $53, $63, $73:
      begin // --reg
        origen^ := origen^ - 2;
        direccion := origen^;
        self.estados_demas := self.estados_demas + 1 + 3;
      end;
    $24, $34, $54, $64, $74:
      begin // reg + deplazamiento 8bits
        direccion := origen^;
        temp := self.getbyte(r.pc);
        r.pc := r.pc + 1;
        direccion := direccion + shortint(temp);
        self.estados_demas := self.estados_demas + 1 + 2;
      end;
    $25, $35, $55, $65, $75:
      begin // reg + deplazamiento 16bits
        direccion := origen^;
        temp2 := self.getword(r.pc);
        r.pc := r.pc + 2;
        direccion := direccion + smallint(temp2);
        self.estados_demas := self.estados_demas + 5 + 1;
      end;
    $26, $36, $56, $66, $76:
      begin // =
        direccion := origen^;
        self.estados_demas := self.estados_demas + 1;
      end;
    $C4:
      begin
        direccion := (self.r.dp shl 8) + self.getbyte(r.pc);
        r.pc := r.pc + 1;
        self.estados_demas := self.estados_demas + 1 + 1;
      end;
    $A0, $B0, $D0, $E0, $F0:
      begin // reg + r.d.a
        direccion := origen^ + shortint(r.d.a);
        self.estados_demas := self.estados_demas + 1 + 1;
      end;

    $A1, $B1, $D1, $E1, $F1:
      begin // reg + r.d.b
        direccion := origen^ + shortint(r.d.b);
        self.estados_demas := self.estados_demas + 1 + 1;
      end;
    $A7, $B7, $D7, $E7, $F7:
      begin // reg + r.d.w
        direccion := origen^ + smallint(r.d.w);
        self.estados_demas := self.estados_demas + 1 + 4;
      end;
  else
    MessageDlg('Unknown indexed. PC=' + inttohex(r.pc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0);
  end;
  if (iindexed and $8) <> 0 then
  begin
    direccion := self.getword(direccion);
    self.estados_demas := self.estados_demas + 2;
  end;
  get_indexed := direccion;
end;

// Functions
{$I m6809.inc}

procedure cpu_konami.run(maximo: single);
var
  tempb, tempb2, cf, instruccion, numero: byte;
  tempw, posicion: word;
  templ: dword;
begin
  self.contador := 0;
  while self.contador < maximo do
  begin
    if self.pedir_reset <> CLEAR_LINE then
    begin
      tempb := self.pedir_reset;
      self.reset;
      if tempb = ASSERT_LINE then
        self.pedir_reset := ASSERT_LINE;
      self.contador := trunc(maximo);
      exit;
    end;
    self.r.old_pc := self.r.pc;
    self.estados_demas := 0;
    if ((self.pedir_firq <> CLEAR_LINE) and not(r.cc.f)) then
      self.estados_demas := self.call_firq
    else if ((self.pedir_irq <> CLEAR_LINE) and not(r.cc.i)) then
      self.estados_demas := self.call_irq;
    self.opcode := true;
    instruccion := self.getbyte(r.pc);
    r.pc := r.pc + 1;
    self.opcode := false;
    case paginacion[instruccion] of
      0:
        ; // implicito 0T
      2:
        begin // inmediato byte
          numero := self.getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      3:
        begin // EXTENDED 3T
          posicion := self.getword(r.pc);
          r.pc := r.pc + 2;
        end;
      4:
        posicion := self.get_indexed; // INDEXED Los estados T son variables
      6:
        numero := self.getbyte(get_indexed); // indexado indirecto byte
      9:
        posicion := self.getword(get_indexed); // indexado indirecto word
      $F:
        MessageDlg('Konami CPU ' + inttostr(self.numero_cpu) + ' instruction: ' + inttohex(instruccion, 2) + ' unknown. PC=' + inttohex(r.pc, 10) + ' OLD_PC=' + inttohex(self.r.old_pc, 10), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], 0)
    end; // del case!!
    case instruccion of
      $08:
        begin // leax 2T
          r.x := posicion;
          r.cc.z := (r.x = 0);
        end;
      $09:
        begin // leay 2T
          r.y := posicion;
          r.cc.z := (r.y = 0);
        end;
      $0A:
        r.u := posicion; // leau 2T
      $0B:
        r.s := posicion; // leas 2T
      $0C:
        begin // pshs 5T
          if (numero and $80) <> 0 then
          begin
            self.push_sw(r.pc);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $40) <> 0 then
          begin
            self.push_sw(r.u);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $20) <> 0 then
          begin
            self.push_sw(r.y);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $10) <> 0 then
          begin
            self.push_sw(r.x);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $8) <> 0 then
          begin
            self.push_s(r.dp);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $4) <> 0 then
          begin
            self.push_s(r.d.b);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $2) <> 0 then
          begin
            self.push_s(r.d.a);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $1) <> 0 then
          begin
            self.push_s(self.dame_pila);
            self.estados_demas := self.estados_demas + 1;
          end;
        end;
      $0D:
        begin // pshu 5T
          if (numero and $80) <> 0 then
          begin
            self.push_uw(r.pc);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $40) <> 0 then
          begin
            self.push_uw(r.s);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $20) <> 0 then
          begin
            self.push_uw(r.y);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $10) <> 0 then
          begin
            self.push_uw(r.x);
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $8) <> 0 then
          begin
            self.push_u(r.dp);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $4) <> 0 then
          begin
            self.push_u(r.d.b);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $2) <> 0 then
          begin
            self.push_u(r.d.a);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $1) <> 0 then
          begin
            self.push_u(self.dame_pila);
            self.estados_demas := self.estados_demas + 1;
          end;
        end;
      $0E:
        begin // puls 4T
          if (numero and $1) <> 0 then
          begin
            self.pon_pila(self.pop_s);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $2) <> 0 then
          begin
            r.d.a := self.pop_s;
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $4) <> 0 then
          begin
            r.d.b := self.pop_s;
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $8) <> 0 then
          begin
            r.dp := self.pop_s;
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $10) <> 0 then
          begin
            r.x := self.pop_sw;
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $20) <> 0 then
          begin
            r.y := self.pop_sw;
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $40) <> 0 then
          begin
            r.u := self.pop_sw;
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $80) <> 0 then
          begin
            r.pc := self.pop_sw;
            self.estados_demas := self.estados_demas + 2;
          end;
        end;
      $0F:
        begin // pulu 4T
          if (numero and $1) <> 0 then
          begin
            self.pon_pila(self.pop_u);
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $2) <> 0 then
          begin
            r.d.a := self.pop_u;
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $4) <> 0 then
          begin
            r.d.b := self.pop_u;
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $8) <> 0 then
          begin
            r.dp := self.pop_u;
            self.estados_demas := self.estados_demas + 1;
          end;
          if (numero and $10) <> 0 then
          begin
            r.x := self.pop_uw;
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $20) <> 0 then
          begin
            r.y := self.pop_uw;
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $40) <> 0 then
          begin
            r.s := self.pop_uw;
            self.estados_demas := self.estados_demas + 2;
          end;
          if (numero and $80) <> 0 then
          begin
            r.pc := self.pop_uw;
            self.estados_demas := self.estados_demas + 2;
          end;
        end;
      $10, $12:
        r.d.a := m680x_ld_st8(numero, @r.cc); // lda 1T
      $11, $13:
        r.d.b := m680x_ld_st8(numero, @r.cc); // ldb 1T
      $14, $16:
        r.d.a := m680x_add8(r.d.a, numero, @r.cc); // adda 1T
      $15, $17:
        r.d.b := m680x_add8(r.d.b, numero, @r.cc); // addb 1T
      $18, $1A:
        r.d.a := m680x_adc(r.d.a, numero, @r.cc); // adca 1T
      $19, $1B:
        r.d.b := m680x_adc(r.d.b, numero, @r.cc); // adcb 1T
      $1C, $1E:
        r.d.a := m680x_sub8(r.d.a, numero, @r.cc); // suba 1T
      $1D, $1F:
        r.d.b := m680x_sub8(r.d.b, numero, @r.cc); // subb 1T
      $20, $22:
        r.d.a := m680x_sbc(r.d.a, numero, @r.cc); // sbca 1T
      $21, $23:
        r.d.b := m680x_sbc(r.d.b, numero, @r.cc); // sbcb 1T
      $24, $26:
        r.d.a := m680x_and(r.d.a, numero, @r.cc); // anda 1T
      $25, $27:
        r.d.b := m680x_and(r.d.b, numero, @r.cc); // andb 1T
      $28, $2A:
        m680x_and(r.d.a, numero, @r.cc); // bita 1T
      $29, $2B:
        m680x_and(r.d.b, numero, @r.cc); // bitb 1T
      $2C, $2E:
        r.d.a := m680x_eor(r.d.a, numero, @r.cc); // eora 1T
      $2D, $2F:
        r.d.b := m680x_eor(r.d.b, numero, @r.cc); // eorb 1T
      $30, $32:
        r.d.a := m680x_or(r.d.a, numero, @r.cc); // ora 1T
      $31, $33:
        r.d.b := m680x_or(r.d.b, numero, @r.cc); // orb 1T
      $34, $36:
        m680x_sub8(r.d.a, numero, @r.cc); // cmpa 1T
      $35, $37:
        m680x_sub8(r.d.b, numero, @r.cc); // cmpb 1T
      $38, $39:
        if @self.set_lines_call <> nil then
          self.set_lines_call(numero);
      $3A:
        self.putbyte(posicion, m680x_ld_st8(r.d.a, @r.cc)); // sta 1T
      $3B:
        self.putbyte(posicion, m680x_ld_st8(r.d.b, @r.cc)); // stb 1T
      $3C:
        begin // andcc  3T
          tempb := self.dame_pila and numero;
          self.pon_pila(tempb);
        end;
      $3D:
        begin // orcc 3T
          tempb := self.dame_pila or numero;
          self.pon_pila(tempb);
        end;
      $3E:
        self.trf_ex(numero); // exg 8T
      $3F:
        self.trf(numero); // trf 4T
      $40, $41:
        r.d.w := m680x_ld_st16(posicion, @r.cc); // ldd 2T
      $42, $43:
        r.x := m680x_ld_st16(posicion, @r.cc); // ldx 2T
      $44, $45:
        r.y := m680x_ld_st16(posicion, @r.cc); // ldy 2T
      $46, $47:
        r.u := m680x_ld_st16(posicion, @r.cc); // ldu 2T
      $48, $49:
        r.s := m680x_ld_st16(posicion, @r.cc); // lds 2T
      $4A, $4B:
        m680x_sub16(r.d.w, posicion, @r.cc); // cmpd
      $4C, $4D:
        m680x_sub16(r.x, posicion, @r.cc); // cmpx
      $4E, $4F:
        m680x_sub16(r.y, posicion, @r.cc); // cmpy
      $50, $51:
        m680x_sub16(r.u, posicion, @r.cc); // cmpu
      $52, $53:
        m680x_sub16(r.s, posicion, @r.cc); // cmps
      $54, $55:
        r.d.w := m680x_add16(r.d.w, posicion, @r.cc); // addd 2T
      $56, $57:
        r.d.w := m680x_sub16(r.d.w, posicion, @r.cc); // subd 2T
      $58:
        self.putword(posicion, m680x_ld_st16(r.d.w, @r.cc)); // std
      $59:
        self.putword(posicion, m680x_ld_st16(r.x, @r.cc)); // stx
      $5A:
        self.putword(posicion, m680x_ld_st16(r.y, @r.cc)); // sty
      $5B:
        self.putword(posicion, m680x_ld_st16(r.u, @r.cc)); // stu
      $5C:
        self.putword(posicion, m680x_ld_st16(r.s, @r.cc)); // sts
      $60:
        r.pc := r.pc + shortint(numero); // bra 3T
      $61:
        if not(r.cc.c or r.cc.z) then
          r.pc := r.pc + shortint(numero); // bhi 3T
      $62:
        if not(r.cc.c) then
          r.pc := r.pc + shortint(numero); // bcc 3T
      $63:
        if not(r.cc.z) then
          r.pc := r.pc + shortint(numero); // bne 3T
      $65:
        if not(r.cc.n) then
          r.pc := r.pc + shortint(numero); // bpl 3T
      $66:
        if (r.cc.n = r.cc.v) then
          r.pc := r.pc + shortint(numero); // bge 3T
      $67:
        if ((r.cc.n = r.cc.v) and not(r.cc.z)) then
          r.pc := r.pc + shortint(numero); // bgt 3T
      $68:
        r.pc := r.pc + smallint(posicion); // lbra 3T
      $69:
        if not(r.cc.c or r.cc.z) then
          r.pc := r.pc + smallint(posicion); // lbhi 3T
      $6A:
        if not(r.cc.c) then
          r.pc := r.pc + smallint(posicion); // lbcc 3T
      $6B:
        if not(r.cc.z) then
          r.pc := r.pc + smallint(posicion); // lbne 3T
      $6D:
        if not(r.cc.n) then
          r.pc := r.pc + smallint(posicion); // lbpl 3T
      $6E:
        if (r.cc.n = r.cc.v) then
          r.pc := r.pc + smallint(posicion); // lbge 3T
      $6F:
        if ((r.cc.n = r.cc.v) and not(r.cc.z)) then
          r.pc := r.pc + smallint(posicion); // lbgt 3T
      $70:
        ; // brn 3T
      $71:
        if (r.cc.c or r.cc.z) then
          r.pc := r.pc + shortint(numero); // bls 3T
      $72:
        if r.cc.c then
          r.pc := r.pc + shortint(numero); // bcs 3T
      $73:
        if r.cc.z then
          r.pc := r.pc + shortint(numero); // beq 3T
      $74:
        if r.cc.v then
          r.pc := r.pc + shortint(numero); // bvs 3T
      $75:
        if r.cc.n then
          r.pc := r.pc + shortint(numero); // bmi 3T
      $76:
        if not(r.cc.n = r.cc.v) then
          r.pc := r.pc + shortint(numero); // blt 3T
      $77:
        if not((r.cc.n = r.cc.v) and not(r.cc.z)) then
          r.pc := r.pc + shortint(numero); // ble 3T
      $78:
        ; // lbrn 3T
      $79:
        if (r.cc.c or r.cc.z) then
          r.pc := r.pc + smallint(posicion); // lbls 3T
      $7A:
        if r.cc.c then
          r.pc := r.pc + smallint(posicion); // lbcs 3T
      $7B:
        if r.cc.z then
          r.pc := r.pc + smallint(posicion); // lbeq 3T
      $7D:
        if r.cc.n then
          r.pc := r.pc + smallint(posicion); // lbmi 3T
      $7E:
        if not(r.cc.n = r.cc.v) then
          r.pc := r.pc + smallint(posicion); // lblt 3T
      $7F:
        if not((r.cc.n = r.cc.v) and not(r.cc.z)) then
          r.pc := r.pc + smallint(posicion); // lble 3T
      $80:
        begin // clra 2T
          r.d.a := 0;
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $81:
        begin // clrb 2T
          r.d.b := 0;
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $82:
        begin // clr 4T
          self.putbyte(posicion, 0);
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
          r.cc.z := true;
        end;
      $83:
        r.d.a := m680x_com(r.d.a, @r.cc); // coma 2T
      $84:
        r.d.b := m680x_com(r.d.b, @r.cc); // comb 2T
      $85:
        self.putbyte(posicion, m680x_com(self.getbyte(posicion), @r.cc)); // com 4T
      $86:
        r.d.a := m680x_neg(r.d.a, @r.cc); // nega 2T
      $87:
        r.d.b := m680x_neg(r.d.b, @r.cc); // negb 2T
      $88:
        self.putbyte(posicion, m680x_neg(self.getbyte(posicion), @r.cc)); // neg 4T
      $89:
        r.d.a := m680x_inc(r.d.a, @r.cc); // inca 2T
      $8A:
        r.d.b := m680x_inc(r.d.b, @r.cc); // incb 2T
      $8B:
        self.putbyte(posicion, m680x_inc(self.getbyte(posicion), @r.cc)); // inc 4T
      $8C:
        r.d.a := m680x_dec(r.d.a, @r.cc); // deca 2T
      $8D:
        r.d.b := m680x_dec(r.d.b, @r.cc); // decb 2T
      $8E:
        self.putbyte(posicion, m680x_dec(self.getbyte(posicion), @r.cc)); // dec 4T
      $8F:
        r.pc := self.pop_sw; // rts 4T
      $90:
        m680x_tst(r.d.a, @r.cc); // tsta 2T
      $91:
        m680x_tst(r.d.b, @r.cc); // tstb 2T
      $92:
        m680x_tst(self.getbyte(posicion), @r.cc); // tst 3T
      $93:
        r.d.a := m680x_lsr(r.d.a, @r.cc); // lsra 2T
      $94:
        r.d.b := m680x_lsr(r.d.b, @r.cc); // lsrb 2T
      $95:
        self.putbyte(posicion, m680x_lsr(self.getbyte(posicion), @r.cc)); // lsr 4T
      $96:
        r.d.a := m680x_ror(r.d.a, @r.cc); // rora 2T
      $97:
        r.d.b := m680x_ror(r.d.b, @r.cc); // rorb 2T
      $98:
        self.putbyte(posicion, m680x_ror(self.getbyte(posicion), @r.cc)); // ror 4T
      $99:
        r.d.a := m680x_asr(r.d.a, @r.cc); // asra 2T
      $9A:
        r.d.b := m680x_asr(r.d.b, @r.cc); // asrb 2T
      $9B:
        self.putbyte(posicion, m680x_asr(self.getbyte(posicion), @r.cc)); // asr 4T
      $9C:
        r.d.a := m680x_asl(r.d.a, @r.cc); // asla 2T
      $9D:
        r.d.b := m680x_asl(r.d.b, @r.cc); // aslb 2T
      $9E:
        self.putbyte(posicion, m680x_asl(self.getbyte(posicion), @r.cc)); // asl 4T
      $9F:
        begin // rti 4T
          self.pon_pila(self.pop_s);
          if r.cc.e then
          begin // 13 T
            self.estados_demas := self.estados_demas + 9;
            r.d.a := self.pop_s;
            r.d.b := self.pop_s;
            r.dp := self.pop_s;
            r.x := self.pop_sw;
            r.y := self.pop_sw;
            r.u := self.pop_sw;
          end;
          r.pc := self.pop_sw;
        end;
      $A0:
        r.d.a := m680x_rol(r.d.a, @r.cc); // rola 2T
      $A1:
        r.d.b := m680x_rol(r.d.b, @r.cc); // rolb 2T
      $A2:
        self.putbyte(posicion, m680x_rol(self.getbyte(posicion), @r.cc)); // rol 4T
      $A3:
        self.putword(posicion, m680x_lsr16(self.getword(posicion), @r.cc)); // lsr16
      $A5:
        self.putword(posicion, m680x_asr16(self.getword(posicion), @r.cc)); // asr16
      $A6:
        self.putword(posicion, m680x_asl16(self.getword(posicion), @r.cc)); // asl16
      $A8:
        r.pc := posicion; // jmp 1T
      $A9:
        begin // jsr 5T
          self.push_sw(r.pc);
          r.pc := posicion;
        end;
      $AA:
        begin // bsr 7T
          self.push_sw(r.pc);
          r.pc := r.pc + shortint(numero);
        end;
      $AB:
        begin // lbsr 9T
          self.push_sw(r.pc);
          r.pc := r.pc + smallint(posicion);
        end;
      $AC:
        begin // decbjnz
          tempw := r.d.b - 1;
          r.cc.z := ((tempw and $FF) = 0);
          r.cc.n := (tempw and $80) <> 0;
          r.cc.v := ((r.d.b xor 1 xor tempw xor (tempw shr 1)) and $80) <> 0;
          r.d.b := tempw;
          if not(r.cc.z) then
            r.pc := r.pc + shortint(numero);
        end;
      $AD:
        begin // decxjnz
          templ := r.x - 1;
          r.cc.z := ((templ and $FFFF) = 0);
          r.cc.n := (templ and $8000) <> 0;
          r.cc.v := ((r.x xor 1 xor templ xor (templ shr 1)) and $8000) <> 0;
          r.x := templ;
          if not(r.cc.z) then
            r.pc := r.pc + shortint(numero);
        end;
      $B0:
        r.x := r.x + r.d.b; // abx 3T
      $B1:
        begin // daa 2T
          cf := 0;
          tempb := r.d.a and $F0;
          tempb2 := r.d.a and $0F;
          if ((tempb2 > $09) or r.cc.h) then
            cf := cf or $06;
          if ((tempb > $80) and (tempb2 > $09)) then
            cf := cf or $60;
          if ((tempb > $90) or r.cc.c) then
            cf := cf or $60;
          tempw := cf + r.d.a;
          r.cc.v := false;
          r.cc.n := (tempw and $80) <> 0;
          r.cc.z := ((tempw and $FF) = 0);
          r.cc.c := r.cc.c or ((tempw and $100) <> 0);
          r.d.a := tempw;
        end;
      $B2:
        begin // sex 2T
          r.d.a := $FF * (r.d.b shr 7);
          r.cc.n := (r.d.w and $8000) <> 0;
          r.cc.z := (r.d.w = 0);
        end;
      $B3:
        begin // mul 11T
          r.d.w := r.d.a * r.d.b;
          r.cc.c := (r.d.w and $80) <> 0;
          r.cc.z := (r.d.w = 0);
        end;
      $B4:
        begin // lmul 21
          templ := r.x * r.y;
          r.x := templ shr 16;
          r.y := templ and $FFFF;
          r.cc.z := (templ = 0);
          r.cc.c := (templ and $8000) <> 0;
        end;
      $B5:
        begin // divx 10
          if r.d.b <> 0 then
          begin
            tempw := r.x div r.d.b;
            tempb := r.x mod r.d.b;
          end
          else
          begin
            tempw := 0;
            tempb := 0;
          end;
          r.x := tempw;
          r.d.b := tempb;
          r.cc.c := (tempw and $80) <> 0;
          r.cc.z := (tempw = 0);
        end;
      $B6:
        while (r.u <> 0) do
        begin // bmove
          tempb := self.getbyte(r.y);
          r.y := r.y + 1;
          self.putbyte(r.x, tempb);
          r.x := r.x + 1;
          r.u := r.u - 1;
          self.estados_demas := self.estados_demas + 2;
        end;
      $B8:
        r.d.w := m680x_lsrd(r.d.w, numero, @r.cc); // lsrd
      $BC:
        r.d.w := m680x_asrd(r.d.w, numero, @r.cc); // asrd
      $BE:
        r.d.w := m680x_asld(r.d.w, numero, @r.cc); // asld
      $C2:
        begin // clrd
          r.d.w := 0;
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $C3:
        begin // clr16
          self.putword(posicion, 0);
          r.cc.z := true;
          r.cc.n := false;
          r.cc.v := false;
          r.cc.c := false;
        end;
      $C4:
        r.d.w := m680x_neg16(r.d.w, @r.cc); // negd
      $C5:
        self.putword(posicion, m680x_neg16(self.getword(posicion), @r.cc)); // neg16
      $C6:
        r.d.w := m680x_inc16(r.d.w, @r.cc); // incd
      $C7:
        self.putword(posicion, m680x_inc16(self.getword(posicion), @r.cc)); // inc16
      $C9:
        self.putword(posicion, m680x_dec16(self.getword(posicion), @r.cc)); // dec16
      $CA:
        m680x_tst16(r.d.w, @r.cc); // tstd
      $CB:
        m680x_tst16(self.getword(posicion), @r.cc);
      $CC:
        r.d.a := m680x_abs8(r.d.a, @r.cc); // absa
      $CD:
        r.d.b := m680x_abs8(r.d.b, @r.cc); // absb
      $CE:
        r.d.w := m680x_abs16(r.d.w, @r.cc); // absd
      $CF:
        while (r.u <> 0) do
        begin // bset
          self.putbyte(r.x, r.d.a);
          r.x := r.x + 1;
          r.u := r.u - 1;
          self.estados_demas := self.estados_demas + 2;
        end;
      $D0:
        while (r.u <> 0) do
        begin // bset2
          self.putword(r.x, r.d.w);
          r.x := r.x + 2;
          r.u := r.u - 1;
          self.estados_demas := self.estados_demas + 3;
        end;
    end;
    tempw := estados_t[instruccion] + self.estados_demas;
    self.contador := self.contador + tempw;
    timers.update(tempw, self.numero_cpu);
  end; // Del while
end;

end.
