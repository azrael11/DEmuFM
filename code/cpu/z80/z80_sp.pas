unit z80_sp;

interface

uses
  main_engine,
  nz80,
  z80daisy,
  vars_hide,
  timer_engine;

type
  tretraso = procedure(direccion: word);
  tretraso_puerto = procedure(direccion: word);

  cpu_z80_sp = class(cpu_z80)
  public
    procedure run(maximo: integer);
    procedure change_retraso_call(retraso: tretraso; retraso_puerto: tretraso_puerto);
  private
    retraso: tretraso;
    retraso_puerto: tretraso_puerto;
    procedure call_irq;
    function spec_getbyte(posicion: word): byte;
    procedure spec_putbyte(posicion: word; valor: byte);
    function spec_inbyte(posicion: word): byte;
    procedure spec_outbyte(posicion: word; valor: byte);
    // Resto opcodes
    procedure exec_cb_sp;
    procedure exec_dd_fd_sp(tipo: boolean);
    procedure exec_dd_cb_sp(tipo: boolean);
    procedure exec_ed_sp;
  end;

var
  spec_z80: cpu_z80_sp;

implementation

uses spectrum_misc;

procedure cpu_z80_sp.change_retraso_call(retraso: tretraso; retraso_puerto: tretraso_puerto);
begin
  self.retraso := retraso;
  self.retraso_puerto := retraso_puerto;
end;

procedure cpu_z80_sp.spec_outbyte(posicion: word; valor: byte);
begin
  self.out_port(posicion, valor);
  self.retraso_puerto(posicion);
end;

function cpu_z80_sp.spec_inbyte(posicion: word): byte;
begin
  self.retraso_puerto(posicion);
  spec_inbyte := self.in_port(posicion);
end;

procedure cpu_z80_sp.spec_putbyte(posicion: word; valor: byte);
begin
  self.putbyte(posicion, valor);
  self.retraso(posicion);
  self.contador := self.contador + 3;
end;

function cpu_z80_sp.spec_getbyte(posicion: word): byte;
begin
  spec_getbyte := self.getbyte(posicion);
  self.retraso(posicion);
  self.contador := self.contador + 3;
end;

procedure cpu_z80_sp.call_irq;
var
  posicion: word;
begin
  self.r.halt_opcode := false;
  if not(r.iff1) then
    exit; // se esta ejecutando otra
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  dec(r.sp, 2);
  self.spec_putbyte(r.sp + 1, r.pc shr 8);
  self.spec_putbyte(r.sp, r.pc and $FF);
  r.IFF2 := false;
  r.iff1 := false;
  Case r.im of
    0:
      begin // 12t
        r.pc := $38;
        self.contador := self.contador + 6; // 12 En total -6 de guardar SP
      end;
    1:
      begin // 13t
        r.pc := $38;
        self.contador := self.contador + 7; // 13 en total -6 de guardar SP
      end;
    2:
      begin // 19t
        if self.daisy then
          posicion := z80daisy_ack
        else
          posicion := self.im2_lo;
        posicion := posicion or (r.i shl 8);
        r.pc := self.spec_getbyte(posicion) + (self.spec_getbyte(posicion + 1) shl 8);
        self.contador := self.contador + 7; // 19 en total -12 de guardar SP y coger PC
      end;
  end;
  r.wz := r.pc;
  self.pedir_irq := CLEAR_LINE;
end;

procedure cpu_z80_sp.run(maximo: integer);
var
  instruccion, temp: byte;
  posicion: parejas;
  ban_temp: band_z80;
  pcontador: integer;
  irq_temp: boolean;
  cantidad_t: word;
begin
  irq_temp := false;
  while self.contador < maximo do
  begin
    self.r.ppc := self.r.pc;
    if not(self.after_ei) then
    begin
      if self.daisy then
        irq_temp := z80daisy_state;
      if (irq_temp or (self.pedir_irq <> CLEAR_LINE)) then
        self.call_irq;
    end
    else
      self.after_ei := false;
    if self.r.halt_opcode then
      r.pc := r.pc - 1;
    pcontador := self.contador;
    self.retraso(r.pc);
    inc(self.contador, 4);
    instruccion := self.getbyte(r.pc);
    r.pc := r.pc + 1;
    r.r := ((r.r + 1) and $7F) or (r.r and $80);
    case instruccion of
      $00, $40, $49, $52, $5B, $64, $6D, $7F: { nop >4t< }
        ;
      $01:
        begin { ld BC,nn >10t< }
          r.bc.l := spec_getbyte(r.pc);
          r.bc.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
        end;
      $02:
        begin { ld (BC),A >7t< }
          spec_putbyte(r.bc.w, r.a);
          r.wz := ((r.bc.w + 1) and $FF) or (r.a shl 8);
        end;
      $03:
        begin { inc BC >6t< }
          self.contador := self.contador + 2;
          r.bc.w := r.bc.w + 1;
        end;
      $04:
        r.bc.h := inc_8(r.bc.h); // inc B >4t<
      $05:
        r.bc.h := dec_8(r.bc.h); // dec B >4t<
      $06:
        begin { ld B,n >7t< }
          r.bc.h := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $07:
        begin // rlca >4t<}
          r.f.c := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or byte(r.f.c);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := false;
          r.f.n := false;
        end;
      $08:
        begin { ex AF,AF' >4t< }
          ban_temp := r.f;
          r.f := r.f2;
          r.f2 := ban_temp;
          temp := r.a;
          r.a := r.a2;
          r.a2 := temp;
        end;
      $09:
        begin { add HL,BC >11t< }
          self.contador := self.contador + 7;
          r.hl.w := add_16(r.hl.w, r.bc.w);
        end;
      $0A:
        begin { ld A,(BC) >7t< }
          r.a := spec_getbyte(r.bc.w);
          r.wz := r.bc.w + 1;
        end;
      $0B:
        begin { dec BC >6t< }
          self.contador := self.contador + 2;
          dec(r.bc.w);
        end;
      $0C:
        r.bc.l := inc_8(r.bc.l); { inc C >4t< }
      $0D:
        r.bc.l := dec_8(r.bc.l); { dec C >4t< }
      $0E:
        begin { ld C,n >7t< }
          r.bc.l := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $0F:
        begin { rrca >4t< }
          r.f.c := (r.a and 1) <> 0;
          r.a := (r.a shr 1) or (byte(r.f.c) shl 7);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := false;
          r.f.n := false;
        end;
      $10:
        begin { dnjz (PC+e) >8t o 13t< }
          self.contador := self.contador + 1;
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          r.bc.h := r.bc.h - 1;
          if r.bc.h <> 0 then
          begin
            // pc-1 esta asi por la memoria contenida!!
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            r.pc := r.pc + shortint(temp);
            r.wz := r.pc;
          end;
        end;
      $11:
        begin { ld DE,nn >10t< }
          r.de.l := spec_getbyte(r.pc);
          r.de.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
        end;
      $12:
        begin { ld (DE),A >7t< }
          spec_putbyte(r.de.w, r.a);
          r.wz := ((r.de.w + 1) and $FF) or (r.a shl 8);
        end;
      $13:
        begin { inc DE >6t< }
          self.contador := self.contador + 2;
          inc(r.de.w);
        end;
      $14:
        r.de.h := inc_8(r.de.h); // inc D >4t<
      $15:
        r.de.h := dec_8(r.de.h); // dec D >4t<
      $16:
        begin { ld D,n >7t< }
          r.de.h := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $17:
        begin // rla >4t<
          r.f.h := (r.a and $80) <> 0;
          r.a := (r.a shl 1) or byte(r.f.c);
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.c := r.f.h;
          r.f.h := false;
          r.f.n := false;
        end;
      $18:
        begin { jr e >12t< }
          temp := spec_getbyte(r.pc);
          self.retraso(r.pc);
          self.contador := self.contador + 1;
          self.retraso(r.pc);
          self.contador := self.contador + 1;
          self.retraso(r.pc);
          self.contador := self.contador + 1;
          self.retraso(r.pc);
          self.contador := self.contador + 1;
          self.retraso(r.pc);
          self.contador := self.contador + 1;
          r.pc := r.pc + 1 + shortint(temp);
          r.wz := r.pc;
        end;
      $19:
        begin // add HL,DE >11t<
          self.contador := self.contador + 7;
          r.hl.w := add_16(r.hl.w, r.de.w);
        end;
      $1A:
        begin { ld A,(DE) >7t< }
          r.a := spec_getbyte(r.de.w);
          r.wz := r.de.w + 1;
        end;
      $1B:
        begin { dec DE >6t< }
          self.contador := self.contador + 2;
          dec(r.de.w);
        end;
      $1C:
        r.de.l := inc_8(r.de.l); // inc E >4t<
      $1D:
        r.de.l := dec_8(r.de.l); // dec E >4t<
      $1E:
        begin { ld E,n >7t< }
          r.de.l := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $1F:
        begin { rra >4t< }
          r.f.h := (r.a and 1) <> 0;
          r.a := (r.a shr 1) or (byte(r.f.c) shl 7);
          r.f.n := false;
          r.f.c := r.f.h;
          r.f.h := false;
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
        end;
      $20:
        begin // jr NZ,(PC+e) >7t o 12t<
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          if not(r.f.z) then
          begin
            // Por la memoria contenida
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            r.pc := r.pc + shortint(temp);
            r.wz := r.pc;
          end;
        end;
      $21:
        begin { ld HL,nn >10t< }
          r.hl.l := spec_getbyte(r.pc);
          r.hl.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
        end;
      $22:
        begin { ld (nn),HL >16t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          spec_putbyte(posicion.w, r.hl.l);
          spec_putbyte(posicion.w + 1, r.hl.h);
          r.wz := posicion.w + 1;
        end;
      $23:
        begin { inc HL >6t< }
          self.contador := self.contador + 2;
          inc(r.hl.w);
        end;
      $24:
        r.hl.h := inc_8(r.hl.h); // inc H >4t<
      $25:
        r.hl.h := dec_8(r.hl.h); // dec H >4t<
      $26:
        begin { ld H,n >7t< }
          r.hl.h := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $27:
        begin { daa >4t< }
          temp := 0;
          If (r.f.h Or ((r.a And $0F) > 9)) Then
            temp := temp or 6;
          If (r.f.c Or (r.a > $9F)) Then
            temp := temp or $60;
          If ((r.a > $8F) And ((r.a And $0F) > 9)) Then
            temp := temp or $60;
          If (r.a > $99) Then
            r.f.c := True;
          If r.f.n Then
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
        begin // jr Z,(PC+e) >7t o 12t<
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          if r.f.z then
          begin
            // Por la memoria contenida!
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            r.pc := r.pc + shortint(temp);
            r.wz := r.pc;
          end;
        end;
      $29:
        begin // add HL,HL >11t<
          self.contador := self.contador + 7;
          r.hl.w := add_16(r.hl.w, r.hl.w);
        end;
      $2A:
        begin { ld HL,(nn) >16t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          r.hl.l := spec_getbyte(posicion.w);
          r.hl.h := spec_getbyte(posicion.w + 1);
          r.wz := posicion.w + 1;
        end;
      $2B:
        begin { dec HL >6t< }
          self.contador := self.contador + 2;
          dec(r.hl.w);
        end;
      $2C:
        r.hl.l := inc_8(r.hl.l); // inc L >4t<
      $2D:
        r.hl.l := dec_8(r.hl.l); // dec L >4t<
      $2E:
        begin { ld L,n >7t< }
          r.hl.l := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $2F:
        begin { cpl >4t< }
          r.a := r.a xor $FF;
          r.f.bit5 := (r.a and $20) <> 0;
          r.f.bit3 := (r.a and 8) <> 0;
          r.f.h := True;
          r.f.n := True;
        end;
      $30:
        begin // jr NC,(PC+e) >7t o 12t<
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          if not(r.f.c) then
          begin
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            r.pc := r.pc + shortint(temp);
            r.wz := r.pc;
          end;
        end;
      $31:
        begin { ld SP,nn >10t< }
          r.sp := spec_getbyte(r.pc) + (spec_getbyte(r.pc + 1) shl 8);
          r.pc := r.pc + 2;
        end;
      $32:
        begin { ld (nn),A >13t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          spec_putbyte(posicion.w, r.a);
          r.wz := ((posicion.w + 1) and $FF) or (r.a shl 8);
        end;
      $33:
        begin { inc SP >6t< }
          self.contador := self.contador + 2;
          inc(r.sp);
        end;
      $34:
        begin // inc (HL) >11t<
          temp := spec_getbyte(r.hl.w);
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          spec_putbyte(r.hl.w, inc_8(temp));
        end;
      $35:
        begin { dec (HL) >11t< }
          temp := spec_getbyte(r.hl.w);
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          spec_putbyte(r.hl.w, dec_8(temp));
        end;
      $36:
        begin { ld (HL),n >10t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          spec_putbyte(r.hl.w, temp);
        end;
      $37:
        begin { scf >4t< }
          r.f.bit5 := ((r.a and $20) <> 0) or r.f.bit5;
          r.f.bit3 := ((r.a and 8) <> 0) or r.f.bit3;
          r.f.c := True;
          r.f.h := false;
          r.f.n := false;
        end;
      $38:
        begin // jr C,(PC+e) >7t o 12t<
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          if r.f.c then
          begin
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            self.retraso(r.pc - 1);
            self.contador := self.contador + 1;
            r.pc := r.pc + shortint(temp);
            r.wz := r.pc;
          end;
        end;
      $39:
        begin // add HL,SP >11t<
          self.contador := self.contador + 7;
          r.hl.w := add_16(r.hl.w, r.sp);
        end;
      $3A:
        begin { ld A,(nn) >13< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          r.a := spec_getbyte(posicion.w);
          r.wz := posicion.w + 1;
        end;
      $3B:
        begin { dec SP >6t< }
          self.contador := self.contador + 2;
          r.sp := r.sp - 1;
        end;
      $3C:
        r.a := inc_8(r.a); // inc A >4t<
      $3D:
        r.a := dec_8(r.a); // dec A >4t<
      $3E:
        begin { ld A,n >7t< }
          r.a := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
        end;
      $3F:
        begin { ccf >4t< }
          r.f.bit5 := ((r.a and $20) <> 0) or r.f.bit5;
          r.f.bit3 := ((r.a and 8) <> 0) or r.f.bit3;
          r.f.h := r.f.c;
          r.f.n := false;
          r.f.c := not(r.f.c);
        end;
      { '$'40: igual que el nop ld B,B }
      $41:
        r.bc.h := r.bc.l; { ld B,C >4t< }
      $42:
        r.bc.h := r.de.h; { ld B,D >4t< }
      $43:
        r.bc.h := r.de.l; { ld B,E >4t< }
      $44:
        r.bc.h := r.hl.h; { ld B,H >4t< }
      $45:
        r.bc.h := r.hl.l; { ld B,L >4t< }
      $46:
        r.bc.h := spec_getbyte(r.hl.w); { ld B,(HL) >7t< }
      $47:
        r.bc.h := r.a; { ld B,A >4t< }
      $48:
        r.bc.l := r.bc.h; { ld C,B >4t< }
      { '$'49: igual que el nop ld C,C }
      $4A:
        r.bc.l := r.de.h; { ld C,D >4t< }
      $4B:
        r.bc.l := r.de.l; { ld C,E >4t< }
      $4C:
        r.bc.l := r.hl.h; { ld C,H >4t< }
      $4D:
        r.bc.l := r.hl.l; { ld C,L >4t< }
      $4E:
        r.bc.l := spec_getbyte(r.hl.w); { ld C,(HL) >7t< }
      $4F:
        r.bc.l := r.a; { ld C,A >4t< }
      $50:
        r.de.h := r.bc.h; { ld D,B >4t< }
      $51:
        r.de.h := r.bc.l; { ld D,C >4t< }
      { '$'52 igual que el nop ld D,D }
      $53:
        r.de.h := r.de.l; { ld D,E >4t< }
      $54:
        r.de.h := r.hl.h; { ld D,H >4t< }
      $55:
        r.de.h := r.hl.l; { ld D,L >4t< }
      $56:
        r.de.h := spec_getbyte(r.hl.w); { ld D,(HL) >7t< }
      $57:
        r.de.h := r.a; { ld D,A >4t< }
      $58:
        r.de.l := r.bc.h; { ld E,B >4t< }
      $59:
        r.de.l := r.bc.l; { ld E,C >4t< }
      $5A:
        r.de.l := r.de.h; { ld E,D >4t< }
      { '$'5b igual que el nop ld E,E }
      $5C:
        r.de.l := r.hl.h; { ld E,H >4t< }
      $5D:
        r.de.l := r.hl.l; { ld E,L >4t< }
      $5E:
        r.de.l := spec_getbyte(r.hl.w); { ld E,(HL) >7t< }
      $5F:
        r.de.l := r.a; { ld E,A >4t< }
      $60:
        r.hl.h := r.bc.h; { ld H,B >4t< }
      $61:
        r.hl.h := r.bc.l; { ld H,C >4t< }
      $62:
        r.hl.h := r.de.h; { ld H,D >4t< }
      $63:
        r.hl.h := r.de.l; { ld H,E >4t< }
      { '$'64: igual que el nop ld H,H }
      $65:
        r.hl.h := r.hl.l; { ld H,L >4t< }
      $66:
        r.hl.h := spec_getbyte(r.hl.w); { ld H,(HL) >7t< }
      $67:
        r.hl.h := r.a; { ld H,A >4t< }
      $68:
        r.hl.l := r.bc.h; { ld L,B >4t< }
      $69:
        r.hl.l := r.bc.l; { ld L,C >4t< }
      $6A:
        r.hl.l := r.de.h; { ld L,D >4t< }
      $6B:
        r.hl.l := r.de.l; { ld L,E >4t< }
      $6C:
        r.hl.l := r.hl.h; { ld L,H >4t< }
      { '$'6d: igual que el nop ld L,L }
      $6E:
        r.hl.l := spec_getbyte(r.hl.w); { ld L,(HL) >7t< }
      $6F:
        r.hl.l := r.a; { ld L,A >4t< }
      $70:
        spec_putbyte(r.hl.w, r.bc.h); { ld (HL),B >7t< }
      $71:
        spec_putbyte(r.hl.w, r.bc.l); { ld (HL),C >7t< }
      $72:
        spec_putbyte(r.hl.w, r.de.h); { ld (HL),D >7t< }
      $73:
        spec_putbyte(r.hl.w, r.de.l); { ld (HL),E >7t< }
      $74:
        spec_putbyte(r.hl.w, r.hl.h); { ld (HL),H >7t< }
      $75:
        spec_putbyte(r.hl.w, r.hl.l); { ld (HL),L >7t< }
      $76:
        self.r.halt_opcode := True; { halt >4t< }
      $77:
        spec_putbyte(r.hl.w, r.a); { ld (HL),A >7t< }
      $78:
        r.a := r.bc.h; { ld A,B >4t< }
      $79:
        r.a := r.bc.l; { ld A,C >4t< }
      $7A:
        r.a := r.de.h; { ld A,D >4t< }
      $7B:
        r.a := r.de.l; { ld A,E >4t< }
      $7C:
        r.a := r.hl.h; { ld A,H >4t< }
      $7D:
        r.a := r.hl.l; { ld A,L >4t< }
      $7E:
        r.a := spec_getbyte(r.hl.w); { ld A,(HL) >7t< }
      { '$'7f: igual que el nop ld A,A }
      $80:
        add_8(r.bc.h); { add A,B >4t< }
      $81:
        add_8(r.bc.l); { add A,C >4t< }
      $82:
        add_8(r.de.h); { add A,D >4t< }
      $83:
        add_8(r.de.l); { add A,E >4t< }
      $84:
        add_8(r.hl.h); { add A,H >4t< }
      $85:
        add_8(r.hl.l); { add A,L >4t< }
      $86:
        add_8(spec_getbyte(r.hl.w)); { add A,(HL) >7t< }
      $87:
        add_8(r.a); { add A,A >4t< }
      $88:
        adc_8(r.bc.h); { adc A,B >4t< }
      $89:
        adc_8(r.bc.l); { adc A,C >4t< }
      $8A:
        adc_8(r.de.h); { adc A,D >4t< }
      $8B:
        adc_8(r.de.l); { adc A,E >4t< }
      $8C:
        adc_8(r.hl.h); { adc A,H >4t< }
      $8D:
        adc_8(r.hl.l); { adc A,L >4t< }
      $8E:
        adc_8(spec_getbyte(r.hl.w)); { adc A,(HL) >7t< }
      $8F:
        adc_8(r.a); { adc A,A >4t< }
      $90:
        sub_8(r.bc.h); { sub B >4t< }
      $91:
        sub_8(r.bc.l); { sub C >4t< }
      $92:
        sub_8(r.de.h); { sub D >4t< }
      $93:
        sub_8(r.de.l); { sub E >4t< }
      $94:
        sub_8(r.hl.h); { sub H >4t< }
      $95:
        sub_8(r.hl.l); { sub L >4t< }
      $96:
        sub_8(spec_getbyte(r.hl.w)); { sub (HL) >4t< }
      $97:
        sub_8(r.a); { sub A  >4t< }
      $98:
        sbc_8(r.bc.h); { sbc A,B >4t< }
      $99:
        sbc_8(r.bc.l); { sbc A,C >4t< }
      $9A:
        sbc_8(r.de.h); { sbc A,D >4t< }
      $9B:
        sbc_8(r.de.l); { sbc A,E >4t< }
      $9C:
        sbc_8(r.hl.h); { sbc A,H >4t< }
      $9D:
        sbc_8(r.hl.l); { sbc A,L >4t< }
      $9E:
        sbc_8(spec_getbyte(r.hl.w)); { sbc A,(HL) >7t< }
      $9F:
        sbc_8(r.a); { sbc A,A >4t< }
      $A0:
        and_a(r.bc.h); { and A,B >4t< }
      $A1:
        and_a(r.bc.l); { and A,C >4t< }
      $A2:
        and_a(r.de.h); { and A,D >4t< }
      $A3:
        and_a(r.de.l); { and A,E >4t< }
      $A4:
        and_a(r.hl.h); { and A,H >4t< }
      $A5:
        and_a(r.hl.l); { and A,L >4t< }
      $A6:
        and_a(spec_getbyte(r.hl.w)); { and A,(HL) >7t< }
      $A7:
        and_a(r.a); { and A,A >4t< }
      $A8:
        xor_a(r.bc.h); { xor A,B >4t< }
      $A9:
        xor_a(r.bc.l); { xor A,C >4t< }
      $AA:
        xor_a(r.de.h); { xor A,D >4t< }
      $AB:
        xor_a(r.de.l); { xor A,E >4t< }
      $AC:
        xor_a(r.hl.h); { xor A,H >4t< }
      $AD:
        xor_a(r.hl.l); { xor A,L >4t< }
      $AE:
        xor_a(spec_getbyte(r.hl.w)); { xor A,(HL) >7t< }
      $AF:
        begin { xor A,A >4t< }
          r.a := 0;
          r.f.s := false;
          r.f.z := True;
          r.f.bit5 := false;
          r.f.h := false;
          r.f.bit3 := false;
          r.f.p_v := True;
          r.f.n := false;
          r.f.c := false;
        end;
      $B0:
        or_a(r.bc.h); { or B >4t< }
      $B1:
        or_a(r.bc.l); { or C >4t< }
      $B2:
        or_a(r.de.h); { or D >4t< }
      $B3:
        or_a(r.de.l); { or E >4t< }
      $B4:
        or_a(r.hl.h); { or H >4t< }
      $B5:
        or_a(r.hl.l); { or L >4t< }
      $B6:
        or_a(spec_getbyte(r.hl.w)); { or (HL) >7t< }
      $B7:
        or_a(r.a); { or A >4t< }
      $B8:
        cp_a(r.bc.h); { cp B >4t< }
      $B9:
        cp_a(r.bc.l); { cp C >4t< }
      $BA:
        cp_a(r.de.h); { cp D >4t< }
      $BB:
        cp_a(r.de.l); { cp E >4t< }
      $BC:
        cp_a(r.hl.h); { cp H >4t< }
      $BD:
        cp_a(r.hl.l); { cp L >4t< }
      $BE:
        cp_a(spec_getbyte(r.hl.w)); { cp (HL) >7t< }
      $BF:
        cp_a(r.a); { cp A >4t< }
      $C0:
        begin { ret NZ >5t o 10t< }
          self.contador := self.contador + 1;
          if not(r.f.z) then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := self.pop_sp;
            r.wz := r.pc;
          end;
        end;
      $C1:
        begin // pop BC  >10t<
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
          r.bc.w := pop_sp;
        end;
      $C2:
        begin // jp NZ,nn >10t<
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if not(r.f.z) then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $C3:
        begin // jp nn >10t<}
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := posicion.w;
          r.wz := posicion.w;
        end;
      $C4:
        begin { call NZ,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if not(r.f.z) then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $C5:
        begin { push BC >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.bc.w);
        end;
      $C6:
        begin { add A,n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          add_8(temp);
        end;
      $C7:
        begin { rst 00H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := 0;
          r.wz := 0;
        end;
      $C8:
        begin { ret Z >5t o 11t< }
          self.contador := self.contador + 1;
          if r.f.z then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $C9:
        begin // ret >10t<
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
          r.pc := pop_sp;
          r.wz := r.pc;
        end;
      $CA:
        begin { jp Z,nn >10t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if r.f.z then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $CB:
        self.exec_cb_sp;
      $CC:
        begin { call Z,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if r.f.z then
          begin
            self.retraso(r.pc);
            inc(self.contador);
            self.retraso(r.sp - 1);
            inc(self.contador, 3);
            self.retraso(r.sp - 2);
            inc(self.contador, 3);
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $CD:
        begin { call nn >17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.wz := posicion.w;
          r.pc := r.pc + 2;
          self.retraso(r.pc);
          inc(self.contador);
          self.retraso(r.sp - 1);
          inc(self.contador, 3);
          self.retraso(r.sp - 2);
          inc(self.contador, 3);
          push_sp(r.pc);
          r.pc := posicion.w;
        end;
      $CE:
        begin { adc A,n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          adc_8(temp);
        end;
      $CF:
        begin { rst 08H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          inc(self.contador, 3);
          self.retraso(r.sp - 2);
          inc(self.contador, 3);
          push_sp(r.pc);
          r.pc := $8;
          r.wz := $8;
        end;
      $D0:
        begin { ret NC >5t o 11t< }
          self.contador := self.contador + 1;
          if not(r.f.c) then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $D1:
        begin // pop DE >10t<
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
          r.de.w := pop_sp;
        end;
      $D2:
        begin // jp NC,nn >10t<
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if not(r.f.c) then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $D3:
        begin // out (n),A >11t<}
          posicion.l := spec_getbyte(r.pc);
          posicion.h := r.a;
          r.pc := r.pc + 1;
          spec_outbyte(posicion.w, r.a);
          r.wz := ((posicion.l + 1) and $FF) or (r.a shl 8);
        end;
      $D4:
        begin { call NC,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if not(r.f.c) then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $D5:
        begin { push DE >11t< }
          inc(self.contador);
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.de.w);
        end;
      $D6:
        begin { sub n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          sub_8(temp);
        end;
      $D7:
        begin { rst 10H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := $10;
          r.wz := $10;
        end;
      $D8:
        begin { ret C >5t o 11t< }
          self.contador := self.contador + 1;
          if r.f.c then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $D9:
        begin { exx >4t< }
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
        begin { jp C,nn >10t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if r.f.c then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $DB:
        begin { in A,(n) >11t< }
          posicion.l := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          posicion.h := r.a;
          r.a := spec_inbyte(posicion.w);
          r.wz := posicion.w + 1;
        end;
      $DC:
        begin { call C,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if r.f.c then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $DD:
        self.exec_dd_fd_sp(True);
      $DE:
        begin { sbc A,n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          sbc_8(temp);
        end;
      $DF:
        begin { rst 18H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := $18;
          r.wz := $18;
        end;
      $E0:
        begin { ret PO >5t o 11t< }
          self.contador := self.contador + 1;
          if not(r.f.p_v) then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $E1:
        begin { pop HL >10t< }
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
          r.hl.w := pop_sp;
        end;
      $E2:
        begin { jp PO,nn >10t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if not(r.f.p_v) then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $E3:
        begin { ex (sp),hl >19t< }
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 4;
          posicion.w := pop_sp;
          push_sp(r.hl.w);
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 1;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 1;
          r.hl := posicion;
          r.wz := posicion.w;
        end;
      $E4:
        begin { call PO,nn >10 o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if not(r.f.p_v) then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $E5:
        begin // push HL >11t<
          self.contador := self.contador + 1;
          push_sp(r.hl.w);
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
        end;
      $E6:
        begin { and A,n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          and_a(temp);
        end;
      $E7:
        begin { rst 20H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := $20;
          r.wz := $20;
        end;
      $E8:
        begin { ret PE >5t o 11t< }
          self.contador := self.contador + 1;
          if r.f.p_v then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $E9:
        r.pc := r.hl.w; { jp (HL) >4t< }
      $EA:
        begin { jp PE,nn >10t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if r.f.p_v then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $EB:
        begin { ex DE,HL >4t< }
          posicion := r.de;
          r.de := r.hl;
          r.hl := posicion;
        end;
      $EC:
        begin { call PE,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if r.f.p_v then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $ED:
        self.exec_ed_sp;
      $EE:
        begin { xor A,n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          xor_a(temp);
        end;
      $EF:
        begin { rst 28H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := $28;
          r.wz := $28;
        end;
      $F0:
        begin { ret NP >5t o 11t< }
          self.contador := self.contador + 1;
          if not(r.f.s) then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $F1:
        begin { pop AF >10t< }
          self.retraso(r.sp);
          self.contador := self.contador + 3;
          self.retraso(r.sp + 1);
          self.contador := self.contador + 3;
          posicion.w := pop_sp;
          r.a := posicion.h;
          r.f.s := (posicion.l and 128) <> 0;
          r.f.z := (posicion.l and 64) <> 0;
          r.f.bit5 := (posicion.l and 32) <> 0;
          r.f.h := (posicion.l and 16) <> 0;
          r.f.bit3 := (posicion.l and 8) <> 0;
          r.f.p_v := (posicion.l and 4) <> 0;
          r.f.n := (posicion.l and 2) <> 0;
          r.f.c := (posicion.l and 1) <> 0;
        end;
      $F2:
        begin { jp P,nn >10t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if not(r.f.s) then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $F3:
        begin { di >4t< }
          r.iff1 := false;
          r.IFF2 := false;
        end;
      $F4:
        begin { call P,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if not(r.f.s) then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $F5:
        begin { push AF >11t< }
          posicion.h := r.a;
          posicion.l := byte(r.f.s) shl 7;
          posicion.l := posicion.l or (byte(r.f.z) shl 6);
          posicion.l := posicion.l or (byte(r.f.bit5) shl 5);
          posicion.l := posicion.l or (byte(r.f.h) shl 4);
          posicion.l := posicion.l or (byte(r.f.bit3) shl 3);
          posicion.l := posicion.l or (byte(r.f.p_v) shl 2);
          posicion.l := posicion.l or (byte(r.f.n) shl 1);
          posicion.l := posicion.l or byte(r.f.c);
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(posicion.w);
        end;
      $F6:
        begin { or n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          or_a(temp);
        end;
      $F7:
        begin { rst 30H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := $30;
          r.wz := $30;
        end;
      $F8:
        begin { ret M >5t o 11t< }
          self.contador := self.contador + 1;
          if r.f.s then
          begin
            self.retraso(r.sp);
            self.contador := self.contador + 3;
            self.retraso(r.sp + 1);
            self.contador := self.contador + 3;
            r.pc := pop_sp;
            r.wz := r.pc;
          end;
        end;
      $F9:
        begin { ld SP,HL >6t< }
          self.contador := self.contador + 2;
          r.sp := r.hl.w;
        end;
      $FA:
        begin { jp M,nn >10t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          if r.f.s then
            r.pc := posicion.w
          else
            r.pc := r.pc + 2;
          r.wz := r.pc;
        end;
      $FB:
        begin { ei >4t< }
          r.iff1 := True;
          r.IFF2 := True;
          self.after_ei := True;
        end;
      $FC:
        begin { call M,nn >10t o 17t< }
          posicion.l := spec_getbyte(r.pc);
          posicion.h := spec_getbyte(r.pc + 1);
          r.pc := r.pc + 2;
          if r.f.s then
          begin
            self.retraso(r.pc);
            self.contador := self.contador + 1;
            self.retraso(r.sp - 1);
            self.contador := self.contador + 3;
            self.retraso(r.sp - 2);
            self.contador := self.contador + 3;
            push_sp(r.pc);
            r.pc := posicion.w;
          end;
          r.wz := r.pc;
        end;
      $FD:
        self.exec_dd_fd_sp(false);
      $FE:
        begin { cp n >7t< }
          temp := spec_getbyte(r.pc);
          r.pc := r.pc + 1;
          cp_a(temp);
        end;
      $FF:
        begin { rst 38H >11t< }
          self.contador := self.contador + 1;
          self.retraso(r.sp - 1);
          self.contador := self.contador + 3;
          self.retraso(r.sp - 2);
          self.contador := self.contador + 3;
          push_sp(r.pc);
          r.pc := $38;
          r.wz := $38;
        end;
    end; { del case }
    cantidad_t := self.contador - pcontador;
    spectrum_despues_instruccion(cantidad_t);
    timers.update(cantidad_t, self.numero_cpu);
    self.totalt := self.totalt + cantidad_t;
  end; { del while }
end;

procedure cpu_z80_sp.exec_cb_sp;
var
  instruccion, temp: byte;
begin
  self.retraso(r.pc);
  self.contador := self.contador + 4;
  instruccion := self.getbyte(r.pc);
  r.pc := r.pc + 1;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  case instruccion of
    $00:
      rlc_8(@r.bc.h); { rlc B >8t< }
    $01:
      rlc_8(@r.bc.l); { rlc C >8t< }
    $02:
      rlc_8(@r.de.h); { rlc D >8t< }
    $03:
      rlc_8(@r.de.l); { rlc E >8t< }
    $04:
      rlc_8(@r.hl.h); { rlc H >8t< }
    $05:
      rlc_8(@r.hl.l); { rlc L >8t< }
    $06:
      begin { rlc (HL) >15t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        rlc_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $07:
      rlc_8(@r.a); { rlc A >8t< }
    $08:
      rrc_8(@r.bc.h); { rrc B >8t< }
    $09:
      rrc_8(@r.bc.l); { rrc C >8t< }
    $0A:
      rrc_8(@r.de.h); { rrc D >8t< }
    $0B:
      rrc_8(@r.de.l); { rrc E >8t< }
    $0C:
      rrc_8(@r.hl.h); { rrc H >8t< }
    $0D:
      rrc_8(@r.hl.l); { rrc L >8t< }
    $0E:
      begin { rrc (HL) >15t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        rrc_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $0F:
      rrc_8(@r.a); { rrc A >8t< }
    $10:
      rl_8(@r.bc.h); { rl B >8t< }
    $11:
      rl_8(@r.bc.l); { rl C >8t< }
    $12:
      rl_8(@r.de.h); { rl D >8t< }
    $13:
      rl_8(@r.de.l); { rl E >8t< }
    $14:
      rl_8(@r.hl.h); { rl H >8t< }
    $15:
      rl_8(@r.hl.l); { rl L >8t< }
    $16:
      begin { rl (HL) >15t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        rl_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $17:
      rl_8(@r.a); { rl A >8t< }
    $18:
      rr_8(@r.bc.h); { rr B >8t< }
    $19:
      rr_8(@r.bc.l); { rr C >8t< }
    $1A:
      rr_8(@r.de.h); { rr D >8t< }
    $1B:
      rr_8(@r.de.l); { rr E >8t< }
    $1C:
      rr_8(@r.hl.h); { rr H >8t< }
    $1D:
      rr_8(@r.hl.l); { rr L >8t< }
    $1E:
      begin { rr (HL) >15t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        rr_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $1F:
      rr_8(@r.a); { rr A >8t< }
    $20:
      sla_8(@r.bc.h); { sla B >8t< }
    $21:
      sla_8(@r.bc.l); { sla C >8t< }
    $22:
      sla_8(@r.de.h); { sla D >8t< }
    $23:
      sla_8(@r.de.l); { sla E >8t< }
    $24:
      sla_8(@r.hl.h); { sla H >8t< }
    $25:
      sla_8(@r.hl.l); { sla L >8t< }
    $26:
      begin { sla (HL) >15t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        sla_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $27:
      sla_8(@r.a); { sla A >8t< }
    $28:
      sra_8(@r.bc.h); // sra B >8t<
    $29:
      sra_8(@r.bc.l); // sra C >8t<
    $2A:
      sra_8(@r.de.h); // sra D >8t<
    $2B:
      sra_8(@r.de.l); // sra E >8t<
    $2C:
      sra_8(@r.hl.h); // sra H >8t<
    $2D:
      sra_8(@r.hl.l); // sra L >8t<
    $2E:
      begin // sra (HL) >15t<
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        sra_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $2F:
      sra_8(@r.a); // sra A >8t<
    $30:
      sll_8(@r.bc.h); { sll B >8t< }
    $31:
      sll_8(@r.bc.l); { sll C >8t< }
    $32:
      sll_8(@r.de.h); { sll D >8t< }
    $33:
      sll_8(@r.de.l); { sll E >8t< }
    $34:
      sll_8(@r.hl.h); { sll H >8t< }
    $35:
      sll_8(@r.hl.l); { sll L >8t< }
    $36:
      begin { sll (HL) }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        sll_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $37:
      sll_8(@r.a); { sll a >8t< }
    $38:
      srl_8(@r.bc.h); { srl B >8t< }
    $39:
      srl_8(@r.bc.l); { srl C >8t< }
    $3A:
      srl_8(@r.de.h); { srl D >8t< }
    $3B:
      srl_8(@r.de.l); { srl E >8t< }
    $3C:
      srl_8(@r.hl.h); { srl H >8t< }
    $3D:
      srl_8(@r.hl.l); { srl L >8t< }
    $3E:
      begin { srl (HL) >15t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        srl_8(@temp);
        spec_putbyte(r.hl.w, temp);
      end;
    $3F:
      srl_8(@r.a); { srl a >8t< }
    $40:
      bit_8(0, r.bc.h); { bit 0,B >8t< }
    $41:
      bit_8(0, r.bc.l); { bit 0,C >8t< }
    $42:
      bit_8(0, r.de.h); { bit 0,D >8t< }
    $43:
      bit_8(0, r.de.l); { bit 0,E >8t< }
    $44:
      bit_8(0, r.hl.h); { bit 0,H >8t< }
    $45:
      bit_8(0, r.hl.l); { bit 0,L >8t< }
    $46:
      begin { bit 0,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(0, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $47:
      bit_8(0, r.a); { bit 0,A >8t< }
    $48:
      bit_8(1, r.bc.h); { bit 1,B >8t< }
    $49:
      bit_8(1, r.bc.l); { bit 1,C >8t< }
    $4A:
      bit_8(1, r.de.h); { bit 1,D >8t< }
    $4B:
      bit_8(1, r.de.l); { bit 1,E >8t< }
    $4C:
      bit_8(1, r.hl.h); { bit 1,H >8t< }
    $4D:
      bit_8(1, r.hl.l); { bit 1,L >8t< }
    $4E:
      begin { bit 1,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(1, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $4F:
      bit_8(1, r.a); { bit 1,A >8t< }
    $50:
      bit_8(2, r.bc.h); { bit 2,B >8t< }
    $51:
      bit_8(2, r.bc.l); { bit 2,C >8t< }
    $52:
      bit_8(2, r.de.h); { bit 2,D >8t< }
    $53:
      bit_8(2, r.de.l); { bit 2,E >8t< }
    $54:
      bit_8(2, r.hl.h); { bit 2,H >8t< }
    $55:
      bit_8(2, r.hl.l); { bit 2,L >8t< }
    $56:
      begin { bit 2,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(2, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $57:
      bit_8(2, r.a); { bit 2,A >8t< }
    $58:
      bit_8(3, r.bc.h); { bit 3,B >8t< }
    $59:
      bit_8(3, r.bc.l); { bit 3,C >8t< }
    $5A:
      bit_8(3, r.de.h); { bit 3,D >8t< }
    $5B:
      bit_8(3, r.de.l); { bit 3,E >8t< }
    $5C:
      bit_8(3, r.hl.h); { bit 3,H >8t< }
    $5D:
      bit_8(3, r.hl.l); { bit 3,L >8t< }
    $5E:
      begin { bit 3,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(3, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $5F:
      bit_8(3, r.a); { bit 3,A >8t< }
    $60:
      bit_8(4, r.bc.h); { bit 4,B >8t< }
    $61:
      bit_8(4, r.bc.l); { bit 4,C >8t< }
    $62:
      bit_8(4, r.de.h); { bit 4,D >8t< }
    $63:
      bit_8(4, r.de.l); { bit 4,E >8t< }
    $64:
      bit_8(4, r.hl.h); { bit 4,H >8t< }
    $65:
      bit_8(4, r.hl.l); { bit 4,L >8t< }
    $66:
      begin { bit 4,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(4, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $67:
      bit_8(4, r.a); { bit 4,A >8t< }
    $68:
      bit_8(5, r.bc.h); { bit 5,B >8t< }
    $69:
      bit_8(5, r.bc.l); { bit 5,C >8t< }
    $6A:
      bit_8(5, r.de.h); { bit 5,D >8t< }
    $6B:
      bit_8(5, r.de.l); { bit 5,E >8t< }
    $6C:
      bit_8(5, r.hl.h); { bit 5,H >8t< }
    $6D:
      bit_8(5, r.hl.l); { bit 5,L >8t< }
    $6E:
      begin { bit 5,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(5, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $6F:
      bit_8(5, r.a); { bit 5,A >8t< }
    $70:
      bit_8(6, r.bc.h); { bit 6,B >8t< }
    $71:
      bit_8(6, r.bc.l); { bit 6,C >8t< }
    $72:
      bit_8(6, r.de.h); { bit 6,D >8t< }
    $73:
      bit_8(6, r.de.l); { bit 6,E >8t< }
    $74:
      bit_8(6, r.hl.h); { bit 6,H >8t< }
    $75:
      bit_8(6, r.hl.l); { bit 6,L >8t< }
    $76:
      begin { bit 6,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_8(6, temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $77:
      bit_8(6, r.a); { bit 6,A >8t< }
    $78:
      bit_7(r.bc.h); { bit 7,B >8t< }
    $79:
      bit_7(r.bc.l); { bit 7,C >8t< }
    $7A:
      bit_7(r.de.h); { bit 7,D >8t< }
    $7B:
      bit_7(r.de.l); { bit 7,E >8t< }
    $7C:
      bit_7(r.hl.h); { bit 7,H >8t< }
    $7D:
      bit_7(r.hl.l); { bit 7,L >8t< }
    $7E:
      begin { bit 7,(HL) >12t< }
        temp := spec_getbyte(r.hl.w);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        bit_7(temp);
        r.f.bit5 := (r.wz and $2000) <> 0;
        r.f.bit3 := (r.wz and $800) <> 0;
      end;
    $7F:
      bit_7(r.a); { bit 7,A >8t< }
    $80:
      r.bc.h := (r.bc.h and $FE); { res 0,B >8t< }
    $81:
      r.bc.l := (r.bc.l and $FE); { res 0,C >8t< }
    $82:
      r.de.h := (r.de.h and $FE); { res 0,D >8t< }
    $83:
      r.de.l := (r.de.l and $FE); { res 0,E >8t< }
    $84:
      r.hl.h := (r.hl.h and $FE); { res 0,H >8t< }
    $85:
      r.hl.l := (r.hl.l and $FE); { res 0,L >8t< }
    $86:
      begin { res 0,(hl) >15t< }
        temp := (spec_getbyte(r.hl.w) and $FE);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $87:
      r.a := r.a and $FE; { res 0,A >8t< }
    $88:
      r.bc.h := r.bc.h and $FD; { res 1,B >8t< }
    $89:
      r.bc.l := r.bc.l and $FD; { res 1,C >8t< }
    $8A:
      r.de.h := r.de.h and $FD; { res 1,D >8t< }
    $8B:
      r.de.l := r.de.l and $FD; { res 1,E >8t< }
    $8C:
      r.hl.h := r.hl.h and $FD; { res 1,H >8t< }
    $8D:
      r.hl.l := r.hl.l and $FD; { res 1,L >8t< }
    $8E:
      begin { res 1,(hl) >15t< }
        temp := (spec_getbyte(r.hl.w) and $FD);
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $8F:
      r.a := r.a and $FD; { res 1,A >8t< }
    $90:
      r.bc.h := r.bc.h and $FB; { res 2,B >8t< }
    $91:
      r.bc.l := r.bc.l and $FB; { res 2,C >8t< }
    $92:
      r.de.h := r.de.h and $FB; { res 2,D >8t< }
    $93:
      r.de.l := r.de.l and $FB; { res 2,E >8t< }
    $94:
      r.hl.h := r.hl.h and $FB; { res 2,H >8t< }
    $95:
      r.hl.l := r.hl.l and $FB; { res 2,L >8t< }
    $96:
      begin { res 2,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) and $FB;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $97:
      r.a := r.a and $FB; { res 2,A }
    $98:
      r.bc.h := r.bc.h and $F7; { res 3,B >8t< }
    $99:
      r.bc.l := r.bc.l and $F7; { res 3,C >8t< }
    $9A:
      r.de.h := r.de.h and $F7; { res 3,D >8t< }
    $9B:
      r.de.l := r.de.l and $F7; { res 3,E >8t< }
    $9C:
      r.hl.h := r.hl.h and $F7; { res 3,H >8t< }
    $9D:
      r.hl.l := r.hl.l and $F7; { res 3,L >8t< }
    $9E:
      begin { res 3,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) and $F7;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $9F:
      r.a := r.a and $F7; { res 3,A }
    $A0:
      r.bc.h := r.bc.h and $EF; { res 4,B >8t< }
    $A1:
      r.bc.l := r.bc.l and $EF; { res 4,C >8t< }
    $A2:
      r.de.h := r.de.h and $EF; { res 4,D >8t< }
    $A3:
      r.de.l := r.de.l and $EF; { res 4,E >8t< }
    $A4:
      r.hl.h := r.hl.h and $EF; { res 4,H >8t< }
    $A5:
      r.hl.l := r.hl.l and $EF; { res 4,L >8t< }
    $A6:
      begin { res 4,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) and $EF;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $A7:
      r.a := r.a and $EF; { res 4,A >8t< }
    $A8:
      r.bc.h := r.bc.h and $DF; { res 5,B >8t< }
    $A9:
      r.bc.l := r.bc.l and $DF; { res 5,C >8t< }
    $AA:
      r.de.h := r.de.h and $DF; { res 5,D >8t< }
    $AB:
      r.de.l := r.de.l and $DF; { res 5,E >8t< }
    $AC:
      r.hl.h := r.hl.h and $DF; { res 5,H >8t< }
    $AD:
      r.hl.l := r.hl.l and $DF; { res 5,L >8t< }
    $AE:
      begin { res 5,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) and $DF;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $AF:
      r.a := r.a and $DF; { res 5,A >8t< }
    $B0:
      r.bc.h := r.bc.h and $BF; { res 6,B >8t< }
    $B1:
      r.bc.l := r.bc.l and $BF; { res 6,C >8t< }
    $B2:
      r.de.h := r.de.h and $BF; { res 6,D >8t< }
    $B3:
      r.de.l := r.de.l and $BF; { res 6,E >8t< }
    $B4:
      r.hl.h := r.hl.h and $BF; { res 6,H >8t< }
    $B5:
      r.hl.l := r.hl.l and $BF; { res 6,L >8t< }
    $B6:
      begin { res 6,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) and $BF;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $B7:
      r.a := r.a and $BF; { res 6,A >8t< }
    $B8:
      r.bc.h := r.bc.h and $7F; { res 7,B >8t< }
    $B9:
      r.bc.l := r.bc.l and $7F; { res 7,C >8t< }
    $BA:
      r.de.h := r.de.h and $7F; { res 7,D >8t< }
    $BB:
      r.de.l := r.de.l and $7F; { res 7,E >8t< }
    $BC:
      r.hl.h := r.hl.h and $7F; { res 7,H >8t< }
    $BD:
      r.hl.l := r.hl.l and $7F; { res 7,L >8t< }
    $BE:
      begin { res 7,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) and $7F;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $BF:
      r.a := r.a and $7F; { res 7,A >8t< }
    $C0:
      r.bc.h := r.bc.h or $1; { set 0,B >8t< }
    $C1:
      r.bc.l := r.bc.l or $1; { set 0,C >8t< }
    $C2:
      r.de.h := r.de.h or $1; { set 0,D >8t< }
    $C3:
      r.de.l := r.de.l or $1; { set 0,E >8t< }
    $C4:
      r.hl.h := r.hl.h or $1; { set 0,H >8t< }
    $C5:
      r.hl.l := r.hl.l or $1; { set 0,L >8t< }
    $C6:
      begin { set 0,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $C7:
      r.a := r.a or $1; { set 0,A >8t< }
    $C8:
      r.bc.h := r.bc.h or $2; { set 1,B >8t< }
    $C9:
      r.bc.l := r.bc.l or $2; { set 1,C >8t< }
    $CA:
      r.de.h := r.de.h or $2; { set 1,D >8t< }
    $CB:
      r.de.l := r.de.l or $2; { set 1,E >8t< }
    $CC:
      r.hl.h := r.hl.h or $2; { set 1,H >8t< }
    $CD:
      r.hl.l := r.hl.l or $2; { set 1,L >8t< }
    $CE:
      begin { set 1,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or 2;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $CF:
      r.a := r.a or $2; { set 1,A >8t< }
    $D0:
      r.bc.h := r.bc.h or $4; { set 2,B >8t< }
    $D1:
      r.bc.l := r.bc.l or $4; { set 2,C >8t< }
    $D2:
      r.de.h := r.de.h or $4; { set 2,D >8t< }
    $D3:
      r.de.l := r.de.l or $4; { set 2,E >8t< }
    $D4:
      r.hl.h := r.hl.h or $4; { set 2,H >8t< }
    $D5:
      r.hl.l := r.hl.l or $4; { set 2,L >8t< }
    $D6:
      begin { set 2,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or 4;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $D7:
      r.a := r.a or $4; { set 2,A >8t< }
    $D8:
      r.bc.h := r.bc.h or $8; { set 3,B >8t< }
    $D9:
      r.bc.l := r.bc.l or $8; { set 3,C >8t< }
    $DA:
      r.de.h := r.de.h or $8; { set 3,D >8t< }
    $DB:
      r.de.l := r.de.l or $8; { set 3,E >8t< }
    $DC:
      r.hl.h := r.hl.h or $8; { set 3,H >8t< }
    $DD:
      r.hl.l := r.hl.l or $8; { set 3,L >8t< }
    $DE:
      begin { set 3,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or 8;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $DF:
      r.a := r.a or $8; { set 3,A >8t< }
    $E0:
      r.bc.h := r.bc.h or $10; { set 4,B >8t< }
    $E1:
      r.bc.l := r.bc.l or $10; { set 4,C >8t< }
    $E2:
      r.de.h := r.de.h or $10; { set 4,D >8t< }
    $E3:
      r.de.l := r.de.l or $10; { set 4,E >8t< }
    $E4:
      r.hl.h := r.hl.h or $10; { set 4,H >8t< }
    $E5:
      r.hl.l := r.hl.l or $10; { set 4,L >8t< }
    $E6:
      begin { set 4,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or $10;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $E7:
      r.a := r.a or $10; { set 4,A >8t< }
    $E8:
      r.bc.h := r.bc.h or $20; { set 5,B >8t< }
    $E9:
      r.bc.l := r.bc.l or $20; { set 5,C >8t< }
    $EA:
      r.de.h := r.de.h or $20; { set 5,D >8t< }
    $EB:
      r.de.l := r.de.l or $20; { set 5,E >8t< }
    $EC:
      r.hl.h := r.hl.h or $20; { set 5,H >8t< }
    $ED:
      r.hl.l := r.hl.l or $20; { set 5,L >8t< }
    $EE:
      begin { set 5,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or $20;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $EF:
      r.a := r.a or $20; { set 5,A >8t< }
    $F0:
      r.bc.h := r.bc.h or $40; { set 6,B >8t< }
    $F1:
      r.bc.l := r.bc.l or $40; { set 6,C >8t< }
    $F2:
      r.de.h := r.de.h or $40; { set 6,D >8t< }
    $F3:
      r.de.l := r.de.l or $40; { set 6,E >8t< }
    $F4:
      r.hl.h := r.hl.h or $40; { set 6,H >8t< }
    $F5:
      r.hl.l := r.hl.l or $40; { set 6,L >8t< }
    $F6:
      begin { set 6,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or $40;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $F7:
      r.a := r.a or $40; { set 6,A >8t< }
    $F8:
      r.bc.h := r.bc.h or $80; { set 7,B >8t< }
    $F9:
      r.bc.l := r.bc.l or $80; { set 7,C >8t< }
    $FA:
      r.de.h := r.de.h or $80; { set 7,D >8t< }
    $FB:
      r.de.l := r.de.l or $80; { set 7,E >8t< }
    $FC:
      r.hl.h := r.hl.h or $80; { set 7,H >8t< }
    $FD:
      r.hl.l := r.hl.l or $80; { set 7,L >8t< }
    $FE:
      begin { set 7,(HL) >15t< }
        temp := spec_getbyte(r.hl.w) or $80;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        spec_putbyte(r.hl.w, temp);
      end;
    $FF:
      r.a := r.a or $80; { set 7,A >8t< }
  end;
end;

procedure cpu_z80_sp.exec_dd_fd_sp(tipo: boolean);
var
  instruccion, temp: byte;
  temp2, temp_cont: word;
  registro: pparejas;
  posicion: parejas;
begin
  if tipo then
    registro := @r.ix
  else
    registro := @r.iy;
  temp2 := registro.w;
  temp_cont := self.contador;
  self.retraso(r.pc);
  self.contador := self.contador + 4;
  instruccion := self.getbyte(r.pc);
  r.pc := r.pc + 1;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  case instruccion of
    $09:
      begin // add IX,BC >15t<
        self.contador := self.contador + 7;
        registro.w := add_16(registro.w, r.bc.w);
      end;
    $19:
      begin // add IX,DE >15t<
        self.contador := self.contador + 7;
        registro.w := add_16(registro.w, r.de.w);
      end;
    $21:
      begin { ld IX,nn >14t< }
        registro.l := spec_getbyte(r.pc);
        registro.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
      end;
    $22:
      begin { ld (nn),IX >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        spec_putbyte(posicion.w, registro.l);
        spec_putbyte(posicion.w + 1, registro.h);
        r.wz := posicion.w + 1;
      end;
    $23:
      begin { inc IX >10t< }
        self.contador := self.contador + 2;
        inc(registro.w);
      end;
    $24:
      begin // inc IXh >9t<
        registro.h := inc_8(registro.h);
        inc(self.contador);
      end;
    $25:
      begin // dec IXh >9t<
        registro.h := dec_8(registro.h);
        inc(self.contador);
      end;
    $26:
      begin { ld IXh,n >11t< }
        registro.h := spec_getbyte(r.pc);
        r.pc := r.pc + 1;
      end;
    $29:
      begin // add IX,IX >15t<
        inc(self.contador, 7);
        registro.w := add_16(registro.w, registro.w);
      end;
    $2A:
      begin { ld (IX,(nn) >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        registro.l := spec_getbyte(posicion.w);
        registro.h := spec_getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    $2B:
      begin { dec IX >10t< }
        inc(self.contador, 2);
        dec(registro.w);
      end;
    $2C:
      begin // inc IXl >9t<
        registro.l := inc_8(registro.l);
        inc(self.contador);
      end;
    $2D:
      begin // dec IXl >9t<
        registro.l := dec_8(registro.l);
        inc(self.contador);
      end;
    $2E:
      begin { ld IXl,n >11t< }
        registro.l := spec_getbyte(r.pc);
        r.pc := r.pc + 1;
      end;
    $34:
      begin { inc (IX+d) >23t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        self.retraso(temp2);
        self.contador := self.contador + 1;
        spec_putbyte(temp2, inc_8(temp));
      end;
    $35:
      begin { dec (IX+d) >23t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        self.retraso(temp2);
        self.contador := self.contador + 1;
        spec_putbyte(temp2, dec_8(temp));
      end;
    $36:
      begin { ld (IX+d),n >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        r.pc := r.pc + 1;
        temp := spec_getbyte(r.pc);
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        spec_putbyte(temp2, temp);
      end;
    $39:
      begin // add IX,SP >15t<
        self.contador := self.contador + 7;
        registro.w := add_16(registro.w, r.sp);
      end;
    $44:
      begin { ld B,IXh >9t< }
        r.bc.h := registro^.h;
        self.contador := self.contador + 1;
      end;
    $45:
      begin { ld B,IXl >9t< }
        r.bc.h := registro^.l;
        self.contador := self.contador + 1;
      end;
    $46:
      begin { ld B,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        r.bc.h := spec_getbyte(temp2);
      end;
    $4C:
      begin { ld C,IXh >9t< }
        r.bc.l := registro^.h;
        self.contador := self.contador + 1;
      end;
    $4D:
      begin { ld C,IXl >9t< }
        r.bc.l := registro^.l;
        self.contador := self.contador + 1;
      end;
    $4E:
      begin { ld C,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        r.bc.l := spec_getbyte(temp2);
      end;
    $54:
      begin { ld D,IXh >9t< }
        r.de.h := registro^.h;
        self.contador := self.contador + 1;
      end;
    $55:
      begin { ld D,IXl >9t< }
        r.de.h := registro^.l;
        self.contador := self.contador + 1;
      end;
    $56:
      begin { ld D,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        r.de.h := spec_getbyte(temp2);
      end;
    $5C:
      begin { ld E,IXh >9t< }
        r.de.l := registro^.h;
        self.contador := self.contador + 1;
      end;
    $5D:
      begin { ld E,IXh >9t< }
        r.de.l := registro^.l;
        self.contador := self.contador + 1;
      end;
    $5E:
      begin { ld E,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        self.retraso(r.pc);
        self.contador := self.contador + 1;
        r.pc := r.pc + 1;
        r.de.l := spec_getbyte(temp2);
      end;
    $60:
      begin { ld IXh,B >9t< }
        registro^.h := r.bc.h;
        inc(self.contador);
      end;
    $61:
      begin { ld IXh,C >9t< }
        registro^.h := r.bc.l;
        inc(self.contador);
      end;
    $62:
      begin { ld IXh,D >9t< }
        registro^.h := r.de.h;
        inc(self.contador);
      end;
    $63:
      begin { ld IXh,E >9t< }
        registro^.h := r.de.l;
        inc(self.contador);
      end;
    $64:
      inc(self.contador); { ld IXh,IXh >9t> }
    $65:
      begin { ld IXh,IXl >9t< }
        registro^.h := registro^.l;
        inc(self.contador);
      end;
    $66:
      begin { ld H,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        r.hl.h := spec_getbyte(temp2);
      end;
    $67:
      begin { ld IXh,A >9t< }
        registro^.h := r.a;
        inc(self.contador);
      end;
    $68:
      begin { ld IXl,B >9t< }
        registro^.l := r.bc.h;
        inc(self.contador);
      end;
    $69:
      begin { ld IXl,C >9t< }
        registro^.l := r.bc.l;
        inc(self.contador);
      end;
    $6A:
      begin { ld IXl,D >9t< }
        registro^.l := r.de.h;
        inc(self.contador);
      end;
    $6B:
      begin { ld IXl,E >9t< }
        registro^.l := r.de.l;
        inc(self.contador);
      end;
    $6C:
      begin { ld IXl,IXh >9t< }
        registro^.l := registro^.h;
        inc(self.contador);
      end;
    $6D:
      inc(self.contador); { ld IXl,IXl >9t< }
    $6E:
      begin { ld L,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        r.hl.l := spec_getbyte(temp2);
      end;
    $6F:
      begin { ld IXl,A >9t< }
        registro^.l := r.a;
        inc(self.contador);
      end;
    $70:
      begin { ld (IX+d),B >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.bc.h);
      end;
    $71:
      begin { ld (IX+d),C >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.bc.l);
      end;
    $72:
      begin { ld (IX+d),D >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.de.h);
      end;
    $73:
      begin { ld (IX+d),E >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.de.l);
      end;
    $74:
      begin { ld (IX+d),H >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.hl.h);
      end;
    $75:
      begin { ld (IX+d),L >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.hl.l);
      end;
    $77:
      begin { ld (IX+d),A >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        spec_putbyte(temp2, r.a);
      end;
    $7C:
      begin { ld A,IXh >9t< }
        r.a := registro^.h;
        inc(self.contador);
      end;
    $7D:
      begin { ld A,IXl >9t< }
        r.a := registro^.l;
        inc(self.contador);
      end;
    $7E:
      begin { ld A,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        r.a := spec_getbyte(temp2);
      end;
    $84:
      begin { add A,IXh >9t< }
        add_8(registro.h);
        inc(self.contador);
      end;
    $85:
      begin { add A,IXl >9t< }
        add_8(registro.l);
        inc(self.contador);
      end;
    $86:
      begin { add A,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        add_8(temp);
      end;
    $8C:
      begin { adc A,IXh >9t< }
        adc_8(registro^.h);
        inc(self.contador);
      end;
    $8D:
      begin { adc A,IXl >9t< }
        adc_8(registro^.l);
        inc(self.contador);
      end;
    $8E:
      begin { adc A,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        adc_8(temp);
      end;
    $94:
      begin { sub IXh >9t< }
        sub_8(registro^.h);
        inc(self.contador);
      end;
    $95:
      begin { sub IXh >9t< }
        sub_8(registro^.l);
        inc(self.contador);
      end;
    $96:
      begin { sub (IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        sub_8(temp);
      end;
    $9C:
      begin { sbc IXh >9t< }
        sbc_8(registro^.h);
        inc(self.contador);
      end;
    $9D:
      begin { sbc IXl >9t< }
        sbc_8(registro^.l);
        inc(self.contador);
      end;
    $9E:
      begin { sbc (IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        sbc_8(temp);
      end;
    $A4:
      begin { and IXh >9t< }
        and_a(registro^.h);
        inc(self.contador);
      end;
    $A5:
      begin { and IXl >9t< }
        and_a(registro^.l);
        inc(self.contador);
      end;
    $A6:
      begin { and A,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        and_a(temp);
      end;
    $AC:
      begin { xor IXh >9t< }
        xor_a(registro^.h);
        inc(self.contador);
      end;
    $AD:
      begin { xor IXl >9t< }
        xor_a(registro^.l);
        inc(self.contador);
      end;
    $AE:
      begin { xor A,(IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        xor_a(temp);
      end;
    $B4:
      begin { or IXh >9t< }
        or_a(registro^.h);
        inc(self.contador);
      end;
    $B5:
      begin { or IXl >9t< }
        or_a(registro^.l);
        inc(self.contador);
      end;
    $B6:
      begin { or (IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        or_a(temp);
      end;
    $BC:
      begin { cp IXh >9t< }
        cp_a(registro^.h);
        inc(self.contador);
      end;
    $BD:
      begin { cp IXl >9t< }
        cp_a(registro^.l);
        inc(self.contador);
      end;
    $BE:
      begin { cp (IX+d) >19t< }
        temp2 := temp2 + shortint(spec_getbyte(r.pc));
        r.wz := temp2;
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        self.retraso(r.pc);
        inc(self.contador);
        r.pc := r.pc + 1;
        temp := spec_getbyte(temp2);
        cp_a(temp);
      end;
    $CB:
      self.exec_dd_cb_sp(tipo);
    $DD, $FD:
      self.exec_dd_fd_sp(tipo);
    $E1:
      begin
        self.retraso(r.sp);
        inc(self.contador, 3);
        self.retraso(r.sp + 1);
        inc(self.contador, 3);
        registro.w := pop_sp; { pop IX >14t< }
      end;
    $E3:
      begin { ex (SP),IX >23t< }
        self.retraso(r.sp);
        inc(self.contador, 3);
        self.retraso(r.sp + 1);
        inc(self.contador, 3);
        posicion.w := pop_sp;
        self.retraso(r.sp + 1);
        inc(self.contador);
        push_sp(registro.w);
        self.retraso(r.sp + 1);
        inc(self.contador, 3);
        self.retraso(r.sp);
        inc(self.contador, 3);
        self.retraso(r.sp);
        inc(self.contador);
        self.retraso(r.sp);
        inc(self.contador);
        registro.w := posicion.w;
        r.wz := posicion.w;
      end;
    $E5:
      begin { push IX >15t< }
        inc(self.contador);
        self.retraso(r.sp - 1);
        inc(self.contador, 3);
        self.retraso(r.sp - 2);
        inc(self.contador, 3);
        push_sp(registro.w);
      end;
    $E9:
      r.pc := registro.w; { jp IX >8t< }
    $F9:
      begin { ld SP,IX >10t< }
        inc(self.contador, 2);
        r.sp := registro.w;
      end;
  else
    begin
      dec(r.pc);
      self.contador := temp_cont;
    end;
  end;
end;

procedure cpu_z80_sp.exec_dd_cb_sp(tipo: boolean);
var
  tempb, instruccion: byte;
  temp2: word;
begin
  if tipo then
    temp2 := r.ix.w
  else
    temp2 := r.iy.w;
  self.retraso(r.pc);
  inc(self.contador, 3);
  instruccion := self.getbyte(r.pc);
  r.pc := r.pc + 1;
  self.retraso(r.pc);
  inc(self.contador, 3);
  temp2 := temp2 + shortint(instruccion);
  r.wz := temp2;
  instruccion := self.getbyte(r.pc);
  self.retraso(r.pc);
  inc(self.contador);
  self.retraso(r.pc);
  inc(self.contador);
  r.pc := r.pc + 1; // >16t<
  case instruccion of
    $00:
      begin { ld B,rlc (IX+d) >23t< }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $01:
      begin { ld C,rlc (IX+d) >23t< }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $02:
      begin { ld D,rlc (IX+d) >23t< }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $03:
      begin { ld E,rlc (IX+d) >23t< }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $04:
      begin { ld H,rlc (IX+d) >23t< }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $05:
      begin { ld L,rlc (IX+d) >23t< }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $06:
      begin { rlc (IX+d) >23t< }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $07:
      begin { ld A,rlc (IX+d) >23t< }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $08:
      begin { ld B,rrc (IX+d) >23t< }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $09:
      begin { ld C,rrc (IX+d) >23t< }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $0A:
      begin { ld D,rrc (IX+d) >23t< }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $0B:
      begin { ld E,rrc (IX+d) >23t< }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $0C:
      begin { ld H,rrc (IX+d) >23t< }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $0D:
      begin { ld L,rlc (IX+d) >23t< }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $0E:
      begin { rrc (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $0F:
      begin { ld A,rrc (IX+d) }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rrc_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $10:
      begin { ld B,rl (IX+d) }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $11:
      begin { ld C,rl (IX+d) }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $12:
      begin { ld D,rl (IX+d) }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $13:
      begin { ld E,rl (IX+d) }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $14:
      begin { ld H,rl (IX+d) }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $15:
      begin { ld L,rlc (IX+d) }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $16:
      begin { rl (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $17:
      begin { ld A,rl (IX+d) }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rl_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $18:
      begin { ld B,rr (IX+d) }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $19:
      begin { ld C,rr (IX+d) }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $1A:
      begin { ld D,rr (IX+d) }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $1B:
      begin { ld E,rr (IX+d) }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $1C:
      begin { ld H,rr (IX+d) }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $1D:
      begin { ld L,rr (IX+d) }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $1E:
      begin { rr (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $1F:
      begin { ld A,rr (IX+d) }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rr_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $20:
      begin { ld B,sla (IX+d) }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $21:
      begin { ld C,sla (IX+d) }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $22:
      begin { ld D,sla (IX+d) }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $23:
      begin { ld E,sla (IX+d) }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        rlc_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $24:
      begin { ld H,sla (IX+d) }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $25:
      begin { ld L,sla (IX+d) }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $26:
      begin { sla (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $27:
      begin { ld A,sla (IX+d) }
        tempb := spec_getbyte(temp2);
        r.a := tempb;
        self.retraso(temp2);
        inc(self.contador);
        sla_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $28:
      begin // ld B,sra (IX+d)
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $29:
      begin // ld C,sra (IX+d)
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $2A:
      begin // ld D,sra (IX+d)
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $2B:
      begin // ld E,sra (IX+d)
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $2C:
      begin // ld H,sra (IX+d)
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $2D:
      begin // ld L,sra (IX+d)
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $2E:
      begin { sra (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $2F:
      begin { ld A,sra (IX+d) }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sra_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $30:
      begin { ld B,sll (IX+d) }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $31:
      begin { ld C,sll (IX+d) }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $32:
      begin { ld D,sll (IX+d) }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $33:
      begin { ld E,sll (IX+d) }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $34:
      begin { ld H,sll (IX+d) }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $35:
      begin { ld L,sll (IX+d) }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $36:
      begin { sll (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $37:
      begin { ld A,sll (IX+d) }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        sll_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $38:
      begin { ld B,srl (IX+d) }
        r.bc.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.bc.h);
        spec_putbyte(temp2, r.bc.h);
      end;
    $39:
      begin { ld C,srl (IX+d) }
        r.bc.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.bc.l);
        spec_putbyte(temp2, r.bc.l);
      end;
    $3A:
      begin { ld D,srl (IX+d) }
        r.de.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.de.h);
        spec_putbyte(temp2, r.de.h);
      end;
    $3B:
      begin { ld E,srl (IX+d) }
        r.de.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.de.l);
        spec_putbyte(temp2, r.de.l);
      end;
    $3C:
      begin { ld H,srl (IX+d) }
        r.hl.h := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.hl.h);
        spec_putbyte(temp2, r.hl.h);
      end;
    $3D:
      begin { ld L,srl (IX+d) }
        r.hl.l := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.hl.l);
        spec_putbyte(temp2, r.hl.l);
      end;
    $3E:
      begin { srl (IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@tempb);
        spec_putbyte(temp2, tempb);
      end;
    $3F:
      begin { ld A,srl (IX+d) }
        r.a := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        srl_8(@r.a);
        spec_putbyte(temp2, r.a);
      end;
    $40 .. $47:
      begin { bit 0,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(0, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $48 .. $4F:
      begin { bit 1,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(1, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $50 .. $57:
      begin { bit 2,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(2, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $58 .. $5F:
      begin { bit 3,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(3, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $60 .. $67:
      begin { bit 4,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(4, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $68 .. $6F:
      begin { bit 5,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(5, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $70 .. $77:
      begin { bit 6,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_8(6, tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $78 .. $7F:
      begin { bit 7,(IX+d) }
        tempb := spec_getbyte(temp2);
        self.retraso(temp2);
        inc(self.contador);
        bit_7(tempb);
        r.f.bit5 := (temp2 and $2000) <> 0;
        r.f.bit3 := (temp2 and $800) <> 0;
      end;
    $80:
      begin { ld B,res 0,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $81:
      begin { ld C,res 0,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $82:
      begin { ld D,res 0,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $83:
      begin { ld E,res 0,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $84:
      begin { ld H,res 0,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $85:
      begin { ld L,res 0,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $86:
      begin { res 0,(IX+d) }
        tempb := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $87:
      begin // LD A,RES 0,(REGISTER+dd) */
        r.a := spec_getbyte(temp2) and $FE;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $88:
      begin // LD B,RES 1,(REGISTER+dd) */
        r.bc.h := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $89:
      begin // LD C,RES 1,(REGISTER+dd) */
        r.bc.l := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $8A:
      begin // LD D,RES 1,(REGISTER+dd) */
        r.de.h := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $8B:
      begin // LD E,RES 1,(REGISTER+dd) */
        r.de.l := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $8C:
      begin // LD H,RES 1,(REGISTER+dd) */
        r.hl.h := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $8D:
      begin // LD L,RES 1,(REGISTER+dd) */
        r.hl.l := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $8E:
      begin { res 1,(IX+d) }
        tempb := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $8F:
      begin { ld A,res 1,(IX+d) }
        r.a := spec_getbyte(temp2) and $FD;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $90:
      begin { ld B,res 2,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $91:
      begin { ld C,res 2,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $92:
      begin { ld D,res 2,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $93:
      begin { ld E,res 2,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $94:
      begin { ld H,res 2,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $95:
      begin { ld L,res 2,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $96:
      begin { res 2,(IX+d) }
        tempb := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $97:
      begin { ld A,res 2,(IX+d) }
        r.a := spec_getbyte(temp2) and $FB;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $98:
      begin { ld B,res 3,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $99:
      begin { ld C,res 3,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $9A:
      begin { ld D,res 3,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $9B:
      begin { ld E,res 3,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $9C:
      begin { ld H,res 3,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $9D:
      begin { ld L,res 3,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $9E:
      begin { res 3,(IX+d) }
        tempb := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $9F:
      begin { ld A,res 3,(IX+d) }
        r.a := spec_getbyte(temp2) and $F7;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $A0:
      begin { ld B,res 4,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $A1:
      begin { ld C,res 4,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $A2:
      begin { ld D,res 4,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $A3:
      begin { ld E,res 4,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $A4:
      begin { ld H,res 4,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $A5:
      begin { ld L,res 4,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $A6:
      begin { res 4,(IX+d) }
        tempb := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $A7:
      begin { ld A,res 4,(IX+d) }
        r.a := spec_getbyte(temp2) and $EF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $A8:
      begin { ld B,res 5,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $A9:
      begin { ld C,res 5,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $AA:
      begin { ld D,res 5,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $AB:
      begin { ld E,res 5,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $AC:
      begin { ld H,res 5,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $AD:
      begin { ld L,res 5,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $AE:
      begin { res 5,(IX+d) }
        tempb := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $AF:
      begin { ld A,res 5,(IX+d) }
        r.a := spec_getbyte(temp2) and $DF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $B0:
      begin { ld B,res 6,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $B1:
      begin { ld C,res 6,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $B2:
      begin { ld D,res 6,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $B3:
      begin { ld E,res 6,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $B4:
      begin { ld H,res 6,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $B5:
      begin { ld L,res 6,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $B6:
      begin { res 6,(IX+d) }
        tempb := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $B7:
      begin { ld A,res 6,(IX+d) }
        r.a := spec_getbyte(temp2) and $BF;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $B8:
      begin { ld B,res 7,(IX+d) }
        r.bc.h := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $B9:
      begin { ld C,res 7,(IX+d) }
        r.bc.l := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $BA:
      begin { ld D,res 7,(IX+d) }
        r.de.h := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $BB:
      begin { ld E,res 7,(IX+d) }
        r.de.l := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $BC:
      begin { ld H,res 7,(IX+d) }
        r.hl.h := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $BD:
      begin { ld L,res 7,(IX+d) }
        r.hl.l := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $BE:
      begin { res 7,(IX+d) }
        tempb := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $BF:
      begin { ld A,res 7,(IX+d) }
        r.a := spec_getbyte(temp2) and $7F;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $C0:
      begin { ld B,set 0,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $C1:
      begin { ld C,set 0,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $C2:
      begin { ld D,set 0,(IX+d) }
        r.de.h := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $C3:
      begin { ld E,set 0,(IX+d) }
        r.de.l := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $C4:
      begin { ld H,set 0,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $C5:
      begin { ld L,set 0,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $C6:
      begin { set 0,(IX+d) }
        tempb := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $C7:
      begin { ld A,set 0,(IX+d) }
        r.a := spec_getbyte(temp2) or 1;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $C8:
      begin { ld B,set 1,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $C9:
      begin { ld C,set 1,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $CA:
      begin { ld D,set 1,(IX+d) }
        r.de.h := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $CB:
      begin { ld E,set 1,(IX+d) }
        r.de.l := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $CC:
      begin { ld H,set 1,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $CD:
      begin { ld L,set 1,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $CE:
      begin { set 1,(IX+d) }
        tempb := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;

    $CF:
      begin { ld A,set 1,(IX+d) }
        r.a := spec_getbyte(temp2) or 2;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $D0:
      begin { ld B,set 2,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $D1:
      begin { ld C,set 2,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $D2:
      begin { ld D,set 2,(IX+d) }
        r.de.h := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $D3:
      begin { ld E,set 2,(IX+d) }
        r.de.l := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $D4:
      begin { ld H,set 2,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $D5:
      begin { ld L,set 2,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $D6:
      begin { set 2,(IX+d) }
        tempb := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $D7:
      begin { ld A,set 2,(IX+d) }
        r.a := spec_getbyte(temp2) or 4;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $D8:
      begin { ld B,set 3,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $D9:
      begin { ld C,set 3,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $DA:
      begin { ld D,set 3,(IX+d) }
        r.de.h := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $DB:
      begin { ld E,set 3,(IX+d) }
        r.de.l := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $DC:
      begin { ld H,set 3,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $DD:
      begin { ld L,set 3,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $DE:
      begin { set 3,(IX+d) }
        tempb := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $DF:
      begin { ld A,set 3,(IX+d) }
        r.a := spec_getbyte(temp2) or 8;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $E0:
      begin { ld B,set 4,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $E1:
      begin { ld C,set 4,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $E2:
      begin { ld D,set 4,(IX+d) }
        r.de.h := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $E3:
      begin { ld E,set 4,(IX+d) }
        r.de.l := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $E4:
      begin { ld H,set 4,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $E5:
      begin { ld L,set 4,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $E6:
      begin { set 4,(IX+d) }
        tempb := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $E7:
      begin { ld A,set 4,(IX+d) }
        r.a := spec_getbyte(temp2) or $10;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $E8:
      begin { ld B,set 5,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $E9:
      begin { ld C,set 5,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $EA:
      begin { ld D,set 5,(IX+d) }
        r.de.h := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $EB:
      begin { ld E,set 5,(IX+d) }
        r.de.l := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $EC:
      begin { ld H,set 5,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $ED:
      begin { ld L,set 5,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $EE:
      begin { set 5,(IX+d) }
        tempb := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $EF:
      begin { ld A,set 5,(IX+d) }
        r.a := spec_getbyte(temp2) or $20;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $F0:
      begin { ld B,set 6,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $F1:
      begin { ld C,set 6,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $F2:
      begin { ld D,set 6,(IX+d) }
        r.de.h := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $F3:
      begin { ld E,set 6,(IX+d) }
        r.de.l := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $F4:
      begin { ld H,set 6,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $F5:
      begin { ld L,set 6,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $F6:
      begin { set 6,(IX+d) }
        tempb := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $F7:
      begin { ld A,set 6,(IX+d) }
        r.a := spec_getbyte(temp2) or $40;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
    $F8:
      begin { ld B,set 7,(IX+d) }
        r.bc.h := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.h);
      end;
    $F9:
      begin { ld C,set 7,(IX+d) }
        r.bc.l := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.bc.l);
      end;
    $FA:
      begin { ld D,set 7,(IX+d) }
        r.de.h := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.h);
      end;
    $FB:
      begin { ld E,set 7,(IX+d) }
        r.de.l := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.de.l);
      end;
    $FC:
      begin { ld H,set 7,(IX+d) }
        r.hl.h := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.h);
      end;
    $FD:
      begin { ld L,set 7,(IX+d) }
        r.hl.l := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.hl.l);
      end;
    $FE:
      begin { set 7,(IX+d) }
        tempb := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, tempb);
      end;
    $FF:
      begin { ld A,set 7,(IX+d) }
        r.a := spec_getbyte(temp2) or $80;
        self.retraso(temp2);
        inc(self.contador);
        spec_putbyte(temp2, r.a);
      end;
  end;
end;

procedure cpu_z80_sp.exec_ed_sp;
var
  instruccion, temp, temp2, temp3: byte;
  tempw: word;
  posicion: parejas;
begin
  self.retraso(r.pc);
  self.contador := self.contador + 4;
  instruccion := self.getbyte(r.pc);
  r.pc := r.pc + 1;
  r.r := ((r.r + 1) and $7F) or (r.r and $80);
  case instruccion of
    $40:
      begin { in B,(c) >12t< }
        r.bc.h := spec_inbyte(r.bc.w);
        r.f.z := (r.bc.h = 0);
        r.f.s := (r.bc.h And $80) <> 0;
        r.f.bit3 := (r.bc.h And 8) <> 0;
        r.f.bit5 := (r.bc.h And $20) <> 0;
        r.f.p_v := paridad[r.bc.h];
        r.f.n := false;
        r.f.h := false;
      end;
    $41:
      spec_outbyte(r.bc.w, r.bc.h); { out (C),B >12t< }
    $42:
      begin // sbc HL,BC >15t<
        self.contador := self.contador + 7;
        r.hl.w := sbc_hl(r.bc.w);
      end;
    $43:
      begin { ld (nn),BC >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        spec_putbyte(posicion.w, r.bc.l);
        spec_putbyte(posicion.w + 1, r.bc.h);
        r.wz := posicion.w + 1;
      end;
    $44, $4C, $54, $5C, $64, $6C, $74, $7C:
      begin { neg >8t< }
        temp := r.a;
        r.a := 0;
        sub_8(temp);
      end;
    $45, $55, $65, $75:
      begin { retn >12t< }
        self.retraso(r.sp);
        self.contador := self.contador + 3;
        self.retraso(r.sp + 1);
        self.contador := self.contador + 3;
        r.pc := pop_sp;
        r.wz := r.pc;
        r.iff1 := r.IFF2;
      end;
    $46, $4E, $66, $6E:
      r.im := 0; { im 0 >8t< }
    $47:
      begin { ld I,A >9t< }
        self.contador := self.contador + 1;
        r.i := r.a;
      end;
    $48:
      begin { in C,(C) >12t< }
        r.bc.l := spec_inbyte(r.bc.w);
        r.f.z := (r.bc.l = 0);
        r.f.s := (r.bc.l And $80) <> 0;
        r.f.bit3 := (r.bc.l And 8) <> 0;
        r.f.bit5 := (r.bc.l And $20) <> 0;
        r.f.p_v := paridad[r.bc.l];
        r.f.n := false;
        r.f.h := false;
      end;
    $49:
      spec_outbyte(r.bc.w, r.bc.l); { out (C),C >12t< }
    $4A:
      begin // adc HL,BC >15t<
        self.contador := self.contador + 7;
        r.hl.w := adc_hl(r.bc.w);
      end;
    $4B:
      begin { ld BC,(nn) >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        r.bc.l := spec_getbyte(posicion.w);
        r.bc.h := spec_getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    // 4c: neg
    $4D, $5D, $6D, $7D:
      begin { reti }
        r.iff1 := r.IFF2;
        self.retraso(r.sp);
        self.contador := self.contador + 3;
        self.retraso(r.sp + 1);
        self.contador := self.contador + 3;
        r.pc := pop_sp;
        r.wz := r.pc;
        if self.daisy then
          z80daisy_reti;
      end;
    // 4e: im 0
    $4F:
      begin { ld R,A >9t< }
        self.contador := self.contador + 1;
        r.r := r.a;
      end;
    $50:
      begin { in D,(c) >12t< }
        r.de.h := spec_inbyte(r.bc.w);
        r.f.z := (r.de.h = 0);
        r.f.s := (r.de.h And $80) <> 0;
        r.f.bit3 := (r.de.h And 8) <> 0;
        r.f.bit5 := (r.de.h And $20) <> 0;
        r.f.p_v := paridad[r.de.h];
        r.f.n := false;
        r.f.h := false;
      end;
    $51:
      spec_outbyte(r.bc.w, r.de.h); { out (C),D >12t< }
    $52:
      begin // sbc HL,DE >15t<
        self.contador := self.contador + 7;
        r.hl.w := sbc_hl(r.de.w);
      end;
    $53:
      begin { ld (nn),DE >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        spec_putbyte(posicion.w, r.de.l);
        spec_putbyte(posicion.w + 1, r.de.h);
        r.wz := posicion.w + 1;
      end;
    { 54: neg
      $55:retn }
    $56, $76:
      r.im := 1; { im 1 >8t< }
    $57:
      begin { ld A,I >9t< }
        self.contador := self.contador + 1;
        r.a := r.i;
        r.f.s := (r.a and $80) <> 0;
        r.f.z := (r.a = 0);
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.h := false;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.p_v := r.IFF2;
        r.f.n := false;
      end;
    $58:
      begin { in E,(C) >12t< }
        r.de.l := spec_inbyte(r.bc.w);
        r.f.z := (r.de.l = 0);
        r.f.s := (r.de.l And $80) <> 0;
        r.f.bit3 := (r.de.l And 8) <> 0;
        r.f.bit5 := (r.de.l And $20) <> 0;
        r.f.p_v := paridad[r.de.l];
        r.f.n := false;
        r.f.h := false;
      end;
    $59:
      spec_outbyte(r.bc.w, r.de.l); { out (C),E >12t< }
    $5A:
      begin // adc HL,DE >15t<
        self.contador := self.contador + 7;
        r.hl.w := adc_hl(r.de.w);
      end;
    $5B:
      begin { ld DE,(nn) >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        r.de.l := spec_getbyte(posicion.w);
        r.de.h := spec_getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    { 5c:neg
      5d:retn }
    $5E, $7E:
      r.im := 2; { im 2 >8t< }
    $5F:
      begin { ld A,R >9t< }
        self.contador := self.contador + 1;
        r.a := r.r;
        r.f.h := false;
        r.f.n := false;
        r.f.p_v := r.IFF2;
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.s := (r.a and $80) <> 0;
        r.f.z := (r.a = 0);
      end;
    $60:
      begin { in H,(c) >12t< }
        r.hl.h := spec_inbyte(r.bc.w);
        r.f.z := (r.hl.h = 0);
        r.f.s := (r.hl.h And $80) <> 0;
        r.f.bit3 := (r.hl.h And 8) <> 0;
        r.f.bit5 := (r.hl.h And $20) <> 0;
        r.f.p_v := paridad[r.hl.h];
        r.f.n := false;
        r.f.h := false;
      end;
    $61:
      spec_outbyte(r.bc.w, r.hl.h); { out (C),H >12t< }
    $62:
      begin // sbc HL,HL >15t<
        self.contador := self.contador + 7;
        r.hl.w := sbc_hl(r.hl.w);
      end;
    $63:
      begin { ld (nn),HL >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        spec_putbyte(posicion.w, r.hl.l);
        spec_putbyte(posicion.w + 1, r.hl.h);
        r.wz := posicion.w + 1;
      end;
    { 64:neg
      $65:retn
      $66:im 0 }
    $67:
      begin { rrd >18t< }
        temp2 := spec_getbyte(r.hl.w);
        r.wz := r.hl.w + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        temp := (r.a and $F) * 16;
        r.a := (r.a and $F0) + (temp2 and $F);
        temp2 := (temp2 div 16) + temp;
        spec_putbyte(r.hl.w, temp2);
        r.f.s := (r.a and $80) <> 0;
        r.f.z := (r.a = 0);
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.h := false;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.p_v := paridad[r.a];
        r.f.n := false;
      end;
    $68:
      begin { in L,(c) >12t< }
        r.hl.l := spec_inbyte(r.bc.w);
        r.f.z := (r.hl.l = 0);
        r.f.s := (r.hl.l And $80) <> 0;
        r.f.bit3 := (r.hl.l And 8) <> 0;
        r.f.bit5 := (r.hl.l And $20) <> 0;
        r.f.p_v := paridad[r.hl.l];
        r.f.n := false;
        r.f.h := false;
      end;
    $69:
      spec_outbyte(r.bc.w, r.hl.l); { out (C),L >12t< }
    $6A:
      begin // adc HL,HL >15t<
        self.contador := self.contador + 7;
        r.hl.w := adc_hl(r.hl.w);
      end;
    $6B:
      begin { ld HL,(nn) >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        r.hl.l := spec_getbyte(posicion.w);
        r.hl.h := spec_getbyte(posicion.w + 1);
        r.wz := posicion.w + 1;
      end;
    { 6c:neg
      $6d:retn
      $6e:im 0 }
    $6F:
      begin { rld >18t< }
        temp2 := spec_getbyte(r.hl.w);
        r.wz := r.hl.w + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        temp := r.a and $0F;
        r.a := (r.a and $F0) + (temp2 div 16);
        temp2 := (temp2 * 16) + temp;
        spec_putbyte(r.hl.w, temp2);
        r.f.s := (r.a and $80) <> 0;
        r.f.z := (r.a = 0);
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.h := false;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.p_v := paridad[r.a];
        r.f.n := false;
      end;
    $70:
      begin { in (C) >12t< }
        temp := spec_inbyte(r.bc.w);
        r.f.z := (temp = 0);
        r.f.s := (temp And $80) <> 0;
        r.f.bit3 := (temp And 8) <> 0;
        r.f.bit5 := (temp And $20) <> 0;
        r.f.p_v := paridad[temp];
        r.f.n := false;
        r.f.h := false;
      end;
    $71:
      spec_outbyte(r.bc.w, 0); { out (C),0 >12t< }
    $72:
      begin // sbc HL,SP >15t<
        self.contador := self.contador + 7;
        r.hl.w := sbc_hl(r.sp);
      end;
    $73:
      begin { ld (nn),SP >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        spec_putbyte(posicion.w, r.sp and $FF);
        spec_putbyte(posicion.w + 1, r.sp shr 8);
        r.wz := posicion.w + 1;
      end;
    { 74:neg
      $75:retn
      $76:im 1
      $77:nop*2 }
    $78:
      begin { in A,(C) >12t< }
        r.a := spec_inbyte(r.bc.w);
        r.f.z := (r.a = 0);
        r.f.s := (r.a and $80) <> 0;
        r.f.bit3 := (r.a and 8) <> 0;
        r.f.bit5 := (r.a and $20) <> 0;
        r.f.p_v := paridad[r.a];
        r.f.n := false;
        r.f.h := false;
        r.wz := r.bc.w + 1;
      end;
    $79:
      begin { out (C),A >12t< }
        spec_outbyte(r.bc.w, r.a);
        r.wz := r.bc.w + 1;
      end;
    $7A:
      begin // adc HL,SP >15t<
        self.contador := self.contador + 7;
        r.hl.w := adc_hl(r.sp);
      end;
    $7B:
      begin { ld SP,(nn) >20t< }
        posicion.l := spec_getbyte(r.pc);
        posicion.h := spec_getbyte(r.pc + 1);
        r.pc := r.pc + 2;
        r.sp := spec_getbyte(posicion.w) + (spec_getbyte(posicion.w + 1) shl 8);
        r.wz := posicion.w + 1;
      end;
    { 7c:neg
      $7d:retn
      $7e:im 2
      $7f..9c:nop*2 }
    $A0:
      begin { ldi >16t< }
        temp := spec_getbyte(r.hl.w);
        r.bc.w := r.bc.w - 1;
        spec_putbyte(r.de.w, temp);
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        r.de.w := r.de.w + 1;
        r.hl.w := r.hl.w + 1;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := false;
        r.f.h := false;
        temp := temp + r.a;
        r.f.bit5 := (temp and 2) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
      end;
    $A1:
      begin // cpi el primer programa que lo usa una demo!!!
        // 08 de feb 2003 >16t<
        temp2 := spec_getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
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
        self.contador := self.contador + 1;
        temp := spec_inbyte(r.bc.w);
        spec_putbyte(r.hl.w, temp);
        r.wz := r.bc.w + 1;
        r.bc.h := r.bc.h - 1;
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
        // 08 de feb 2003 >16t<
        self.contador := self.contador + 1;
        temp := spec_getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w + 1;
        spec_outbyte(r.bc.w, temp);
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
    // $a4..$a7:nop*2
    $A8:
      begin { ldd >16t< }
        temp := spec_getbyte(r.hl.w);
        r.bc.w := r.bc.w - 1;
        spec_putbyte(r.de.w, temp);
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        r.de.w := r.de.w - 1;
        r.hl.w := r.hl.w - 1;
        r.f.p_v := (r.bc.w <> 0);
        temp := temp + r.a;
        r.f.bit5 := (temp and 2) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
        r.f.n := false;
        r.f.h := false;
      end;
    $A9:
      begin // cpd el primer juego que la usa Ace 2
        // 20-09-04 >16t<
        temp2 := spec_getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz - 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
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
      begin // ind  >16t<
        self.contador := self.contador + 1;
        temp := spec_inbyte(r.bc.w);
        spec_putbyte(r.hl.w, temp);
        r.wz := r.bc.w - 1;
        r.bc.h := r.bc.h - 1;
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
      begin { outd >16t< }
        self.contador := self.contador + 1;
        temp := spec_getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w - 1;
        spec_outbyte(r.bc.w, temp);
        r.hl.w := r.hl.w - 1;
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
      begin { ldir >16t o 21t< }
        temp := spec_getbyte(r.hl.w);
        spec_putbyte(r.de.w, temp);
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        r.bc.w := r.bc.w - 1;
        r.hl.w := r.hl.w + 1;
        r.de.w := r.de.w + 1;
        if (r.bc.w <> 0) then
        begin
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := false;
        r.f.h := false;
        temp := temp + r.a;
        r.f.bit5 := (temp and 2) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
      end;
    $B1:
      begin { cpir >16t o 21t< }
        temp2 := spec_getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        r.bc.w := r.bc.w - 1;
        r.f.s := (temp and $80) <> 0;
        r.f.z := (temp = 0);
        r.f.h := (temp3 and 16) <> 0;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := True;
        r.f.bit5 := ((temp - ((temp3 and 16) shr 4)) and 2) <> 0;
        r.f.bit3 := ((temp - ((temp3 shr 4) and 1)) and 8) <> 0;
        r.hl.w := r.hl.w + 1;
        If (r.f.p_v And not(r.f.z)) then
        begin
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
      end;
    $B2:
      begin // inir
        self.contador := self.contador + 1;
        temp := spec_inbyte(r.bc.w);
        spec_putbyte(r.hl.w, temp);
        r.wz := r.bc.w + 1;
        r.bc.h := r.bc.h - 1;
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.bc.l + 1;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
        r.hl.w := r.hl.w + 1;
        if r.bc.h <> 0 then
        begin
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
        end;
      end;
    $B3:
      begin // otir añadido el dia 18-09-04 >16t o 21t<
        self.contador := self.contador + 1;
        temp := spec_getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w + 1;
        spec_outbyte(r.bc.w, temp);
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.hl.l;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.hl.w := r.hl.w + 1;
        if r.bc.h <> 0 then
        begin
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
        end;
      end;
    { $b4..$b7:nop*2 }
    $B8:
      begin { lddr >16t o 21t< }
        temp := spec_getbyte(r.hl.w);
        spec_putbyte(r.de.w, temp);
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        self.retraso(r.de.w);
        self.contador := self.contador + 1;
        r.bc.w := r.bc.w - 1;
        r.hl.w := r.hl.w - 1;
        r.de.w := r.de.w - 1;
        if (r.bc.w <> 0) then
        begin
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          self.retraso(r.de.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
        r.f.p_v := (r.bc.w <> 0);
        temp := temp + r.a;
        r.f.bit5 := (temp and 2) <> 0;
        r.f.bit3 := (temp and 8) <> 0;
        r.f.n := false;
        r.f.h := false;
      end;
    $B9:
      begin { cpdr >16t o 21t< }
        temp2 := spec_getbyte(r.hl.w);
        temp := r.a - temp2;
        temp3 := r.a xor temp2 xor temp;
        r.wz := r.wz - 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        self.retraso(r.hl.w);
        self.contador := self.contador + 1;
        r.bc.w := r.bc.w - 1;
        r.f.s := (temp and $80) <> 0;
        r.f.z := (temp = 0);
        r.f.h := (temp3 and 16) <> 0;
        r.f.p_v := (r.bc.w <> 0);
        r.f.n := True;
        r.f.bit5 := ((temp - ((temp3 and 16) shr 4)) and 2) <> 0;
        r.f.bit3 := ((temp - ((temp3 shr 4) and 1)) and 8) <> 0;
        r.hl.w := r.hl.w - 1;
        if r.f.p_v and not(r.f.z) then
        begin
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
          r.wz := r.pc + 1;
        end;
      end;
    $BA:
      begin // indr  >16t<
        self.contador := self.contador + 1;
        temp := spec_inbyte(r.bc.w);
        spec_putbyte(r.hl.w, temp);
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
        r.hl.w := r.hl.w - 1;
        if (r.bc.h <> 0) then
        begin
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
        end;
      end;
    $BB:
      begin // otdr
        self.contador := self.contador + 1;
        temp := spec_getbyte(r.hl.w);
        r.bc.h := r.bc.h - 1;
        r.wz := r.bc.w - 1;
        spec_outbyte(r.bc.w, temp);
        r.f.n := (temp and $80) <> 0;
        tempw := temp + r.hl.l;
        r.f.h := (tempw and $100) <> 0;
        r.f.c := (tempw and $100) <> 0;
        r.f.p_v := paridad[(tempw and $7) xor r.bc.h];
        r.f.z := (r.bc.h = 0);
        r.f.bit5 := (r.bc.h and $20) <> 0;
        r.f.bit3 := (r.bc.h and 8) <> 0;
        r.f.s := (r.bc.h and $80) <> 0;
        r.hl.w := r.hl.w - 1;
        if (r.bc.h <> 0) then
        begin
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          self.retraso(r.hl.w);
          self.contador := self.contador + 1;
          r.pc := r.pc - 2;
        end;
      end;
    $FB:
      main_vars.mainmessage := 'Instruction not implemented EDFB';
  end;
end;

end.
