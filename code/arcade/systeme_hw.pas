unit systeme_hw;

interface

uses
  nz80,
  WinApi.Windows,
  main_engine,
  controls_engine,
  sega_vdp,
  sn_76496,
  rom_engine,
  sound_engine,
  ppi8255,
  mc8123;

function start_systeme: boolean;

implementation

const
  hangonjr_rom: array [0 .. 4] of tipo_roms = ((n: 'epr-7257b.ic7'; l: $8000; p: 0; crc: $D63925A7), (n: 'epr-7258.ic5'; l: $8000; p: $8000; crc: $EE3CAAB3), (n: 'epr-7259.ic4'; l: $8000; p: $10000;
    crc: $D2BA9BC9), (n: 'epr-7260.ic3'; l: $8000; p: $18000; crc: $E14DA070), (n: 'epr-7261.ic2'; l: $8000; p: $20000; crc: $3810CBF5));
  hangonjr_dip_b: array [0 .. 2] of def_dip = ((mask: $6; name: 'Enemies'; number: 4; dip: ((dip_val: $6; dip_name: 'Easy'), (dip_val: $4; dip_name: 'Medium'), (dip_val: $2;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $18; dip_name: 'Easy'), (dip_val: $10; dip_name: 'Medium'), (dip_val: $8; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  slapshtr_rom: array [0 .. 4] of tipo_roms = ((n: 'epr-7351.ic7'; l: $8000; p: 0; crc: $894ADB04), (n: 'epr-7352.ic5'; l: $8000; p: $8000; crc: $61C938B6), (n: 'epr-7353.ic4'; l: $8000; p: $10000;
    crc: $8EE2951A), (n: 'epr-7354.ic3'; l: $8000; p: $18000; crc: $41482AA0), (n: 'epr-7355.ic2'; l: $8000; p: $20000; crc: $C67E1AEF));
  fantzn2_rom: array [0 .. 4] of tipo_roms = ((n: 'epr-11416.ic7'; l: $8000; p: 0; crc: $76DB7B7B), (n: 'epr-11415.ic5'; l: $10000; p: $8000; crc: $57B45681), (n: 'epr-11413.ic3'; l: $10000;
    p: $18000; crc: $A231DC85), (n: 'epr-11414.ic4'; l: $10000; p: $28000; crc: $6F7A9F5F), (n: 'epr-11412.ic2'; l: $10000; p: $38000; crc: $B14DB5AF));
  fantzn2_key: tipo_roms = (n: '317-0057.key'; l: $2000; p: 0; crc: $EE43D0F0);
  fantzn2_dip_b: array [0 .. 4] of def_dip = ((mask: $2; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Timer'; number: 4; dip: ((dip_val: $20; dip_name: '90'), (dip_val: $30; dip_name: '80'), (dip_val: $10;
    dip_name: '70'), (dip_val: $0; dip_name: '60'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $80; dip_name: 'Easy'), (dip_val: $C0; dip_name: 'Normal'), (dip_val: $40; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  // ATENCION: La posicion de las ROMS esta adrede para desencriptarlas bien
  opaopa_rom: array [0 .. 4] of tipo_roms = ((n: 'epr-11054.ic7'; l: $8000; p: 0; crc: $024B1244), (n: 'epr-11053.ic5'; l: $8000; p: $10000; crc: $6BC41D6E), (n: 'epr-11052.ic4'; l: $8000; p: $18000;
    crc: $395C1D0A), (n: 'epr-11051.ic3'; l: $8000; p: $20000; crc: $4CA132A2), (n: 'epr-11050.ic2'; l: $8000; p: $28000; crc: $A165E2EF));
  opaopa_key: tipo_roms = (n: '317-0042.key'; l: $2000; p: 0; crc: $D6312538);
  opaopa_dip_b: array [0 .. 4] of def_dip = ((mask: $2; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4;
    dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $20; dip_name: '25K 45K 70K'), (dip_val: $30; dip_name: '40K 60K 90K'), (dip_val: $10; dip_name: '50K 90K'), (dip_val: $0; dip_name: 'None'), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $C0; name: 'Difficulty'; number: 4; dip: ((dip_val: $80; dip_name: 'Easy'), (dip_val: $C0; dip_name: 'Normal'), (dip_val: $40; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  tetrisse_rom: array [0 .. 2] of tipo_roms = ((n: 'epr-12213.7'; l: $8000; p: 0; crc: $EF3C7A38), (n: 'epr-12212.5'; l: $8000; p: $8000; crc: $28B550BF), (n: 'epr-12211.4'; l: $8000; p: $10000;
    crc: $5AA114E9));
  tetris_dip_b: array [0 .. 2] of def_dip = ((mask: $2; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (),
    (), (), ())), (mask: $30; name: 'Difficulty'; number: 4; dip: ((dip_val: $20; dip_name: 'Easy'), (dip_val: $30; dip_name: 'Normal'), (dip_val: $10; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Hardets'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  transfrm_rom: array [0 .. 4] of tipo_roms = ((n: 'epr-7605.ic7'; l: $8000; p: 0; crc: $CCF1D123), (n: 'epr-7347.ic5'; l: $8000; p: $8000; crc: $DF0F639F), (n: 'epr-7348.ic4'; l: $8000; p: $10000;
    crc: $0F38EA96), (n: 'epr-7606.ic3'; l: $8000; p: $18000; crc: $9D485DF6), (n: 'epr-7350.ic2'; l: $8000; p: $20000; crc: $0052165D));
  transfrm_dip_b: array [0 .. 5] of def_dip = ((mask: $1; name: '1 Player Only'; number: 2; dip: ((dip_val: $0; dip_name: 'Off'), (dip_val: $1; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (),
    (), (), (), ())), (mask: $2; name: 'Demo Sounds'; number: 2; dip: ((dip_val: $2; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())),
    (mask: $C; name: 'Lives'; number: 4; dip: ((dip_val: $C; dip_name: '3'), (dip_val: $8; dip_name: '4'), (dip_val: $4; dip_name: '5'), (dip_val: $0; dip_name: 'Infinite'), (), (), (), (), (), (),
    (), (), (), (), (), ())), (mask: $30; name: 'Bonus Life'; number: 4; dip: ((dip_val: $20; dip_name: '10K 30K 50K 70K'), (dip_val: $30; dip_name: '20K 60K 100K 140K'), (dip_val: $10;
    dip_name: '30K 80K 130K 180K'), (dip_val: $0; dip_name: '50K 150K 250K'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $40; dip_name: 'Easy'), (dip_val: $C0; dip_name: 'Normal'), (dip_val: $80; dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  ridleofp_rom: array [0 .. 4] of tipo_roms = ((n: 'epr-10426.bin'; l: $8000; p: 0; crc: $4404C7E7), (n: 'epr-10425.bin'; l: $8000; p: $8000; crc: $35964109), (n: 'epr-10424.bin'; l: $8000; p: $10000;
    crc: $FCDA1DFA), (n: 'epr-10423.bin'; l: $8000; p: $18000; crc: $0B87244F), (n: 'epr-10422.bin'; l: $8000; p: $20000; crc: $14781E56));
  ridleofp_dip_b: array [0 .. 3] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '3'), (dip_val: $2; dip_name: '4'), (dip_val: $1; dip_name: '5'), (dip_val: $0;
    dip_name: '100'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Ball Speed'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Easy'), (dip_val: $0; dip_name: 'Difficult'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $60; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $60; dip_name: '50K 100K 200K 1M 2M 10M 20M 50M'), (dip_val: $40; dip_name: '100K 200K 1M 2M 10M 20M 50M'), (dip_val: $20; dip_name: '200K 1M 2M 10M 20M 50M'), (dip_val: $0;
    dip_name: 'None'), (), (), (), (), (), (), (), (), (), (), (), ())), ());
  // DIP Generales
  systeme_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: $F; dip: ((dip_val: $07; dip_name: '4C 1C'), (dip_val: $08; dip_name: '3C 1C'), (dip_val: $09;
    dip_name: '2C 1C'), (dip_val: $05; dip_name: '2C1C 5C3C 6C4C'), (dip_val: $04; dip_name: '1C2C 4C3C'), (dip_val: $0F; dip_name: '1C 1C'), (dip_val: $03; dip_name: '1C1C 5C6C'), (dip_val: $02;
    dip_name: '1C1C 4C5C'), (dip_val: $01; dip_name: '1C1C 2C3C'), (dip_val: $06; dip_name: '2C 3C'), (dip_val: $0E; dip_name: '1C 2C'), (dip_val: $0D; dip_name: '1C 3C'), (dip_val: $0C;
    dip_name: '1C 4C'), (dip_val: $0B; dip_name: '1C 5C'), (dip_val: $0A; dip_name: '1C 6C'), (dip_val: $0; dip_name: 'Invalid'))), (mask: $F0; name: 'Coin B'; number: $F;
    dip: ((dip_val: $70; dip_name: '4C 1C'), (dip_val: $88; dip_name: '3C 1C'), (dip_val: $90; dip_name: '2C 1C'), (dip_val: $50; dip_name: '2C1C 5C3C 6C4C'), (dip_val: $40;
    dip_name: '1C2C 4C3C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $30; dip_name: '1C1C 5C6C'), (dip_val: $20; dip_name: '1C1C 4C5C'), (dip_val: $10; dip_name: '1C1C 2C3C'), (dip_val: $60;
    dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $D0; dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $0A;
    dip_name: '1C 6C'), (dip_val: $0; dip_name: 'Invalid'))), ());

var
  memoria_rom, memoria_dec: array [0 .. $F, 0 .. $3FFF] of byte;
  rom_dec: array [0 .. $7FFF] of byte;
  port_select, vdp0_bank, vdp1_bank, vram_write, rom_bank: byte;
  vdp_ram: array [0 .. 1, 0 .. $7FFF] of byte;
  // ridleofp
  last, diff: array [0 .. 1] of word;

procedure events_systeme;
begin
  if event.arcade then
  begin
    // SYS
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);; // HangOn Jr.
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $7F)
    else
      marcade.in0 := (marcade.in0 or $80);
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or 1);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or 2);
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or 4);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or 8);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or 1);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or 2);
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or 4);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or 8);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
  end;
end;

procedure systeme_loop;
var
  frame: single;
  f: word;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to (vdp_0.VIDEO_Y_TOTAL - 1) do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        vdp_0.refresh(f);
        vdp_1.refresh(f);
      end;
      update_region(0, 0, 284, vdp_0.VIDEO_VISIBLE_Y_TOTAL, 1, 0, 0, 284, vdp_0.VIDEO_VISIBLE_Y_TOTAL, 3);
      update_region(0, 0, 284, vdp_0.VIDEO_VISIBLE_Y_TOTAL, 2, 0, 0, 284, vdp_0.VIDEO_VISIBLE_Y_TOTAL, 3);
      update_region(0, 0, 284, vdp_0.VIDEO_VISIBLE_Y_TOTAL, 3, 0, 0, 284, vdp_0.VIDEO_VISIBLE_Y_TOTAL, PANT_TEMP);
      events_systeme;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function systeme_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF, $C000 .. $FFFF:
      systeme_getbyte := memory[direccion];
    $8000 .. $BFFF:
      systeme_getbyte := memoria_rom[rom_bank, direccion and $3FFF]; // ROM bank
  end;
end;

procedure systeme_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $7FFF:
      ; // ROM
    $8000 .. $BFFF:
      case vram_write of
        0, 4:
          vdp_ram[1, $4000 + (direccion and $3FFF)] := valor;
        1, 3:
          vdp_ram[0, $4000 + (direccion and $3FFF)] := valor;
        2, 6:
          vdp_ram[1, direccion and $3FFF] := valor;
        5, 7:
          vdp_ram[0, direccion and $3FFF] := valor;
      end;
    $C000 .. $FFFF:
      memory[direccion] := valor;
  end;
end;

function vdp0_read_mem(direccion: word): byte;
begin
  vdp0_read_mem := vdp_ram[0, (vdp0_bank * $4000) + (direccion and $3FFF)]
end;

function vdp1_read_mem(direccion: word): byte;
begin
  vdp1_read_mem := vdp_ram[1, (vdp1_bank * $4000) + (direccion and $3FFF)]
end;

procedure vdp0_write_mem(direccion: word; valor: byte);
begin
  vdp_ram[0, (vdp0_bank * $4000) + (direccion and $3FFF)] := valor;
end;

procedure vdp1_write_mem(direccion: word; valor: byte);
begin
  vdp_ram[1, (vdp1_bank * $4000) + (direccion and $3FFF)] := valor;
end;

function systeme_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $7E:
      systeme_inbyte := vdp_0.linea_back;
    $BA:
      systeme_inbyte := vdp_0.vram_r;
    $BB:
      systeme_inbyte := vdp_0.register_r;
    $BE:
      systeme_inbyte := vdp_1.vram_r;
    $BF:
      systeme_inbyte := vdp_1.register_r;
    $E0:
      systeme_inbyte := marcade.in0;
    $E1:
      systeme_inbyte := marcade.in1;
    $E2:
      systeme_inbyte := marcade.in2;
    $F2:
      systeme_inbyte := marcade.dswa;
    $F3:
      systeme_inbyte := marcade.dswb;
    $F8 .. $FB:
      systeme_inbyte := pia8255_0.read(puerto and $3);
  end;
end;

procedure systeme_outbyte(puerto: word; valor: byte);
begin
  case (puerto and $FF) of
    $7B:
      sn_76496_0.Write(valor);
    $7E, $7F:
      sn_76496_1.Write(valor);
    $BA:
      vdp_0.vram_w(valor);
    $BB:
      vdp_0.register_w(valor);
    $BE:
      vdp_1.vram_w(valor);
    $BF:
      vdp_1.register_w(valor);
    $F7:
      begin
        vdp0_bank := (valor and $80) shr 7;
        vdp1_bank := (valor and $40) shr 6;
        vram_write := valor shr 5;
        rom_bank := valor and $F;
      end;
    $F8 .. $FB:
      pia8255_0.Write(puerto and $3, valor);
  end;
end;

procedure systeme_interrupt(int: boolean);
begin
  if int then
    z80_0.change_irq(ASSERT_LINE)
  else
    z80_0.change_irq(CLEAR_LINE);
end;

procedure systeme_sound_update;
begin
  sn_76496_0.update;
  sn_76496_1.update;
end;

function ppi8255_systeme_rportc: byte;
begin
  ppi8255_systeme_rportc := 0;
end;

// HangOn Jr.
function ppi8255_hangonjr_rporta: byte;
var
  ret: byte;
begin
  ret := 0;
  if port_select = 8 then
    ret := analog.c[0].x[0]; // in2 move
  if port_select = 9 then
    ret := analog.c[1].val[0]; // in3 pedal
  ppi8255_hangonjr_rporta := ret;
end;

procedure ppi8255_hangonjr_wportc(valor: byte);
begin
  port_select := valor and $F;
end;

// FantZone II
function fantzone2_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      if z80_0.opcode then
        fantzone2_getbyte := rom_dec[direccion]
      else
        fantzone2_getbyte := memory[direccion];
    $8000 .. $BFFF:
      fantzone2_getbyte := memoria_rom[rom_bank, direccion and $3FFF]; // ROM bank
    $C000 .. $FFFF:
      fantzone2_getbyte := memory[direccion];
  end;
end;

// Opa Opa
function opaopa_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $7FFF:
      if z80_0.opcode then
        opaopa_getbyte := rom_dec[direccion]
      else
        opaopa_getbyte := memory[direccion];
    $8000 .. $BFFF:
      if z80_0.opcode then
        opaopa_getbyte := memoria_dec[rom_bank, direccion and $3FFF]
      else
        opaopa_getbyte := memoria_rom[rom_bank, direccion and $3FFF];
    $C000 .. $FFFF:
      opaopa_getbyte := memory[direccion];
  end;
end;

// reidleofp
function ridleofp_inbyte(puerto: word): byte;
begin
  case (puerto and $FF) of
    $7E:
      ridleofp_inbyte := vdp_0.linea_back;
    $BA:
      ridleofp_inbyte := vdp_0.vram_r;
    $BB:
      ridleofp_inbyte := vdp_0.register_r;
    $BE:
      ridleofp_inbyte := vdp_1.vram_r;
    $BF:
      ridleofp_inbyte := vdp_1.register_r;
    $E0:
      ridleofp_inbyte := marcade.in0;
    $E1:
      ridleofp_inbyte := marcade.in1;
    $E2:
      ridleofp_inbyte := marcade.in2;
    $F2:
      ridleofp_inbyte := marcade.dswa;
    $F3:
      ridleofp_inbyte := marcade.dswb;
    $F8:
      case port_select of
        1:
          ridleofp_inbyte := diff[0] shr 8;
        2:
          ridleofp_inbyte := diff[1] and $FF;
        3:
          ridleofp_inbyte := diff[1] shr 8;
      else
        ridleofp_inbyte := diff[0] and $FF;
      end;
    $F9 .. $FB:
      ridleofp_inbyte := pia8255_0.read(puerto and $3);
  end;
end;

procedure ridleofp_outbyte(puerto: word; valor: byte);
var
  curr: word;
begin
  case (puerto and $FF) of
    $7B:
      sn_76496_0.Write(valor);
    $7E, $7F:
      sn_76496_1.Write(valor);
    $BA:
      vdp_0.vram_w(valor);
    $BB:
      vdp_0.register_w(valor);
    $BE:
      vdp_1.vram_w(valor);
    $BF:
      vdp_1.register_w(valor);
    $F7:
      begin
        vdp0_bank := (valor and $80) shr 7;
        vdp1_bank := (valor and $40) shr 6;
        vram_write := valor shr 5;
        rom_bank := valor and $F;
      end;
    $F8, $F9, $FB:
      pia8255_0.Write(puerto and $3, valor);
    $FA:
      begin
        port_select := (valor and $0C) shr 2;
        if (valor and 1) <> 0 then
        begin
          curr := analog.c[0].x[0] or ((marcade.in1 and $10) shl 10);
          diff[0] := ((curr - last[0]) and $0FFF) or (curr and $F000);
          last[0] := curr;
        end;
        if (valor and 2) <> 0 then
        begin
          curr := analog.c[0].x[1] or ((marcade.in2 and $10) shl 10);
          diff[1] := ((curr - last[1]) and $0FFF) or (curr and $F000);
          last[1] := curr;
        end;
      end;
  end;
end;

procedure reset_systeme;
begin
  z80_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  vdp_0.reset;
  vdp_1.reset;
  pia8255_0.reset;
  reset_audio;
  rom_bank := 0;
  vdp0_bank := 0;
  vdp1_bank := 0;
  vram_write := 0;
  port_select := 0;
  diff[0] := 0;
  diff[1] := 0;
  last[0] := 0;
  last[1] := 0;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
end;

function start_systeme: boolean;
var
  mem_temp: array [0 .. $47FFF] of byte;
  mem_temp_dec: array [0 .. $2FFFF] of byte;
  mem_key: array [0 .. $1FFF] of byte;
  f: byte;
begin
  machine_calls.general_loop := systeme_loop;
  machine_calls.reset := reset_systeme;
  machine_calls.fps_max := 59.922738;
  start_systeme := false;
  start_audio(false);
  if main_vars.machine_type = 257 then
    main_screen.rot90_screen := true;
  screen_init(1, 284, 243);
  screen_init(2, 284, 243, true);
  screen_init(3, 284, 243);
  start_video(284, 243);
  // Main CPU
  z80_0 := cpu_z80.create(10738635 div 2, LINES_NTSC);
  z80_0.change_ram_calls(systeme_getbyte, systeme_putbyte);
  z80_0.change_io_calls(systeme_inbyte, systeme_outbyte);
  z80_0.init_sound(systeme_sound_update);
  // VDP
  vdp_0 := vdp_chip.create(1, nil, z80_0.numero_cpu, vdp0_read_mem, vdp0_write_mem);
  vdp_0.video_ntsc(0);
  sn_76496_0 := sn76496_chip.create(10738635 div 2);
  vdp_1 := vdp_chip.create(2, systeme_interrupt, z80_0.numero_cpu, vdp1_read_mem, vdp1_write_mem, true);
  vdp_1.video_ntsc(0);
  sn_76496_1 := sn76496_chip.create(10738635 div 2);
  pia8255_0 := pia8255_chip.create;
  pia8255_0.change_ports(nil, nil, ppi8255_systeme_rportc, nil, nil, nil);
  // DIP
  marcade.dswa := $FF;
  case main_vars.machine_type of
    251:
      begin // HangOn Jr.
        pia8255_0.change_ports(ppi8255_hangonjr_rporta, nil, ppi8255_systeme_rportc, nil, nil, ppi8255_hangonjr_wportc);
        if not(roms_load(@mem_temp, hangonjr_rom)) then
          exit;
        copymemory(@memory, @mem_temp, $8000);
        for f := 0 to 7 do
          copymemory(@memoria_rom[f, 0], @mem_temp[$8000 + (f * $4000)], $4000);
        // Init Analog
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(100, 4, $80, $E0, $20, true, false, true, true);
        analog_1(100, 20, $FF, 0, true);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb_val := @hangonjr_dip_b;
        marcade.dswb := $FF;
      end;
    252:
      begin // Slap Shooter
        if not(roms_load(@mem_temp, slapshtr_rom)) then
          exit;
        copymemory(@memory, @mem_temp, $8000);
        for f := 0 to 7 do
          copymemory(@memoria_rom[f, 0], @mem_temp[$8000 + (f * $4000)], $4000);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb := $FF;
      end;
    253:
      begin // Fantasy Zone II
        z80_0.change_ram_calls(fantzone2_getbyte, systeme_putbyte);
        if not(roms_load(@mem_temp, fantzn2_rom)) then
          exit;
        if not(roms_load(@mem_key, fantzn2_key)) then
          exit;
        copymemory(@memory, @mem_temp, $8000);
        mc8123_decrypt_rom(@mem_key, @memory, @rom_dec, $8000);
        for f := 0 to $F do
          copymemory(@memoria_rom[f, 0], @mem_temp[$8000 + (f * $4000)], $4000);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb_val := @fantzn2_dip_b;
        marcade.dswb := $FD;
      end;
    254:
      begin // Opa Opa
        z80_0.change_ram_calls(opaopa_getbyte, systeme_putbyte);
        if not(roms_load(@mem_key, opaopa_key)) then
          exit;
        if not(roms_load(@mem_temp, opaopa_rom)) then
          exit;
        mc8123_decrypt_rom(@mem_key, @mem_temp, @mem_temp_dec, $30000);
        // Main ROM
        copymemory(@memory, @mem_temp, $8000);
        copymemory(@rom_dec, @mem_temp_dec, $8000);
        // ROM Banks
        for f := 0 to 7 do
          copymemory(@memoria_rom[f, 0], @mem_temp[$10000 + (f * $4000)], $4000);
        for f := 0 to 7 do
          copymemory(@memoria_dec[f, 0], @mem_temp_dec[$10000 + (f * $4000)], $4000);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb_val := @opaopa_dip_b;
        marcade.dswb := $FD;
      end;
    255:
      begin // Tetris
        if not(roms_load(@mem_temp, tetrisse_rom)) then
          exit;
        copymemory(@memory, @mem_temp, $8000);
        for f := 0 to 3 do
          copymemory(@memoria_rom[f, 0], @mem_temp[$8000 + (f * $4000)], $4000);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb_val := @tetris_dip_b;
        marcade.dswb := $FD;
      end;
    256:
      begin // Transformer
        if not(roms_load(@mem_temp, transfrm_rom)) then
          exit;
        copymemory(@memory, @mem_temp, $8000);
        for f := 0 to 7 do
          copymemory(@memoria_rom[f, 0], @mem_temp[$8000 + (f * $4000)], $4000);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb_val := @transfrm_dip_b;
        marcade.dswb := $FC;
      end;
    257:
      begin // Riddle of Pythagoras
        z80_0.change_io_calls(ridleofp_inbyte, ridleofp_outbyte);
        if not(roms_load(@mem_temp, ridleofp_rom)) then
          exit;
        copymemory(@memory, @mem_temp, $8000);
        for f := 0 to 7 do
          copymemory(@memoria_rom[f, 0], @mem_temp[$8000 + (f * $4000)], $4000);
        // Init Analog
        init_analog(z80_0.numero_cpu, z80_0.clock);
        analog_0(60, 35, $3FF, $FFF, 0, false, false, true, true);
        // DIP
        marcade.dswa_val := @systeme_dip_a;
        marcade.dswb_val := @ridleofp_dip_b;
        marcade.dswb := $FE;
      end;
  end;
  // final
  reset_systeme;
  start_systeme := true;
end;

end.
