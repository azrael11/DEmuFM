unit timepilot84_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  sn_76496,
  main_engine,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine;

function start_timepilot84: boolean;

implementation

const
  tp84_rom: array [0 .. 3] of tipo_roms = ((n: '388_f04.7j'; l: $2000; p: $8000; crc: $605F61C7), (n: '388_05.8j'; l: $2000; p: $A000; crc: $4B4629A4), (n: '388_f06.9j'; l: $2000; p: $C000;
    crc: $DBD5333B), (n: '388_07.10j'; l: $2000; p: $E000; crc: $A45237C4));
  tp84_rom2: tipo_roms = (n: '388_f08.10d'; l: $2000; p: $E000; crc: $36462FF1);
  tp84_proms: array [0 .. 4] of tipo_roms = ((n: '388d14.2c'; l: $100; p: 0; crc: $D737EABA), (n: '388d15.2d'; l: $100; p: $100; crc: $2F6A9A2A), (n: '388d16.1e'; l: $100; p: $200; crc: $2E21329B),
    (n: '388d18.1f'; l: $100; p: $300; crc: $61D2D398), (n: '388j17.16c'; l: $100; p: $400; crc: $13C4E198));
  tp84_chars: array [0 .. 1] of tipo_roms = ((n: '388_h02.2j'; l: $2000; p: 0; crc: $05C7508F), (n: '388_d01.1j'; l: $2000; p: $2000; crc: $498D90B7));
  tp84_sprites: array [0 .. 3] of tipo_roms = ((n: '388_e09.12a'; l: $2000; p: $0; crc: $CD682F30), (n: '388_e10.13a'; l: $2000; p: $2000; crc: $888D4BD6), (n: '388_e11.14a'; l: $2000; p: $4000;
    crc: $9A220B39), (n: '388_e12.15a'; l: $2000; p: $6000; crc: $FAC98397));
  tp84_sound: tipo_roms = (n: '388j13.6a'; l: $2000; p: $0; crc: $C44414DA);
  // Dip
  tp84_dip_a: array [0 .. 2] of def_dip = ((mask: $0F; name: 'Coin A'; number: 16; dip: ((dip_val: $2; dip_name: '4C 1C'), (dip_val: $5; dip_name: '3C 1C'), (dip_val: $8;
    dip_name: '2C 1C'), (dip_val: $4; dip_name: '3C 2C'), (dip_val: $1; dip_name: '4C 3C'), (dip_val: $F; dip_name: '1C 1C'), (dip_val: $3; dip_name: '3C 4C'), (dip_val: $7;
    dip_name: '2C 3C'), (dip_val: $E; dip_name: '1C 2C'), (dip_val: $6; dip_name: '2C 5C'), (dip_val: $D; dip_name: '1C 3C'), (dip_val: $C; dip_name: '1C 4C'), (dip_val: $B;
    dip_name: '1C 5C'), (dip_val: $A; dip_name: '1C 6C'), (dip_val: $9; dip_name: '1C 7C'), (dip_val: $0; dip_name: 'Free Play'))), (mask: $F0; name: 'Coin B'; number: 15;
    dip: ((dip_val: $20; dip_name: '4C 1C'), (dip_val: $50; dip_name: '3C 1C'), (dip_val: $80; dip_name: '2C 1C'), (dip_val: $40; dip_name: '3C 2C'), (dip_val: $10; dip_name: '4C 3C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $30; dip_name: '3C 4C'), (dip_val: $70; dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $60; dip_name: '2C 5C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (dip_val: $A0; dip_name: '1C 6C'), (dip_val: $90; dip_name: '1C 7C'), ())), ());
  tp84_dip_b: array [0 .. 5] of def_dip = ((mask: $3; name: 'Lives'; number: 4; dip: ((dip_val: $3; dip_name: '2'), (dip_val: $2; dip_name: '3'), (dip_val: $1; dip_name: '5'), (dip_val: $0;
    dip_name: '7'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $4; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $4; dip_name: 'Cocktail'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $18; name: 'Bonus Life'; number: 4;
    dip: ((dip_val: $18; dip_name: '10K 50K+'), (dip_val: $10; dip_name: '20K 60K+'), (dip_val: $8; dip_name: '30K 70K+'), (dip_val: $0; dip_name: '40K 80K+'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $60; name: 'Difficulty'; number: 4; dip: ((dip_val: $60; dip_name: 'Easy'), (dip_val: $40; dip_name: 'Normal'), (dip_val: $20; dip_name: 'Hard'), (dip_val: $0;
    dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (), (), ())), (mask: $80; name: 'Demo Sounds'; number: 2;
    dip: ((dip_val: $80; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (), (), (), (), (), (), (), ())), ());

var
  irq_enable: boolean;
  tp84_pal_bank, sound_latch, scroll_x, scroll_y, linea: byte;

procedure update_video_tp84;
var
  x, y, f, nchar, color: word;
  atrib: byte;
  procedure draw_sprites;
  var
    f: byte;
    palette_base, color: word;
    x, y, nchar, atrib: byte;
    flip_x, flip_y: boolean;
  begin
    palette_base := (tp84_pal_bank and $07) shl 4;
    for f := $17 downto 0 do
    begin
      y := mem_misc[$67A0 + (f * 4)];
      x := mem_misc[$67A3 + (f * 4)];
      nchar := mem_misc[$67A1 + (f * 4)];
      atrib := mem_misc[$67A2 + (f * 4)];
      color := (palette_base or (atrib and $0F)) shl 4;
      flip_y := (atrib and $40) = 0;
      flip_x := (atrib and $80) <> 0;
      put_gfx_sprite_mask(nchar, color, flip_x, flip_y, 1, 0, $F);
      update_gfx_sprite(x, y, 3, 1);
    end;
  end;

begin
  for f := $0 to $3FF do
  begin
    x := 31 - (f div 32);
    y := f mod 32;
    // Background
    if gfx[0].buffer[f] then
    begin
      atrib := memory[$4800 + f]; // colorram
      nchar := ((atrib and $30) shl 4) or memory[$4000 + f];
      color := ((tp84_pal_bank and $07) shl 6) or ((tp84_pal_bank and $18) shl 1) or (atrib and $0F);
      put_gfx_flip(x * 8, y * 8, nchar, color shl 2, 1, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
      gfx[0].buffer[f] := false;
    end;
    // Foreground
    if gfx[0].buffer[$400 + f] then
    begin
      atrib := memory[$4C00 + f]; // colorram
      color := ((tp84_pal_bank and $07) shl 6) or ((tp84_pal_bank and $18) shl 1) or (atrib and $0F);
      nchar := ((atrib and $30) shl 4) or memory[$4400 + f];
      if (atrib and $F) <> 0 then
        put_gfx_flip(x * 8, y * 8, nchar, color shl 2, 2, 0, (atrib and $80) <> 0, (atrib and $40) <> 0)
      else
        put_gfx_block_trans(x * 8, y * 8, 2, 8, 8);
      gfx[0].buffer[$400 + f] := false;
    end;
  end;
  scroll_x_y(1, 3, scroll_x, scroll_y);
  draw_sprites;
  update_region(0, 0, 256, 256, 2, 0, 0, 256, 256, 3);
  update_final_piece(16, 0, 224, 256, 3);
end;

procedure events_tp84;
begin
  if event.arcade then
  begin
    // System
    if p_contrls.map_arcade.coin[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or 1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or 2);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // P1
    if p_contrls.map_arcade.left[0] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.up[0] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.down[0] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.but1[0] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    if p_contrls.map_arcade.but0[0] then
      marcade.in1 := (marcade.in1 and $DF)
    else
      marcade.in1 := (marcade.in1 or $20);
    // P2
    if p_contrls.map_arcade.left[1] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.up[1] then
      marcade.in2 := (marcade.in2 and $FB)
    else
      marcade.in2 := (marcade.in2 or $4);
    if p_contrls.map_arcade.down[1] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
    if p_contrls.map_arcade.but0[1] then
      marcade.in2 := (marcade.in2 and $DF)
    else
      marcade.in2 := (marcade.in2 or $20);
  end;
end;

procedure tp84_loop;
var
  frame_m, frame_2, frame_s: single;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_2 := m6809_1.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for linea := 0 to $FF do
      begin
        // Main CPU
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // SubCPU
        m6809_1.run(frame_2);
        frame_2 := frame_2 + m6809_1.tframes - m6809_1.contador;
        // Sound CPU
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if linea = 239 then
        begin
          if irq_enable then
          begin
            m6809_0.change_irq(HOLD_LINE);
            m6809_1.change_irq(HOLD_LINE);
          end;
          update_video_tp84;
        end;
      end;
      events_tp84;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function tp84_getbyte(direccion: word): byte;
begin
  case direccion of
    $2800:
      tp84_getbyte := marcade.in0; // System
    $2820:
      tp84_getbyte := marcade.in1; // p1
    $2840:
      tp84_getbyte := marcade.in2; // p2
    $2860:
      tp84_getbyte := marcade.dswa;
    $3000:
      tp84_getbyte := marcade.dswb;
    $4000 .. $57FF, $8000 .. $FFFF:
      tp84_getbyte := memory[direccion];
  end;
end;

procedure tp84_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $2000:
      ; // wd
    $2800:
      if tp84_pal_bank <> valor then
      begin
        tp84_pal_bank := valor;
        fillchar(gfx[0].buffer[0], $800, 1);
      end;
    $3004, $3005:
      ; // Flip X y flip Y
    $3800:
      z80_0.change_irq(HOLD_LINE);
    $3A00:
      sound_latch := valor;
    $3C00:
      scroll_y := valor;
    $3E00:
      scroll_x := not(valor);
    $4000 .. $43FF, $4800 .. $4BFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $4400 .. $47FF, $4C00 .. $4FFF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[$400 + (direccion and $3FF)] := true;
        memory[direccion] := valor;
      end;
    $5000 .. $57FF:
      memory[direccion] := valor;
    $8000 .. $FFFF:
      ; // ROM
  end;
end;

function cpu2_tp84_getbyte(direccion: word): byte;
begin
  case direccion of
    $2000:
      cpu2_tp84_getbyte := linea;
    $6000 .. $67FF, $E000 .. $FFFF:
      cpu2_tp84_getbyte := mem_misc[direccion];
    $8000 .. $87FF:
      cpu2_tp84_getbyte := memory[$5000 + (direccion and $7FF)];
  end;
end;

procedure cpu2_tp84_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $4000:
      irq_enable := (valor and 1) <> 0;
    $6000 .. $67FF:
      mem_misc[direccion] := valor;
    $8000 .. $87FF:
      memory[$5000 + (direccion and $7FF)] := valor;
    $E000 .. $FFFF:
      ; // ROM
  end;
end;

function sound_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $43FF:
      sound_getbyte := mem_snd[direccion];
    $6000:
      sound_getbyte := sound_latch;
    $8000:
      sound_getbyte := (z80_0.totalt shr 10) and $F;
  end;
end;

procedure sound_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $43FF:
      mem_snd[direccion] := valor;
    $A000 .. $A1FF:
      ; // filtros
    $C001:
      sn_76496_0.Write(valor);
    $C003:
      sn_76496_1.Write(valor);
    $C004:
      sn_76496_2.Write(valor);
  end;
end;

procedure sound_instruccion;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
  sn_76496_2.Update;
end;

// Main
procedure reset_tp84;
begin
  m6809_0.reset;
  m6809_1.reset;
  z80_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  sn_76496_2.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_enable := false;
  tp84_pal_bank := 0;
  sound_latch := 0;
  scroll_x := 0;
  scroll_y := 0;
end;

function start_timepilot84: boolean;
var
  f, i, pos: word;
  colores: tpaleta;
  memory_temp: array [0 .. $7FFF] of byte;
  weights: array [0 .. 3] of single;
  bit0, bit1, bit2, bit3: integer;
const
  ps_x: array [0 .. 15] of dword = (0, 1, 2, 3, 8 * 8 + 0, 8 * 8 + 1, 8 * 8 + 2, 8 * 8 + 3, 16 * 8 + 0, 16 * 8 + 1, 16 * 8 + 2, 16 * 8 + 3, 24 * 8 + 0, 24 * 8 + 1, 24 * 8 + 2, 24 * 8 + 3);
  ps_y: array [0 .. 15] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8, 32 * 8, 33 * 8, 34 * 8, 35 * 8, 36 * 8, 37 * 8, 38 * 8, 39 * 8);
  resistances: array [0 .. 3] of integer = (1000, 470, 220, 100);
begin
  start_timepilot84 := false;
  machine_calls.general_loop := tp84_loop;
  machine_calls.reset := reset_tp84;
  start_audio(false);
  // Pantallas
  screen_init(1, 256, 256);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256, true);
  screen_init(3, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(1536000, 256, TCPU_M6809);
  m6809_0.change_ram_calls(tp84_getbyte, tp84_putbyte);
  // Second CPU
  m6809_1 := cpu_m6809.Create(1536000, 256, TCPU_M6809);
  m6809_1.change_ram_calls(cpu2_tp84_getbyte, cpu2_tp84_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(3579545, $100);
  z80_0.change_ram_calls(sound_getbyte, sound_putbyte);
  z80_0.init_sound(sound_instruccion);
  // Audio chips
  sn_76496_0 := sn76496_chip.Create(1789772);
  sn_76496_1 := sn76496_chip.Create(1789772);
  sn_76496_2 := sn76496_chip.Create(1789772);
  // cargar roms
  if not(roms_load(@memory, tp84_rom)) then
    exit;
  // Cargar roms CPU2
  if not(roms_load(@mem_misc, tp84_rom2)) then
    exit;
  // Cargar roms sound
  if not(roms_load(@mem_snd, tp84_sound)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, tp84_chars)) then
    exit;
  init_gfx(0, 8, 8, $400);
  gfx_set_desc_data(2, 0, 16 * 8, 4, 0);
  convert_gfx(0, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // sprites
  if not(roms_load(@memory_temp, tp84_sprites)) then
    exit;
  init_gfx(1, 16, 16, $100);
  gfx_set_desc_data(4, 0, 64 * 8, 4 + $4000 * 8, 0 + $4000 * 8, 4, 0);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // Colores y lookup
  if not(roms_load(@memory_temp, tp84_proms)) then
    exit;
  compute_resistor_weights(0, 255, -1.0, 4, @resistances, @weights, 470, 0, 0, nil, nil, 0, 0, 0, nil, nil, 0, 0);
  for f := 0 to $FF do
  begin
    // red component */
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    bit3 := (memory_temp[f] shr 3) and $01;
    colores[f].r := combine_4_weights(@weights, bit0, bit1, bit2, bit3);
    // green component */
    bit0 := (memory_temp[f + $100] shr 0) and $01;
    bit1 := (memory_temp[f + $100] shr 1) and $01;
    bit2 := (memory_temp[f + $100] shr 2) and $01;
    bit3 := (memory_temp[f + $100] shr 3) and $01;
    colores[f].g := combine_4_weights(@weights, bit0, bit1, bit2, bit3);
    // blue component */
    bit0 := (memory_temp[f + $200] shr 0) and $01;
    bit1 := (memory_temp[f + $200] shr 1) and $01;
    bit2 := (memory_temp[f + $200] shr 2) and $01;
    bit3 := (memory_temp[f + $200] shr 3) and $01;
    colores[f].b := combine_4_weights(@weights, bit0, bit1, bit2, bit3);
  end;
  set_pal(colores, $100);
  for i := 0 to $1FF do
  begin
    for f := 0 to 7 do
    begin
      bit0 := ((not(i) and $100) shr 1) or (f shl 4) or (memory_temp[i + $300] and $0F);
      pos := ((i and $100) shl 3) or (f shl 8) or (i and $FF);
      if pos > 2047 then
        gfx[1].colores[pos - 2048] := bit0
      else
        gfx[0].colores[pos] := bit0;
    end;
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $32;
  marcade.dswa_val := @tp84_dip_a;
  marcade.dswb_val := @tp84_dip_b;
  // final
  reset_tp84;
  start_timepilot84 := true;
end;

end.
