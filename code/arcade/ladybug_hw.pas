unit ladybug_hw;

interface

uses
  WinApi.Windows,
  nz80,
  main_engine,
  sn_76496,
  controls_engine,
  gfx_engine,
  rom_engine,
  pal_engine,
  sound_engine,
  misc_functions;

function start_ladybug: boolean;

implementation

const
  // Lady Bug
  ladybug_rom: array [0 .. 5] of tipo_roms = ((n: 'l1.c4'; l: $1000; p: 0; crc: $D09E0ADB),
    (n: 'l2.d4'; l: $1000; p: $1000; crc: $88BC4A0A), (n: 'l3.e4'; l: $1000; p: $2000;
    crc: $53E9EFCE), (n: 'l4.h4'; l: $1000; p: $3000; crc: $FFC424D7), (n: 'l5.j4'; l: $1000;
    p: $4000; crc: $AD6AF809), (n: 'l6.k4'; l: $1000; p: $5000; crc: $CF1ACCA4));
  ladybug_pal: array [0 .. 1] of tipo_roms = ((n: '10-2.k1'; l: $20; p: 0; crc: $DF091E52),
    (n: '10-1.f4'; l: $20; p: $20; crc: $40640D8F));
  ladybug_char: array [0 .. 1] of tipo_roms = ((n: 'l9.f7'; l: $1000; p: 0; crc: $77B1DA1E),
    (n: 'l0.h7'; l: $1000; p: $1000; crc: $AA82E00B));
  ladybug_sprites: array [0 .. 1] of tipo_roms = ((n: 'l8.l7'; l: $1000; p: 0; crc: $8B99910B),
    (n: 'l7.m7'; l: $1000; p: $1000; crc: $86A5B448));
  // Snap Jack
  snapjack_rom: array [0 .. 5] of tipo_roms = ((n: 'sj1.c4'; l: $1000; p: 0; crc: $6B30FCDA),
    (n: 'sj2.d4'; l: $1000; p: $1000; crc: $1F1088D1), (n: 'sj3.e4'; l: $1000; p: $2000;
    crc: $EDD65F3A), (n: 'sj4.h4'; l: $1000; p: $3000; crc: $F4481192), (n: 'sj5.j4'; l: $1000;
    p: $4000; crc: $1BFF7D05), (n: 'sj6.k4'; l: $1000; p: $5000; crc: $21793EDF));
  snapjack_pal: array [0 .. 1] of tipo_roms = ((n: '10-2.k1'; l: $20; p: 0; crc: $CBBD9DD1),
    (n: '10-1.f4'; l: $20; p: $20; crc: $5B16FBD2));
  snapjack_char: array [0 .. 1] of tipo_roms = ((n: 'sj9.f7'; l: $1000; p: 0; crc: $FF2011C7),
    (n: 'sj0.h7'; l: $1000; p: $1000; crc: $F097BABB));
  snapjack_sprites: array [0 .. 1] of tipo_roms = ((n: 'sj8.l7'; l: $1000; p: 0; crc: $B7F105B6),
    (n: 'sj7.m7'; l: $1000; p: $1000; crc: $1CDB03A8));
  // Cosmic Avenger
  cavenger_rom: array [0 .. 5] of tipo_roms = ((n: '1.c4'; l: $1000; p: 0; crc: $9E0CC781),
    (n: '2.d4'; l: $1000; p: $1000; crc: $5CE5B950), (n: '3.e4'; l: $1000; p: $2000;
    crc: $BC28218D), (n: '4.h4'; l: $1000; p: $3000; crc: $2B32E9F5), (n: '5.j4'; l: $1000;
    p: $4000; crc: $D117153E), (n: '6.k4'; l: $1000; p: $5000; crc: $C7D366CB));
  cavenger_pal: array [0 .. 1] of tipo_roms = ((n: '10-2.k1'; l: $20; p: 0; crc: $42A24DD5),
    (n: '10-1.f4'; l: $20; p: $20; crc: $D736B8DE));
  cavenger_char: array [0 .. 1] of tipo_roms = ((n: '9.f7'; l: $1000; p: 0; crc: $63357785),
    (n: '0.h7'; l: $1000; p: $1000; crc: $52AD1133));
  cavenger_sprites: array [0 .. 1] of tipo_roms = ((n: '8.l7'; l: $1000; p: 0; crc: $B022BF2D),
    (n: '8.l7'; l: $1000; p: $1000; crc: $B022BF2D));
  // Dip
  ladybug_dip_a: array [0 .. 7] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Medium'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'High Score Names'; number: 2;
    dip: ((dip_val: $0; dip_name: '3 Letters'), (dip_val: $4; dip_name: '10 Letters'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Rack Test'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), (mask: $10; name: 'Freeze'; number: 2;
    dip: ((dip_val: $10; dip_name: 'Off'), (dip_val: $0; dip_name: 'On'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $20; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $20; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $40; name: 'Free Play'; number: 2;
    dip: ((dip_val: $40; dip_name: 'No'), (dip_val: $0; dip_name: 'Yes'), (), (), (), (), (), (),
    (), (), (), (), (), (), (), ())), (mask: $80; name: 'Lives'; number: 2;
    dip: ((dip_val: $80; dip_name: '3'), (dip_val: $0; dip_name: '5'), (), (), (), (), (), (), (),
    (), (), (), (), (), (), ())), ());
  ladybug_dip_b: array [0 .. 2] of def_dip = ((mask: $F0; name: 'Coin A'; number: 10;
    dip: ((dip_val: $60; dip_name: '4C 1C'), (dip_val: $80; dip_name: '3C 1C'), (dip_val: $A0;
    dip_name: '2C 1C'), (dip_val: $70; dip_name: '3C 2C'), (dip_val: $F0;
    dip_name: '1C 1C'), (dip_val: $90; dip_name: '2C 3C'), (dip_val: $E0;
    dip_name: '1C 2C'), (dip_val: $D0; dip_name: '1C 3C'), (dip_val: $C0;
    dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (), (), (), (), (), ())), (mask: $0F;
    name: 'Coin B'; number: 10; dip: ((dip_val: $06; dip_name: '4C 1C'), (dip_val: $08;
    dip_name: '3C 1C'), (dip_val: $0A; dip_name: '2C 1C'), (dip_val: $07;
    dip_name: '3C 2C'), (dip_val: $0F; dip_name: '1C 1C'), (dip_val: $09;
    dip_name: '2C 3C'), (dip_val: $0E; dip_name: '1C 2C'), (dip_val: $0D;
    dip_name: '1C 3C'), (dip_val: $0C; dip_name: '1C 4C'), (dip_val: $0B; dip_name: '1C 5C'), (),
    (), (), (), (), ())), ());
  snapjack_dip_a: array [0 .. 4] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Medium'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'High Score Names'; number: 2;
    dip: ((dip_val: $0; dip_name: '3 Letters'), (dip_val: $4; dip_name: '10 Letters'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $8; dip_name: 'Upright'), (dip_val: $0; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $80;
    dip_name: '4'), (dip_val: $40; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());
  snapjack_dip_b: array [0 .. 2] of def_dip = ((mask: $F0; name: 'Coin A'; number: 11;
    dip: ((dip_val: $50; dip_name: '4C 1C'), (dip_val: $70; dip_name: '3C 1C'), (dip_val: $A0;
    dip_name: '2C 1C'), (dip_val: $60; dip_name: '3C 2C'), (dip_val: $90;
    dip_name: '2C 2C'), (dip_val: $F0; dip_name: '1C 1C'), (dip_val: $80;
    dip_name: '2C 3C'), (dip_val: $E0; dip_name: '1C 2C'), (dip_val: $D0;
    dip_name: '1C 3C'), (dip_val: $C0; dip_name: '1C 4C'), (dip_val: $B0; dip_name: '1C 5C'), (),
    (), (), (), ())), (mask: $0F; name: 'Coin B'; number: 11;
    dip: ((dip_val: $05; dip_name: '4C 1C'), (dip_val: $07; dip_name: '3C 1C'), (dip_val: $0A;
    dip_name: '2C 1C'), (dip_val: $06; dip_name: '3C 2C'), (dip_val: $09;
    dip_name: '2C 2C'), (dip_val: $0F; dip_name: '1C 1C'), (dip_val: $08;
    dip_name: '2C 3C'), (dip_val: $0E; dip_name: '1C 2C'), (dip_val: $0D;
    dip_name: '1C 3C'), (dip_val: $0C; dip_name: '1C 4C'), (dip_val: $0B; dip_name: '1C 5C'), (),
    (), (), (), ())), ());
  cavenger_dip_a: array [0 .. 5] of def_dip = ((mask: $3; name: 'Difficulty'; number: 4;
    dip: ((dip_val: $3; dip_name: 'Easy'), (dip_val: $2; dip_name: 'Medium'), (dip_val: $1;
    dip_name: 'Hard'), (dip_val: $0; dip_name: 'Hardest'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $4; name: 'High Score Names'; number: 2;
    dip: ((dip_val: $0; dip_name: '3 Letters'), (dip_val: $4; dip_name: '10 Letters'), (), (), (),
    (), (), (), (), (), (), (), (), (), (), ())), (mask: $8; name: 'Cabinet'; number: 2;
    dip: ((dip_val: $0; dip_name: 'Upright'), (dip_val: $8; dip_name: 'Cocktail'), (), (), (), (),
    (), (), (), (), (), (), (), (), (), ())), (mask: $30; name: 'Initial High Score'; number: 4;
    dip: ((dip_val: $0; dip_name: '0'), (dip_val: $30; dip_name: '5000'), (dip_val: $20;
    dip_name: '8000'), (dip_val: $10; dip_name: '10000'), (), (), (), (), (), (), (), (), (), (),
    (), ())), (mask: $C0; name: 'Lives'; number: 4;
    dip: ((dip_val: $0; dip_name: '2'), (dip_val: $C0; dip_name: '3'), (dip_val: $80;
    dip_name: '4'), (dip_val: $40; dip_name: '5'), (), (), (), (), (), (), (), (), (), (), (),
    ())), ());

procedure update_video_ladybug;
var
  f, h, color, nchar: word;
  x, y, atrib: byte;
  flipx, flipy: boolean;
  i: integer;
  scroll_y: array [0 .. $1F] of word;
begin
  fill_full_screen(1, 0);
  for f := $0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := f div 32;
      y := 31 - (f mod 32);
      atrib := memory[$D400 + f];
      nchar := memory[$D000 + f] + (atrib and $08) shl 5;
      color := (atrib and $7) shl 2;
      put_gfx_trans(x * 8, y * 8, nchar, color, 2, 0);
      gfx[0].buffer[f] := false;
    end;
  end;
  for f := 0 to $1F do
    scroll_y[f] := not(memory[$D000 + (32 * (f and 3) + (f shr 2))]);
  scroll__y_part2(2, 1, 8, @scroll_y);
  for f := $E downto $2 do
  begin
    i := 0;
    h := f * $40;
    while ((i < $40) and (buffer_sprites[h + i] <> 0)) do
      i := i + 4;
    while (i > 0) do
    begin
      i := i - 4;
      atrib := buffer_sprites[h + i];
      if (atrib and $80) <> 0 then
      begin
        color := (buffer_sprites[$2 + (h + i)] and $F) shl 2;
        if main_screen.flip_main_screen then
          y := buffer_sprites[$3 + (h + i)] + 1
        else
          y := 241 - (buffer_sprites[$3 + (h + i)]);
        flipy := (atrib and $20) <> 0;
        flipx := (atrib and $10) <> 0;
        if (atrib and $40) <> 0 then
        begin // 16x16
          nchar := (buffer_sprites[$1 + (h + i)] shr 2) + 4 *
            (buffer_sprites[$2 + (h + i)] and $10);
          x := (h shr 2) - 8 + (atrib and $F);
          if main_screen.flip_main_screen then
          begin
            x := 240 - x;
            flipy := not(flipy);
            flipx := not(flipx);
          end;
          put_gfx_sprite(nchar and $7F, color, flipx, flipy, 1);
          update_gfx_sprite(x, y, 1, 1);
        end
        else
        begin // 8x8 Parece ser que LB no usa los sprites pequeños!!!
          nchar := buffer_sprites[$1 + (h + i)] + 16 * (buffer_sprites[$2 + (h + i)] and $10);
          x := (h shr 2) + (atrib and $F);
          if main_screen.flip_main_screen then
          begin
            x := 240 - x;
            flipy := not(flipy);
            flipx := not(flipx);
          end;
          put_gfx_sprite(nchar and $1FF, color, flipx, flipy, 2);
          update_gfx_sprite(x, y + 8, 1, 2);
        end;
      end;
    end;
  end;
  actualiza_trozo_final(32, 8, 192, 240, 1);
end;

procedure events_ladybug;
begin
  if event.arcade then
  begin
    // P1
    if p_contrls.map_arcade.up[0] then
      marcade.in0 := (marcade.in0 and $F7)
    else
      marcade.in0 := (marcade.in0 or $8);
    if p_contrls.map_arcade.down[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FB)
    else
      marcade.in0 := (marcade.in0 or $4);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    if p_contrls.map_arcade.start[0] then
      marcade.in0 := (marcade.in0 and $DF)
    else
      marcade.in0 := (marcade.in0 or $20);
    if p_contrls.map_arcade.start[1] then
      marcade.in0 := (marcade.in0 and $BF)
    else
      marcade.in0 := (marcade.in0 or $40);
    // MISC
    if p_contrls.map_arcade.but1[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.but1[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    // P2
    if p_contrls.map_arcade.up[1] then
      marcade.in1 := (marcade.in1 and $F7)
    else
      marcade.in1 := (marcade.in1 or $8);
    if p_contrls.map_arcade.down[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FB)
    else
      marcade.in1 := (marcade.in1 or $4);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    // SYS
    if p_contrls.map_arcade.coin[0] then
      z80_0.change_nmi(ASSERT_LINE)
    else
      z80_0.change_nmi(CLEAR_LINE);
    if p_contrls.map_arcade.coin[1] then
      z80_0.change_irq(HOLD_LINE);
  end;
end;

procedure ladybug_loop;
var
  frame: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if EmulationPaused = false then
    begin
      for f := 0 to $FF do
      begin
        z80_0.run(frame);
        frame := frame + z80_0.tframes - z80_0.contador;
        if f = 224 then
        begin
          marcade.in1 := $80 or (marcade.in1 and $3F);
          update_video_ladybug;
          copymemory(@buffer_sprites, @memory[$7000], $400);
        end;
      end;
      events_ladybug;
      video_sync;
      marcade.in1 := $40 or (marcade.in1 and $3F);
    end
    else
      pause_action;
  end;
end;

function ladybug_getbyte(direccion: word): byte;
begin
  case direccion of
    0 .. $6FFF, $D000 .. $D7FF:
      ladybug_getbyte := memory[direccion];
    $9000:
      ladybug_getbyte := marcade.in0;
    $9001:
      ladybug_getbyte := marcade.in1;
    $9002:
      ladybug_getbyte := marcade.dswa;
    $9003:
      ladybug_getbyte := marcade.dswb;
    $E000:
      ladybug_getbyte := marcade.in2;
  end;
end;

procedure ladybug_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $5FFF:
      ;
    $6000 .. $73FF:
      memory[direccion] := valor;
    $A000:
      main_screen.flip_main_screen := (valor and 1) <> 0;
    $B000 .. $BFFF:
      sn_76496_0.Write(valor);
    $C000 .. $CFFF:
      sn_76496_1.Write(valor);
    $D000 .. $D7FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
  end;
end;

procedure ladybug_sound_update;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
end;

// Main
procedure reset_ladybug;
begin
  z80_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  reset_audio;
  marcade.in0 := $FF;
  marcade.in1 := $7F;
  marcade.in2 := $FF;
end;

function start_ladybug: boolean;
var
  colores: tpaleta;
  f: word;
  bit0, bit1: byte;
  memory_temp: array [0 .. $1FFF] of byte;
  rweights, gweights, bweights: array [0 .. 1] of single;
const
  ps_x: array [0 .. 15] of dword = (0, 2, 4, 6, 8, 10, 12, 14, 8 * 16 + 0, 8 * 16 + 2, 8 * 16 + 4,
    8 * 16 + 6, 8 * 16 + 8, 8 * 16 + 10, 8 * 16 + 12, 8 * 16 + 14);
  ps_y: array [0 .. 15] of dword = (23 * 16, 22 * 16, 21 * 16, 20 * 16, 19 * 16, 18 * 16, 17 * 16,
    16 * 16, 7 * 16, 6 * 16, 5 * 16, 4 * 16, 3 * 16, 2 * 16, 1 * 16, 0 * 16);
  pc_x: array [0 .. 7] of dword = (7, 6, 5, 4, 3, 2, 1, 0);
  pc_y: array [0 .. 7] of dword = (0 * 8, 1 * 8, 2 * 8, 3 * 8, 4 * 8, 5 * 8, 6 * 8, 7 * 8);
  pss_y: array [0 .. 7] of dword = (7 * 16, 6 * 16, 5 * 16, 4 * 16, 3 * 16, 2 * 16, 1 * 16, 0 * 16);
  resistances: array [0 .. 1] of integer = (470, 220);
begin
  machine_calls.general_loop := ladybug_loop;
  machine_calls.reset := reset_ladybug;
  start_ladybug := false;
  start_audio(false);
  screen_init(1, 256, 256, false, true);
  screen_init(2, 256, 256, true);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  if main_vars.machine_type <> 34 then
    main_screen.rot90_screen := true;
  start_video(192, 240);
  // Main CPU
  z80_0 := cpu_z80.create(4000000, 256);
  z80_0.change_ram_calls(ladybug_getbyte, ladybug_putbyte);
  z80_0.init_sound(ladybug_sound_update);
  // Audio chips
  sn_76496_0 := sn76496_chip.create(4000000);
  sn_76496_1 := sn76496_chip.create(4000000);
  case main_vars.machine_type of
    34:
      begin // Lady bug
        // cargar roms
        if not(roms_load(@memory, ladybug_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, ladybug_char)) then
          exit;
        init_gfx(0, 8, 8, 512);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 8 * 8, 0, 512 * 8 * 8);
        convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, true);
        // convertir sprites
        if not(roms_load(@memory_temp, ladybug_sprites)) then
          exit;
        init_gfx(1, 16, 16, 128);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(2, 0, 64 * 8, 1, 0);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
        // convetir sprites pequeños
        init_gfx(2, 8, 8, 512);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 1, 0);
        convert_gfx(2, 0, @memory_temp, @ps_x, @pss_y, false, true);
        // DIP
        marcade.dswa := $DF;
        marcade.dswb := $FF;
        marcade.dswa_val := @ladybug_dip_a;
        marcade.dswb_val := @ladybug_dip_b;
        // poner la paleta
        if not(roms_load(@memory_temp, ladybug_pal)) then
          exit;
      end;
    200:
      begin // SnapJack
        // cargar roms
        if not(roms_load(@memory, snapjack_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, snapjack_char)) then
          exit;
        init_gfx(0, 8, 8, 512);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 8 * 8, 0, 512 * 8 * 8);
        convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, true);
        // convertir sprites
        if not(roms_load(@memory_temp, snapjack_sprites)) then
          exit;
        init_gfx(1, 16, 16, 128);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(2, 0, 64 * 8, 1, 0);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
        // convetir sprites pequeños
        init_gfx(2, 8, 8, 512);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 1, 0);
        convert_gfx(2, 0, @memory_temp, @ps_x, @pss_y, false, true);
        // DIP
        marcade.dswa := $C7;
        marcade.dswb := $FF;
        marcade.dswa_val := @snapjack_dip_a;
        marcade.dswb_val := @snapjack_dip_b;
        // poner la paleta
        if not(roms_load(@memory_temp, snapjack_pal)) then
          exit;
      end;
    201:
      begin // Cosmic Avenger
        // cargar roms
        if not(roms_load(@memory, cavenger_rom)) then
          exit;
        // convertir chars
        if not(roms_load(@memory_temp, cavenger_char)) then
          exit;
        init_gfx(0, 8, 8, 512);
        gfx[0].trans[0] := true;
        gfx_set_desc_data(2, 0, 8 * 8, 0, 512 * 8 * 8);
        convert_gfx(0, 0, @memory_temp, @pc_x, @pc_y, false, true);
        // convertir sprites
        if not(roms_load(@memory_temp, cavenger_sprites)) then
          exit;
        init_gfx(1, 16, 16, 128);
        gfx[1].trans[0] := true;
        gfx_set_desc_data(2, 0, 64 * 8, 1, 0);
        convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, false, true);
        // convetir sprites pequeños
        init_gfx(2, 8, 8, 512);
        gfx[2].trans[0] := true;
        gfx_set_desc_data(2, 0, 16 * 8, 1, 0);
        convert_gfx(2, 0, @memory_temp, @ps_x, @pss_y, false, true);
        // DIP
        marcade.dswa := $C7;
        marcade.dswb := $FF;
        marcade.dswa_val := @cavenger_dip_a;
        marcade.dswb_val := @ladybug_dip_b;
        // poner la paleta
        if not(roms_load(@memory_temp, cavenger_pal)) then
          exit;
      end;
  end;
  compute_resistor_weights(0, 255, -1.0, 2, @resistances[0], @rweights, 470, 0, 2, @resistances[0],
    @gweights, 470, 0, 2, @resistances[0], @bweights, 470, 0);
  for f := 0 to $1F do
  begin
    // red component */
    bit0 := (not(memory_temp[f]) shr 0) and $01;
    bit1 := (not(memory_temp[f]) shr 5) and $01;
    colores[f].r := combine_2_weights(@rweights, bit0, bit1);
    // green component */
    bit0 := (not(memory_temp[f]) shr 2) and $01;
    bit1 := (not(memory_temp[f]) shr 6) and $01;
    colores[f].g := combine_2_weights(@gweights, bit0, bit1);
    // blue component */
    bit0 := (not(memory_temp[f]) shr 4) and $01;
    bit1 := (not(memory_temp[f]) shr 7) and $01;
    colores[f].b := combine_2_weights(@bweights, bit0, bit1);
  end;
  set_pal(colores, $20);
  for f := 0 to $1F do
  begin
    gfx[0].colores[f] := ((f shl 3) and $18) or ((f shr 2) and $07);
    gfx[1].colores[f] := BITSWAP8((memory_temp[f + $20] shr 0) and $F, 7, 6, 5, 4, 0, 1, 2, 3);
    gfx[1].colores[f + $20] := BITSWAP8((memory_temp[f + $20] shr 4) and $F, 7, 6, 5, 4, 0,
      1, 2, 3);
    gfx[2].colores[f] := gfx[1].colores[f];
    gfx[2].colores[f + $20] := gfx[1].colores[f + $20];
  end;
  // final
  reset_ladybug;
  start_ladybug := true;
end;

end.
