unit circuscharlie_hw;

interface

uses
  WinApi.Windows,
  m6809,
  nz80,
  main_engine,
  controls_engine,
  sn_76496,
  gfx_engine,
  dac,
  rom_engine,
  pal_engine,
  konami_decrypt,
  sound_engine;

function start_circuscharlie: boolean;

implementation

const
  circusc_rom: array [0 .. 4] of tipo_roms = ((n: '380_s05.3h'; l: $2000; p: $6000; crc: $48FEAFCF), (n: '380_r04.4h'; l: $2000; p: $8000; crc: $C283B887), (n: '380_r03.5h'; l: $2000; p: $A000; crc: $E90C0E86), (n: '380_q02.6h'; l: $2000; p: $C000; crc: $4D847DC6),
    (n: '380_q01.7h'; l: $2000; p: $E000; crc: $18C20ADF));
  circusc_snd: array [0 .. 1] of tipo_roms = ((n: '380_l14.5c'; l: $2000; p: 0; crc: $607DF0FB), (n: '380_l15.7c'; l: $2000; p: $2000; crc: $A6AD30E1));
  circusc_char: array [0 .. 1] of tipo_roms = ((n: '380_j12.4a'; l: $2000; p: 0; crc: $56E5B408), (n: '380_j13.5a'; l: $2000; p: $2000; crc: $5ACA0193));
  circusc_sprites: array [0 .. 5] of tipo_roms = ((n: '380_j06.11e'; l: $2000; p: 0; crc: $DF0405C6), (n: '380_j07.12e'; l: $2000; p: $2000; crc: $23DFE3A6), (n: '380_j08.13e'; l: $2000; p: $4000; crc: $3BA95390), (n: '380_j09.14e'; l: $2000; p: $6000; crc: $A9FBA85A),
    (n: '380_j10.15e'; l: $2000; p: $8000; crc: $0532347E), (n: '380_j11.16e'; l: $2000; p: $A000; crc: $E1725D24));
  circusc_pal: array [0 .. 2] of tipo_roms = ((n: '380_j18.2a'; l: $20; p: 0; crc: $10DD4EAA), (n: '380_j17.7b'; l: $100; p: $20; crc: $13989357), (n: '380_j16.10c'; l: $100; p: $120; crc: $C244F2AA));
  // Dip
  circusc_dip_a: array [0 .. 2] of def_dip2 = ((mask: $F; name: 'Coin A'; number: 16; val16: (2, 5, 8, 4, 1, $F, 3, 7, $E, 6, $D, $C, $B, $A, 9, 0);
    name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), (mask: $F0; name: 'Coin B'; number: 16;
    val16: ($20, $50, $80, $40, $10, $F0, $30, $70, $E0, $60, $D0, $C0, $B0, $A0, $90, 0); name16: ('4C 1C', '3C 1C', '2C 1C', '3C 2C', '4C 3C', '1C 1C', '3C 4C', '2C 3C', '1C 2C', '2C 5C', '1C 3C', '1C 4C', '1C 5C', '1C 6C', '1C 7C', 'Free Play')), ());
  circusc_dip_b: array [0 .. 5] of def_dip2 = ((mask: $3; name: 'Lives'; number: 4; val4: (3, 2, 1, 0); name4: ('3', '4', '5', '7')), (mask: $4; name: 'Cabinet'; number: 2; val2: (0, 4); name2: ('Upright', 'Cocktail')), (mask: $8; name: 'Bonus Life'; number: 2; val2: (8, 0);
    name2: ('20K 90K 70K+', '30K 110K 80K+')), (mask: $60; name: 'Difficulty'; number: 4; val4: ($60, $40, $20, 0); name4: ('Easy', 'Normal', 'Hard', 'Hardest')), (mask: $80; name: 'Demo Sounds'; number: 2; val2: ($80, 0); name2: ('Off', 'On')), ());

var
  irq_ena: boolean;
  mem_opcodes: array [0 .. $9FFF] of byte;
  sound_latch, scroll_x: byte;
  spritebank: word;

procedure update_video_circusc;
var
  x, y, atrib: byte;
  f, nchar, color: word;
begin
  for f := 0 to $3FF do
  begin
    if gfx[0].buffer[f] then
    begin
      x := 31 - (f div 32);
      y := f mod 32;
      atrib := memory[$3000 + f];
      nchar := memory[$3400 + f] + ((atrib and $20) shl 3);
      color := (atrib and $F) shl 4;
      if (atrib and $10) = 0 then
      begin
        put_gfx_flip(x * 8, y * 8, nchar, color, 1, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
        put_gfx_block(x * 8, y * 8, 2, 8, 8, 0);
      end
      else
      begin
        put_gfx_block_trans(x * 8, y * 8, 1, 8, 8);
        put_gfx_flip(x * 8, y * 8, nchar, color, 2, 0, (atrib and $80) <> 0, (atrib and $40) <> 0);
      end;
      gfx[0].buffer[f] := false;
    end;
  end;
  update_region(0, 0, 256, 80, 2, 0, 0, 256, 80, 3);
  scroll__x_part(2, 3, scroll_x, 0, 80, 176);
  // Sprites
  for f := 0 to $3F do
  begin
    atrib := memory[spritebank + 1 + (f * 4)];
    nchar := (memory[spritebank + (f * 4)] + ((atrib and $20) shl 3)) mod 384;
    color := (atrib and $F) shl 4;
    x := 240 - memory[spritebank + 3 + (f * 4)];
    y := memory[spritebank + 2 + (f * 4)];
    put_gfx_sprite_mask(nchar, color, (atrib and $80) <> 0, (atrib and $40) <> 0, 1, 0, $F);
    update_gfx_sprite(x, y, 3, 1);
  end;
  update_region(0, 0, 256, 80, 1, 0, 0, 256, 80, 3);
  scroll__x_part(1, 3, scroll_x, 0, 80, 176);
  update_final_piece(16, 0, 224, 256, 3);
end;

procedure events_circusc;
begin
  if event.arcade then
  begin
    // p1
    if p_contrls.map_arcade.left[0] then
      marcade.in0 := (marcade.in0 and $FE)
    else
      marcade.in0 := (marcade.in0 or $1);
    if p_contrls.map_arcade.right[0] then
      marcade.in0 := (marcade.in0 and $FD)
    else
      marcade.in0 := (marcade.in0 or $2);
    if p_contrls.map_arcade.but0[0] then
      marcade.in0 := (marcade.in0 and $EF)
    else
      marcade.in0 := (marcade.in0 or $10);
    // p2
    if p_contrls.map_arcade.left[1] then
      marcade.in1 := (marcade.in1 and $FE)
    else
      marcade.in1 := (marcade.in1 or $1);
    if p_contrls.map_arcade.right[1] then
      marcade.in1 := (marcade.in1 and $FD)
    else
      marcade.in1 := (marcade.in1 or $2);
    if p_contrls.map_arcade.but0[1] then
      marcade.in1 := (marcade.in1 and $EF)
    else
      marcade.in1 := (marcade.in1 or $10);
    // misc
    if p_contrls.map_arcade.coin[0] then
      marcade.in2 := (marcade.in2 and $FE)
    else
      marcade.in2 := (marcade.in2 or $1);
    if p_contrls.map_arcade.coin[1] then
      marcade.in2 := (marcade.in2 and $FD)
    else
      marcade.in2 := (marcade.in2 or $2);
    if p_contrls.map_arcade.start[0] then
      marcade.in2 := (marcade.in2 and $F7)
    else
      marcade.in2 := (marcade.in2 or $8);
    if p_contrls.map_arcade.start[1] then
      marcade.in2 := (marcade.in2 and $EF)
    else
      marcade.in2 := (marcade.in2 or $10);
  end;
end;

procedure circusc_loop;
var
  frame_m, frame_s: single;
  f: byte;
begin
  init_controls(false, false, false, true);
  frame_m := m6809_0.tframes;
  frame_s := z80_0.tframes;
  while EmuStatus = EsRunning do
  begin
    if machine_calls.pause = false then
    begin
      for f := 0 to $FF do
      begin
        // main
        m6809_0.run(frame_m);
        frame_m := frame_m + m6809_0.tframes - m6809_0.contador;
        // snd
        z80_0.run(frame_s);
        frame_s := frame_s + z80_0.tframes - z80_0.contador;
        if f = 239 then
        begin
          if irq_ena then
            m6809_0.change_irq(HOLD_LINE);
          update_video_circusc;
        end;
      end;
      events_circusc;
      video_sync;
    end
    else
      pause_action;
  end;
end;

function circusc_getbyte(direccion: word): byte;
begin
  case direccion of
    $1000 .. $13FF:
      case (direccion and $3) of
        0:
          circusc_getbyte := marcade.in2; // system
        1:
          circusc_getbyte := marcade.in0; // p1
        2:
          circusc_getbyte := marcade.in1; // p2
        3:
          circusc_getbyte := 0;
      end;
    $1400 .. $17FF:
      circusc_getbyte := marcade.dswa; // dsw1
    $1800 .. $1BFF:
      circusc_getbyte := marcade.dswb; // dsw2
    $2000 .. $3FFF:
      circusc_getbyte := memory[direccion];
    $6000 .. $FFFF:
      if m6809_0.opcode then
        circusc_getbyte := mem_opcodes[direccion - $6000]
      else
        circusc_getbyte := memory[direccion];
  end;
end;

procedure circusc_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    $0 .. $3FF:
      case (direccion and $7) of
        0:
          main_screen.flip_main_screen := (valor and 1) <> 0;
        1:
          irq_ena := (valor <> 0);
        5:
          spritebank := $3800 + ((valor and 1) shl 8);
      end;
    $800 .. $BFF:
      sound_latch := valor;
    $C00 .. $FFF:
      z80_0.change_irq(HOLD_LINE);
    $1C00 .. $1FFF:
      scroll_x := 256 - valor;
    $2000 .. $2FFF, $3800 .. $3FFF:
      memory[direccion] := valor;
    $3000 .. $37FF:
      if memory[direccion] <> valor then
      begin
        gfx[0].buffer[direccion and $3FF] := true;
        memory[direccion] := valor;
      end;
    $6000 .. $FFFF:
      ; // ROM
  end;
end;

function circusc_snd_getbyte(direccion: word): byte;
begin
  case direccion of
    $0 .. $3FFF:
      circusc_snd_getbyte := mem_snd[direccion];
    $4000 .. $5FFF:
      circusc_snd_getbyte := mem_snd[$4000 + (direccion and $3FF)];
    $6000 .. $7FFF:
      circusc_snd_getbyte := sound_latch;
    $8000 .. $9FFF:
      circusc_snd_getbyte := (z80_0.totalt shr 9) and $1E;
  end;
end;

procedure circusc_snd_putbyte(direccion: word; valor: byte);
begin
  case direccion of
    0 .. $3FFF:
      ; // ROM
    $4000 .. $5FFF:
      mem_snd[$4000 + (direccion and $3FF)] := valor;
    $A000 .. $BFFF:
      case (direccion and $7) of
        0:
          sound_latch := valor;
        1:
          sn_76496_0.Write(sound_latch);
        2:
          sn_76496_1.Write(sound_latch);
        3:
          dac_0.data8_w(valor);
      end;
  end;
end;

procedure circusc_sound;
begin
  sn_76496_0.Update;
  sn_76496_1.Update;
  dac_0.Update;
end;

// Main
procedure reset_circusc;
begin
  m6809_0.reset;
  z80_0.reset;
  sn_76496_0.reset;
  sn_76496_1.reset;
  dac_0.reset;
 reset_game_general;
  marcade.in0 := $FF;
  marcade.in1 := $FF;
  marcade.in2 := $FF;
  irq_ena := false;
  sound_latch := 0;
  scroll_x := 0;
  spritebank := $3800;
end;

function start_circuscharlie: boolean;
var
  colores: tpaleta;
  bit0, bit1, bit2: byte;
  f: word;
  memory_temp: array [0 .. $FFFF] of byte;
const
  pc_y: array [0 .. 7] of dword = (0 * 32, 1 * 32, 2 * 32, 3 * 32, 4 * 32, 5 * 32, 6 * 32, 7 * 32);
  ps_x: array [0 .. 15] of dword = (0 * 4, 1 * 4, 2 * 4, 3 * 4, 4 * 4, 5 * 4, 6 * 4, 7 * 4, 8 * 4, 9 * 4, 10 * 4, 11 * 4, 12 * 4, 13 * 4, 14 * 4, 15 * 4);
  ps_y: array [0 .. 15] of dword = (0 * 4 * 16, 1 * 4 * 16, 2 * 4 * 16, 3 * 4 * 16, 4 * 4 * 16, 5 * 4 * 16, 6 * 4 * 16, 7 * 4 * 16, 8 * 4 * 16, 9 * 4 * 16, 10 * 4 * 16, 11 * 4 * 16, 12 * 4 * 16, 13 * 4 * 16, 14 * 4 * 16, 15 * 4 * 16);
begin
  start_circuscharlie := false;
  machine_calls.general_loop := circusc_loop;
  machine_calls.reset := reset_circusc;
  start_audio(false);
  screen_init(1, 256, 256, true);
  screen_mod_scroll(1, 256, 256, 255, 256, 256, 255);
  screen_init(2, 256, 256);
  screen_mod_scroll(2, 256, 256, 255, 256, 256, 255);
  screen_init(3, 256, 256, false, true);
  start_video(224, 256);
  // Main CPU
  m6809_0 := cpu_m6809.Create(2048000, $100, TCPU_M6809);
  m6809_0.change_ram_calls(circusc_getbyte, circusc_putbyte);
  // Sound CPU
  z80_0 := cpu_z80.Create(3579545, $100);
  z80_0.change_ram_calls(circusc_snd_getbyte, circusc_snd_putbyte);
  z80_0.init_sound(circusc_sound);
  // Sound Chip
  sn_76496_0 := sn76496_chip.Create(1789772);
  sn_76496_1 := sn76496_chip.Create(1789772);
  dac_0 := dac_chip.Create;
  // cargar roms y desencriptarlas
  if not(roms_load(@memory, circusc_rom)) then
    exit;
  konami1_decode(@memory[$6000], @mem_opcodes[0], $A000);
  // roms sonido
  if not(roms_load(@mem_snd, circusc_snd)) then
    exit;
  // convertir chars
  if not(roms_load(@memory_temp, circusc_char)) then
    exit;
  init_gfx(0, 8, 8, 512);
  gfx[0].trans[0] := true;
  gfx_set_desc_data(4, 0, 32 * 8, 0, 1, 2, 3);
  convert_gfx(0, 0, @memory_temp, @ps_x, @pc_y, true, false);
  // sprites
  if not(roms_load(@memory_temp, circusc_sprites)) then
    exit;
  init_gfx(1, 16, 16, 384);
  gfx_set_desc_data(4, 0, 128 * 8, 0, 1, 2, 3);
  convert_gfx(1, 0, @memory_temp, @ps_x, @ps_y, true, false);
  // paleta
  if not(roms_load(@memory_temp, circusc_pal)) then
    exit;
  for f := 0 to $1F do
  begin
    bit0 := (memory_temp[f] shr 0) and $01;
    bit1 := (memory_temp[f] shr 1) and $01;
    bit2 := (memory_temp[f] shr 2) and $01;
    colores[f].r := $21 * bit0 + $47 * bit1 + $97 * bit2;
    bit0 := (memory_temp[f] shr 3) and $01;
    bit1 := (memory_temp[f] shr 4) and $01;
    bit2 := (memory_temp[f] shr 5) and $01;
    colores[f].g := $21 * bit0 + $47 * bit1 + $97 * bit2;
    bit0 := 0;
    bit1 := (memory_temp[f] shr 6) and $01;
    bit2 := (memory_temp[f] shr 7) and $01;
    colores[f].b := $21 * bit0 + $47 * bit1 + $97 * bit2;
  end;
  set_pal(colores, 32);
  for f := 0 to $FF do
  begin
    gfx[0].colores[f] := (memory_temp[$20 + f] and $F) + $10; // chars
    gfx[1].colores[f] := memory_temp[$120 + f] and $F; // sprites
  end;
  // DIP
  marcade.dswa := $FF;
  marcade.dswb := $4B;
  marcade.dswa_val2 := @circusc_dip_a;
  marcade.dswb_val2 := @circusc_dip_b;
  // final
  reset_circusc;
  start_circuscharlie := true;
end;

end.
